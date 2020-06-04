/*
 * simple_class.c
 *
 * Provide a simple single-inheritance class framework for Lua
 * userdata objects (and tables).
 *
 * Copyright 2020 Andrew Gierth.
 * SPDX-License-Identifier: MIT
 *
 * Though this file is initially distributed under the MIT license, the
 * original author grants permission for its redistribution under alternative
 * licenses as set forth at <https://rhodiumtoad.github.io/RELICENSE.txt>.
 * This paragraph and the RELICENSE.txt file are not part of the license and
 * may be omitted in redistributions.
 */

#include "simple_class.h"
#include "utils.h"

// Utility for generating ephemeron tables. We use a common metatable for
// these, which requires setting its __metatable entry for safety.

static RegMeta ephemeron_meta = {
    .name = "ephemeron",
};

LUAI_FUNC void
make_ephemeron(lua_State *L)
{
    lua_newtable(L);
    if (unlikely(lua_rawgetp(L, LUA_REGISTRYINDEX,
							 &ephemeron_meta) != LUA_TTABLE))
    {
		lua_pop(L, 1);
		make_metatable(L, &ephemeron_meta, 0);
		lua_pushstring(L, "k");
		lua_setfield(L, -2, "__mode");
    }
    lua_setmetatable(L, -2);
}

// count funcs in a luaL_Reg.

static int
count_funcs(luaL_Reg *funcs)
{
    int n = 0;
    for (luaL_Reg *p = funcs; p && p->name; ++p, ++n)
		continue;
    return n;
}

// make_metatable
//
// This is the part that does all the hard work.

LUAI_FUNC void
make_metatable(lua_State *L, RegMeta *meta, int nups)
{
    int nmethods = (meta->method_table_hashsize > 0
					? meta->method_table_hashsize
					: count_funcs(meta->methods));
    int nfuncs = (meta->metatable_hashsize > 0
				  ? meta->metatable_hashsize
				  : count_funcs(meta->metamethods) + 4);

    lua_createtable(L, 0, nfuncs);

    if (!meta->expose_metatable)
    {
		lua_pushboolean(L, 1);
		lua_setfield(L, -2, "__metatable");
    }

    if (meta->methods || nmethods > 0)
    {
		// Create methods table. We could populate this from the parent and
		// then add our own methods, or just add our own and use the parent's
		// metatable. The latter is a bit simpler, though the former may be
		// faster at runtime.
		//
		// We do have to make sure we add our methods _before_ setting the
		// metatable, though, because luaL_setfuncs does non-raw assignments
		// and the parent might have a __newindex.

		lua_createtable(L, 0, nmethods);
		if (meta->methods)
		{
			if (nups > 0)
			{
				// If there are upvalues, they are now under 2 tables, and we
				// need to copy them because luaL_setfuncs will pop them.

				int offs = -(nups+2);
				luaL_checkstack(L, nups + 5, NULL);
				for (int i = 0; i < nups; ++i)
					lua_pushvalue(L, offs);
			}
			luaL_setfuncs(L, meta->methods, nups);
		}

		// Attach the parent metatable. It's up to the caller to ensure that
		// any further modifications to the methods table after this use raw
		// accesses only.

		if (meta->parent)
		{
			get_metatable(L, meta->parent);
			lua_pushvalue(L, -1);
			lua_setmetatable(L, -3);
			lua_setfield(L, -3, "_parent");
		}

		lua_setfield(L, -2, "__index");
    }

    // Populate the metatable with functions. If there are upvalues, we have
    // to shuffle the stack to put the table underneath.

    if (nups > 0)
		lua_insert(L, -(nups+1));
    if (meta->metamethods)
		luaL_setfuncs(L, meta->metamethods, nups);
    else if (nups > 0)
		lua_pop(L, nups);

    if (meta->name)
    {
		lua_pushstring(L, meta->name);
		lua_setfield(L, -2, "__name");
    }

	// If we allow derived classes, we create a table which will get a key
	// for every such class, including this one.

    if (meta->allow_derived)
    {
		make_ephemeron(L);
		lua_pushvalue(L, -2);
		lua_pushboolean(L, 1);
		lua_rawset(L, -3);
		lua_pushvalue(L, -1);
		lua_rawsetp(L, LUA_REGISTRYINDEX, &meta->allow_derived);
		lua_setfield(L, -2, "_derived");
    }

	// Link ourselves into the registry.

    if (!meta->no_register)
    {
		lua_pushvalue(L, -1);
		lua_rawsetp(L, LUA_REGISTRYINDEX, meta);
    }

	// Add ourselves to the allow_derived tables of all parent classes.

    for (RegMeta *parent = meta->parent; parent; parent = parent->parent)
    {
		if (unlikely(lua_rawgetp(L, LUA_REGISTRYINDEX,
								 &parent->allow_derived) != LUA_TTABLE))
			luaL_error(L, "hierarchy initialization error");
		lua_pushvalue(L, -2);
		lua_pushboolean(L, 1);
		lua_rawset(L, -3);
		lua_pop(L, 1);
    }
}

// Push the metatable for the specified class on the stack.

LUAI_FUNC void
get_metatable(lua_State *L, RegMeta *meta)
{
    if (unlikely(lua_rawgetp(L, LUA_REGISTRYINDEX, meta) != LUA_TTABLE))
		luaL_error(L, "missing metatable for object type %s",
				   meta->name);
}

// Create a new userdata

LUAI_FUNC void *
make_object(lua_State *L, RegMeta *meta)
{
#if LUA_VERSION_NUM >= 504
	void *obj = lua_newuserdatauv(L, meta->obj_size, meta->num_uservalues);
#else
    void *obj = lua_newuserdata(L, meta->obj_size);
#endif
    get_metatable(L, meta);
    lua_setmetatable(L, -2);
    return obj;
}

// Returns true if the value at IDX is a userdata of exactly the specified
// type, without allowing derived types.

LUAI_FUNC bool
verify_object_exact(lua_State *L, int idx, RegMeta *meta)
{
	if (unlikely(!lua_getmetatable(L, idx)))
		return false;
	lua_rawgetp(L, LUA_REGISTRYINDEX, meta);
	bool res = lua_rawequal(L, -1, -2);
    lua_pop(L, 2);
    return res;
}

// Returns true if the value at IDX is a userdata of the specified type or one
// of its derivatives if it has any.

LUAI_FUNC bool
verify_object(lua_State *L, int idx, RegMeta *meta)
{
	if (!meta->allow_derived)
		return verify_object_exact(L, idx, meta);
	if (unlikely(lua_rawgetp(L, LUA_REGISTRYINDEX,
							 &meta->allow_derived) != LUA_TTABLE
				 || !lua_getmetatable(L, (idx >= 0) ? idx : idx-1)))
	{
		lua_pop(L, 1);
		return false;
	}

	bool res = (lua_rawget(L, -2) != LUA_TNIL);
	lua_pop(L, 2);
	return res;
}

// Return the userdata pointer of the object at IDX, or NULL if it's not the
// right type

LUAI_FUNC void *
to_object_exact(lua_State *L, int idx, RegMeta *meta)
{
    void *p = lua_touserdata(L, idx);
    if (p != NULL && !verify_object_exact(L, idx, meta))
		return NULL;
    return p;
}

LUAI_FUNC void *
to_object(lua_State *L, int idx, RegMeta *meta)
{
    void *p = lua_touserdata(L, idx);
    if (p != NULL && !verify_object(L, idx, meta))
		return NULL;
    return p;
}

NO_INLINE NO_RETURN
static void
type_check_error(lua_State *L, int arg, RegMeta *meta, bool allow_derived)
{
    const char *typearg;

    if (luaL_getmetafield(L, arg, "__name") == LUA_TSTRING)
		typearg = lua_tostring(L, -1);
    else if (lua_type(L, arg) == LUA_TLIGHTUSERDATA)
		typearg = "light userdata";
    else
		typearg = luaL_typename(L, arg);

    const char *msg
		= lua_pushfstring(L,
						  "%s %sexpected, got %s",
						  meta->name,
						  ((allow_derived && meta->allow_derived)
						   ? "or derivative "
						   : ""),
						  typearg);
    luaL_argerror(L, arg, msg);
}

// Return the userdata pointer of the object at IDX, or throw error if it's
// not the right type

LUAI_FUNC void *
check_object_exact(lua_State *L, int idx, RegMeta *meta)
{
    void *p = lua_touserdata(L, idx);
    if (unlikely(p == NULL || !verify_object_exact(L, idx, meta)))
		type_check_error(L, idx, meta, false);
    return p;
}

LUAI_FUNC void *
check_object(lua_State *L, int idx, RegMeta *meta)
{
    void *p = lua_touserdata(L, idx);
    if (unlikely(p == NULL || !verify_object(L, idx, meta)))
		type_check_error(L, idx, meta, true);
    return p;
}

// end
