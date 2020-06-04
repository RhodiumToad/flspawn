/*
 * auto_close.c
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
#include "auto_close.h"
#include "utils.h"

#if LUA_VERSION_NUM < 504
#define lua_toclose(L_, nd_) /*nothing*/
#endif

static int auto_close_close(lua_State *L);
static int auto_close_get(lua_State *L);
static int auto_close_release(lua_State *L);

static RegMeta auto_close_meta = {
    .name = "auto_close",
	.obj_size = 0 ,
	.num_uservalues = 1,
	.metamethods = (luaL_Reg[]){
		{ "__close", auto_close_close },
		{ "__call", auto_close_get },
		{ NULL, NULL }
	},
	.methods = (luaL_Reg[]){
		{ "get", auto_close_get },
		{ "release", auto_close_release },
		{ "close", auto_close_close },
		{ NULL, NULL }
	}
};

static bool mod_init = false;

static void
init_auto_close(lua_State *L)
{
	make_metatable(L, &auto_close_meta, 0);
	lua_pop(L, 1);
	mod_init = true;
}

static int
get_closemethod(lua_State *L, int idx)
{
	int typ = lua_type(L, idx);
	if (typ == LUA_TFUNCTION)
		lua_pushvalue(L, idx);
	else if (typ != LUA_TNIL)
	{
		typ = luaL_getmetafield(L, idx, "__close");
#if LUA_VERSION_NUM < 504
		if (typ == LUA_TNIL)
		{
			typ = lua_getfield(L, idx, "close");
			if (typ == LUA_TNIL)
				lua_pop(L, 1);
		}
#endif
	}
	return typ;
}

LUAI_FUNC void
make_auto_closevar(lua_State *L, int idx)
{
	idx = lua_absindex(L, idx);

	if (!mod_init)
		init_auto_close(L);

	make_object(L, &auto_close_meta);

	lua_pushvalue(L, idx);
	lua_setuservalue(L, -2);

	lua_replace(L, idx);
	lua_toclose(L, idx);
	return;
}

LUAI_FUNC int
auto_closevar_get(lua_State *L, int idx)
{
	if (to_object_exact(L, idx, &auto_close_meta))
		return lua_getuservalue(L, idx);
	lua_pushnil(L);
	return LUA_TNIL;
}

static int
auto_close_get(lua_State *L)
{
	check_object_exact(L, 1, &auto_close_meta);
	lua_getuservalue(L, 1);
	return 1;
}

LUAI_FUNC int
auto_closevar_release(lua_State *L, int idx)
{
	if (verify_object_exact(L, idx, &auto_close_meta))
	{
		int typ = lua_getuservalue(L, idx);
		lua_pushnil(L);
		lua_setuservalue(L, idx);
		return typ;
	}
	lua_pushnil(L);
	return LUA_TNIL;
}

static int
auto_close_release(lua_State *L)
{
	check_object_exact(L, 1, &auto_close_meta);
	lua_getuservalue(L, 1);
	lua_pushnil(L);
	lua_setuservalue(L, 1);
	return 1;
}

static int
auto_close_close(lua_State *L)
{
	check_object_exact(L, 1, &auto_close_meta);

	int typ = lua_getuservalue(L, 1);
	lua_pushnil(L);
	lua_setuservalue(L, 1);

	if (typ != LUA_TNIL	&& get_closemethod(L, -1) != LUA_TNIL)
	{
		lua_rotate(L, -2, 1);
		lua_call(L, 1, 0);
	}

	return 0;
}

LUAI_FUNC void
auto_closevar_force(lua_State *L, int idx)
{
	if (lua_toboolean(L, idx)
		&& luaL_callmeta(L, idx, "__close"))
		lua_pop(L, 1);
}

#ifdef AUTO_CLOSE_MODULE

static int
auto_close_new(lua_State *L)
{
	if (lua_gettop(L) < 2)
		lua_settop(L, 2);

	make_object(L, &auto_close_meta);
	lua_replace(L, 1);
	lua_pushvalue(L, 2);
	lua_setuservalue(L, 1);

	return lua_gettop(L);
}

static RegMeta auto_close_module_meta = {
    .name = "auto_close_module",
	.metamethods = (luaL_Reg[]){
		{ "__call", auto_close_new },
		{ NULL, NULL }
	},
};

EXPORTED
int luaopen_auto_close(lua_State *L)
{
	init_auto_close(L);
	lua_newtable(L);
	make_metatable(L, &auto_close_module_meta);
	lua_setmetatable(L, -2);
	return 1;
}

#endif

/*end*/
