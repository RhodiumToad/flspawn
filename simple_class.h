/*
 * simple_class.h
 *
 * Provide a simple single-inheritance class framework for Lua
 * userdata objects (and tables).
 *
 * Copyright 2020 Andrew Gierth.
 */

#ifndef H_4532C614_9F6B_11EA_A8A6_6CF049962D5A_
#define H_4532C614_9F6B_11EA_A8A6_6CF049962D5A_

#include <lua.h>
#include <lauxlib.h>

#include <stddef.h>
#include <stdbool.h>

// Almost all of the fields of RegMeta are optional. The idea is that typical
// usage will initialize only the needed fields using named initializers:
//
//   .obj_size = size_t
//     This is the size of the userdata objects that make_object will create.
//   .name = "some string"
//     This value is stored as __name in the metatable.
//   .parent = (RegMeta *)
//     Specifies the parent class.
//   .allow_derived = bool
//     true if this class can be used as the .parent of another.
//   .no_register = bool
//     If true, the metatable will not be registered in the registry. This
//     prevents use of make_object, check_object, etc., so is useful only in
//     special cases.
//   .expose_metatable = bool
//     If true, __metatable = true is not added to the metatable.
//   .num_uservalues = int
//     Specify the number of uservalues here. Note that for compatibility with
//     both 5.3 and 5.4, you should specify 1 if a uservalue is needed. Note
//     that the default is 0, which on 5.4 will mean there are no uservalues
//     by default. This value is ignored on 5.3.
//   .method_table_hashsize = int
//     Specify the hash size parameter to lua_createtable for the methods
//     table. Setting it to 1 also forces the methods table to exist, which is
//     important for derived classes which add no methods of their own (if you
//     don't set this, methods won't work on such classes). Normally not
//     needed if there are methods, since the number of methods supplied will
//     be used.
//   .metatable_hashsize = int
//     Hash size parameter for the metatable. Not normally needed, since it
//     will be set by counting the metamethods and adding some extra.
//   .methods = (luaL_Reg[]){ { "name", func }, ... { NULL, NULL } }
//     Table of methods.
//   .metamethods = (luaL_Reg[]){ { "name", func }, ... { NULL, NULL } }
//     Table of metamethods.

typedef struct RegMeta {
	// used by make_object
	size_t		obj_size;

	// used by make_metatable and verify_object
    int			num_uservalues;
    bool		allow_derived;	// note: must not be the first member

	// used by make_metatable:
    bool		no_register;
    bool		expose_metatable;
    const char *name;
    struct RegMeta *parent;
    int			method_table_hashsize;
    int			metatable_hashsize;
    luaL_Reg   *methods;
    luaL_Reg   *metamethods;
} RegMeta;

LUAI_FUNC void make_metatable(lua_State *L, RegMeta *meta, int nups);
LUAI_FUNC void make_ephemeron(lua_State *L);
LUAI_FUNC void get_metatable(lua_State *L, RegMeta *meta);
LUAI_FUNC void *make_object(lua_State *L, RegMeta *meta);
LUAI_FUNC bool verify_object_exact(lua_State *L, int idx, RegMeta *meta);
LUAI_FUNC bool verify_object(lua_State *L, int idx, RegMeta *meta);
LUAI_FUNC void *to_object_exact(lua_State *L, int idx, RegMeta *meta);
LUAI_FUNC void *to_object(lua_State *L, int idx, RegMeta *meta);
LUAI_FUNC void *check_object_exact(lua_State *L, int idx, RegMeta *meta);
LUAI_FUNC void *check_object(lua_State *L, int idx, RegMeta *meta);

#endif
