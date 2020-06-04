/*
 * auto_close.h
 *
 * Provide a simple conditional closure object.
 *
 * Copyright 2020 Andrew Gierth.
 */

#ifndef H_5896F5B6_B630_11EA_89E6_6CF049962D5A_
#define H_5896F5B6_B630_11EA_89E6_6CF049962D5A_

#include <lua.h>
#include <lauxlib.h>

// make_auto_closevar(L, idx)
//
// Replaces the value at idx with an auto_close object referencing that
// value and marks the index as toclose. The object will forward a __close
// call to the referenced object except as detailed below, and supports these
// methods:
//
//   obj:get()     -- returns the original value
//   obj:release() -- returns the original value and unreferences it
//
// These are available as C functions too.

LUAI_FUNC void make_auto_closevar(lua_State *L, int idx);

LUAI_FUNC int auto_closevar_get(lua_State *L, int idx);
LUAI_FUNC int auto_closevar_release(lua_State *L, int idx);

LUAI_FUNC void auto_closevar_force(lua_State *L, int idx);

// On versions without lua_toclose, the caller should use
// auto_close_compat() to explicitly close slots that are going out of
// scope. (Since error paths won't do this, it's also important to ensure that
// nothing relies on it too hard, e.g. by ensuring that garbage collection
// also does any needed cleanup.)
//
// On versions with toclose, the macro does nothing.

#if LUA_VERSION_NUM < 504
#define auto_closevar_compat(L_, i_) auto_closevar_force((L_),(i_))
#else
#define auto_closevar_compat(L_, i_) ((void)(L_),(void)(i_))
#endif

#endif
