/*
 * lspawn.c
 *
 * Copyright 2020 Andrew Gierth.
 * SPDX-License-Identifier: BSD-2-Clause OR MIT
 *
 * Though this file is initially distributed under the 2-clause BSD license OR
 * the MIT license, the original author grants permission for its
 * redistribution under alternative licenses as set forth at
 * <https://rhodiumtoad.github.io/RELICENSE.txt>. This paragraph and the
 * RELICENSE.txt file are not part of the license and may be omitted in
 * redistributions.
 */

#include <lua.h>
#include <lauxlib.h>

#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <paths.h>

#include <sys/wait.h>

#include "myspawn.h"
#include "simple_class.h"
#include "auto_close.h"
#include "utils.h"

extern const char **environ;

// Lua version cruft

#if LUA_VERSION_NUM < 503
#error This code requires Lua 5.3 or later
#endif

//
// File action objects for the user to populate the files={} table with.
//

enum lspawn_fa_type {
	FA_INVALID = 0,
    FA_CLOSE,
    FA_NULL,
    FA_INHERIT,
	FA_INPIPE,
	FA_OUTPIPE,
    FA_OPEN,
    FA_INHERIT_FROM,
    FA_COPY_FROM,
};

struct lspawn_fa_data
{
    enum lspawn_fa_type type;
    int value1;
    int value2;
};

static int lspawn_fa_tostring(lua_State *L);
static int lspawn_fa_call(lua_State *L);

static RegMeta lspawn_file_action_meta = {
	.name = "lspawn file action",
	.obj_size = sizeof(struct lspawn_fa_data),
	.num_uservalues = 1,
	.metamethods = (luaL_Reg[]){
		{ "__tostring", lspawn_fa_tostring },
		{ "__call", lspawn_fa_call },
		{ NULL, NULL }
	}
};

/*
 * Mainly to allow printing for diagnostic purposes.
 */
static int
lspawn_fa_tostring(lua_State *L)
{
	struct lspawn_fa_data *p = check_object_exact(L, 1,
												  &lspawn_file_action_meta);
	const char *name = NULL;
	const char *fname = NULL;
	char extras[64];

	extras[0] = 0;

	switch (p->type)
	{
		case FA_INVALID:	name = "invalid";		break;
		case FA_CLOSE:		name = "close";			break;
		case FA_NULL:		name = "null";			break;
		case FA_INHERIT:	name = "inherit";		break;
		case FA_INPIPE:		name = "input pipe";	break;
		case FA_OUTPIPE:	name = "output pipe";	break;

		case FA_INHERIT_FROM:
			name = "inherit from";
			FALLTHROUGH; /* FALLTHROUGH */

		case FA_COPY_FROM:
			if (!name)
				name = "copy from";
			snprintf(extras, sizeof(extras),
					 " fd=%d", p->value1);
			break;

		case FA_OPEN:
			name = "open";
			lua_getuservalue(L, 1);
			fname = lua_tostring(L, -1);
			snprintf(extras, sizeof(extras),
					 " flags=%#04x perms=%#04o", p->value1, p->value2);
			lua_pushfstring(L,
							"lspawn file action: %s: fname=\"%s\"%s <%p>",
							name, fname, extras, p);
			return 1;
	}

	lua_pushfstring(L,
					"lspawn file action: %s:%s <%p>",
					name, extras, p);
	return 1;
}

static int
lspawn_fa_new(lua_State *L,
			  enum lspawn_fa_type type,
			  int value1, int value2)
{
	struct lspawn_fa_data *p = make_object(L, &lspawn_file_action_meta);

	p->type = type;
	p->value1 = value1;
	p->value2 = value2;
	return 1;
}

static int
lspawn_fa_make_close(lua_State *L)
{
	return lspawn_fa_new(L, FA_CLOSE, 0, 0);
}
static int
lspawn_fa_make_null(lua_State *L)
{
	return lspawn_fa_new(L, FA_NULL, 0, 0);
}
static int
lspawn_fa_make_inherit(lua_State *L)
{
	return lspawn_fa_new(L, FA_INHERIT, 0, 0);
}
static int
lspawn_fa_make_inpipe(lua_State *L)
{
	return lspawn_fa_new(L, FA_INPIPE, 0, 0);
}
static int
lspawn_fa_make_outpipe(lua_State *L)
{
	return lspawn_fa_new(L, FA_OUTPIPE, 0, 0);
}

static int
lspawn_fileflags(lua_State *L, int idx)
{
	if (lua_isinteger(L, idx))
		return lua_tointeger(L, idx);
	else if (lua_isstring(L, idx))
	{
		const char *modestr = lua_tostring(L, idx);
		int flags0 = O_WRONLY;
		int flags1 = 0;

		switch (modestr[0])
		{
			case 'r':	flags0 = O_RDONLY;				break;
			case 'w':	flags1 = O_CREAT | O_TRUNC;		break;
			case 'a':	flags1 = O_CREAT | O_APPEND;	break;
			default:
				return luaL_argerror(L, idx, "unknown file open mode");
		}
		switch (modestr[1])
		{
			case '\0':	break;
			case '+':	flags0 = O_RDWR;	break;
			default:
				return luaL_argerror(L, idx, "unknown file open mode");
		}

		return flags0 | flags1;
	}
	else
		return luaL_argerror(L, idx, "expected integer or string");
}

static int
lspawn_fa_make_open(lua_State *L)
{
	int flags = lspawn_fileflags(L, 2);
	int perms = luaL_optinteger(L, 3, 0666);
	luaL_checkstring(L, 1);

	lspawn_fa_new(L, FA_OPEN, flags, perms);
	lua_pushvalue(L, 1);
	lua_setuservalue(L, -2);
	return 1;
}

static int
lspawn_fa_make_inherit_from(lua_State *L)
{
	int isint = 0;
	int fd = lua_tointegerx(L, 1, &isint);
	luaL_Stream *fp = luaL_testudata(L, 1, LUA_FILEHANDLE);

	luaL_argcheck(L, ((isint && fd >= 0) || (fp && fp->f && !fp->closef)),
				  1, "expected integer fd or open file");

	if (!fp)
		return lspawn_fa_new(L, FA_INHERIT_FROM, fd, 0);

	lua_settop(L, 1);
	return 1;
}

static int
lspawn_fa_make_copy_from(lua_State *L)
{
	int fd = luaL_checkinteger(L, 1);
	return lspawn_fa_new(L, FA_INHERIT, fd, 0);
}

// This does nothing. It simply means that, e.g. spawn.null and spawn.null()
// are equivalent.

static int
lspawn_fa_call(lua_State *L)
{
	check_object_exact(L, 1, &lspawn_file_action_meta);
	if (lua_gettop(L) != 1)
		luaL_argerror(L, 1, "no arguments expected");
	return 1;
}

// These are created as functions

static luaL_Reg lspawn_funcs[] = {
	{ "inherit_from", lspawn_fa_make_inherit_from },
	{ "copy_from", lspawn_fa_make_copy_from },
	{ "open", lspawn_fa_make_open },
	{ NULL, NULL }
};

// And these functions are called to create their objects

static luaL_Reg lspawn_objs[] = {
	{"null", lspawn_fa_make_null },
	{"close", lspawn_fa_make_close },
	{"inherit", lspawn_fa_make_inherit },
	{"input_pipe", lspawn_fa_make_inpipe },
	{"output_pipe", lspawn_fa_make_outpipe },
	{NULL, NULL }
};

//
// lspawn_file_actions object is only used internally, not exposed to the
// user. It exists to garbage-collect the spawn_file_actions_t that we create.
//

static int lspawn_file_actions_gc(lua_State *L);

static RegMeta lspawn_file_actions_meta = {
	.name = "lspawn file actions",
	.obj_size = sizeof(spawn_file_actions_t),
	.metamethods = (luaL_Reg[]){
		{ "__gc", lspawn_file_actions_gc },
		{ "__close",  lspawn_file_actions_gc },
		{ NULL, NULL }
	}
};

static int
lspawn_file_actions_gc(lua_State *L)
{
	spawn_file_actions_t *p = check_object(L, 1, &lspawn_file_actions_meta);
	lua_pushnil(L);
	lua_setmetatable(L, 1);
	spawn_file_actions_destroy(p);
	return 0;
}

static spawn_file_actions_t *
lspawn_file_actions_new(lua_State *L)
{
	spawn_file_actions_t *p = make_object(L, &lspawn_file_actions_meta);
	spawn_file_actions_init(p);
	return p;
}

//
// Lua container convenience functions.
//
// We define a value as "indexable" if it supports gettable, etc.; this is
// true if it is a table or has a __index metavalue.
//
// We define a value as a "container" if it supports pairs() (not necessarily
// ipairs); i.e. it is either a table or has a __pairs metamethod. We assume
// that a container is indexable consistently with its pairs() result (there's
// no general way we could check this anyway).
//
// We use pairs_start / pairs_next to do a pairs() loop from C while
// respecting metamethods.
//

static bool
is_indexable(lua_State *L, int nd)
{
	if (lua_type(L, nd) == LUA_TTABLE)
		return true;
	if (luaL_getmetafield(L, nd, "__index") != LUA_TNIL)
	{
		lua_pop(L, 1);
		return true;
	}
	return false;
}

static bool
is_container(lua_State *L, int nd)
{
	if (lua_type(L, nd) == LUA_TTABLE)
		return true;
	if (luaL_getmetafield(L, nd, "__pairs") != LUA_TNIL)
	{
		lua_pop(L, 1);
		return true;
	}
	return false;
}

// This is a full emulation of a lua "for k,v in pairs(t) do" loop, even
// including the 5.4 close slot.
//
// toclose, if >0, specifies a stack slot to put the iterator's close object
// into. This has no effect on 5.3, only on 5.4+. It's up to the caller to
// ensure that it gets popped in a safe way at an appropriate time.

static bool
pairs_start(lua_State *L, int nd, int toclose, bool noerror)
{
	(void) toclose;
	nd = lua_absindex(L, nd);
	if (luaL_getmetafield(L, nd, "__pairs") == LUA_TNIL)
	{
		if (!noerror)
			luaL_checktype(L, nd, LUA_TTABLE);
		lua_pushnil(L); /* initial key for lua_next */
		return false;
	}
#if LUA_VERSION_NUM >= 504
	else if (toclose > 0)
	{
		lua_pushvalue(L, nd);
		lua_call(L, 1, 4);
		lua_replace(L, toclose);
		lua_toclose(L, toclose);
		return true;
	}
#endif
	else
	{
		lua_pushvalue(L, nd);
		lua_call(L, 1, 3);
		return true;
	}
}

// At call, the stack is:
//
//    iterfunc \ state \ key
// or
//    iterfunc \ nil \ key
//
// On true return, we leave
//
//	iterfunc \ state \ key \ value
//
// On false return, we pop all three
//
// The intended standard usage is:
//
// bool metaflag = pairs_start(...);
// while (metaflag ? pairs_next(...) : lua_next(...)) { ... }
//

static int
pairs_next(lua_State *L)
{
	lua_pushvalue(L, -3);	// iter state key iter
	lua_pushvalue(L, -3);	// iter state key iter state
	lua_rotate(L, -3, -1);	// iter state iter state key
	lua_call(L, 2, 2);	  /* iter state key val */
	if (lua_isnil(L, -2))
	{
		lua_pop(L, 4);
		return 0;
	}
	return 1;
}

//
// Given absolute stack indices of the command string and arg table, push (at
// the current stack top) string values for argv[0..n-1] and return n. Note
// that nothing is pushed for the terminating null, that's handled later.
//

static int
process_args(lua_State *L, int cmd_idx, int argt_idx)
{
	bool have_argv0 = false;

	// argv[0] is defaulted to the command string if not in the table

	if (!lua_isnil(L, argt_idx))
	{
		if (lua_geti(L, argt_idx, 0) != LUA_TSTRING)
		{
			if (!lua_isnil(L, -1))
				return luaL_argerror(L, 1, "bad value for args[0], expected string");
			lua_pop(L, 1);
		}
		else
			have_argv0 = true;
	}

	if (!have_argv0)
		lua_pushvalue(L, cmd_idx);

	int nargs = 1;

	if (!lua_isnil(L, argt_idx))
	{
		while (lua_geti(L, argt_idx, nargs) != LUA_TNIL)
		{
			if ((nargs & 31) == 0)
				luaL_checkstack(L, 80, "processing spawn arguments");
			// note: isstring accepts numbers too
			if (!lua_isstring(L, -1))
				return luaL_argerror(L, 1, "bad value in args, expected string");
			// force stringiness of value
			lua_tostring(L, -1);
			++nargs;
		}
	}

	return nargs;
}

//
// Given absolute stack indices of the env table, and a slot for an iterator
// close object, push (at the current stack top) string values for
// envp[0..n-1] and return n. Note that nothing is pushed for the terminating
// null, that's handled later. Return -1 instead if we're just using the
// existing environment unchanged. If inherit is false, start from a clean
// slate, otherwise keep vars from the existing env (pushed as lightudata to
// avoid overhead of interning strings).
//

static int
process_envs(lua_State *L, int envt_idx, int iter_idx, bool inherit)
{
	int nenvs = 0;
	bool have_env = !lua_isnil(L, envt_idx);

	if (inherit && !have_env)
		return -1;

	if (inherit && environ)
	{
		for (int i = 0; environ[i]; ++i)
		{
			const char *name = environ[i];
			const char *eq = strchr(name, '=');
			int argt;

			if (!eq || name == eq)
				continue;

			lua_pushlstring(L, name, eq - name);
			argt = lua_gettable(L, envt_idx);
			lua_pop(L, 1);

			if (argt == LUA_TNIL)
			{
				// push these as light udata rather than as strings,
				// to avoid the overhead of copying/interning them
				lua_pushlightuserdata(L, (void*) name);
				++nenvs;
				if ((nenvs & 31) == 0)
					luaL_checkstack(L, 80, "processing spawn environment");
			}
		}
	}

	if (have_env)
	{
		bool metaloop = pairs_start(L, envt_idx, iter_idx, false);

		while (metaloop ? pairs_next(L) : lua_next(L, envt_idx))
		{
			// consider only string keys, and skip false values
			if (lua_type(L, -2) != LUA_TSTRING
				|| !lua_toboolean(L, -1))
			{
				lua_pop(L, 1);
				continue;
			}
			// string or number values are ok
			if (!lua_isstring(L, -1))
				return luaL_argerror(L, 1, "bad value in environ, expected string");
			// stack:  key \ value
			lua_pushvalue(L, -2);
			// stack:  key \ value \ key
			lua_pushvalue(L, lua_upvalueindex(1));
			// stack:  key \ value \ key \ "="
			lua_rotate(L, -3, -1);
			// stack:  key \ key \ "=" \ value
			lua_concat(L, 3);
			// stack:  [iterfunc \ state \] key \ key.."="..value
			lua_rotate(L, (metaloop ? -4 : -2), 1);
			// stack:  key.."="..value \ [iterfunc \ state \] key
			++nenvs;
			if ((nenvs & 31) == 0)
				luaL_checkstack(L, 80, "processing spawn environment");
		}
	}

	return nenvs;
}

// Given argv[0..n-1] and envp[0..e-1] on the lua stack starting at idx,
// construct C-style argv[] and envp[] (with envp[] starting after argv[n])

static void
vectorize_args(lua_State *L,
			   const char **argv,
			   int idx,
			   int nargs,
			   int nenvs)
{
	int i = 0;

	for (; nargs > 0; --nargs, ++idx)
		argv[i++] = lua_tostring(L, idx);
	argv[i++] = NULL;

	if (nenvs < 0)
		return;

	for (; nenvs > 0; --nenvs, ++idx)
		if (lua_islightuserdata(L, idx))
			argv[i++] = lua_touserdata(L, idx);
		else
			argv[i++] = lua_tostring(L, idx);
	argv[i++] = NULL;
}

// Given a sequence on the top of the lua stack, add the signals in it
// to sigs_add and remove them from sigs_del. Signals can be specified
// by name or number.

static int
process_siglist(lua_State *L, sigset_t *sigs_add, sigset_t *sigs_del)
{
	int argt;

	for (int i = 1; (argt = lua_geti(L, -1, i)) != LUA_TNIL; ++i)
	{
		int isint = 0;
		int rc = 0;
		int sig;

		if (argt != LUA_TNUMBER)
			argt = lua_rawget(L, lua_upvalueindex(2));
		sig = lua_tointegerx(L, -1, &isint);
		if (isint)
		{
			if (sigs_add)
				rc = sigaddset(sigs_add, sig);
			if (rc == 0 && sigs_del)
				rc = sigdelset(sigs_del, sig);
		}
		else
			rc = -1;
		if (rc < 0)
			return luaL_argerror(L, 1, "bad entry in signals table");
		lua_pop(L, 1);
	}
	lua_pop(L, 1);
	return 0;
}

// process the signals argument.

static int
process_signals(lua_State *L,
				int sigt_idx,
				sigset_t *default_sigs,
				sigset_t *ignore_sigs,
				sigset_t *block_sigs)
{
	sigt_idx = lua_absindex(L, sigt_idx);

	// initially default_sigs is full and the others empty.
	// ignored sigs are added to ignore_sigs and removed from
	// default_sigs; preserved sigs are removed from default_sigs;
	// blocked sigs added to block_sigs.

	lua_getfield(L, sigt_idx, "ignore");
	if (is_indexable(L, -1))
		process_siglist(L, ignore_sigs, default_sigs);
	else if (!lua_isnil(L, -1))
		return luaL_argerror(L, 1, "bad signal ignore table, expected indexable value");

	lua_getfield(L, sigt_idx, "block");
	if (is_indexable(L, -1))
		process_siglist(L, block_sigs, NULL);
	else if (!lua_isnil(L, -1))
		return luaL_argerror(L, 1, "bad signal block table, expected indexable value");

	lua_getfield(L, sigt_idx, "preserve");
	if (is_indexable(L, -1))
		process_siglist(L, NULL, default_sigs);
	else if (!lua_isnil(L, -1))
		return luaL_argerror(L, 1, "bad signal preserve table, expected indexable value");

	lua_pop(L, 3);
	return 0;
}

static int
file_act_err(lua_State *L, int err)
{
	return luaL_error(L, "error adding file action: %s", strerror(err));
}

// If no table of file actions was passed in, do the default thing.

static int
process_files_default(lua_State *L,
					  spawn_file_actions_t *facts,
					  int foreground_fd)
{
	if (foreground_fd >= 0)
	{
		if (foreground_fd > 2)
			return file_act_err(L, ENOTTY);

		int err = spawn_file_actions_addsetpgrp_np(facts, foreground_fd);
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	int err = spawn_file_actions_addclosefrom_np(facts, 3);
	if (unlikely(err != 0))
		return file_act_err(L, err);

	return 0;
}

//
// Stuff for handling file descriptors in a way that avoids leaking them; at
// worst, they should get cleaned up by GC. Also, define our own stream
// subtype for a pipe returned from spawn, which emulates pclose() on close.
//

typedef struct spawn_pipe_stream {
	luaL_Stream lstr;
	pid_t pid;
} SPStream;

// This is the ->closef of our custom FILE*

static int
spawn_pipe_close(lua_State *L)
{
	SPStream *s = luaL_checkudata(L, 1, LUA_FILEHANDLE);
	if (s->lstr.f)
	{
		fclose(s->lstr.f);
		s->lstr.f = NULL;
	}
	if (s->pid != -1)
	{
		int status = -1;
		waitpid(s->pid, &status, 0);
		return luaL_execresult(L, status);
	}
	else
		return 0;
}

// This is a ->closef for a plain FILE*
// Unlike the usual one, it is safe on unopened handles.

static int
spawn_file_close(lua_State *L)
{
	luaL_Stream *s = luaL_checkudata(L, 1, LUA_FILEHANDLE);
	if (s->f)
	{
		int res = fclose(s->f);
		s->f = NULL;
		return luaL_fileresult(L, (res == 0), NULL);
	}
	else
		return 0;
}

// idx is the absolute index of a pair of stack slots to receive the new FILE*
// objects. The first slot is the parent's end and the second the child's. The
// fd (in the parent) of the child's end of the pipe is returned.

static int
make_pipe(lua_State *L,
		  enum lspawn_fa_type fa_type,
		  int idx)
{
	int fds[2];
	int parent_end;
	int child_end;
	const char *parent_mode;

	SPStream *sp = lua_newuserdata(L, sizeof(SPStream));
	sp->lstr.f = NULL;
	sp->lstr.closef = spawn_pipe_close;
	sp->pid = -1;
	luaL_setmetatable(L, LUA_FILEHANDLE);

	luaL_Stream *lsp = lua_newuserdata(L, sizeof(luaL_Stream));
	lsp->f = NULL;
	lsp->closef = spawn_file_close;
	luaL_setmetatable(L, LUA_FILEHANDLE);

	if (pipe(fds) < 0)
		return luaL_error(L, "pipe failed: %s", strerror(errno));

	// We respect the conventional pipe directions even though our pipes allow
	// bidirectional use. INPIPE means it's an input from the child's view
	if (fa_type == FA_INPIPE)
	{
		child_end = fds[0];
		parent_end = fds[1];
		parent_mode = "w";
	}
	else
	{
		parent_end = fds[0];
		child_end = fds[1];
		parent_mode = "r";
	}

	FILE *f1 = fdopen(parent_end, parent_mode);
	if (!f1)
	{
		const char *str = strerror(errno);
		close(parent_end);
		close(child_end);
		return luaL_error(L, "fdopen failed: %s", str);
	}

	// Use r+ for the mode because we don't really care.
	FILE *f2 = fdopen(child_end, "r+");
	if (!f2)
	{
		const char *str = strerror(errno);
		close(child_end);
		fclose(f1);
		return luaL_error(L, "fdopen failed: %s", str);
	}

	// After this, the files are considered owned by Lua
	sp->lstr.f = f1;
	lsp->f = f2;

	// stack:  SPstream (parent end) \ luaL_Stream (child end)

	lua_copy(L, -2, idx);
	lua_copy(L, -1, idx+1);
	lua_pop(L, 2);

	make_auto_closevar(L, idx);
	make_auto_closevar(L, idx+1);

	return child_end;
}

// Do all the hairy file juggling stuff.

static int
process_files(lua_State *L,
			  int ftab_idx, int pipe_idx,
			  spawn_file_actions_t *facts, int foreground_fd)
{
	int err;
	int hiwat_out = 2;
	int hiwat_in = 0;
	int free_fd;
	int nullfd = -1;
	int pipe_child_fd = -1;
	int pipe_parent_fd = -1;
	int dst;
	int nstack = 0;
	int *map_in_counts;
	int *map_out_to_in;
	int map_in_buf[128];
	int map_out_buf[128];

	// ftab isn't a sequence: it may be sparse. So we first iterate it in
	// arbitrary order to find the maximum integer key present, and reject any
	// unexpected non-integer keys. This gives us the high water mark for
	// output fds.

	// placeholder for iterator close object
	lua_pushnil(L);

	bool metaloop = pairs_start(L, ftab_idx, lua_absindex(L, -1), false);

	while (metaloop ? pairs_next(L) : lua_next(L, ftab_idx))
	{
		// stack: [ initfunc \ state \ ] \ key \ value
		lua_pop(L, 1);
		int isint = 0;
		int fd_out = lua_tointegerx(L, -1, &isint);
		if (!isint || fd_out < 0)
			return luaL_argerror(L, 1, "bad entry in files table, expected integer key >= 0");
		if (fd_out > hiwat_out)
			hiwat_out = fd_out;
	}

	// pop the iterator close object if any (which will close it)
	lua_pop(L, 1);

	// Allocate a buffer (which we will abandon to the garbage collector)
	// if there are more fds than we're prepared to accomodate on the stack.

	if (hiwat_out + 1 >= countof(map_out_buf))
		map_out_to_in = lua_newuserdata(L, (hiwat_out + 2) * sizeof(int));
	else
		map_out_to_in = map_out_buf;

	// The real input high-water-mark may be lower, but it suits us to force
	// it to be no smaller than the output one.

	hiwat_in = hiwat_out;

	// For each output fd, find its input fd and build the out->in map. Find
	// the input high-water-mark in the process. Also count how many stack
	// slots we'll need for "open" call filenames, while we're here.

	for (int i = 0; i <= hiwat_out; ++i)
	{
		enum lspawn_fa_type fa_type = FA_INVALID;
		int fd_in = -1;

		map_out_to_in[i] = -1;

		switch (lua_geti(L, ftab_idx, i))
		{
			// Entries in the file table must be either a file action, a Lua
			// FILE* which is treated as FA_INHERIT_FROM, or nil which is
			// treated as the default action (inherit for stdin/out/err, close
			// for all other fds).

			case LUA_TUSERDATA:
			{
				struct lspawn_fa_data *p = to_object(L, -1, &lspawn_file_action_meta);
				if (p)
				{
					fa_type = p->type;
					switch (fa_type)
					{
						case FA_INHERIT_FROM:
							fd_in = p->value1;
							break;
						case FA_OPEN:
							++nstack;
							break;
						case FA_INPIPE:
						case FA_OUTPIPE:
							if (pipe_child_fd >= 0)
								return luaL_argerror(L, 1, "bad entry in files table, only one pipe object permitted");
							pipe_parent_fd = make_pipe(L, fa_type, pipe_idx);
							pipe_child_fd = i;
							fd_in = pipe_parent_fd;
							break;
						default:
							break;
					}
				}
				else
				{
					luaL_Stream *sp = luaL_testudata(L, -1, LUA_FILEHANDLE);
					if (sp && sp->f)
					{
						fa_type = FA_INHERIT_FROM;
						fd_in = fileno(sp->f);
					}
				}
				break;
			}
			case LUA_TNIL:
				if (i < 3)
					fa_type = FA_INHERIT;
				else
					fa_type = FA_CLOSE;
				break;
		}
		if (fa_type == FA_INVALID)
			return luaL_argerror(L, 1, "bad entry in files table, expected file action or open file");
		if (fa_type == FA_INHERIT)
			fd_in = i;
		if (fd_in >= 0)
		{
			int fdflags;
			if (fcntl(fd_in, F_GETFD, &fdflags) < 0)
				return luaL_argerror(L, 1, "bad entry in files table, file not open");
			if (fd_in > hiwat_in)
				hiwat_in = fd_in;
			map_out_to_in[i] = fd_in;
		}
		lua_pop(L, 1);
	}

	if (hiwat_in + 1 >= countof(map_in_buf))
		map_in_counts = lua_newuserdata(L, (hiwat_in + 2) * sizeof(int));
	else
		map_in_counts = map_in_buf;

	for (int i = 0; i <= hiwat_in + 1; ++i)
		map_in_counts[i] = 0;
	for (int i = 0; i <= hiwat_out; ++i)
		map_in_counts[map_out_to_in[i]] += 1;

	// Find the lowest free fd to use for staging.

	free_fd = hiwat_in + 1;
	for (int i = 3; i <= hiwat_in; ++i)
	{
		if (map_in_counts[i] == 0 && map_out_to_in[i] < 0)
		{
			free_fd = i;
			break;
		}
	}

	// What we have at this point is a directed graph, possibly
	// with cycles, in which each node has inward degree of at
	// most 1. This implies that all cycles are disjoint (no node
	// or edge can participate in multiple cycles), and also that
	// if a node has outward degree greater than 1, only one of
	// those edges can be part of a cycle.
	//
	// An arc from a->b is represented by (map_out_to_in[b] == a)
	// map_in_counts[a] is the number of _outgoing_ edges from a.
	//
	// Therefore, we can process the graph as follows: first
	// process all nodes with outward degree of 0, removing links,
	// until none remain; at this point the graph consists only of
	// cycles, so choose any node, break the cycle at that node,
	// and repeat until done.

	for (;;)
	{
		int n = 0;
		for (dst = 0; dst <= hiwat_out; ++dst)
		{
			int src = map_out_to_in[dst];
			if ((map_in_counts[dst] == 0 && src >= 0)
				|| (src == dst))
			{
				err = spawn_file_actions_adddup2(facts, src, dst);
				if (unlikely(err != 0))
					return file_act_err(L, err);
				map_in_counts[src] -= 1;
				map_out_to_in[dst] = -1;
				++n;
			}
		}
		if (n)
			continue;
		for (dst = 0; dst <= hiwat_out; ++dst)
			if (map_out_to_in[dst] >= 0)
				break;
		if (dst > hiwat_out)
			break;
		int src = map_out_to_in[dst];
		err = spawn_file_actions_adddup2(facts, src, free_fd);
		if (unlikely(err != 0))
			return file_act_err(L, err);
		map_in_counts[src] = 0;
		map_in_counts[free_fd] = 1;
		map_out_to_in[dst] = free_fd;
	}

	// We're done with any input files that have not been copied
	// into the range up to hiwat_out; get rid of them now, to
	// help make space for any files to be opened.

	err = spawn_file_actions_addclosefrom_np(facts, hiwat_out + 1);
	if (unlikely(err != 0))
		return file_act_err(L, err);

	// Now process all other action types that don't copy from input
	// fds. But do foreground_fd first if it needs it.

	luaL_checkstack(L, 10 + nstack, "in process_files");

	if (foreground_fd >= 0)
	{
		enum lspawn_fa_type fa_type = FA_INVALID;
		struct lspawn_fa_data *p = NULL;
		switch (lua_geti(L, ftab_idx, foreground_fd))
		{
			case LUA_TNIL:
				if (foreground_fd > 2)
					return file_act_err(L, ENOTTY);
				break;

			case LUA_TUSERDATA:
			{
				p = to_object(L, -1, &lspawn_file_action_meta);
				if (p)
					fa_type = p->type;
				else
					fa_type = FA_INHERIT_FROM;
				break;
			}
		}
		switch (fa_type)
		{
			case FA_INVALID:
			case FA_CLOSE:
			case FA_NULL:
			case FA_INPIPE:
			case FA_OUTPIPE:
				return file_act_err(L, ENOTTY);

			case FA_INHERIT:
			case FA_INHERIT_FROM:
				err = 0;
				break;

			case FA_OPEN:
			{
				lua_getuservalue(L, -1);
				lua_rotate(L, -2, 1);
				const char *fname = lua_tostring(L, -2);
				err = spawn_file_actions_addopen(facts, foreground_fd, fname, p->value1, p->value2);
				break;
			}
			case FA_COPY_FROM:
				err = spawn_file_actions_adddup2(facts, foreground_fd, p->value1);
				break;
		}
		if (unlikely(err != 0))
			return file_act_err(L, err);
		err = spawn_file_actions_addsetpgrp_np(facts, foreground_fd);
		if (unlikely(err != 0))
			return file_act_err(L, err);

		lua_pop(L, 1);
	}

	for (int i = 0; i <= hiwat_out; ++i)
	{
		enum lspawn_fa_type fa_type = FA_INVALID;
		struct lspawn_fa_data *p = NULL;
		if (i == foreground_fd)
			continue;
		switch (lua_geti(L, ftab_idx, i))
		{
			case LUA_TNIL:
				switch (i)
				{
					case 0:
					case 1:
					case 2:
						fa_type = FA_INHERIT;
						break;
					default:
						fa_type =  FA_CLOSE;
				}
				break;

			case LUA_TUSERDATA:
			{
				p = to_object(L, -1, &lspawn_file_action_meta);
				if (p)
					fa_type = p->type;
				else
					fa_type = FA_INHERIT_FROM;
				break;
			}
		}
		switch (fa_type)
		{
			case FA_INVALID:
			case FA_INHERIT:
			case FA_INHERIT_FROM:
			case FA_INPIPE:
			case FA_OUTPIPE:
				// already handled above
				err = 0;
				break;
			case FA_CLOSE:
				err = spawn_file_actions_addclose(facts, i);
				break;
			case FA_NULL:
				if (nullfd < 0)
				{
					nullfd = i;
					err = spawn_file_actions_addopen(facts, i, "/dev/null", O_RDWR, 0);
				}
				else
					err = spawn_file_actions_adddup2(facts, nullfd, i);
				break;
			case FA_OPEN:
			{
				lua_getuservalue(L, -1);
				lua_rotate(L, -2, 1);
				const char *fname = lua_tostring(L, -2);
				err = spawn_file_actions_addopen(facts, i, fname, p->value1, p->value2);
				break;
			}
			case FA_COPY_FROM:
				err = spawn_file_actions_adddup2(facts, i, p->value1);
				break;
		}
		if (unlikely(err != 0))
			return file_act_err(L, err);

		lua_pop(L, 1);
	}

	return 0;
}


// Library proper begins here.

static int lspawn_call(lua_State *L);

static RegMeta lspawn_lib_meta = {
	.name = "lspawn library",
	.metamethods = (luaL_Reg[]){
		{ "__call", lspawn_call },
		{ NULL, NULL }
	}
};

//
// spawn {
//   exec = boolean,        -- if true, exec() the process (no return)
//   program = string,		-- required; filename of executable
//   args = table,			-- optional: argument strings
//   environ = table,       -- optional: { NAME = string, ... }
//   search_path = string or true,
//                          -- optional: colon-separated string
//                          -- true means use getenv("PATH")
//   signals = "preserve",  -- optional: leaves all signals untouched
//   signals = { ignore = {...},
//               block = {...},
//               preserve = {...} }
//                          -- optional: sub-args are sequences of name/num
//                          -- any signals not specified are reset to
//                          -- default and unblocked
//   reset_ids = true,      -- optional: reset euid/egid to ruid/rgid
//   process_group = true or integer,
//                          -- set process group to new or specified group
//   foreground_tty = integer or true,
//                          -- set as foreground pgroup on specified tty fd
//                          -- (fd is assumed 0 if value is not integer)
//   new_session = true,    -- set new session
//   clean_environ = true,  -- start with empty rather than inherited env
//   directory = string,    -- chdir after processing files
//   directory_before = string,
//                          -- chdir before processing files
//   files = { [n] = act, ... }
// }
//

#define SIDX_BASE		2

#define SIDX_PROGRAM	(SIDX_BASE + 0)
#define SIDX_ARGS		(SIDX_BASE + 1)
#define SIDX_ENVIRON	(SIDX_BASE + 2)
#define SIDX_FILES		(SIDX_BASE + 3)
#define SIDX_CHDIR		(SIDX_BASE + 4)
#define SIDX_CHDIR_BEFORE (SIDX_BASE + 5)
#define SIDX_SEARCHPATH	(SIDX_BASE + 6)
#define SIDX_SIGNALS	(SIDX_BASE + 7)

// everything after this can be dropped after copying to locals
#define SIDX__DROP1		(SIDX_BASE + 7)

#define SIDX_EXEC		(SIDX__DROP1 + 1)
#define SIDX_RESET_IDS	(SIDX__DROP1 + 2)
#define SIDX_SETSID		(SIDX__DROP1 + 3)
#define SIDX_PGROUP		(SIDX__DROP1 + 4)
#define SIDX_FOREGROUND	(SIDX__DROP1 + 5)
#define SIDX_CLEANENV	(SIDX__DROP1 + 6)

#define SIDX__LASTARG	(SIDX_BASE + 13)

static int
lspawn_call_do_args(lua_State *L, int idx)
{
	const struct {
		const char *keyname;
		int expected_type;
		bool required;
	} table_args[] = {
		[SIDX_PROGRAM]	=	{ "program",	LUA_TSTRING,	true },
		[SIDX_ARGS]		=	{ "args",		LUA_TTABLE,		false },
		[SIDX_ENVIRON]	=	{ "environ",	LUA_TTABLE,		false },
		[SIDX_FILES]	=	{ "files",		LUA_TTABLE,		false },
		[SIDX_CHDIR]	=	{ "directory",	LUA_TSTRING,	false },
		[SIDX_CHDIR_BEFORE]={ "directory_before", LUA_TSTRING, false },
		[SIDX_SEARCHPATH]=	{ "search_path", LUA_TNONE,		false },
		[SIDX_SIGNALS]	=	{ "signals",	LUA_TNONE,		false },
		[SIDX_EXEC]		=	{ "exec",		LUA_TBOOLEAN,	false },
		[SIDX_RESET_IDS]=	{ "reset_ids",	LUA_TBOOLEAN,	false },
		[SIDX_SETSID]	=	{ "new_session", LUA_TBOOLEAN,	false },
		[SIDX_PGROUP]	=	{ "process_group", LUA_TNONE,	false },
		[SIDX_FOREGROUND]=	{ "foreground_tty",	LUA_TNONE,	false },
		[SIDX_CLEANENV]	=	{ "clean_environ", LUA_TBOOLEAN, false }
	};

	int stackp = lua_gettop(L);

	for (int i = SIDX_BASE; i <= SIDX__LASTARG; ++i)
	{
		const char *expect = NULL;
		int argt = lua_getfield(L, idx, table_args[i].keyname);
		int expt = table_args[i].expected_type;
		int isint = 0;

		if (table_args[i].required || argt != LUA_TNIL)
		{
			switch (i)
			{
				case SIDX_SIGNALS:
					// must be the string "preserve" or a table
					if (argt == LUA_TSTRING)
					{
						if (strcmp(lua_tostring(L, -1), "preserve") != 0)
							return luaL_argerror(L, 1, "bad action string in signals field");
					}
					else if (argt != LUA_TTABLE)
						expect = "table or string";
					break;

				case SIDX_SEARCHPATH:
					if ((argt != LUA_TSTRING) && (argt != LUA_TBOOLEAN))
						expect = "string or boolean";
					break;

				case SIDX_PGROUP:
				case SIDX_FOREGROUND:
					lua_tointegerx(L, -1, &isint);
					if ((argt != LUA_TBOOLEAN) && !isint)
						expect = "integer or boolean";
					break;

				default:
					switch (expt)
					{
						case LUA_TTABLE:
							// args only needs to be indexable, while files and
							// environ need to support pairs()
							if ((i == SIDX_ARGS)
								? !is_indexable(L, -1)
								: !is_container(L, -1))
								expect = "table";
							break;
						case LUA_TNUMBER:	// actually requires "integer"
							lua_tointegerx(L, -1, &isint);
							if (!isint)
								expect = "integer";
							break;
						default:
							if (argt != expt)
								expect = lua_typename(L, expt);
							break;
					}
			}
		}
		if (expect)
		{
			char errbuf[128];
			snprintf(errbuf, sizeof(errbuf),
					 "bad %s field, expected %s",
					 table_args[i].keyname, expect);
			return luaL_argerror(L, 1, errbuf);
		}
		lua_copy(L, -1, i);
	}

	lua_settop(L, stackp);

	return 0;
}

// Actual main entry point.

static int
lspawn_call(lua_State *L)
{
	sigset_t	default_sigs;
	sigset_t	ignore_sigs;
	sigset_t	block_sigs;
	spawnattr_t	sa;
	spawn_file_actions_t *file_acts;
	short		spawn_attr_flags = (SPAWN_SETSIGDEF
									| SPAWN_SETSIGIGN
									| SPAWN_SETSIGMASK);
	pid_t		pgrp = 0;

	bool		do_exec = false;
	bool		inherit_env = true;
	int			foreground_fd = -1;
	const char *filename;
	const char *search_path = NULL;

	int			nargs = 0;
	int			nenvs = -1;		// -1 == use environ unchanged

	char	   *argvec[256];
	char	  **argv = argvec;
	char	  **envp;

	pid_t		child_pid;
	int			err;

	// We might push a bunch of things for args, env vars, files, etc. We aim
	// to keep 63 free slots minimum, and check for extension in indefinite
	// loops after 32 iterations. Include a chunk of slop here to avoid
	// unnecessary reallocs.

	luaL_checkstack(L, 100 + 2*SIDX__LASTARG, "in spawn");

	// Argument errors currently show as off-by-one in __call metamethods
	// (which this is). Jiggle the stack to compensate.
	if (verify_object_exact(L, 1, &lspawn_lib_meta))
		lua_remove(L, 1);

	if (lua_gettop(L) != 1)
		return luaL_argerror(L, 1, "exactly one arg expected");

	luaL_checktype(L, 1, LUA_TTABLE);

	// Process args and fill in all the stack slots.
	lua_settop(L, SIDX__LASTARG);
	lspawn_call_do_args(L, 1);

	do_exec = lua_toboolean(L, SIDX_EXEC);
	if (lua_toboolean(L, SIDX_RESET_IDS))
		spawn_attr_flags |= SPAWN_RESETIDS;
	if (lua_toboolean(L, SIDX_SETSID))
		spawn_attr_flags |= SPAWN_SETSID;
	if (lua_toboolean(L, SIDX_PGROUP))
	{
		pgrp = (pid_t) lua_tointeger(L, SIDX_PGROUP);
		spawn_attr_flags |= SPAWN_SETPGROUP;
	}
	if (lua_toboolean(L, SIDX_FOREGROUND))
		foreground_fd = lua_tointeger(L, SIDX_FOREGROUND);
	if (lua_toboolean(L, SIDX_CLEANENV))
		inherit_env = false;

	lua_settop(L, SIDX__DROP1);
#undef SIDX__DROP1

	// Note that slots for toclose objects must be allocated in order of
	// usage.
	// Reserve a slot for a possible toclose field for the envt iterator
	// Reserve a slot for the file_actions object
	// Reserve another 2 slots for possible FILE* for pipe objects
	// Slots above that are args/envs first, then other stuff

#define SIDX_ENVT_ITER	(SIDX_SIGNALS + 1)
#define SIDX_FILE_ACTS	(SIDX_SIGNALS + 2)
#define SIDX_PIPEOBJS	(SIDX_SIGNALS + 3)
#define SIDX_ARGBASE	(SIDX_SIGNALS + 5)

	lua_settop(L, SIDX_ARGBASE-1);

	filename = lua_tostring(L, SIDX_PROGRAM);
	if (lua_type(L, SIDX_SEARCHPATH) == LUA_TBOOLEAN
		&& lua_toboolean(L, SIDX_SEARCHPATH))
	{
		search_path = getenv("PATH");
		if (!search_path)
			search_path = _PATH_DEFPATH;
	}
	else
		search_path = lua_tostring(L, SIDX_SEARCHPATH);

	sigfillset(&default_sigs);
	sigemptyset(&ignore_sigs);
	sigemptyset(&block_sigs);

	// If a string, it must be "preserve", already checked above
	if (lua_type(L, SIDX_SIGNALS) == LUA_TSTRING)
		spawn_attr_flags &= ~(SPAWN_SETSIGDEF | SPAWN_SETSIGIGN | SPAWN_SETSIGMASK);
	else if (!lua_isnil(L, SIDX_SIGNALS))
		process_signals(L, SIDX_SIGNALS, &default_sigs, &ignore_sigs, &block_sigs);

	nargs = process_args(L, SIDX_PROGRAM, SIDX_ARGS);
	nenvs = process_envs(L, SIDX_ENVIRON, SIDX_ENVT_ITER, inherit_env);

	if (nargs + nenvs + 2 > countof(argvec))
		argv = lua_newuserdata(L, (nargs + nenvs + 2) * sizeof(const char *));

	file_acts = lspawn_file_actions_new(L);
	lua_replace(L, SIDX_FILE_ACTS);
	make_auto_closevar(L, SIDX_FILE_ACTS);

	if (!lua_isnil(L, SIDX_CHDIR_BEFORE))
	{
		err = spawn_file_actions_addchdir(file_acts,
										  lua_tostring(L, SIDX_CHDIR_BEFORE));
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	if (lua_isnil(L, SIDX_FILES))
		process_files_default(L, file_acts, foreground_fd);
	else
		process_files(L, SIDX_FILES, SIDX_PIPEOBJS, file_acts, foreground_fd);

	if (!lua_isnil(L, SIDX_CHDIR))
	{
		err = spawn_file_actions_addchdir(file_acts,
										  lua_tostring(L, SIDX_CHDIR));
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	vectorize_args(L, (const char **) argv, SIDX_ARGBASE, nargs, nenvs);
	if (nenvs >= 0)
		envp = argv + nargs + 1;
	else
		envp = NULL;

	spawnattr_init(&sa);
	if (spawn_attr_flags & SPAWN_SETPGROUP)
		spawnattr_setpgroup(&sa, pgrp);
	spawnattr_setsigdefault(&sa, &default_sigs);
	spawnattr_setsigignore_np(&sa, &ignore_sigs);
	spawnattr_setsigmask(&sa, &block_sigs);
	spawnattr_setflags(&sa, spawn_attr_flags);

	// finally!

	if (do_exec)
	{
		spawnexecP(filename,
				   search_path,
				   file_acts,
				   &sa,
				   argv,
				   envp);
		// should never be reached
		_exit(127);
	}
	else
		err = spawnP(&child_pid,
					 filename,
					 search_path,
					 file_acts,
					 &sa,
					 argv,
					 envp);

	spawnattr_destroy(&sa);

	auto_closevar_compat(L, SIDX_PIPEOBJS+1);
	auto_closevar_compat(L, SIDX_FILE_ACTS);

	if (err)
	{
		auto_closevar_compat(L, SIDX_PIPEOBJS);

		lua_pushnil(L);
		lua_pushstring(L, strerror(err));
		lua_pushinteger(L, err);
		return 3;
	}

	lua_pushinteger(L, child_pid);

	if (lua_type(L, SIDX_PIPEOBJS) != LUA_TNIL)
	{
		auto_closevar_release(L, SIDX_PIPEOBJS);
		SPStream *sp = luaL_checkudata(L, -1, LUA_FILEHANDLE);
		sp->pid = child_pid;
		return 2;
	}

	return 1;
}

// There may be signals above NSIG; but if so their names are not in
// sys_signame. Such signals can be referred to only by number or by
// defintions found elsewhere, e.g. in a POSIX module.

static void
make_signal_table(lua_State *L)
{
	lua_createtable(L, NSIG, NSIG);
	for (int i = 1; i < NSIG; ++i)
	{
		lua_pushfstring(L, "SIG%s", sys_signame[i]);
		lua_pushvalue(L, -1);
		lua_pushinteger(L, i);
		lua_rawset(L, -4);
		lua_rawseti(L, -2, i);
	}
}

EXPORTED
int
luaopen_lspawn(lua_State *L)
{
	lua_settop(L, 0);

	// metatables for FA objects
	make_metatable(L, &lspawn_file_action_meta, 0);
	make_metatable(L, &lspawn_file_actions_meta, 0);
	lua_settop(L, 0);

	lua_newtable(L);
	lua_pushstring(L, "=");
	make_signal_table(L);
	make_metatable(L, &lspawn_lib_meta, 2);
	lua_setmetatable(L, 1);

	luaL_setfuncs(L, lspawn_funcs, 0);
	for (luaL_Reg *rp = lspawn_objs; rp->name != NULL; ++rp)
	{
		rp->func(L);
		lua_setfield(L, -2, rp->name);
	}

	return 1;
}
