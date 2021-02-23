/*
 * lspawn.c
 *
 * Copyright 2020-2021 Andrew Gierth.
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
#include <stdint.h>
#include <math.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <paths.h>

#include <sys/wait.h>

#include "myspawn.h"
#include "simple_class.h"
#include "auto_close.h"
#include "utils.h"

extern const char **environ;

#define DECONST(t_,v_) ((t_)(uintptr_t)(const void *)(v_))

// Lua version cruft

#if LUA_VERSION_NUM < 503
#error This code requires Lua 5.3 or later
#endif

#if LUA_VERSION_NUM < 504
#define luaL_pushfail(L_) lua_pushnil(L_)
#endif

enum spawn_op {
	LSPAWN_CALL,
	LSPAWN_WAIT,
	LSPAWN_READFROM,
	LSPAWN_WRITETO,
};

//==========================================================================

// This ought to be luaL_execresult, but in 5.4 that is broken due to a very
// misguided attempt at windows compatibility. But while we're here, fix the
// exit code.

static int
my_execresult(lua_State *L, int status)
{
	bool		exited = WIFEXITED(status);
	bool		signaled = WIFSIGNALED(status);
	lua_pushboolean(L, exited && (WEXITSTATUS(status) == 0));
	lua_pushstring(L, signaled ? "signal" : "exit");
	lua_pushinteger(L, status);
	return 3;
}

static int
my_errresult(lua_State *L, int err)
{
	luaL_pushfail(L);
	lua_pushstring(L, strerror(err));
	lua_pushinteger(L, err);
	return 3;
}

static int
do_waitpid(lua_State *L, pid_t cpid)
{
	int			status = -1;
	pid_t		rpid;

	do
		rpid = waitpid(cpid, &status, 0);
	while (rpid == -1 && errno == EINTR);

	if (rpid == cpid)
		return my_execresult(L, status);
	else if (rpid >= 0)
		return luaL_error(L, "waitpid returned wrong pid: expected %ld got %ld",
						  (long)cpid, (long)rpid);
	else
		return my_errresult(L, errno);
}

//
// Lua container convenience functions.
//
// We define a value as "indexable" if it supports gettable, etc.; this is
// true if it is a table or has a __index metavalue.
//
// We define a value as a "container" if it supports pairs() (not necessarily
// ipairs) in a way that is likely to work; i.e. it is either a table with no
// __index metavalue or it has a __pairs metamethod.
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
	if (luaL_getmetafield(L, nd, "__pairs") != LUA_TNIL)
	{
		lua_pop(L, 1);
		return true;
	}
	if (lua_type(L, nd) == LUA_TTABLE)
	{
		if (luaL_getmetafield(L, nd, "__index") == LUA_TNIL)
			return true;
		lua_pop(L, 1);
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

//==========================================================================

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
	char		extras[64];

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
	int			flags = lspawn_fileflags(L, 2);
	int			perms = luaL_optinteger(L, 3, 0666);

	luaL_checkstring(L, 1);

	lspawn_fa_new(L, FA_OPEN, flags, perms);
	lua_pushvalue(L, 1);
	lua_setuservalue(L, -2);
	return 1;
}

static int
lspawn_fa_make_inherit_from(lua_State *L)
{
	int			isint = 0;
	int			fd = lua_tointegerx(L, 1, &isint);
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
	int			fd = luaL_checkinteger(L, 1);

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

static luaL_Reg lspawn_fa_funcs[] = {
	{ "inherit_from", lspawn_fa_make_inherit_from },
	{ "copy_from", lspawn_fa_make_copy_from },
	{ "open", lspawn_fa_make_open },
	{ NULL, NULL }
};

// And these functions are called to create their objects

static luaL_Reg lspawn_fa_objs[] = {
	{ "null", lspawn_fa_make_null },
	{ "close", lspawn_fa_make_close },
	{ "inherit", lspawn_fa_make_inherit },
	{ "input_pipe", lspawn_fa_make_inpipe },
	{ "output_pipe", lspawn_fa_make_outpipe },
	{ NULL, NULL }
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


// File actions processing

static int
file_act_err(lua_State *L, int err)
{
	return luaL_error(L, "error adding file action: %s", strerror(err));
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
		return do_waitpid(L, s->pid);
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
	int			fds[2];
	int			parent_end;
	int			child_end;
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
			  spawn_file_actions_t *facts,
			  int foreground_fd)
{
	int			err;
	int			hiwat_out = 2;
	int			hiwat_in = 0;
	int			free_fd;
	int			nullfd = -1;
	int			pipe_child_fd = -1;
	int			pipe_parent_fd = -1;
	int			dst;
	int			tmpt_idx = 0;
	int		   *map_in_counts;
	int		   *map_out_to_in;
	int			map_in_buf[128];
	int			map_out_buf[128];

	// ftab isn't a sequence: it may be sparse. So we first iterate it in
	// arbitrary order to find the maximum integer key present, and reject any
	// unexpected non-integer keys. This gives us the high water mark for
	// output fds.

	// placeholder for temp table if needed
	lua_pushnil(L);
	tmpt_idx = lua_absindex(L, -1);

	// placeholder for iterator close object
	lua_pushnil(L);

	bool metaloop = pairs_start(L, ftab_idx, lua_absindex(L, -1), false);

	if (metaloop)
	{
		lua_createtable(L, 4, 1);
		lua_replace(L, tmpt_idx);
	}

	while (metaloop ? pairs_next(L) : lua_next(L, ftab_idx))
	{
		// stack: [ initfunc \ state \ ] \ key \ value
		int isint = 0;
		int fd_out = lua_tointegerx(L, -2, &isint);
		if (!isint || fd_out < 0)
			return luaL_argerror(L, 1, "bad entry in files table, expected integer key >= 0");
		if (fd_out > hiwat_out)
			hiwat_out = fd_out;
		if (metaloop)
			lua_rawseti(L, tmpt_idx, fd_out);
		else
			lua_pop(L, 1);
	}

	// pop the iterator close object if any (which will close it)
	lua_pop(L, 1);

	if (metaloop)
		ftab_idx = tmpt_idx;

	// Allocate a buffer (which we will abandon to the garbage collector)
	// if there are more fds than we're prepared to accomodate on the stack.

	if (hiwat_out + 1 >= countof(map_out_buf))
		map_out_to_in = lua_newuserdata(L, (unsigned)(hiwat_out + 2) * sizeof(int));
	else
		map_out_to_in = map_out_buf;

	// The real input high-water-mark may be lower, but it suits us to force
	// it to be no smaller than the output one.

	hiwat_in = hiwat_out;

	// For each output fd, find its input fd and build the out->in map. Find
	// the input high-water-mark in the process.

	for (int i = 0; i <= hiwat_out; ++i)
	{
		enum lspawn_fa_type fa_type = FA_INVALID;
		int		fd_in = -1;

		map_out_to_in[i] = -1;

		switch (lua_rawgeti(L, ftab_idx, i))
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
						case FA_INPIPE:
						case FA_OUTPIPE:
							if (pipe_child_fd >= 0)
								return luaL_argerror(L, 1, "bad entry in files table, only one pipe object permitted");
							if (pipe_idx == 0)
								return luaL_argerror(L, 1, "bad entry in files table, pipes not permitted with wait or exec");
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
			if (fcntl(fd_in, F_GETFD, 0) == -1)
				return luaL_argerror(L, 1, "bad entry in files table, file not open");
			if (fd_in > hiwat_in)
				hiwat_in = fd_in;
			map_out_to_in[i] = fd_in;
		}
		lua_pop(L, 1);
	}

	if (hiwat_in + 1 >= countof(map_in_buf))
		map_in_counts = lua_newuserdata(L, (unsigned)(hiwat_in + 2) * sizeof(int));
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

	if (foreground_fd >= 0)
	{
		enum lspawn_fa_type fa_type = FA_INVALID;
		struct lspawn_fa_data *p = NULL;
		switch (lua_rawgeti(L, ftab_idx, foreground_fd))
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
				const char *fname = lua_tostring(L, -1);
				err = spawn_file_actions_addopen(facts, foreground_fd, fname, p->value1, (mode_t) p->value2);
				// addopen copied the filename so we don't need to keep it on stack
				lua_pop(L, 1);
				break;
			}

			case FA_COPY_FROM:
				return luaL_argerror(L, 1, "foreground_tty must not refer to a copy_from");
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
		switch (lua_rawgeti(L, ftab_idx, i))
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
				const char *fname = lua_tostring(L, -1);
				err = spawn_file_actions_addopen(facts, i, fname, p->value1, (mode_t) p->value2);
				// addopen copied the filename so we don't need to keep it on stack
				lua_pop(L, 1);
				break;
			}
			case FA_COPY_FROM:
				err = 0;
				break;
		}
		if (unlikely(err != 0))
			return file_act_err(L, err);

		lua_pop(L, 1);
	}

	for (int i = 0; i <= hiwat_out; ++i)
	{
		struct lspawn_fa_data *p = NULL;
		if (i == foreground_fd)
			continue;
		if (lua_rawgeti(L, ftab_idx, i) == LUA_TUSERDATA)
		{
			p = to_object(L, -1, &lspawn_file_action_meta);
			if (p && p->type == FA_COPY_FROM)
			{
				err = spawn_file_actions_adddup2(facts, p->value1, 1);
				if (unlikely(err != 0))
					return file_act_err(L, err);
			}
		}

		lua_pop(L, 1);
	}

	// pop the temp table slot
	lua_pop(L, 1);
	return 0;
}

// If no table of file actions was passed in, do the default thing.

static void
process_default_pipe(lua_State *L, int pipe_idx,
					 spawn_file_actions_t *facts,
					 enum spawn_op context)
{
	enum lspawn_fa_type fa_type = FA_INVALID;
	int			pipe_child_fd = -1;
	int			pipe_parent_fd;
	int			err;

	switch (context)
	{
		case LSPAWN_READFROM:
			pipe_child_fd = 1;
			fa_type = FA_OUTPIPE;
			break;
		case LSPAWN_WRITETO:
			pipe_child_fd = 0;
			fa_type = FA_INPIPE;
			break;
		default:
			return;
	}

	if (pipe_idx == 0)
		luaL_error(L, "pipes not permitted with wait or exec");

	pipe_parent_fd = make_pipe(L, fa_type, pipe_idx);

	err = spawn_file_actions_adddup2(facts, pipe_parent_fd, pipe_child_fd);
	if (unlikely(err != 0))
		file_act_err(L, err);

	if (context == LSPAWN_READFROM)
	{
		err = spawn_file_actions_addopen(facts, 0, "/dev/null", O_RDWR, 0);
		if (unlikely(err != 0))
			file_act_err(L, err);
	}
}

static int
process_files_default(lua_State *L, int pipe_idx,
					  spawn_file_actions_t *facts,
					  enum spawn_op context,
					  int foreground_fd)
{
	process_default_pipe(L, pipe_idx, facts, context);

	if (foreground_fd >= 0)
	{
		if (foreground_fd > 2)
			return file_act_err(L, ENOTTY);

		int err = spawn_file_actions_addsetpgrp_np(facts, foreground_fd);
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	int	err = spawn_file_actions_addclosefrom_np(facts, 3);
	if (unlikely(err != 0))
		return file_act_err(L, err);

	return 0;
}


//==========================================================================

// Arguments and environment

//
// Given absolute stack indices of the command string and arg table, push (at
// the current stack top) string values for argv[0..n-1] and return n. Note
// that nothing is pushed for the terminating null, that's handled later.
//

static int
process_args(lua_State *L, int cmd_idx, int argt_idx)
{
	bool		have_argv0 = false;
	int			nargs = 1;

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
// Note that we may clobber the envt_idx stack slot with a temp copy.
//

static int
process_envs(lua_State *L, int envt_idx, int iter_idx, bool inherit)
{
	int			nenvs = 0;
	bool		have_env = !lua_isnil(L, envt_idx);

	if (inherit && !have_env)
		return -1;

	if (inherit && environ)
	{
		// envt_idx has already been checked for container-ness
		if (luaL_getmetafield(L, envt_idx, "__pairs") != LUA_TNIL)
		{
			lua_pop(L, 1);
			lua_createtable(L, 0, 64);
			int tmp_idx = lua_absindex(L, -1);

			bool metaloop = pairs_start(L, envt_idx, iter_idx, true);

			while (metaloop ? pairs_next(L) : lua_next(L, envt_idx))
			{
				lua_pushvalue(L, -2);
				lua_insert(L, -2);
				lua_rawset(L, tmp_idx);
			}

			lua_replace(L, envt_idx);
		}
		for (int i = 0; environ[i]; ++i)
		{
			const char *name = environ[i];
			const char *eq = strchr(name, '=');
			int argt;

			if (!eq || name == eq)
				continue;

			lua_pushlstring(L, name, (size_t)(eq - name));
			argt = lua_gettable(L, envt_idx);
			lua_pop(L, 1);

			if (argt == LUA_TNIL)
			{
				// push these as light udata rather than as strings,
				// to avoid the overhead of copying/interning them
				lua_pushlightuserdata(L, DECONST(void *, name));
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
			lua_pushvalue(L, lua_upvalueindex(2));
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
	int			i = 0;

	for (; nargs > 0; --nargs, ++idx)
		argv[i++] = lua_tostring(L, idx);
	argv[i++] = NULL;

	if (nenvs < 0)
		return;

	for (; nenvs > 0; --nenvs, ++idx)
	{
		const char *ptr = lua_tostring(L, idx);
		argv[i++] = ptr ? ptr : lua_touserdata(L, idx);
	}
	argv[i++] = NULL;
}

//==========================================================================

// Signal handling

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

// It beggars belief that this ghodawful hack is needed.

extern int __isthreaded;

static int
safe_setsigmask(int how,
				sigset_t * __restrict newmask,
				sigset_t * __restrict oldmask)
{
	if (__isthreaded)
		return pthread_sigmask(how, newmask, oldmask);
	else
		return sigprocmask(how, newmask, oldmask);
}

// Iterate over a sigset.

static int
iterate_sigs(sigset_t *sigs, int sig)
{
	for (;;)
		switch (sigismember(sigs, ++sig))
		{
			case -1:	return -1;
			case  0:	continue;
			default:	return sig;
		}
}

// Given a sequence at sigt_idx[name], add the signals in it to sigs_add and
// remove them from sigs_del. Signals can be specified by name or number.

static bool
process_siglist(lua_State *L,
				int sigt_idx, const char *name,
				sigset_t *sigs_add, sigset_t *sigs_del)
{
	int			argt;

	if (lua_getfield(L, sigt_idx, name) == LUA_TNIL)
	{
		lua_pop(L, 1);
		return false;
	}

	if (!is_indexable(L, -1))
		return luaL_error(L, "bad signal %s table, expected indexable value", name);

	for (int i = 1; (argt = lua_geti(L, -1, i)) != LUA_TNIL; ++i)
	{
		int		isint = 0;
		int		rc = 0;
		int		sig;

		if (argt != LUA_TNUMBER)
			argt = lua_rawget(L, lua_upvalueindex(3));
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
	lua_pop(L, 2);
	return true;
}

// process the signals argument.

static int
process_signals(lua_State *L,
				int sigt_idx,
				sigset_t *default_sigs,
				sigset_t *ignore_sigs,
				sigset_t *block_sigs,
				sigset_t *wait_ignore_sigs,
				sigset_t *wait_block_sigs)
{
	sigset_t	tmp_wait_ignore;
	sigset_t	tmp_wait_block;

	sigt_idx = lua_absindex(L, sigt_idx);

	sigemptyset(&tmp_wait_ignore);
	sigemptyset(&tmp_wait_block);

	// initially default_sigs is full and the others empty.
	// ignored sigs are added to ignore_sigs and removed from
	// default_sigs; preserved sigs are removed from default_sigs;
	// blocked sigs added to block_sigs.

	process_siglist(L, sigt_idx, "ignore", ignore_sigs, default_sigs);
	process_siglist(L, sigt_idx, "block", block_sigs, NULL);
	process_siglist(L, sigt_idx, "preserve", NULL, default_sigs);

	if (wait_block_sigs
		&& process_siglist(L, sigt_idx, "block_waiting", &tmp_wait_block, NULL))
		*wait_block_sigs = tmp_wait_block;

	if (wait_ignore_sigs
		&& process_siglist(L, sigt_idx, "ignore_waiting", &tmp_wait_ignore, NULL))
		*wait_ignore_sigs = tmp_wait_ignore;

	return 0;
}

//==========================================================================

// Resource handling

static uint64_t
u64_mul(lua_State *L, uint64_t a, uint64_t b)
{
	uint64_t	result;
	bool		overflow;

#if __has_builtin(__builtin_mul_overflow)
	overflow = __builtin_mul_overflow(a, b, &result);
#else
#error Missing overflow intrinsic
#endif

	if (overflow)
		luaL_error(L, "numeric overflow in argument");
	return result;
}

typedef bool (rv_mult)(char *ptr, uint64_t *mult);

static bool
rvm_size(char *ptr, uint64_t *multp)
{
	uint64_t	mult = 1;

	switch (*ptr)
	{
		case 0:
			break;
		case 'b': case 'B':
			mult = 512;
			break;
		case 'k': case 'K':
			mult = (1ULL << 10);
			break;
		case 'm': case 'M':
			mult = (1ULL << 20);
			break;
		case 'g': case 'G':
			mult = (1ULL << 30);
			break;
		case 't': case 'T':
			mult = (1ULL << 40);
			break;
		default:
			return false;
	}

	*multp = mult;
	return true;
}

static bool
rvm_time(char *ptr, uint64_t *multp)
{
	uint64_t	mult = 1;

	switch (*ptr)
	{
		case 0:
			break;
		case 's': case 'S':	/* seconds */
			break;
		case 'm': case 'M':	/* minutes */
			mult = 60U;
			break;
		case 'h': case 'H':	/* hours */
			mult = 60UL * 60UL;
			break;
		case 'd': case 'D':	/* days */
			mult = 60UL * 60UL * 24UL;
			break;
		case 'w': case 'W':	/* weeks */
			mult = 60UL * 60UL * 24UL * 7UL;
			break;
		case 'y': case 'Y':	/* 365-day years */
			mult = 60UL * 60UL * 24UL * 365UL;
			break;
		default:
			return false;
	}

	*multp = mult;
	return true;
}

static rlim_t
rv_from_string(lua_State *L,
			   const char *ptr,
			   rv_mult *func)
{
	char	   *endptr = NULL;
	bool		valid = false;
	uint64_t	mult = 1;

	errno = 0;

	uint64_t	sz = strtoull(ptr, &endptr, 0);

	if (endptr != ptr && errno == 0)
	{
		if (func && *endptr != 0)
			valid = func(endptr, &mult);
		else if (*endptr == 0)
			valid = true;
	}

	sz = u64_mul(L, sz, mult);

	if (!valid || sz > INT64_MAX || (rlim_t)sz == RLIM_INFINITY)
		luaL_error(L, "invalid resource limit value");

	return (rlim_t) sz;
}

static bool
is_infinity_string(lua_State *L, int idx)
{
	const char *p = lua_tostring(L, idx);
	if (p
		&& (p[0] == 'I' || p[0] == 'i')
		&& (p[1] == 'N' || p[1] == 'n')
		&& (p[2] == 'F' || p[2] == 'f'))
		return true;
	return false;
}

static struct rlimit_entry {
	rv_mult *mult;
	const char *names[4];
} rlimit_table[RLIM_NLIMITS] = {
#ifdef RLIMIT_AS
	[RLIMIT_AS]		= { rvm_size, {"as","vmemoryuse","vmem"} },
#endif
#ifdef RLIMIT_CORE
	[RLIMIT_CORE]	= { rvm_size, {"core","coredumpsize"} },
#endif
#ifdef RLIMIT_CPU
	[RLIMIT_CPU]	= { rvm_time, {"cpu","cputime"} },
#endif
#ifdef RLIMIT_DATA
	[RLIMIT_DATA]	= { rvm_size, {"data","datasize"} },
#endif
#ifdef RLIMIT_FSIZE
	[RLIMIT_FSIZE]	= { rvm_size, {"fsize","filesize"} },
#endif
#ifdef RLIMIT_KQUEUES
	[RLIMIT_KQUEUES]= { NULL, {"kqueues"} },
#endif
#ifdef RLIMIT_MEMLOCK
	[RLIMIT_MEMLOCK]= { rvm_size, {"memlock","memorylocked"} },
#endif
#ifdef RLIMIT_NOFILE
	[RLIMIT_NOFILE]	= { NULL, {"nofile","openfiles"} },
#endif
#ifdef RLIMIT_NPROC
	[RLIMIT_NPROC]	= { NULL, {"nproc","maxproc"} },
#endif
#ifdef RLIMIT_NPTS
	[RLIMIT_NPTS]	= { NULL, {"npts","pseudoterminals"} },
#endif
#ifdef RLIMIT_RSS
	[RLIMIT_RSS]	= { rvm_size, {"rss","memoryuse"} },
#endif
#ifdef RLIMIT_SBSIZE
	[RLIMIT_SBSIZE]	= { rvm_size, {"sbsize"} },
#endif
#ifdef RLIMIT_STACK
	[RLIMIT_STACK]	= { rvm_size, {"stack","stacksize"} },
#endif
#ifdef RLIMIT_SWAP
	[RLIMIT_SWAP]	= { rvm_size, {"swap","swapuse"} },
#endif
#ifdef RLIMIT_UMTXP
	[RLIMIT_UMTXP]	= { NULL, {"umtxp"} },
#endif
};

static inline bool
is_rvaltype(int vtype)
{
	return vtype == LUA_TBOOLEAN || vtype == LUA_TNUMBER || vtype == LUA_TSTRING;
}

// resource value can be a number (to set both soft and hard limits),
// boolean false = 0 for certain limits, true or infinity = infinity,
// string "infinity" = infinity, or an indexable with cur= max=.

static rlim_t
rv_getvalue(lua_State *L, int res, int vtype)
{
	switch (vtype)
	{
		case LUA_TBOOLEAN:
			if (lua_toboolean(L, -1))
				return RLIM_INFINITY;
			else
				return 0;

		case LUA_TSTRING:
			if (is_infinity_string(L, -1))
				return RLIM_INFINITY;
			return rv_from_string(L, lua_tostring(L, -1), rlimit_table[res].mult);

		case LUA_TNUMBER:
		{
			int isint = 0;
			lua_Integer val = lua_tointegerx(L, -1, &isint);
			if (isint)
				return val;
			lua_Number nval = lua_tonumber(L, -1);
			if (nval > 0 && isinf(nval))
				return RLIM_INFINITY;
			if (nval < 0
				|| (nval > 0 && -nval <= (INT64_MIN)))
				luaL_argerror(L, 1, "bad value in resources table");
			return llrint(nval);
		}

		default:
			luaL_argerror(L, 1, "bad value in resources table");
	}
}

static bool
process_resources(lua_State *L, int idx, bool *rlim_set, struct rlimit *rlims)
{
	bool		result = false;

	idx = lua_absindex(L, idx);

	for (int res = 0; res < RLIM_NLIMITS; ++res)
	{
		if (rlimit_table[res].names[0] == NULL)
			continue;

		int		vtype = lua_geti(L, idx, res);

		for (int i = 0;
			 vtype == LUA_TNIL
				 && i < countof(rlimit_table[res].names)
				 && rlimit_table[res].names[i];
			 ++i)
		{
			lua_pop(L, 1);
			vtype = lua_getfield(L, idx, rlimit_table[res].names[i]);
		}

		if (is_rvaltype(vtype))
		{
			rlim_t limval = rv_getvalue(L, res, vtype);
			rlims[res].rlim_cur = limval;
			rlims[res].rlim_max = limval;
			rlim_set[res] = result = true;
		}
		else if (is_indexable(L, -1))
		{
			int	vtype1 = lua_getfield(L, -1, "cur");
			if (vtype1 == LUA_TNIL)
			{
				lua_pop(L, 1);
				vtype1 = lua_getfield(L, -1, "rlim_cur");
			}

			int	vtype2 = lua_getfield(L, -2, "max");
			if (vtype2 == LUA_TNIL)
			{
				lua_pop(L, 1);
				vtype2 = lua_getfield(L, -2, "rlim_max");
			}

			rlim_t val_cur;
			rlim_t val_max;
			bool cur_set = false;
			bool max_set = false;
			if (is_rvaltype(vtype2))
			{
				val_max = rv_getvalue(L, res, vtype2);
				max_set = true;
			}
			else if (vtype2 != LUA_TNIL)
				luaL_argerror(L, 1, "bad value in resources table");
			lua_pop(L, 1);
			if (is_rvaltype(vtype1))
			{
				val_cur = rv_getvalue(L, res, vtype1);
				cur_set = true;
			}
			else if (vtype1 != LUA_TNIL)
				luaL_argerror(L, 1, "bad value in resources table");
			lua_pop(L, 1);
			if (cur_set != max_set)
				if (getrlimit(res, &rlims[res]) < 0)
					luaL_error(L, "could not fetch resource limit (res %d, error %d)", res, errno);
			if (cur_set)
				rlims[res].rlim_cur = val_cur;
			if (max_set)
				rlims[res].rlim_max = val_max;
			if (cur_set || max_set)
				rlim_set[res] = result = true;
		}
		else if (vtype != LUA_TNIL)
			luaL_argerror(L, 1, "bad value in resources table");
		lua_pop(L, 1);
	}

	return result;
}


//==========================================================================

// Actual main library code.

//
// spawn {
//   exec = boolean,        -- if true, exec() the process (no return)
//   wait = boolean,		-- if true, waitpid() for the child
//   verbose = string,      -- prefix for verbose errors
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
//   chroot = string,       -- chroot after processing files
//   chroot_before = string,
//                          -- chroot before processing files
//   resources = { [n] = {...} }
//   jail_before = integer,
//   jail_after = integer,
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
#define SIDX_CHROOT		(SIDX_BASE + 6)
#define SIDX_CHROOT_BEFORE (SIDX_BASE + 7)
#define SIDX_SEARCHPATH	(SIDX_BASE + 8)
#define SIDX_VERBOSE	(SIDX_BASE + 9)

// everything after this can be dropped after copying to locals
#define SIDX__DROP1		(SIDX_BASE + 9)

#define SIDX_SIGNALS	(SIDX__DROP1 + 1)
#define SIDX_EXEC		(SIDX__DROP1 + 2)
#define SIDX_WAIT		(SIDX__DROP1 + 3)
#define SIDX_RESET_IDS	(SIDX__DROP1 + 4)
#define SIDX_SETSID		(SIDX__DROP1 + 5)
#define SIDX_PGROUP		(SIDX__DROP1 + 6)
#define SIDX_FOREGROUND	(SIDX__DROP1 + 7)
#define SIDX_CLEANENV	(SIDX__DROP1 + 8)
#define SIDX_JAIL_BEFORE (SIDX__DROP1 + 9)
#define SIDX_JAIL_AFTER (SIDX__DROP1 + 10)
#define SIDX_RESOURCES	(SIDX__DROP1 + 11)

#define SIDX__LASTARG	(SIDX__DROP1 + 11)

typedef const char *(validate_func)(lua_State *L, int typ);

static const char *v_boolean(lua_State *L, int typ)
{
	return (typ == LUA_TBOOLEAN) ? NULL : "string";
}
static const char *v_string(lua_State *L, int typ)
{
	return (typ == LUA_TSTRING) ? NULL : "string";
}
static const char *v_indexable(lua_State *L, int typ)
{
	return is_indexable(L, -1) ? NULL : "table or indexable object";
}
static const char *v_container(lua_State *L, int typ)
{
	return is_container(L, -1) ? NULL : "table or container";
}
static const char *v_integer(lua_State *L, int typ)
{
	int isint = 0;
	lua_tointegerx(L, -1, &isint);
	return isint ? NULL : "integer";
}
static const char *v_int_or_bool(lua_State *L, int typ)
{
	int isint = 0;
	lua_tointegerx(L, -1, &isint);
	return (typ == LUA_TBOOLEAN || isint) ? NULL : "integer or boolean";
}
static const char *v_str_or_bool(lua_State *L, int typ)
{
	return (typ == LUA_TBOOLEAN || typ == LUA_TSTRING) ? NULL : "string or boolean";
}

// string "preserve" or a table
static const char *v_s_signals(lua_State *L, int typ)
{
	if (typ != LUA_TTABLE
		&& (typ != LUA_TSTRING
			|| strcmp(lua_tostring(L, -1), "preserve") != 0))
		return "table or string 'preserve'";
	return NULL;
}

static const struct {
	const char *keyname;
	validate_func *isvalid;
	bool required;
} lspawn_args[] = {
	[SIDX_PROGRAM]		=	{ "program",		v_string,		true },
	[SIDX_ARGS]			=	{ "args",			v_indexable,	false },
	[SIDX_ENVIRON]		=	{ "environ",		v_container,	false },
	[SIDX_FILES]		=	{ "files",			v_container,	false },
	[SIDX_CHDIR]		=	{ "directory",		v_string,		false },
	[SIDX_CHDIR_BEFORE]	=	{ "directory_before", v_string,		false },
	[SIDX_CHROOT]		=	{ "chroot",			v_string,		false },
	[SIDX_CHROOT_BEFORE]=	{ "chroot_before", 	v_string, 		false },
	[SIDX_SEARCHPATH]	=	{ "search_path", 	v_str_or_bool,	false },
	[SIDX_SIGNALS]		=	{ "signals",		v_s_signals,	false },
	[SIDX_RESOURCES]	=	{ "resources",		v_indexable,	false },
	[SIDX_EXEC]			=	{ "exec",			v_boolean,		false },
	[SIDX_WAIT]			=	{ "wait",			v_boolean,		false },
	[SIDX_RESET_IDS]	=	{ "reset_ids",		v_boolean,		false },
	[SIDX_SETSID]		=	{ "new_session", 	v_boolean,		false },
	[SIDX_PGROUP]		=	{ "process_group", 	v_int_or_bool,	false },
	[SIDX_FOREGROUND]	=	{ "foreground_tty", v_int_or_bool,	false },
	[SIDX_CLEANENV]		=	{ "clean_environ", 	v_boolean, 		false },
	[SIDX_JAIL_BEFORE]	=	{ "jail_before", 	v_integer,		false },
	[SIDX_JAIL_AFTER]	=	{ "jail_after",		v_integer,		false },
	[SIDX_VERBOSE]		=	{ "verbose",		v_str_or_bool,	false },
};

static void
lspawn_call_init_args(lua_State *L)
{
	lua_createtable(L, 0, countof(lspawn_args) + 1);
	for (int i = 0; i < countof(lspawn_args); ++i)
	{
		if (lspawn_args[i].keyname)
		{
			lua_pushinteger(L, i);
			lua_setfield(L, -2, lspawn_args[i].keyname);
		}
	}
}

static void
lspawn_call_do_args(lua_State *L, int idx)
{
	idx = lua_absindex(L, idx);

	// Don't need the full pairs() pushups here, since we know
	// the refidx table has no metatable
	lua_pushnil(L);
	while (lua_next(L, lua_upvalueindex(1)))
	{
		int i = lua_tointeger(L, -1);

		lua_copy(L, -2, -1);
		int argt = lua_gettable(L, idx);

		if (argt != LUA_TNIL)
		{
			const char *expect = lspawn_args[i].isvalid(L, argt);

			if (expect)
				luaL_error(L, "bad argument: field '%s' expected %s",
						   lspawn_args[i].keyname, expect);

			lua_copy(L, -1, i);
		}
		else if (lspawn_args[i].required)
			luaL_error(L, "required field '%s' missing", lspawn_args[i].keyname);

		lua_pop(L, 1);
	}

	// Check the original arg table for spurious keys.

	lua_pushnil(L);

	bool metaloop = pairs_start(L, idx, lua_absindex(L, -1), true);
	while (metaloop ? pairs_next(L) : lua_next(L, idx))
	{
		lua_pushvalue(L, -2);
		if (lua_type(L, -1) != LUA_TSTRING)
			luaL_error(L, "key in argument table is not a string");
		else if (lua_rawget(L, lua_upvalueindex(1)) == LUA_TNIL)
			luaL_error(L, "unknown key '%s' in argument table", lua_tostring(L, -3));
		lua_pop(L, 2);
	}

	lua_pop(L, 1);
}

static int
wait_for_proc(lua_State *L,
			  pid_t child_pid,
			  sigset_t *wait_ignore_sigs,
			  sigset_t *save_sigmask)
{
	struct sigaction wait_oact[sizeof(sigset_t)*8];
	int			wait_oact_err[sizeof(sigset_t)*8];

	struct sigaction nact = { .sa_flags = 0, .sa_handler = SIG_IGN };
	int			i;
	int			status = 0;
	int			err;
	pid_t		rpid;

	i = 0;
	while ((i = iterate_sigs(wait_ignore_sigs, i)) >= 0)
		wait_oact_err[i] = sigaction(i, &nact, &wait_oact[i]);

	do
		rpid = waitpid(child_pid, &status, 0);
	while (rpid < 0 && errno == EINTR);
	err = errno;

	i = 0;
	while ((i = iterate_sigs(wait_ignore_sigs, i)) >= 0)
		if (wait_oact_err[i] == 0)
			sigaction(i, &wait_oact[i], NULL);

	safe_setsigmask(SIG_SETMASK, save_sigmask, NULL);
	if (rpid == child_pid)
		return my_execresult(L, status);
	else if (rpid >= 0)
		return luaL_error(L, "waitpid returned wrong pid: expected %ld got %ld",
						  (long)child_pid, (long)rpid);
	else
		return my_errresult(L, err);
}

// Actual main entry point.


static int
lspawn_doit(lua_State *L, enum spawn_op context)
{
	sigset_t	default_sigs;
	sigset_t	ignore_sigs;
	sigset_t	block_sigs;

	sigset_t	wait_block_sigs;
	sigset_t	wait_ignore_sigs;
	sigset_t	save_sigmask;

	spawnattr_t	sa;
	spawn_file_actions_t *file_acts;
	short		spawn_attr_flags = (SPAWN_SETSIGDEF
									| SPAWN_SETSIGIGN_NP
									| SPAWN_SETSIGMASK);
	pid_t		pgrp = 0;
	int			jail_before = -1;
	int			jail_after = -1;

	bool		rlim_set[RLIM_NLIMITS];
	struct rlimit rlim[RLIM_NLIMITS];

	bool		do_exec = false;
	bool		do_wait = (context == LSPAWN_WAIT);
	bool		inherit_env = true;
	int			foreground_fd = -1;
	const char *filename;
	const char *search_path = NULL;
	const char *errprefix = NULL;

	int			nargs = 0;
	int			nenvs = -1;		// -1 == use environ unchanged

	const char *argvec[256];
	const char **argv = argvec;
	const char **envp;

	pid_t		child_pid;
	int			err = 0;

	// We might push a bunch of things for args, env vars, etc. We aim to keep
	// 63 free slots minimum, and check for extension in indefinite loops
	// after 32 iterations. Include a chunk of slop here to avoid unnecessary
	// reallocs.

	luaL_checkstack(L, 100 + SIDX__LASTARG, "in spawn");

	// Allow simplified call style of spawn(prog,arg,...) by converting it to
	// the full table form; prog must be a string. We assume that simpified
	// form wants a path search.
	switch (lua_type(L, 1))
	{
		case LUA_TSTRING:
		{
			int n = lua_gettop(L) - 1;
			lua_createtable(L, n, 0);
			lua_insert(L, 1);
			// table prog arg1 arg2 ... argn
			for (; n >= 1; --n)
			{
				if (!lua_isstring(L, -1))
					luaL_argerror(L, n+1, "expected string");
				lua_rawseti(L, 1, n);
			}
			lua_createtable(L, 0, 1);
			lua_insert(L, 1);
			lua_setfield(L, 1, "program");
			lua_setfield(L, 1, "args");
			lua_pushboolean(L, 1);
			lua_setfield(L, 1, "search_path");
			lua_settop(L, 1);
			break;
		}
		case LUA_TTABLE:
			if (lua_gettop(L) > 1)
				return luaL_argerror(L, 2, "only one argument expected");
			break;
		default:
			return luaL_argerror(L, 1, "expected table or string");
	}

	// Process args and fill in all the stack slots.
	lua_settop(L, SIDX__LASTARG);
	lspawn_call_do_args(L, 1);

	do_exec = lua_toboolean(L, SIDX_EXEC);
	if (!lua_isnil(L, SIDX_WAIT))
		do_wait = lua_toboolean(L, SIDX_WAIT);
	if (do_wait && do_exec)
		return luaL_argerror(L, 1, "cannot specify both wait and exec");
	if (lua_toboolean(L, SIDX_VERBOSE)
		|| (do_exec && lua_type(L, SIDX_VERBOSE) != LUA_TBOOLEAN))
	{
		errprefix = lua_tostring(L, SIDX_VERBOSE);
		spawn_attr_flags |= SPAWN_VERBOSE_NP;
	}
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
	if (!lua_isnil(L, SIDX_JAIL_BEFORE))
		jail_before = lua_tointeger(L, SIDX_JAIL_BEFORE);
	if (!lua_isnil(L, SIDX_JAIL_AFTER))
		jail_after = lua_tointeger(L, SIDX_JAIL_AFTER);

	if (jail_before >= 0 && jail_after >= 0)
		return luaL_argerror(L, 1, "cannot specify both jail_before and jail_after");

	sigfillset(&default_sigs);
	sigemptyset(&ignore_sigs);
	sigemptyset(&block_sigs);
	if (do_wait)
	{
		sigemptyset(&wait_ignore_sigs);
		sigemptyset(&wait_block_sigs);
		sigaddset(&wait_block_sigs, SIGINT);
		sigaddset(&wait_block_sigs, SIGQUIT);
		sigaddset(&wait_block_sigs, SIGCHLD);
	}

	// If a string, it must be "preserve", already checked above
	if (lua_type(L, SIDX_SIGNALS) == LUA_TSTRING)
	{
		if (do_wait)
		{
			spawn_attr_flags &= ~(SPAWN_SETSIGDEF | SPAWN_SETSIGIGN_NP);
			safe_setsigmask(SIG_UNBLOCK, NULL, &block_sigs);
		}
		else
			spawn_attr_flags &= ~(SPAWN_SETSIGDEF | SPAWN_SETSIGIGN_NP | SPAWN_SETSIGMASK);
	}
	else if (!lua_isnil(L, SIDX_SIGNALS))
		process_signals(L, SIDX_SIGNALS,
						&default_sigs, &ignore_sigs, &block_sigs,
						do_wait ? &wait_ignore_sigs : NULL,
						do_wait ? &wait_block_sigs : NULL);

	for (int res = 0; res < RLIM_NLIMITS; ++res)
		rlim_set[res] = false;

	if (!lua_isnil(L, SIDX_RESOURCES))
		if (process_resources(L, SIDX_RESOURCES, rlim_set, rlim))
			spawn_attr_flags |= SPAWN_SETRLIMITS_NP;

	lua_settop(L, SIDX__DROP1);

	// Note that slots for toclose objects must be allocated in order of
	// usage.
	// Reserve a slot for a possible toclose field for the envt iterator
	// Reserve a slot for the file_actions object
	// Reserve another 2 slots for possible FILE* for pipe objects
	// Slots above that are args/envs first, then other stuff

#define SIDX_ENVT_ITER	(SIDX__DROP1 + 1)
#define SIDX_FILE_ACTS	(SIDX__DROP1 + 2)
#define SIDX_PIPEOBJS	(SIDX__DROP1 + 3)
#define SIDX_ARGBASE	(SIDX__DROP1 + 5)

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

	nargs = process_args(L, SIDX_PROGRAM, SIDX_ARGS);
	nenvs = process_envs(L, SIDX_ENVIRON, SIDX_ENVT_ITER, inherit_env);

	if (nargs + nenvs + 2 > countof(argvec))
		argv = lua_newuserdata(L, (unsigned)(nargs + nenvs + 2) * sizeof(const char *));

	vectorize_args(L, argv, SIDX_ARGBASE, nargs, nenvs);
	if (nenvs >= 0)
		envp = argv + nargs + 1;
	else
		envp = NULL;

	file_acts = lspawn_file_actions_new(L);
	lua_replace(L, SIDX_FILE_ACTS);
	make_auto_closevar(L, SIDX_FILE_ACTS);

	if (jail_before >= 0)
	{
		err = spawn_file_actions_addjail_np(file_acts, jail_before);
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	if (!lua_isnil(L, SIDX_CHROOT_BEFORE))
	{
		err = spawn_file_actions_addchroot_np(file_acts,
											  lua_tostring(L, SIDX_CHROOT_BEFORE));
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	if (!lua_isnil(L, SIDX_CHDIR_BEFORE))
	{
		err = spawn_file_actions_addchdir(file_acts,
										  lua_tostring(L, SIDX_CHDIR_BEFORE));
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	if (lua_isnil(L, SIDX_FILES))
		process_files_default(L,
							  (do_exec || do_wait) ? 0 : SIDX_PIPEOBJS,
							  file_acts, context, foreground_fd);
	else
		process_files(L, SIDX_FILES,
					  (do_exec || do_wait) ? 0 : SIDX_PIPEOBJS,
					  file_acts, foreground_fd);

	if (jail_after >= 0)
	{
		err = spawn_file_actions_addjail_np(file_acts, jail_after);
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	if (!lua_isnil(L, SIDX_CHROOT))
	{
		err = spawn_file_actions_addchroot_np(file_acts,
											  lua_tostring(L, SIDX_CHROOT));
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	if (!lua_isnil(L, SIDX_CHDIR))
	{
		err = spawn_file_actions_addchdir(file_acts,
										  lua_tostring(L, SIDX_CHDIR));
		if (unlikely(err != 0))
			return file_act_err(L, err);
	}

	// set up to block signals if we'll be waiting.

	if (do_wait)
		safe_setsigmask(SIG_BLOCK, &wait_block_sigs, &save_sigmask);

	// No Lua errors after here, so we don't need to worry about
	// freeing the spawnattr on a nonlocal exit.

	spawnattr_init(&sa);
	if (spawn_attr_flags & SPAWN_SETPGROUP)
		spawnattr_setpgroup(&sa, pgrp);
	spawnattr_setsigdefault(&sa, &default_sigs);
	spawnattr_setsigignore_np(&sa, &ignore_sigs);
	spawnattr_setsigmask(&sa, &block_sigs);
	if (errprefix)
		err = spawnattr_seterrprefix_np(&sa, errprefix);
	spawnattr_setflags(&sa, spawn_attr_flags);

	for (int res = 0; res < RLIM_NLIMITS; ++res)
		if (err == 0 && rlim_set[res])
			err = spawnattr_setrlimit_np(&sa, res, true, &rlim[res]);

	// finally!

	if (do_exec && err == 0)
	{
		spawnexecP(filename,
				   search_path,
				   file_acts,
				   &sa,
				   DECONST(char **, argv),
				   DECONST(char **, envp));
		// should never be reached
		_exit(127);
	}
	else if (err == 0)
		err = spawnP(&child_pid,
					 filename,
					 search_path,
					 file_acts,
					 &sa,
					 DECONST(char **, argv),
					 DECONST(char **, envp));

	spawnattr_destroy(&sa);

	// Lua errors can happen again now.

	auto_closevar_compat(L, SIDX_PIPEOBJS+1);
	auto_closevar_compat(L, SIDX_FILE_ACTS);

	if (err)
	{
		auto_closevar_compat(L, SIDX_PIPEOBJS);
		return my_errresult(L, err);
	}

	if (do_wait)
		return wait_for_proc(L, child_pid, &wait_ignore_sigs, &save_sigmask);

	if (context != LSPAWN_READFROM && context != LSPAWN_WRITETO)
		lua_pushinteger(L, child_pid);

	if (lua_type(L, SIDX_PIPEOBJS) != LUA_TNIL)
	{
		auto_closevar_release(L, SIDX_PIPEOBJS);
		SPStream *sp = luaL_checkudata(L, -1, LUA_FILEHANDLE);
		sp->pid = child_pid;
		return (context == LSPAWN_READFROM || context == LSPAWN_WRITETO) ? 1 : 2;
	}

	return 1;
}


static int lspawn_call(lua_State *L);

static RegMeta lspawn_lib_meta = {
	.name = "lspawn library",
	.metamethods = (luaL_Reg[]){
		{ "__call", lspawn_call },
		{ NULL, NULL }
	}
};

static int
lspawn_call(lua_State *L)
{
	// Argument errors currently show as off-by-one in __call metamethods
	// (which this is). Jiggle the stack to compensate.
	if (verify_object_exact(L, 1, &lspawn_lib_meta))
		lua_remove(L, 1);

	return lspawn_doit(L, LSPAWN_CALL);
}

static int
lspawn_wait(lua_State *L)
{
	return lspawn_doit(L, LSPAWN_WAIT);
}

static int
lspawn_readfrom(lua_State *L)
{
	return lspawn_doit(L, LSPAWN_READFROM);
}

static int
lspawn_writeto(lua_State *L)
{
	return lspawn_doit(L, LSPAWN_WRITETO);
}

static int
lspawn_waitpid(lua_State *L)
{
	pid_t pid = (pid_t) luaL_checkinteger(L, 1);

	luaL_argcheck(L, (lua_gettop(L) == 1), 2, "none expected");

	return do_waitpid(L, pid);
}

static luaL_Reg lspawn_funcs[] = {
	{ "wait", lspawn_wait },
	{ "waitpid", lspawn_waitpid },
	{ "read_from", lspawn_readfrom },
	{ "write_to", lspawn_writeto },
	{ NULL, NULL }
};

//==========================================================================

// Library initialization.

EXPORTED int luaopen_lspawn(lua_State *);

int
luaopen_lspawn(lua_State *L)
{
	lua_settop(L, 0);

	// metatables for FA objects
	make_metatable(L, &lspawn_file_action_meta, 0);
	make_metatable(L, &lspawn_file_actions_meta, 0);
	lua_settop(L, 0);

	lua_newtable(L);
	lspawn_call_init_args(L);
	lua_pushstring(L, "=");
	make_signal_table(L);

	lua_pushvalue(L, -3);
	lua_pushvalue(L, -3);
	lua_pushvalue(L, -3);
	make_metatable(L, &lspawn_lib_meta, 3);
	lua_setmetatable(L, 1);

	luaL_setfuncs(L, lspawn_funcs, 3);
	luaL_setfuncs(L, lspawn_fa_funcs, 0);
	for (luaL_Reg *rp = lspawn_fa_objs; rp->name != NULL; ++rp)
	{
		rp->func(L);
		lua_setfield(L, -2, rp->name);
	}

	return 1;
}
