
lspawn
======

NOTE: the name `lspawn` is provisional and may be changed in the
future.

All examples in this file assume the module has been loaded as
follows:

	local spawn = require 'lspawn'

Simplified usage
----

The simplest usage is:

	pid,err,errno = spawn("prog","arg","arg")

or

	flag,status_or_err,code = spawn.wait("prog","arg","arg")

This creates a subprocess which will execute `prog` (searched for in
the caller's `PATH`) with the specified args. No shell is executed
unless `prog` itself is a shell or a shell script. No interpretation
is performed on the arguments, which must be strings (or numbers which
will be converted to strings by Lua). `argv[0]` is set to `prog`.

For `spawn()`, the `pid` of the new subprocess is returned if a
subprocess could be started; otherwise a false value is returned along
with an error message string and an error code. The subprocess
executes concurrently and is not waited for.

For `spawn.wait()`, if the subprocess could not be started then an
error is returned as for `spawn()`, otherwise the call waits for the
subprocess to complete, and then returns the same three results as
`os.execute()`: a success flag (true if the subprocess exited with a
success code), a string `"exit"` or `"signal"`, and an exit code.
While waiting for the subprocess, the signals `SIGINT`, `SIGQUIT` and
`SIGCHLD` are blocked (but unlike `system()`, no signals are ignored -
see below for how to do that).

This function differs from other comparable "spawn" or "exec"
functions in critical respects:

  1. File descriptors 0,1,2 are passed to the subprocess unchanged,
     but **no other open files** are passed to it regardless of any
	 file descriptor flags.
	 
  0. All signals are reset to defaults and unblocked in the
     subprocess.


General usage
----

General usage is as follows:

    pid,err_or_pipe,errno =
        spawn {
            exec = bool,
			wait = bool,
            verbose = label,
            program = "prog",           -- required
            args = {...},
            environ = {...},
            files = {
                [n] = action, ...
            },
            directory = "dir",
            directory_before = "dir",
            chroot = "dir",
            chroot_before = "dir",
            search_path = path,
            signals = {
                ignore = {...},
                block = {...},
                preserve = {...},
				ignore_waiting = {...},
				block_waiting = {...}
            },
            resources = {
                name = limit, ...
            },
            reset_ids = bool,
            new_session = bool,
            process_group = pgrp,
            foreground_tty = ttyfd,
            clean_environ = bool,
            jail_before = jid,
            jail_after = jid
        }

All arguments except `program` are optional.

  * `exec` (boolean) If set to true means that the call will not start
    a subprocess, but will replace the current process. All other
    actions are performed identically. In the event of an error after
    preliminary processing of parameters, the current process will be
    terminated by `_exit(127)`. This option enables the `verbose`
    option by default, since error reporting is otherwise impossible.
    Default is false.

  * `wait` (boolean) If set to true, then if the subprocess could be
    started, wait for it to complete and then return result status
    values in the same form as `os.execute` in place of the subprocess
    pid. If the subprocess could not be started then the error is
    returned normally.

    If invoked as `spawn.wait {...}`, then `wait` is defaulted to
    true, otherwise false.

  * `verbose` (boolean or string) If set to true or a string, errors
    in preparing the subprocess will be reported to wherever the
    subprocess `stderr` descriptor points at the time (which will be
    either the `stderr` of the invoking process, or the destination
    specified in `files` for descriptor 2 of the subprocess). If a
    string is given, it is used as a prefix. If explicitly set to
    false, nothing is written to stderr even in the `exec` case.

    Note that a pipe created by `spawn.output_pipe` will be closed if
	the subprocess is not started, so is not a suitable destination
	for verbose errors.

  * `program` (string) specifies the executable file to run. If
    `search_path` is unset or false, then `program` is an absolute
    path or is relative to the current directory. If `search_path` is
    set and `program` is not absolute, it is searched for in the
    applicable path.

    Note that all file and directory actions, including all jail and
    chroot operations, are performed before the program is searched
    for, so any specified paths must be resolvable within the final
    root and current directories.

  * `args` (sequence of string or number values) specifies the
    arguments to the program. `argv[0]` defaults to `program` but may
    be overridden by an `[0] =` entry in `args`.
	
	An `__index` metavalue (but not `__pairs` or `__ipairs`) is
    respected.
	
  * `environ` (table with string keys and string or number values)
    specifies environment variables to add to the environment of the
    spawned program. Note that setting `PATH` here never affects the
    path used to find `program` (see `search_path`).
	
	A `__pairs` metamethod is respected (but not `__index`).

  * `clean_environ` (boolean) if true, the subprocess starts with an
    empty environment, with only the entries specified by `environ`
    added.
  
  * `files` (table with non-negative integer keys, not necessarily a
    sequence) specifies the desired assignment of file descriptors in
    the spawned program. All file descriptors above 2 which are not
    explicitly assigned here will be closed in the subprocess. The
    value specified for a descriptor must be one of:
	
	  + `spawn.close` (leaves the descriptor closed; default for fds above 2)
	  
	  + `spawn.null` (open the descriptor on `/dev/null`)
	  
	  + `spawn.inherit` (inherit the corresponding descriptor from the
        invoking process; default for stdin, stdout and stderr)
	  
	  + `spawn.input_pipe` (create a pipe for the spawned program to
        read from; the invoker's end of the pipe is returned as the
        second result)
	  
	  + `spawn.output_pipe` (create a pipe for the spawned program to
        write to; the invoker's end is returned as the second result)
	  
	  + `spawn.inherit_from(n)` (inherit from the specified descriptor
        of the invoking process)
	  
	  + `spawn.copy_from(n)` (copy whatever descriptor n is set to in
        the subprocess after all `open` actions were performed)
	  
	  + `spawn.open(filename,flags[,mode])` (open the specified file
	    in the subprocess; `flags` can be "r", "r+", etc., or numeric)

    File actions are performed in this order: inheritance and pipes
	first, then all other actions except copies, and copies last.
	Dependencies between inherit operations are properly handled (for
	example, it's safe to exchange two descriptors).

	A `__pairs` metamethod is respected (but not `__index`).

  * `search_path` (string or boolean) if set to true or a string,
    specifies that `program` is searched for in the specified path;
    true uses the caller's `PATH` environment variable (not affected
    by the `environ` parameter), or a string can be used to specify an
    explicit list of directories with colon separators as normal.
  
  * `jail_before` (integer) specifies the jail id of a jail to attach
    to before processing any other directory or file actions

  * `chroot_before` (string) specifies a directory to change to after
    processing `jail_before` but before all other directory and file
    actions
	
  * `directory_before` (string) specifies a directory to change to
    before processing file actions, but after `jail_before` and
    `chroot_before`

  * `jail_after` (integer) specifies a jail to attach to after
    processing file actions, but before `chroot` and `directory`

  * `chroot` (string) specifies a directory to chroot to after
    processing all file actions and jails, but before `directory`

  * `directory` (string) specifies a directory to change to after
    processing all file actions, jails, and chroots
	
  * `signals` (table) specifies signal dispositions for the
    subprocess. By default, all signals are reset to default and
    unblocked. Each of the elements `ignore`, `block`, and `preserve`
    may specify a list (in the form of a sequence of numbers or
    `SIGxxx` strings) of signals to ignore, block, or preserve the
    blocked/ignored state, respectively. (If a signal is listed under
    both `ignore` and `preserve`, then `ignore` takes precedence.)
    Signals caught in the current process are reset to default unless
    listed in `ignore`.

    `signals` can also be set to a string `"preserve"` to preserve all
    signals from the invoking process.

    If `wait` is enabled, then `ignore_waiting` specifies a list of
    signals to ignore while waiting for the subprocess, and
    `block_waiting` specifies a list of signals to block while
    waiting. The default for `block_waiting` is to block `SIGINT`,
    `SIGQUIT`, and `SIGCHLD`; no signals are ignored by default.

	Respects `__index` but not `__pairs` on both the signals table and
    its elements.
	
  * `resources` (table) specifies resource limits to be set with
    `setrlimit` in the subprocess. The keys of the `resources`
    table are resource names or numbers; the names may be either
    lowercase versions of the `RLIMIT_*` constants (without the
    `RLIMIT_` prefix), or the names recognized in `login.conf`. The
    values may be strings, which are processed with optional suffixes
    as in `login.conf`, or integers, or tables with optional elements
    `cur` and `max` (or `rlim_cur` and `rlim_max`) to set soft and
    hard limits independently. (String or integer values set both the
    soft and hard limit together.)

	Respects `__index` but not `__pairs` on both the resources table
    and any table elements.

  * `reset_ids` (boolean) If true, the effective GID and UID are reset
    to the real GID and UID before processing any files, directories,
    or jails.
	
  * `new_session` (boolean) If true, a new session is created for the
    subprocess before any file or directory actions.
	
  * `process_group` (integer or boolean) If set, the subprocess is
    moved to the specified process group. A value of `true` uses the
    subprocess's own pid as the process group (thus guaranteeing a new
    process group).
	
  * `foreground_tty` (integer or boolean) If set, the subprocess'
    process group (after processing `process_group`) is set as the
    foreground process group of the specified descriptor, which must
    be a tty device either inherited or opened as a file action.
    `true` uses descriptor 0 as the tty.

If no `input_pipe` or `output_pipe` actions were specified, then the
return value is just the subprocess id on success, or a false value
followed by the error text and error code on failure.

If an `input_pipe` or `output_pipe` action was specified (it is an
error to specify more than one pipe action), then on success, a second
result is returned with a Lua filehandle opened on the invoker's side
of the pipe. This filehandle, like one returned by `io.popen`, will
wait for the subprocess to terminate when the handle is closed.

(If you want any more complex plumbing than this, you can open your
own pipes using the posix module and pass the appropriate
descriptors.)

