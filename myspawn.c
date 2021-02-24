/*-
 * myspawn.c
 *
 * SPDX-License-Identifier: BSD-2-Clause
 *
 * Copyright (c) 2020-2021 Andrew Gierth <andrew@tao11.riddles.org.uk>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Originally taken (but substantially altered) from code with:
 *
 * Copyright (c) 2008 Ed Schouten <ed@FreeBSD.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * and also from code with:
 *
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <sys/cdefs.h>
#include <sys/param.h>
#include <sys/types.h>

#include <sys/queue.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <sys/user.h>

#include <errno.h>
#include <fcntl.h>
#include <sched.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include <paths.h>
#include <link.h>
#include <dlfcn.h>
#include <err.h>

#include "myspawn.h"

extern char **environ;
extern int __isthreaded;
extern int __sys_sigprocmask(int, const __sigset_t *, __sigset_t *);
extern int __sys_sigaction(int, const struct sigaction *, struct sigaction *);
extern int _fcntl(int, int, ...);
extern int _close(int);
extern int _closefrom(int);
extern int _chdir(const char *);
extern int _chroot(const char *);
extern int _fchdir(int);
extern int _jail_attach(int);
extern int _openat(int, const char *, int, ...);
extern int _write(int, const void *, __size_t);
extern int _writev(int, const struct iovec *, int);
extern int _execve(const char *, char * const *argv, char * const *envp);
extern int _setrlimit(int rlim, const struct rlimit *val);
extern int _stat(const char *, struct stat *);
extern int _waitpid(pid_t, int *, int);

#ifdef COVERAGE
extern void __gcov_flush(void);
#endif

#define STR_WITH_LEN(s_) (s_), sizeof(s_)-1

/* private data structs */

struct spawnattr {
    short		sa_flags;
    pid_t		sa_pgroup;
    int			sa_schedpolicy;
    sigset_t	sa_sigdefault;
    sigset_t	sa_sigignore;
    sigset_t	sa_sigmask;
    struct sched_param	sa_schedparam;
	const char *sa_errprefix;
	struct rlimit sa_rlimits[RLIM_NLIMITS];
	bool		sa_rlimits_set[RLIM_NLIMITS];
};

struct spawn_file_actions {
    STAILQ_HEAD(, spawn_file_actions_entry) fa_list;
};

typedef struct spawn_file_actions_entry {
    STAILQ_ENTRY(spawn_file_actions_entry) fae_list;
    enum {
		FAE_INVALID,
		FAE_OPEN,
		FAE_DUP2,
		FAE_CLOSE,
		FAE_CLOSEFROM,
		FAE_CHROOT,
		FAE_CHDIR,
		FAE_FCHDIR,
		FAE_SETPGRP,
		FAE_JAIL
	}			fae_action;

    int			fae_fildes;
    char	   *fae_path;
    int			fae_atfd;
    int			fae_oflag;
    mode_t		fae_mode;
} spawn_file_actions_entry_t;

/*
 * We (noinline) most of these because controlling stack usage is more
 * important than squeezing cycles.
 */

static bool
sig_ignored_by_default(int sig)
{
	return ((sig == SIGIO)
			|| (sig == SIGURG)
			|| (sig == SIGWINCH)
			|| (sig == SIGINFO));
}

/*
 * this is just used as an optimization, so it's ok for it to fail.
 *
 * Note that SIGCHLD is weird and this interface can't correctly capture its
 * state, so we make special exceptions for that below. Another discrepancy is
 * that SIGCONT will never show up here as "ignored" (even if it is), while
 * SIGIO, SIGURG, SIGWINCH, SIGINFO will show as "ignored" if they are either
 * SIG_IGN or SIG_DFL. We slightly reduce the number of syscalls needed if we
 * assume SIG_DFL in such cases (see below).
 */
__noinline
static int
get_sigstatus(sigset_t *sigign, sigset_t *sigcatch)
{
    struct kinfo_proc p;
    int			name[4];
    size_t		len = sizeof(p);

    name[0] = CTL_KERN;
    name[1] = KERN_PROC;
    name[2] = KERN_PROC_PID;
    name[3] = (int) getpid();

    if (sysctl(name, 4, &p, &len, NULL, 0) < 0)
		return errno;
    else if (len == 0)
		return ESRCH;
    else if (len != sizeof(p)
			 || p.ki_structsize != sizeof(p))
		return EINVAL;

    if (sigign)
		*sigign = p.ki_sigignore;
    if (sigcatch)
		*sigcatch = p.ki_sigcatch;

    return 0;
}

/*
 * Spawn routines
 */

struct spawn_args {
    const char *file;
    const char *path;
    const spawn_file_actions_t *fa;
    const spawnattr_t *sa;
    char * const * argv;
    char * const * envp;
    sigset_t vfmask;
    bool is_vfork;
	bool verbose;
	bool is_exec;
    volatile int error;
};

__noinline
static int
really_report(struct spawn_args *psa, int err, const char *what, const char *arg)
{
	const char *estr = err < sys_nerr && sys_errlist[err] ? sys_errlist[err] : "unknown error";
	size_t estr_len = strlen(estr);
	static const char *const sep = ": ";
	static const char *const nel = "\n";
	const char *errprefix = psa->sa ? (*psa->sa)->sa_errprefix : NULL;
	struct iovec iov[8] = {
		{ .iov_base = __DECONST(void *, errprefix), .iov_len = errprefix ? strlen(errprefix) : 0 },
		{ .iov_base = __DECONST(void *, sep),  .iov_len = 2 },
		{ .iov_base = __DECONST(void *, what), .iov_len = strlen(what) },
		{ .iov_base = __DECONST(void *, sep),  .iov_len = 2 },
		{ .iov_base = __DECONST(void *, arg ? arg : estr), .iov_len = arg ? strlen(arg) : estr_len },
		{ .iov_base = __DECONST(void *, arg ? sep : nel),  .iov_len = arg ? 2 : 1 },
		{ .iov_base = __DECONST(void *, estr), .iov_len = estr_len },
		{ .iov_base = __DECONST(void *, nel), .iov_len = 1 }
	};

	if (errprefix)
		(void)_writev(STDERR_FILENO, iov, 6 + (arg ? 2 : 0));
	else
		(void)_writev(STDERR_FILENO, iov + 2, 4 + (arg ? 2 : 0));
	return err;
}

static int
report_err(struct spawn_args *psa, int err, const char *what, const char *arg)
{
	if (err <= 0 || !psa->verbose)
		return err;
	return really_report(psa, err, what, arg);
}

/*
 * In the vfork case, parent blocked all signals (but we do that again here
 * just in case), and vfmask is the signal mask we're supposed to restore,
 * whether the parent's original mask or the one set in the spawnattr.
 *
 * We must reset every signal that is caught, regardless, since we can't allow
 * any handler to execute (would corrupt parent process memory).
 */

__noinline
static int
process_signals_for_vfork(struct spawn_args *psa)
{
	const spawnattr_t *sa = psa->sa;
    struct sigaction sigact = { .sa_flags = 0, .sa_handler = SIG_DFL };
    struct sigaction sigiact = { .sa_flags = 0, .sa_handler = SIG_IGN };
    struct sigaction sigoact;
	sigset_t   *vfmask = &psa->vfmask;
    sigset_t	allsigs;
    sigset_t	sig_was_ignored;
    sigset_t	sig_was_caught;
    bool		sigs_known = false;

    sigfillset(&allsigs);
    if (__sys_sigprocmask(SIG_SETMASK, &allsigs, NULL) < 0)
		return report_err(psa, errno, "sigprocmask", NULL);

    sigfillset(&sig_was_caught);
    sigfillset(&sig_was_ignored);
    if (get_sigstatus(&sig_was_ignored, &sig_was_caught) == 0)
		sigs_known = true;

    bool setdefflag = sa && (*sa)->sa_flags & SPAWN_SETSIGDEF;
    bool setignflag = sa && (*sa)->sa_flags & SPAWN_SETSIGIGN_NP;

    for (int sig = 1; sig <= _SIG_MAXSIG; ++sig)
	{
		bool setdef = setdefflag && sigismember(&(*sa)->sa_sigdefault, sig);
		bool setign = setignflag && sigismember(&(*sa)->sa_sigignore, sig);
		bool was_caught = sigismember(&sig_was_caught, sig);
		bool was_ignored = sigismember(&sig_was_ignored, sig);

		if (!setdef && !setign)
		{
			if (sigs_known)
				setdef = was_caught;
			else
			{
				if (__sys_sigaction(sig, NULL, &sigoact) != 0)
					return report_err(psa, errno, "sigaction", NULL);
				if (sigoact.sa_handler != SIG_DFL
					&& sigoact.sa_handler != SIG_IGN)
					setdef = true;
			}
		}
		if (setdef)
		{
			if (!sigs_known
				|| was_caught
				|| (was_ignored && !sig_ignored_by_default(sig))
				|| sig == SIGCHLD)
				if (__sys_sigaction(sig, &sigact, NULL) != 0)
					return report_err(psa, errno, "sigaction", NULL);
		}
		else if (setign)
		{
			if (!sigs_known || was_caught || !was_ignored || sig == SIGCHLD)
				if (__sys_sigaction(sig, &sigiact, NULL) != 0)
					return report_err(psa, errno, "sigaction", NULL);
		}
    }

    if (__sys_sigprocmask(SIG_SETMASK, vfmask, NULL) != 0)
		return report_err(psa, errno, "sigprocmask", NULL);

    return 0;
}

/*
 * rfork case is much simpler because first: we don't need to worry about
 * resetting caught signals to default, because rfork did that for us;
 * second, we don't need to worry about blocking or unblocking signals
 * unless the caller specifically asked.
 */
__noinline
static int
process_signals_for_rfork(struct spawn_args *psa)
{
	const spawnattr_t sa = *(psa->sa);
    struct sigaction sigact = { .sa_flags = 0, .sa_handler = SIG_DFL };
    struct sigaction sigiact = { .sa_flags = 0, .sa_handler = SIG_IGN };
    sigset_t	sig_was_ignored;
    bool		sigs_known = false;

    if (sa->sa_flags & SPAWN_SETSIGMASK)
		if (__sys_sigprocmask(SIG_SETMASK, &sa->sa_sigmask, NULL) != 0)
			return report_err(psa, errno, "sigprocmask", NULL);

    sigfillset(&sig_was_ignored);
    if (get_sigstatus(&sig_was_ignored, NULL) == 0)
		sigs_known = true;

    if (sa->sa_flags & (SPAWN_SETSIGDEF | SPAWN_SETSIGIGN_NP))
	{
		bool setdefflag = sa->sa_flags & SPAWN_SETSIGDEF;
		bool setignflag = sa->sa_flags & SPAWN_SETSIGIGN_NP;

		for (int sig = 1; sig <= _SIG_MAXSIG; ++sig)
		{
			if (setdefflag && sigismember(&sa->sa_sigdefault, sig))
			{
				if (!sigs_known
					|| sig == SIGCHLD
					|| (!sig_ignored_by_default(sig) && sigismember(&sig_was_ignored, sig)))
					if (__sys_sigaction(sig, &sigact, NULL) != 0)
						return report_err(psa, errno, "sigaction", NULL);
			}
			else if (setignflag && sigismember(&sa->sa_sigignore, sig))
			{
				if (!sigs_known || sig == SIGCHLD || !sigismember(&sig_was_ignored, sig))
					if (__sys_sigaction(sig, &sigiact, NULL) != 0)
						return report_err(psa, errno, "sigaction", NULL);
			}
		}
    }

    return 0;
}

static int
process_spawnattr(struct spawn_args *psa)
{
	const spawnattr_t sa = *(psa->sa);

    /* Set session */
    if (sa->sa_flags & SPAWN_SETSID)
		if (setsid() < 0)
			return report_err(psa, errno, "setsid", NULL);

    /* Set process group */
    if (sa->sa_flags & SPAWN_SETPGROUP)
		if (setpgid(0, sa->sa_pgroup) != 0)
			return report_err(psa, errno, "setpgid", NULL);

    /* Set scheduler policy */
    if (sa->sa_flags & SPAWN_SETSCHEDULER)
	{
		if (sched_setscheduler(0,
							   sa->sa_schedpolicy,
							   &sa->sa_schedparam) != 0)
			return report_err(psa, errno, "sched_setscheduler", NULL);
    }
	else if (sa->sa_flags & SPAWN_SETSCHEDPARAM)
	{
		if (sched_setparam(0, &sa->sa_schedparam) != 0)
			return report_err(psa, errno, "sched_setparam", NULL);
    }

	/* Set resources */
	if (sa->sa_flags & SPAWN_SETRLIMITS_NP)
	{
		for (int res = 0; res < RLIM_NLIMITS; ++res)
		{
			if (sa->sa_rlimits_set[res])
			{
				if (_setrlimit(res, &sa->sa_rlimits[res]) < 0)
					return report_err(psa, errno, "setrlimit", NULL);
			}
		}
	}

    /* Reset user ID's */
    if (sa->sa_flags & SPAWN_RESETIDS)
	{
		if (setgid(getgid()) != 0)
			return report_err(psa, errno, "setgid", NULL);
		if (setuid(getuid()) != 0)
			return report_err(psa, errno, "setuid", NULL);
    }

    return 0;
}

static int
process_file_actions_entry(struct spawn_args *psa,
						   spawn_file_actions_entry_t *fae)
{
    int			fd;

    switch (fae->fae_action)
	{
		case FAE_OPEN:
			/* Perform an open(), make it use the right fd */
			fd = _openat(fae->fae_atfd,
						 fae->fae_path,
						 fae->fae_oflag,
						 fae->fae_mode);
			if (fd < 0)
				return report_err(psa, errno, "openat", fae->fae_path);

			if (fd != fae->fae_fildes)
			{
				if (_fcntl(fd,
						   ((fae->fae_oflag & O_CLOEXEC)
							? F_DUP2FD_CLOEXEC
							: F_DUP2FD),
						   fae->fae_fildes) < 0)
				{
					int saved_errno = errno;
					(void)_close(fd);
					return report_err(psa, saved_errno, "fcntl(dup for open)", NULL);
				}
				if (_close(fd) != 0)
					if (errno == EBADF)
						return report_err(psa, EBADF, "close", NULL);
			}
			return 0;

		case FAE_DUP2:
			/* Perform a dup2(), unless the descriptors are equal, in which
			 * case we regard this as an attempt to turn off FD_CLOEXEC on the
			 * fd.
			 */
			if (fae->fae_atfd != fae->fae_fildes)
			{
				if (_fcntl(fae->fae_atfd, F_DUP2FD, fae->fae_fildes) < 0)
					return report_err(psa, errno, "fcntl(DUP2)", NULL);
			}
			else if (_fcntl(fae->fae_fildes, F_SETFD, 0) < 0)
				return report_err(psa, errno, "fcntl(SETFD)", NULL);
			return 0;

		case FAE_CLOSE:
			/* Perform a close(), do not fail if already closed */
			(void)_close(fae->fae_fildes);
			return 0;

		case FAE_CLOSEFROM:
			/* Perform a closefrom(), do not fail if already closed */
			(void)_closefrom(fae->fae_fildes);
			return 0;

		case FAE_CHROOT:
			/* Perform a chroot() */
			if (_chroot(fae->fae_path) < 0)
				return report_err(psa, errno, "chroot", fae->fae_path);
			return 0;

		case FAE_CHDIR:
			/* Perform a chdir() */
			if (_chdir(fae->fae_path) < 0)
				return report_err(psa, errno, "chdir", fae->fae_path);
			return 0;

		case FAE_FCHDIR:
			/* Perform an fchdir() */
			if (_fchdir(fae->fae_fildes) < 0)
				return report_err(psa, errno, "fchdir", NULL);
			return 0;

		case FAE_JAIL:
			/* Perform a jail_attach() */
			if (_jail_attach(fae->fae_fildes) < 0)
				return report_err(psa, errno, "jail_attach", NULL);
			return 0;

		case FAE_SETPGRP: 			/* Perform a tcsetpgrp() */
		{
			sigset_t osigs;
			sigset_t sigs;
			/*
			 * Have to block SIGTTOU, since we're not likely to be the
			 * foreground pgrp already and we'll get EIO otherwise from trying
			 * to suspend while in vfork.
			 */
			sigemptyset(&sigs);
			sigaddset(&sigs, SIGTTOU);
			if (__sys_sigprocmask(SIG_BLOCK, &sigs, &osigs) < 0)
				return report_err(psa, errno, "sigprocmask(block TTOU)", NULL);
			if (tcsetpgrp(fae->fae_fildes, getpgrp()) < 0)
				return report_err(psa, errno, "tcsetpgrp", NULL);
			if (__sys_sigprocmask(SIG_SETMASK, &osigs, NULL) < 0)
				return report_err(psa, errno, "sigprocmask(restore TTOU)", NULL);
			return 0;
		}

		default:
			return report_err(psa, EINVAL, "process_file_action", NULL);
    }
}

__noinline
static int
process_file_actions(struct spawn_args *psa)
{
	const spawn_file_actions_t fa = *psa->fa;
    spawn_file_actions_entry_t *fae;

    /* Replay all file descriptor modifications */
    STAILQ_FOREACH(fae, &fa->fa_list, fae_list)
	{
		int	error = process_file_actions_entry(psa, fae);
		if (error)
			return error;
	}
    return 0;
}

__noinline
static int
try_exec_shell(const char *prog,
			   char * const *argv,
			   char * const *envp)
{
	const char **memp;
	size_t		argc;

	for (argc = 0; argv[argc]; ++argc)
		continue;
	/* minimum 3 here is to handle argc==0 edge case */
	memp = alloca(MAX(3, argc + 2) * sizeof(char *));
	if (memp == NULL)
		return -1; /* can't actually happen */

	if (argc > 0)
	{
		memp[0] = argv[0];
		memp[1] = prog;
		bcopy(argv + 1, memp + 2, argc * sizeof(char *));
	}
	else
	{
		memp[0] = "sh";
		memp[1] = argv[0];
		memp[2] = NULL;
	}
	(void)_execve(_PATH_BSHELL,
				  __DECONST(char **, memp), envp);
	return -1;
}

/*
 * Returns 0 for "try the next path item", -1 (with errno set) for "abandon
 * all hope now". (Obviously, does not return at all on success.)
 *
 * In the 0 return case, EACCES is stored in *errorp in the event that the
 * error from execve() was EACCES _and_ the file could be accessed by stat(),
 * thus implying that access permissions existed on all the directories in the
 * path but not on the executable itself. Otherwise, *errorp is unchanged.
 */
__noinline
static int
try_exec(const char *prog,
		 char * const *argv,
		 char * const *envp,
		 int *errorp)
{
	(void)_execve(prog, argv, envp);

	switch (errno)
	{
		/* Error cases for which we do not retry. */
		case E2BIG:
		case ENOMEM:
		case ETXTBSY:  /* We used to retry for this, but sh(1) doesn't. */
			return -1;

		/* Error cases for which we do retry. */
		case ELOOP:
		case ENAMETOOLONG:
		case ENOENT:
		case ENOTDIR:
			return 0;

		/* This case ends any retries, we either succeed or fail here */
		case ENOEXEC:
			return try_exec_shell(prog, argv, envp);

		default:
		{
			int 		save_errno = errno;
			struct stat sb;

			/*
			 * EACCES may be for an inaccessible directory or
			 * a non-executable file.  Call stat() to decide
			 * which.  This also handles ambiguities for EFAULT
			 * and EIO, and undocumented errors like ESTALE.
			 * We hope that the race for a stat() is unimportant.
			 */
			if (_stat(prog, &sb) != 0)
				return 0;
			if (save_errno == EACCES)
			{
				*errorp = EACCES;
				return 0;
			}
			errno = save_errno;
			return -1;
		}
	}
}

__noinline
static int
execvPe(const char *name,
		const char *path,
		char * const *argv,
		char * const *envp)
{
	size_t		namelen;
    int			final_errno = ENOENT;
    char		buf[PATH_MAX];

    /* If it's an absolute or relative path name, it's easy. */
    if (strchr(name, '/'))
	{
		if (try_exec(name, argv, envp, &final_errno) == 0)
			errno = final_errno;
		return -1;
    }

	namelen = strlen(name);

    /* If it's an empty path name, fail in the usual POSIX way. */
    if (namelen == 0)
	{
		errno = ENOENT;
		return -1;
    }

	while (path)
	{
		const char *delim = strchrnul(path, ':');
		const char *pathptr = path;
		size_t		pathlen;

		/* set up for next time */
		if (*delim == '\0')
			path = NULL;
		else
			path = delim + 1;

		/*
		 * It's a SHELL path -- double, leading and trailing colons
		 * mean the current directory.
		 */
		if (delim == pathptr)
		{
			/* Double colon in the middle, or a leading or trailing colon. */
			pathptr = ".";
			pathlen = 1;
		}
		else
			/* Standard non-empty component. */
			pathlen = (size_t)(delim - pathptr);

		/*
		 * If the path is too long complain.  This is a possible
		 * security issue; given a way to make the path too long
		 * the user may execute the wrong program.
		 */
		if (pathlen + namelen + 2 > sizeof(buf))
		{
			(void)_write(STDERR_FILENO, STR_WITH_LEN("execvP: "));
			(void)_write(STDERR_FILENO, pathptr, pathlen);
			(void)_write(STDERR_FILENO, STR_WITH_LEN(": path too long\n"));
			continue;
		}

		bcopy(pathptr, buf, pathlen);
		buf[pathlen] = '/';
		bcopy(name, buf + pathlen + 1, namelen + 1);

		if (try_exec(buf, argv, envp, &final_errno) < 0)
			return -1;
    }

	errno = final_errno;
    return -1;
}



#if defined(__i386__) || defined(__amd64__)
/*
 * We assume that MINSIGSTKSZ is a good size for a couple of levels of
 * function calls without large local vars; add a moderate amount of slop plus
 * the size of the largest large stack var we will use.
 */
#define	RFORK_THREAD_STACK_SIZE \
	(MINSIGSTKSZ + 512 + MAX(PATH_MAX, KINFO_PROC_SIZE))
#endif

__noinline
static int
_spawn_thr(void *data)
{
    struct spawn_args *psa;
	int err = 0;

    psa = data;

    if (psa->is_vfork)
		err = process_signals_for_vfork(psa);
    else if (psa->sa != NULL)
		err = process_signals_for_rfork(psa);

    if (!err && psa->sa != NULL)
		err = process_spawnattr(psa);

    if (!err && psa->fa != NULL)
		err = process_file_actions(psa);

	if (!err)
	{
#ifdef COVERAGE
		if (psa->is_exec)
			__gcov_flush();
#endif
		if (psa->path)
			execvPe(psa->file, psa->path, psa->argv, psa->envp);
		else
			_execve(psa->file, psa->argv, psa->envp);
		err = report_err(psa, errno, psa->path ? "execvPe" : "_execve", psa->file);
	}

	psa->error = err;

	if (psa->verbose)
		report_err(psa, err, "spawn failed", NULL);

    /* This is called in such a way that it must not return. */
    _exit(127);
}

int
spawnP(pid_t *pidp,
	   const char *file,
	   const char *path,
       const spawn_file_actions_t *fa,
       const spawnattr_t *sa,
       char * const argv[],
	   char * const envp[])
{
	struct spawn_args psa;
	sigset_t	allsigs;
	sigset_t	oldsigs;
	pid_t		pid;
#ifdef RFORK_THREAD_STACK_SIZE
	char	   *stack;
	size_t		stacksz;

	stacksz = RFORK_THREAD_STACK_SIZE;
	if (path)
	    for (size_t argc = 0; argv[argc] != NULL; ++argc)
			stacksz += sizeof(char *);
	stacksz = (stacksz & ~(size_t)15);
	stack = malloc(stacksz);
	if (!stack)
		return errno;
	stacksz -= 16 + ((uintptr_t)stack & (uintptr_t)15);
#if defined(__i386__)
	stacksz += 4;
#endif
#endif

	psa.file = file;
	psa.fa = fa;
	psa.sa = sa;
	psa.argv = argv;
	psa.envp = envp ? envp : environ;
	psa.path = path;
	psa.is_vfork = false;
	psa.is_exec = false;
	psa.verbose = sa && (*sa)->sa_flags & SPAWN_VERBOSE_NP;
	psa.error = 0;

	/*
	 * Passing RFSPAWN to rfork(2) gives us effectively a vfork that drops
	 * non-ignored signal handlers.  We'll fall back to the slightly less
	 * ideal vfork(2) if we get an EINVAL from rfork -- this should only
	 * happen with newer libc on older kernel that doesn't accept
	 * RFSPAWN.
	 */
#ifdef RFORK_THREAD_STACK_SIZE
	/*
	 * x86 stores the return address on the stack, so rfork(2) cannot work
	 * as-is because the child would clobber the return address om the
	 * parent.  Because of this, we must use rfork_thread instead while
	 * almost every other arch stores the return address in a register.
	 */
	pid = rfork_thread((int)RFSPAWN, stack + stacksz, _spawn_thr, &psa);
	{
		int save_errno = errno;
		free(stack);
		errno = save_errno;
	}
#else
	pid = rfork((int)RFSPAWN);
	if (pid == 0)
		/* _spawn_thr does not return */
		_spawn_thr(&psa);
#endif

	/*
	 * The above block should leave us in a state where we've either
	 * succeeded and we're ready to process the results, or we need to
	 * fallback to vfork() if the kernel didn't like RFSPAWN.
	 */
	if (pid == -1 && errno == EINVAL)
	{
	    psa.is_vfork = true;

	    sigfillset(&allsigs);
	    if (__isthreaded)
			pthread_sigmask(SIG_BLOCK, &allsigs, &oldsigs);
	    else
			sigprocmask(SIG_BLOCK, &allsigs, &oldsigs);

	    if ((*sa)->sa_flags & SPAWN_SETSIGMASK)
			psa.vfmask = (*sa)->sa_sigmask;
	    else
			psa.vfmask = oldsigs;

	    pid = vfork();
	    if (pid == 0)
			_spawn_thr(&psa);	/* _spawn_thr does not return */
	    else
		{
			int save_errno = errno;
			if (__isthreaded)
				pthread_sigmask(SIG_SETMASK, &oldsigs, NULL);
			else
				sigprocmask(SIG_SETMASK, &oldsigs, NULL);
			errno = save_errno;
	    }
	}
	if (pid == -1)
		return errno;
	if (psa.error != 0)
		/* Failed; ready to reap */
		_waitpid(pid, NULL, WNOHANG);
	else if (pidp != NULL)
		/* exec succeeded */
		*pidp = pid;

	return psa.error;
}

static void _suspend_all_np(void) __attribute__((weakref("pthread_suspend_all_np")));

/*
 * Checks whether name looks like libthr.so[.n.n.n]
 */
static bool
looks_like_libthr(const char *name)
{
    const char *p = strrchr(name, '/');
    if (p == NULL)
		p = name;
    else
		++p;
    if (strncmp(p, STR_WITH_LEN("libthr.so")) != 0)
		return false;
    p += 9;
    if (*p != '.')
		return !*p;
    return (strspn(p, ".0123456789") == strlen(p));
}

#define SUSP_FUNC "pthread_suspend_all_np"

__noinline
static void
suspend_threads_the_hard_way(void)
{
    void (*func)(void) = NULL;

    /* Try the default lookup first. */
    func = (void (*)(void)) dlfunc(RTLD_DEFAULT, SUSP_FUNC);

    /* RTLD_NEXT might find an RTLD_LOCAL library loaded after us */
    if (func == NULL)
        func = (void (*)(void)) dlfunc(RTLD_NEXT, SUSP_FUNC);

    /*
     * Brute-force it by checking every single loaded object, starting with
     * anything that has a name like "libthr.so[.nnn]".
     */
    if (func == NULL)
    {
        Link_map   *map = NULL;

        if (dlinfo(RTLD_SELF, RTLD_DI_LINKMAP, &map) == 0)
        {
            while (map && map->l_prev)
                map = map->l_prev;

			Link_map   *pmap = map;

            while (map)
            {
                /* First pass, look only for libthr; second pass, try all */
                if (!pmap || looks_like_libthr(map->l_name))
                {
                    void *mod = dlopen(map->l_name, RTLD_LOCAL | RTLD_NOLOAD);
                    if (mod != NULL)
                    {
                        func = (void (*)(void)) dlfunc(mod, SUSP_FUNC);
                        if (func != NULL)
                            break;
                    }
                }
                map = map->l_next;
                if (!map)
				{
                    map = pmap;
					pmap = NULL;
				}
            }
        }
    }
    if (func == NULL)
    {
        /*
         * At this point it is a matter of complete bafflement to us how the
         * process managed to even become threaded at all. Maybe it has a
         * renamed copy of libthr with the functions renamed? Who knows.
         * Certainly there is nothing we can usefully do here; we're going to
         * rip out open files from underneath running threads in ways that
         * could result in garbage being written to bad places. Honestly, why
         * are you calling this from a threaded process in the first place,
         * let alone one with some magically hidden thread library?
         */
        abort();
    }

    /* Do it. */
    func();
}

void
spawnexecP(const char *file,
		   const char *path,
		   const spawn_file_actions_t *fa,
		   const spawnattr_t *sa,
		   char * const argv[],
		   char * const envp[])
{
	struct spawn_args psa;
	sigset_t	allsigs;
	sigset_t	oldsigs;

	psa.file = file;
	psa.fa = fa;
	psa.sa = sa;
	psa.argv = argv;
	psa.envp = envp ? envp : environ;
	psa.path = path;
	psa.error = 0;
	psa.verbose = !sa || (*sa)->sa_flags & SPAWN_VERBOSE_NP;

	/* Pretend we did a vfork. */
	psa.is_vfork = true;
	psa.is_exec = true;

	sigfillset(&allsigs);
	if (__isthreaded)
	{
	    pthread_sigmask(SIG_BLOCK, &allsigs, &oldsigs);
		if (_suspend_all_np != NULL)
			_suspend_all_np();
		else
			suspend_threads_the_hard_way();
	}
	else
	    sigprocmask(SIG_BLOCK, &allsigs, &oldsigs);

	if ((*sa)->sa_flags & SPAWN_SETSIGMASK)
	    psa.vfmask = (*sa)->sa_sigmask;
	else
	    psa.vfmask = oldsigs;

	/* _spawn_thr does not return */
	_spawn_thr(&psa);
}

int
spawn(pid_t *pid,
	  const char *file,
	  const spawn_file_actions_t *fa,
	  const spawnattr_t *sa,
	  char * const argv[],
	  char * const envp[])
{
    return spawnP(pid, file, NULL, fa, sa, argv, envp);
}

int
spawnp(pid_t *pid,
	   const char *file,
	   const spawn_file_actions_t *fa,
	   const spawnattr_t *sa,
	   char * const argv[],
	   char * const envp[])
{
    const char *path = getenv("PATH");
    if (!path)
		path = _PATH_DEFPATH;
    return spawnP(pid, file, path, fa, sa, argv, envp);
}

/*
 * File descriptor actions
 */

int
spawn_file_actions_init(spawn_file_actions_t *ret)
{
    spawn_file_actions_t fa;

    fa = malloc(sizeof(struct spawn_file_actions));
    if (fa == NULL)
		return -1;

    STAILQ_INIT(&fa->fa_list);
    *ret = fa;
    return 0;
}

int
spawn_file_actions_destroy(spawn_file_actions_t *fa)
{
    spawn_file_actions_entry_t *fae;

    while ((fae = STAILQ_FIRST(&(*fa)->fa_list)) != NULL)
	{
		/* Remove file action entry from the queue */
		STAILQ_REMOVE_HEAD(&(*fa)->fa_list, fae_list);

		/* Deallocate file action entry */
		free(fae);
    }

    free(*fa);
	*fa = NULL;
    return 0;
}


static int
alloc_action(spawn_file_actions_t * __restrict fa,
			 spawn_file_actions_entry_t * __restrict fae_in,
			 const char * __restrict path,
			 bool have_path)
{
    spawn_file_actions_entry_t *fae;
	size_t plen = 0;

	if (have_path)
		plen = strlen(path) + 1;

    fae = malloc(sizeof(spawn_file_actions_entry_t) + plen);
    if (fae == NULL)
		return errno;

	*fae = *fae_in;
	if (have_path)
	{
		char *npath = (char *)(fae + 1);
		memcpy(npath, path, plen);
		fae->fae_path = npath;
	}
	else
		fae->fae_path = NULL;

	STAILQ_INSERT_TAIL(&(*fa)->fa_list, fae, fae_list);
    return 0;
}

#define FAE_ALLOC(fa_,...) alloc_action(fa_, &((spawn_file_actions_entry_t){ __VA_ARGS__ }), NULL, false)
#define FAE_ALLOC_PATH(fa_,path_,...) alloc_action(fa_, &((spawn_file_actions_entry_t){ __VA_ARGS__ }), (path_), true)

int
spawn_file_actions_addopen(spawn_file_actions_t * __restrict fa,
						   int fildes,
						   const char * __restrict path,
						   int oflag,
						   mode_t mode)
{
    if (fildes < 0)
		return EBADF;

	return FAE_ALLOC_PATH(fa, path,
						  .fae_action	= FAE_OPEN,
						  .fae_fildes	= fildes,
						  .fae_atfd		= AT_FDCWD,
						  .fae_oflag	= oflag,
						  .fae_mode		= mode
		);
}

int
spawn_file_actions_addchdir(spawn_file_actions_t * __restrict fa,
							const char * __restrict path)
{
	return FAE_ALLOC_PATH(fa, path,
						  .fae_action	= FAE_CHDIR
		);
}

int
spawn_file_actions_addchroot_np(spawn_file_actions_t * __restrict fa,
								const char * __restrict path)
{
	return FAE_ALLOC_PATH(fa, path,
						  .fae_action	= FAE_CHROOT
		);
}

int
spawn_file_actions_addopenat_np(spawn_file_actions_t * __restrict fa,
								int fildes,
								int atfd,
								const char * __restrict path,
								int oflag,
								mode_t mode)
{
    if (fildes < 0 || (atfd < 0 && atfd != AT_FDCWD))
		return EBADF;

	return FAE_ALLOC_PATH(fa, path,
						  .fae_action	= FAE_OPEN,
						  .fae_fildes	= fildes,
						  .fae_atfd		= atfd,
						  .fae_oflag	= oflag,
						  .fae_mode		= mode
		);
}

int
spawn_file_actions_adddup2(spawn_file_actions_t *fa,
						   int fildes,
						   int newfildes)
{
    if (fildes < 0 || newfildes < 0)
		return EBADF;

	return FAE_ALLOC(fa,
					 .fae_action	= FAE_DUP2,
					 .fae_fildes	= newfildes,
					 .fae_atfd		= fildes
		);
}

int
spawn_file_actions_addclose(spawn_file_actions_t *fa,
							int fildes)
{
    if (fildes < 0)
		return EBADF;

	return FAE_ALLOC(fa,
					 .fae_action	= FAE_CLOSE,
					 .fae_fildes	= fildes
		);
}

int
spawn_file_actions_addfchdir(spawn_file_actions_t *fa,
							 int fildes)
{
    if (fildes < 0)
		return EBADF;

	return FAE_ALLOC(fa,
					 .fae_action	= FAE_FCHDIR,
					 .fae_fildes	= fildes
		);
}

int
spawn_file_actions_addsetpgrp_np(spawn_file_actions_t *fa,
								 int fildes)
{
    if (fildes < 0)
		return EBADF;

	return FAE_ALLOC(fa,
					 .fae_action	= FAE_SETPGRP,
					 .fae_fildes	= fildes
		);
}

int
spawn_file_actions_addclosefrom_np(spawn_file_actions_t *fa,
								   int fildes)
{
    if (fildes < 0)
		return EBADF;

	return FAE_ALLOC(fa,
					 .fae_action	= FAE_CLOSEFROM,
					 .fae_fildes	= fildes
		);
}

int
spawn_file_actions_addjail_np(spawn_file_actions_t *fa,
							  int jailid)
{
    if (jailid < 0)
		return EINVAL;

	return FAE_ALLOC(fa,
					 .fae_action	= FAE_JAIL,
					 .fae_fildes	= jailid
		);
}

/*
 * Spawn attributes
 */

int
spawnattr_init(spawnattr_t *ret)
{
	static const struct spawnattr nullsa;
	spawnattr_t sa;

	sa = malloc(sizeof(struct spawnattr));
	if (sa == NULL)
		return errno;

	/* Set defaults as specified by POSIX, cleared above */
	*sa = nullsa;
	sigfillset(&sa->sa_sigdefault);
	*ret = sa;
	return 0;
}

int
spawnattr_destroy(spawnattr_t *sa)
{
	if ((*sa)->sa_errprefix)
		free(__DECONST(void *, (*sa)->sa_errprefix));
	free(*sa);
	*sa = NULL;
	return 0;
}

int
spawnattr_getflags(const spawnattr_t * __restrict sa,
				   short * __restrict flags)
{
	*flags = (*sa)->sa_flags;
	return 0;
}

int
spawnattr_getpgroup(const spawnattr_t * __restrict sa,
					pid_t * __restrict pgroup)
{
	*pgroup = (*sa)->sa_pgroup;
	return 0;
}

int
spawnattr_getschedparam(const spawnattr_t * __restrict sa,
						struct sched_param * __restrict schedparam)
{
	*schedparam = (*sa)->sa_schedparam;
	return 0;
}

int
spawnattr_getschedpolicy(const spawnattr_t * __restrict sa,
						 int * __restrict schedpolicy)
{
	*schedpolicy = (*sa)->sa_schedpolicy;
	return 0;
}

int
spawnattr_getsigdefault(const spawnattr_t * __restrict sa,
						sigset_t * __restrict sigdefault)
{
	*sigdefault = (*sa)->sa_sigdefault;
	return 0;
}

int
spawnattr_getsigignore_np(const spawnattr_t * __restrict sa,
						  sigset_t * __restrict sigignore)
{
	*sigignore = (*sa)->sa_sigignore;
	return 0;
}

int
spawnattr_getsigmask(const spawnattr_t * __restrict sa,
					 sigset_t * __restrict sigmask)
{
	*sigmask = (*sa)->sa_sigmask;
	return 0;
}

int
spawnattr_getrlimit_np(const spawnattr_t * __restrict sa,
					int rlim,
					bool * __restrict isset,
					struct rlimit * __restrict val)
{
	if (rlim < 0 || rlim >= RLIM_NLIMITS)
		return EINVAL;
	*isset = (*sa)->sa_rlimits_set[rlim];
	*val = (*sa)->sa_rlimits[rlim];
	return 0;
}

int
spawnattr_setflags(spawnattr_t *sa, short flags)
{
	(*sa)->sa_flags = flags;
	return 0;
}

int
spawnattr_setpgroup(spawnattr_t *sa, pid_t pgroup)
{
	(*sa)->sa_pgroup = pgroup;
	return 0;
}

int
spawnattr_setschedparam(spawnattr_t * __restrict sa,
						const struct sched_param * __restrict schedparam)
{
	(*sa)->sa_schedparam = *schedparam;
	return 0;
}

int
spawnattr_setschedpolicy(spawnattr_t *sa, int schedpolicy)
{
	(*sa)->sa_schedpolicy = schedpolicy;
	return 0;
}

int
spawnattr_setsigdefault(spawnattr_t * __restrict sa,
						const sigset_t * __restrict sigdefault)
{
	(*sa)->sa_sigdefault = *sigdefault;
	return 0;
}

int
spawnattr_setsigignore_np(spawnattr_t * __restrict sa,
						  const sigset_t * __restrict sigignore)
{
	(*sa)->sa_sigignore = *sigignore;
	return 0;
}

int
spawnattr_setsigmask(spawnattr_t * __restrict sa,
					 const sigset_t * __restrict sigmask)
{
	(*sa)->sa_sigmask = *sigmask;
	return 0;
}

int
spawnattr_setrlimit_np(spawnattr_t * __restrict sa,
					   int rlim,
					   bool isset,
					   const struct rlimit * __restrict val)
{
	if (rlim < 0 || rlim >= RLIM_NLIMITS)
		return EINVAL;
	(*sa)->sa_rlimits_set[rlim] = isset;
	(*sa)->sa_rlimits[rlim] = *val;
	return 0;
}

int
spawnattr_seterrprefix_np(spawnattr_t * __restrict sa,
						  const char * __restrict pfx)
{
	(*sa)->sa_errprefix = strdup(pfx);
	return 0;
}
