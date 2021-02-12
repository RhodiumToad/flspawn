/*
 * myspawn.h
 */

#ifndef H_7ECB5AE8_9FAB_11EA_A8A6_6CF049962D5A_
#define H_7ECB5AE8_9FAB_11EA_A8A6_6CF049962D5A_

#include <sys/cdefs.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <stddef.h>
#include <fcntl.h>
#include <signal.h>
#include <sched.h>
#include <stdbool.h>

typedef struct spawnattr		*spawnattr_t;
typedef struct spawn_file_actions	*spawn_file_actions_t;

#define SPAWN_RESETIDS		0x0001
#define SPAWN_SETPGROUP		0x0002
#define SPAWN_SETSCHEDPARAM	0x0004
#define SPAWN_SETSCHEDULER	0x0008
#define SPAWN_SETSIGDEF		0x0010
#define SPAWN_SETSIGMASK	0x0020
#define SPAWN_VERBOSE_NP	0x0100
#define SPAWN_SETSID_NP		0x0200
#define SPAWN_SETSIGIGN_NP	0x0400
#define SPAWN_SETRLIMITS_NP	0x0800

int spawn(pid_t * __restrict, const char * __restrict,
    const spawn_file_actions_t *, const spawnattr_t * __restrict,
    char * const [], char * const []);
int spawnp(pid_t * __restrict, const char * __restrict,
    const spawn_file_actions_t *, const spawnattr_t * __restrict,
    char * const [], char * const []);
int spawnP(pid_t * __restrict, const char * __restrict,
	   const char * __restrict,
	   const spawn_file_actions_t *,
	   const spawnattr_t * __restrict,
	   char * const [], char * const []);
void spawnexecP(const char * __restrict,
		const char * __restrict,
		const spawn_file_actions_t *,
		const spawnattr_t * __restrict,
		char * const [], char * const []);

/*
 * File descriptor actions
 */
int spawn_file_actions_init(spawn_file_actions_t *);
int spawn_file_actions_destroy(spawn_file_actions_t *);

int spawn_file_actions_addopen(spawn_file_actions_t * __restrict,
    int, const char * __restrict, int, mode_t);
int spawn_file_actions_addopenat_np(spawn_file_actions_t * __restrict,
				    int, int, const char * __restrict,
				    int, mode_t);
int spawn_file_actions_adddup2(spawn_file_actions_t *, int, int);
int spawn_file_actions_addclose(spawn_file_actions_t *, int);
int spawn_file_actions_addclosefrom_np(spawn_file_actions_t *, int);
int spawn_file_actions_addfchdir(spawn_file_actions_t *, int);
int spawn_file_actions_addchdir(spawn_file_actions_t *, const char *);
int spawn_file_actions_addchroot_np(spawn_file_actions_t *, const char *);
int spawn_file_actions_addsetpgrp_np(spawn_file_actions_t *, int);
int spawn_file_actions_addjail_np(spawn_file_actions_t *, int);

/*
 * Spawn attributes
 */
int spawnattr_init(spawnattr_t *);
int spawnattr_destroy(spawnattr_t *);

int spawnattr_getflags(const spawnattr_t * __restrict,
    short * __restrict);
int spawnattr_getpgroup(const spawnattr_t * __restrict,
    pid_t * __restrict);
int spawnattr_getschedparam(const spawnattr_t * __restrict,
    struct sched_param * __restrict);
int spawnattr_getschedpolicy(const spawnattr_t * __restrict,
    int * __restrict);
int spawnattr_getsigdefault(const spawnattr_t * __restrict,
    sigset_t * __restrict);
int spawnattr_getsigignore_np(const spawnattr_t * __restrict,
    sigset_t * __restrict);
int spawnattr_getsigmask(const spawnattr_t * __restrict,
    sigset_t * __restrict sigmask);
int spawnattr_getrlimit_np(const spawnattr_t * __restrict,
						   int,
						   bool * __restrict,
						   struct rlimit * __restrict);

int spawnattr_setflags(spawnattr_t *, short);
int spawnattr_setpgroup(spawnattr_t *, pid_t);
int spawnattr_setschedparam(spawnattr_t * __restrict,
    const struct sched_param * __restrict);
int spawnattr_setschedpolicy(spawnattr_t *, int);
int spawnattr_setsigdefault(spawnattr_t * __restrict,
    const sigset_t * __restrict);
int spawnattr_setsigignore_np(spawnattr_t * __restrict,
    const sigset_t * __restrict);
int spawnattr_setsigmask(spawnattr_t * __restrict,
    const sigset_t * __restrict);
int spawnattr_setrlimit_np(spawnattr_t * __restrict,
						   int,
						   bool,
						   const struct rlimit * __restrict);
int spawnattr_seterrprefix_np(spawnattr_t * __restrict,
							  const char * __restrict);

#endif
