
#include <sys/cdefs.h>
#include <sys/param.h>
#include <sys/sysctl.h>

int get_nfiles(void)
{
	int nfiles = 0;
	int mib[4];
	size_t len;

	len = sizeof(int);
	mib[0] = CTL_KERN;
	mib[1] = KERN_PROC;
	mib[2] = KERN_PROC_NFDS;
	mib[3] = 0;
	if (sysctl(mib, nitems(mib), &nfiles, &len, NULL, 0) < 0)
		return -1;
	if (len != sizeof(int))
		return -1;
	return nfiles;
}
