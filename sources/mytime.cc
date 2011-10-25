#include <sys/time.h>
#include <cstdlib>

#ifndef mBSD
/* timersub is not in POSIX, but presents on most BSD derivatives.
   This implementation is borrowed from glibc. (TU 23 Oct 2011) */
#define timersub(a, b, result) \
	do { \
		(result)->tv_sec = (a)->tv_sec - (b)->tv_sec; \
		(result)->tv_usec = (a)->tv_usec - (b)->tv_usec; \
		if ((result)->tv_usec < 0) { \
			--(result)->tv_sec; \
			(result)->tv_usec += 1000000; \
		} \
	} while (0)
#endif

bool starttime_set = false;
timeval starttime;

double thetime () {
	if (!starttime_set) {
		gettimeofday(&starttime,NULL);
		starttime_set=true;
	}
	
  timeval now,diff;
  gettimeofday(&now,NULL);
  timersub(&now,&starttime,&diff);
  return diff.tv_sec+diff.tv_usec/1000000.0;
}
