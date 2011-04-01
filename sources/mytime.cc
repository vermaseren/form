#include <sys/time.h>
#include <cstdlib>

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
