/*
*/
#ifndef PORTSIGNAL_H
#define PORTSIGNAL_H

/*Attention! This include MUST be BEFORE all SIG... deinitions!*/
#include <signal.h>

#define FATAL_SIG_ERROR 4



/*
 * Some of systems (especially LINUX) has too small number of
 * signals available to the system so some of the (!documented!) signals
 * are not defined. This file contains the definition of all signals used
 * in the program.
 * If the signal is not define we define it as unused (>NSIG).
 */

#ifndef NSIG
/* Must be enough:) :*/
#define NSIG (1024)
#endif

#ifndef SIGSEGV
#define SIGSEGV (NSIG+1)
#endif
#ifndef SIGFPE
#define SIGFPE (NSIG+2)
#endif
#ifndef SIGILL
#define SIGILL (NSIG+3)
#endif
#ifndef SIGEMT
#define SIGEMT (NSIG+4)
#endif
#ifndef SIGSYS
#define SIGSYS (NSIG+5)
#endif
#ifndef SIGPIPE
#define SIGPIPE  (NSIG+6)
#endif
#ifndef SIGLOST
#define SIGLOST (NSIG+7)
#endif
#ifndef SIGXCPU
#define SIGXCPU  (NSIG+8)
#endif
#ifndef SIGXFSZ
#define SIGXFSZ (NSIG+9)
#endif
#ifndef SIGTERM
#define SIGTERM (NSIG+10)
#endif
#ifndef SIGINT
#define SIGINT (NSIG+11)
#endif
#ifndef SIGQUIT
#define SIGQUIT (NSIG+12)
#endif
#ifndef SIGHUP
#define SIGHUP (NSIG+13)
#endif
#ifndef SIGALRM
#define SIGALRM (NSIG+14)
#endif
#ifndef SIGVTALRM
#define SIGVTALRM (NSIG+15)
#endif
#ifndef SIGPROF
#define SIGPROF (NSIG+16)
#endif

#endif
