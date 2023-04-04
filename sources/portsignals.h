#ifndef PORTSIGNAL_H
#define PORTSIGNAL_H

/** @file portsignals.h
 *
 *  Contains definitions for signals used/intercepted in FORM.
 *
 *	Some systems (especially LINUX) have not enough 
 *	signals available so some of the (!documented!) signals
 *	are not defined. This file contains the definition of all
 *	signals used in the program.
 *	If the signal is not defined we define it as unused (>NSIG).
 *
 *	The include of signal.h must be first, before we try to define
 *	undefined signals.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2017 J.A.M. Vermaseren
 *   When using this file you are requested to refer to the publication
 *   J.A.M.Vermaseren "New features of FORM" math-ph/0010025
 *   This is considered a matter of courtesy as the development was paid
 *   for by FOM the Dutch physics granting agency and we would like to
 *   be able to track its scientific use to convince FOM of its value
 *   for the community.
 *
 *   This file is part of FORM.
 *
 *   FORM is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 *   FORM is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *   details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with FORM.  If not, see <http://www.gnu.org/licenses/>.
 */
/* #] License : */ 

#include <signal.h>

#define FATAL_SIG_ERROR 4

#ifndef NSIG
/*
	The value of NSIG must be enough to fall outside the range of defined signals
*/
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
