#-
* Print an estimate for form units per hour on the current machine.
* The definition of 1 form unit is:
*
*   1fu = performing a trace with 14 Dirac's gamma matrices 3600 times.
*
* This program is based on aap1000.frm.
*
* Caveat: the number obtained by this program may (strongly) depend on the
* number of CPUs, buffer sizes as well as other environmental conditions.
* Parallel versions can have relatively large overheads for such simple tasks.
* MPI implementations tend to use busy-wait on blocking operations, which
* suppresses the number. Moreover, here we use the CPU time instead of the real
* time.

*--#[ Fuph :

#ifndef `N'
  #define N "192"
#endif

#define NUM "14"

#ifdef `QUIET'
  Off stats;
#endif

#procedure FormatFloat(x,str,n)
  #$x = `x';
  #do i=0,`n'
    #$i = integer_($x);
    #$x = ($x - $i) * 10;
    #if `i' == 0
      #redefine `str' "`$i'."
    #else
      #redefine `str' "``str''`$i'"
    #endif
  #enddo
#endprocedure

I m1,...,m`NUM';
S x,j;
CF f;

#$t0 = `timer_';

L FF = sum_(j,1,`N',f(j));
.sort
id f(x?) = 1;
multiply g_(1,m1,...,m`NUM');
trace4,1;
.sort
Drop;

#$t = (`timer_' - `$t0') / 1000;
#$fu = `N' / 3600;
#$fuph = $fu / $t * 3600;

#define t
#call FormatFloat($t,t,3)
#define fu
#call FormatFloat($fu,fu,6)
#define fuph
#call FormatFloat($fuph,fuph,2)

Format 120;

#if `NTHREADS_' >= 2
  #write "  `fu' form units in `t' seconds (total cpu time) with {`NTHREADS_'-1} workers"
  #write "  corresponding to `fuph' form units per hour per core"
#elseif `NPARALLELTASKS_' >= 2
  #write "  `fu' form units in `t' seconds (total cpu time) with `NPARALLELTASKS_' processes"
  #write "  corresponding to `fuph' form units per hour per core"
#else
  #write "  `fu' form units in `t' seconds (cpu time)"
  #write "  corresponding to `fuph' form units per hour"
#endif

*--#] Fuph :
.end
