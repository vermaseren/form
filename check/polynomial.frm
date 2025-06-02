#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

* Tests of the polynomial routines, with flint and poly

*--#[ polynomial_prc :
#-
#ifndef `NVARS'
	#define NVARS "2"
#endif
Off statistics;
Symbol y1,...,y`NVARS';
CFunction tmp,prf,fa;


#procedure genpoly(NPOLYS,NVARS,NEGPOW,MAXPOW,MAXCOEFF,NTERMS)
* Generate random polynomials for input
* Make sure we include some special cases
#$p1 = 0;
#$p2 = 1;
#$p3 = -1;
#$p4 = 1/19;
#$p5 = -1/19;
#do i = 6,`NPOLYS'
	#$TERMS = random_(`NTERMS');
	#$p`i' =
		#do t = 1,`$TERMS'
*			"tmp" wrapper stops the generation of two terms each
*			iter, since random_ is evaluated after bracket expansion
			+ tmp(random_(2*`MAXCOEFF')-`MAXCOEFF')
			#do v = 1,`NVARS'
				* y`v'^(random_(`NEGPOW'+`MAXPOW'+1)-1-`NEGPOW')
			#enddo
		#enddo
		;
#enddo
#inside <$p1>,...,<$p`NPOLYS'>
	Identify tmp(y1?) = y1;
#endinside
*#do i = 1,`NPOLYS'
*	#message p`i' = `$p`i''
*#enddo
#endprocedure


* testijaik: a common factor between arguments
#procedure testijaik(op,imin,imax,jmin,jmax,kmin,kmax)

#reset timer
On flint;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `k' >= `j'
	Local flint`op'p`i'p`j'p`k' = `op'_($p`i'*$p`j',$p`i'*$p`k');
#endif
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op': flint runtime: `TIMER_'

#reset timer
Off flint;
Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `k' >= `j'
	Local poly`op'p`i'p`j'p`k' = `op'_($p`i'*$p`j',$p`i'*$p`k');
#endif
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op':  poly runtime: `TIMER_'

Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `k' >= `j'
	Local diff`op'p`i'p`j'p`k' = flint`op'p`i'p`j'p`k' - poly`op'p`i'p`j'p`k';
	#if `op' == gcd
		Local diff2`op'p`i'p`j'p`k' = flint`op'p`i'p`j'p`k' + poly`op'p`i'p`j'p`k';
	#endif
#endif
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort

#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `k' >= `j'
	#if `op' == gcd
		#if (`ZERO_diff`op'p`i'p`j'p`k'' == 0) && (`ZERO_diff2`op'p`i'p`j'p`k'' == 1)
			#message Warning: sign difference in `op' p`i'p`j'p`k'
		#elseif (`ZERO_diff`op'p`i'p`j'p`k'' == 0) && (`ZERO_diff2`op'p`i'p`j'p`k'' == 0)
			#message Error: difference in `op' p`i'p`j'p`k'
			Print +s flint`op'p`i'p`j'p`k';
			Print +s poly`op'p`i'p`j'p`k';
			Print +s diff`op'p`i'p`j'p`k';
			.sort
			#terminate
		#endif
	#else
		#if `ZERO_diff`op'p`i'p`j'p`k'' == 0
			#message Error: difference in `op' p`i'p`j'p`k'
			Print flint`op'p`i'p`j'p`k';
			Print poly`op'p`i'p`j'p`k';
			Print diff`op'p`i'p`j'p`k';
			.sort
			#terminate
		#endif
	#endif
#endif
#enddo
#enddo
#enddo
Drop;
.sort
#message `op': OK
#endprocedure


* testiaj
#procedure testiaj(op,imin,imax,jmin,jmax)

#reset timer
On flint;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	Local flint`op'p`i'p`j' = `op'_($p`i',$p`j');
#endif
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op': flint runtime: `TIMER_'

#reset timer
Off flint;
Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	Local poly`op'p`i'p`j' = `op'_($p`i',$p`j');
#endif
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op':  poly runtime: `TIMER_'

Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	Local diff`op'p`i'p`j' = flint`op'p`i'p`j' - poly`op'p`i'p`j';
#endif
#enddo
#enddo
ModuleOption inparallel;
.sort

#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	#if `ZERO_diff`op'p`i'p`j'' == 0
		#message Error: difference in `op' p`i'p`j'
		Print flint`op'p`i'p`j';
		Print poly`op'p`i'p`j';
		Print diff`op'p`i'p`j';
		.sort
		#terminate
	#endif
#endif
#enddo
#enddo
Drop;
.sort
#message `op': OK
#endprocedure


* testijaj: the second arg divides the first
#procedure testijaj(op,imin,imax,jmin,jmax)

#reset timer
On flint;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	Local flint`op'p`i'p`j' = `op'_($p`i'*$p`j',$p`j');
#endif
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op': flint runtime: `TIMER_'

#reset timer
Off flint;
Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	Local poly`op'p`i'p`j' = `op'_($p`i'*$p`j',$p`j');
#endif
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op':  poly runtime: `TIMER_'

Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	Local diff`op'p`i'p`j' = flint`op'p`i'p`j' - poly`op'p`i'p`j';
#endif
#enddo
#enddo
ModuleOption inparallel;
.sort

#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	#if `ZERO_diff`op'p`i'p`j'' == 0
		#message Error: difference in `op' p`i'p`j'
		Print flint`op'p`i'p`j';
		Print poly`op'p`i'p`j';
		Print diff`op'p`i'p`j';
		.sort
		#terminate
	#endif
#endif
#enddo
#enddo
Drop;
.sort
#message `op': OK
#endprocedure


* testijak: the second arg usually has higher degree than the first
#procedure testijak(op,imin,imax,jmin,jmax,kmin,kmax)

#reset timer
On flint;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `j' >= `i'
	Local flint`op'p`i'p`j'p`k' = `op'_($p`i'*$p`j',$p`k');
#endif
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op': flint runtime: `TIMER_'

#reset timer
Off flint;
Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `j' >= `i'
	Local poly`op'p`i'p`j'p`k' = `op'_($p`i'*$p`j',$p`k');
#endif
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort
#message `op':  poly runtime: `TIMER_'

Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `j' >= `i'
	Local diff`op'p`i'p`j'p`k' = flint`op'p`i'p`j'p`k' - poly`op'p`i'p`j'p`k';
#endif
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort

#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#if `j' >= `i'
	#if `ZERO_diff`op'p`i'p`j'p`k'' == 0
		#message Error: difference in `op' p`i'p`j'p`k'
		Print flint`op'p`i'p`j'p`k';
		Print poly`op'p`i'p`j'p`k';
		Print diff`op'p`i'p`j'p`k';
		.sort
		#terminate
	#endif
#endif
#enddo
#enddo
#enddo
Drop;
.sort
#message `op': OK
#endprocedure


* Here it is hard to compare flint and poly, since FORM caches the factorization.
* For now, check at least that nothing crashes and valgrind is clean.
#procedure testfactij(imin,imax,jmin,jmax)

#reset timer
On flint;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	Local flintp`i'p`j' = fa($p`i'*$p`j');
#endif
#enddo
#enddo
FactArg fa;
ModuleOption inparallel;
.sort
#message factarg: flint runtime: `TIMER_'

Drop;
.sort
#message factarg: OK
#endprocedure


#procedure testprf(op,imin,imax,jmin,jmax,kmin,kmax,lmin,lmax)

#reset timer
On flint;
PolyRatFun prf;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#do l = `lmin',`lmax'
#if `j' >= `i'
#if `l' >= `i'
	Local flintprf`op'p`i'p`j'p`k'p`l' =
		#if `op' == norm
			+ prf($p`i'*$p`j',$p`k') * prf($p`k',$p`i'*$p`l')
		#elseif `op' == add
			+ prf($p`i'*$p`j',$p`k') + prf($p`k',$p`i'*$p`l')
		#endif
	;
#endif
#endif
#enddo
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort
#message prf `op': flint runtime: `TIMER_'

#reset timer
Off flint;
PolyRatFun prf;
Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#do l = `lmin',`lmax'
#if `j' >= `i'
#if `l' >= `i'
	Local polyprf`op'p`i'p`j'p`k'p`l' =
		#if `op' == norm
			+ prf($p`i'*$p`j',$p`k') * prf($p`k',$p`i'*$p`l')
		#elseif `op' == add
			+ prf($p`i'*$p`j',$p`k') + prf($p`k',$p`i'*$p`l')
		#endif
	;
#endif
#endif
#enddo
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort
#message prf `op':  poly runtime: `TIMER_'

PolyRatFun;
Hide;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#do l = `lmin',`lmax'
#if `j' >= `i'
#if `l' >= `i'
	Local diffprf`op'p`i'p`j'p`k'p`l' =
		+ flintprf`op'p`i'p`j'p`k'p`l'
		- polyprf`op'p`i'p`j'p`k'p`l'
	;
#endif
#endif
#enddo
#enddo
#enddo
#enddo
ModuleOption inparallel;
.sort

#do i = `imin',`imax'
#do j = `jmin',`jmax'
#do k = `kmin',`kmax'
#do l = `lmin',`lmax'
#if `j' >= `i'
#if `l' >= `i'
	#if `ZERO_diffprf`op'p`i'p`j'p`k'p`l'' == 0
		#message Error: difference in prf `op' p`i'p`j'p`k'p`l'
		Print flintprf`op'p`i'p`j'p`k'p`l';
		Print polyprf`op'p`i'p`j'p`k'p`l';
		Print diffprf`op'p`i'p`j'p`k'p`l';
		.sort
		#terminate
	#endif
#endif
#endif
#enddo
#enddo
#enddo
#enddo
Drop;
.sort
#message prf `op': OK
#endprocedure


#procedure testinv(x3min,x3max,x1min,x1max)

#reset timer
On flint;
#do x3 = `x3min',`x3max'
#do x1 = `x1min',`x1max'
*	Construct an x2 which produces an inverse without crashing FORM
	#$arg2 = $p`x1'*$p`x3'-1;
*	Avoid 0 for second arg:
	#if `$arg2' == 0
		Local flintinvp`x3'p`x1' = 0;
	#else
		Local flintinvp`x3'p`x1' = inverse_($p`x1',`$arg2');
	#endif
#enddo
#enddo
ModuleOption inparallel;
.sort
#message inverse: flint runtime: `TIMER_'

#reset timer
Off flint;
Hide;
#do x3 = `x3min',`x3max'
#do x1 = `x1min',`x1max'
*	Construct an x2 which produces an inverse without crashing FORM
	#$arg2 = $p`x1'*$p`x3'-1;
*	Avoid 0 for second arg:
	#if `$arg2' == 0
		Local polyinvp`x3'p`x1' = 0;
	#else
		Local polyinvp`x3'p`x1' = inverse_($p`x1',`$arg2');
	#endif
#enddo
#enddo
ModuleOption inparallel;
.sort
#message inverse: poly runtime: `TIMER_'

Hide;
#do x3 = `x3min',`x3max'
#do x1 = `x1min',`x1max'
	Local diffinvp`x3'p`x1' = flintinvp`x3'p`x1' - polyinvp`x3'p`x1';
#enddo
#enddo
ModuleOption inparallel;
.sort

#do x3 = `x3min',`x3max'
#do x1 = `x1min',`x1max'
	#if `ZERO_diffinvp`x3'p`x1'' == 0
		#message Error: difference in inv p`x3'p`x1'
		Print flintinvp`x3'p`x1';
		Print polyinvp`x3'p`x1';
		Print diffinvp`x3'p`x1';
		.sort
	#endif
#enddo
#enddo

Drop;
.sort
#message inv: OK
#endprocedure
*--#] polynomial_prc :
*--#[ polynomial_gcd_nvar_1 :
#-
#define NPOLYS "16"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijaik(gcd,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("gcd: OK")
*--#] polynomial_gcd_nvar_1 :
*--#[ polynomial_gcd_nvar_2 :
#-
#define NPOLYS "12"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijaik(gcd,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("gcd: OK")
*--#] polynomial_gcd_nvar_2 :
*--#[ polynomial_gcd_nvar_5 :
#-
#define NPOLYS "9"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "9"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijaik(gcd,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("gcd: OK")
*--#] polynomial_gcd_nvar_5 :
*--#[ polynomial_mul_nvar_1 :
#-
#define NPOLYS "80"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "60"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testiaj(mul,2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("mul: OK")
*--#] polynomial_mul_nvar_1 :
*--#[ polynomial_mul_nvar_2 :
#-
#define NPOLYS "40"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "60"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testiaj(mul,2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("mul: OK")
*--#] polynomial_mul_nvar_2 :
*--#[ polynomial_mul_nvar_5 :
#-
#define NPOLYS "25"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "60"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testiaj(mul,2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("mul: OK")
*--#] polynomial_mul_nvar_5 :
*--#[ polynomial_div_nvar_1 :
#-
#define NPOLYS "80"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "60"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijaj(div,2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("div: OK")
*--#] polynomial_div_nvar_1 :
*--#[ polynomial_div_nvar_2 :
#-
#define NPOLYS "40"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "50"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijaj(div,2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("div: OK")
*--#] polynomial_div_nvar_2 :
*--#[ polynomial_div_nvar_5 :
#-
#define NPOLYS "25"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "50"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijaj(div,2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("div: OK")
*--#] polynomial_div_nvar_5 :
*--#[ polynomial_rem_nvar_1 :
#-
#define NPOLYS "20"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijak(rem,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("rem: OK")
*--#] polynomial_rem_nvar_1 :
*--#[ polynomial_rem_nvar_2 :
#-
#define NPOLYS "15"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijak(rem,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("rem: OK")
*--#] polynomial_rem_nvar_2 :
*--#[ polynomial_rem_nvar_5 :
#-
#define NPOLYS "10"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testijak(rem,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("rem: OK")
*--#] polynomial_rem_nvar_5 :
*--#[ polynomial_factarg_nvar_1 :
#-
#define NPOLYS "30"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testfactij(2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
# This takes too long when running without FLINT under lcov. Skip if valgrind, and hence lcov.
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 2.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factarg: OK")
*--#] polynomial_factarg_nvar_1 :
*--#[ polynomial_factarg_nvar_2 :
#-
#define NPOLYS "20"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testfactij(2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
# This takes too long when running without FLINT under lcov. Skip if valgrind, and hence lcov.
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 3.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factarg: OK")
*--#] polynomial_factarg_nvar_2 :
*--#[ polynomial_factarg_nvar_5 :
#-
#define NPOLYS "12"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testfactij(2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
# This takes too long when running without FLINT under lcov. Skip if valgrind, and hence lcov.
# This one is not valgrind clean for flint < 3.2.1 !
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 3.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factarg: OK")
*--#] polynomial_factarg_nvar_5 :
*--#[ polynomial_prf_norm_nvar_1 :
#-
#define NPOLYS "9"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testprf(norm,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("prf norm: OK")
*--#] polynomial_prf_norm_nvar_1 :
*--#[ polynomial_prf_norm_nvar_2 :
#-
#define NPOLYS "9"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testprf(norm,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("prf norm: OK")
*--#] polynomial_prf_norm_nvar_2 :
*--#[ polynomial_prf_norm_nvar_5 :
#-
#define NPOLYS "7"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "10"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testprf(norm,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("prf norm: OK")
*--#] polynomial_prf_norm_nvar_5 :
*--#[ polynomial_prf_add_nvar_1 :
#-
#define NPOLYS "9"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testprf(add,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("prf add: OK")
*--#] polynomial_prf_add_nvar_1 :
*--#[ polynomial_prf_add_nvar_2 :
#-
#define NPOLYS "9"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testprf(add,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("prf add: OK")
*--#] polynomial_prf_add_nvar_2 :
*--#[ polynomial_prf_add_nvar_5 :
#-
#define NPOLYS "7"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "10"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testprf(add,2,`NPOLYS',2,`NPOLYS',2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("prf add: OK")
*--#] polynomial_prf_add_nvar_5 :
*--#[ polynomial_inverse_nvar_1 :
#-
#define NPOLYS "30"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "20"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testinv(1,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("inv: OK")
*--#] polynomial_inverse_nvar_1 :
