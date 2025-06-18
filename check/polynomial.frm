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


#procedure testfactdolij(imin,imax,jmin,jmax)

#reset timer
On flint;
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	#$flintp`i'p`j' = $p`i'*$p`j';
	#factdollar $flintp`i'p`j'
#endif
#enddo
#enddo
.sort
#message factdollar: flint runtime: `TIMER_'
#reset timer
Off flint;
.sort
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	#$polyp`i'p`j' = $p`i'*$p`j';
	#factdollar $polyp`i'p`j'
#endif
#enddo
#enddo
.sort
#message factdollar: poly runtime: `TIMER_'
#do i = `imin',`imax'
#do j = `jmin',`jmax'
#if `j' >= `i'
	#if `$flintp`i'p`j'[0]' != `$polyp`i'p`j'[0]'
		#message Error: factor count difference in factdollar p`i'p`j'
		#message $flintp`i'`j'[0] = `$flintp`i'p`j'[0]'
		#message $polyp`i'p`j'[0] = `$polyp`i'p`j'[0]'
		#terminate
	#endif
	#do f = 1,`$flintp`i'p`j'[0]'
		#$diffp`i'p`j' = (`$flintp`i'p`j'[`f']') - (`$polyp`i'p`j'[`f']');
		#if `$diffp`i'p`j'' != 0
			#message Error: factor difference in factdollar p`i'p`j' f`f'
		#message $flintp`i'`j'[`f'] = `$flintp`i'p`j'[`f']'
		#message $polyp`i'p`j'[`f'] = `$polyp`i'p`j'[`f']'
		#terminate
		#endif
	#enddo
#endif
#enddo
#enddo

#message factdollar: OK
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
#define NPOLYS "12"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
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
#define NPOLYS "10"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
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
#define NPOLYS "8"
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
#define NPOLYS "60"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "40"
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
#define NPOLYS "30"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "40"
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
#define NPOLYS "20"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "40"
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
#define NPOLYS "60"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "40"
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
#define NPOLYS "30"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "35"
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
#define NPOLYS "20"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "35"
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
#define NPOLYS "16"
#define NVARS "1"
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
*--#] polynomial_rem_nvar_1 :
*--#[ polynomial_rem_nvar_2 :
#-
#define NPOLYS "12"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "10"
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
#define NPOLYS "8"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "10"
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
#define NPOLYS "25"
#define NVARS "1"
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
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 2.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factarg: OK")
*--#] polynomial_factarg_nvar_1 :
*--#[ polynomial_factarg_nvar_2 :
#-
#define NPOLYS "15"
#define NVARS "2"
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
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 3.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factarg: OK")
*--#] polynomial_factarg_nvar_2 :
*--#[ polynomial_factarg_nvar_5 :
#-
#define NPOLYS "10"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "10"
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
*--#[ polynomial_factdol_nvar_1 :
#-
#define NPOLYS "25"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "14"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testfactdolij(2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
# This takes too long when running without FLINT under lcov. Skip if valgrind, and hence lcov.
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 2.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factdollar: OK")
*--#] polynomial_factdol_nvar_1 :
*--#[ polynomial_factdol_nvar_2 :
#-
#define NPOLYS "15"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "14"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testfactdolij(2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
# This takes too long when running without FLINT under lcov. Skip if valgrind, and hence lcov.
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 2.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factdollar: OK")
*--#] polynomial_factdol_nvar_2 :
*--#[ polynomial_factdol_nvar_5 :
#-
#define NPOLYS "10"
#define NVARS "5"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "9"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testfactdolij(2,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
# This takes too long when running without FLINT under lcov. Skip if valgrind, and hence lcov.
#pend_if valgrind?
# This needs longer if running without flint.
#time_dilation 2.0
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("factdollar: OK")
*--#] polynomial_factdol_nvar_5 :
*--#[ polynomial_prf_norm_nvar_1 :
#-
#define NPOLYS "8"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
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
#define NPOLYS "8"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
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
#define NTERMS "9"
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
#define NPOLYS "8"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
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
#define NPOLYS "8"
#define NVARS "2"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
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
#define NTERMS "8"
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
#define NPOLYS "20"
#define NVARS "1"
#define NEGPOW "0"
#define MAXPOW "10"
#define MAXCOEFF "100"
#define NTERMS "15"
#include polynomial.frm # polynomial_prc
#call genpoly(`NPOLYS',`NVARS',`NEGPOW',`MAXPOW',`MAXCOEFF',`NTERMS')
#call testinv(1,`NPOLYS',2,`NPOLYS')
.end
#pend_if wordsize == 2 || mpi?
assert succeeded? || warning?("FORM was not built with FLINT support.")
assert stdout =~ exact_pattern("inv: OK")
*--#] polynomial_inverse_nvar_1 :
*--#[ polynomial_extra_1 :
* Assorted additional tests, to fully cover flintinterface.cc with just this test set
Symbol w,x,y,z;
CFunction f,g,prf;

* Negative powers and fast-notation symbols
Local test1 = prf(1/x + y + z,w);
Local test2 = prf(1/x + x + 1,x);
* Zero conversion
#$zero = 0;
Local test3 = mul_($zero,1);
* GCD which must fit in a term: I don't think this is reachable
*Local test4 = f(gcd_((x+1)^2,(x+1)*(x-1)));
*Local test5 = f(gcd_((x+1)^2,(x+1)*(y-1)));
* Long integer conversion with different limb counts
Local test6 =
	+ g(1)*prf(y+2^32-1,1)
	+ g(2)*prf(y+2^64-1,1)
	+ g(3)*prf(y+2^96-1,1)
	+ g(4)*prf(y+2^128-1,1)
	+ g(5)*prf(y+2^160-1,1);
* Output coeff has longer denominator than numerator:
Local test7 = mul_(1+x,1/2^64+x) + mul_(y+x,y/2^64+x);
* Result is 0
Local test8 = prf(1+x,1) + prf(-1-x,1);
Local test9 = prf(y+x,1) + prf(-y-x,1);
* Result is fast notation
Local test10 = prf(x+1,2) + prf(x-1,2);
Local test11 = prf(x+y,2) + prf(x-y,2);
* Copy non-prf term data, absorb coefficient
Local test12 = prf(x+1,1) + prf(x-1,1)*(f(1)*g(1)*x*3/2);
Local test13 = prf(x+y,1) + prf(x-y,1)*(f(1)*g(1)*x*3/2);

.sort
PolyRatFun prf;

Print;
.end
#pend_if mpi?
assert succeeded?
assert result("test1")  =~ expr("prf(x*y + x*z + 1,w*x)")
assert result("test2")  =~ expr("prf(x^2 + x + 1,x^2)")
assert result("test3")  =~ expr("0")
# assert result("test4")  =~ expr("f(1 + x)*prf(1,1)")
# assert result("test5")  =~ expr("f(1 + x)*prf(1,1)")
# assert result("test6")  =~ expr("g(1)*prf(y + 4294967295,1) + g(2)*prf(y + 18446744073709551615,1) + g(3)*prf(y + 79228162514264337593543950335,1) + g(4)*prf(y + 340282366920938\463463374607431768211455,1) + g(5)*prf(y + 14615016373309029182036848327\16283019655932542975,1);")
assert result("test7")  =~ expr("y^2*prf(1,18446744073709551616) + x*prf(18446744073709551617,18446744073709551616) + x*y*prf(18446744073709551617,18446744073709551616) + x^2*prf(2,1) + prf(1,18446744073709551616)")
assert result("test8")  =~ expr("0")
assert result("test9")  =~ expr("0")
assert result("test10") =~ expr("prf(x,1)")
assert result("test11") =~ expr("prf(x,1)")
assert result("test12") =~ expr("f(1)*g(1)*x*prf(3*x - 3,2) + prf(x + 1,1)")
assert result("test13") =~ expr("f(1)*g(1)*x*prf(3*x - 3*y,2) + prf(x + y,1)")
*--#] polynomial_extra_1 :
*--#[ polynomial_error_1 :
CFunction f,prf;
PolyRatFun prf;
Local test = prf(f(1),1);
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] polynomial_error_1 :
*--#[ polynomial_error_2 :
CFunction f,prf;
PolyRatFun prf;
* Fast notation version
Local test = prf(f,1);
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] polynomial_error_2 :
*--#[ polynomial_error_3 :
Symbol x;
Local test = inverse_(x+1,x+1);
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("flint::inverse_poly error: inverse does not exist") || stdout =~ exact_pattern("ERROR: polynomial inverse does not exist")
*--#] polynomial_error_3 :
*--#[ polynomial_error_4 :
CFunction prf;
PolyRatFun prf;
Local test = prf;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("ERROR: PolyRatFun cannot have zero arguments")
*--#] polynomial_error_4 :
*--#[ polynomial_error_5 :
CFunction prf;
PolyRatFun prf;
Local test = prf(1,2,3);
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("ERROR: PolyRatFun cannot have more than two arguments")
*--#] polynomial_error_5 :
*--#[ polynomial_error_6 :
CFunction prf;
Symbol x,y;
PolyRatFun prf;
Local test = prf(x,y,3);
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("ERROR: PolyRatFun cannot have more than two arguments")
*--#] polynomial_error_6 :
*--#[ polynomial_error_7 :
#-
#: MaxTermSize 200
CFunction prf;
Symbol x;
PolyRatFun prf;
#define N "3"
* This one goes wrong in flint::ratfun_normalize rather than to_argument_poly
Local test = prf((x+1)^`N',(x+2)^`N')*prf((x^`N'-1)^`N',(x^`N'-2)^`N');
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("flint::ratfun_normalize: output exceeds MaxTermSize") || stdout =~ exact_pattern("ERROR: PolyRatFun doesn't fit in a term")
*--#] polynomial_error_7 :
*--#[ polynomial_error_8 :
#: MaxTermSize 200
CFunction prf;
Symbol x;
PolyRatFun prf;
#define N "4"
Local test = prf((x+1)^`N',(x+2)^`N')*prf((x^`N'-1)^`N',(x^`N'-2)^`N');
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("flint::to_argument_poly: output exceeds MaxTermSize") || stdout =~ exact_pattern("ERROR: PolyRatFun doesn't fit in a term")
*--#] polynomial_error_8 :
*--#[ polynomial_error_9 :
#: MaxTermSize 241
CFunction prf;
Symbol x;
PolyRatFun prf;
#define N "5"
* This one goes wrong before properly starting to write the second argument
Local test = prf((x+1)^`N',(x+2)^`N')*prf((x^`N'-1)^`N',(x^`N'-2)^`N');
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("flint::to_argument_poly: output exceeds MaxTermSize") || stdout =~ exact_pattern("ERROR: PolyRatFun doesn't fit in a term")
*--#] polynomial_error_9 :
*--#[ polynomial_error_10 :
#: MaxTermSize 322
CFunction prf;
Symbol x,y;
PolyRatFun prf;
#define N "3"
* This one goes wrong in flint::ratfun_normalize rather than to_argument_mpoly
Local test = prf((x+y)^`N',(x+2*y)^`N')*prf((x^`N'-y)^`N',(x^`N'-2*y)^`N');
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("flint::ratfun_normalize: output exceeds MaxTermSize") || stdout =~ exact_pattern("ERROR: PolyRatFun doesn't fit in a term")
*--#] polynomial_error_10 :
*--#[ polynomial_error_11 :
#: MaxTermSize 200
CFunction prf;
Symbol x,y;
PolyRatFun prf;
#define N "3"
Local test = prf((x+y)^`N',(x+2*y)^`N')*prf((x^`N'-y)^`N',(x^`N'-2*y)^`N');
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("flint::to_argument_mpoly: output exceeds MaxTermSize") || stdout =~ exact_pattern("ERROR: PolyRatFun doesn't fit in a term")
*--#] polynomial_error_11 :
*--#[ polynomial_error_12 :
#: MaxTermSize 283
CFunction prf;
Symbol x,y,z;
PolyRatFun prf;
#define N "2"
* This one goes wrong before properly starting to write the second argument
Local test = prf((x+y+z)^`N',(x+2*y)^`N')*prf((x^`N'-y-z)^`N',(x^`N'-2*y)^`N');
Print;
.end
#pend_if mpi?
assert runtime_error?
assert stdout =~ exact_pattern("flint::to_argument_mpoly: output exceeds MaxTermSize") || stdout =~ exact_pattern("ERROR: PolyRatFun doesn't fit in a term")
*--#] polynomial_error_12 :
