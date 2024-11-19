#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ SparseTable1 :
#ifndef `TableSize'
  #define TableSize "10"
#endif
* Bugs reported 2004-04-06 by Misha Tentukov
* PrintTable and FillExpression did not work with non-sparse tables
* Fixed 2005-09-27
cf f;
s x;
ctable Tab(1:`TableSize');
ctable TabNew(1:`TableSize');

#do i=1,`TableSize',1
Fill Tab(`i')=f(`i');
.sort
#enddo

* BUG1 (not all elements are printed):
PrintTable Tab;

bracket x;
.sort
L expr1=table_(Tab,x);
print;
.sort

bracket x;
.sort

* BUG 2 ( seems only TabNew(1) is ok - further everything is broken):
Fillexpression TabNew=expr1(x);
.sort

#do i=1,`TableSize'
L e`i'=TabNew(`i');
#enddo
print;
.sort
.end
	assert succeeded?
	assert result("expr1") =~ expr("f(1)*x + f(2)*x^2 + f(3)*x^3 + f(4)*x^4 + f(5)*x^5 + f(6)*x^6 + f(7)*x^7 + f(8)*x^8 + f(9)*x^9 + f(10)*x^10")
	assert result("e10") =~ expr("f(10)")
*--#] SparseTable1 : 
*--#[ SymNonZero :
* Bug reported 2005-09-27 by Aneesh Manohar
* Symmetrize did not make expression y equal to zero
* Fixed 2005-10-09
cfunctions      f,g;
symbols         a,b;
local x=f(a,b)-f(b,a);
local y=f(g(a),b)-f(b,g(a));
symmetrize f;
.sort
print;
.end
	assert succeeded?
	assert result("x") =~ expr("0")
	assert result("y") =~ expr("0")
*--#] SymNonZero : 
*--#[ NegDimension :
* Parser accepted negative numbers as arguments to Dimension, Tracen, ...
* Fixed 2009-09-08
Dimension -1;
I i;
L f = d_(i,i);
print;
.end
assert compile_error?
*--#] NegDimension : 
*--#[ Transform-mulargs_1 :
CF f;
Auto S x;
L F = f(<x1+x2+x3>,...,<x7+x8+x9>);

* Consume ebuf. (Assume the default setup parameters on 64-bit systems.)
#do i=1,10
  id f(?a,x1?,x2?,?c) = f(?a,x1,x2,?c);
#enddo

* This extends ebuf.
transform f,mulargs(1,last);

* Crashed here.
id f(?a) = f(?a);

* Check a "hash", just in case.
multiply replace_(<x1,1>,...,<x9,1>);
id f(x?) = x;
P;
.end
# Only for 64-bit systems. Otherwise "Sorted function argument too long".
#require wordsize == 4
assert succeeded?
assert result("F") =~ expr("2187")
*--#] Transform-mulargs_1 : 
*--#[ Forum3t187 :
* bug in argument environment? [function specified by a set]
CF f1,f2,f3;
Set ff1: f1;
Set ff2: f2;
Set ff3: f3;
L F = f1(1) + f2(2) + f3(3);
argument ff2;
  discard;
endargument;
P;
.end
assert succeeded?
assert result("F") =~ expr("f1(1)+f2(0)+f3(3)")
*--#] Forum3t187 : 
*--#[ Issue7_1 :
* SegFault when #optimizing trivial bracket
Symbol x;
Local expr = x;
Bracket x;
Print;
.sort

Format O3;
#optimize expr
Print;
.end
assert succeeded?
assert result("expr", 0) =~ expr("+ x * ( 1 )")
assert result("expr", 1) =~ expr("x")
*--#] Issue7_1 :
*--#[ Issue7_2 :
* nontrivial bracket case was OK
Symbol x,y;
Local expr = x*(12+y);
Bracket x;
Print;
.sort

Format O3;
#optimize expr
Print;
.end
assert succeeded?
assert result("expr", 0) =~ expr("+ x * ( 12 + y )")
# See #364
assert result("expr", 1) =~ expr("Z1_*x") || result("expr", 1) =~ expr("x*Z1_")
*--#] Issue7_2 :
*--#[ Issue7_3 :
Symbols x,y,z;
L expr = 1;
AB x,y,z;
Format O3;
.sort
#optimize expr
Print;
.end
assert succeeded?
assert result("expr") =~ expr("1")
*--#] Issue7_3 :
*--#[ Issue8 :
* Bug with function replacement
Symbols a, b;
Functions fun, nDUMMY1, nDUMMY2;
Local expr= fun(a)*fun(b) ;
Id nDUMMY1?(?args1) * nDUMMY2?(?args2) = 1;
.sort
Print;
.end
assert succeeded?
assert result("expr") =~ expr("1")
*--#] Issue8 : 
*--#[ Issue21 :
* Occurs() with two or more terms in function arguments may get freeze
S x;
CF f;
L F = f(1+x);
if (occurs(x));
  id f(?a) = 1;
endif;
P;
.end
assert succeeded?
assert result("F") =~ expr("1")
*--#] Issue21 : 
*--#[ Issue23 :
#-
CFunction f,g;
Symbol x;

Local F = (x+1)^2*(f(x)+g(x))^2;
Local G = (x+1)^2*(f(x)+g(x))^2;

Format Mathematica;

Bracket x;
Print +s F;
.sort

Print +s G;
.end
assert succeeded?
assert stdout =~ exact_pattern(<<'EOF')
   F = (

       + x * (
          + 2*f[x]^2
          + 4*f[x]*g[x]
          + 2*g[x]^2
          )

       + x^2 * (
          + f[x]^2
          + 2*f[x]*g[x]
          + g[x]^2
          )

       + f[x]^2
          + 2*f[x]*g[x]
          + g[x]^2
         );
EOF
assert stdout =~ exact_pattern(<<'EOF')
   G = (
       + f[x]^2
       + 2*f[x]^2*x
       + f[x]^2*x^2
       + 2*f[x]*g[x]
       + 4*f[x]*g[x]*x
       + 2*f[x]*g[x]*x^2
       + g[x]^2
       + 2*g[x]^2*x
       + g[x]^2*x^2
      );
EOF
*--#] Issue23 :
*--#[ Issue25 :
* [tform] ZERO_ is always 1 when InParallel mode
L F1 = 1;
ModuleOption inparallel;
.sort
#message ZERO_F1 = `ZERO_F1'
#message ZERO_ = `ZERO_'
.end
assert succeeded?
assert stdout =~ /~~~ZERO_F1 = 0/
assert stdout =~ /~~~ZERO_ = 0/
*--#] Issue25 : 
*--#[ Issue30_1 :
* Substitutions just after putinside/antiputinside may fail
S x;
CF f;
L F1 = 1+x+x^2;
L F2 =-1-x-x^2;
putinside f, x;
*argument; endargument;  * <-- (1)
id f( 1)   = 0;
id f(-1)   = 0;
id f( x)   = 0;
id f(-x)   = 0;
id f( x^2) = 0;
id f(-x^2) = 0;
P;
.end
assert succeeded?
assert result("F1") =~ expr("0")
assert result("F2") =~ expr("0")
*--#] Issue30_1 : 
*--#[ Issue30_2 :
S x;
CF f;
L F1 = 1+x+x^2;
L F2 =-1-x-x^2;
antiputinside f, x;
*argument; endargument;  * <-- (1)
id f( 1) = 0;
id f(-1) = 0;
P;
.end
assert succeeded?
assert result("F1") =~ expr("0")
assert result("F2") =~ expr("0")
*--#] Issue30_2 : 
*--#[ Issue30_3:
CF f;
S x;
L F = 1;
$a = f;
inside $a;
  putinside f,x;
endinside;
*inside $a; endinside;  * <-- (1) workaround
P " a=%$;", $a;
$a = f($a);
P " a=%$;", $a;
.end
assert succeeded?
assert result("a", 0) =~ expr("f*f(1)")
assert result("a", 1) =~ expr("f(f*f(1))")
*--#] Issue30_3 :
*--#[ Issue37_1 :
* Polyratfun infinite loop in Print statement
S ep;
CF rat;
PolyRatFun rat(expand,ep,6);
L F  = rat(ep,ep);
Print;
.end
assert succeeded?
assert result("F") =~ expr("rat(1)")
*--#] Issue37_1 : 
*--#[ Issue37_2 :
S ep;
CF rat;
PolyRatFun rat(expand,ep,6);
L F = rat(1,1)*rat(ep,ep);
Print;
.end
assert succeeded?
assert result("F") =~ expr("rat(1)")
*--#] Issue37_2 : 
*--#[ Issue38 :
* Wrong normalization of PolyRatFun
CF num,rat;
PolyRatFun rat;
S n1,x,ep;
L F1 = num(n1)*num(1/2);
L F2 = num(n1)*num(-1/2);
L F3 = rat(1,1) - rat(1,1);
L F4 = rat(x,1)*rat(1+ep,1);
id num(x?) = rat(x,1);
P;
.end
assert succeeded?
assert result("F1") =~ expr("rat(n1,2)")
assert result("F2") =~ expr("rat( - n1,2)")
assert result("F3") =~ expr("0")
assert result("F4") =~ expr("rat(x*ep + x,1)")
*--#] Issue38 : 
*--#[ Issue39 :
* Freeze when PolyRatFun contains dot products
V a;
CF rat;
PolyRatFun rat;
L F = rat(a.a,1);
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue39 : 
*--#[ Issue41 :
* replace_ in #assign
S n;
#$x = n * replace_(n,n+1);
L F = `$x';
P;
.end
assert succeeded?
assert result("F") =~ expr("1+n")
*--#] Issue41 : 
*--#[ Issue42_1 :
* Factorize/FactDollar are much slower than FactArg
CF num;
S ep,n1,...,n14;
L F =
      +(n1)^5*(n2)^2*(n5)^4*(n6)^4*(n7)^7*(n8)^10*(n9)^21*(-8589934592)
      *(-1+n5)^2*(1+n9)*(30-19*n9+2*n9^2-18*n8+2*n8*n9-64*n7+14*n7*n9+
      16*n7*n8+12*n7^2-22*n6+4*n6*n9+4*n6*n8+8*n6*n7+4*n6^2+12*n5-2*n5*
      n9+4*n5*n8-4*n5*n6-4*n5^2+20*n4-6*n4*n9+4*n4*n8-4*n4*n7-8*n4*n6+4
      *n4^2-2*n3*n9+4*n3*n7+4*n3*n5-4*n3*n4-32*n2+8*n2*n9+2*n2*n8+22*n2
      *n7+8*n2*n6-2*n2*n5-4*n2*n4+6*n2^2+6*n1+7*n1*n9+6*n1*n7+10*n1*n6-
      6*n1*n5-12*n1*n4+4*n1*n2-2*n1^2-16*ep+8*ep*n9+8*ep*n8+32*ep*n7+8*
      ep*n6-16*ep*n5-16*ep*n4+16*ep*n2)
;
.sort

* FactArg

L F1 = num(F);
factarg num;
chainout num;
.sort

* #FactDollar
* FIXME: ParFORM hangs. (#46)
#$F2 = F;
#factdollar $F2
L F2 =
  num(`$F2[1]')
  #do i=2,`$F2[0]'
    * num(`$F2[`i']')
  #enddo
;
.sort

* FactDollar

L F3 = 1;
inexpression F3;
  $F3 = F;
  factdollar $F3;
  do $i=1,$F3[0];
    multiply num($F3[$i]);
  enddo;
endinexpression;

.sort

* FIXME: Factorize still have the performance issue. (#44)
#if 0
L F4 = F;
Factorize F4;
.sort
#endif

P;
.end
# ParFORM hangs for #FactDollar (#46)
#pend_if mpi?
assert succeeded?
f = expr("""
      num(n1)^5*num(n2)^2*num(n5)^4*num(n6)^4*num(n7)^7*num(n8)^10*num(n9)^21*
      num( - 8589934592)*num( - 1 + n5)^2*num(1 + n9)*num(30 - 19*n9 + 2*n9^2
       - 18*n8 + 2*n8*n9 - 64*n7 + 14*n7*n9 + 16*n7*n8 + 12*n7^2 - 22*n6 + 4*
      n6*n9 + 4*n6*n8 + 8*n6*n7 + 4*n6^2 + 12*n5 - 2*n5*n9 + 4*n5*n8 - 4*n5*n6
       - 4*n5^2 + 20*n4 - 6*n4*n9 + 4*n4*n8 - 4*n4*n7 - 8*n4*n6 + 4*n4^2 - 2*
      n3*n9 + 4*n3*n7 + 4*n3*n5 - 4*n3*n4 - 32*n2 + 8*n2*n9 + 2*n2*n8 + 22*n2*
      n7 + 8*n2*n6 - 2*n2*n5 - 4*n2*n4 + 6*n2^2 + 6*n1 + 7*n1*n9 + 6*n1*n7 +
      10*n1*n6 - 6*n1*n5 - 12*n1*n4 + 4*n1*n2 - 2*n1^2 - 16*ep + 8*ep*n9 + 8*
      ep*n8 + 32*ep*n7 + 8*ep*n6 - 16*ep*n5 - 16*ep*n4 + 16*ep*n2)
""")
assert result("F1") =~ f
assert result("F2") =~ f
assert result("F3") =~ f
*--#] Issue42_1 : 
*--#[ Issue42_2 :
S x;
L F = gcd_(
  (1+x),
  2*(1+x),
  3*(1+x)
);
P;
.end
assert succeeded?
assert result("F") =~ expr("1+x")
*--#] Issue42_2 : 
*--#[ Issue42_3 :
S n1,...,n4;
L F1 = (1+n1)*(1+n2)*n1*n2*n3;
L F2 = (1+n2)*n1*n2*n3*n4;
L F3 = (1+n4)*n1*n2*n3*n4^2;
L F = gcd_(F1,F2,F3);
P F;
.end
assert succeeded?
assert result("F") =~ expr("n1*n2*n3")
*--#] Issue42_3 : 
*--#[ Issue42_4 :
#procedure PrintFactorizedDollar(name,dollar)
  #write " `name' = (%$)%", `dollar'[1]
  #do i=2,``dollar'[0]'
    #write "*(%$)%", `dollar'[`i']
  #enddo
  #write ";"
#endprocedure

S x,y;
#$a = (1-x)*(1+y);
#$b = (1-x)*(1-y);
#factdollar $a
#factdollar $b
#call PrintFactorizedDollar(F1,$a)
#call PrintFactorizedDollar(F2,$b)
.end
assert succeeded?
assert result("F1") =~ expr("(-1)*(-1+x)*(1+y)")
assert result("F2") =~ expr("(-1+y)*(-1+x)")
*--#] Issue42_4 : 
*--#[ Issue45 :
* FactDollar still broken
#procedure PrintFactorizedDollar(name,dollar)
  #write " `name' = (%$)%", `dollar'[1]
  #do i=2,``dollar'[0]'
    #write "*(%$)%", `dollar'[`i']
  #enddo
  #write ";"
#endprocedure

S x,y;
#$a = 1+x-y;  * <-- The bug was found for this.
#$b = 2*(1+x-y);
#$c = (1+x+y)*(1+x-y);
#factdollar $a
#factdollar $b
#factdollar $c
#call PrintFactorizedDollar(F1,$a)
#call PrintFactorizedDollar(F2,$b)
#call PrintFactorizedDollar(F3,$c)
.end
assert succeeded?
assert result("F1") =~ expr("(-1)*(-1+y-x)")
assert result("F2") =~ expr("(-1+y-x)*(-2)")
assert result("F3") =~ expr("(-1)*(-1+y-x)*(1+y+x)")
*--#] Issue45 : 
*--#[ Issue48 :
* Memory error on dollar matching
CFunction TOPO,topo;
CFunction color;
Symbol M1,M2,x,cOlNA,cOlNR,ca,cf,nf,[dabc^2/n],[d4RR/n],[d4RA/n],[d4AA/n];

L   Diagrams=
      +topo(M1)*color(24*[d4RR/n]*cOlNA*cOlNR^-1+12*ca*[dabc^2/n]+
      ca^2*cf*nf)
      +topo(M2)*color(24*[d4RA/n]*cOlNA*cOlNR^-1+24*cf^4-72*ca*cf^3
      +66*ca^2*cf^2-19*ca^3*cf)
;

.sort

   id  topo(x?$topo) = 1;
   id  color(x?$color) = 1;

   $color = $color * topo($topo);

.sort
L Color = `$color';
P;
.end
assert succeeded?
assert result("Diagrams") =~ expr("2")
assert result("Color") =~ expr("
      24*topo(M2)*cf^4 - 72*topo(M2)*ca*cf^3 + 66*topo(M2)*ca^2*cf^2 - 19*
      topo(M2)*ca^3*cf + 24*topo(M2)*cOlNA*cOlNR^-1*[d4RA/n]")
*--#] Issue48 : 
*--#[ Issue52 :
* CopySpectator crashes when empty
CreateSpectator TMP, "xTMP";
S x;
L F = (1+x)^2;
.sort
CopySpectator G = TMP;
P;
.end
assert succeeded?
assert result("F") =~ expr("1 + 2*x + x^2")
assert result("G") =~ expr("0")
*--#] Issue52 : 
*--#[ Issue54_1 :
* Transform,replace xarg_ acts only on symbols
CF f;
S a;
L xx = f(a,1);
Transform,f,replace(1,last)=(xarg_,2*xarg_);
P;
.end
assert succeeded?
assert result("xx") =~ expr("f(2*a,2)")
*--#] Issue54_1 : 
*--#[ Issue54_2 :
CF f;
S a;
L xx = f(a,a^2,1,2);
Transform,f,replace(1,last)=(xarg_,2*xarg_,1,3);
Print;
.end
assert succeeded?
assert result("xx") =~ expr("f(2*a,2*a^2,3,4)")
*--#] Issue54_2 : 
*--#[ Issue55_1 :
* Pattern matching with sets, and (ex-)PolyRatFun CFunction
CFunction coeff,coeff2;
Symbol x,y,z;
Symbol ca,cf,zeta2;

Local test1 = + dum_( - 7117/81 - 64/9*zeta2)*ca^2*cf;
Local test2 = + dum_(1 + 576/7117*zeta2)*coeff(- 7117,81)*ca^2*cf;
.sort

Identify coeff(x?neg_,y?) = -coeff(-x,y);
Identify dum_(z?)*coeff(x?,y?) = dum_(z * x/y);
Print +s;
.sort

PolyRatFun coeff;
Normalize dum_;
Print +s;
.sort
PolyRatFun;
.sort

Identify coeff(x?neg_,y?) = -coeff(-x,y);
*Identify coeff(x?,y?) = coeff2(x,y);
*Identify coeff2(x?neg_,y?) = -coeff2(-x,y);

Print +s;
.end
assert succeeded?
assert result("test1") =~ expr("- (1 + 576/7117*zeta2)*coeff(7117,81)*ca^2*cf")
assert result("test2") =~ expr("- (1 + 576/7117*zeta2)*coeff(7117,81)*ca^2*cf")
*--#] Issue55_1 : 
*--#[ Issue55_2 :
* Pattern matching with sets, and (ex-)PolyRatFun CFunction
CF frac;
S x,y;
L F = - 2/3*x;
P;
.sort(PolyRatFun=frac);
*.sort;  * putting .sort is useless for this bug
*argument frac,1;endargument;  * workaround
id frac(x?neg_,y?) = - frac(-x,y);  * doesn't match
P;
.end
assert succeeded?
assert result("F") =~ expr("- frac(2,3)*x")
*--#] Issue55_2 : 
*--#[ Issue56 :
* PolyRatFun(expand) does not expand substituted expressions
CF rat;
S x;
PolyRatFun rat;
L F = rat(1,1+x);
L G = rat(1-x,1);
.sort
PolyRatFun rat(expand,x,2);
Drop;
L H = F - G;
*.sort;  * <-- (1)
P;
.end
assert succeeded?
assert result("H") =~ expr("rat(x^2)")
*--#] Issue56 : 
*--#[ Issue59_1 :
* Crash when PolyRatFun(expand)
CF num,rat;
S x;
PolyRatFun rat(expand,x,2);
L F = <num(1+5*x)>*...*<num(28+5*x)>
    * <num(-1-3*x)>*...*<num(-27-3*x)>
;
id num(x?) = rat(x,1);
.sort
P +s;
.end
assert succeeded?
assert result("F") =~ expr('
       + rat( - 3319889381431113865517677688157339126513795072000000000000 -
      103946485016901161789833595241629175725192946647040000000000*x -
      1536456092437457859275118833518144965878613654110208000000000*x^2)
')
*--#] Issue59_1 : 
*--#[ Issue59_2 :
CF rat;
S x;
PolyRatFun rat(expand,x,2);
L F1 = rat(1+x,1)^270;
L F2 = rat(10+10*x,1)^47;
P;
.end
assert succeeded?
assert result("F1") =~ expr('
      rat(1 + 270*x + 36315*x^2)
')
assert result("F2") =~ expr('
      rat(100000000000000000000000000000000000000000000000 + 47000000000000000\
      00000000000000000000000000000000*x + 10810000000000000000000000000000000\
      0000000000000000*x^2)
')
*--#] Issue59_2 : 
*--#[ Issue60 :
* No error for skipped semicolon in Save statement
Symbol x;
Global test = x;
.store
Save test.sav
.end
assert compile_error?
*--#] Issue60 : 
*--#[ Issue61 :
* IntoHide + Bracket for expressions with bracket index
S x,y;
L F = 1+x;
B+ y;
.sort
IntoHide F;
B x;
.sort
L G = F[x];
P;
.end
assert succeeded?
assert result("G") =~ expr("1")
*--#] Issue61 : 
*--#[ Issue69 :
* No warnings/errors for the same labels
On allwarning;
L F = 1;
goto 1;
label 1;
  multiply 2;
label 1;
  multiply 3;
label 1;
  multiply 5;
P;
.end
assert compile_error?
*--#] Issue69 : 
*--#[ Issue73 :
* "PolyRatFun cannot have zero arguments" when used in function
S ep;
CF rat,K;
PolyRatfun rat;
L F =  K(rat(ep+1,1)) + K(rat(1,1));
P;
.end
assert succeeded?
assert result("F") =~ expr("K(rat(ep + 1,1))*rat(1,1) + K(rat(1,1))*rat(1,1)")
*--#] Issue73 : 
*--#[ Issue74 :
* occurs() freezes with tensors #74
CF a,acc;

S x,y;
I i,j;
V p,q;
CT t;
CF f,g;

L F1 = 1;
L F2 = x;
L F3 = 1/x;
L F4 = i;
L F5 = p;
L F6 = p(i);
L F7 = p(N1_?);
L F8 = p.p;
L F9 = p.q;
L F10 = t;
L F11 = t(i);
L F12 = t(p);
L F13 = f;
L F14 = f(1);
L F15 = f(x);
L F16 = f(-x);
L F17 = f(1/x);
L F18 = f(x+y);
L F19 = f(i);
L F20 = f(-i);
L F21 = f(i+j);
L F22 = f(p);
L F23 = f(-p);
L F24 = f(p+q);
L F25 = f(p(i));
L F26 = f(p(N1_?));
L F27 = f(p.p);
L F28 = f(p.q);
L F29 = f(t);
L F30 = f(t(i));
L F31 = f(t(p));
L F32 = g(f(x));
L F33 = g(f(i));
L F34 = g(f(p));
L F35 = g(f(t));
L F36 = g(g(f));
L F37 = g_(i,p);
L F38 = g_(1,i,p);
L F39 = g(1,g(2,3-f(x))+g(t(p),t(i)));
L F40 = d_(p,i);

if (occurs(x)) multiply a(1);
if (occurs(i)) multiply a(2);
if (occurs(p)) multiply a(3);
if (occurs(t)) multiply a(4);
if (occurs(f)) multiply a(5);
chainin a;

antiputinside acc,a;
id acc(?a) = 1;

P;
.end
assert succeeded?
assert result("F1") =~ expr("1")
assert result("F2") =~ expr("a(1)")
assert result("F3") =~ expr("a(1)")
assert result("F4") =~ expr("a(2)")
assert result("F5") =~ expr("a(3)")
assert result("F6") =~ expr("a(2,3)")
assert result("F7") =~ expr("a(3)")
assert result("F8") =~ expr("a(3)")
assert result("F9") =~ expr("a(3)")
assert result("F10") =~ expr("a(4)")
assert result("F11") =~ expr("a(2,4)")
assert result("F12") =~ expr("a(3,4)")
assert result("F13") =~ expr("a(5)")
assert result("F14") =~ expr("a(5)")
assert result("F15") =~ expr("a(1,5)")
assert result("F16") =~ expr("a(1,5)")
assert result("F17") =~ expr("a(1,5)")
assert result("F18") =~ expr("a(1,5)")
assert result("F19") =~ expr("a(2,5)")
assert result("F20") =~ expr("a(2,5)")
assert result("F21") =~ expr("a(2,5)")
assert result("F22") =~ expr("a(3,5)")
assert result("F23") =~ expr("a(3,5)")
assert result("F24") =~ expr("a(3,5)")
assert result("F25") =~ expr("a(2,3,5)")
assert result("F26") =~ expr("a(3,5)")
assert result("F27") =~ expr("a(3,5)")
assert result("F28") =~ expr("a(3,5)")
assert result("F29") =~ expr("a(4,5)")
assert result("F30") =~ expr("a(2,4,5)")
assert result("F31") =~ expr("a(3,4,5)")
assert result("F32") =~ expr("a(1,5)")
assert result("F33") =~ expr("a(2,5)")
assert result("F34") =~ expr("a(3,5)")
assert result("F35") =~ expr("a(4,5)")
assert result("F36") =~ expr("a(5)")
assert result("F37") =~ expr("a(2,3)")
assert result("F38") =~ expr("a(2,3)")
assert result("F39") =~ expr("a(1,2,3,4,5)")
assert result("F40") =~ expr("a(2,3)")
*--#] Issue74 : 
*--#[ Issue77_1 :
* Freeze when pattern matchings with powers of dollar variables ($x^n?)
S x,n;
L F = 1;
#$x = x;
id $x^n? = 1;
P;
.end
assert succeeded?
assert result("F") =~ expr("1")
*--#] Issue77_1 : 
*--#[ Issue77_2 :
S x,y,z,n;
V p,q;
L F = x^3 * y^5 * p.q^6;
#$x = x*y*p.q;
id $x^n? = z^n;
P;
.end
assert succeeded?
assert result("F") =~ expr("p.q^3*y^2*z^3")
*--#] Issue77_2 : 
*--#[ Issue78_1 :
* Minus sign is ignored in set restriction
V p,p1;
CF vx;
L F1 = vx(-p1);
L F2 = F1;
inexpression F1;
  id vx(p?!{p1,-p1}) = 1;
endinexpression;
inexpression F2;
  id vx(p?!{-p1,p1}) = 1;
endinexpression;
Print;
.end
assert succeeded?
assert result("F1") =~ expr("vx(-p1)")
assert result("F2") =~ expr("vx(-p1)")
*--#] Issue78_1 : 
*--#[ Issue78_2 :
V Q;
CF vx;
L F1 = vx(-Q);
L F2 = F1;
inexpression F1;
  id vx(Q?{Q,-Q}) = 1;
endinexpression;
inexpression F2;
  id vx(Q?{-Q,Q}) = 1;
endinexpression;
Print;
.end
assert succeeded?
assert result("F1") =~ expr("1")
assert result("F2") =~ expr("1")
*--#] Issue78_2 : 
*--#[ Issue82 :
* Minus sign matching bug in latest version
V p1,p2;
CF vx;
L F = vx(-p2);
id vx(p2?!{p1}) = 1;
Print;
.end
assert succeeded?
assert result("F") =~ expr("1")
*--#] Issue82 : 
*--#[ Issue88 :
* Strange error in 'also once' in combination with 'replace_'
cf ABB;
i mu;
L test = 1;
once ABB(mu?) * ABB(mu?) = 1;
also once ABB(mu?, ?b, mu?) = replace_(mu, N100_?);
P;
.end
assert succeeded?
assert result("test") =~ expr("1")
*--#] Issue88 : 
*--#[ Issue90_1 :
* Errors in symbol powers
CFunction SP;
Symbol nn, shat;
Vector k1,k2,k3;
Local testExpr0 = shat^(-1+nn);
Local testExpr1 = shat^(-3+nn);
Local testExpr4 = SP(k2,k3)*(shat)^(-3+nn);
Argument;
    Identify nn = 2;
EndArgument;
Print +s;
.end
assert succeeded?
assert result("testExpr0") =~ expr("+shat")
assert result("testExpr1") =~ expr("+shat^-1")
assert result("testExpr4") =~ expr("+SP(k2,k3)*shat^-1")
*--#] Issue90_1 : 
*--#[ Issue90_2:
Symbol i,x,y,n;

Local test1 = 5^(n) * sum_(i,1,n, x^i);
Multiply replace_(n,3);
Print +s test1;
.sort

Local test2 = 5^(-n);
Multiply replace_(n,3);
Print +s test2;
.sort

Local test3 = 5^(-n) * sum_(i,1,n, x^i);
Multiply replace_(n,3);
Print +s test3;
.end
assert succeeded?
assert result("test1") =~ expr("+ 125*x + 125*x^2 + 125*x^3")
assert result("test2") =~ expr("+ 1/125")
assert result("test3") =~ expr("+ 1/125*x + 1/125*x^2 + 1/125*x^3")
*--#] Issue90_2 :
*--#[ Issue94 :
* No check for Dirac gamma matrices without any arguments
CF f;
L F1 = 123*g5_;
L F2 = 123*g6_;
L F3 = 123*g7_;
L F4 = 123*g_;
L F5 = 123*gi_;
L F6 = f(1000*g5_);
L F7 = f(10000*g5_);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue94 : 
*--#[ Issue95 :
#-
#: filepatches 32
#: largepatches 32
#: largesize 6250000
#: maxtermsize 1250
#: smallsize 1250000
#: smallextension 2500032
#: termsinsmall 12500

Off Statistics;

Symbol x,n;

* One fewer term than reported in the Issue, since now the
* non-x term is in the initial definition of test.
#define NTERMS "266905"

Local test = sum_(n,1,`NTERMS',x^n) - `NTERMS'*(`NTERMS'+1)/2;
.sort

* Check all terms present
Identify x^n?pos_ = n;

Print;
.end
# This takes a long time for tform under valgrind.
# tform -w4 also doesn't crash here anyway (but -w2 does).
#pend_if valgrind? && threaded?
# Also doesn't run properly for 32bit form.
#require wordsize >= 4
assert succeeded?
assert result("test") =~ expr("0")
*--#] Issue95 : 
*--#[ Issue95b :
#-
#:filepatches       16
#:largepatches      20
#:maxtermsize      200
#:termsinsmall      16

Off stats;

#define N "323"
S x,k;
L F = sum_(k,1,`N',x^k);
.sort

L CheckZero = F - {`N'*(`N'+1)/2};
id x^k?pos_ = k;

Print CheckZero;
.end
assert succeeded?
assert result("CheckZero") =~ expr("0")
*--#] Issue95b : 
*--#[ Issue97_1 :
* "Program terminating" with oldFactArg and dot products
V e1, e2, k1, k2;
S a, b;
CF dotM;
L testbad = dotM(e1.k1*e2.k1);
L testok = dotM(a*b);
.sort
On oldFactArg;
factarg dotM;
P;
.end
assert succeeded?
assert result("testbad") =~ expr("dotM(e1.k1,e2.k1,1)")
assert result("testok") =~ expr("dotM(a,b,1)")
*--#] Issue97_1 : 
*--#[ Issue97_2 :
On OldFactArg;
V p1,p2,p3,p4;
S x;
CF f;
T t;
L OK1 = f(t(p1)*x);
L OK2 = f(t(p1,p2)*x);
L OK3 = f(t(p1,p2,p3)*x);
L BAD = f(t(p1,p2,p3,p4)*x);
factarg f;
P;
.end
assert succeeded?
assert result("OK1") =~ expr("f(t(p1),x,1)")
assert result("OK2") =~ expr("f(t(p1,p2),x,1)")
assert result("OK3") =~ expr("f(t(p1,p2,p3),x,1)")
assert result("BAD") =~ expr("f(t(p1,p2,p3,p4),x,1)")
*--#] Issue97_2 : 
*--#[ Issue104 :
* Leading zeroes in rational numbers not handled consistently
Local test1 = 0001;
Local test2 = 00001;
Local test3 = 00010;
Local test4 = 00011;
Print +s;
.end
assert succeeded?
assert result("test1") =~ expr("+ 1")
assert result("test2") =~ expr("+ 1")
assert result("test3") =~ expr("+ 10")
assert result("test4") =~ expr("+ 11")
*--#] Issue104 : 
*--#[ Issue105 :
* Crash by replace_(x,0)
S x;
V p;
CF f;

L F = f(p.p+x);
L G = f(p.p*x);

multiply replace_(x,0);
P;
.end
assert succeeded?
assert result("F") =~ expr("f(p.p)")
assert result("G") =~ expr("f(0)")
*--#] Issue105 : 
*--#[ Issue106 :
* Crash with replace_ and nested functions
cfunction prop, mom;
vector q1, q2, k1, k2, p;

l test =  prop(mom(-q1-q2+p));
multiply replace_(q1,k1-k2);
print+s;
.sort
multiply replace_(q2,k2);
print+s;
.end

CF f,g;
V p1,p2;
L F1 = f(f(p1-p2));
L F2 = f(f(f(p1-p2)));
L F3 = f(f(f(f(p1-p2)+g(p1-p2))+g(p1-p2)));
multiply replace_(p1,p2);
P;
.end
assert succeeded?
assert result("test") =~ expr("+ prop(mom(- k1 + p))")
assert result("F1") =~ expr("f(f(0))")
assert result("F2") =~ expr("f(f(f(0)))")
assert result("F3") =~ expr("f(f(f(f(0)+g(0))+g(0)))")
*--#] Issue106 : 
*--#[ Issue111 :
* PolyRatFun(expand) doesn't expand numeric coefficients in one go
S x;
CF rat;
PolyRatFun rat(expand,x,3);
L F = rat(1+x);
.sort
multiply 2;
*.sort; * <-- workaround
P;
.sort
Drop;
L F1 = 3/5;
L F2 = 6/5;
L F3 = 2/5;
L F4 = 12345678901234567890123456789012345678901234567890;
L F5 = 2/5 * rat(1+x);
L F6 = 2/5 * rat(1,1-x);
L F7 = 2/5 * rat(1+x) * rat(1-2*x);
L F8 = 2/5 * rat(1+x) * rat(1,1-x);
L F9 = 2/5 * rat(1,1+x) * rat(1,1-2*x);
multiply 5/3;
P;
.end
assert succeeded?
assert result("F") =~ expr("rat(2 + 2*x)")
assert result("F1") =~ expr("rat(1)")
assert result("F2") =~ expr("rat(2)")
assert result("F3") =~ expr("rat(2/3)")
assert result("F4") =~ expr("rat(20576131502057613150205761315020576131502057613150)")
assert result("F5") =~ expr("rat(2/3 + 2/3*x)")
assert result("F6") =~ expr("rat(2/3 + 2/3*x + 2/3*x^2 + 2/3*x^3)")
assert result("F7") =~ expr("rat(2/3 - 2/3*x - 4/3*x^2)")
assert result("F8") =~ expr("rat(2/3 + 4/3*x + 4/3*x^2 + 4/3*x^3)")
assert result("F9") =~ expr("rat(2/3 + 2/3*x + 2*x^2 + 10/3*x^3)")
*--#] Issue111 : 
*--#[ Issue113 :
* ?a crashes the program if used only on the rhs
CF f;
L F = f;
id f(?a) = f(?a);
id f = f(?a);
Print;
.end
assert compile_error?
*--#] Issue113 : 
*--#[ Issue114 :
* Crash on PolyRatFun(expand) when the result is zero
CF rat;
S x;
L F = rat(x^10,1-x);
P;
.sort
PolyRatFun rat(expand,x,5);
P;
.end
assert succeeded?
assert result("F") =~ expr("rat(x^10 + x^11 + x^12 + x^13 + x^14 + x^15)")
*--#] Issue114 : 
*--#[ Issue117_1 :
* Id not matching when using ?a and symmetric function
S n1,n2;
CF f,g(s);
L F = f(n1,n2)*g(n1,n2);
id f(n1?,n2?,?a)*g(n1?,n2?) = 1; * works if g not symmetric or ?a is removed
Print;
.end
assert succeeded?
assert result("F") =~ expr("1")
*--#] Issue117_1 : 
*--#[ Issue117_2 :
S n1,n2;
CF f(s),g(s);
id f(n1?,n2?,?a)*g(n1?,n2?) = 1;
.end
assert compile_error?
*--#] Issue117_2 : 
*--#[ Issue117_3 :
S n1,n2;
S x1,x2,x3;
CF f,g(s);
L F1 = f(x1,x2)*g(x1,x2);
L F2 = f(x2,x1)*g(x1,x2);
L F3 = f(x1,x2,x3)*g(x1,x2);
L F4 = f(x2,x1,x3)*g(x1,x2);
L F5 = f(x1,x2)*f(x2,x1,x3)*g(x2,x1)^2;
id f(n1?,n2?,?a) * g(n1?,n2?) = 1;
P;
.end
assert succeeded?
assert result("F1") =~ expr("1")
assert result("F2") =~ expr("1")
assert result("F3") =~ expr("1")
assert result("F4") =~ expr("1")
assert result("F5") =~ expr("1")
*--#] Issue117_3 : 
*--#[ Issue121 :
* repeat ignored in some output terms of dd_
V p1,p2,p3,p4;
CF f;
L F = f(p1,p2,p3,p4)*f(p3,p4);
repeat id once f(?a) = dd_(?a);
P +s;
.end
assert succeeded?
assert result("F") =~ expr("
       + p1.p2*p3.p4^2
       + p1.p3*p2.p4*p3.p4
       + p1.p4*p2.p3*p3.p4
")
*--#] Issue121 : 
*--#[ Issue125_1 :
* Form compiler allows lone ? on rhs
CF f;
L F = f;
id f = f(?);
.end
assert compile_error?
*--#] Issue125_1 : 
*--#[ Issue125_2 :
V p;
I mu;
CF f;
L F = p(mu);
id p = f(?);
P;
.end
assert succeeded?
assert result("F") =~ expr("f(mu)")
*--#] Issue125_2 : 
*--#[ Issue126 :
* Print rejects local-to be unhidden expressions
L F = 1;
.sort
Hide;
.sort
Unhide;
P F;
.end
assert succeeded?
assert result("F") =~ expr("1")
*--#] Issue126 : 
*--#[ Issue128 :
* Rational arithmetic giving pi_
CF rat;
PolyRatFun rat;
S cw,sw,e;
*S MZ,sp12;  * <-- This fixes the problem.
S sp12,MZ;

L F =     cw * sw * e * rat(- MZ, 2 * sp12 - 1 * MZ);
L G = 2 * cw * sw * e * rat(- MZ, 4 * sp12 - 2 * MZ);
.sort
PolyRatFun rat;  * <-- workaround: renormalize rat
.sort
L FF = F^2;
L GG = G^2;
P +s;
.end
assert succeeded?
assert result("F") =~ expr("
       + cw*sw*e*rat(MZ, - 2*sp12 + MZ)
")
assert result("G") =~ expr("
       + cw*sw*e*rat(MZ, - 2*sp12 + MZ)
")
assert result("FF") =~ expr("
       + cw^2*sw^2*e^2*rat(MZ^2,4*sp12^2 - 4*sp12*MZ + MZ^2)
")
assert result("GG") =~ expr("
       + cw^2*sw^2*e^2*rat(MZ^2,4*sp12^2 - 4*sp12*MZ + MZ^2)
")
*--#] Issue128 : 
*--#[ Issue129_1 :
* Redefining a hidden expression #129
L F = 1;
.sort

#procedure redefine()
  Hide F;
  .sort
  L F = F + 1;
  .sort
#endprocedure

#do i=1,5
  #call redefine()
#enddo

On names;
P;
.end
assert succeeded?
assert result("F") =~ expr("6")
assert stdout =~ exact_pattern(<<'EOF')
 Expressions
   F(local)
 Expressions to be printed
   F
EOF
*--#] Issue129_1 : 
*--#[ Issue129_2:
L F = 1;
.sort

#procedure redefine()
  Hide F;
  .sort
  L tmp = 1;
  .sort
  Drop tmp;
  L F = F + 1;
  .sort
#endprocedure

#do i=1,5
  #call redefine()
#enddo

On names;
P;
.end
assert succeeded?
assert result("F") =~ expr("6")
assert stdout =~ exact_pattern(<<'EOF')
 Expressions
   F(local)
 Expressions to be printed
   F
EOF
*--#] Issue129_2 :
*--#[ Issue139 :
* Corrupted characters in printing f(-2147483648)
CF f;
* Check numbers near danguous ones up to 64 bits.
* 2^15 = 32768
L F15p6 = f(+32766);
L F15p7 = f(+32767);
L F15p8 = f(+32768);
L F15p9 = f(+32769);
L F15p0 = f(+32770);
L F15m6 = f(-32766);
L F15m7 = f(-32767);
L F15m8 = f(-32768);
L F15m9 = f(-32769);
L F15m0 = f(-32770);
* 2^16 = 65536
L F16p4 = f(+65534);
L F16p5 = f(+65535);
L F16p6 = f(+65536);
L F16p7 = f(+65537);
L F16p8 = f(+65538);
L F16m4 = f(-65534);
L F16m5 = f(-65535);
L F16m6 = f(-65536);
L F16m7 = f(-65537);
L F16m8 = f(-65538);
* 2^31 = 2147483648
L F31p6 = f(+2147483646);
L F31p7 = f(+2147483647);
L F31p8 = f(+2147483648);
L F31p9 = f(+2147483649);
L F31p0 = f(+2147483650);
L F31m6 = f(-2147483646);
L F31m7 = f(-2147483647);
L F31m8 = f(-2147483648);
L F31m9 = f(-2147483649);
L F31m0 = f(-2147483650);
* 2^32 = 4294967296
L F32p4 = f(+4294967294);
L F32p5 = f(+4294967295);
L F32p6 = f(+4294967296);
L F32p7 = f(+4294967297);
L F32p8 = f(+4294967298);
L F32m4 = f(-4294967294);
L F32m5 = f(-4294967295);
L F32m6 = f(-4294967296);
L F32m7 = f(-4294967297);
L F32m8 = f(-4294967298);
* 2^63 = 9223372036854775808
L F63p6 = f(+9223372036854775806);
L F63p7 = f(+9223372036854775807);
L F63p8 = f(+9223372036854775808);
L F63p9 = f(+9223372036854775809);
L F63p0 = f(+9223372036854775810);
L F63m6 = f(-9223372036854775806);
L F63m7 = f(-9223372036854775807);
L F63m8 = f(-9223372036854775808);
L F63m9 = f(-9223372036854775809);
L F63m0 = f(-9223372036854775810);
* 2^64 = 18446744073709551616
L F64p4 = f(+18446744073709551614);
L F64p5 = f(+18446744073709551615);
L F64p6 = f(+18446744073709551616);
L F64p7 = f(+18446744073709551617);
L F64p8 = f(+18446744073709551618);
L F64m4 = f(-18446744073709551614);
L F64m5 = f(-18446744073709551615);
L F64m6 = f(-18446744073709551616);
L F64m7 = f(-18446744073709551617);
L F64m8 = f(-18446744073709551618);
P;
.end
assert succeeded?

assert result("F15p6") =~ expr("f(32766)")
assert result("F15p7") =~ expr("f(32767)")
assert result("F15p8") =~ expr("f(32768)")
assert result("F15p9") =~ expr("f(32769)")
assert result("F15p0") =~ expr("f(32770)")
assert result("F15m6") =~ expr("f(-32766)")
assert result("F15m7") =~ expr("f(-32767)")
assert result("F15m8") =~ expr("f(-32768)")
assert result("F15m9") =~ expr("f(-32769)")
assert result("F15m0") =~ expr("f(-32770)")

assert result("F16p4") =~ expr("f(65534)")
assert result("F16p5") =~ expr("f(65535)")
assert result("F16p6") =~ expr("f(65536)")
assert result("F16p7") =~ expr("f(65537)")
assert result("F16p8") =~ expr("f(65538)")
assert result("F16m4") =~ expr("f(-65534)")
assert result("F16m5") =~ expr("f(-65535)")
assert result("F16m6") =~ expr("f(-65536)")
assert result("F16m7") =~ expr("f(-65537)")
assert result("F16m8") =~ expr("f(-65538)")

assert result("F31p6") =~ expr("f(2147483646)")
assert result("F31p7") =~ expr("f(2147483647)")
assert result("F31p8") =~ expr("f(2147483648)")
assert result("F31p9") =~ expr("f(2147483649)")
assert result("F31p0") =~ expr("f(2147483650)")
assert result("F31m6") =~ expr("f(-2147483646)")
assert result("F31m7") =~ expr("f(-2147483647)")
assert result("F31m8") =~ expr("f(-2147483648)")
assert result("F31m9") =~ expr("f(-2147483649)")
assert result("F31m0") =~ expr("f(-2147483650)")

assert result("F32p4") =~ expr("f(4294967294)")
assert result("F32p5") =~ expr("f(4294967295)")
assert result("F32p6") =~ expr("f(4294967296)")
assert result("F32p7") =~ expr("f(4294967297)")
assert result("F32p8") =~ expr("f(4294967298)")
assert result("F32m4") =~ expr("f(-4294967294)")
assert result("F32m5") =~ expr("f(-4294967295)")
assert result("F32m6") =~ expr("f(-4294967296)")
assert result("F32m7") =~ expr("f(-4294967297)")
assert result("F32m8") =~ expr("f(-4294967298)")

assert result("F63p6") =~ expr("f(9223372036854775806)")
assert result("F63p7") =~ expr("f(9223372036854775807)")
assert result("F63p8") =~ expr("f(9223372036854775808)")
assert result("F63p9") =~ expr("f(9223372036854775809)")
assert result("F63p0") =~ expr("f(9223372036854775810)")
assert result("F63m6") =~ expr("f(-9223372036854775806)")
assert result("F63m7") =~ expr("f(-9223372036854775807)")
assert result("F63m8") =~ expr("f(-9223372036854775808)")
assert result("F63m9") =~ expr("f(-9223372036854775809)")
assert result("F63m0") =~ expr("f(-9223372036854775810)")

assert result("F64p4") =~ expr("f(18446744073709551614)")
assert result("F64p5") =~ expr("f(18446744073709551615)")
assert result("F64p6") =~ expr("f(18446744073709551616)")
assert result("F64p7") =~ expr("f(18446744073709551617)")
assert result("F64p8") =~ expr("f(18446744073709551618)")
assert result("F64m4") =~ expr("f(-18446744073709551614)")
assert result("F64m5") =~ expr("f(-18446744073709551615)")
assert result("F64m6") =~ expr("f(-18446744073709551616)")
assert result("F64m7") =~ expr("f(-18446744073709551617)")
assert result("F64m8") =~ expr("f(-18446744073709551618)")
*--#] Issue139 : 
*--#[ Issue146 :
* Memory bug via expanding the triple dot operator
Auto S x;
L F = x1+...+x123;
#$n = 1;
.sort
L G = x1+...+x1000;
#$m = F;
.end
assert succeeded?
*--#] Issue146 : 
*--#[ Issue149_1 :
* Index matches to -1 but crashes in output
Index mu;
CF f;
L F1 = f(-1);
L F2 = <f(-2)>+...+<f(130)>;
id f(mu?) = mu;
P;
.end
assert succeeded?
assert result("F1") =~ expr("f(-1)")
assert result("F2") =~ expr("8256 + f(-2) + f(-1) + f(129) + f(130)")
*--#] Issue149_1 : 
*--#[ Issue149_2 :
Index mu;
CF f1(s),f2(a),f3(c),f4(r);
L F1 = <f1(-2)>+...+<f1(130)>;
L F2 = <f2(-2)>+...+<f2(130)>;
L F3 = <f3(-2)>+...+<f3(130)>;
L F4 = <f4(-2)>+...+<f4(130)>;
id f1?(mu?) = mu;
P;
.end
assert succeeded?
assert result("F1") =~ expr("8256 + f1(-2) + f1(-1) + f1(129) + f1(130)")
assert result("F2") =~ expr("8256 + f2(-2) + f2(-1) + f2(129) + f2(130)")
assert result("F3") =~ expr("8256 + f3(-2) + f3(-1) + f3(129) + f3(130)")
assert result("F4") =~ expr("8256 + f4(-2) + f4(-1) + f4(129) + f4(130)")
*--#] Issue149_2 : 
*--#[ Issue151 :
* Compiler crashes with Print
#do i=1,200
  P "123456789012345678901234567890";
  P "%t";
#enddo
.end
assert succeeded?
*--#] Issue151 : 
*--#[ Issue153_1 :
* Pattern with index and set restriction matches to number
I mu1,...,mu9;
CF f;
Set indices: mu1,...,mu9;
Set indices2: mu1,...,mu9, 127, 128;
L F1 = f(132);
L F2 = <f(126)>+...+<f(132)>;
id f(mu1?indices) = 1;
id f(mu1?indices2) = 0;
P;
.end
assert succeeded?
assert result("F1") =~ expr("f(132)")
assert result("F2") =~ expr("f(126) + f(129) + f(130) + f(131) + f(132)")
*--#] Issue153_1 : 
*--#[ Issue153_2 :
I mu1,...,mu9;
CF f1(s),f2(a),f3(c),f4(r);
Set indices: mu1,...,mu9;
Set indices2: mu1,...,mu9, 127, 128;
L F1 = <f1(126)>+...+<f1(132)>;
L F2 = <f2(126)>+...+<f2(132)>;
L F3 = <f3(126)>+...+<f3(132)>;
L F4 = <f4(126)>+...+<f4(132)>;
id f1?(mu1?indices) = 1;
id f1?(mu1?indices2) = 0;
P;
.end
assert succeeded?
assert result("F1") =~ expr("f1(126) + f1(129) + f1(130) + f1(131) + f1(132)")
assert result("F2") =~ expr("f2(126) + f2(129) + f2(130) + f2(131) + f2(132)")
assert result("F3") =~ expr("f3(126) + f3(129) + f3(130) + f3(131) + f3(132)")
assert result("F4") =~ expr("f4(126) + f4(129) + f4(130) + f4(131) + f4(132)")
*--#] Issue153_2 : 
*--#[ Issue154 :
* CompressSize insufficient while the compression is off, when Keep Brackets
Off compress;
I mu1,...,mu16;
L F = g_(1,mu1,...,mu16);
B g_;
.sort;
Keep Brackets;
tracen,1;
.sort
Drop;
L F1 = termsin_(F);
P;
.end
# Too slow on Travis CI. ParFORM didn't have this bug.
#pend_if travis? && (!linux? || valgrind? || mpi?)
#time_dilation 2.0
assert succeeded?
assert result("F1") =~ expr("2027025")
*--#] Issue154 : 
*--#[ Issue162 :
* Missing Expr[x] with B+ for functions
#define N "5"
#define M "2"
#define P "3"

S x;
CF x1,...,x`M';
S x{`M'+1},...,x`N';

* Test input.

L F = (x1+...+x`N')^`P';
.sort:input;

* Bracket for some functions.

B+ x1,...,x`M';
Print[];
.sort:bracket;
Hide;

* Check if all entries exist.

L FF = F;
B x1,...,x`M';
.sort:test input;
Keep Brackets;

#define failed "0"

$x = term_;
$y = F[$x];
$n = termsin_($y);
if ($n == 0);
  P "Error: F[%$] == %$", $x, $y;
  redefine failed "1";
endif;
.sort:test;

#if `failed'
  #terminate
#endif
.end
assert succeeded?
*--#] Issue162 : 
*--#[ Issue163 :
* Normalize statement doesn't work for "MINVECTOR"
CF f1,f2;
V p;
L F1 = f1(-p);
L F2 = f2(-p);
normalize f1;
normalize (0) f2;
P;
.end
assert succeeded?
assert result("F1") =~ expr("-f1(p)")
assert result("F2") =~ expr("f2(p)")
*--#] Issue163 : 
*--#[ Issue165 :
* [tform] reading a bracket may crash with B+ when the expression doesn't fit in the scratch buffer
#:MaxTermSize 200
#:ScratchSize 12800
CF f,g;
S n;
#define N "100"
#define M "100"
L F = <f(1)>+...+<f(`N')>;
multiply <g(1)>+...+<g(`M')>;
B+ f;
*B- f;  * <-- (1)
*ModuleOption noparallel;
.sort
id g(n?) = F[f(n)];
*ModuleOption noparallel;  * <-- (2)
.sort
* Checksum
id f(n?) = n;
id g(n?) = n;
P;
.end
# Known to fail with ParFORM (#166)
#pend_if mpi?
assert succeeded?
assert result("F") =~ expr("2550250000")
*--#] Issue165 : 
*--#[ Issue167 :
* Mystery of count_ in functions
S x;
CF f;
L F = 1 + x + x^2;
multiply f(count_(x,1));
P;
.sort
Drop;
L G = 1 + x + x^2;
$x = f(count_(x,1));
multiply $x;
P;
.end
assert succeeded?
assert result("F") =~ expr("f(0) + f(0)*x + f(0)*x^2")
assert result("G") =~ expr("f(0) + f(1)*x + f(2)*x^2")
*--#] Issue167 : 
*--#[ Issue169 :
* Crash from multiply replace_ in large expression
S x;
CF den;
L F =
  + 16608736983689726473/192*den(2+x)
  + 18358130244940416000*den(2+x)
;
multiply replace_(x,1);
P +s;
.end
assert succeeded?
assert result("F") =~ expr("+ 3541369744012249598473/192*den(3)")
*--#] Issue169 : 
*--#[ Issue178 :
* PolyRatFun performance regression
* Josh's example:
Symbol a,b,c,ep;
CFunction redprf,epprf;

Local test1 =
       + epprf(-1, - 1 + ep)*redprf(1,1)
       + epprf(-1,1 - 3*ep + 2*ep^2)*redprf(-1,1)
      ;
.sort

PolyRatFun redprf;
Identify redprf(a?,b?) = redprf(a*c,b*c);
Identify epprf(a?,b?) = redprf(a,b);
.sort

Print;
.end
assert succeeded?
assert result("test1") =~ expr("redprf(-2,2*ep - 1)")
*--#] Issue178 : 
*--#[ Issue180 :
* Broken RAT
S ep;
CF rat,RAT;
PolyRatFun rat,RAT;

L F = 1;

P "A1:%t";
multiply RAT(1+ep,1);
P "A2:%t";
P;
.sort

P "B1:%t";
multiply RAT(1+ep,1);
P "B2:%t";
P;
.sort

P;
.end
assert succeeded?
assert result("F") =~ expr("rat(1,ep^2 + 2*ep + 1)")
*--#] Issue180 :
*--#[ Issue183 :
#: MaxTermSize 16K
#: SubTermsInSmall 800

CF f;
Auto S x;
L F = f(<x1+x2+x3+x4>,...,<x6+x7+x8+x9>);
*repeat id f(x1?,x2?,?a) = f(x1*x2,?a);  * error: Sorted function argument...
transform f,mulargs(1,last);  * silent crash
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("Term too complex during normalization")
*--#] Issue183 :
*--#[ Issue185 :
* Wrong result of content_

* This is OK.
S x,y;
#$p = (x/3+2/y)^2;
#$c = content_($p);
#$q = $p/$c;
L C1 = $c;
L Q1 = $q;
.sort

* This was BAD.
S x;
#$p = 1+1/x;
#$c = content_($p);
#$q = $p/$c;
L C2 = $c;
L Q2 = $q;
.sort

* Workaround.
S x,xxx;
#$p = 1+1/x;
#$tmp = $p*xxx;
#$c = content_($tmp)/xxx;
#$q = $p/$c;
L C3 = $c;
L Q3 = $q;

P;
.end
assert succeeded?
assert result("C1") =~ expr("1/9*y^-2")
assert result("Q1") =~ expr("36+12*x*y+x^2*y^2")
assert result("C2") =~ expr("x^-1")
assert result("Q2") =~ expr("1+x")
assert result("C3") =~ expr("x^-1")
assert result("Q3") =~ expr("1+x")
*--#] Issue185 : 
*--#[ Issue186 :
* $args not expanded for distrib_
S x1,...,x4;
CF f;
L F = f(x1,...,x4);
id f(?a$a) = 1;
multiply distrib_(1,1,f,dummy_,$a);
P;
.end
assert succeeded?
assert result("F") =~ expr("f(x1) + f(x2) + f(x3) + f(x4)")
*--#] Issue186 : 
*--#[ Issue190 :
* Polyratfun coming from function argument does not add properly
Auto S x1,x2,ep;
CF f,rat;
Polyratfun rat;

* x1 and x2 should have coefficient -1
L F =
      +f((rat(1-ep,1)*x1-2*x2)*rat(1,1+ep))
      +f((rat(1-ep,1)*x2-2*x1)*rat(1,1+ep))
;

id f(x1?) = x1;

Print +s;
.end
assert succeeded?
assert result("F") =~ expr("
       + x2*rat(-1,1)
       + x1*rat(-1,1)
")
*--#] Issue190 : 
*--#[ Issue191 :
* gcd_ crashes for zero $-variables

S x;

* immediate values
#define a1 "10"
#define a2 "-20"
#define a3 "100000000000000000000"
#define a4 "-200000000000000000000"
#define a5 "x"
#define a6 "-x"
#define a7 "1+x"
L F0  = gcd_(0,0);
#do i=1,7
  L Fa`i' = gcd_(0,`a`i'');
  L Fb`i' = gcd_(`a`i'',0);
#enddo
L Fc1 = gcd_(0,1+x,0,0,0);
L Fc2 = gcd_(0,1+x,0,-x,0,0);
L Fc3 = gcd_(0,1+x,0,1-x^2,0,0);
P;
.sort
Drop;

* subexpressions
L a0 = 0;
L a00 = 0;
L a1 = 10;
L a2 = -20;
L a3 = 100000000000000000000;
L a4 = -200000000000000000000;
L a5 = x;
L a6 = -x;
L a7 = 1+x;
L G0 = gcd_(a0,a00);
#do i=1,7
  L Ga`i' = gcd_(a0,a`i');
  L Gb`i' = gcd_(a`i',a0);
#enddo
L Gc1 = gcd_(0,a7,0,0,a0);
L Gc2 = gcd_(0,a7,0,a3,0,a0);
L Gc3 = gcd_(0,a7,0,1-x^2,0,a0);
P;
.sort
Drop;

* $-variables
#$a0 = 0;
#$a00 = 0;
#$a1 = 10;
#$a2 = -20;
#$a3 = 100000000000000000000;
#$a4 = -200000000000000000000;
#$a5 = x;
#$a6 = -x;
#$a7 = 1+x;
L H0 = gcd_($a0,$a00);
#do i=1,7
  L Ha`i' = gcd_($a0,$a`i');
  L Hb`i' = gcd_($a`i',$a0);
#enddo
L Hc1 = gcd_(0,$a7,0,0,$a0);
L Hc2 = gcd_(0,$a7,0,$a3,0,$a0);
L Hc3 = gcd_(0,$a7,0,1-x^2,0,$a0);
P;
.end
assert succeeded?
assert result("F0") =~ expr("0")
assert result("Fa1") =~ expr("10")
assert result("Fa2") =~ expr("-20")
assert result("Fa3") =~ expr("100000000000000000000")
assert result("Fa4") =~ expr("-200000000000000000000")
assert result("Fa5") =~ expr("x")
assert result("Fa6") =~ expr("-x")
assert result("Fa7") =~ expr("1+x")
for i in 1..7
  assert result("Fb#{i}") == result("Fa#{i}")
end
assert result("Fc1") =~ expr("1+x")
assert result("Fc2") =~ expr("1")
assert result("Fc3") =~ expr("1+x")
assert result("G0") =~ expr("0")
for i in 1..7
  assert result("Ga#{i}") == result("Fa#{i}")
  assert result("Gb#{i}") == result("Fa#{i}")
end
for i in 1..3
  assert result("Gc#{i}") == result("Fc#{i}")
end
assert result("H0") =~ expr("0")
for i in 1..7
  assert result("Ha#{i}") == result("Fa#{i}")
  assert result("Hb#{i}") == result("Fa#{i}")
end
for i in 1..3
  assert result("Hc#{i}") == result("Fc#{i}")
end
*--#] Issue191 : 
*--#[ Issue192_1 :
* Also related: Issue 334.
Global abcdefghijklmnop = 16;
Global abcdefghijklmnopq = 17;
.store
Save test.res;
.end
assert warning?("saved expr name over 16 char: abcdefghijklmnopq")
*--#] Issue192_1 :
*--#[ Issue192_2 :
* Also related: Issue 334.
Global abcdefghijklmnop = 16;
Global abcdefghijklmnopq = 17;
.store
Save test.res abcdefghijklmnopq;
.end
assert warning?("saved expr name over 16 char: abcdefghijklmnopq")
*--#] Issue192_2 :
*--#[ Issue192_3 :
* Also related: Issue 334.
Global abcdefghijklmnop = 16;
Global abcdefghijklmnopq = 17;
.store
Save test.res abcdefghijklmnop;
.end
assert succeeded?
*--#] Issue192_3 :
*--#[ Issue192_4 :
* Also related: Issue 334.
Global abcdefghijklmnop1 = 17;
Global abcdefghijklmnop2 = 17;
.store
#do i = 1,2
    Save test.res abcdefghijklmnop`i';
#enddo
.end
assert warning?("saved expr name over 16 char: abcdefghijklmnop1")
assert warning?("saved expr name over 16 char: abcdefghijklmnop2")
*--#] Issue192_4 :
*--#[ Issue197 :
* mul_ ignores denominator factors
#if "{2^32}" == "0"
* LONG has 4 bytes, which indicates WORD has 2 bytes.
* Avoid the "polynomials too large" error.
  #define n "3"
#else
  #define n "5"
#endif
S x,y,z;
L F1 = mul_(2/3,5/7);
L F2 = mul_(1/2+x/3,1/5+x/7);
P;
.sort
Drop;
L A1 = (5000000029/7+3/2*x-5/11*x/y+7/8*y*z+z-x*z)^`n';
L A2 = (3/4-1/9*x+9/5000000039*x*y+5/12*y*z+2/z*z^3)^`n';
.sort
Drop;
L G1 = A1 * A2;
L G2 = mul_(A1,A2);
.sort
Drop;
L Nterms = termsin_(G1);
L Zero = G1 - G2;
P;
.end
assert succeeded?
assert result("F1") =~ expr("10/21")
assert result("F2") =~ expr("1/10 + 29/210*x + 1/21*x^2")
if wordsize == 2
  assert result("Nterms") =~ expr("333")
else
  assert result("Nterms") =~ expr("1351")
end
assert result("Zero") =~ expr("0")
*--#] Issue197 : 
*--#[ Issue214 :
#-
#: MaxTermSize 500
#: ScratchSize 1K
#: SortIOSize 1K

Off compress;
Symbol x,y,z,i;
CFunction f;

Local test = sum_(i,1,100,f(i*(x+y))*(x+y)^20) - 52824783675150;
Bracket f;
.sort
Keep Brackets;

Identify f(x?) = x;
.sort

Identify x = 1;
Identify y = 2;

Print +s;
.end
# This is not valgrind clean under parform
#pend_if valgrind? && mpi?
assert succeeded?
assert result("test") =~ expr("0")
*--#] Issue214 :
*--#[ Issue219 :
* Corrupted characters in {-9223372036854775808}
#$n32  = -2^31;
#$n64  = -2^63;
#$n128 = -2^127;
L F32  = {`$n32'};
L F64  = {`$n64'};
L F128 = {`$n128'};
* In previous versions, "(" was returned from the preprocessor calculator
* on systems using two's complement for signed numbers, leading to an
* "Unmatched ()" error. Note that overflow/underflow doesn't give any errors in
* the preprocessor calculator (e.g., for F128), just gives a strange number
* (though in a strict sense it is an undefined behaviour and can cause a crash;
* let's hope compilers will take a little more time to become so insidious).
P;
.end
assert succeeded?
*--#] Issue219 : 
*--#[ Issue211 :
* Unexpected code in ReNumber
#: TermsInSmall 128
#: LargePatches 16
#: FilePatches 4
#: SubTermsInSmall 64
#: SubLargePatches 8
#: SubFilePatches 2

CFunction f,g;
Symbol x,y;

* 128*16=2048 terms cause a sort of the large buffer to disk.
* multiples of 2048*4=8192 terms cause a stage 4 sort
#define NTERMS "40001"

#define ARGNTERMS "2001"

Local test1 = <f(1)>+...+<f(`NTERMS')>;
Local test2 = g(<f(1)>+...+<f(`ARGNTERMS')>);
.sort

* Cancel all terms, but keep distance so that most terms only cancel in the final sort
Identify f(x?) = f(x) - f(`NTERMS'-x+1);
Argument g;
  Identify f(x?) = f(x) - f(`ARGNTERMS'-x+1);
EndArgument;

Print;
.end
# Only for 64-bit systems. Otherwise "Output term too large".
#require wordsize == 4
# For now it fails because
#   "Currently Stage 4 sorts are not allowed for function arguments or $ variables."
assert runtime_error?
# Runtime errors may freeze ParFORM.
#pend_if mpi?
#assert succeeded?
#assert result("test1") =~ expr("0")
#assert result("test2") =~ expr("g(0)")
*--#] Issue211 : 
*--#[ Issue222 :
* accessing #factdollar factors causes program termination
Symbol x;
#$a = 1;  * Error
*#$a = x;  * Fine
#factdollar $a;
#write "Number of factors in `$a' is `$a[0]'"
#write "Factor 1 is `$a[1]'"
.end
assert succeeded?
*--#] Issue222 : 
*--#[ Issue230 :
#-
#: MaxTermSize 16K
#: SubTermsInSmall 800


Off Statistics;

Symbol x1,...,x11;
CFunction f;

Local test = f(x1+...+x11) - (x1+...+x11)^4;
Identify f(x1?) = f(x1,x1,x1,x1);
.sort

Transform f mulargs(1,last);
Identify f(x1?) = x1;

Print +s;
.end
assert succeeded?
assert result("test") =~ expr("0")
*--#] Issue230 :
*--#[ Issue231 :
Symbol x,y,z;
Local F = x + y + x*z;
.sort
#do i=1,9
	CreateSpectator S`i',"S`i'.spec";
#enddo
If (Count(y,1) > 0) ToSpectator S1;
.sort
* Remove some spectators, making holes
#do i=1,9,2
	RemoveSpectator S`i';
#enddo
* Add to a spectator after making "holes"
If (Count(z,1) > 0) ToSpectator S4;
.sort
* Retrieve:
CopySpectator G = S4;
.sort
* Empty
RemoveSpectator S4;
Print;
.sort
On Codes;
* New spectator, should fill in the first "hole"
CreateSpectator S1 "S1.spec";
.end
assert succeeded?
assert result("F") =~ expr("x")
assert result("G") =~ expr("x*z")
assert stdout =~ exact_pattern(<<'EOF')
 Expressions
   F(local)(0) S2(spectator)(1) S6(spectator)(2) S8(spectator)(3) G(local)(4) 
   S1(spectator)(5)
EOF
*--#] Issue231 :
*--#[ Issue253 :
* Memory error for local $-variable in TFORM
#$x = 0;
ModuleOption local $x;
.end
assert succeeded?
*--#] Issue253 : 
*--#[ Issue258 :
* gcd_ gives wrong results
S s,t,m;
L test1   = 1/5*s + 1/5*(s+t)*m;
L test2   = (s+t)*m;
L result1 = gcd_(test1*replace_(s,t,t,m,m,s),test2*replace_(s,t,t,m,m,s));
L result2 = gcd_(test1,test2);
* Previous versions gave
*   result1 = 1 (correct), but had Valgrind errors
*   result2 = m (wrong)
P;
.end
assert succeeded?
assert result("result1") =~ expr("1")
assert result("result2") =~ expr("1")
*--#] Issue258 : 
*--#[ Issue260 :
* gcd_ doesn't give the correct result
S x1,...,x5;
#$a = 34*x2^2*x5 + x1^2*x2*x4*x5 + x1^5;
#$b = x4^5 + x3^5 + x2*x3*x5^3;
#$g = x3*x4^4 + x2^3*x4 + x1*x3;
#$p = $a * $g;
#$q = $b * $g;
L F1 = gcd_($p,$q);
.sort

#$a = 79*x2 + x2^4 + x1*x3*x4;
#$b = x4^5 + x1*x3^4 + x1^5;
#$g = x2^4*x3 + 84*x1^5;
#$p = $a * $g;
#$q = $b * $g;
L F2 = gcd_($p,$q);

P;
.end
assert succeeded?
assert result("F1") =~ expr("x3*x4^4 + x2^3*x4 + x1*x3")
assert result("F2") =~ expr("x2^4*x3 + 84*x1^5")
*--#] Issue260 : 
*--#[ Issue261_1 :
* Division by zero error by mul_(1,0)
S x;
#$x = 1 + x + x^2;
#$z = 0;
L F1  = mul_(1,0);
L F2  = mul_(0,1);
L F3  = mul_(0,0);
L F4  = mul_($x,$z);
L F5  = mul_($z,$x);
L F6  = mul_($z,$z);
L F7  = mul_($x,0);
L F8  = mul_(0,$x);
L F9  = mul_(1,$z);
L F10 = mul_($z,1);

L F12 = div_(0,1);
L F15 = div_($z,$x);
L F18 = div_(0,$x);
L F20 = div_($z,1);

L F22 = rem_(0,1);
L F25 = rem_($z,$x);
L F28 = rem_(0,$x);
L F30 = rem_($z,1);

L F32 = inverse_(0,1);
L F35 = inverse_($z,$x);
L F38 = inverse_(0,$x);
L F40 = inverse_($z,1);

P;
.end
assert succeeded?
assert result("F1")  =~ expr("0")
assert result("F2")  =~ expr("0")
assert result("F3")  =~ expr("0")
assert result("F4")  =~ expr("0")
assert result("F5")  =~ expr("0")
assert result("F6")  =~ expr("0")
assert result("F7")  =~ expr("0")
assert result("F8")  =~ expr("0")
assert result("F9")  =~ expr("0")
assert result("F10") =~ expr("0")
assert result("F12") =~ expr("0")
assert result("F15") =~ expr("0")
assert result("F18") =~ expr("0")
assert result("F20") =~ expr("0")
assert result("F22") =~ expr("0")
assert result("F25") =~ expr("0")
assert result("F28") =~ expr("0")
assert result("F30") =~ expr("0")
assert result("F32") =~ expr("0")
assert result("F35") =~ expr("0")
assert result("F38") =~ expr("0")
assert result("F40") =~ expr("0")
*--#] Issue261_1 : 
*--#[ Issue261_2 :
L F11 = div_(1,0);
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue261_2 : 
*--#[ Issue261_3 :
L F23 = rem_(0,0);
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue261_3 : 
*--#[ Issue261_4 :
S x;
#$x = 1 + x + x^2;
#$z = 0;
L F34 = inverse_($x,$z);
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue261_4 : 
*--#[ Issue261_5 :
#$z = 0;
L F16 = div_($z,$z);
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue261_5 : 
*--#[ Issue261_6 :
S x;
#$x = 1 + x + x^2;
L F27 = rem_($x,0);
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue261_6 : 
*--#[ Issue261_7 :
#$z = 0;
L F39 = inverse_(1,$z);
P;
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?
*--#] Issue261_7 : 
*--#[ Issue268_1 :
* Invalid read in Normalize
#define N "9999"
CF f;
S x1,x2;
L F = f(1,...,`N');
id f(x1?,?a,x2?) = f(?a);
id f(?a,x2?) = x2+1;
P;
.end
# On 32-bit systems, "Term too complex during substitution" error occurs.
#require wordsize >= 4
assert succeeded?
assert result("F") =~ expr("9999")
*--#] Issue268_1 :
*--#[ Issue268_2 :
#define N "9999"
CF f;
L F = f(1,...,`N');
id f(?a) = nargs_(?a);
P;
.end
# On 32-bit systems, "Term too complex during substitution" error occurs.
#require wordsize >= 4
assert succeeded?
assert result("F") =~ expr("9999")
*--#] Issue268_2 :
*--#[ Issue277 :
* A question about addargs
CFunction f,g,h;
S x,y;
Local test = f(-1,1)*g(2);
Transform f addargs(1,last);
P;
.sort
id f(?x)*g(y?) = h(y,?x);
P;
.end
assert succeeded?
assert result("test", -2) =~ expr("f(0)*g(2)")
assert result("test", -1) =~ expr("h(2,0)")
*--#] Issue277 : 
*--#[ Issue292 :
#-
#: MaxTermSize 16K
#: SubTermsInSmall 800
#: SubSortIOSize 8K

CFunction f;
AutoDeclare Symbol x;

Local test = x - f(x);
.sort

Identify x = (x1+...+x11)^4;
Argument f;
	Identify x = (x1+...+x11)^4;
EndArgument;
.sort

Identify f(x?) = x;
Print test;
.end
assert result("test") =~ expr("0")
*--#] Issue292 :
*--#[ Issue307 :
* replace_ with nested CFunctions crashes
S N,j1;
CF cfun1,cfun2;
V p;
L test1 = cfun1(cfun2(-p,-3 - j1 + N)*cfun2(p,j1)) * cfun2(p,j1);
L test2 = cfun1(j1,0, - 3 + N,cfun2(-p, - 3 - j1 + N)*cfun2(p,j1));
multiply replace_(N,3);
Print;
.end
assert succeeded?
assert result("test1") =~ expr("cfun1(cfun2(-p, - j1)*cfun2(p,j1))*cfun2(p,j1)")
assert result("test2") =~ expr("cfun1(j1,0,0,cfun2(-p, - j1)*cfun2(p,j1))")
*--#] Issue307 : 
*--#[ Issue313 :
CFunction prf,pf;
Symbol x,y,z;

Local test = prf(x+y,z) + prf(x,z) + prf(y-z,z) + pf(x+y) + pf(x) + pf(y-z);
.sort

Multiply 2;
ModuleOption polyratfun,prf;
.sort
Multiply 2;
ModuleOption polyfun,pf;
.sort
Multiply 2;
ModuleOption polyratfun = prf;
.sort
Multiply 2;
ModuleOption polyfun = pf;
.sort
Multiply 2;
.sort(polyratfun=prf)
Multiply 2;
.sort(polyfun=pf)

* Combination with other ModuleOptions:
ModuleOption polyratfun,prf,parallel;
.sort

Identify pf(x?$tmp) = pf(x);
* This works, but not if you specify the "local" option first:
ModuleOption polyfun, pf, local, $tmp;
.sort

Identify pf(x?$tmp) = pf(x);
* This works, but not if you specify the "local" option first:
ModuleOption polyfun=pf, local, $tmp;

Print;
.end
assert succeeded?
assert result("test") =~ expr("prf(16*x + 16*y - 8*z,z)*pf(8) + prf(8,1)*pf( - 8*z + 16*y + 16*x)")
*--#] Issue313 :
*--#[ Issue324 :
* Wrong implicit symbol declaration in "autodeclare index"
autodeclare index randomIndex=n;
local A = n;
print +s;
.end
assert warning?
assert result("A") =~ expr("+n")
*--#] Issue324 :
*--#[ Issue325 :
* Another wrong outcome of autodeclared index dimension
autodeclare symbol N;
autodeclare index randomIndex=N1;
local A = d_(randomIndex1,randomIndex2)*d_(randomIndex1,randomIndex2);
print +s;
.end
assert succeeded?
assert result("A") =~ expr("+N1")
*--#] Issue325 :
*--#[ Issue336_1 :
#-
CFunction rat,f,tag;
Symbol a,b,y,x;

* Build lots of combinations
Local test = (
	+ f(1)
	+ f(1+x)
	+ f(1+x+x^2)
	+ f(x+x^2)
	+ f(1+x^-1)
	+ f(1+x^-2)
	+ f(1+x+x^-1)
	+ f(x+x^-1)
	+ f(x^-1)
	+ f(x^-2)
	+ f(x^-1+x^-2)
	+ f(1+x+y)
	+ f(1+x+x^2+y+y^2)
	+ f(x+x^2+y+y^2)
	+ f(1+x^-1+y^-1)
	+ f(1+x^-2+y^-2)
	+ f(1+x+x^-1+y+y^-1)
	+ f(x+x^-1+y+y^-1)
	+ f(x^-1+y^-1)
	+ f(x^-2+y^-2)
	+ f(x^-1+x^-2+y^-1+y^-2)
	+ f(x^-1*x^-1)
	+ f(x^-2+x^-1*y^-1+y^-2)
	+ f(x^2+x^1*y^-1+y^-2)
	)^2;
DropCoefficient;
* Create rational polys, with both orderings of the num and den
Identify f(a?)*f(b?) = rat(a,b) + rat(b,a);
.sort
DropCoefficient;
#$i = 0;
Multiply tag($i);
$i = $i+1;
Print +s;
.sort

* Everything should cancel in the end, and we should get zero.
Identify rat(?a) = rat(?a) - x*rat(?a);
Bracket x;
.sort

PolyRatFun rat;
* Give f for overflow, to suppress "Bracket contents too long" warning
Collect f,f;
.sort
Identify x = 1;
Identify f(x?) = x;

Print;
.end
# False-positive valgrind errors on Ubuntu 20.04, MPICH
#pend_if valgrind? && mpi?
assert succeeded?
assert result("test") =~ expr("0")
*--#] Issue336_1 :
*--#[ Issue336_2 :
#-
CFunction rat,f,tag;
Symbol a,b,y,x;

* Build lots of combinations
Local test = (
	+ f(1)
	+ f(1+x)
	+ f(1+x+x^2)
	+ f(x+x^2)
	+ f(1+x^-1)
	+ f(1+x^-2)
	+ f(1+x+x^-1)
	+ f(x+x^-1)
	+ f(x^-1)
	+ f(x^-2)
	+ f(x^-1+x^-2)
	+ f(1+x+y)
	+ f(1+x+x^2+y+y^2)
	+ f(x+x^2+y+y^2)
	+ f(1+x^-1+y^-1)
	+ f(1+x^-2+y^-2)
	+ f(1+x+x^-1+y+y^-1)
	+ f(x+x^-1+y+y^-1)
	+ f(x^-1+y^-1)
	+ f(x^-2+y^-2)
	+ f(x^-1+x^-2+y^-1+y^-2)
	+ f(x^-1*x^-1)
	+ f(x^-2+x^-1*y^-1+y^-2)
	+ f(x^2+x^1*y^-1+y^-2)
	)^2;
DropCoefficient;
* Create rational polys, with both orderings of the num and den
Identify f(a?)*f(b?) = rat(a,b) + rat(b,a);
.sort
DropCoefficient;
#$i = 0;
Multiply tag($i);
$i = $i+1;
Print +s;
.sort

* Everything should cancel in the end, and we should get zero.
Identify rat(?a) = rat(?a) - f(rat(?a));
.sort

PolyRatFun rat;
.sort

Identify f(x?) = x;

Print;
.end
# False-positive valgrind errors on Ubuntu 20.04, MPICH
#pend_if valgrind? && mpi?
assert succeeded?
assert result("test") =~ expr("0")
*--#] Issue336_2 :
*--#[ Issue340 :
* Calling "argtoextrasymbol" of a function containing "g5_" crashes
CF f, g;

L A = f( g(g5_(1)) );
L B = f( g5_(1) );
L C = f( gi_(1) );

argtoextrasymbol f;

print +s;
.end
assert succeeded?
assert result("A") =~ expr("+f(Z1_)")
assert result("B") =~ expr("+f(Z2_)")
assert result("C") =~ expr("+f(Z3_)")
*--#] Issue340 : 
*--#[ Issue345 :
* PolyRatFun and Argument
Symbol x,s,t,u,m1,m2;
Symbol q1q2,q1q3,q2q3,q3q3;
CFunction rat;

PolyRatFun rat;
Local test = x*rat( - 4*s^2,q1q2^2*q3q3 - 2*q1q2*q1q3*q2q3);
.sort

Argument;
  Identify q1q2 = s/2;
  Identify q1q3 = (t-m1^2)/2;
  Identify q2q3 = (-s-t+m1^2+m2^2)/2;
  Identify q3q3 = m1^2;
EndArgument;

* This works fine:
*Multiply replace_(q1q2,s/2);
*Multiply replace_(q1q3,(t-m1^2)/2);
*Multiply replace_(q2q3,(-s-t+m1^2+m2^2)/2);
*Multiply replace_(q3q3,m1^2);

* PROBLEM 1
* rat is not properly normalized at the end of this module, in the case of the Argument environment
Print +s;
.sort

* PROBLEM 2
* move some other symbol into the rat
* Now the overall factor of the denominator is lost completely
Identify x^s? = rat(x^s,1);
.sort

Print +s;
.end
assert succeeded?
assert result("test", -2) =~ expr("
       + x*rat( - 16*s,s*t + t^2 - 2*t*m1^2 - t*m2^2 + m1^4 + m1^2*m2^2)
")
assert result("test", -1) =~ expr("
       + rat( - 16*x*s,s*t + t^2 - 2*t*m1^2 - t*m2^2 + m1^4 + m1^2*m2^2)
")
*--#] Issue345 : 
*--#[ Issue353_1 :
* Puzzling behavior of factarg (freezes etc.)
CF f;
S p1,p2;
L exp = f(-p1-p2);
factarg,(0),f;
print;
.end
assert succeeded?
assert result("exp") =~ expr("f(p2+p1)")
*--#] Issue353_1 : 
*--#[ Issue353_2 :
CF f;
S p1,p2;
L exp = f(p1+p2);
factarg,(0),f;
print;
.end
assert succeeded?
assert result("exp") =~ expr("f(p2+p1)")
*--#] Issue353_2 : 
*--#[ Issue353_3 :
CF f;
V p1,p2;
L exp = f(p1+2*p2);
factarg,(0),f;
print;
.end
assert succeeded?
assert result("exp") =~ expr("f(p1+2*p2)")
*--#] Issue353_3 : 
*--#[ Issue353_4 :
CF f;
S x;
L F = f((1+x)^2);
factarg(0);
P;
.end
assert succeeded?
assert result("F") =~ expr("f(1+2*x+x^2)")
*--#] Issue353_4 : 
*--#[ Issue358 :
* Replacing power sign using dictionaries
Symbols x,y;
#OpenDictionary test
  #add ^:"**"
#CloseDictionary
Local F = x^2;
#UseDictionary test
Print;
.end
assert succeeded?
assert result("F") =~ expr("x**2")
*--#] Issue358 : 
*--#[ Issue359 :
* Inconsistent use of power sign with "Format reduce"
Symbols x;
CFunctions f;
Local F = f(x^2)+f(x)^2;
Format reduce;
Print;
.end
assert succeeded?
assert result("F") =~ expr("f(x**2) + f(x)**2")
*--#] Issue359 : 
*--#[ Issue400 :
* denominators statement for nested functions
S x;
CF den;
L F1 = 1/(1+x);
L F2 = 1/(1+1/(1+x));
L F3 = 1/(1+1/(1+1/(1+x)));
L F4 = 1/(1+1/(1+1/(1+1/(1+x))));
denominators den;
multiply replace_(x,2);
P;
.end
assert succeeded?
assert result("F1") =~ expr("den(3)")
assert result("F2") =~ expr("den(1 + den(3))")
assert result("F3") =~ expr("den(1 + den(1 + den(3)))")
assert result("F4") =~ expr("den(1 + den(1 + den(1 + den(3))))")
*--#] Issue400 :
*--#[ Issue405 :
* Dimension of index gets wrongly mapped when it is declared as auto symbol
Auto S D;
Auto I mu=D;
L F3 = d_(mu3,mu3);
Print +s;
.end
assert succeeded?
assert result("F3") =~ expr("+D")
*--#] Issue405 :
*--#[ Issue434 :
* Memory error with macros with arguments
* The loop (appending a variable and calling the macro) is repeated enough
* to hit the problem (Valgrind error).
#define var(a) "`~a'"
#do n=1,{12*2^5}
  #define x
  #message `var(`n')'
#enddo
.end
assert succeeded?
*--#] Issue434 :
*--#[ Issue460_1 :
* Improve format mathematica, for powers of dot products
Vector p,q;
Local test = p.q + p.q^2 + 1/p.q + 1/p.q^2;
Format Mathematica;
Print;
.end
assert succeeded?
assert result("test") =~ expr("((p.q)^(-2) + (p.q)^(-1) + (p.q) + (p.q)^2)")
*--#] Issue460_1 :
*--#[ Issue460_2 :
* Improve format mathematica, for powers of dot products
* Check regular mode is unchanged:
Vector p,q;
Local test = p.q + p.q^2 + 1/p.q + 1/p.q^2;
Print;
.end
assert succeeded?
assert result("test") =~ expr("p.q^-2 + p.q^-1 + p.q + p.q^2")
*--#] Issue460_2 :
*--#[ Issue460_3 :
* Improve format mathematica, for powers of dot products
* Also check C mode:
Vector p,q;
Local test = p.q + p.q^2 + 1/p.q + 1/p.q^2;
Format C;
Print;
.end
assert succeeded?
assert result("test") =~ expr("pow(p_q,-2) + pow(p_q,-1) + p_q + pow(p_q,2)")
*--#] Issue460_3 :
*--#[ Issue460_4 :
* Improve format mathematica, for powers of dot products
* Also check Fortran mode:
Vector p,q;
Local test = p.q + p.q^2 + 1/p.q + 1/p.q^2;
Format Fortran;
Print;
.end
assert succeeded?
assert stdout =~ exact_pattern(<<'EOF')
& p_q**(-2) + p_q**(-1) + p_q + p_q**2
EOF
*--#] Issue460_4 :
*--#[ Issue468 :
#-

#: MaxTermSize 200K
#: SortIOSize 200K
#: SubSortIOSize 200K

#: SubLargeSize 134400480
#: SubSmallSize 12800016
#: SubSmallExtension 19200032
#: SubTermsInSmall 5008

#define N "30"

Symbol a,b,c,d,e;
CFunction prf;

Local test =
	+ prf((a+b+c+d)^`N'+1,(a+b+c+d)^`N')
	- prf((a+b+c+d)^`N'-1,(a+b+c+d)^`N')
	;
.sort

#message Enable prf
PolyRatFun prf;
.sort

Multiply prf((a+b+c+d)^`N',1);

Print;
.end
#require wordsize >= 4
assert succeeded?
assert result("test") =~ expr("prf(2,1)")
*--#] Issue468 :
*--#[ Issue486 :
On codes;
Symbol x,y;
Set empty;
Set nonempty: x,y;
.end
assert succeeded?
assert stdout =~ exact_pattern(<<'EOF')
   empty(13):
   nonempty(14): x y
EOF
*--#] Issue486 :
*--#[ Issue490_1 :
T t;
I i1,...,i4;
L F = t(i1,i3)*t(i2,i4);
id t(i1?,?a)*t(i2?,?b) = t(i1,i2,?a,?b);
P;
.end
assert succeeded?
assert result("F") =~ expr("t(i1,i2,i3,i4)")
*--#] Issue490_1 :
*--#[ Issue490_2 :
T t(cyclic);
I i1,...,i4;
L F = t(i1,i3)*t(i2,i4);
id t(i1?,?a)*t(i2?,?b) = t(i1,i2,?a,?b);
P;
.end
assert succeeded?
assert result("F") =~ expr("t(i1,i2,i3,i4)")
*--#] Issue490_2 :
*--#[ Issue490_3 :
T t(cyclic);
I i1,...,i6;
L F = t(i1,i3,i2,i5)*t(i1,i4,i2,i6);
id t(i1?,?a,i2?,?c)*t(i1?,?b,i2?,?d) = t(i1,i2)*t(?a,?b)*t(?c,?d);
P;
.end
assert succeeded?
assert result("F") =~ expr("t(i1,i2)*t(i3,i4)*t(i5,i6)")
*--#] Issue490_3 :
*--#[ Issue490_4 :
T t;
CF f;
I i1,i2;
L F = t(i1,i2)*t(i1,i2);
id t(?a)*t(?a) = f(?a);
P;
.end
assert succeeded?
assert result("F") =~ expr("f(i1,i2)")
*--#] Issue490_4 :
*--#[ Issue499_1 :
* Freeze in parsing complex conjugate
S x#c;
L F = x#;
.end
assert compile_error?("Complex conjugate operator (#) is not implemented")
*--#] Issue499_1 :
*--#[ Issue499_2 :
S x#c;
L F = #x;
.end
assert compile_error?("Illegal position for #")
*--#] Issue499_2 :
*--#[ Issue508 :
Off statistics;
cf f g;
s N;
on highfirst;
l F =
#do i=1,5000
+ f(`i')*(g(1+N/`i')*g(N/`i'-1))^25
#enddo
;
.sort
makeinteger g;
id f(N?) = N^50;
id g(N?) = 1;
print;
.end
#require linux?
#ulimit -v 8_000_000
# We assume more memory than a 32bit system can provide
#require wordsize >= 4
assert succeeded?
assert result("F") =~ expr("5000")
*--#] Issue508 :
*--#[ Issue512_1 :
#-
* Sort which fills SmallExtension:

* These are the smallest buffer sizes that are OK for -w4 tform workers
#: SmallSize 10240064
#: SmallExtension 15360096
#: TermsInSmall 100K

Symbol x,n;
CFunction g,f,prf;

Local test = (<f(1)>+...+<f(150)>)*(<g(1)>+...+<g(350)>);
.sort

PolyRatFun prf;

Identify f(x?) = prf(n-x,n+x);

.end
# Fails due to polynomial size on 32bit builds
#require wordsize >= 4
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("Please increase SmallExtension setup parameter.")
*--#] Issue512_1 :
*--#[ Issue512_2 :
#-

* Sort which fills SubSmallExtension:
* These are the default sizes at the time of writing:
#: SubSmallSize 2560016
#: SubSmallExtension 3840032
* These are not default:
#: SubTermsInSmall 100K

Symbol x,n;
CFunction f,g,prf;

Local test = 1;
.sort

PolyRatFun prf;
Term;
	Multiply (<f(1)>+...+<f(150)>)*(<g(1)>+...+<g(100)>);
	Identify f(x?) = prf(n-x,n+x);
EndTerm;

.end
# Fails due to polynomial size on 32bit builds
#require wordsize >= 4
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("Please increase SubSmallExtension setup parameter.")
*--#] Issue512_2 :
*--#[ Issue512_3 :
#-

* Sort which fits in SmallExtension, but needs GarbHand
#: SmallSize 10240064
#: SmallExtension 20360K
#: TermsInSmall 100K

Symbol x,n;
CFunction g,f,prf;

Local test = (<f(1)>+...+<f(150)>)*(<g(1)>+...+<g(350)>);
.sort

PolyRatFun prf;
Identify f(x?) = prf(n-x,n+x);

.end
# Fails due to polynomial size on 32bit builds
#require wordsize >= 4
assert succeeded?
*--#] Issue512_3 :
*--#[ Issue512_4 :
#-

* Sort which fits in SubSmallExtension, but needs GarbHand
* These are the default sizes at the time of writing:
#: SubSmallSize 2560016
* These are not default:
#: SubSmallExtension 5090K
#: SubTermsInSmall 100K

Symbol x,n;
CFunction f,g,prf;

Local test = 1;
.sort

PolyRatFun prf;
Term;
	Multiply (<f(1)>+...+<f(150)>)*(<g(1)>+...+<g(100)>);
	Identify f(x?) = prf(n-x,n+x);
EndTerm;

.end
# Fails due to polynomial size on 32bit builds
#require wordsize >= 4
assert succeeded?
*--#] Issue512_4 :
*--#[ Issue546 :
#-
Off statistics;
Symbol x,y;

Local test =
	+ 1
	+ 3*x
	+ 3/4*x^2
	+ (2^y-1)*x^3
	+ (2^y)*x^4
	+ 1/(2^y-1)*x^5
	+ 1/(2^y)*x^6
	+ 3/(2^y-1)*x^7
	+ 3/(2^y)*x^8
	+ (2^y)/(2^y-1)*x^9
	+ (2^y-1)/(2^y)*x^10
	+ (2^y)/(2^y+1)*x^11
	;
.sort
Drop;
Local test31 = replace_(y,31)*test;
Local test32 = replace_(y,32)*test;

#message C
Format C;
Print +s;
.sort
#message Fortran
Format Fortran;
Print +s;
.sort
#message Doublefortran
Format Doublefortran;
Print +s;
.sort
#message Quadfortran
Format Quadfortran;
Print +s;
.sort
#message Fortran90
Format Fortran90;
Print +s;
.sort
#message Fortran90_ki
Format Fortran90,_ki;
Print +s;
.sort
#message Pfortran;
Format Pfortran;
Print +s;
.sort
#message Float
Format Float;
Print +s;
.sort
#message Normal
Format Normal;
Print +s;
.sort
.end
# 32bit FORM prints these differently: there a WORD is 16 bits so 2^31-1 etc
# all require multiple WORD to store, and so trigger a floating suffix.
#require wordsize >= 4
assert succeeded?
assert stdout =~ exact_pattern(<<'EOF')
~~~C

   test31 =
       + 1
       + 3*x
       + 3./4.*pow(x,2)
       + 2147483647*pow(x,3)
       + 2147483648*pow(x,4)
       + 1./2147483647.*pow(x,5)
       + 1./2147483648.*pow(x,6)
       + 3./2147483647.*pow(x,7)
       + 3./2147483648.*pow(x,8)
       + 2147483648./2147483647.*pow(x,9)
       + 2147483647./2147483648.*pow(x,10)
       + 2147483648./2147483649.*pow(x,11)
      ;

   test32 =
       + 1
       + 3*x
       + 3./4.*pow(x,2)
       + 4294967295*pow(x,3)
       + 4294967296.*pow(x,4)
       + 1./4294967295.*pow(x,5)
       + 1./4294967296.*pow(x,6)
       + 1./1431655765.*pow(x,7)
       + 3./4294967296.*pow(x,8)
       + 4294967296./4294967295.*pow(x,9)
       + 4294967295./4294967296.*pow(x,10)
       + 4294967296./4294967297.*pow(x,11)
      ;

~~~Fortran

      test31 = + 1
     &  + 3*x
     &  + 3./4.*x**2
     &  + 2147483647*x**3
     &  + 2147483648.*x**4
     &  + 1./2147483647.*x**5
     &  + 1./2147483648.*x**6
     &  + 3./2147483647.*x**7
     &  + 3./2147483648.*x**8
     &  + 2147483648./2147483647.*x**9
     &  + 2147483647./2147483648.*x**10
     &  + 2147483648./2147483649.*x**11
     &

      test32 = + 1
     &  + 3*x
     &  + 3./4.*x**2
     &  + 4294967295.*x**3
     &  + 4294967296.*x**4
     &  + 1./4294967295.*x**5
     &  + 1./4294967296.*x**6
     &  + 1./1431655765.*x**7
     &  + 3./4294967296.*x**8
     &  + 4294967296./4294967295.*x**9
     &  + 4294967295./4294967296.*x**10
     &  + 4294967296./4294967297.*x**11
     &

~~~Doublefortran

      test31 = + 1
     &  + 3*x
     &  + 3.D0/4.D0*x**2
     &  + 2147483647*x**3
     &  + 2147483648.D0*x**4
     &  + 1.D0/2147483647.D0*x**5
     &  + 1.D0/2147483648.D0*x**6
     &  + 3.D0/2147483647.D0*x**7
     &  + 3.D0/2147483648.D0*x**8
     &  + 2147483648.D0/2147483647.D0*x**9
     &  + 2147483647.D0/2147483648.D0*x**10
     &  + 2147483648.D0/2147483649.D0*x**11
     &

      test32 = + 1
     &  + 3*x
     &  + 3.D0/4.D0*x**2
     &  + 4294967295.D0*x**3
     &  + 4294967296.D0*x**4
     &  + 1.D0/4294967295.D0*x**5
     &  + 1.D0/4294967296.D0*x**6
     &  + 1.D0/1431655765.D0*x**7
     &  + 3.D0/4294967296.D0*x**8
     &  + 4294967296.D0/4294967295.D0*x**9
     &  + 4294967295.D0/4294967296.D0*x**10
     &  + 4294967296.D0/4294967297.D0*x**11
     &

~~~Quadfortran

      test31 = + 1
     &  + 3*x
     &  + 3.Q0/4.Q0*x**2
     &  + 2147483647*x**3
     &  + 2147483648.Q0*x**4
     &  + 1.Q0/2147483647.Q0*x**5
     &  + 1.Q0/2147483648.Q0*x**6
     &  + 3.Q0/2147483647.Q0*x**7
     &  + 3.Q0/2147483648.Q0*x**8
     &  + 2147483648.Q0/2147483647.Q0*x**9
     &  + 2147483647.Q0/2147483648.Q0*x**10
     &  + 2147483648.Q0/2147483649.Q0*x**11
     &

      test32 = + 1
     &  + 3*x
     &  + 3.Q0/4.Q0*x**2
     &  + 4294967295.Q0*x**3
     &  + 4294967296.Q0*x**4
     &  + 1.Q0/4294967295.Q0*x**5
     &  + 1.Q0/4294967296.Q0*x**6
     &  + 1.Q0/1431655765.Q0*x**7
     &  + 3.Q0/4294967296.Q0*x**8
     &  + 4294967296.Q0/4294967295.Q0*x**9
     &  + 4294967295.Q0/4294967296.Q0*x**10
     &  + 4294967296.Q0/4294967297.Q0*x**11
     &

~~~Fortran90

      test31 = + 1
     &  + 3*x
     &  + 3./4.*x**2
     &  + 2147483647*x**3
     &  + 2147483648.*x**4
     &  + 1./2147483647.*x**5
     &  + 1./2147483648.*x**6
     &  + 3./2147483647.*x**7
     &  + 3./2147483648.*x**8
     &  + 2147483648./2147483647.*x**9
     &  + 2147483647./2147483648.*x**10
     &  + 2147483648./2147483649.*x**11
     &

      test32 = + 1
     &  + 3*x
     &  + 3./4.*x**2
     &  + 4294967295.*x**3
     &  + 4294967296.*x**4
     &  + 1./4294967295.*x**5
     &  + 1./4294967296.*x**6
     &  + 1./1431655765.*x**7
     &  + 3./4294967296.*x**8
     &  + 4294967296./4294967295.*x**9
     &  + 4294967295./4294967296.*x**10
     &  + 4294967296./4294967297.*x**11
     &

~~~Fortran90_ki

      test31 = + 1_ki
     &  + 3_ki*x
     &  + 3_ki/4_ki*x**2
     &  + 2147483647_ki*x**3
     &  + 2147483648_ki*x**4
     &  + 1_ki/2147483647_ki*x**5
     &  + 1_ki/2147483648_ki*x**6
     &  + 3_ki/2147483647_ki*x**7
     &  + 3_ki/2147483648_ki*x**8
     &  + 2147483648_ki/2147483647_ki*x**9
     &  + 2147483647_ki/2147483648_ki*x**10
     &  + 2147483648_ki/2147483649_ki*x**11
     &

      test32 = + 1_ki
     &  + 3_ki*x
     &  + 3_ki/4_ki*x**2
     &  + 4294967295_ki*x**3
     &  + 4294967296_ki*x**4
     &  + 1_ki/4294967295_ki*x**5
     &  + 1_ki/4294967296_ki*x**6
     &  + 1_ki/1431655765_ki*x**7
     &  + 3_ki/4294967296_ki*x**8
     &  + 4294967296_ki/4294967295_ki*x**9
     &  + 4294967295_ki/4294967296_ki*x**10
     &  + 4294967296_ki/4294967297_ki*x**11
     &

~~~Pfortran

      test31 = + one
     &  + 3*x
     &  + ((one*3)/4)*x**2
     &  + 2147483647*x**3
     &  + 2147483648.D0*x**4
     &  + (one/2147483647)*x**5
     &  + (one/2147483648.D0)*x**6
     &  + ((one*3)/2147483647)*x**7
     &  + ((one*3)/2147483648.D0)*x**8
     &  + ((one*2147483648.D0)/2147483647)*x**9
     &  + ((one*2147483647)/2147483648.D0)*x**10
     &  + ((one*2147483648.D0)/2147483649.D0)*x**11
     &

      test32 = + one
     &  + 3*x
     &  + ((one*3)/4)*x**2
     &  + 4294967295.D0*x**3
     &  + 4294967296.D0*x**4
     &  + (one/4294967295.D0)*x**5
     &  + (one/4294967296.D0)*x**6
     &  + (one/1431655765)*x**7
     &  + ((one*3)/4294967296.D0)*x**8
     &  + ((one*4294967296.D0)/4294967295.D0)*x**9
     &  + ((one*4294967295.D0)/4294967296.D0)*x**10
     &  + ((one*4294967296.D0)/4294967297.D0)*x**11
     &

~~~Float

      test31 = + 1.E+0
     &  + 3.E+0*x
     &  + 7.5E-1*x**2
     &  + 2.147483647E+9*x**3
     &  + 2.147483648E+9*x**4
     &  + 4.656612875E-10*x**5
     &  + 4.656612873E-10*x**6
     &  + 1.396983862E-9*x**7
     &  + 1.396983861E-9*x**8
     &  + 1.000000000E+0*x**9
     &  + 9.999999995E-1*x**10
     &  + 9.999999995E-1*x**11
     &

      test32 = + 1.E+0
     &  + 3.E+0*x
     &  + 7.5E-1*x**2
     &  + 4.294967295E+9*x**3
     &  + 4.294967296E+9*x**4
     &  + 2.328306437E-10*x**5
     &  + 2.328306436E-10*x**6
     &  + 6.984919311E-10*x**7
     &  + 6.984919309E-10*x**8
     &  + 1.000000000E+0*x**9
     &  + 9.999999997E-1*x**10
     &  + 9.999999997E-1*x**11
     &

~~~Normal

   test31 =
       + 1
       + 3*x
       + 3/4*x^2
       + 2147483647*x^3
       + 2147483648*x^4
       + 1/2147483647*x^5
       + 1/2147483648*x^6
       + 3/2147483647*x^7
       + 3/2147483648*x^8
       + 2147483648/2147483647*x^9
       + 2147483647/2147483648*x^10
       + 2147483648/2147483649*x^11
      ;

   test32 =
       + 1
       + 3*x
       + 3/4*x^2
       + 4294967295*x^3
       + 4294967296*x^4
       + 1/4294967295*x^5
       + 1/4294967296*x^6
       + 1/1431655765*x^7
       + 3/4294967296*x^8
       + 4294967296/4294967295*x^9
       + 4294967295/4294967296*x^10
       + 4294967296/4294967297*x^11
      ;
EOF
*--#] Issue546 :
*--#[ Issue525 :
#:threadbucketsize 5
#:processbucketsize 5
S x;
L F = (1-x)^100;
L F1 = 1;
L F2 = 1;
.sort
#define x "0"
if (expression(F1)) redefine x "1";
.sort
id x = `x';
P F;
.end
assert succeeded?
assert result("F") =~ expr("0")
*--#] Issue525 :
*--#[ Issue544 :
#-
Off Statistics;

Symbol x;
Vector D,p,q;
CFunction tag;
CFunction f,g,h,i,j,k,l,m;
CFunction F,G,H,I,J,K,L,M;

#define N "3"

Local test =
	#do i = -`N',`N'
		+ f(p,`i')
		+ f(-q,`i')
		+ f(p,q,`i')
		+ f(-p,q,`i')
		+ f(p,-q,`i')
		+ f(-p,-q,`i')
	#enddo
	;

* Use tags to make sure the cancellation is unique
Identify f(?a) =
	+ (f(?a) - F(?a)) * tag(f,?a)
	+ (g(?a) - G(?a)) * tag(g,?a)
	+ (h(?a) - H(?a)) * tag(h,?a)
	+ (i(?a) - I(?a)) * tag(i,?a)
	+ (j(?a) - J(?a)) * tag(j,?a)
	+ (k(?a) - K(?a)) * tag(k,?a)
	+ (l(?a) - L(?a)) * tag(l,?a)
	+ (m(?a) - M(?a)) * tag(m,?a)
	;
.sort

Identify f(p?,x?) = D.p^x;
Identify f(p?,q?,x?) = p.q^x;
Identify g(p?,x?) = D.p^-x;
Identify g(p?,q?,x?) = p.q^-x;

Identify h(p?,x?) = (D.p)^x;
Identify h(p?,q?,x?) = (p.q)^x;
Identify i(p?,x?) = D.p^(x);
Identify i(p?,q?,x?) = p.q^(x);

* And with - signs on the pattern vector:
Identify j(-p?,x?) = D.p^x;
Identify j(-p?,-q?,x?) = p.q^x;
Identify k(-p?,x?) = D.p^-x;
Identify k(-p?,-q?,x?) = p.q^-x;

Identify l(-p?,x?) = (D.p)^x;
Identify l(-p?,-q?,x?) = (p.q)^x;
Identify m(-p?,x?) = D.p^(x);
Identify m(-p?,-q?,x?) = p.q^(x);

* Cancel all terms, with no pattern for the power
#do i = -`N',`N'
	Identify F(p?,`i') = D.p^`i';
	Identify F(p?,q?,`i') = p.q^`i';
	Identify G(p?,`i') = D.p^-`i';
	Identify G(p?,q?,`i') = p.q^-`i';

	Identify H(p?,`i') = (D.p)^`i';
	Identify H(p?,q?,`i') = (p.q)^`i';
	Identify I(p?,`i') = D.p^(`i');
	Identify I(p?,q?,`i') = p.q^(`i');

	Identify J(-p?,`i') = D.p^`i';
	Identify J(-p?,-q?,`i') = p.q^`i';
	Identify K(-p?,`i') = D.p^-`i';
	Identify K(-p?,-q?,`i') = p.q^-`i';

	Identify L(-p?,`i') = (D.p)^`i';
	Identify L(-p?,-q?,`i') = (p.q)^`i';
	Identify M(-p?,`i') = D.p^(`i');
	Identify M(-p?,-q?,`i') = p.q^(`i');
#enddo

Print +s;
.end
assert succeeded?
assert result("test") =~ expr("0")
*--#] Issue544 :
*--#[ Issue554_1 :
CF f;
S x;
L F = f(x);
id f(x?{}) = x;
print;
.end
assert succeeded?
assert result("F") =~ expr("f(x)")
*--#] Issue554_1 :
*--#[ Issue554_2 :
CF f;
S x;
L F = f(x);
id f(x?!{}) = x;
print;
.end
assert succeeded?
assert result("F") =~ expr("x")
*--#] Issue554_2 :
*--#[ Issue554_3 :
CF f;
S x;
Set empty: ;
L F = f(x);
id f(x?empty) = x;
print;
.end
assert succeeded?
assert result("F") =~ expr("f(x)")
*--#] Issue554_3 :
*--#[ Issue554_4 :
CF f;
S x;
Set empty: ;
L F = f(x);
id f(x?!empty) = x;
print;
.end
assert succeeded?
assert result("F") =~ expr("x")
*--#] Issue554_4 :
*--#[ Issue554_5 :
CF f;
S x,y;
L F = f(1)+f(2)+f(3);
id f(x?{}) = x;
id f(y?{1,2}) = x^y;
print;
.end
assert succeeded?
assert result("F") =~ expr("x + x^2 + f(3)")
*--#] Issue554_5 :
*--#[ Issue554_6 :
#-
CF f,g,h,i;
S x;

#$x1 = f(1,3,5);
#$x2 = f();

#inside $x1
  if (match(f(?a$a)));
  endif;
#endinside
#inside $x2
  if (match(f(?a$b)));
  endif;
#endinside

L F = f(1,2,3,4,5,6);
L G = g(1,2,3,4,5,6);
L H = h(1,2,3,4,5,6);
L I = i(1,2,3,4,5,6);

repeat id f(?a,x? {`$a',},?b) = x * f(?a,?b);
repeat id g(?a,x?!{`$a',},?b) = x * g(?a,?b);
repeat id h(?a,x? {`$b',},?b) = x * h(?a,?b);
repeat id i(?a,x?!{`$b',},?b) = x * i(?a,?b);

P;
.end
assert succeeded?
assert result("F") =~ expr("15*f(2,4,6)")
assert result("G") =~ expr("48*g(1,3,5)")
assert result("H") =~ expr("h(1,2,3,4,5,6)")
assert result("I") =~ expr("720*i")
*--#] Issue554_6 :
*--#[ Issue563 :
#: SubTermsInSmall 64

CFunction f,g;
Symbol a;

* Generate a function arg with more than SubTermsInSmall terms:
* Make sure one of the factors has more than SubTermsInSmall itself.
* Use functions to trigger LocalConvertToPoly and the EndSort there.
Local F = f((g(1)+g(2)) * (<g(1)>+...+<g(65)>)) - g(1+2, 1+...+65);

FactArg f;
Argument f;
	Identify g(a?) = a;
EndArgument;
Identify g(?a) = f(?a);

Print F;
.end
assert succeeded?
assert result("F") =~ expr("0")
*--#] Issue563 :
*--#[ Issue567_1 :
CF rat;
Vector v;
PolyRatFun rat;
Local F = rat(v,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_1 :
*--#[ Issue567_2 :
CF rat;
Index i;
PolyRatFun rat;
Local F = rat(i,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_2 :
*--#[ Issue567_3a :
CF rat;
Function f;
PolyRatFun rat;
Local F = rat(f,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_3a :
*--#[ Issue567_3b :
CF rat;
CFunction f;
PolyRatFun rat;
Local F = rat(f,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_3b :
*--#[ Issue567_3c :
CF rat;
Table f(1);
PolyRatFun rat;
Local F = rat(f,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_3c :
*--#[ Issue567_3d :
CF rat;
CTable f(1);
PolyRatFun rat;
Local F = rat(f,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_3d :
*--#[ Issue567_3e :
CF rat;
Tensor f;
PolyRatFun rat;
Local F = rat(f,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_3e :
*--#[ Issue567_3f :
CF rat;
CTensor f;
PolyRatFun rat;
Local F = rat(f,1);
.end
# Runtime errors may freeze ParFORM.
#pend_if mpi?
assert runtime_error?("ERROR: polynomials and polyratfuns must contain symbols only")
*--#] Issue567_3f :
*--#[ Issue577_1 :
#-
Off stats;
Symbol x,y,z;

Local test1 = 1;
.sort
Hide test1;
Local test2 = 2;
.sort

Local test3 = 3;

#if ( isnumerical(test1) )
  #message test1
#endif
#if ( isnumerical(test2) )
  #message test2
#endif
* This causes and error and terminate: test3 is not defined when preprocessing.
*#if ( isnumerical(test3) )
*  #message test3
*#endif
#message module1
.sort

#if ( isnumerical(test1) )
  #message test1
#endif
#if ( isnumerical(test2) )
  #message test2
#endif
#if ( isnumerical(test3) )
  #message test3
#endif
#message module2
.sort

Local test4 = x*firstterm_(test1) + y*firstterm_(test2) + z*firstterm_(test3);
Local test5 = x*firstterm_(test4);

Multiply 2;

print;
.end
# ParFORM has valgrind errors with this. See discussion in PR 586.
#pend_if mpi?
assert succeeded?
assert result("test2") =~ expr("4")
assert result("test3") =~ expr("6")
assert result("test4") =~ expr("6*z + 4*y + 2*x")
# This one does not work in TFORM. Consider it to be "illegal".
# assert result("test5") =~ expr("12*x*z")
assert stdout =~ exact_pattern(<<'EOF')
~~~test1
~~~test2
~~~module1
~~~test1
~~~test2
~~~test3
~~~module2
EOF
*--#] Issue577_1 :
*--#[ Issue577_2 :
#-
Off stats;

Local test3 = 3;

* This causes and error and terminate: test3 is not defined when preprocessing.
#if ( isnumerical(test3) )
  #message test3
#endif
.end
# ParFORM has valgrind errors with this. See discussion in PR 586.
#pend_if mpi?
assert runtime_error?("isnumerical: expression is not yet defined!")
*--#] Issue577_2 :
*--#[ Issue599 :
#-
On names;
Off statistics;
CFunction A1,...,A6;
Symbol j,x,z,y;

Local ff =
	+ A1(1) + A2(2) + A3(3)
		+ A4(1) + A5(2) + A6(3)
		;

Identify A1(j?{1,2,3}[x]) = A1({ 10, 20, 30}[x]);
Identify A2(j?{1,2,3}[x]) = A2({+10,+20,+30}[x]);
Identify A3(j?{1,2,3}[x]) = A3({-10,-20,-30}[x]);
Identify A4(j?{1,2,3}[x]) = A4({+-10,--20,-+30}[x]);
Identify A5(j?{1,2,3}[x]) = A5({--10,-+20,+-30}[x]);
Identify A6(j?{1,2,3}[x]) = A6({-+10,+-20,--30}[x]);

Print;
.end
assert succeeded?
assert result("ff") =~ expr("A1(10) + A2(20) + A3(-30) + A4(-10) + A5(-20) + A6(30)")
assert stdout =~ exact_pattern(<<'EOF')
 Sets
   {}: 1 2 3
   {}: 10 20 30
   {}: -10 -20 -30
   {}: -10 20 -30
   {}: 10 -20 -30
   {}: -10 -20 30
EOF
*--#] Issue599 : 
*--#[ PullReq535 :
* This test requires more than the specified 50K workspace.
#:maxtermsize 200
#:workspace 50000
S x1,...,x19;
L F = (x1+...+x19)^4;
Format O1;
.sort
#optimize F
L G = `optimvalue_';
P G;
.end
assert succeeded?
assert result("G") =~ expr("389")
*--#] PullReq535 :
