#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ divmod_1 :
* Test div_, rem_ functions for monic univariate polynomials
#-
S x1;

L F1 = (x1+1)^2;
L F2 = (x1+1);
.sort
L F3 = div_(F1,F2);
L F4 = rem_(F1,F2);

P;
.end
assert succeeded?
assert result("F3")  =~ expr("1 + x1")
assert result("F4")  =~ expr("0")
*--#] divmod_1 :
*--#[ divmod_2 :
* Test div_, rem_ functions for non-monic univariate polynomials with remainder
#-
S x1;

L F1 = (2*x1+1)^2+3*x1+5;
L F2 = (2*x1+1);
.sort
L F3 = div_(F1,F2);
L F4 = rem_(F1,F2);

P;
.end
assert succeeded?
assert result("F3")  =~ expr("5/2 + 2*x1")
assert result("F4")  =~ expr("7/2")
*--#] divmod_2 :
*--#[ divmod_3 :
* Test div_, rem_ functions for non-monic multivariate polynomials without remainder
#-
S x1,x2;

L F1 = (2*x1*x2+1*x1)^2;
L F2 = (2*x1*x2+1*x1);
.sort
L F3 = div_(F1,F2);
L F4 = rem_(F1,F2);

P;
.end
assert succeeded?
assert result("F3")  =~ expr("x1 + 2*x1*x2")
assert result("F4")  =~ expr("0")
*--#] divmod_3 :
*--#[ divmod_4 :
* Test div_, rem_ functions for non-monic multivariate polynomials with remainder
#-
S x1,x2,x3;

L F1 = -7*x1*x2^9*x3+5*x1*x2^10*x3-3*x1^2*x2^3*x3^2+35*x1^2*x2^4*x3^4
-42*x1^2*x2^5*x3^2-25*x1^2*x2^5*x3^4+30*x1^2*x2^6*x3^2-8*x1^4*x2^5*x3^3+40*x1^5*x3^6
-48*x1^5*x2*x3^4+3*x1^6*x2^2*x3;
L F2 = x1*x2^5-5*x1^2*x3^3+6*x1^2*x2*x3;
L F3 = div_(F1,F2);
L F4 = rem_(F1,F2);

P;
.end
assert succeeded?
assert result("F3")  =~ expr("
      - 25/72*x3^5 + 12969970703125/2742118830047232*x3^30 - 5/12*x2*x3^3 +
      2593994140625/457019805007872*x2*x3^28 - 1/2*x2^2*x3 + 518798828125/
      76169967501312*x2^2*x3^26 + 103759765625/12694994583552*x2^3*x3^24 - 7*
      x2^4*x3 + 20751953125/2115832430592*x2^4*x3^22 + 5*x2^5*x3 + 29052734375/
      2821109907456*x2^5*x3^20 + 830078125/78364164096*x2^6*x3^18 + 830078125/
      78364164096*x2^7*x3^16 + 33203125/3265173504*x2^8*x3^14 + 6640625/
      725594112*x2^9*x3^12 + 5703125/725594112*x2^10*x3^10 + 765625/120932352*
      x2^11*x3^8 + 15625/3359232*x2^12*x3^6 + 625/209952*x2^13*x3^4 + 875/
      559872*x2^14*x3^2 + 25/46656*x2^15 - 20751953125/2821109907456*x1*x3^23
       - 4150390625/470184984576*x1*x2*x3^21 - 830078125/78364164096*x1*x2^2*
      x3^19 - 166015625/13060694016*x1*x2^3*x3^17 - 33203125/2176782336*x1*
      x2^4*x3^15 - 11328125/725594112*x1*x2^5*x3^13 - 78125/5038848*x1*x2^6*
      x3^11 - 296875/20155392*x1*x2^7*x3^9 - 21875/1679616*x1*x2^8*x3^7 - 625/
      62208*x1*x2^9*x3^5 - 625/93312*x1*x2^10*x3^3 - 25/7776*x1*x2^11*x3 +
      9765625/725594112*x1^2*x3^16 + 1953125/120932352*x1^2*x2*x3^14 + 390625/
      20155392*x1^2*x2^2*x3^12 + 78125/3359232*x1^2*x2^3*x3^10 + 15625/559872*
      x1^2*x2^4*x3^8 + 625/23328*x1^2*x2^5*x3^6 + 125/5184*x1^2*x2^6*x3^4 + 25/
      1296*x1^2*x2^7*x3^2 + 5/432*x1^2*x2^8 - 8*x1^3*x3^3 - 3125/93312*x1^3*
      x3^9 - 625/15552*x1^3*x2*x3^7 - 125/2592*x1^3*x2^2*x3^5 - 25/432*x1^3*
      x2^3*x3^3 - 5/72*x1^3*x2^4*x3 + 5/12*x1^4*x3^2 + 1/2*x1^4*x2
")
assert result("F4")  =~ expr("
      25/72*x1*x2^5*x3^5 - 12969970703125/2742118830047232*x1*x2^5*x3^30 + 5/
      12*x1*x2^6*x3^3 - 2593994140625/457019805007872*x1*x2^6*x3^28 + 1/2*x1*
      x2^7*x3 - 518798828125/76169967501312*x1*x2^7*x3^26 - 103759765625/
      12694994583552*x1*x2^8*x3^24 - 20751953125/2115832430592*x1*x2^9*x3^22
       - 29052734375/2821109907456*x1*x2^10*x3^20 - 830078125/78364164096*x1*
      x2^11*x3^18 - 830078125/78364164096*x1*x2^12*x3^16 - 33203125/3265173504
      *x1*x2^13*x3^14 - 6640625/725594112*x1*x2^14*x3^12 - 5703125/725594112*
      x1*x2^15*x3^10 - 765625/120932352*x1*x2^16*x3^8 - 15625/3359232*x1*x2^17
      *x3^6 - 625/209952*x1*x2^18*x3^4 - 875/559872*x1*x2^19*x3^2 - 25/46656*
      x1*x2^20 - 125/72*x1^2*x3^8 + 64849853515625/2742118830047232*x1^2*x3^33
       - 103759765625/2821109907456*x1^3*x3^26 - 5/432*x1^3*x2^13 + 48828125/
      725594112*x1^4*x3^19 - 15625/93312*x1^5*x3^12 - 1/2*x1^5*x2^6 + 25/12*
      x1^6*x3^5
")
*--#] divmod_4 :
*--#[ partitions_ :
* Test partitions function
#-
V p1,p2,p3,p4,p5,p6;
CF f1,f2,f3;

L F1 = partitions_(3,f1,2,f1,2,f1,2,p1,p2,p3,p4,p5,p6) - dd_(p1,p2,p3,p4,p5,p6);
L F2 = partitions_(0,f1,2,p1,p2,p3,p4,p5,p6) - dd_(p1,p2,p3,p4,p5,p6);
L F3 = partitions_(4,f1,2,f1,2,f2,1,f3,1,p1,p1,p1,p1,p1,p1) - 90*f1(p1,p1)^2*f2(p1)*f3(p1);
L F4 = partitions_(2,f1,2,f2,0,p1,p2,p3,p4,p5,p6) - distrib_(1,2,f1,f2,p1,p2,p3,p4,p5,p6);
id p1?.p2? = f1(p1,p2); * for dd_

P;
.end
assert succeeded?
assert result("F1")  =~ expr("0")
assert result("F2")  =~ expr("0")
assert result("F3")  =~ expr("0")
assert result("F4")  =~ expr("0")
*--#] partitions_ :
*--#[ AppendPath :
#include foo/foo1.h
* foo/bar/p1.prc
#call p1
P;
.end
#:path foo:bar
#include foo1.h
* foo/bar/p2.prc
#call p2
P;
.end
#:path foo:bar
#include foo2.h
* bar/p1.prc
#call p1
P;
.end
#prepare write "foo/foo1.h", "#prependpath bar\n"
#prepare write "foo/foo2.h", "#appendpath bar\n"
#prepare write "foo/bar/p1.prc", "#procedure p1()\nL F=1234;\n#endprocedure\n"
#prepare write "foo/bar/p2.prc", "#procedure p2()\nL G=5678;\n#endprocedure\n"
#prepare write "bar/p1.prc", "#procedure p1()\nL H=9012;\n#endprocedure\n"
assert succeeded?
assert result("F") =~ expr("1234")
assert result("G") =~ expr("5678")
assert result("H") =~ expr("9012")
*--#] AppendPath :
*--#[ dedup :
* Test deduplication
#-
Auto S n;
Auto V p;
CF f1,f2,f3,f,g;
T t1,t2,t3;

L F1 =
#do i = 1,20
  +ranperm_(f,<p1,p1>,...,<p50,p50>)
#enddo
;

L F2 = f1(1,2,3,p,1,1,2,2,p);
L F3 = f2(1,2,3,p,1,1,2,2,p);
L F4 = f3(1,2,3,p,1,1,2,2,p);
L F5 = t1(1,2,3,p,1,1,2,2,p);
L F6 = t2(1,2,3,p,1,1,2,2,p);
L F7 = t3(1,2,3,p,1,1,2,2,p);
L F8 = f1(1,2,1,100000000,n^4,100,n^4,n^5,-10000,p1.p2,p6,p1.p2);

id f(?a) = f(?a)*g(?a);
transform f,dedup(1,last);
repeat id g(?a,p?,?b,p?,?c) = g(?a,p,?b,?c);
id f(?a)*g(?a) = 0;

* Test functions
transform f1,dedup(1,last);
transform f2,dedup(3,last);
transform f3,dedup(1,5);

* Test tensors
transform t1,dedup(1,last);
transform t2,dedup(3,last);
transform t3,dedup(1,5);

P;
.end
assert succeeded?
assert result("F1")  =~ expr("0")
assert result("F2")  =~ expr("f1(1,2,3,p)")
assert result("F3")  =~ expr("f2(1,2,3,p,1,2)")
assert result("F4")  =~ expr("f3(1,2,3,p,1,2,2,p)")
assert result("F5")  =~ expr("t1(1,2,3,p)")
assert result("F6")  =~ expr("t2(1,2,3,p,1,2)")
assert result("F7")  =~ expr("t3(1,2,3,p,1,2,2,p)")
assert result("F8")  =~ expr("f1(1,2,100000000,n^4,100,n^5,-10000,p1.p2,p6)")
*--#] dedup :
*--#[ CoToTensor :
V p1,p2,q1,q2,nosquare;
Set pp:p1,p2;
CF f;
T Q1,functions;
#$q1 = q1;
#$Q1 = Q1;
L F0 = f(q1,q2) * p1.q1 * p2.q1 * q1.q1 * q1.q2;
#do i={1,...,7,11,...,17,51,61,71,72}
  L F`i' = F0;
#enddo
inexpression F1;
  totensor q1,Q1;
endinexpression;
inexpression F2;
  totensor nosquare,q1,Q1;
endinexpression;
inexpression F3;
  totensor functions,q1,Q1;
endinexpression;
inexpression F4;
  totensor nosquare,functions,q1,Q1;
endinexpression;
inexpression F5;
  totensor !pp,q1,Q1;
endinexpression;
inexpression F6;
  totensor !{p1},q1,Q1;
endinexpression;
inexpression F7;
  totensor nosquare,functions,!pp,q1,Q1;
endinexpression;

inexpression F11;
  totensor $q1,Q1;
endinexpression;
inexpression F12;
  totensor q1,$Q1;
endinexpression;
inexpression F13;
  totensor $q1,$Q1;
endinexpression;
inexpression F14;
  totensor Q1,q1;
endinexpression;
inexpression F15;
  totensor $Q1,q1;
endinexpression;
inexpression F16;
  totensor Q1,$q1;
endinexpression;
inexpression F17;
  totensor $Q1,$q1;
endinexpression;

inexpression F51;
  totensor !{p1,p2},q1,Q1;
endinexpression;

inexpression F61;
  totensor !p1,q1,Q1;
endinexpression;

inexpression F71;
  multiply replace_(q1,nosquare);
  totensor nosquare,functions;
endinexpression;

inexpression F72;
  multiply replace_(q1,nosquare);
  totensor nosquare,functions,nosquare,functions;
endinexpression;

P;
.end
assert succeeded?

assert result("F0") =~ expr("f(q1,q2)*p1.q1*p2.q1*q1.q1*q1.q2")
assert result("F1") =~ expr("f(q1,q2)*Q1(p1,p2,q2,N1_?,N1_?)")
assert result("F2") =~ expr("f(q1,q2)*Q1(p1,p2,q2)*q1.q1")
assert result("F3") =~ expr("f(N1_?,q2)*Q1(p1,p2,q2,N1_?,N2_?,N2_?)")
assert result("F4") =~ expr("f(N1_?,q2)*Q1(p1,p2,q2,N1_?)*q1.q1")
assert result("F5") =~ expr("f(q1,q2)*Q1(q2,N1_?,N1_?)*p1.q1*p2.q1")
assert result("F6") =~ expr("f(q1,q2)*Q1(p2,q2,N1_?,N1_?)*p1.q1")
assert result("F7") =~ expr("f(N1_?,q2)*Q1(q2,N1_?)*p1.q1*p2.q1*q1.q1")

assert result("F1") == result("F11")
assert result("F1") == result("F12")
assert result("F1") == result("F13")
assert result("F1") == result("F14")
assert result("F1") == result("F15")
assert result("F1") == result("F16")
assert result("F1") == result("F17")

assert result("F5") == result("F51")

assert result("F6") == result("F61")

assert result("F71") =~ expr("f(nosquare,q2)*functions(p1,p2,q2,N1_?,N1_?)")
assert result("F72") =~ expr("f(N1_?,q2)*functions(p1,p2,q2,N1_?)*nosquare.nosquare")
*--#] CoToTensor :
*--#[ Issue49 :
* Add mul_ function for polynomial multiplications
Symbols x,y,z;
#$p = (1+x+y+z)^4;
#$q = $p+1;
#$r = mul_($p,$q);
L r1 = $r;
L r2 = $p^2 + $p;
.sort
Drop;
L Zero = r1 - r2;
P;
.end
assert succeeded?
assert result("Zero") =~ expr("0")
*--#] Issue49 : 
*--#[ Issue72 :
* "Setups: PATHVALUE not yet implemented"
#:incdir foo
#:path
* foo/p1.prc
#call p1()
P;
.end
#:incdir
#:path foo/bar
* foo/bar/p1.prc
#call p1()
P;
.end
#prepare write "foo/p1.prc", "#procedure p1()\nL F=12345;\n#endprocedure\n"
#prepare write "foo/bar/p1.prc", "#procedure p1()\nL G=123456;\n#endprocedure\n"
assert succeeded?
assert result("F") =~ expr("12345")
assert result("G") =~ expr("123456")
*--#] Issue72 :
*--#[ Issue84 :
* Set to match with a vector
V p,p1,...,p6;
CF f,g,h;
L F = f(p1,-p1,p2,-p2);
id,all,f(?a,-p?vector_,?b) = f(?a,p,?b)*g(p);
Print +s;
.end
assert succeeded?
assert result("F") =~ expr("
       + f(p1,p1,p2,-p2)*g(p1)
       + f(p1,-p1,p2,p2)*g(p2)
")
*--#] Issue84 :
*--#[ Issue86_1 :
* Feature request: take/drop n-th argument of list
* [with zero-dimensional tables]
CF f;
S x,n,n1,n2;

* Get [1,1]. nargs >= 1.
Table first(f?(x?,?a));
Fill first = f(x);

* Get [last,last]. nargs >= 1.
Table last(f?(?a,x?));
Fill last = f(x);

* Get [2,last]. nargs >= 1.
Table rest(f?(x?,?a));
Fill rest = f(?a);

* Get [1,last-1]. nargs >= 1.
Table most(f?(?a,x?));
Fill most = f(?a);

* Join two functions.
Table join(f?(?a),f?(?b));
Fill join = f(?a,?b);

* Rotate left by n. nargs >= 1.
Table roll(n?int_,f?(?a));
Fill roll =
  + delta_(n)   * f(?a)
  + thetap_(n)  * roll(n-1,join(rest(f(?a)),first(f(?a))))
  + thetap_(-n) * roll(n+1,join(last(f(?a)),most(f(?a))))
;

* Get [1,n]. 1 <= n <= nargs.
Table firstn(n?pos_,f?(?a));
Table firstnimpl(n?pos0_,f?(?a),f?(x?,?b));
Fill firstn = firstnimpl(n,f,f(?a,dum_));
Fill firstnimpl =
  + delta_(n)  * f(?a)
  + thetap_(n) * firstnimpl(n-1,f(?a,x),f(?b))
;

* Get the n-th argument. 1 <= n <= nargs.
Table take(n?pos_,f?(?a));
Fill take = first(roll(n-1,f(?a)));

* Drop the n-th argument. 1 <= n <= nargs.
Table drop(n?pos_,f?(?a));
Fill drop = roll(1-n,most(roll(n,f(?a))));

* Get [n1,n2]. Negative indices count from the end. 1 <= n1 <= n2 <= nargs.
Table slice(n1?!{0,},n2?!{0,},f?(?a));
Fill slice =
  + thetap_(n1) * thetap_(n2) * firstn(n2-n1+1,roll(n1-1,f(?a)))
  + thetap_(n1) * thetap_(-n2) * slice(n1,nargs_(?a)+n2+1,f(?a))
  + thetap_(-n1) * thetap_(n2) * slice(nargs_(?a)+n1+1,n2,f(?a))
  + thetap_(-n1) * thetap_(-n2) * slice(nargs_(?a)+n1+1,nargs_(?a)+n2+1,f(?a))
;

L F0  = f(1,...,9);
L F1  = first(F0);
L F2  = last(F0);
L F3  = rest(F0);
L F4  = most(F0);
L F5  = roll(0,F0);
L F6  = roll(2,F0);
L F7  = roll(-2,F0);
L F8  = firstn(3,F0);
L F9  = take(3,F0);
L F10 = drop(3,F0);
L F11 = slice(3,3,F0);
L F12 = slice(3,6,F0);
L F13 = slice(3,-4,F0);
L F14 = slice(-7,6,F0);
L F15 = slice(-7,-4,F0);

P;
.end
assert succeeded?
assert result("F0")  =~ expr("f(1,2,3,4,5,6,7,8,9)")
assert result("F1")  =~ expr("f(1)")
assert result("F2")  =~ expr("f(9)")
assert result("F3")  =~ expr("f(2,3,4,5,6,7,8,9)")
assert result("F4")  =~ expr("f(1,2,3,4,5,6,7,8)")
assert result("F5")  =~ expr("f(1,2,3,4,5,6,7,8,9)")
assert result("F6")  =~ expr("f(3,4,5,6,7,8,9,1,2)")
assert result("F7")  =~ expr("f(8,9,1,2,3,4,5,6,7)")
assert result("F8")  =~ expr("f(1,2,3)")
assert result("F9")  =~ expr("f(3)")
assert result("F10") =~ expr("f(1,2,4,5,6,7,8,9)")
assert result("F11") =~ expr("f(3)")
assert result("F12") =~ expr("f(3,4,5,6)")
assert result("F13") =~ expr("f(3,4,5,6)")
assert result("F14") =~ expr("f(3,4,5,6)")
assert result("F15") =~ expr("f(3,4,5,6)")
*--#] Issue86_1 :
*--#[ Issue86_2 :
* [with the Translate statement]
CF f;
L F0  = f(1,2,3,4,5,6,7,8,9);

#do i=1,4
  #do j=1,8
    L F`i'`j' = F0;
  #enddo
#enddo

$n1 = 3;
$n2 = 5;
$n3 = 4;

#procedure Test(F,trans)
  inexpression `F'1;
    transform,f,`trans'(3,5);
  endinexpression;
  inexpression `F'2;
    transform,f,`trans'(3,$n2);
  endinexpression;
  inexpression `F'3;
    transform,f,`trans'(3,last-4);
  endinexpression;
  inexpression `F'4;
    transform,f,`trans'(3,last-$n3);
  endinexpression;
  inexpression `F'5;
    transform,f,`trans'($n1,5);
  endinexpression;
  inexpression `F'6;
    transform,f,`trans'($n1,$n2);
  endinexpression;
  inexpression `F'7;
    transform,f,`trans'($n1,last-4);
  endinexpression;
  inexpression `F'8;
    transform,f,`trans'($n1,last-$n3);
  endinexpression;
#endprocedure

#call Test(F1,dropargs)
#call Test(F2,selectargs)
#call Test(F3,addargs)
#call Test(F4,mulargs)
P;
ModuleOption local, $n1,$n2,$n3;
.end
assert succeeded?
assert result("F0")  =~ expr("f(1,2,3,4,5,6,7,8,9)")
assert result("F11") =~ expr("f(1,2,6,7,8,9)")
assert result("F21") =~ expr("f(3,4,5)")
assert result("F31") =~ expr("f(1,2,12,6,7,8,9)")
assert result("F41") =~ expr("f(1,2,60,6,7,8,9)")

assert result("F12") == result("F11")
assert result("F13") == result("F11")
assert result("F14") == result("F11")
assert result("F15") == result("F11")
assert result("F16") == result("F11")
assert result("F17") == result("F11")
assert result("F18") == result("F11")

assert result("F22") == result("F21")
assert result("F23") == result("F21")
assert result("F24") == result("F21")
assert result("F25") == result("F21")
assert result("F26") == result("F21")
assert result("F27") == result("F21")
assert result("F28") == result("F21")

assert result("F32") == result("F31")
assert result("F33") == result("F31")
assert result("F34") == result("F31")
assert result("F35") == result("F31")
assert result("F36") == result("F31")
assert result("F37") == result("F31")
assert result("F38") == result("F31")

assert result("F42") == result("F41")
assert result("F43") == result("F41")
assert result("F44") == result("F41")
assert result("F45") == result("F41")
assert result("F46") == result("F41")
assert result("F47") == result("F41")
assert result("F48") == result("F41")
*--#] Issue86_2 :
*--#[ Issue87 :
* Feature request: (anti)bracketing w.r.t. a set
s a, b, c, d;
set ab: a, b;
L test = (a + b)*(c + d);
b ab;
print +s;
.end
assert succeeded?
assert result("test") =~ expr("
       + b * (
          + d
          + c
          )
       + a * (
          + d
          + c
          )
")
*--#] Issue87 :
*--#[ Issue135_1 :
* "Assign instructions cannot occur inside statements" without inside statements
L F =
  #do i=1,10
    #$x = `i';
    + `$x'
  #enddo
;
P;
.end
assert succeeded?
assert result("F") =~ expr("55")
*--#] Issue135_1 :
*--#[ Issue135_2 :
S a1,...,a10;
L F =
  #do i = 1,10
    #$x = `i'*a`i'
          +2;
    +`$x'
  #enddo
;
P;
.end
assert succeeded?
assert result("F") =~ expr("
      20 + 10*a10 + 9*a9 + 8*a8 + 7*a7 + 6*a6 + 5*a5 + 4*a4 + 3*a3 + 2*a2 + a1
")
*--#] Issue135_2 :
*--#[ Issue135_3 :
S a1,...,a10,x;
CF f;
CTable sparse,tab(1);

#do i=1,10
  Fill tab(`i') = f(`i'*a`i') + 2;
#enddo

L F =
  #do i = 1,10
    #$tmp = tab(`i');
    #inside $tmp
      id f(x?) = x;
    #endinside
    + (`$tmp')
  #enddo
;
P;
.end
assert succeeded?
assert result("F") =~ expr("
      20 + 10*a10 + 9*a9 + 8*a8 + 7*a7 + 6*a6 + 5*a5 + 4*a4 + 3*a3 + 2*a2 + a1
")
*--#] Issue135_3 :
*--#[ Issue137_1 :
* New command: ArgToExtraSymbol (,ToNumber)
S a,b;
CF f;
L F = f(1) + f(a) + f(b) + f(a+b);
ArgToExtraSymbol f;
P;
.end
assert succeeded?
assert result("F") =~ expr("f(Z4_) + f(Z3_) + f(Z2_) + f(Z1_)")
*--#] Issue137_1 :
*--#[ Issue137_2 :
S a,b;
CF f;
L F = f(1) + f(a) + f(b) + f(a+b);
ArgToExtraSymbol,ToNumber,f;
P;
.end
assert succeeded?
assert result("F") =~ expr("f(1) + f(2) + f(3) + f(4)")
*--#] Issue137_2 :
*--#[ Issue137_3 :
CF f;
S s;
I i;
V v;
* Fast notation.
L F = f(0) + f(1) + f(-1) + f(s) + f(i) + f(v) + f(-v) + f(f);
argtoextrasymbol;
P;
.end
assert succeeded?
assert result("F") =~ expr("
         f(Z8_) + f(Z7_) + f(Z6_) + f(Z5_) + f(Z4_) + f(Z3_) + f(Z2_) + f(Z1_)
")
*--#] Issue137_3 :
*--#[ Issue137_4 :
#:threadbucketsize 10
#:processbucketsize 10
CF f;
Auto S x;

* NOTE: Large N gives another problem with ParFORM (#141).
#define N "500"
L F0 =
  #do i=1,`N'
    + f(1+x`i') * f(1+x{`i'+100}) * f(1+x{`i'+200})
  #enddo
;
.sort
Hide;

L F1 = F0;
.sort

* If all workers fail to share an unique mapping in a consistent way,
* the following code gives a non-zero result or a crash.
argtoextrasymbol;
.sort
argument;
  frompolynomial;
endargument;
.sort

Drop;

L ZERO = F1 - F0;
P;
.end
assert succeeded?
assert result("ZERO") =~ expr("0")
*--#] Issue137_4 :
*--#[ Issue175_1 :
* Loop over currently active expressions #175
L FF = 1;
L [FF|a,b] = 1;
L [FF,[GG]] = 1;
#do e={`activeexprnames_'}
  L `e' = `e' + 1;
#enddo
L N = `numactiveexprs_';
P;
.end
assert succeeded?
assert result("FF") =~ expr("2")
assert result("[FF|a,b]") =~ expr("2")
assert result("[FF,[GG]]") =~ expr("2")
assert result("N") =~ expr("3")
*--#] Issue175_1 :
*--#[ Issue175_2 :
L F1 = 1;
L F2 = 1;
L F3 = 1;

L F1 = 1;  * redefine in the same module!

*.sort  ;* workaround

#message `numactiveexprs_'
#message `activeexprnames_'

#do e={`activeexprnames_'}
  L `e' = `e' + 1;
#enddo

P;
.end
assert succeeded?
assert result("F1") =~ expr("2")
assert result("F2") =~ expr("2")
assert result("F3") =~ expr("2")
*--#] Issue175_2 :
*--#[ Issue175_3 :
L F1 = 1;
L F2 = 1;
L F3 = 1;

.sort

L F1 = 1;  * replace an existing expression!

*.sort  ;* workaround

#message `numactiveexprs_'
#message `activeexprnames_'

#do e={`activeexprnames_'}
  L `e' = `e' + 1;
#enddo

P;
.end
assert succeeded?
assert result("F1") =~ expr("2")
assert result("F2") =~ expr("2")
assert result("F3") =~ expr("2")
*--#] Issue175_3 :
*--#[ Issue175_4 :
CF F1,F2,F3;

L [F1(1,1,1,1)] = F1(1,1,1,1);
L [F2(-1,1,1,1)] = F2(-1,1,1,1);
.sort

* Redefine.
Local [F1(1,1,1,1)] = F1(1,1,1,1);
.sort

#message `numactiveexprs_'
#message `activeexprnames_'

#do e={`activeexprnames_'}
  L `e' = `e' + 1;
#enddo

P;
.end
assert succeeded?
assert result("[F1(1,1,1,1)]") =~ expr("1 + F1(1,1,1,1)")
assert result("[F2(-1,1,1,1)]") =~ expr("1 + F2(-1,1,1,1)")
*--#] Issue175_4 :
*--#[ Issue187 :
* What is the fastest equivalent of Foreach in FORM?

* distrib_ generates combinations in lexicographical order (in the given
* arguments.)

S x1,...,x5;
CF f;
L F = f(x2,x5,x3,x1,x4);
#$counter = 0;
id f(?a$a) = 1;
term;
  multiply distrib_(1,3,f,dummy_,$a);
  $counter = $counter + 1;
  id f(?a) = f($counter,?a);
endterm;
P +s;
ModuleOption noparallel;
.end
assert succeeded?
assert result("F") =~ expr("
       + f(1,x2,x5,x3)
       + f(2,x2,x5,x1)
       + f(3,x2,x5,x4)
       + f(4,x2,x3,x1)
       + f(5,x2,x3,x4)
       + f(6,x2,x1,x4)
       + f(7,x5,x3,x1)
       + f(8,x5,x3,x4)
       + f(9,x5,x1,x4)
       + f(10,x3,x1,x4)
")
*--#] Issue187 : 
