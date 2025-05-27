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
*--#[ moebius_1 :
S i,x;
L F = sum_(i,1,200,moebius_(i)*x^i);
P;
.end:result;
assert succeeded?
# Sum[MoebiusMu[i] x^i, {i, 1, 200}] // InputForm
assert result("F") =~ expr("
 x - x^2 - x^3 - x^5 + x^6 - x^7 + x^10 - x^11 - x^13 + x^14 + x^15 - x^17 -
 x^19 + x^21 + x^22 - x^23 + x^26 - x^29 - x^30 - x^31 + x^33 + x^34 + x^35 -
 x^37 + x^38 + x^39 - x^41 - x^42 - x^43 + x^46 - x^47 + x^51 - x^53 + x^55 +
 x^57 + x^58 - x^59 - x^61 + x^62 + x^65 - x^66 - x^67 + x^69 - x^70 - x^71 -
 x^73 + x^74 + x^77 - x^78 - x^79 + x^82 - x^83 + x^85 + x^86 + x^87 - x^89 +
 x^91 + x^93 + x^94 + x^95 - x^97 - x^101 - x^102 - x^103 - x^105 + x^106 -
 x^107 - x^109 - x^110 + x^111 - x^113 - x^114 + x^115 + x^118 + x^119 +
 x^122 + x^123 - x^127 + x^129 - x^130 - x^131 + x^133 + x^134 - x^137 -
 x^138 - x^139 + x^141 + x^142 + x^143 + x^145 + x^146 - x^149 - x^151 -
 x^154 + x^155 - x^157 + x^158 + x^159 + x^161 - x^163 - x^165 + x^166 -
 x^167 - x^170 - x^173 - x^174 + x^177 + x^178 - x^179 - x^181 - x^182 +
 x^183 + x^185 - x^186 + x^187 - x^190 - x^191 - x^193 + x^194 - x^195 -
 x^197 - x^199
")
*--#] moebius_1 :
*--#[ moebius_2 :
S x,x1,x2;
CF f1,f2;
L F = 1;
multiply <f1(1)*x1^1>+...+<f1(10)*x1^10>;
multiply <f2(1)*x2^1>+...+<f2(10)*x2^10>;
id x1 = x;
id x2 = x^20;
.sort
S n;
id f1(n?) = moebius_(10000+n);
.sort
id f2(n?) = moebius_(20000+n);
P;
.end:result;
assert succeeded?
# Sum[MoebiusMu[10000 + i] MoebiusMu[20000 + j] x^(i + 20 * j), {i, 1, 10}, {j, 1, 10}] // InputForm
assert result("F") =~ expr("
-x^21 + x^22 - x^23 - x^25 - x^26 + x^27 + x^29 + x^30 - x^41 + x^42 - x^43 -
 x^45 - x^46 + x^47 + x^49 + x^50 + x^61 - x^62 + x^63 + x^65 + x^66 - x^67 -
 x^69 - x^70 + x^101 - x^102 + x^103 + x^105 + x^106 - x^107 - x^109 - x^110 -
 x^121 + x^122 - x^123 - x^125 - x^126 + x^127 + x^129 + x^130 - x^181 +
 x^182 - x^183 - x^185 - x^186 + x^187 + x^189 + x^190 - x^201 + x^202 -
 x^203 - x^205 - x^206 + x^207 + x^209 + x^210
")
*--#] moebius_2 :
*--#[ moebius_3 :
* corner cases, see #430
#do i=1,9
  L F`i' = moebius_(2^15-`i');
#enddo
#do i=1,9
  L G`i' = moebius_(2^31-`i');
#enddo
P;
.end
#time_dilation 4.0
# memory usage is so intense
#pend_if serial? && total_memory < 8_000_000_000
#pend_if threaded? && total_memory < 20_000_000_000
#pend_if mpi? && total_memory < 20_000_000_000
# too heavy on GitHub (often fails)
#pend_if github? && valgrind?
assert succeeded?
if wordsize >= 2
  assert result("F1") =~ expr("-1")
  assert result("F2") =~ expr("1")
  assert result("F3") =~ expr("1")
  assert result("F4") =~ expr("0")
  assert result("F5") =~ expr("-1")
  assert result("F6") =~ expr("1")
  assert result("F7") =~ expr("0")
  assert result("F8") =~ expr("0")
  assert result("F9") =~ expr("-1")
end
if wordsize >= 4
  assert result("G1") =~ expr("-1")
  assert result("G2") =~ expr("0")
  assert result("G3") =~ expr("-1")
  assert result("G4") =~ expr("0")
  assert result("G5") =~ expr("1")
  assert result("G6") =~ expr("-1")
  assert result("G7") =~ expr("1")
  assert result("G8") =~ expr("0")
  assert result("G9") =~ expr("-1")
end
*--#] moebius_3 :
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
*--#[ AppendPath_unix :
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
#require unix?
#prepare write "foo/foo1.h", "#prependpath bar\n"
#prepare write "foo/foo2.h", "#appendpath bar\n"
#prepare write "foo/bar/p1.prc", "#procedure p1()\nL F=1234;\n#endprocedure\n"
#prepare write "foo/bar/p2.prc", "#procedure p2()\nL G=5678;\n#endprocedure\n"
#prepare write "bar/p1.prc", "#procedure p1()\nL H=9012;\n#endprocedure\n"
assert succeeded?
assert result("F") =~ expr("1234")
assert result("G") =~ expr("5678")
assert result("H") =~ expr("9012")
*--#] AppendPath_unix :
*--#[ AppendPath_windows :
#include foo\foo1.h
* foo/bar/p1.prc
#call p1
P;
.end
#:path foo;bar
#include foo1.h
* foo/bar/p2.prc
#call p2
P;
.end
#:path foo;bar
#include foo2.h
* bar/p1.prc
#call p1
P;
.end
#require windows?
#prepare write "foo/foo1.h", "#prependpath bar\n"
#prepare write "foo/foo2.h", "#appendpath bar\n"
#prepare write "foo/bar/p1.prc", "#procedure p1()\nL F=1234;\n#endprocedure\n"
#prepare write "foo/bar/p2.prc", "#procedure p2()\nL G=5678;\n#endprocedure\n"
#prepare write "bar/p1.prc", "#procedure p1()\nL H=9012;\n#endprocedure\n"
assert succeeded?
assert result("F") =~ expr("1234")
assert result("G") =~ expr("5678")
assert result("H") =~ expr("9012")
*--#] AppendPath_windows :
*--#[ TimeoutAfter_1 :
#procedure problematicprocedure
* Do nothing.
#endprocedure

#timeoutafter 1000
#call problematicprocedure
#timeoutafter 0
.end
#require unix?
assert succeeded?
*--#] TimeoutAfter_1 :
*--#[ TimeoutAfter_2 :
#procedure problematicprocedure
* Infinite loop.
  #do i=1,1
    #redefine i "0"
  #enddo
#endprocedure

#timeoutafter 1
#call problematicprocedure
#timeoutafter 0
.end
#require unix?
# ParFORM may terminate without printing the error message,
# depending on the MPI environment.
#pend_if mpi?
assert runtime_error?
*--#] TimeoutAfter_2 :
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
*--#[ Format_allfloat :
* See also Issue #216.
#-
Off stats;
S x;
L F = x - x^2 + 2*x^3 + 1/2*x^4 - 2/3*x^5;
.sort

#message (0) normal
#write " F = %E;", F
.sort

#message (1) Fortran
Format Fortran;
#write " F = %E;", F
.sort

* TODO: this combination doesn't work correctly, though no one may use
* single-precision Fortran seriously.
*
* #message Fortran,allfloat
* Format Fortran;
* Format allfloat;
* #write " F = %E;", F
* .sort

#message (2) DoubleFortran
Format DoubleFortran;
#write " F = %E;", F
.sort

#message (3) DoubleFortran,allfloat
Format DoubleFortran;
Format allfloat;
#write " F = %E;", F
.sort

#message (4) QuadrupleFortran
Format QuadrupleFortran;
#write " F = %E;", F
.sort

#message (5) QuadrupleFortran,allfloat
Format QuadrupleFortran;
Format allfloat;
#write " F = %E;", F
.sort

#message (6) Fortran90,.0_wp
Format Fortran90,.0_wp;  * forcibly allfloat
#write " F = %E;", F
.end
assert succeeded?
assert result("F", 0) =~ expr("x - x^2 + 2*x^3 + 1/2*x^4 - 2/3*x^5")
assert result("F", 1) =~ expr("x - x**2 + 2*x**3 + 1./2.*x**4 - 2./3.*x**5")
assert result("F", 2) =~ expr("x - x**2 + 2*x**3 + 1.D0/2.D0*x**4 - 2.D0/3.D0*x**5")
assert result("F", 3) =~ expr("x - x**2 + 2.D0*x**3 + 1.D0/2.D0*x**4 - 2.D0/3.D0*x**5")
assert result("F", 4) =~ expr("x - x**2 + 2*x**3 + 1.Q0/2.Q0*x**4 - 2.Q0/3.Q0*x**5")
assert result("F", 5) =~ expr("x - x**2 + 2.Q0*x**3 + 1.Q0/2.Q0*x**4 - 2.Q0/3.Q0*x**5")
assert result("F", 6) =~ expr("x - x**2 + 2.0_wp*x**3 + 1.0_wp/2.0_wp*x**4 - 2.0_wp/3.0_wp*x**5")
*--#] Format_allfloat :
*--#[ Format_noreset_linelen :
#-
Off stats;
Auto S x;
L F = (x1+...+x5)^3;
.sort

#message (0) normal,80
#write " F = %E;", F
.sort

#message (1) Fortran,72
Format Fortran;
#write " F = %E;", F
.sort

#message (2) C,50
Format 50;
Format C;
#write " F = %E;", F
.sort

#message (3) Fortran,50
Format Fortran;
#write " F = %E;", F
.end
assert succeeded?
assert result("F", 0) =~ expr("
     x5^3 + 3*x4*x5^2 + 3*x4^2*x5 + x4^3 + 3*x3*x5^2 + 6*x3*x4*x5 + 3*x3*x4^2
       + 3*x3^2*x5 + 3*x3^2*x4 + x3^3 + 3*x2*x5^2 + 6*x2*x4*x5 + 3*x2*x4^2 + 6
      *x2*x3*x5 + 6*x2*x3*x4 + 3*x2*x3^2 + 3*x2^2*x5 + 3*x2^2*x4 + 3*x2^2*x3
       + x2^3 + 3*x1*x5^2 + 6*x1*x4*x5 + 3*x1*x4^2 + 6*x1*x3*x5 + 6*x1*x3*x4
       + 3*x1*x3^2 + 6*x1*x2*x5 + 6*x1*x2*x4 + 6*x1*x2*x3 + 3*x1*x2^2 + 3*x1^2
      *x5 + 3*x1^2*x4 + 3*x1^2*x3 + 3*x1^2*x2 + x1^3
")
assert result("F", 1) =~ expr("
     x5**3 + 3*x4*x5**2 + 3*x4**2*x5 + x4**3 + 3*x3*x5**2 + 6*x3*x4*x5
     &  + 3*x3*x4**2 + 3*x3**2*x5 + 3*x3**2*x4 + x3**3 + 3*x2*x5**2 + 6
     & *x2*x4*x5 + 3*x2*x4**2 + 6*x2*x3*x5 + 6*x2*x3*x4 + 3*x2*x3**2 +
     & 3*x2**2*x5 + 3*x2**2*x4 + 3*x2**2*x3 + x2**3 + 3*x1*x5**2 + 6*x1
     & *x4*x5 + 3*x1*x4**2 + 6*x1*x3*x5 + 6*x1*x3*x4 + 3*x1*x3**2 + 6*
     & x1*x2*x5 + 6*x1*x2*x4 + 6*x1*x2*x3 + 3*x1*x2**2 + 3*x1**2*x5 + 3
     & *x1**2*x4 + 3*x1**2*x3 + 3*x1**2*x2 + x1**3
")
assert result("F", 2) =~ expr("
     pow(x5,3) + 3*x4*pow(x5,2) + 3*pow(x4,2)*x5
       + pow(x4,3) + 3*x3*pow(x5,2) + 6*x3*x4*x5
       + 3*x3*pow(x4,2) + 3*pow(x3,2)*x5 + 3*pow(
      x3,2)*x4 + pow(x3,3) + 3*x2*pow(x5,2) + 6*
      x2*x4*x5 + 3*x2*pow(x4,2) + 6*x2*x3*x5 + 6*
      x2*x3*x4 + 3*x2*pow(x3,2) + 3*pow(x2,2)*x5
       + 3*pow(x2,2)*x4 + 3*pow(x2,2)*x3 + pow(
      x2,3) + 3*x1*pow(x5,2) + 6*x1*x4*x5 + 3*x1*
      pow(x4,2) + 6*x1*x3*x5 + 6*x1*x3*x4 + 3*x1*
      pow(x3,2) + 6*x1*x2*x5 + 6*x1*x2*x4 + 6*x1*
      x2*x3 + 3*x1*pow(x2,2) + 3*pow(x1,2)*x5 + 3
      *pow(x1,2)*x4 + 3*pow(x1,2)*x3 + 3*pow(
      x1,2)*x2 + pow(x1,3)
")
assert result("F", 3) =~ expr("
     x5**3 + 3*x4*x5**2 + 3*x4**2*x5 + x4**3 + 3*
     & x3*x5**2 + 6*x3*x4*x5 + 3*x3*x4**2 + 3*
     & x3**2*x5 + 3*x3**2*x4 + x3**3 + 3*x2*x5**2
     &  + 6*x2*x4*x5 + 3*x2*x4**2 + 6*x2*x3*x5 +
     & 6*x2*x3*x4 + 3*x2*x3**2 + 3*x2**2*x5 + 3*
     & x2**2*x4 + 3*x2**2*x3 + x2**3 + 3*x1*x5**2
     &  + 6*x1*x4*x5 + 3*x1*x4**2 + 6*x1*x3*x5 +
     & 6*x1*x3*x4 + 3*x1*x3**2 + 6*x1*x2*x5 + 6*
     & x1*x2*x4 + 6*x1*x2*x3 + 3*x1*x2**2 + 3*
     & x1**2*x5 + 3*x1**2*x4 + 3*x1**2*x3 + 3*
     & x1**2*x2 + x1**3
")
*--#] Format_noreset_linelen :
*--#[ Float_1 :
#-
* Example from the FORM Workshop (Madrid 2023) slides

#StartFloat 500,15

Local F1 =
	-mzv_(8,1,1,5)
	+29056868/39414375*mzv_(2)^6*mzv_(3)
	-47576/40425*mzv_(2)^5*mzv_(5)
	-163291/18375*mzv_(2)^4*mzv_(7)
	-4/105*mzv_(2)^3*mzv_(3)^3
	-450797/11025*mzv_(2)^3*mzv_(9)
	+7/5*mzv_(2)^2*mzv_(3)^2*mzv_(5)
	+16/25*mzv_(2)^2*mzv_(3)*mzv_(5,3)
	+454049/1400*mzv_(2)^2*mzv_(11)
	-16/25*mzv_(2)^2*mzv_(5,3,3)
	+3*mzv_(2)*mzv_(3)^2*mzv_(7)
	+61/14*mzv_(2)*mzv_(3)*mzv_(5)^2
	+2/7*mzv_(2)*mzv_(3)*mzv_(7,3)
	+2172853/420*mzv_(2)*mzv_(13)
	-2/7*mzv_(2)*mzv_(7,3,3)
	+1/7*mzv_(2)*mzv_(5,5,3)
	-33/4*mzv_(3)^2*mzv_(9)
	-133/6*mzv_(3)*mzv_(5)*mzv_(7)
	-25/9*mzv_(3)*mzv_(9,3)
	-244/105*mzv_(5)^3
	-359/105*mzv_(5)*mzv_(7,3)
	+3/10*mzv_(7)*mzv_(5,3)
	+89/18*mzv_(9,3,3)
	+569/105*mzv_(7,3,5);
L F2 = mzv_(15);
Evaluate mzv_;
Print;
.sort

Skip F1,F2;
Local X = F1/F2;
ToRational;
Print;
.sort

#EndFloat
Local G1 = F1;
Local G2 = F2;

Print G1,G2;
.end
#pend_if wordsize == 2
assert succeeded?
assert result("X") =~ expr("229903169/25200")
assert stdout =~ exact_pattern(<<'EOF')
   F1 =
      9.1234206877960755900164875575406726239325002222490534540605137258846994\
      916348297032751308227224952419629422497720599224543719959652966613231560\
      6913925597e+03;
EOF
assert stdout =~ exact_pattern(<<'EOF')
   F2 =
      1.0000305882363070204935517285106450625876279487068581775065699328933322\
      671563422795730723343470175484943669684442492832530297757588781904321794\
      4047700034253e+00;
EOF
assert stdout =~ exact_pattern(<<'EOF')
   G1 =
      float_(9,9,1,12232507224603456570941665221340125041397786269129604087876\
      792424004667562978035312462415295319766403611636425161522971836411112965\
      9196489428266949058466393830);
EOF
assert stdout =~ exact_pattern(<<'EOF')
   G2 =
      float_(9,9,1,13408218051139917327008657439505375023483221929057707900254\
      962082967965638918465272397044054754117520277123188070120899041524740650\
      320272590902507813075652);
EOF
*--#] Float_1 :
*--#[ evaluate_symbol :
#-
#StartFloat 64

Symbol a,b;
Local PI0 = a*b;
Local PI1 = pi_;
Local PI2 = pi_*3;
Local PI3 = pi_*sqrt_(3);
Local PI4 = pi_*pi_*pi_;
Local PI5 = a*pi_*b;
Local PI6 = sqrt_(pi_);

Local EE1 = ee_;
Local EE2 = pi_*ee_;

Local EM1 = em_;

ToFloat;
Evaluate;

Print;
.end
#pend_if wordsize == 2
assert result("PI0") =~ expr("1.0e+00*a*b")
assert result("PI1") =~ expr("3.14159265358979323846e+00")
assert result("PI2") =~ expr("9.42477796076937971538e+00")
assert result("PI3") =~ expr("5.4413980927026535518e+00")
assert result("PI4") =~ expr("3.10062766802998201755e+01")
assert result("PI5") =~ expr("3.14159265358979323846e+00*a*b")
assert result("PI6") =~ expr("1.77245385090551602731e+00")
assert result("EE1") =~ expr("2.71828182845904523537e+00")
assert result("EE2") =~ expr("8.53973422267356706549e+00")
assert result("EM1") =~ expr("5.77215664901532860607e-01")
*--#] evaluate_symbol :
*--#[ evaluate_symbol_pi :
#-
#StartFloat 128

Local PI = pi_;
Local EE = ee_;
Local EM = em_;

ToFloat;
Evaluate pi_;

Print;
.end
#pend_if wordsize == 2
assert result("PI") =~ expr("3.141592653589793238462643383279502884198e+00")
assert result("EE") =~ expr("1.0e+00*ee_")
assert result("EM") =~ expr("1.0e+00*em_")
*--#] evaluate_symbol_pi :
*--#[ evaluate_symbol_ee :
#-
#StartFloat 160

Local PI = pi_;
Local EE = ee_;
Local EM = em_;

ToFloat;
Evaluate ee_;

Print;
.end
#pend_if wordsize == 2
assert result("PI") =~ expr("1.0e+00*pi_")
assert result("EE") =~ expr("2.718281828459045235360287471352662497757247093699959574967e+00")
assert result("EM") =~ expr("1.0e+00*em_")
*--#] evaluate_symbol_ee :
*--#[ evaluate_symbol_em :
#-
#StartFloat 192

Local PI = pi_;
Local EE = ee_;
Local EM = em_;

ToFloat;
Evaluate em_;

Print;
.end
#pend_if wordsize == 2
assert result("PI") =~ expr("1.0e+00*pi_")
assert result("EE") =~ expr("1.0e+00*ee_")
assert result("EM") =~ expr("5.7721566490153286060651209008240243104215933593992359880577e-01")
*--#] evaluate_symbol_em :
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
*--#[ Issue243_1 :
* #continuedo?
NF f;
L F = 1;
#do i=1,5
  #if `i' == 3
    #continuedo
  #endif
  multiply right, f(`i');
#enddo
chainin f;
P;
.end
assert succeeded?
assert result("F") =~ expr("f(1,2,4,5)")
*--#] Issue243_1 : 
*--#[ Issue243_2 :
#continuedo
.end
assert preprocess_error?
assert stdout =~ exact_pattern("#continuedo without #do")
*--#] Issue243_2 : 
*--#[ Issue392_ContinuationLines_1 :
#: ContinuationLines 1
* Setting ContinuationLines to 0 should remove continuation line limit.
auto symbol x;
local ex = (xabcdefg + xhijklmnop)^100;
.sort
format C;
#write <out.c> "%e" ex
.end
assert succeeded?
assert file("out.c") =~ /[_] [+]=  /
*--#] Issue392_ContinuationLines_1 :
*--#[ Issue392_ContinuationLines_0 :
#: ContinuationLines 0
* Setting ContinuationLines to 0 should remove continuation line limit.
auto symbol x;
local ex = (xabcdefg + xhijklmnop)^100;
.sort
format C;
#write <out.c> "%e" ex
.end
assert succeeded?
assert !(file("out.c") =~ /[_] [+]=  /)
*--#] Issue392_ContinuationLines_0 :
*--#[ Sortrealloc_1 :
On sortreallocate;
Symbol x,y;
Local F = (x+y)^10;
.sort
Identify x = - y;
.sort
Print +s;
.end
assert succeeded?
assert result("F") =~ expr("0");
*--#] Sortrealloc_1 :
*--#[ Sortrealloc_2 :
Symbol x,y;
Local F = (x+y)^10;
.sort
#sortreallocate
Identify x = - y;
.sort
Print +s;
.end
assert succeeded?
assert result("F") =~ expr("0");
*--#] Sortrealloc_2 :
*--#[ TempSortDir_unix :
#: TempSortDir bad/path
Local test = 1;
.end
#require unix?
if mpi?
  assert runtime_error?("Could not create sort file: bad/path/0formxxx.sor")
else
  assert runtime_error?("Could not create sort file: bad/path/xformxxx.sor")
end
*--#] TempSortDir_unix :
*--#[ TempSortDir_windows :
#: TempSortDir bad_path
Local test = 1;
.end
#require windows?
if mpi?
  assert runtime_error?('Could not create sort file: bad_path\0formxxx.sor')
else
  assert runtime_error?('Could not create sort file: bad_path\xformxxx.sor')
end
*--#] TempSortDir_windows :
*--#[ ZeroUnchanged :
#-

#procedure exprinfo
	#message Module `CMODULE_':
	#do e = {`activeexprnames_'}
		#if `ZERO_`e''
			#message zero `e': `ZERO_`e''
		#endif
		#if `UNCHANGED_`e''
			#message unchanged `e': `UNCHANGED_`e''
		#endif
	#enddo
	#if `ZERO_'
		#message All zero: `ZERO_'
	#endif
	#if `UNCHANGED_'
		#message All unchanged: `UNCHANGED_'
	#endif
	#message
#endprocedure


Off stats;

Symbol x,y;

Local test1 = x;
Local test2 = y;
Local test3 = 1;
.sort:1;

#call exprinfo
Identify x = 0;
.sort:2;

#call exprinfo
Identify y = 0;
.sort:3;

#call exprinfo
.sort:4;

#call exprinfo
Multiply 0;
.sort:5;

#message Here, test3 is incorrectly flagged as unchanged:
#call exprinfo
Print;
.end
assert succeeded?
assert result("test1") =~ expr("0")
assert result("test2") =~ expr("0")
assert result("test3") =~ expr("0")
assert stdout =~ exact_pattern(<<'EOF')
~~~Module 2:
~~~
~~~Module 3:
~~~zero test1: 1
~~~unchanged test2: 1
~~~unchanged test3: 1
~~~
~~~Module 4:
~~~zero test1: 1
~~~unchanged test1: 1
~~~zero test2: 1
~~~unchanged test3: 1
~~~
~~~Module 5:
~~~zero test1: 1
~~~unchanged test1: 1
~~~zero test2: 1
~~~unchanged test2: 1
~~~unchanged test3: 1
~~~All unchanged: 1
~~~
~~~Here, test3 is incorrectly flagged as unchanged:
~~~Module 6:
~~~zero test1: 1
~~~unchanged test1: 1
~~~zero test2: 1
~~~unchanged test2: 1
~~~zero test3: 1
~~~unchanged test3: 1
~~~All zero: 1
~~~All unchanged: 1
~~~
EOF
*--#] ZeroUnchanged :
*--#[ tablebase_ro_1 :
Table,sparse,no1fill(1);
Fill no1fill(1) = 1;
Fill no1fill(2) = 2;
Fill no1fill(3) = 3;
TableBase "no1.tbl" create;
TableBase "no1.tbl" addto no1fill;
.end
Table,sparse,no2fill(1);
Fill no2fill(1) = 1;
Fill no2fill(2) = 2;
Fill no2fill(3) = 3;
.sort
TableBase "no1.tbl" open,readonly;
TableBase "no1.tbl" addto no2fill;
.end
assert runtime_error?('Tablebase with the name no1.tbl opened in read only mode')
*--#] tablebase_ro_1 :
*--#[ tablebase_ro_2 :
TableBase "no212.tbl" open, readonly;
.end
assert runtime_error?('Trying to open non-existent TableBase in readonly mode: no212.tbl')
*--#] tablebase_ro_2 :
