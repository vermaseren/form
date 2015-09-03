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
*--#[ Issue37_1 :
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
V a;
CF rat;
PolyRatFun rat;
L F = rat(a.a,1);
P;
.end
assert runtime_error?
*--#] Issue39 :
*--#[ Issue42_1 :
* Performance issue of FactDollar.
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
* FIXME: ParFORM hangs.
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

* FIXME: Factorize still have the performance issue.
#if 0
L F4 = F;
Factorize F4;
.sort
#endif

P;
.end
#require not mpi?
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
