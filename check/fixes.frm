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
