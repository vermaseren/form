#[ SparseTable1 :
=begin
	Bugs reported 2004-04-06 by Misha Tentukov
	PrintTable and FillExpression did not work with non-sparse tables
	Fixed 2005-09-27
=end
class SparseTable1 < FormTest
def setup
	input <<-EOF
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
	EOF
	extra_parameter "-D TableSize=10"
end
def test1
	execute FORM
	assert no_problem
	assert result("expr1") =~ pattern("f(1)*x + f(2)*x^2 + f(3)*x^3 + f(4)*x^4 + f(5)*x^5 + f(6)*x^6 + f(7)*x^7 + f(8)*x^8 + f(9)*x^9 + f(10)*x^10")
	assert result("e10") =~ pattern("f(10)")
end
end
#] SparseTable1 :
