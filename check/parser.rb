#[ NegArgument :
=begin
	Parser accepted negative numbers as arguments to Dimension, Tracen, ...
 	Fixed 2009-09-08
=end
class NegArgument < FormTest
def setup
	input <<-EOF
Dimension -1;
I i;
L f = d_(i,i);
print;
.end
	EOF
end
def test1
	execute FORM
	assert compile_error
end
end
#] NegArgument :
