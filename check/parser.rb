require 'form'

#[ NegArgument :
class NegArgument < FormTest
#	Parser accepted negative numbers as arguments to Dimension, Tracen, ...
# 	Fixed 2009-09-08
def setup
	@input = <<-EOF
Dimension -1;
I i;
L f = d_(i,i);
print;
.end
	EOF
	super
end
def test
	execute FORM
	assert @compile_error
end
end
#] NegArgument :
