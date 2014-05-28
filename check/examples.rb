# Tests using the examples in the manual
#
# Some assertions here check for trivial, secondary things like runtime
# information or layout. Usually, this should be avoided. But since we are here
# not only testing FORM but also the code examples in the manual, this extra
# strictness makes sometimes sense.
# In the manual the example code has been given a comment that says it is used
# here. Therefore, if you change something here, consider applying the
# appropriate changes also in the manual.

#[ 2.2 :
class Sec_2_2 < FormTest
def setup
	input <<-EOF
    s     x(:10),y;
    L     F=y^7;
    id    y=x+x^2;
    print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
#	assert @stdout =~ /Bytes used \s+=\s+54$/ # correct on 64bit?!?
	assert result("F") =~ pattern("x^7 + 7*x^8 + 21*x^9 + 35*x^10")
end
end
#] 2.2 :
#[ 2.6 :
class Sec_2_6 < FormTest
def setup
	input <<-EOF
    Symbols a1,a2,a3,b1,b2,b3,x,n;
    CFunctions g1,g2,g3,g;
    Local expr =
        g(a1)+g(a2)+g(a3)+g(x);
    id,g(x?{a1,a2,a3}[n]) = {g1,g2,g3}[n]({b1,b2,b3}[n]);
    print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("expr") =~ pattern("g1(b1) + g2(b2) + g3(b3) + g(x)")
end
end
#] 2.6 :
#[ 2.9 :
class Sec_2_9_1 < FormTest
def setup
	input <<-EOF
   i  mu,nu;
   f  f1,f2;
   L  F=f1(mu)*f2(mu)+f1(nu)*f2(nu);
   sum  mu;
   sum  nu;
   print;
   .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("2*f1(N1_?)*f2(N1_?)")
end
end

class Sec_2_9_2 < FormTest
def setup
	input <<-EOF
    Index mu,nu;
    CFunctions f,g;
    Vectors p,q;
    Local F = (f(mu)*g(mu))^2;
    sum mu;
    id f(nu?) = p(nu);
    id g(nu?) = q(nu);
    print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("p.p*q.q")
end
end

class Sec_2_9_3 < FormTest
def setup
	input <<-EOF
    Index mu,nu;
    Symbol x;
    CFunctions f,g;
    Vectors p,q;
    Local F = x^2;
    repeat;
        id,once,x = f(mu)*g(mu);
        sum mu;
    endrepeat;
    id f(nu?) = p(nu);
    id g(nu?) = q(nu);
    print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("p.q^2")
end
end

class Sec_2_9_4 < FormTest
def setup
	input <<-EOF
     Indices mu,nu;
     CFunctions f;
     L F = f(mu,nu)*f(nu,mu);
     sum  mu, nu;
     Print;
     .sort
     Indices rho,si;
     Vectors p1,p2,p3,v;
     Tensor g;
     Local G = e_(mu,nu,rho,si)*g(mu,nu,p1,v)*g(rho,si,p2,v);
     sum mu,nu,rho,si;
     Multiply F^3;
     id  v = e_(p1,p2,p3,?);
     print;
     .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("G") =~ pattern("f(N1_?,N2_?)*f(N2_?,N1_?)*f(N3_?,N4_?)*f(N4_?,N3_?)*f(N5_?,N6_?)*f(N6_?,N5_?)
		*g(N7_?,N8_?,p1,N9_?)*g(N10_?,N11_?,p2,N12_?)*e_(p1,p2,p3,N9_?)*e_(p1,p2,p3,N12_?)*e_(N7_?,N8_?,N10_?,N11_?)")
end
end
#] 2.9 :
#[ 3.6 :
class Sec_3_6 < FormTest
def setup
	input <<-EOF
    #define a "1"
    #define bc2 "x"
    #define bc3 "y"
    #define b "c`~a'"
    #procedure hop(c,?d);
    #redefine a "3"
    #message This is the call: `c',`?d'
    #endprocedure
    
    #redefine a "2"
    #message This is b: `b'
    #call hop(`b`!b''`!b'`b'`!b'`b',`~a',`b',`a')
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert @stdout =~ /#message This is b: `b'\n~~~This is b: c2\n/
	assert @stdout =~ /#call hop\(`b`!b''`!b'`b'`!b'`b',`~a',`b',`a'\)\n~~~This is the call: xc2c3c2c3,3,c3,2\n/
end
end
#] 3.6 :
#[ 3.12 :
class Sec_3_12 < FormTest
def setup
	input <<-EOF
    #define c "3"
    #define var1(a,b) "(`~a'+`~b'+`c')"
    #define var2(a,b) "(`~a'+`~b'+`~c')"
    #redefine c "4"
    Local F1 = `var1(1,2)';
    Local F2 = `var2(1,2)';
    Print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F1") =~ pattern("6")
	assert result("F2") =~ pattern("7")
end
end
#] 3.12 :
#[ 3.29 :
class Sec_3_29 < FormTest
def setup
	input <<-EOF
    #PreOut ON
    S   a1,...,a4;
    L   F = (a1+...+a4)^2;
    id  a4 = -a1;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert @stdout =~ exact_pattern(<<-EOF
        #PreOut ON
        S   a1,...,a4;
 S,a1,a2,a3,a4
        L   F = (a1+...+a4)^2;
 L,F=(a1+a2+a3+a4)^2
        id  a4 = -a1;
 id,a4=-a1
        .end
		EOF
		)
end
end
#] 3.29 :
#[ 3.44 :
class Sec_3_44_1 < FormTest
def setup
	input <<-EOF
    Symbols a,b;
    L   F = a+b;
    \#$a1 = a+b;
    \#$a2 = (a+b)^2;
    \#$a3 = $a1^3;
    #write " One power: %$\\n Two powers: %$\\n Three powers: %$\\n%s"\\
           ,$a1,$a2,$a3," The end"
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert @stdout =~ exact_pattern(<<-EOF
 One power: b+a
 Two powers: b^2+2*a*b+a^2
 Three powers: b^3+3*a*b^2+3*a^2*b+a^3
 The end
        .end
		EOF
		)
end
end

class Sec_3_44_2 < FormTest
Fortranfile = "fun.f"
def setup
	input <<-EOF
    S   x1,...,x10;
    L   MyExpression = (x1+...+x10)^4;
    .sort
    Format Fortran;
    #write <fun.f> "      FUNCTION fun(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)"
    #write <fun.f> "      REAL x1,x2,x3,x4,x5,x6,x7,x8,x9,x10"
    #write <fun.f> "      fun = %e",MyExpression(fun)
    #write <fun.f> "      RETURN"
    #write <fun.f> "      END"
    .end
	EOF
	File.delete(Fortranfile) if File.exists?(Fortranfile)
end
def teardown
	super
	File.delete(Fortranfile) if File.exists?(Fortranfile)
end
def test1
	execute FORM
	assert no_problem
	assert File.exists?(Fortranfile)
	fun_f = File.open(Fortranfile, "r").readlines.join
	assert fun_f == <<-EOF
      FUNCTION fun(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
      REAL x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
      fun = x10**4 + 4*x9*x10**3 + 6*x9**2*x10**2 + 4*x9**3*x10 + x9**4
     &  + 4*x8*x10**3 + 12*x8*x9*x10**2 + 12*x8*x9**2*x10 + 4*x8*x9**3
     &  + 6*x8**2*x10**2 + 12*x8**2*x9*x10 + 6*x8**2*x9**2 + 4*x8**3*
     & x10 + 4*x8**3*x9 + x8**4 + 4*x7*x10**3 + 12*x7*x9*x10**2 + 12*x7
     & *x9**2*x10 + 4*x7*x9**3 + 12*x7*x8*x10**2 + 24*x7*x8*x9*x10 + 12
     & *x7*x8*x9**2 + 12*x7*x8**2*x10 + 12*x7*x8**2*x9 + 4*x7*x8**3 + 6
     & *x7**2*x10**2 + 12*x7**2*x9*x10 + 6*x7**2*x9**2 + 12*x7**2*x8*
     & x10 + 12*x7**2*x8*x9 + 6*x7**2*x8**2 + 4*x7**3*x10 + 4*x7**3*x9
     &  + 4*x7**3*x8 + x7**4 + 4*x6*x10**3 + 12*x6*x9*x10**2 + 12*x6*
     & x9**2*x10 + 4*x6*x9**3 + 12*x6*x8*x10**2 + 24*x6*x8*x9*x10 + 12*
     & x6*x8*x9**2 + 12*x6*x8**2*x10 + 12*x6*x8**2*x9 + 4*x6*x8**3 + 12
     & *x6*x7*x10**2 + 24*x6*x7*x9*x10 + 12*x6*x7*x9**2 + 24*x6*x7*x8*
     & x10 + 24*x6*x7*x8*x9 + 12*x6*x7*x8**2 + 12*x6*x7**2*x10 + 12*x6*
     & x7**2*x9 + 12*x6*x7**2*x8 + 4*x6*x7**3 + 6*x6**2*x10**2 + 12*
     & x6**2*x9*x10 + 6*x6**2*x9**2 + 12*x6**2*x8*x10 + 12*x6**2*x8*x9
     &  + 6*x6**2*x8**2
      fun = fun + 12*x6**2*x7*x10 + 12*x6**2*x7*x9 + 12*x6**2*x7*x8 + 6
     & *x6**2*x7**2 + 4*x6**3*x10 + 4*x6**3*x9 + 4*x6**3*x8 + 4*x6**3*
     & x7 + x6**4 + 4*x5*x10**3 + 12*x5*x9*x10**2 + 12*x5*x9**2*x10 + 4
     & *x5*x9**3 + 12*x5*x8*x10**2 + 24*x5*x8*x9*x10 + 12*x5*x8*x9**2
     &  + 12*x5*x8**2*x10 + 12*x5*x8**2*x9 + 4*x5*x8**3 + 12*x5*x7*
     & x10**2 + 24*x5*x7*x9*x10 + 12*x5*x7*x9**2 + 24*x5*x7*x8*x10 + 24
     & *x5*x7*x8*x9 + 12*x5*x7*x8**2 + 12*x5*x7**2*x10 + 12*x5*x7**2*x9
     &  + 12*x5*x7**2*x8 + 4*x5*x7**3 + 12*x5*x6*x10**2 + 24*x5*x6*x9*
     & x10 + 12*x5*x6*x9**2 + 24*x5*x6*x8*x10 + 24*x5*x6*x8*x9 + 12*x5*
     & x6*x8**2 + 24*x5*x6*x7*x10 + 24*x5*x6*x7*x9 + 24*x5*x6*x7*x8 + 
     & 12*x5*x6*x7**2 + 12*x5*x6**2*x10 + 12*x5*x6**2*x9 + 12*x5*x6**2*
     & x8 + 12*x5*x6**2*x7 + 4*x5*x6**3 + 6*x5**2*x10**2 + 12*x5**2*x9*
     & x10 + 6*x5**2*x9**2 + 12*x5**2*x8*x10 + 12*x5**2*x8*x9 + 6*x5**2
     & *x8**2 + 12*x5**2*x7*x10 + 12*x5**2*x7*x9 + 12*x5**2*x7*x8 + 6*
     & x5**2*x7**2 + 12*x5**2*x6*x10 + 12*x5**2*x6*x9 + 12*x5**2*x6*x8
     &  + 12*x5**2*x6*x7
      fun = fun + 6*x5**2*x6**2 + 4*x5**3*x10 + 4*x5**3*x9 + 4*x5**3*x8
     &  + 4*x5**3*x7 + 4*x5**3*x6 + x5**4 + 4*x4*x10**3 + 12*x4*x9*
     & x10**2 + 12*x4*x9**2*x10 + 4*x4*x9**3 + 12*x4*x8*x10**2 + 24*x4*
     & x8*x9*x10 + 12*x4*x8*x9**2 + 12*x4*x8**2*x10 + 12*x4*x8**2*x9 + 
     & 4*x4*x8**3 + 12*x4*x7*x10**2 + 24*x4*x7*x9*x10 + 12*x4*x7*x9**2
     &  + 24*x4*x7*x8*x10 + 24*x4*x7*x8*x9 + 12*x4*x7*x8**2 + 12*x4*
     & x7**2*x10 + 12*x4*x7**2*x9 + 12*x4*x7**2*x8 + 4*x4*x7**3 + 12*x4
     & *x6*x10**2 + 24*x4*x6*x9*x10 + 12*x4*x6*x9**2 + 24*x4*x6*x8*x10
     &  + 24*x4*x6*x8*x9 + 12*x4*x6*x8**2 + 24*x4*x6*x7*x10 + 24*x4*x6*
     & x7*x9 + 24*x4*x6*x7*x8 + 12*x4*x6*x7**2 + 12*x4*x6**2*x10 + 12*
     & x4*x6**2*x9 + 12*x4*x6**2*x8 + 12*x4*x6**2*x7 + 4*x4*x6**3 + 12*
     & x4*x5*x10**2 + 24*x4*x5*x9*x10 + 12*x4*x5*x9**2 + 24*x4*x5*x8*
     & x10 + 24*x4*x5*x8*x9 + 12*x4*x5*x8**2 + 24*x4*x5*x7*x10 + 24*x4*
     & x5*x7*x9 + 24*x4*x5*x7*x8 + 12*x4*x5*x7**2 + 24*x4*x5*x6*x10 + 
     & 24*x4*x5*x6*x9 + 24*x4*x5*x6*x8 + 24*x4*x5*x6*x7 + 12*x4*x5*
     & x6**2
      fun = fun + 12*x4*x5**2*x10 + 12*x4*x5**2*x9 + 12*x4*x5**2*x8 + 
     & 12*x4*x5**2*x7 + 12*x4*x5**2*x6 + 4*x4*x5**3 + 6*x4**2*x10**2 + 
     & 12*x4**2*x9*x10 + 6*x4**2*x9**2 + 12*x4**2*x8*x10 + 12*x4**2*x8*
     & x9 + 6*x4**2*x8**2 + 12*x4**2*x7*x10 + 12*x4**2*x7*x9 + 12*x4**2
     & *x7*x8 + 6*x4**2*x7**2 + 12*x4**2*x6*x10 + 12*x4**2*x6*x9 + 12*
     & x4**2*x6*x8 + 12*x4**2*x6*x7 + 6*x4**2*x6**2 + 12*x4**2*x5*x10
     &  + 12*x4**2*x5*x9 + 12*x4**2*x5*x8 + 12*x4**2*x5*x7 + 12*x4**2*
     & x5*x6 + 6*x4**2*x5**2 + 4*x4**3*x10 + 4*x4**3*x9 + 4*x4**3*x8 + 
     & 4*x4**3*x7 + 4*x4**3*x6 + 4*x4**3*x5 + x4**4 + 4*x3*x10**3 + 12*
     & x3*x9*x10**2 + 12*x3*x9**2*x10 + 4*x3*x9**3 + 12*x3*x8*x10**2 + 
     & 24*x3*x8*x9*x10 + 12*x3*x8*x9**2 + 12*x3*x8**2*x10 + 12*x3*x8**2
     & *x9 + 4*x3*x8**3 + 12*x3*x7*x10**2 + 24*x3*x7*x9*x10 + 12*x3*x7*
     & x9**2 + 24*x3*x7*x8*x10 + 24*x3*x7*x8*x9 + 12*x3*x7*x8**2 + 12*
     & x3*x7**2*x10 + 12*x3*x7**2*x9 + 12*x3*x7**2*x8 + 4*x3*x7**3 + 12
     & *x3*x6*x10**2 + 24*x3*x6*x9*x10 + 12*x3*x6*x9**2 + 24*x3*x6*x8*
     & x10
      fun = fun + 24*x3*x6*x8*x9 + 12*x3*x6*x8**2 + 24*x3*x6*x7*x10 + 
     & 24*x3*x6*x7*x9 + 24*x3*x6*x7*x8 + 12*x3*x6*x7**2 + 12*x3*x6**2*
     & x10 + 12*x3*x6**2*x9 + 12*x3*x6**2*x8 + 12*x3*x6**2*x7 + 4*x3*
     & x6**3 + 12*x3*x5*x10**2 + 24*x3*x5*x9*x10 + 12*x3*x5*x9**2 + 24*
     & x3*x5*x8*x10 + 24*x3*x5*x8*x9 + 12*x3*x5*x8**2 + 24*x3*x5*x7*x10
     &  + 24*x3*x5*x7*x9 + 24*x3*x5*x7*x8 + 12*x3*x5*x7**2 + 24*x3*x5*
     & x6*x10 + 24*x3*x5*x6*x9 + 24*x3*x5*x6*x8 + 24*x3*x5*x6*x7 + 12*
     & x3*x5*x6**2 + 12*x3*x5**2*x10 + 12*x3*x5**2*x9 + 12*x3*x5**2*x8
     &  + 12*x3*x5**2*x7 + 12*x3*x5**2*x6 + 4*x3*x5**3 + 12*x3*x4*
     & x10**2 + 24*x3*x4*x9*x10 + 12*x3*x4*x9**2 + 24*x3*x4*x8*x10 + 24
     & *x3*x4*x8*x9 + 12*x3*x4*x8**2 + 24*x3*x4*x7*x10 + 24*x3*x4*x7*x9
     &  + 24*x3*x4*x7*x8 + 12*x3*x4*x7**2 + 24*x3*x4*x6*x10 + 24*x3*x4*
     & x6*x9 + 24*x3*x4*x6*x8 + 24*x3*x4*x6*x7 + 12*x3*x4*x6**2 + 24*x3
     & *x4*x5*x10 + 24*x3*x4*x5*x9 + 24*x3*x4*x5*x8 + 24*x3*x4*x5*x7 + 
     & 24*x3*x4*x5*x6 + 12*x3*x4*x5**2 + 12*x3*x4**2*x10 + 12*x3*x4**2*
     & x9
      fun = fun + 12*x3*x4**2*x8 + 12*x3*x4**2*x7 + 12*x3*x4**2*x6 + 12
     & *x3*x4**2*x5 + 4*x3*x4**3 + 6*x3**2*x10**2 + 12*x3**2*x9*x10 + 6
     & *x3**2*x9**2 + 12*x3**2*x8*x10 + 12*x3**2*x8*x9 + 6*x3**2*x8**2
     &  + 12*x3**2*x7*x10 + 12*x3**2*x7*x9 + 12*x3**2*x7*x8 + 6*x3**2*
     & x7**2 + 12*x3**2*x6*x10 + 12*x3**2*x6*x9 + 12*x3**2*x6*x8 + 12*
     & x3**2*x6*x7 + 6*x3**2*x6**2 + 12*x3**2*x5*x10 + 12*x3**2*x5*x9
     &  + 12*x3**2*x5*x8 + 12*x3**2*x5*x7 + 12*x3**2*x5*x6 + 6*x3**2*
     & x5**2 + 12*x3**2*x4*x10 + 12*x3**2*x4*x9 + 12*x3**2*x4*x8 + 12*
     & x3**2*x4*x7 + 12*x3**2*x4*x6 + 12*x3**2*x4*x5 + 6*x3**2*x4**2 + 
     & 4*x3**3*x10 + 4*x3**3*x9 + 4*x3**3*x8 + 4*x3**3*x7 + 4*x3**3*x6
     &  + 4*x3**3*x5 + 4*x3**3*x4 + x3**4 + 4*x2*x10**3 + 12*x2*x9*
     & x10**2 + 12*x2*x9**2*x10 + 4*x2*x9**3 + 12*x2*x8*x10**2 + 24*x2*
     & x8*x9*x10 + 12*x2*x8*x9**2 + 12*x2*x8**2*x10 + 12*x2*x8**2*x9 + 
     & 4*x2*x8**3 + 12*x2*x7*x10**2 + 24*x2*x7*x9*x10 + 12*x2*x7*x9**2
     &  + 24*x2*x7*x8*x10 + 24*x2*x7*x8*x9 + 12*x2*x7*x8**2 + 12*x2*
     & x7**2*x10
      fun = fun + 12*x2*x7**2*x9 + 12*x2*x7**2*x8 + 4*x2*x7**3 + 12*x2*
     & x6*x10**2 + 24*x2*x6*x9*x10 + 12*x2*x6*x9**2 + 24*x2*x6*x8*x10
     &  + 24*x2*x6*x8*x9 + 12*x2*x6*x8**2 + 24*x2*x6*x7*x10 + 24*x2*x6*
     & x7*x9 + 24*x2*x6*x7*x8 + 12*x2*x6*x7**2 + 12*x2*x6**2*x10 + 12*
     & x2*x6**2*x9 + 12*x2*x6**2*x8 + 12*x2*x6**2*x7 + 4*x2*x6**3 + 12*
     & x2*x5*x10**2 + 24*x2*x5*x9*x10 + 12*x2*x5*x9**2 + 24*x2*x5*x8*
     & x10 + 24*x2*x5*x8*x9 + 12*x2*x5*x8**2 + 24*x2*x5*x7*x10 + 24*x2*
     & x5*x7*x9 + 24*x2*x5*x7*x8 + 12*x2*x5*x7**2 + 24*x2*x5*x6*x10 + 
     & 24*x2*x5*x6*x9 + 24*x2*x5*x6*x8 + 24*x2*x5*x6*x7 + 12*x2*x5*
     & x6**2 + 12*x2*x5**2*x10 + 12*x2*x5**2*x9 + 12*x2*x5**2*x8 + 12*
     & x2*x5**2*x7 + 12*x2*x5**2*x6 + 4*x2*x5**3 + 12*x2*x4*x10**2 + 24
     & *x2*x4*x9*x10 + 12*x2*x4*x9**2 + 24*x2*x4*x8*x10 + 24*x2*x4*x8*
     & x9 + 12*x2*x4*x8**2 + 24*x2*x4*x7*x10 + 24*x2*x4*x7*x9 + 24*x2*
     & x4*x7*x8 + 12*x2*x4*x7**2 + 24*x2*x4*x6*x10 + 24*x2*x4*x6*x9 + 
     & 24*x2*x4*x6*x8 + 24*x2*x4*x6*x7 + 12*x2*x4*x6**2 + 24*x2*x4*x5*
     & x10
      fun = fun + 24*x2*x4*x5*x9 + 24*x2*x4*x5*x8 + 24*x2*x4*x5*x7 + 24
     & *x2*x4*x5*x6 + 12*x2*x4*x5**2 + 12*x2*x4**2*x10 + 12*x2*x4**2*x9
     &  + 12*x2*x4**2*x8 + 12*x2*x4**2*x7 + 12*x2*x4**2*x6 + 12*x2*
     & x4**2*x5 + 4*x2*x4**3 + 12*x2*x3*x10**2 + 24*x2*x3*x9*x10 + 12*
     & x2*x3*x9**2 + 24*x2*x3*x8*x10 + 24*x2*x3*x8*x9 + 12*x2*x3*x8**2
     &  + 24*x2*x3*x7*x10 + 24*x2*x3*x7*x9 + 24*x2*x3*x7*x8 + 12*x2*x3*
     & x7**2 + 24*x2*x3*x6*x10 + 24*x2*x3*x6*x9 + 24*x2*x3*x6*x8 + 24*
     & x2*x3*x6*x7 + 12*x2*x3*x6**2 + 24*x2*x3*x5*x10 + 24*x2*x3*x5*x9
     &  + 24*x2*x3*x5*x8 + 24*x2*x3*x5*x7 + 24*x2*x3*x5*x6 + 12*x2*x3*
     & x5**2 + 24*x2*x3*x4*x10 + 24*x2*x3*x4*x9 + 24*x2*x3*x4*x8 + 24*
     & x2*x3*x4*x7 + 24*x2*x3*x4*x6 + 24*x2*x3*x4*x5 + 12*x2*x3*x4**2
     &  + 12*x2*x3**2*x10 + 12*x2*x3**2*x9 + 12*x2*x3**2*x8 + 12*x2*
     & x3**2*x7 + 12*x2*x3**2*x6 + 12*x2*x3**2*x5 + 12*x2*x3**2*x4 + 4*
     & x2*x3**3 + 6*x2**2*x10**2 + 12*x2**2*x9*x10 + 6*x2**2*x9**2 + 12
     & *x2**2*x8*x10 + 12*x2**2*x8*x9 + 6*x2**2*x8**2 + 12*x2**2*x7*x10
     &  + 12*x2**2*x7*x9
      fun = fun + 12*x2**2*x7*x8 + 6*x2**2*x7**2 + 12*x2**2*x6*x10 + 12
     & *x2**2*x6*x9 + 12*x2**2*x6*x8 + 12*x2**2*x6*x7 + 6*x2**2*x6**2
     &  + 12*x2**2*x5*x10 + 12*x2**2*x5*x9 + 12*x2**2*x5*x8 + 12*x2**2*
     & x5*x7 + 12*x2**2*x5*x6 + 6*x2**2*x5**2 + 12*x2**2*x4*x10 + 12*
     & x2**2*x4*x9 + 12*x2**2*x4*x8 + 12*x2**2*x4*x7 + 12*x2**2*x4*x6
     &  + 12*x2**2*x4*x5 + 6*x2**2*x4**2 + 12*x2**2*x3*x10 + 12*x2**2*
     & x3*x9 + 12*x2**2*x3*x8 + 12*x2**2*x3*x7 + 12*x2**2*x3*x6 + 12*
     & x2**2*x3*x5 + 12*x2**2*x3*x4 + 6*x2**2*x3**2 + 4*x2**3*x10 + 4*
     & x2**3*x9 + 4*x2**3*x8 + 4*x2**3*x7 + 4*x2**3*x6 + 4*x2**3*x5 + 4
     & *x2**3*x4 + 4*x2**3*x3 + x2**4 + 4*x1*x10**3 + 12*x1*x9*x10**2
     &  + 12*x1*x9**2*x10 + 4*x1*x9**3 + 12*x1*x8*x10**2 + 24*x1*x8*x9*
     & x10 + 12*x1*x8*x9**2 + 12*x1*x8**2*x10 + 12*x1*x8**2*x9 + 4*x1*
     & x8**3 + 12*x1*x7*x10**2 + 24*x1*x7*x9*x10 + 12*x1*x7*x9**2 + 24*
     & x1*x7*x8*x10 + 24*x1*x7*x8*x9 + 12*x1*x7*x8**2 + 12*x1*x7**2*x10
     &  + 12*x1*x7**2*x9 + 12*x1*x7**2*x8 + 4*x1*x7**3 + 12*x1*x6*
     & x10**2
      fun = fun + 24*x1*x6*x9*x10 + 12*x1*x6*x9**2 + 24*x1*x6*x8*x10 + 
     & 24*x1*x6*x8*x9 + 12*x1*x6*x8**2 + 24*x1*x6*x7*x10 + 24*x1*x6*x7*
     & x9 + 24*x1*x6*x7*x8 + 12*x1*x6*x7**2 + 12*x1*x6**2*x10 + 12*x1*
     & x6**2*x9 + 12*x1*x6**2*x8 + 12*x1*x6**2*x7 + 4*x1*x6**3 + 12*x1*
     & x5*x10**2 + 24*x1*x5*x9*x10 + 12*x1*x5*x9**2 + 24*x1*x5*x8*x10
     &  + 24*x1*x5*x8*x9 + 12*x1*x5*x8**2 + 24*x1*x5*x7*x10 + 24*x1*x5*
     & x7*x9 + 24*x1*x5*x7*x8 + 12*x1*x5*x7**2 + 24*x1*x5*x6*x10 + 24*
     & x1*x5*x6*x9 + 24*x1*x5*x6*x8 + 24*x1*x5*x6*x7 + 12*x1*x5*x6**2
     &  + 12*x1*x5**2*x10 + 12*x1*x5**2*x9 + 12*x1*x5**2*x8 + 12*x1*
     & x5**2*x7 + 12*x1*x5**2*x6 + 4*x1*x5**3 + 12*x1*x4*x10**2 + 24*x1
     & *x4*x9*x10 + 12*x1*x4*x9**2 + 24*x1*x4*x8*x10 + 24*x1*x4*x8*x9
     &  + 12*x1*x4*x8**2 + 24*x1*x4*x7*x10 + 24*x1*x4*x7*x9 + 24*x1*x4*
     & x7*x8 + 12*x1*x4*x7**2 + 24*x1*x4*x6*x10 + 24*x1*x4*x6*x9 + 24*
     & x1*x4*x6*x8 + 24*x1*x4*x6*x7 + 12*x1*x4*x6**2 + 24*x1*x4*x5*x10
     &  + 24*x1*x4*x5*x9 + 24*x1*x4*x5*x8 + 24*x1*x4*x5*x7 + 24*x1*x4*
     & x5*x6
      fun = fun + 12*x1*x4*x5**2 + 12*x1*x4**2*x10 + 12*x1*x4**2*x9 + 
     & 12*x1*x4**2*x8 + 12*x1*x4**2*x7 + 12*x1*x4**2*x6 + 12*x1*x4**2*
     & x5 + 4*x1*x4**3 + 12*x1*x3*x10**2 + 24*x1*x3*x9*x10 + 12*x1*x3*
     & x9**2 + 24*x1*x3*x8*x10 + 24*x1*x3*x8*x9 + 12*x1*x3*x8**2 + 24*
     & x1*x3*x7*x10 + 24*x1*x3*x7*x9 + 24*x1*x3*x7*x8 + 12*x1*x3*x7**2
     &  + 24*x1*x3*x6*x10 + 24*x1*x3*x6*x9 + 24*x1*x3*x6*x8 + 24*x1*x3*
     & x6*x7 + 12*x1*x3*x6**2 + 24*x1*x3*x5*x10 + 24*x1*x3*x5*x9 + 24*
     & x1*x3*x5*x8 + 24*x1*x3*x5*x7 + 24*x1*x3*x5*x6 + 12*x1*x3*x5**2
     &  + 24*x1*x3*x4*x10 + 24*x1*x3*x4*x9 + 24*x1*x3*x4*x8 + 24*x1*x3*
     & x4*x7 + 24*x1*x3*x4*x6 + 24*x1*x3*x4*x5 + 12*x1*x3*x4**2 + 12*x1
     & *x3**2*x10 + 12*x1*x3**2*x9 + 12*x1*x3**2*x8 + 12*x1*x3**2*x7 + 
     & 12*x1*x3**2*x6 + 12*x1*x3**2*x5 + 12*x1*x3**2*x4 + 4*x1*x3**3 + 
     & 12*x1*x2*x10**2 + 24*x1*x2*x9*x10 + 12*x1*x2*x9**2 + 24*x1*x2*x8
     & *x10 + 24*x1*x2*x8*x9 + 12*x1*x2*x8**2 + 24*x1*x2*x7*x10 + 24*x1
     & *x2*x7*x9 + 24*x1*x2*x7*x8 + 12*x1*x2*x7**2 + 24*x1*x2*x6*x10 + 
     & 24*x1*x2*x6*x9
      fun = fun + 24*x1*x2*x6*x8 + 24*x1*x2*x6*x7 + 12*x1*x2*x6**2 + 24
     & *x1*x2*x5*x10 + 24*x1*x2*x5*x9 + 24*x1*x2*x5*x8 + 24*x1*x2*x5*x7
     &  + 24*x1*x2*x5*x6 + 12*x1*x2*x5**2 + 24*x1*x2*x4*x10 + 24*x1*x2*
     & x4*x9 + 24*x1*x2*x4*x8 + 24*x1*x2*x4*x7 + 24*x1*x2*x4*x6 + 24*x1
     & *x2*x4*x5 + 12*x1*x2*x4**2 + 24*x1*x2*x3*x10 + 24*x1*x2*x3*x9 + 
     & 24*x1*x2*x3*x8 + 24*x1*x2*x3*x7 + 24*x1*x2*x3*x6 + 24*x1*x2*x3*
     & x5 + 24*x1*x2*x3*x4 + 12*x1*x2*x3**2 + 12*x1*x2**2*x10 + 12*x1*
     & x2**2*x9 + 12*x1*x2**2*x8 + 12*x1*x2**2*x7 + 12*x1*x2**2*x6 + 12
     & *x1*x2**2*x5 + 12*x1*x2**2*x4 + 12*x1*x2**2*x3 + 4*x1*x2**3 + 6*
     & x1**2*x10**2 + 12*x1**2*x9*x10 + 6*x1**2*x9**2 + 12*x1**2*x8*x10
     &  + 12*x1**2*x8*x9 + 6*x1**2*x8**2 + 12*x1**2*x7*x10 + 12*x1**2*
     & x7*x9 + 12*x1**2*x7*x8 + 6*x1**2*x7**2 + 12*x1**2*x6*x10 + 12*
     & x1**2*x6*x9 + 12*x1**2*x6*x8 + 12*x1**2*x6*x7 + 6*x1**2*x6**2 + 
     & 12*x1**2*x5*x10 + 12*x1**2*x5*x9 + 12*x1**2*x5*x8 + 12*x1**2*x5*
     & x7 + 12*x1**2*x5*x6 + 6*x1**2*x5**2 + 12*x1**2*x4*x10 + 12*x1**2
     & *x4*x9
      fun = fun + 12*x1**2*x4*x8 + 12*x1**2*x4*x7 + 12*x1**2*x4*x6 + 12
     & *x1**2*x4*x5 + 6*x1**2*x4**2 + 12*x1**2*x3*x10 + 12*x1**2*x3*x9
     &  + 12*x1**2*x3*x8 + 12*x1**2*x3*x7 + 12*x1**2*x3*x6 + 12*x1**2*
     & x3*x5 + 12*x1**2*x3*x4 + 6*x1**2*x3**2 + 12*x1**2*x2*x10 + 12*
     & x1**2*x2*x9 + 12*x1**2*x2*x8 + 12*x1**2*x2*x7 + 12*x1**2*x2*x6
     &  + 12*x1**2*x2*x5 + 12*x1**2*x2*x4 + 12*x1**2*x2*x3 + 6*x1**2*
     & x2**2 + 4*x1**3*x10 + 4*x1**3*x9 + 4*x1**3*x8 + 4*x1**3*x7 + 4*
     & x1**3*x6 + 4*x1**3*x5 + 4*x1**3*x4 + 4*x1**3*x3 + 4*x1**3*x2 + 
     & x1**4
      RETURN
      END
	EOF
end
end
#] 3.44 :
#[ 6.0 :
class Sec_6_0 < FormTest
def setup
	input <<-EOF
    S	x,a,b;
    Off statistics;
    L	F = (a+b)^4+a*(a+x)^3;
    .sort
    \#$a = 0;
    if ( count(x,1) > $a ) $a = count_(x,1);
    Print "      >> After %t the maximum power of x is %$",$a;
    #write "     ># $a = `$a'"
    .sort
    #write "     ># $a = `$a'"
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert @stdout =~ Regexp.new(<<-EOF
     ># \\$a = 0
        \.sort
      >> .+0
      >> .+0
      >> .+0
      >> .+0
      >> .+0
      >> .+1
      >> .+2
      >> .+3
        #write "     ># \\$a = `\\$a'"
     ># \\$a = 3
	EOF
	)
end
end
#] 6.0 :
#[ 6.1 :
class Sec_6_1 < FormTest
def setup
	input <<-EOF
    S   a1,...,a10;
    L   F = (a1+...+a10)^3;
    .sort
    \#$c = 0;
    Print +f "<%w> %t";
    Multiply,(a1+...+a10);
    $c = $c+1;
    ModuleOption,sum,$c;
    .sort
    #message $c = `$c'
    \#$max = 0;
    \#$min = 10;
    if ( count(a1,1) > $max ) $max = count_(a1,1);
    if ( count(a4,1) < $min ) $min = count_(a4,1);
    ModuleOption,maximum,$max;
    ModuleOption,minimum,$min;
    .sort
    #message $max = `$max'
    #message $min = `$min'
    .end
	EOF
end
def test1
	extra_parameter "-w4"
	execute TFORM
	assert no_problem
	assert @stdout =~ /\s+\.sort\n(<(1|2|3|4)>\ \ \+\ \S+\n){220}\n/
	assert @stdout =~ /~~~\$c = 2200/
	assert @stdout =~ /~~~\$max = 4/
	assert @stdout =~ /~~~\$min = 0/
end
end
#] 6.1 :
#[ 7.6 :
class Sec_7_6 < FormTest
def setup
	input <<-EOF
    CF Z1, ..., Z4;
    S x, a, b;
    L s = a * (Z1(0,0,0,1,0,0,-1) - Z2(0,0,0,1,0,0,-1))
        + b * (Z3(-2,8,-1,-1) - Z4(-2,8,-1,-1));
    ArgImplode Z1;
    repeat id Z2(?a,0,x?!{0,0},?b) = Z2(?a,x+sig_(x),?b);
    ArgExplode Z3;
    repeat id Z4(?a,x?!{1,0,-1},?b) = Z4(?a,0,x-sig_(x),?b);
    id Z2(?a) = Z1(?a);
    id Z4(?a) = Z3(?a);
    Print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("s") =~ pattern("s = 0")
end
end
#] 7.6 :
#[ 7.40 :
class Sec_7_40 < FormTest
def setup
	input <<-EOF
    On OldFactArg;
    Symbols a,b,c;
    CFunctions f,f1,f2,f3;
    Local F = f(-3*a*b)+f(3*a*b)
             +f1(-3*a*b)+f1(3*a*b)
             +f2(-3*a*b)+f2(3*a*b)
             +f3(-3*a*b)+f3(3*a*b);
    FactArg,f;
    Factarg,(0),f1;
    Factarg,(1),f2;
    Factarg,(-1),f3;
    Print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("f(a,b,-1,3) + f(a,b,3) + 2*f1(a*b) + f2(a*b,-1,3) + f2(a*b,3) + f3(a*b,-3) + f3(a*b,3)")
end
end
#] 7.40 :
#[ 7.61 :
class Sec_7_61 < FormTest
def setup
	input <<-EOF
	CF f,g;
	I i1;
	S x,y,z;
    L F = f(i1,x)*(g(i1,y)+g(i1,z));
    B  f;
    .sort
    Keep Brackets;
    sum i1;
	Print;
	.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("f(N1_?,x)*g(i1,y)+f(N1_?,x)*g(i1,z)")
end
end
#] 7.61 :
#[ 7.65 :
class Sec_7_65 < FormTest
def setup
	input <<-EOF
    S   a,b,c;
    CF  f;
    L   F = f(22/3*a+14/5*b+18/7*c);
    MakeInteger,f;
    Print +f;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("2/105*f(135*c + 147*b + 385*a)")
end
end
#] 7.65 :
#[ 7.88 :
class Sec_7_88 < FormTest
def setup
	input <<-EOF
	Symbol x,y;
	CF acc;
    PolyFun acc;
    Local F = 3*x^2*acc(1+y+y^2)+2*x^2*acc(1-y+y^2);
	Print;
	.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("x^2 * acc(5 + y + 5*y^2)")
end
end
#] 7.88 :
#[ 7.91 :
class Sec_7_91 < FormTest
def setup
	input <<-EOF
    Symbols a,b,c;
    Local F = 3*a+2*b;
    Print "> %T";
    id  a = b+c;
    Print ">> %t";
    Print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert @stdout =~ exact_pattern(<<-EOF
> 3*a
>>  + 3*b
>>  + 3*c
> 2*b
>>  + 2*b
	EOF
	)
	assert result("F") =~ pattern("3*c + 5*b")
end
end
#] 7.91 :
#[ 7.101 :
class Sec_7_101 < FormTest
def setup
	input <<-EOF
    Functions f(antisymmetric),ff(cyclesymmetric);
    Indices i1,...,i8;
    Local F = f(i1,i4,i2)*f(i5,i2,i3)*f(i3,i1,i6)*f(i4,i7,i8);
    ReplaceLoop f,arg=3,loop=3,out=ff;
	Print;
	.sort
	.clear
    Functions f(antisymmetric),ff(cyclesymmetric);
    Indices i1,...,i9;
    Local F = f(i1,i4,i2)*f(i5,i2,i3)*f(i3,i1,i6)*f(i4,i7,i8)
            *f(i6,i7,i8);
    ReplaceLoop f,arg=3,loop=all,out=ff;
	Print;
	.end
	EOF
end
def test1
	execute FORM
	assert no_problem
    assert result("F",0) =~ pattern("-ff(i4,i5,i6)*f(i4,i7,i8)")
    assert result("F",1)=~ pattern("-f(i1,i2,i4)*f(i2,i3,i5)*f(i1,i3,i6)*ff(i4,i6)")
end
end
#] 7.101 :
#[ 7.106 :
class Sec_7_106 < FormTest
def setup
	input <<-EOF
   CF f,ff,g;
   S a,b,c,d,x1,x2;
   Local F1 = ff*f(a,b)*f(c,d);
   Local F2 = g(a,b)*g(c,d);
   repeat id f(x1?,?a)*f(x2?,?b)*ff(?c) =
            +f(?a)*f(x2,?b)*ff(?c,x1)
            +f(x1,?a)*f(?b)*ff(?c,x2);
   id f(?a)*f(?b)*ff(?c) = f(?c,?a,?b);
   shuffle,g;
   Print;
   .end
\end{verbatim}
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F1") =~ pattern("f(a,b,c,d)+f(a,c,b,d)+f(a,c,d,b)+f(c,a,b,d)+f(c,a,d,b)+f(c,d,a,b)")
	assert result("F2") =~ pattern("g(a,b,c,d)+g(a,c,b,d)+g(a,c,d,b)+g(c,a,b,d)+g(c,a,d,b)+g(c,d,a,b)")
end
end
#] 7.106 :
#[ 7.113 :
class Sec_7_113 < FormTest
def setup
	input <<-EOF
    CF  S,R;
    Symbols N,n;
    L   F = S(R(1,-3),N)*S(R(-5,1),N);
    id  S(R(?a),n?)*S(R(?b),n?) = S(?a)*S(?b)*R(n);
    Stuffle,S-;
    id  S(?a)*R(n?) = S(R(?a),n);
    Print +s;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern(<<-EOF
       + S(R(-6,-4),N)
       - S(R(-6,-3,1),N)
       - S(R(-6,1,-3),N)
       - S(R(-5,1,-4),N)
       + S(R(-5,1,-3,1),N)
       + 2*S(R(-5,1,1,-3),N)
       - S(R(-5,2,-3),N)
       - S(R(1,-5,-4),N)
       + S(R(1,-5,-3,1),N)
       + S(R(1,-5,1,-3),N)
       + S(R(1,-3,-5,1),N)
       - S(R(1,8,1),N)
      ;
	  EOF
	)
end
end
#] 7.113 :
#[ 8.11 :
class Sec_8_11 < FormTest
def setup
	input <<-EOF
    Symbols x1,...,x4;
    CFunctions f,f1,f2;
    Local F = f(x1,...,x4);
    id  f(?a) = distrib_(-1,2,f1,f2,?a);
    Print +s;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern(<<-EOF
       + f1(x1,x2)*f2(x3,x4)
       - f1(x1,x3)*f2(x2,x4)
       + f1(x1,x4)*f2(x2,x3)
       + f1(x2,x3)*f2(x1,x4)
       - f1(x2,x4)*f2(x1,x3)
       + f1(x3,x4)*f2(x1,x2)
	   EOF
	)
end
end
#] 8.11 :
#[ 8.53 :
class Sec_8_53 < FormTest
def setup
	input <<-EOF
    Symbol i,x;
    Local F = sump_(i,0,5,x/i);
    Print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F") =~ pattern("1 + x + 1/2*x^2 + 1/6*x^3 + 1/24*x^4 + 1/120*x^5")
end
end
#] 8.53 :
#[ 9 :
class Sec_9 < FormTest
def setup
	input <<-EOF
    Symbols a,b,c,x;
    L  F = a*x^2+b*x+c;
    B x;
    .sort
    L  Discriminant = F[x]^2-4*F[x^2]*F[1];
    Print;
    .end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("Discriminant") =~ pattern("b^2-4*a*c");
end
end
#] 9 :
#[ 11 :
class Sec_11_1 < FormTest
def setup
	input <<-EOF
*
*   Symmetric trace of a gamma5 and 12 regular matrices
*
I   m1,...,m12;
F   G5,g1,g2;
L   F = G5(m1,...,m12);
id  G5(?a) = distrib_(-1,4,g1,g2,?a);
id  g1(?a) = e_(?a);
id  g2(?a) = g_(1,?a);
tracen,1;
.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert @stdout =~ /Generated terms =      51975$/
	assert @stdout =~ /Terms in output =      51975$/
#	assert @stdout =~ /Bytes used      =     919164$/ # correct on 64bit?!?
end
end
class Sec_11_2 < FormTest
def setup
	input <<-EOF
*
*   Regular trace of a gamma5 and 12 regular matrices
*
I   m1,...,m12;
L   F = g_(1,5_,m1,...,m12);
trace4,1;
.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert @stdout =~ /Generated terms =       1053$/
	assert @stdout =~ /Terms in output =       1029$/
#	assert @stdout =~ /Bytes used      =      20284$/ # correct on 64bit?!?
end
end
#] 11 :
#[ 12 :
class Sec_12_1 < FormTest
def setup
	input <<-EOF
Indices m1,m2,m3,n1,n2,n3,i1,i2,i3;
Cfunction eta(symmetric),e(antisymmetric);
Off Statistics;
*
*   We have our own Levi-Civita tensor e
*
Local F = e(m1,m2,m3)*e(m1,m2,m3);
*
*   We write the contraction as
*
id  e(m1?,m2?,m3?)*e(n1?,n2?,n3?) =
        e_(m1,m2,m3)*e_(i1,i2,i3)*
            eta(n1,i1)*eta(n2,i2)*eta(n3,i3);
*
*   Now we can use the internal workings of the contract:
*
Contract;
Print +s;
.sort;
*
*   For specifying a metric we need individual components:
*
Sum i1,1,2,3;
Sum i2,1,2,3;
Sum i3,1,2,3;
Print +s;
.sort;
*
*   And now we can provide the metric tensor
*
id  eta(1,1) = 1;
id  eta(2,2) = 1;
id  eta(3,3) = -1;
id  eta(1,2) = 0;
id  eta(1,3) = 0;
id  eta(2,3) = 0;
Print +s;
.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F",0) =~ pattern(<<-EOF
   F =
       + eta(i1,i1)*eta(i2,i2)*eta(i3,i3)
       - eta(i1,i1)*eta(i2,i3)^2
       - eta(i1,i2)^2*eta(i3,i3)
       + 2*eta(i1,i2)*eta(i1,i3)*eta(i2,i3)
       - eta(i1,i3)^2*eta(i2,i2)
      ;
	  EOF
	)
	assert result("F",1) =~ pattern(<<-EOF
   F =
       + 6*eta(1,1)*eta(2,2)*eta(3,3)
       - 6*eta(1,1)*eta(2,3)^2
       - 6*eta(1,2)^2*eta(3,3)
       + 12*eta(1,2)*eta(1,3)*eta(2,3)
       - 6*eta(1,3)^2*eta(2,2)
      ;
	  EOF
	)
	assert result("F") =~ pattern("-6")
end
end
class Sec_12_2 < FormTest
def setup
	input <<-EOF
Indices i1,i2,i3;
FixIndex 1:1,2:1,3:-1;
Off Statistics;
*
Local F = e_(i1,i2,i3)*e_(i1,i2,i3);
Sum i1,1,2,3;
Sum i2,1,2,3;
Sum i3,1,2,3;
Print +s;
.sort
Contract;
Print +s;
.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F",0) =~ pattern("+6*e_(1,2,3)*e_(1,2,3)")
	assert result("F") =~ pattern("-6")
end
end
class Sec_12_3 < FormTest
def setup
	input <<-EOF
Indices i1=0,i2=0,i3=0;
FixIndex 1:1,2:1,3:-1;
Off Statistics;
*
Local F = e_(i1,i2,i3)*e_(i1,i2,i3);
Contract;
Print +s;
.sort
Sum i1,1,2,3;
Sum i2,1,2,3;
Sum i3,1,2,3;
Print +s;
.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("F",0) =~ pattern(<<-EOF
   F =
       + d_(i1,i1)*d_(i2,i2)*d_(i3,i3)
       - d_(i1,i1)*d_(i2,i3)*d_(i2,i3)
       - d_(i1,i2)*d_(i1,i2)*d_(i3,i3)
       + 2*d_(i1,i2)*d_(i1,i3)*d_(i2,i3)
       - d_(i1,i3)*d_(i1,i3)*d_(i2,i2)
      ;
	  EOF
	)
	assert result("F") =~ pattern("-6")
end
end
#] 12 :
#[ 15 :
class Sec_15 < FormTest
def setup
	input <<-EOF
symbol a,b;

#external "n1" cat -u

#external "n2" cat -u

*  cat simply repeats its input. The default prompt is an
*  empty line. So we use "\n\n" here -- one "\n" is to finish
*  the line, and the next "\n" is the prompt:
#toexternal "(a+b)^2\n\n"

#setexternal `n1'
*  For this channel the prompt will be "READY\n":
#toexternal "(a+b)^3\nREADY\n"

#setexternal `n2'
*  Set the default prompt:
#prompt
Local aPLUSbTO2=
#fromexternal
       ;

#setexternal `n1'
#prompt READY
Local aPLUSbTO3=
#fromexternal
       ;

#rmexternal `n1'
#rmexternal `n2'

Print;
.end
	EOF
end
def test1
	execute FORM
	assert no_problem
	assert result("aPLUSbTO2") =~ pattern("")
	assert result("aPLUSbTO3") =~ pattern("")
end
end
#] 15 :
