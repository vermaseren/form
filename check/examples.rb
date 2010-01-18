# Tests using the examples in the manual
#
# Some assertions here check for trivial, secondary things like runtime
# information or layout. Usually, this should be avoided. But since we are here
# not only testing FORM but also the code examples in the manual, this extra
# strictness makes sometimes sense.

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
	assert @stdout =~ /Bytes used \s+=\s+54$/
	assert result("F") =~ Pattern("x^7 + 7*x^8 + 21*x^9 + 35*x^10")
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
	assert result("expr") =~ Pattern("g1(b1) + g2(b2) + g3(b3) + g(x)")
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
	assert result("F") =~ Pattern("2*f1(N1_?)*f2(N1_?)")
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
	assert result("F") =~ Pattern("p.p*q.q")
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
	assert result("F") =~ Pattern("p.q^2")
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
	assert result("G") =~ Pattern("f(N1_?,N2_?)*f(N2_?,N1_?)*f(N3_?,N4_?)*f(N4_?,N3_?)*f(N5_?,N6_?)*f(N6_?,
		N5_?)*g(N7_?,N8_?,p1,N9_?)*g(N10_?,N11_?,p2,N12_?)*e_(p1,p2,p3,N9_?)*e_(
		p1,p2,p3,N12_?)*e_(N7_?,N8_?,N10_?,N11_?)")
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
	assert result("F1") =~ Pattern("6")
	assert result("F2") =~ Pattern("7")
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
	assert @stdout =~ ExactPattern(<<-EOF
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
	assert @stdout =~ ExactPattern(<<-EOF
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
