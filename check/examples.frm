* Tests using the examples in the manual
*
* Some assertions here check for trivial, secondary things like runtime
* information or layout. Usually, this should be avoided. But since we are here
* not only testing FORM but also the code examples in the manual, this extra
* strictness makes sometimes sense.
* In the manual the example code has been given a comment that says it is used
* here. Therefore, if you change something here, consider applying the
* appropriate changes also in the manual.

#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ Var_Symbols_1 :
    s     x(:10),y;
    L     F=y^7;
    id    y=x+x^2;
    print;
    .end
	assert succeeded?
	assert bytesize("F") == 27 * wordsize
	assert result("F") =~ expr("x^7 + 7*x^8 + 21*x^9 + 35*x^10")
*--#] Var_Symbols_1 : 
*--#[ Var_Sets_1 :
    Symbols a1,a2,a3,b1,b2,b3,x,n;
    CFunctions g1,g2,g3,g;
    Local expr =
        g(a1)+g(a2)+g(a3)+g(x);
    id,g(x?{a1,a2,a3}[n]) = {g1,g2,g3}[n]({b1,b2,b3}[n]);
    print;
    .end
	assert succeeded?
	assert result("expr") =~ expr("g1(b1) + g2(b2) + g3(b3) + g(x)")
*--#] Var_Sets_1 : 
*--#[ Var_Dummy_indices_1 :
   i  mu,nu;
   f  f1,f2;
   L  F=f1(mu)*f2(mu)+f1(nu)*f2(nu);
   sum  mu;
   sum  nu;
   print;
   .end
	assert succeeded?
	assert result("F") =~ expr("2*f1(N1_?)*f2(N1_?)")
*--#] Var_Dummy_indices_1 : 
*--#[ Var_Dummy_indices_2 :
    Index mu,nu;
    CFunctions f,g;
    Vectors p,q;
    Local F = (f(mu)*g(mu))^2;
    sum mu;
    id f(nu?) = p(nu);
    id g(nu?) = q(nu);
    print;
    .end
	assert succeeded?
	assert result("F") =~ expr("p.p*q.q")
*--#] Var_Dummy_indices_2 : 
*--#[ Var_Dummy_indices_3 :
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
	assert succeeded?
	assert result("F") =~ expr("p.q^2")
*--#] Var_Dummy_indices_3 : 
*--#[ Var_Dummy_indices_4 :
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
	assert succeeded?
	assert result("G") =~ expr("f(N1_?,N2_?)*f(N2_?,N1_?)*f(N3_?,N4_?)*f(N4_?,N3_?)*f(N5_?,N6_?)*f(N6_?,N5_?)
		*g(N7_?,N8_?,p1,N9_?)*g(N10_?,N11_?,p2,N12_?)*e_(p1,p2,p3,N9_?)*e_(p1,p2,p3,N12_?)*e_(N7_?,N8_?,N10_?,N11_?)")
*--#] Var_Dummy_indices_4 : 
*--#[ Var_Extra_Symbols_1 :
* NOTE: removed "Generated on `DATE_'"
    Vector p,q,p1,p2;
    CFunction f;
    CFunction Dot,InvDot;
    Symbol x,x1,x2;
    Set pdot:p,q;
    Off Statistics;
    Local F = x+x^2+1/x+1/x^2+f(x1)+f(x2)*p.q*x+f(x2)/p.q^2;
    id  p1?pdot.p2?pdot = Dot(p1,p2);
    id  1/p1?pdot.p2?pdot = InvDot(p1,p2);
    Print;
    .sort

    ExtraSymbols,array,Y;
    Format DOUBLEFORTRAN;
    ToPolynomial;
    Print;
    .sort

    #write <sub.f> "      SUBROUTINE sub(Y)"
    #write <sub.f> "*"
    #write <sub.f> "*      Compute the extra symbols."
    #write <sub.f> "*"
    #write <sub.f> "      REAL*8 Y(`EXTRASYMBOLS_')"
    #write <sub.f> "      REAL*8 Dot,InvDot"
    #write <sub.f> "      Dot(p1,p2)=p1(1)*p2(1)-p1(2)*p2(2)-p1(3)*p2(3)\
                                                              -p1(4)*p2(4)"
    #write <sub.f> "      InvDot(p1,p2)=1.D0/(Dot(p1,p2))"
    #write <sub.f> "*"
    #write <sub.f> "*        We still have to add definitions here."
    #write <sub.f> "*        And we have to import all the variables."
    #write <sub.f> "*"
    #write <sub.f> "%X"
    #write <sub.f> "*"
    #write <sub.f> "      RETURN"
    #write <sub.f> "      END"
    ExtraSymbols,underscore,Z;
    Format Normal;
    Format 80;
    Print;
    .sort

    FromPolynomial;
    Print;
    .end
    assert succeeded?
    if !threaded?
      # In TFORM, the output can differ.
      assert stdout =~ exact_pattern(<<'EOF')
   F =
      x^-2 + x^-1 + x + x^2 + f(x1) + f(x2)*Dot(p,q)*x + f(x2)*InvDot(p,q)^2;

    
        ExtraSymbols,array,Y;
        Format DOUBLEFORTRAN;
        ToPolynomial;
        Print;
        .sort

      F =
     & Y(1) + Y(1)**2 + Y(2) + Y(5)**2*Y(3) + x + x*Y(4)*Y(3) + x**2

    
        #write <sub.f> "      SUBROUTINE sub(Y)"
        #write <sub.f> "*"
        #write <sub.f> "*      Compute the extra symbols."
        #write <sub.f> "*"
        #write <sub.f> "      REAL*8 Y(`EXTRASYMBOLS_')"
        #write <sub.f> "      REAL*8 Dot,InvDot"
        #write <sub.f> "      Dot(p1,p2)=p1(1)*p2(1)-p1(2)*p2(2)-p1(3)*p2(3)\
                                                                  -p1(4)*p2(4)"
        #write <sub.f> "      InvDot(p1,p2)=1.D0/(Dot(p1,p2))"
        #write <sub.f> "*"
        #write <sub.f> "*        We still have to add definitions here."
        #write <sub.f> "*        And we have to import all the variables."
        #write <sub.f> "*"
        #write <sub.f> "%X"
        #write <sub.f> "*"
        #write <sub.f> "      RETURN"
        #write <sub.f> "      END"
        ExtraSymbols,underscore,Z;
        Format Normal;
        Format 80;
        Print;
        .sort

   F =
      Z1_ + Z1_^2 + Z2_ + Z5_^2*Z3_ + x + x*Z4_*Z3_ + x^2;

    
        FromPolynomial;
        Print;
        .end

   F =
      x^-2 + x^-1 + x + x^2 + f(x1) + f(x2)*Dot(p,q)*x + f(x2)*InvDot(p,q)^2;
EOF
      assert file("sub.f") == <<-'EOF'
      SUBROUTINE sub(Y)
*
*      Compute the extra symbols.
*
      REAL*8 Y(5)
      REAL*8 Dot,InvDot
      Dot(p1,p2)=p1(1)*p2(1)-p1(2)*p2(2)-p1(3)*p2(3)-p1(4)*p2(4)
      InvDot(p1,p2)=1.D0/(Dot(p1,p2))
*
*        We still have to add definitions here.
*        And we have to import all the variables.
*
      Y(1)=x**(-1)
      Y(2)=f(x1)
      Y(3)=f(x2)
      Y(4)=Dot(p,q)
      Y(5)=InvDot(p,q)

*
      RETURN
      END
EOF
    end
*--#] Var_Extra_Symbols_1 : 
*--#[ Pre_call_1 :
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
	assert succeeded?
	assert stdout =~ /#message This is b: `b'\n~~~This is b: c2\n/
	assert stdout =~ /#call hop\(`b`!b''`!b'`b'`!b'`b',`~a',`b',`a'\)\n~~~This is the call: xc2c3c2c3,3,c3,2\n/
*--#] Pre_call_1 : 
*--#[ Pre_define_1 :
    #define c "3"
    #define var1(a,b) "(`~a'+`~b'+`c')"
    #define var2(a,b) "(`~a'+`~b'+`~c')"
    #redefine c "4"
    Local F1 = `var1(1,2)';
    Local F2 = `var2(1,2)';
    Print;
    .end
	assert succeeded?
	assert result("F1") =~ expr("6")
	assert result("F2") =~ expr("7")
*--#] Pre_define_1 : 
*--#[ Pre_preout_1 :
    #PreOut ON
    S   a1,...,a4;
    L   F = (a1+...+a4)^2;
    id  a4 = -a1;
    .end
	assert succeeded?
	assert stdout =~ exact_pattern(<<-EOF
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
*--#] Pre_preout_1 : 
*--#[ Pre_write_1 :
    Symbols a,b;
    L   F = a+b;
    #$a1 = a+b;
    #$a2 = (a+b)^2;
    #$a3 = $a1^3;
    #write " One power: %$\\n Two powers: %$\\n Three powers: %$\n%s"\
           ,$a1,$a2,$a3," The end"
    .end
	assert succeeded?
	assert stdout =~ exact_pattern(<<-EOF
 One power: b+a
 Two powers: b^2+2*a*b+a^2
 Three powers: b^3+3*a*b^2+3*a^2*b+a^3
 The end
        .end
		EOF
		)
*--#] Pre_write_1 : 
*--#[ Pre_write_2 :
* TODO: change the result in the manual.
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
	assert succeeded?
	assert file("fun.f") == <<-EOF
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
*--#] Pre_write_2 : 
*--#[ DolVars_1 :
    S	x,a,b;
    Off statistics;
    L	F = (a+b)^4+a*(a+x)^3;
    .sort
    #$a = 0;
    if ( count(x,1) > $a ) $a = count_(x,1);
    Print "      >> After %t the maximum power of x is %$",$a;
    ModuleOption noparallel; * suppresses noparallel warning
    #write "     ># $a = `$a'"
    .sort
    #write "     ># $a = `$a'"
    .end
	assert succeeded?
	assert stdout =~ Regexp.new(<<-EOF
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
*--#] DolVars_1 : 
*--#[ DolVarsParallel_1 :
    S   a1,...,a10;
    L   F = (a1+...+a10)^3;
    .sort
    #$c = 0;
    Print +f "<%w> %t";
    Multiply,(a1+...+a10);
    $c = $c+1;
    ModuleOption,sum,$c;
    .sort
    #message $c = `$c'
    #$max = 0;
    #$min = 10;
    if ( count(a1,1) > $max ) $max = count_(a1,1);
    if ( count(a4,1) < $min ) $min = count_(a4,1);
    ModuleOption,maximum,$max;
    ModuleOption,minimum,$min;
    .sort
    #message $max = `$max'
    #message $min = `$min'
    .end
	assert succeeded?
	if serial?
		assert stdout =~ /\s+\.sort\n(<>\ \ \+\ \S+\n){220}\n/
	else
		assert stdout =~ /\s+\.sort\n(<\d+>\ \ \+\ \S+\n){220}\n/
	end
	assert stdout =~ /~~~\$c = 2200/
	assert stdout =~ /~~~\$max = 4/
	assert stdout =~ /~~~\$min = 0/
*--#] DolVarsParallel_1 : 
*--#[ Sta_ArgImplode_1 :
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
	assert succeeded?
	assert result("s") =~ expr("0")
*--#] Sta_ArgImplode_1 : 
*--#[ Sta_Collect_1 :
* TODO: change the result in the manual.
S a,b,c;
CF cfun;
L  F =
      a*(b^2+c)
    + a^2*(b+6)
    + b^3 + c*b + 12;
B a;
.sort
Collect cfun;
P;
.end
assert succeeded?
assert result("F") =~ expr("
      cfun(6 + b)*a^2 + cfun(12 + b*c + b^3) + cfun(c + b^2)*a
")
*--#] Sta_Collect_1 : 
*--#[ Sta_CommuteInSet_1 :
    I   i1,...,i10;
    F   A1,...,A10;
    CommuteInSet{A1,A3,A5},{A1,g_},{A1,A1};
    L   F = A5*A1*A5*A1*A5*A2*A3*A5*A1*A5*A3*A1;
    L   G = g_(2,i1)*g_(2,i2,i3)*A1(i2)*g_(1,i4)*g_(1,5_,i5,i6)
                    *A1(i1)*A1(i3)*g5_(1)*A3(i5)*A3(i4)*g5_(1);
    Print +f +s;
    .end
assert succeeded?
assert result("F") =~ expr("
       + A1*A1*A5*A5*A5*A2*A1*A1*A3*A3*A5*A5
")
assert result("G") =~ expr("
       + g_(1,i4,i5,i6)*g_(2,i1,i2,i3)*A1(i1)*A1(i2)*A1(i3)*
       A3(i5)*A3(i4)*g_(1,5_)
")
*--#] Sta_CommuteInSet_1 : 
*--#[ Sta_FactArg_1 :
*TODO: OldFactArg is needed for the result in the manual.
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
	assert succeeded?
	assert result("F") =~ expr("
          f(a,b,-1,3) + f(a,b,3) + 2*f1(a*b) + f2(a*b,-1,3) + f2(a*b,3)
          + f3(a*b,-3) + f3(a*b,3)
        ")
*--#] Sta_FactArg_1 : 
*--#[ Sta_Fill_1 :
    Table B(1:1);
    Local dummy = 1;
    .sort
    Fill B(1) = dummy;
    Drop dummy;
    .sort
* The next line is disabled to prevent a segmentation fault.
*   Local F = B(1);
    Print;
    .end
#pend_if windows?
    assert finished?
    assert warning?
*--#] Sta_Fill_1 : 
*--#[ Sta_Fill_2 :
    Table B(1:1);
    Local dummy = 1;
    .sort
    Fill B(1) = dummy;
    .sort
    Local F = B(1);
    Print;
    .sort
    Drop;
    .sort
    Local dummy = 2;
    .sort
    Local F = B(1);
    Print;
    .end
# RHS expressions in Fill doesn't work in ParFORM. (#17)
# Anyway, the user is warned even in the sequential FORM, and should avoid it.
#pend_if mpi?
    assert finished?
    assert warning?
    assert return_value == 0
    assert result("F", 0) =~ expr("1")
    assert result("F", 1) =~ expr("2")
*--#] Sta_Fill_2 : 
*--#[ Sta_Fill_3 :
    Table B(1:1);
    Local dummy = 1;
    .sort
    #$value = dummy;
    Fill B(1) = `$value';
    Drop dummy;
    .sort
    Local F = B(1);
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("1")
*--#] Sta_Fill_3 : 
*--#[ Sta_Fill_4 :
    Table B(1:1);
    Local u = 2;
    Local dummy = 1;
    .sort
    Fill B(1) = dummy;
    Drop dummy;
    .sort
    Local v = 5;
    Local F = B(1);
    Print;
    .end
# RHS expressions in Fill doesn't work in ParFORM. (#17)
# Anyway, the user is warned even in the sequential FORM, and should avoid it.
#pend_if mpi?
    assert finished?
    assert warning?
    assert return_value == 0
    assert result("F") =~ expr("5")
*--#] Sta_Fill_4 : 
*--#[ Sta_Identify_1 :
    Vector Q,p1,...,p5,q1,...,q5;
    Cfunction V(s),replace;
    Format 60;
*   This is a t1 topology:
    L   F = V(Q,p1,p4)*V(p1,p2,p5)*
            V(p2,p3,Q)*V(p3,p4,p5);
    $t = term_;
    id,all,$t*replace_(<p1,p1?>,...,<p5,p5?>) =
         $t*replace(<p1,q1>,...,<p5,q5>);
    Print +s;
    ModuleOption local $t; * allow parallel execution
    .end
    assert succeeded?
    assert result("F") =~ expr("
       + V(Q,p1,p4)*V(Q,p2,p3)*V(p1,p2,p5)*V(p3,p4,p5)*
      replace(p1,q1,p2,q2,p3,q3,p4,q4,p5,q5)
       + V(Q,p1,p4)*V(Q,p2,p3)*V(p1,p2,p5)*V(p3,p4,p5)*
      replace(p2,q1,p1,q2,p4,q3,p3,q4,p5,q5)
       + V(Q,p1,p4)*V(Q,p2,p3)*V(p1,p2,p5)*V(p3,p4,p5)*
      replace(p3,q1,p4,q2,p1,q3,p2,q4,p5,q5)
       + V(Q,p1,p4)*V(Q,p2,p3)*V(p1,p2,p5)*V(p3,p4,p5)*
      replace(p4,q1,p3,q2,p2,q3,p1,q4,p5,q5)
    ")
*--#] Sta_Identify_1 : 
*--#[ Sta_Keep_1 :
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
	assert succeeded?
	assert result("F") =~ expr("f(N1_?,x)*g(i1,y)+f(N1_?,x)*g(i1,z)")
*--#] Sta_Keep_1 : 
*--#[ Sta_LFactorized_1 :
    Symbols x,y,z;
    LocalFactorized F1 = 3*(x+y)*(y+z)*((x+z)*(2*x+1));
    LocalFactorized F2 = 3*(x+y)*(y+z)+((x+z)*(2*x+1));
    Print;
    .end
    assert succeeded?
    assert result("F1") =~ expr("
         ( 3 )
       * ( y + x )
       * ( z + y )
       * ( z + x + 2*x*z + 2*x^2 )
    ")
    assert result("F2") =~ expr("
         ( z + 3*y*z + 3*y^2 + x + 5*x*z + 3*x*y + 2*x^2 )
    ")
*--#] Sta_LFactorized_1 : 
*--#[ Sta_MakeInteger_1 :
    S   a,b,c;
    CF  f;
    L   F = f(22/3*a+14/5*b+18/7*c);
    MakeInteger,f;
    Print +f;
    .end
	assert succeeded?
	assert result("F") =~ expr("2/105*f(135*c + 147*b + 385*a)")
*--#] Sta_MakeInteger_1 : 
*--#[ Sta_PolyFun_1 :
	Symbol x,y;
	CF acc;
    PolyFun acc;
    Local F = 3*x^2*acc(1+y+y^2)+2*x^2*acc(1-y+y^2);
	Print;
	.end
	assert succeeded?
	assert result("F") =~ expr("x^2 * acc(5 + y + 5*y^2)")
*--#] Sta_PolyFun_1 : 
*--#[ Sta_PolyRatFun_1 :
    S x,y;
    CF acc;
    PolyRatFun acc;
    Local F = 3*x^2*acc(1+y+y^2,1-y)+2*x^2*acc(1-y+y^2,1+y);
    P;
    .end
    assert succeeded?
    assert result("F") =~ expr("x^2*acc(-y^3-10*y^2-2*y-5,y^2-1)")
*--#] Sta_PolyRatFun_1 : 
*--#[ Sta_Print_1 :
    Symbols a,b,c;
    Local F = 3*a+2*b;
    Print "> %T";
    id  a = b+c;
    Print ">> %t";
    Print;
    .end
	assert succeeded?
	assert stdout =~ exact_pattern(<<-'EOF'
> 3*a
>>  + 3*b
>>  + 3*c
> 2*b
>>  + 2*b
	EOF
	)
	assert result("F") =~ expr("3*c + 5*b")
*--#] Sta_Print_1 : 
*--#[ Sta_ReplaceLoop_1 :
*TODO: change the result in the manual.
    Functions f(antisymmetric),ff(cyclesymmetric);
    Indices i1,...,i8;
    Local F = f(i1,i4,i2)*f(i5,i2,i3)*f(i3,i1,i6)*f(i4,i7,i8);
    ReplaceLoop f,arg=3,loop=3,out=ff;
    P;
    .end
    assert succeeded?
    assert result("F") =~ expr("- ff(i4,i5,i6)*f(i4,i7,i8)")
*--#] Sta_ReplaceLoop_1 : 
*--#[ Sta_ReplaceLoop_2 :
*TODO: change the result in the manual.
    Functions f(antisymmetric),ff(cyclesymmetric);
    Indices i1,...,i9;
    Local F = f(i1,i4,i2)*f(i5,i2,i3)*f(i3,i1,i6)*f(i4,i7,i8)
            *f(i6,i7,i8);
    ReplaceLoop f,arg=3,loop=all,out=ff;
    P;
    .end
    assert succeeded?
    assert result("F") =~ expr("- f(i1,i2,i4)*f(i2,i3,i5)*f(i1,i3,i6)*ff(i4,i6)")
*--#] Sta_ReplaceLoop_2 : 
*--#[ Sta_Shuffle_1 :
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
	assert succeeded?
	assert result("F1") =~ expr("f(a,b,c,d)+f(a,c,b,d)+f(a,c,d,b)+f(c,a,b,d)+f(c,a,d,b)+f(c,d,a,b)")
	assert result("F2") =~ expr("g(a,b,c,d)+g(a,c,b,d)+g(a,c,d,b)+g(c,a,b,d)+g(c,a,d,b)+g(c,d,a,b)")
*--#] Sta_Shuffle_1 : 
*--#[ Sta_Stuffle_1 :
    CF  S,R;
    Symbols N,n;
    L   F = S(R(1,-3),N)*S(R(-5,1),N);
    id  S(R(?a),n?)*S(R(?b),n?) = S(?a)*S(?b)*R(n);
    Stuffle,S-;
    id  S(?a)*R(n?) = S(R(?a),n);
    Print +s;
    .end
	assert succeeded?
	assert result("F") =~ expr(<<-'EOF'
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
	  EOF
	)
*--#] Sta_Stuffle_1 : 
*--#[ Sta_ToTensor_1 :
*NOTE: "functions" option is needed.
V p,p1,p2;
F f;
I mu;
T tt,t;
L F = p.p1^2*f(p,p1)*p(mu)*tt(p1,p,p2,p);
totensor functions,p,t;
P;
.end
assert succeeded?
assert result("F") =~ expr("
  f(N1_?,p1)*tt(p1,N2_?,p2,N3_?)*t(p1,p1,mu,N1_?,N2_?,N3_?)
")
*--#] Sta_ToTensor_1 : 
*--#[ Sta_Transform_1 :
    Symbol x,x1,x2;
    CF  H,H1;
    Off Statistics;
    L   F = H(3,4,2,6,1,1,1,2);
    Transform,H,explode(1,last),
                replace(1,last)=(0,1,1,0),
                encode(1,last):base=2;
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("H(907202)")
*--#] Sta_Transform_1 : 
*--#[ Fun_distrib_1 :
    Symbols x1,...,x4;
    CFunctions f,f1,f2;
    Local F = f(x1,...,x4);
    id  f(?a) = distrib_(-1,2,f1,f2,?a);
    Print +s;
    .end
	assert succeeded?
	assert result("F") =~ expr("
       + f1(x1,x2)*f2(x3,x4)
       - f1(x1,x3)*f2(x2,x4)
       + f1(x1,x4)*f2(x2,x3)
       + f1(x2,x3)*f2(x1,x4)
       - f1(x2,x4)*f2(x1,x3)
       + f1(x3,x4)*f2(x1,x2)
	")
*--#] Fun_distrib_1 : 
*--#[ Fun_exteuclidean_1 :
    Symbols x1,x2,x3,x4;
    Local F = exteuclidean_(54,84);
    Print;
    .sort
    id exteuclidean_(x1?,x2?,x3?,x4?) = x1*x3+x2*x4;
    Print;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("exteuclidean_(54,84,-3,2)")
    assert result("F", 1) =~ expr("6")
*--#] Fun_exteuclidean_1 : 
*--#[ Fun_exteuclidean_2 :
    Symbols x1,x2,x3,x4,a,b;
    Local F = exteuclidean_(97,101);
    Print;
    .sort
    id exteuclidean_(x1?,x2?,x3?,x4?) = x1*x3+x2*x4
        +a*mod2_(1/97,101)+b*mod2_(1/101,97);
    Print;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("exteuclidean_(97,101,25,-24)")
    assert result("F", 1) =~ expr("1 - 24*b + 25*a")
*--#] Fun_exteuclidean_2 : 
*--#[ Fun_makerational_1 :
    #$m = prime_(1);
    #write <> "The prime number is %$",$m
    L   F = MakeRational_(12345678,$m);
    Print;
    .sort
    Modulus `$m';
    Print;
    .end
    assert succeeded?
    if wordsize == 2
      assert stdout =~ /The prime number is 32719/
      assert result("F", 0) =~ expr("127/37")
      assert result("F", 1) =~ expr("10615")
    elsif wordsize == 4
      assert stdout =~ /The prime number is 2147483587/
      assert result("F", 0) =~ expr("9719/38790")
      assert result("F", 1) =~ expr("12345678")
    end
*--#] Fun_makerational_1 : 
*--#[ Fun_perm_1 :
    CFunction f;
    Symbols x1,...,x3;
    Local F = perm_(f,x1,x2,x3);
    Print +s;
    .end
    assert succeeded?
    assert result("F") =~ expr("""
       + f(x1,x2,x3)
       + f(x1,x3,x2)
       + f(x2,x1,x3)
       + f(x2,x3,x1)
       + f(x3,x1,x2)
       + f(x3,x2,x1)
    """)
*--#] Fun_perm_1 : 
*--#[ Fun_prime_1 :
    Symbols x1,x2,x3,x4;
    ON highfirst;
    Local F = x1*prime_(1)+x2*prime_(2)
             +x3*prime_(3)+x4*prime_(4);
    Print;
    .end
    assert succeeded?
    if wordsize == 2
      assert result("F") =~ expr("32719*x1 + 32717*x2 + 32713*x3 + 32707*x4")
    elsif wordsize == 4
      assert result("F") =~ expr("2147483587*x1 + 2147483579*x2 + 2147483563*x3 + 2147483549*x4")
    end
*--#] Fun_prime_1 : 
*--#[ Fun_putfirst_1 :
    S   a,a1,...,a10;
    CF  f,g;
    L   F = g(a,a1,...,a10);
    id  g(?a) = putfirst_(f,4,?a);
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("
      f(a3,a,a1,a2,a4,a5,a6,a7,a8,a9,a10)
    ")
*--#] Fun_putfirst_1 : 
*--#[ Fun_ranperm_1 :
    Function f;
    Symbols x1,...,x5;
    Local F = ranperm_(f,1,2,3,4,5,6)
             +ranperm_(f,x1,x2,x3+x1,x4,x5);
    Print +s;
    .end
    assert succeeded?
    # We can't predict the results!
*--#] Fun_ranperm_1 : 
*--#[ Fun_sump_1 :
    Symbol i,x;
    Local F = sump_(i,0,5,x/i);
    Print;
    .end
	assert succeeded?
	assert result("F") =~ expr("1 + x + 1/2*x^2 + 1/6*x^3 + 1/24*x^4 + 1/120*x^5")
*--#] Fun_sump_1 : 
*--#[ Brackets_1 :
    Symbols a,b,c,x;
    L  F = a*x^2+b*x+c;
    B x;
    .sort
    L  Discriminant = F[x]^2-4*F[x^2]*F[1];
    Print;
    .end
	assert succeeded?
	assert result("Discriminant") =~ pattern("b^2-4*a*c");
*--#] Brackets_1 : 
*--#[ PolyandFact_1 :
    Symbol x,y;
    CFunction rat;
    PolyRatFun rat;
    L   F = rat(x+y,x-y)+rat(x-y,x+y);
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("rat(2*x^2 + 2*y^2,x^2 - y^2)")
*--#] PolyandFact_1 : 
*--#[ PolyandFact_2 :
* TODO: change the result in the manual.
    Symbol x,y;
    CFunction f1,f2;
    Local F = f1(x^4-y^4)+f2(3*y^4-3*x^4);
    FactArg,f1,f2;
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("f1(-1,y - x,y + x,y^2 + x^2) + f2(3,y - x,y + x,y^2 + x^2)")
*--#] PolyandFact_2 : 
*--#[ PolyandFact_3 :
* TODO: change the the first result in the manual.
    Symbol x,y;
    CFunction f1,f2;
    Local F = f2(3*y^4-3*x^4);
    FactArg,f2;
    Print;
    .sort
    ChainOut,f2;
    id f2(x?number_) = x;
    Print;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("f2(3,y - x,y + x,y^2 + x^2)")
    assert result("F", 1) =~ expr("3*f2(y-x)*f2(y+x)*f2(y^2+x^2)")
*--#] PolyandFact_3 : 
*--#[ PolyandFact_4 :
    Symbol x,y;
    Local F = x^4-y^4;
    Print;
    .sort
    Print;
    Factorize F;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("-y^4+x^4")
    assert result("F", 1) =~ expr("(-1)*(y-x)*(y+x)*(y^2+x^2)")
*--#] PolyandFact_4 : 
*--#[ PolyandFact_5 :
* TODO: change the the result of F2 in the manual.
    Symbol x,y;
    Local F1 = x^4-y^4;
    Local F2 = 0;
    Local F3 = 1;
    Local F4 = x^4-y^4;
    Print;
    Factorize F1,F2,F3;
    .sort
    #do i = 1,4
    #$n`i' = numfactors_(F`i');
    #message expression F`i' has `$n`i'' factors
    #enddo
    .end
    assert succeeded?
    assert result("F1") =~ expr("(-1)*(y-x)*(y+x)*(y^2+x^2)")
    assert result("F2") =~ expr("(0)")
    assert result("F3") =~ expr("(1)")
    assert result("F4") =~ expr("-y^4+x^4")
    assert stdout =~ /~~~expression F1 has 4 factors/
    assert stdout =~ /~~~expression F2 has 1 factors/
    assert stdout =~ /~~~expression F3 has 1 factors/
    assert stdout =~ /~~~expression F4 has 0 factors/
*--#] PolyandFact_5 : 
*--#[ PolyandFact_6 :
    Symbol x,y;
    Local F = x^4-y^4;
    Factorize F;
    .sort
    #$n = numfactors_(F);
    #do i = 1,`$n'
    Local F`i' = F[factor_^`i'];
    #enddo
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("(-1)*(y-x)*(y+x)*(y^2+x^2)")
    assert result("F1") =~ expr("-1")
    assert result("F2") =~ expr("y-x")
    assert result("F3") =~ expr("y+x")
    assert result("F4") =~ expr("y^2+x^2")
*--#] PolyandFact_6 : 
*--#[ PolyandFact_7 :
    Symbol x,y;
    LocalFactorize E = -(x+1)*(x+2)*((x+3)*(x+4));
    Print;
    .end
    assert succeeded?
    assert result("E") =~ expr("
         (  - 1 )
       * ( 1 + x )
       * ( 2 + x )
       * ( 12 + 7*x + x^2 )
    ")
*--#] PolyandFact_7 : 
*--#[ PolyandFact_8 :
    Symbol x,y;
    LocalFactorize E = -(x+1)*(x+2)*((x+3)*(x+4));
    Local   F = -(x+1)*(x+2)*((x+3)*(x+4));
    Print;
    .sort
    LF G = (x-1)*(x+2)^2*E^2*F^2;
    Print G;
    .end
    assert succeeded?
    assert result("E") =~ expr("
       (-1)
      *(1+x)
      *(2+x)
      *(12+7*x+x^2)
    ")
    assert result("F") =~ expr("
       -24-50*x-35*x^2-10*x^3-x^4
    ")
    assert result("G") =~ expr("
       (-1+x)
      *(2+x)
      *(2+x)
      *(-1)
      *(1+x)
      *(2+x)
      *(12+7*x+x^2)
      *(-1)
      *(1+x)
      *(2+x)
      *(12+7*x+x^2)
      *(-24-50*x-35*x^2-10*x^3-x^4)
      *(-24-50*x-35*x^2-10*x^3-x^4)
    ")
*--#] PolyandFact_8 : 
*--#[ PolyandFact_9 :
    Symbol x,y;
    LocalFactorize E = -(x+1)*(x+2)*((x+3)*(x+4));
    Local   F = -(x+1)*(x+2)*((x+3)*(x+4));
    .sort
    LF G = (x-1)*(x+2)^2*E^2*F^2;
    Print G;
    Factorize G;
    .end
    assert succeeded?
    assert result("G") =~ expr("
       (-1+x)
      *(1+x)
      *(1+x)
      *(1+x)
      *(1+x)
      *(2+x)
      *(2+x)
      *(2+x)
      *(2+x)
      *(2+x)
      *(2+x)
      *(3+x)
      *(3+x)
      *(3+x)
      *(3+x)
      *(4+x)
      *(4+x)
      *(4+x)
      *(4+x)
    ")
*--#] PolyandFact_9 : 
*--#[ PolyandFact_10 :
    Symbol x,y;
    LocalFactorize E = -0*(x+1)*(x+2)*0*((x+3)*(x+4));
    Print;
    .end
    assert succeeded?
    assert result("E") =~ expr("
       (-1)
      *(0)
      *(1+x)
      *(2+x)
      *(0)
      *(12+7*x+x^2)
    ")
*--#] PolyandFact_10 : 
*--#[ PolyandFact_11 :
    Symbol x,y;
    Format Nospaces;
    LocalFactorize E = -0*3*(x+1)*(x+2)/2*0*((x+3)*(x+4));
    Print;
    .sort
    Print;
    Factorize(keepzero) E;
    .end
    assert succeeded?
    assert result("E", 0) =~ expr("
       (-1)
      *(0)
      *(3)
      *(1+x)
      *(2+x)
      *(1/2)
      *(0)
      *(12+7*x+x^2)
    ")
    assert result("E", 1) =~ expr("
       (0)
      *(-3/2)
      *(1+x)
      *(2+x)
      *(3+x)
      *(4+x)
    ")
*--#] PolyandFact_11 : 
*--#[ PolyandFact_12 :
    Symbol x,y;
    LFactorized F = (x+1)*(x+y)*(y+1);
    Print;
    .sort
    Print;
    Bracket x;
    UnFactorize F;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("
       (1+x)
      *(y+x)
      *(1+y)
    ")
    assert result("F", 1) =~ expr("
      +x*(1+2*y+y^2)
      +x^2*(1+y)
      +y+y^2
    ")
*--#] PolyandFact_12 : 
*--#[ PolyandFact_13 :
    Symbol x,y;
    LFactorized F = (x+1)*(x+y)*(y+1);
    Print;
    .sort
    #$num = numfactors_(F);
    Local   G = <F[factor_^1]>*...*<F[factor_^`$num']>;
    Bracket x;
    Print;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("
       (1+x)
      *(y+x)
      *(1+y)
    ")
    assert result("F", 1) =~ expr("
       (1+x)
      *(y+x)
      *(1+y)
    ")
    assert result("G") =~ expr("
      +x*(1+2*y+y^2)
      +x^2*(1+y)
      +y+y^2
    ")
*--#] PolyandFact_13 : 
*--#[ PolyandFact_14 :
    Symbol x,y;
    CFunction f;
    Off Statistics;
    #$a = x^4-y^4;
    Local F = f(x^4-y^4)+f(x^6-y^6);
    Print;
    .sort
    #factdollar $a;
    #do i = 1,`$a[0]'
    #write <> "Factor `i' of `$a' is `$a[`i']'"
    #enddo
    id  f(x?$b) = f(x);
    FactDollar $b;
    do $i = 1,$b[0];
      Print "Factor %$ of %$ is %$",$i,$b,$b[$i];
    enddo;
    Print;
    ModuleOption noparallel; * suppresses noparallel warning
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("
      f(-y^4+x^4)+f(-y^6+x^6)
    ")
    assert result("F", 1) =~ expr("
      f(-y^4+x^4)+f(-y^6+x^6)
    ")
    assert stdout =~ exact_pattern("
Factor 1 of -y^4+x^4 is -1
        #enddo
Factor 2 of -y^4+x^4 is y-x
Factor 3 of -y^4+x^4 is y+x
Factor 4 of -y^4+x^4 is y^2+x^2
        id  f(x?$b) = f(x);
")
    assert stdout =~ exact_pattern("
Factor 1 of  - y^4 + x^4 is  - 1
Factor 2 of  - y^4 + x^4 is y - x
Factor 3 of  - y^4 + x^4 is y + x
Factor 4 of  - y^4 + x^4 is y^2 + x^2
Factor 1 of  - y^6 + x^6 is  - 1
Factor 2 of  - y^6 + x^6 is y - x
Factor 3 of  - y^6 + x^6 is y + x
Factor 4 of  - y^6 + x^6 is y^2 - x*y + x^2
Factor 5 of  - y^6 + x^6 is y^2 + x*y + x^2
")
*--#] PolyandFact_14 : 
*--#[ PolyandFact_15 :
    Symbol x,y;
    CFunction f;
    Format Nospaces;
    #$a = x^4-y^4;
    #factdollar $a;
    Local F = f(numfactors_($a))
        +f(<$a[1]>,...,<$a[`$a[0]']>);
    Print;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("
      f(-1,y-x,y+x,y^2+x^2)+f(4)
    ")
*--#] PolyandFact_15 : 
*--#[ PolyandFact_16 :
    Symbol x,y;
    Format NoSpaces;
    On ShortStats;
    Local F1 = x^60-1;
    Local F2 = y^60-x^60;
    Factorize F1,F2;
    Print;
    .end
    if wordsize < 4
      # ERROR: polynomials too large (> WORDSIZE)
    else
      assert succeeded?
      assert result("F1") =~ expr("
         (-1+x)
        *(1-x+x^2)
        *(1-x+x^2-x^3+x^4)
        *(1-x+x^3-x^4+x^5-x^7+x^8)
        *(1+x)
        *(1+x+x^2)
        *(1+x+x^2+x^3+x^4)
        *(1+x-x^3-x^4-x^5+x^7+x^8)
        *(1-x^2+x^4)
        *(1-x^2+x^4-x^6+x^8)
        *(1+x^2)
        *(1+x^2-x^6-x^8-x^10+x^14+x^16)
      ")
      assert result("F2") =~ expr("
         (y-x)
        *(y+x)
        *(y^2-x*y+x^2)
        *(y^4-x*y^3+x^2*y^2-x^3*y+x^4)
        *(y^4+x*y^3+x^2*y^2+x^3*y+x^4)
        *(y^2+x*y+x^2)
        *(y^2+x^2)
        *(y^8-x*y^7+x^3*y^5-x^4*y^4+x^5*y^3-x^7*y+x^8)
        *(y^8+x*y^7-x^3*y^5-x^4*y^4-x^5*y^3+x^7*y+x^8)
        *(y^8-x^2*y^6+x^4*y^4-x^6*y^2+x^8)
        *(y^4-x^2*y^2+x^4)
        *(y^16+x^2*y^14-x^6*y^10-x^8*y^8-x^10*y^6+x^14*y^2+x^16)
      ")
    end
*--#] PolyandFact_16 : 
*--#[ PolyandFact_17 :
    Symbols a,b;
    LF F = (a+b)^2;
    multiply 2;
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("
      ( 2*b + 2*a )
       * ( 2*b + 2*a )
    ")
*--#] PolyandFact_17 : 
*--#[ PolyandFact_18 :
    Symbols a,b;
    LF F = (a+b)^2;
    .sort
    LF F = 2*F;
    Print;
    .end
    assert succeeded?
    assert result("F") =~ expr("
         ( 2 )
       * ( b + a )
       * ( b + a )
    ")
*--#] PolyandFact_18 : 
*--#[ OutputOptimization_1 :
   CF  f;
   S   a,b,c;
   L   H = f(a)+f(b)+(a+b+c)^2;
   L   G = f(c)+(a+b+c)^3;
   Format O2;
   Print +f;
   .sort
   ExtraSymbols,array,w;
   Format Fortran;
   #optimize G
   #write <outg.f> "      REAL*8 w(`optimmaxvar_')"
   #write <outg.f> "%O"
   #write <outg.f> "      G = %e",G
   #clearoptimize
   .sort
   #optimize H
   #write <outh.f> "      REAL*8 w(`optimmaxvar_')"
   #write <outh.f> "%O"
   #write <outh.f> "      H = %e",H
   .end
   assert succeeded?
   if serial?
     # TFORM may optimize the expressions in a different way.
     assert file("outg.f") == <<-'EOF'
      REAL*8 w(4)

      w(1)=f(c)
      w(2)=c**2
      w(3)=3*c + b
      w(3)=b*w(3)
      w(3)=3*w(2) + w(3)
      w(3)=b*w(3)
      w(4)=2*c + b
      w(4)=b*w(4)
      w(2)=w(2) + w(4)
      w(4)=c + b
      w(4)=3*w(4) + a
      w(4)=a*w(4)
      w(2)=3*w(2) + w(4)
      w(2)=a*w(2)
      w(4)=c**3

      G = w(1) + w(2) + w(3) + w(4)
EOF
     assert file("outh.f") == <<-'EOF'
      REAL*8 w(5)

      w(1)=f(a)
      w(2)=f(b)
      w(3)=c**2
      w(4)=2*c + b
      w(4)=b*w(4)
      w(5)=c + b
      w(5)=2*w(5) + a
      w(5)=a*w(5)

      H = w(1) + w(2) + w(3) + w(4) + w(5)
EOF
   end
*--#] OutputOptimization_1 : 
*--#[ Dictionaries_1 :
    Symbols x1,y2,z3,N;
    Indices mu,nu,ro,si;
    Tensor tens;
    CFunction S,R,f;
    ExtraSymbols array w;
    #OpenDictionary test
      #add x1: "x_1"
      #add y2: "y^{(2)}"
      #add z3: "{\cal Z}"
      #add *: " "
      #add S(R(1),N): "S_1(N)"
      #add S(R(2),N): "S_2(N)"
      #add S(R(1,1),N): "S_{1,1}(N)"
      #add f: "\ln"
      #add mu: "\mu"
      #add nu: "\nu"
      #add ro: "\rho"
      #add si: "\sigma"
      #add tens: "T"
    #CloseDictionary
    Local F = x1*y2*z3
         + S(R(1),N) + S(R(1,1),N) + S(R(2),N)
         + tens(mu,nu,ro,si) + f(x1+1);
    #usedictionary test
    Print +s;
    .end
    assert succeeded?
    assert result("F") =~ expr("
       + x_1 y^2 {\\cal Z}
       + T(\\mu,\\nu,\\rho,\\sigma)
       + S_1(N)
       + S_{1,1}(N)
       + S_2(N)
       + \\ln(1 + x_1)
    ")
*--#] Dictionaries_1 : 
*--#[ Dictionaries_2 :
    Symbol x,n;
    Format DoubleFortran;
    #OpenDictionary numbers
      #add 2: "TWO"
      #add 5: "FIVE"
      #add 7: "SEVEN"
    #CloseDictionary
    Local F = (1+x)^7/7;
    id  x^n? = x*x^n/(n+1);
    #UseDictionary numbers
    Print;
    .end
    assert succeeded?
    assert stdout =~ exact_pattern("
      F =
     & 1/SEVEN*x + 1/TWO*x**2 + x**3 + FIVE/4*x**4 + x**5 + 1/TWO*x**6
     &  + 1/SEVEN*x**7 + 1.D0/56.D0*x**8
")
*--#] Dictionaries_2 : 
*--#[ Dictionaries_3 :
    Symbol x,n;
    Format DoubleFortran;
    #OpenDictionary numbers
      #add 2: "TWO"
      #add 5: "FIVE"
      #add 7: "SEVEN"
      #add 1/2: "HALF"
    #CloseDictionary
    Local F = (1+x)^7/7;
    id  x^n? = x*x^n/(n+1);
    #UseDictionary numbers
    Print;
    .end
    assert succeeded?
    assert stdout =~ exact_pattern("
      F =
     & 1/SEVEN*x + HALF*x**2 + x**3 + FIVE/4*x**4 + x**5 + HALF*x**6 + 
     & 1/SEVEN*x**7 + 1.D0/56.D0*x**8
")
*--#] Dictionaries_3 : 
*--#[ Dictionaries_4 :
    Symbol x,n;
    Format DoubleFortran;
    #OpenDictionary numbers
      #add 2: "TWO"
      #add 5: "FIVE"
      #add 7: "SEVEN"
      #add 1/2: "HALF"
    #CloseDictionary
    Local F = (1+x)^7/7;
    id  x^n? = x*x^n/(n+1);
    #UseDictionary numbers (warnings)
    Print;
    .end
    assert succeeded?
    assert stdout =~ exact_pattern("
      F =
     & 1/SEVEN*x + HALF*x**2 + x**3 + FIVE/4*x**4 + x**5 + HALF*x**6 + 
>>>>>>>>Could not translate coefficient with dictionary numbers<<<<<<<<<
<<<
     & 1/SEVEN*x**7 + 1.D0/56.D0*x**8
")
*--#] Dictionaries_4 : 
*--#[ Dictionaries_5 :
    Symbol x,n;
    Format DoubleFortran;
    #OpenDictionary numbers
      #add 2: "cd2"
      #add 5: "cd5"
      #add 7: "cd7"
      #add 56: "cd56"
      #add 1/2: "c1d2"
      #add 5/4: "c5d4"
    #CloseDictionary
    Local F = (1+x)^7/7;
    id  x^n? = x*x^n/(n+1);
    #UseDictionary numbers (warnings)
    Print;
    .end
    assert succeeded?
    assert stdout =~ exact_pattern("
      F =
     & 1/cd7*x + c1d2*x**2 + x**3 + c5d4*x**4 + x**5 + c1d2*x**6 + 1/
     & cd7*x**7 + 1/cd56*x**8
")
*--#] Dictionaries_5 : 
*--#[ Dictionaries_6 :
    Symbol x;
    CFunction f;
    #OpenDictionary ranges
      #add (1,2): "w(%#)"
      #add (3): "ww(%#)"
      #add (4,6): "www(%@)"
    #CloseDictionary
    Local F = <f(1)*x^1>+...+<f(6)*x^6>;
    ToPolynomial;
    Print;
    .sort
    #UseDictionary ranges
    Print;
    .end
    assert succeeded?
    assert result("F", 0) =~ expr("
      x*Z1_ + x^2*Z2_ + x^3*Z3_ + x^4*Z4_ + x^5*Z5_ + x^6*Z6_
    ")
    assert result("F", 1) =~ expr("
      x*w(1) + x^2*w(2) + x^3*ww(3) + x^4*www(1) + x^5*www(2) + x^6*www(3)
    ")
*--#] Dictionaries_6 : 
*--#[ DiracAlgebra_1 :
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
	assert succeeded?
	assert stdout =~ /Generated terms =      51975$/
	assert stdout =~ /Terms in output =      51975$/
	assert bytesize("F") == 459582 * wordsize
*--#] DiracAlgebra_1 : 
*--#[ DiracAlgebra_2 :
*
*   Regular trace of a gamma5 and 12 regular matrices
*
I   m1,...,m12;
L   F = g_(1,5_,m1,...,m12);
trace4,1;
.end
	assert succeeded?
	assert @stdout =~ /Generated terms =       1053$/
	assert @stdout =~ /Terms in output =       1029$/
	assert bytesize("F") == 10142 * wordsize
*--#] DiracAlgebra_2 : 
*--#[ NotesMetric_1 :
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
	assert succeeded?
	assert result("F",0) =~ expr(<<-EOF
       + eta(i1,i1)*eta(i2,i2)*eta(i3,i3)
       - eta(i1,i1)*eta(i2,i3)^2
       - eta(i1,i2)^2*eta(i3,i3)
       + 2*eta(i1,i2)*eta(i1,i3)*eta(i2,i3)
       - eta(i1,i3)^2*eta(i2,i2)
	  EOF
	)
	assert result("F",1) =~ expr(<<-EOF
       + 6*eta(1,1)*eta(2,2)*eta(3,3)
       - 6*eta(1,1)*eta(2,3)^2
       - 6*eta(1,2)^2*eta(3,3)
       + 12*eta(1,2)*eta(1,3)*eta(2,3)
       - 6*eta(1,3)^2*eta(2,2)
	  EOF
	)
	assert result("F") =~ expr("-6")
*--#] NotesMetric_1 : 
*--#[ NotesMetric_2 :
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
	assert succeeded?
	assert result("F",0) =~ expr("+6*e_(1,2,3)*e_(1,2,3)")
	assert result("F") =~ expr("-6")
*--#] NotesMetric_2 : 
*--#[ NotesMetric_3 :
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
	assert succeeded?
	assert result("F",0) =~ expr(<<-EOF
       + d_(i1,i1)*d_(i2,i2)*d_(i3,i3)
       - d_(i1,i1)*d_(i2,i3)*d_(i2,i3)
       - d_(i1,i2)*d_(i1,i2)*d_(i3,i3)
       + 2*d_(i1,i2)*d_(i1,i3)*d_(i2,i3)
       - d_(i1,i3)*d_(i1,i3)*d_(i2,i2)
	  EOF
	)
	assert result("F") =~ expr("-6")
*--#] NotesMetric_3 : 
*--#[ ExtComm_1 :
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
#require unix?
# This gives Valgrind errors (3 memory leaks) on Travis CI
# (osx-gcc-valgrind-parvorm), but cleanly works on Linux with mpich 3.2.
# Might be an OS- or implementation-specific bug.
# Update (22 Sep 2017): Now I see even for Linux (both on Travis CI and
# a desktop PC) each child process leads to 1 memory leak. Best to skip this
# test for Valgrind.
#pend_if valgrind?
	assert succeeded?
	assert result("aPLUSbTO2") =~ expr("b^2 + 2*a*b + a^2")
	assert result("aPLUSbTO3") =~ expr("b^3 + 3*a*b^2 + 3*a^2*b + a^3")
*--#] ExtComm_1 : 
*--#[ Diagrams_1 :
    Vectors Q1,Q2,p1,...,p8;
    Set QQ:Q1,Q2;
    Set PP:p1,...,p8;
    #define LOOPS "2"
    Local F = topologies_(`LOOPS',2,{3,},QQ,PP);
    Print +f +s;
    .end
# TODO: enable it
#pend_if true
    assert succeeded?
    assert result("F") =~ expr("
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,p1,-p3)*
      node_(4,p2,-p4,-p5)*node_(5,p3,p4,p5)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,p1,-p3,-p4)*
      node_(4,p2,p3,-p5)*node_(5,Q2,p4,p5)
    ")
*--#] Diagrams_1 : 
*--#[ Diagrams_2 :
    Vectors Q1,Q2,p1,...,p8;
    Set QQ:Q1,Q2;
    Set PP:p1,...,p8;
    #define LOOPS "2"
    Local F = topologies_(`LOOPS',2,{3,4},QQ,PP);
    Print +f +s;
    .end
# TODO: enable it
#pend_if true
    assert succeeded?
    assert result("F") =~ expr("
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,Q2,-p1,-p2)*node_(3,p1,p2,-p3,p3
      )
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,p1,-p3)*
      node_(4,p2,p3,-p4,p4)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,p1,-p3)*
      node_(4,p2,-p4,-p5)*node_(5,p3,p4,p5)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,-p3,-p4)*
      node_(4,p1,p2,p3,p4)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,p1,-p3,-p4)*
      node_(4,Q2,p2,p3,p4)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,p1,-p3,-p4)*
      node_(4,p2,p3,-p5)*node_(5,Q2,p4,p5)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2,-p3)*node_(3,Q2,p1,p2,p3
      )
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,-p1,-p2,-p3)*node_(3,Q2,p1,-p4)*
      node_(4,Q1,p2,p3,p4)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,-p1,-p2,-p3)*node_(3,p1,p2,-p4)*
      node_(4,Q1,Q2,p3,p4)
    ")
*--#] Diagrams_2 : 
*--#[ Diagrams_3 :
    Vectors Q1,Q2,p1,...,p8;
    Set QQ:Q1,Q2;
    Set PP:p1,...,p8;
    #define LOOPS "2"
    Local F = topologies_(`LOOPS',-2,{3,4},QQ,PP);
    Print +f +s;
    .end
# TODO: enable it
#pend_if true
    assert succeeded?
    assert result("F") =~ expr("
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,Q2,-p1,-p2)*node_(3,p1,p2,-p3,p3
      )
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,p1,-p3)*
      node_(4,p2,p3,-p4,p4)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,p1,-p3)*
      node_(4,p2,-p4,-p5)*node_(5,p3,p4,p5)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,-p3,-p4)*
      node_(4,p1,p2,p3,p4)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,Q2,-p3,-p4)*
      node_(4,p1,p3,-p5)*node_(5,p2,p4,p5)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2)*node_(3,p1,-p3,-p4)*
      node_(4,Q2,p2,p3,p4)
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,Q1,-p1,-p2,-p3)*node_(3,Q2,p1,p2,p3
      )
       + node_(0,-Q1)*node_(1,-Q2)*node_(2,-p1,-p2,-p3)*node_(3,p1,p2,-p4)*
      node_(4,Q1,Q2,p3,p4)
    ")
*--#] Diagrams_3 : 
*--#[ Diagrams_4 :
    Vectors Q1,Q2,p1,...,p17;
    Set QQ:Q1,Q2;
    Set PP:p1,...,p17;
    #define LOOPS "6"
    Local F = topologies_(`LOOPS',-2,{3,},QQ,PP);
    .end
# TODO: enable it
#pend_if true
    #time_dilation 2.0
    assert succeeded?
    assert nterms("F") == 2793
*--#] Diagrams_4 : 
*--#[ Diagrams_5 :
    #define LOOPS "6"
    Model PHI3;
        Particle phi,+1;
        Vertex phi,phi,phi:g;
    EndModel;
    Vector Q1,Q2,p1,...,p{3*`LOOPS'};
    Set QQ:Q1,Q2;
    Set pp:p1,...,p{3*`LOOPS'};
    Set empty:;
    Local F = diagrams_(PHI3,{phi,phi},empty,QQ,pp,`LOOPS',
            `OnePI_'+`NoTadpoles_'+`Symmetrize_'+`TopologiesOnly_');
    .end
    assert succeeded?
    assert nterms("F") == 2793
*--#] Diagrams_5 : 
*--#[ Diagrams_6 :
*    #define LOOPS "6"
*    Model PHI3;
*        Particle phi,+1;
*        Vertex phi,phi,phi:g;
*    EndModel;
*    Vector Q1,Q2,p1,...,p{3*`LOOPS'};
*    Set QQ:Q1,Q2;
*    Set pp:p1,...,p{3*`LOOPS'};
*    Set empty:;
*    Local F = diagrams_(PHI3,{phi,phi},empty,QQ,pp,`LOOPS',
*            `OnePI_'+`NoTadpoles_'+`TopologiesOnly_');
*    .end
*    assert succeeded?
*    assert nterms("F") == 4999
*--#] Diagrams_6 : 

