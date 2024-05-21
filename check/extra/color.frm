#ifndef `TEST'
  #message Use -D TEST=XXX
  #terminate
#else
  #include `NAME_' # `TEST'
#endif
.end

*--#[ color_tloop :
#-

* This is a modified version of tloop.frm from the color.h example files.

Off Statistics;

* Larger than 5 becomes extremely slow under valgrind.
#define SIZE "5"
*#define ALTERNATEMETHOD "1"
*
*	Test program for the color traces in the paper and a few more.
*	The paper: "Color traces for arbitrary groups"
*	by T. van Ritbergen, A.N. Schellekens and J.A.M. Vermaseren.
*
*	The object is to express color traces in terms of group invariants
*	in such a way that the group has not been specified yet. The few
*	remaining invariants can be substituted afterwards. The expressions
*	in terms of invariants are better for publications. One sees more
*	about the structure of the problem this way.
*
*	All declarations are made in the file cfactors.h
*	One needs version 3 or later of FORM to run these programs.
*	The programs will handle all color structures with up to 14 vertices.
*	One vertex is one generator in any representation.
*	Currently the program handles only one type of non-adjoint representaion.
*	If there are two different representations of such type one has to
*	try to run the problem in stages.
*
*	Examples are:
*	One quark-loop with SIZE gluons in a maximally non-planar configuration
*		(TYPE = qloop)
*	Same but qluon loop (everything in the adjoint representation)
*		(TYPE = gloop)
*	Two quark loops with SIZE connecting gluons like a circular ladder.
*		(TYPE = qqloop)
*	Same with one quark loop and one gluon loop.
*		(TYPE = qgloop)
*	Same with two gluon loops.
*		(TYPE = ggloop)
*	The Coxeter graph (diagram with 14 vertices in the adjoint representation
*	in which there are no loops with fewer than 6 lines) (TYPE = g14)
*
*	One should not choose SIZE larger than 7 or most likely the simplification
*	routine cannot simplify the final result completely (in some rare cases
*	it can though for SIZE = 8).
*	For execution times, see the paper.
*	It does get very slow for quarks and SIZE > 7.
*
*	Program by J.Vermaseren, 24-may-1997
*
#include color.h
AutoDeclare Index i,j,k;
CFunction acc;
Symbol x;
.global
G	Q{2*`SIZE'} = <T(i1,i2,j1)>*...*<T(i`SIZE',i{`SIZE'+1},j`SIZE')>
	*<T(i{`SIZE'+1},i{`SIZE'+2},j1)>*...*<T(i{`SIZE'*2},i{2*`SIZE'+1},j`SIZE')>
	*replace_(i{`SIZE'*2+1},i1);

G	G{2*`SIZE'} = <f(i1,i2,j1)>*...*<f(i`SIZE',i{`SIZE'+1},j`SIZE')>
	*<f(i{`SIZE'+1},i{`SIZE'+2},j1)>*...*<f(i{`SIZE'*2},i{2*`SIZE'+1},j`SIZE')>
	*replace_(i{`SIZE'*2+1},i1);

G	QQ`SIZE' = <T(i1,i2,j1)>*...*<T(i`SIZE',i{`SIZE'+1},j`SIZE')>
	*<T(k1,k2,j1)>*...*<T(k`SIZE',k{`SIZE'+1},j`SIZE')>
	*replace_(i{`SIZE'+1},i1,k{`SIZE'+1},k1);

G	QG`SIZE' = <T(i1,i2,j1)>*...*<T(i`SIZE',i{`SIZE'+1},j`SIZE')>
	*<f(k1,k2,j1)>*...*<f(k`SIZE',k{`SIZE'+1},j`SIZE')>
	*replace_(i{`SIZE'+1},i1,k{`SIZE'+1},k1);

G	GG`SIZE' = <f(i1,i2,j1)>*...*<f(i`SIZE',i{`SIZE'+1},j`SIZE')>
	*<f(k1,k2,j1)>*...*<f(k`SIZE',k{`SIZE'+1},j`SIZE')>
	*replace_(i{`SIZE'+1},i1,k{`SIZE'+1},k1);

* These take too long under valgrind.
*G	girth14 =
*		f(i1,i2,i3)*f(i1,i4,i5)*f(i2,i6,i7)*f(i3,i8,i9)
*		*f(i4,i10,i11)*f(i5,i12,i13)*f(i6,i14,i15)*f(i7,i16,i17)
*		*f(i8,i18,i19)*f(i9,i20,i21)
*		*f(i10,i21,i15)*f(i13,i19,i14)*f(i17,i11,i18)*f(i12,i16,i20);
*
*G	girth24 =
*	f(i36,i1,i2)*f(i36,i3,i4)*f(i1,i5,i6)*f(i2,i7,i8)
*	*f(i3,i9,i10)*f(i4,i11,i12)*f(i5,i13,i14)*f(i6,i15,i16)
*	*f(i7,i17,i18)*f(i8,i19,i20)*f(i9,i21,i22)*f(i10,i23,i24)
*	*f(i11,i25,i26)*f(i12,i27,i28)*f(i13,i23,i29)*f(i14,i27,i30)
*	*f(i15,i25,i31)*f(i16,i21,i32)*f(i17,i26,i29)*f(i18,i32,i33)
*	*f(i19,i31,i34)*f(i20,i22,i30)*f(i24,i34,i35)*f(i28,i33,i35);
*
*G	fiveq = T(i1,i2,j1)*T(i2,i3,j2)*T(i3,i1,j3)*
*		 T(i4,i5,j2)*T(i5,i6,j4)*T(i6,i4,j5)*
*		 T(i7,i8,j4)*T(i8,i9,j6)*T(i9,i7,j7)*
*		 T(i10,i11,j6)*T(i11,i12,j1)*T(i12,i10,j8)*
*		 T(i13,i14,j3)*T(i14,i15,j5)*T(i15,i16,j7)*T(i16,i13,j8);

Sum i1,...,i{`SIZE'*2},j1,...,j`SIZE',k1,...,k`SIZE';
.sort

#call color
#call SORT(tloop-1)
#call adjoint
#call SORT(tloop-2)
.sort
#call simpli
id	acc(x?) = x;
*Print +s;
.sort

* Check results
Local Q10 = - Q10
       + NA*I2R*cR^4
       - 5*NA*I2R*cA*cR^3
       + 35/4*NA*I2R*cA^2*cR^2
       - 155/24*NA*I2R*cA^3*cR
       + 125/72*NA*I2R*cA^4
       + 5*d44(cOlpR1,cOlpA1)*cR
       - 6*d44(cOlpR1,cOlpA1)*cA
       + 1/3*d44(cOlpA1,cOlpA2)*I2R
      ;

Local G10 = - G10
       - 1/36*NA*cA^5
       + 2/3*d44(cOlpA1,cOlpA2)*cA
      ;

Local QQ5 = - QQ5
       - 5/144*NA*I2R^2*cA^3
       + 11/48*d33(cOlpR1,cOlpR2)*cA^2
       - 5/6*d44(cOlpR1,cOlpR2)*cA
       + 1/6*d44(cOlpR1,cOlpA1)*I2R
       + d55(cOlpR1,cOlpR2)
      ;

Local QG5 = - QG5
       - 5/144*i_*NA*I2R*cA^4
       - 3/4*d44(cOlpR1,cOlpA1)*i_*cA
       + 1/12*d44(cOlpA1,cOlpA2)*i_*I2R
      ;

Local GG5 = - GG5
       + 5/144*NA*cA^5
       + 2/3*d44(cOlpA1,cOlpA2)*cA
      ;
.sort

#do ex = {`activeexprnames_'}
	#if `ZERO_`ex'' == 0
		#message Error in `ex'
		#terminate
	#endif
#enddo

.end
assert succeeded?
*--#] color_tloop :
