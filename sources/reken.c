/*
  	#[ Includes : reken.c
*/

#include "form3.h"
 
#define GCDMAX 3

#define NEWTRICK 1
/*
  	#] Includes :
  	#[ RekenRational :
 		#[ Pack :			VOID Pack(a,na,b,nb)

	Packs the contents of the numerator a and the denominator b into
	one normalized fraction a. The return value is the half length.

*/

VOID
Pack ARG4(UWORD *,a,WORD *,na,UWORD *,b,WORD,nb)
{
	WORD c, sgn = 1, i;
	UWORD *to,*from;
	if ( (c = *na) == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Caught a zero in Pack");
		UNLOCK(ErrorMessageLock);
		return;
	}
	if ( nb == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Division by zero in Pack");
		UNLOCK(ErrorMessageLock);
		return;
	}
	if ( *na < 0 ) { sgn = -sgn; c = -c; }
	if ( nb < 0 ) { sgn = -sgn; nb = -nb; }
	*na = MaX(c,nb);
	to = a + c;
	i = *na - c;
	while ( --i >= 0 ) *to++ = 0;
	i = *na - nb;
	from = b;
	NCOPY(to,from,nb);
	while ( --i >= 0 ) *to++ = 0;
	if ( sgn < 0 ) *na = -*na;
}

/*
 		#] Pack : 
 		#[ UnPack :			VOID UnPack(a,na,denom,numer)

	Determines the sizes of the numerator and the denominator in the
	normalized fraction a with length na.

*/

VOID
UnPack ARG4(UWORD *,a,WORD,na,WORD *,denom,WORD *,numer)
{
	UWORD *pos;
	WORD i, sgn = na;
	if ( na < 0 ) { na = -na; }
	i = na;
	if ( i > 1 ) {		/* Find the respective leading words */
		a += i;
		a--;
		pos = a + i;
		while ( !(*a) ) { i--; a--; }
		while ( !(*pos) ) { na--; pos--; }
	}
	*denom = na;
	if ( sgn < 0 ) i = -i;
	*numer = i;
}

/*
 		#] UnPack : 
 		#[ Mully :			WORD Mully(a,na,b,nb)

	Multiplies the rational a by the Long b.

*/

WORD
Mully BARG4(UWORD *,a,WORD *,na,UWORD *,b,WORD,nb)
{
	GETBIDENTITY
	UWORD *d, *e;
	WORD i, sgn = 1;
	WORD nd, ne, adenom, anumer;
	if ( !nb ) { *na = 0; return(0); }
	else if ( *b == 1 ) {
		if ( nb == 1 ) return(0);
		else if ( nb == -1 ) { *na = -*na; return(0); }
	}
	if ( *na < 0 ) { sgn = -sgn; *na = -*na; }
	if ( nb < 0 ) { sgn = -sgn; nb = -nb; }
	UnPack(a,*na,&adenom,&anumer);
	if ( AN.Myscrat1 == 0 ) {
		AN.Myscrat1 = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"Mully");
		AN.Myscrat2 = AN.Myscrat1 + AM.MaxTal+2;
	}
	d = AN.Myscrat1; e = AN.Myscrat2;
	for ( i = 0; i < nb; i++ ) { e[i] = *b++; }
	ne = nb;
	if ( Simplify(BHEAD a+*na,&adenom,e,&ne) ) goto MullyEr;
	if ( MulLong(a,anumer,e,ne,d,&nd) ) goto MullyEr;
	b = a+*na;
	for ( i = 0; i < *na; i++ ) { e[i] = *b++; }
	ne = adenom;
	*na = nd;
	b = d;
	*na = nd;
	for ( i = 0; i < *na; i++ ) { a[i] = *b++; }
	Pack(a,na,e,ne);
	if ( sgn < 0 ) *na = -*na;
	return(0);
MullyEr:
	LOCK(ErrorMessageLock);
	MesCall("Mully");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] Mully : 
 		#[ Divvy :			WORD Divvy(a,na,b,nb)

	Divides the rational a by the Long b.

*/

WORD
Divvy BARG4(UWORD *,a,WORD *,na,UWORD *,b,WORD,nb)
{
	GETBIDENTITY
	UWORD *d,*e;
	WORD i, sgn = 1;
	WORD nd, ne, adenom, anumer;
	if ( AN.Dyscrat1 == 0 ) {
		AN.Dyscrat1 = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"Divvy");
		AN.Dyscrat2 = AN.Dyscrat1 + AM.MaxTal+2;
	}
	d = AN.Dyscrat1;
	e = AN.Dyscrat2;
	if ( !nb ) {
		LOCK(ErrorMessageLock);
		MesPrint("Division by zero in Divvy");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	if ( nb < 0 ) { sgn = -sgn; nb = -nb; }
	if ( *na < 0 ) { sgn = -sgn; *na = -*na; }
	UnPack(a,*na,&adenom,&anumer);
	for ( i = 0; i < nb; i++ ) { e[i] = *b++; }
	ne = nb;
	if ( Simplify(BHEAD a,&anumer,e,&ne) ) goto DivvyEr;
	if ( MulLong(a+*na,adenom,e,ne,d,&nd) ) goto DivvyEr;
	*na = anumer;
	Pack(a,na,d,nd);
	if ( sgn < 0 ) *na = -*na;
	return(0);
DivvyEr:
	LOCK(ErrorMessageLock);
	MesCall("Divvy");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] Divvy : 
 		#[ AddRat :			WORD AddRat(a,na,b,nb,c,nc)
*/

WORD
AddRat BARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	GETBIDENTITY
	UWORD *d, *e, *f, *g;
	WORD nd, ne, nf, ng, adenom, anumer, bdenom, bnumer;
	if ( !na ) {
		WORD i;
		*nc = nb;
		if ( nb < 0 ) nb = -nb;
		nb <<= 1;
		for ( i = 0; i < nb; i++ ) *c++ = *b++;
		return(0);
	}
	else if ( !nb ) {
		WORD i;
		*nc = na;
		if ( na < 0 ) na = -na;
		na <<= 1;
		for ( i = 0; i < na; i++ ) *c++ = *a++;
		return(0);
	}
	else if ( b[1] == 1 && a[1] == 1 ) {
		if ( na == 1 ) {
			if ( nb == 1 ) {
				*c = *a + *b;
				c[1] = 1;
				if ( *c < *a ) { c[2] = 1; c[3] = 0; *nc = 2; }
				else { *nc = 1; }
				return(0);
			}
			else if ( nb == -1 ) {
				if ( *b > *a ) { 
					*c = *b - *a; *nc = -1;
				}
				else if ( *b < *a ) {
					*c = *a - *b; *nc = 1;
				}
				else *nc = 0;
				c[1] = 1;
				return(0);
			}
		}
		else if ( na == -1 ){
			if ( nb == -1 ) {
				c[1] = 1;
				*c = *a + *b;
				if ( *c < *a ) { c[2] = 1; c[3] = 0; *nc = -2; }
				else { *nc = -1; }
				return(0);
			}
			else if ( nb == 1 ) {
				if ( *b > *a ) {
					*c = *b - *a; *nc = 1;
				}
				else if ( *b < *a ) {
					*c = *a - *b; *nc = -1;
				}
				else *nc = 0;
				c[1] = 1;
				return(0);
			}
		}
	}
	UnPack(a,na,&adenom,&anumer);
	UnPack(b,nb,&bdenom,&bnumer);
	if ( na < 0 ) na = -na;
	if ( nb < 0 ) nb = -nb;
	if ( AN.ARscrat1 == 0 ) {
		AN.ARscrat1 = (UWORD *)Malloc1(4*(AM.MaxTal+2)*sizeof(UWORD),"AddRat");
		AN.ARscrat2 = AN.ARscrat1 + AM.MaxTal+2;
		AN.ARscrat3 = AN.ARscrat2 + AM.MaxTal+2;
		AN.ARscrat4 = AN.ARscrat3 + AM.MaxTal+2;
	}
	if ( na == 1 && nb == 1 ) {
		RLONG t1, t2, t3;
		t3 = ((RLONG)a[1])*((RLONG)b[1]);
		t1 = ((RLONG)a[0])*((RLONG)b[1]);
		t2 = ((RLONG)a[1])*((RLONG)b[0]);
		if ( ( anumer > 0 && bnumer > 0 ) || ( anumer < 0 && bnumer < 0 ) ) {
			if ( ( t1 = t1 + t2 ) < t2 ) {
				c[2] = 1;
				c[0] = (UWORD)t1;
				c[1] = (UWORD)(t1 >> BITSINWORD);
				*nc = 3;
			}
			else {
				c[0] = (UWORD)t1;
				if ( ( c[1] = (UWORD)(t1 >> BITSINWORD) ) != 0 ) *nc = 2;
				else *nc = 1;
			}
		}
		else {
			if ( t1 == t2 ) { *nc = 0; return(0); }
			if ( t1 > t2 ) {
				t1 -= t2;
			}
			else {
				t1 = t2 - t1;
				anumer = -anumer;
			}
			c[0] = (UWORD)t1;
			if ( ( c[1] = (UWORD)(t1 >> BITSINWORD) ) != 0 ) *nc = 2;
			else *nc = 1;
		}
		if ( anumer < 0 ) *nc = -*nc;
		d = AN.ARscrat1;
		d[0] = (UWORD)t3;
		if ( ( d[1] = (UWORD)(t3 >> BITSINWORD) ) != 0 ) nd = 2;
		else nd = 1;
		if ( Simplify(BHEAD c,nc,d,&nd) ) goto AddRer;
	}
	else {
		d = AN.ARscrat1; e = AN.ARscrat2; f = AN.ARscrat3; g = AN.ARscrat4;
		if ( GcdLong(BHEAD a+na,adenom,b+nb,bdenom,d,&nd) ) goto AddRer;
		if ( *d == 1 && nd == 1 ) nd = 0;
		if ( nd ) {
			if ( DivLong(a+na,adenom,d,nd,e,&ne,c,nc) ) goto AddRer;
			if ( DivLong(b+nb,bdenom,d,nd,f,&nf,c,nc) ) goto AddRer;
			if ( MulLong(a,anumer,f,nf,c,nc) ) goto AddRer;
			if ( MulLong(b,bnumer,e,ne,g,&ng) ) goto AddRer;
		}
		else {
			if ( MulLong(a+na,adenom,b,bnumer,c,nc) ) goto AddRer;
			if ( MulLong(b+nb,bdenom,a,anumer,g,&ng) ) goto AddRer;
		}
		if ( AddLong(c,*nc,g,ng,c,nc) ) goto AddRer;
		if ( !*nc ) return(0);
		if ( nd ) {
			if ( Simplify(BHEAD c,nc,d,&nd) ) goto AddRer;
			if ( MulLong(e,ne,d,nd,g,&ng) ) goto AddRer;
			if ( MulLong(g,ng,f,nf,d,&nd) ) goto AddRer;
		}
		else {
			if ( MulLong(a+na,adenom,b+nb,bdenom,d,&nd) ) goto AddRer;
		}
	}
	Pack(c,nc,d,nd);
	return(0);
AddRer:
	LOCK(ErrorMessageLock);
	MesCall("AddRat");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] AddRat : 
 		#[ MulRat :			WORD MulRat(a,na,b,nb,c,nc)

	Multiplies the rationals a and b. The Gcd of the individual
	pieces is divided out first to minimize the chances of spurious
	overflows.

*/

WORD
MulRat BARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	WORD i;
	WORD sgn = 1;
	if ( *b == 1 && b[1] == 1 ) {
		if ( nb == 1 ) {
			*nc = na;
			i = ABS(na); i <<= 1;
			while ( --i >= 0 ) *c++ = *a++;
			return(0);
		}
		else if ( nb == -1 ) {
			*nc = - na;
			i = ABS(na); i <<= 1;
			while ( --i >= 0 ) *c++ = *a++;
			return(0);
		}
	}
	if ( *a == 1 && a[1] == 1 ) {
		if ( na == 1 ) {
			*nc = nb;
			i = ABS(nb); i <<= 1;
			while ( --i >= 0 ) *c++ = *b++;
			return(0);
		}
		else if ( na == -1 ) {
			*nc = - nb;
			i = ABS(nb); i <<= 1;
			while ( --i >= 0 ) *c++ = *b++;
			return(0);
		}
	}
	if ( na < 0 ) { na = -na; sgn = -sgn; }
	if ( nb < 0 ) { nb = -nb; sgn = -sgn; }
	if ( !na || !nb ) { *nc = 0; return(0); }
	if ( na != 1 || nb != 1 ) {
		GETBIDENTITY
		UWORD *xd,*xe;
		UWORD *xf,*xg;
		WORD dden, dnumr, eden, enumr;
		UnPack(a,na,&dden,&dnumr);
		UnPack(b,nb,&eden,&enumr);
		if ( AN.MRscrat1 == 0 ) {
			AN.MRscrat1 = (UWORD *)Malloc1(4*(AM.MaxTal+2)*sizeof(UWORD),"MulRat");
			AN.MRscrat2 = AN.MRscrat1 + AM.MaxTal+2;
			AN.MRscrat3 = AN.MRscrat2 + AM.MaxTal+2;
			AN.MRscrat4 = AN.MRscrat3 + AM.MaxTal+2;
		}
		xd = AN.MRscrat3; xf = AN.MRscrat4;
		for ( i = 0; i < dnumr; i++ ) xd[i] = a[i];
		a += na;
		for ( i = 0; i < dden; i++ ) xf[i] = a[i];
		xe = AN.MRscrat1; xg = AN.MRscrat2;
		for ( i = 0; i < enumr; i++ ) xe[i] = b[i];
		b += nb;
		for ( i = 0; i < eden; i++ ) xg[i] = b[i];
		if ( Simplify(BHEAD xd,&dnumr,xg,&eden) ) goto MulRer;
		if ( Simplify(BHEAD xe,&enumr,xf,&dden) ) goto MulRer;
		if ( MulLong(xd,dnumr,xe,enumr,c,nc) ) goto MulRer;
		if ( MulLong(xf,dden,xg,eden,xd,&dnumr) ) goto MulRer;
		Pack(c,nc,xd,dnumr);
	}
	else {
		UWORD y;
		UWORD a0,a1,b0,b1;
		RLONG xx;
		y = a[0]; b1=b[1];
		do { a0 = y % b1; y = b1; } while ( ( b1 = a0 ) != 0 );
		if ( y != 1 ) {
			a0 = a[0] / y;
			b1 = b[1] / y;
		}
		else {
			a0 = a[0];
			b1 = b[1];
		}
		y=b[0]; a1=a[1];
		do { b0 = y % a1; y = a1; } while ( ( a1 = b0 ) != 0 );
		if ( y != 1 ) {
			a1 = a[1] / y;
			b0 = b[0] / y;
		}
		else {
			a1 = a[1];
			b0 = b[0];
		}
		xx = ((RLONG)a0)*b0;
		if ( xx & AWORDMASK ) {
			*nc = 2;
			c[0] = (UWORD)xx;
			c[1] = (UWORD)(xx >> BITSINWORD);
			xx = ((RLONG)a1)*b1;
			c[2] = (UWORD)xx;
			c[3] = (UWORD)(xx >> BITSINWORD);
		}
		else {
			c[0] = (UWORD)xx;
			xx = ((RLONG)a1)*b1;
			if ( xx & AWORDMASK ) {
				c[1] = 0;
				c[2] = (UWORD)xx;
				c[3] = (UWORD)(xx >> BITSINWORD);
				*nc = 2;
			}
			else {
				c[1] = (UWORD)xx;
				*nc = 1;
			}
		}
	}
	if ( sgn < 0 ) *nc = -*nc;
	return(0);
MulRer:
	LOCK(ErrorMessageLock);
	MesCall("MulRat");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] MulRat : 
 		#[ DivRat :			WORD DivRat(a,na,b,nb,c,nc)

	Divides the rational a by the rational b.

*/

WORD
DivRat BARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	GETBIDENTITY
	WORD i, j;
	UWORD *xd,*xe,xx;
	if ( !nb ) {
		LOCK(ErrorMessageLock);
		MesPrint("Rational division by zero");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	j = i = (nb >= 0)? nb: -nb;
	xd = b; xe = b + i;
	do { xx = *xd; *xd++ = *xe; *xe++ = xx; } while ( --j > 0 );
	j = MulRat(BHEAD a,na,b,nb,c,nc);
	xd = b; xe = b + i;
	do { xx = *xd; *xd++ = *xe; *xe++ = xx; } while ( --i > 0 );
	return(j);
}

/*
 		#] DivRat : 
 		#[ Simplify :		WORD Simplify(a,na,b,nb)

	Determines the greatest common denominator of a and b and
	devides both by it. A possible sign is put in a. This is
	the simplification of the fraction a/b.

*/

WORD
Simplify BARG4(UWORD *,a,WORD *,na,UWORD *,b,WORD *,nb)
{
	GETBIDENTITY
	UWORD *x1,*x2,*x3;
	UWORD *x4;
	WORD n1,n2,n3,n4,sgn = 1;
	WORD i;
	if ( *na < 0 ) { *na = -*na; sgn = -sgn; }
	if ( *nb < 0 ) { *nb = -*nb; sgn = -sgn; }
	if ( AN.Siscrat5 == 0 ) {
		AN.Siscrat5 = (UWORD *)Malloc1(4*(AM.MaxTal+2)*sizeof(UWORD),"Simplify");
		AN.Siscrat6 = AN.Siscrat5 + AM.MaxTal+2;
		AN.Siscrat7 = AN.Siscrat6 + AM.MaxTal+2;
		AN.Siscrat8 = AN.Siscrat7 + AM.MaxTal+2;
	}
	x1 = AN.Siscrat8; x2 = AN.Siscrat7;
	if ( *nb == 1 ) {
		x3 = AN.Siscrat6;
		if ( DivLong(a,*na,b,*nb,x1,&n1,x2,&n2) ) goto SimpErr;
		if ( !n2 ) {
			for ( i = 0; i < n1; i++ ) *a++ = *x1++;
			*na = n1;
			*b = 1;
		}
		else {
			UWORD y1, y2, y3;
			y2 = *b;
			y3 = *x2;
			do { y1 = y2 % y3; y2 = y3; } while ( ( y3 = y1 ) != 0 );
			if ( ( *x2 = y2 ) != 1 ) {
				*b /= y2;
				if ( DivLong(a,*na,x2,(WORD)1,x1,&n1,x3,&n3) ) goto SimpErr;
				for ( i = 0; i < n1; i++ ) *a++ = *x1++;
				*na = n1;
			}
		}
	}
#ifdef NEWTRICK
	else if ( *na >= GCDMAX && *nb >= GCDMAX ) {
		n1 = i = *na; x3 = a;
		NCOPY(x1,x3,i);
		x3 = b; n2 = i = *nb;
		NCOPY(x2,x3,i);
		x4 = AN.Siscrat5;
		x2 = AN.Siscrat6;
		if ( GcdLong(BHEAD AN.Siscrat8,n1,AN.Siscrat7,n2,x2,&n3) ) goto SimpErr;
		n2 = n3;
		if ( *x2 != 1 || n2 != 1 ) {
			DivLong(a,*na,x2,n2,x1,&n1,x4,&n4);
			*na = i = n1;
			NCOPY(a,x1,i);
			DivLong(b,*nb,x2,n2,x3,&n3,x4,&n4);
			*nb = i = n3;
			NCOPY(b,x3,i);
		}
	}
#endif
	else {
		x4 = AN.Siscrat5;
		n1 = i = *na; x3 = a;
		NCOPY(x1,x3,i);
		x3 = b; n2 = i = *nb;
		NCOPY(x2,x3,i);
		x1 = AN.Siscrat8; x2 = AN.Siscrat7; x3 = AN.Siscrat6;
		for(;;){
			if ( DivLong(x1,n1,x2,n2,x4,&n4,x3,&n3) ) goto SimpErr;
			if ( !n3 ) break;
			if ( n2 == 1 ) {
				while ( ( *x1 = (*x2) % (*x3) ) != 0 ) { *x2 = *x3; *x3 = *x1; }
				*x2 = *x3;
				break;
			}
			if ( DivLong(x2,n2,x3,n3,x4,&n4,x1,&n1) ) goto SimpErr;
			if ( !n1 ) { x2 = x3; n2 = n3; x3 = AN.Siscrat7; break; }
			if ( n3 == 1 ) {
				while ( ( *x2 = (*x3) % (*x1) ) != 0 ) { *x3 = *x1; *x1 = *x2; }
				*x2 = *x1;
				n2 = 1;
				break;
			}
			if ( DivLong(x3,n3,x1,n1,x4,&n4,x2,&n2) ) goto SimpErr;
			if ( !n2 ) { x2 = x1; n2 = n1; x1 = AN.Siscrat7; break; }
			if ( n1 == 1 ) {
				while ( ( *x3 = (*x1) % (*x2) ) != 0 ) { *x1 = *x2; *x2 = *x3; }
				break;
			}
		}
		if ( *x2 != 1 || n2 != 1 ) {
			DivLong(a,*na,x2,n2,x1,&n1,x4,&n4);
			*na = i = n1;
			NCOPY(a,x1,i);
			DivLong(b,*nb,x2,n2,x3,&n3,x4,&n4);
			*nb = i = n3;
			NCOPY(b,x3,i);
		}
	}
	if ( sgn < 0 ) *na = -*na;
	return(0);
SimpErr:
	LOCK(ErrorMessageLock);
	MesCall("Simplify");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] Simplify : 
 		#[ AccumGCD :		WORD AccumGCD(a,na,b,nb)

		Routine takes the rational GCD of the fractions in a and b and
		replaces a by the GCD of the two.
		The rational GCD is defined as the rational that consists of
		the GCD of the numerators divided by the GCD of the denominators
*/

WORD AccumGCD ARG4(UWORD *,a,WORD *,na,UWORD *,b,WORD,nb)
{
	GETIDENTITY
	WORD nna,nnb,numa,numb,dena,denb,numc,denc;
	int i;
	if ( AN.GCDbuffer == 0 ) {
		AN.GCDbuffer  = (UWORD *)Malloc1(5*(AM.MaxTal+2)*sizeof(UWORD),"GCDbuffer");
		AN.GCDbuffer2 = AN.GCDbuffer + AM.MaxTal+2;
		AN.LCMbuffer  = AN.GCDbuffer2 + AM.MaxTal+2;
		AN.LCMb = AN.LCMbuffer + AM.MaxTal+2;
		AN.LCMc = AN.LCMb + AM.MaxTal+2;
	}
	nna = *na; if ( nna < 0 ) nna = -nna; nna = (nna-1)/2;
	nnb = nb;  if ( nnb < 0 ) nnb = -nnb; nnb = (nnb-1)/2;
	UnPack(a,nna,&dena,&numa);
	UnPack(b,nnb,&denb,&numb);
	if ( GcdLong(BHEAD a,numa,b,numb,AN.GCDbuffer,&numc) ) goto AccErr;
	numa = numc;
	for ( i = 0; i < numa; i++ ) a[i] = AN.GCDbuffer[i];
	if ( GcdLong(BHEAD a+nna,dena,b+nnb,denb,AN.GCDbuffer,&denc) ) goto AccErr;
	dena = denc;
	for ( i = 0; i < dena; i++ ) a[i+nna] = AN.GCDbuffer[i];
	Pack(a,&numa,a+nna,dena);
	*na = INCLENG(numa);
	return(0);
AccErr:
	LOCK(ErrorMessageLock);
	MesCall("AccumLong");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] AccumGCD : 
 		#[ TakeRatRoot:
*/

int TakeRatRoot ARG3(UWORD *,a,WORD *,n,WORD,power)
{
	WORD numer,denom, nn;
	if ( ( power & 1 ) == 0 && *n < 0 ) return(1);
	if ( ABS(*n) == 1 && a[0] == 1 && a[1] == 1 ) return(0);
	nn = ABS(*n);
	UnPack(a,nn,&denom,&numer);
	if ( TakeLongRoot(a+nn,&denom,power) ) return(1);
	if ( TakeLongRoot(a,&numer,power) ) return(1);
	Pack(a,&numer,a+nn,denom);
	if ( *n < 0 ) *n = -numer;
	else          *n = numer;
	return(0);
}

/*
 		#] TakeRatRoot: 
  	#] RekenRational : 
  	#[ RekenLong :
 		#[ AddLong :		WORD AddLong(a,na,b,nb,c,nc)

	Long addition. Uses addition and subtraction of positive numbers.

*/

WORD
AddLong ARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	WORD sgn;
	if ( na < 0 ) {
		if ( nb < 0 ) {
			if ( AddPLon(a,-na,b,-nb,c,(UWORD *)nc) ) return(-1);
			*nc = -*nc;
			return(0);
		}
		else {
			na = -na;
			sgn = -1;
		}
	}
	else {
		if ( nb < 0 ) {
			nb = -nb;
			sgn = 1;
		}
		else { return( AddPLon(a,na,b,nb,c,(UWORD *)nc) ); }
	}
	if ( BigLong(a,na,b,nb) >= 0 ) {
		SubPLon(a,na,b,nb,c,nc);
		if ( sgn < 0 ) *nc = -*nc;
	}
	else {
		SubPLon(b,nb,a,na,c,nc);
		if ( sgn > 0 ) *nc = -*nc;
	}
	return(0);
}

/*
 		#] AddLong : 
 		#[ AddPLon :		WORD AddPLon(a,na,b,nb,c,nc)

	Adds two long integers a and b and puts the result in c.
	The length of a and b are na and nb. The length of c is returned in nc.
	c can be a or b.
*/

WORD
AddPLon ARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,UWORD *,nc)
{
	UWORD carry = 0, e, nd = 0;
	while ( na && nb ) {
		e = *a;
		*c = e + *b + carry;
		if ( carry ) {
			if ( e < *c ) carry = 0;
		}
		else {
			if ( e > *c ) carry = 1;
		}
		a++; b++; c++; nd++; na--; nb--;
	}
	while ( na ) {
		if ( carry ) {
			*c = *a++ + carry;
			if ( *c++ ) carry = 0;
		}
		else *c++ = *a++;
		nd++; na--;
	}
	while ( nb ) {
		if ( carry ) {
			*c = *b++ + carry;
			if ( *c++ ) carry = 0;
		}
		else *c++ = *b++;
		nd++; nb--;
	}
	if ( carry ) {
		nd++;
		if ( nd > (UWORD)AM.MaxTal ) {
			LOCK(ErrorMessageLock);
			MesPrint("Overflow in addition");
			UNLOCK(ErrorMessageLock);
			return(-1);
		}
		*c++ = carry;
	}
	*nc = nd;
	return(0);
}

/*
 		#] AddPLon : 
 		#[ SubPLon :		VOID SubPLon(a,na,b,nb,c,nc)

	Subtracts b from a. Assumes that a > b. Result in c.
	c can be a or b.

*/

VOID
SubPLon ARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	UWORD borrow = 0, e, nd = 0;
	while ( nb ) {
		e = *a;
		if ( borrow ) {
			*c = e - *b - borrow;
			if ( *c < e ) borrow = 0;
		}
		else {
			*c = e - *b;
			if ( *c > e ) borrow = 1;
		}
		a++; b++; c++; na--; nb--; nd++;
	}
	while ( na ) {
		if ( borrow ) {
			if ( *a ) { *c++ = *a++ - 1; borrow = 0; }
			else { *c++ = (UWORD)(-1); a++; }
		}
		else *c++ = *a++;
		na--; nd++;
	}
	while ( !*--c && nd ) { nd--; }
	*nc = (WORD)nd;
}

/*
 		#] SubPLon : 
 		#[ MulLong :		WORD MulLong(a,na,b,nb,c,nc)

	Does a Long multiplication. Assumes that WORD is half the size
	of a LONG to work out the scheme! The number of operations is
	the canonical na*nm multiplications.
	c should not overlap with a or b.

*/

WORD
MulLong ARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	WORD sgn = 1;
	UWORD i, *ic, *ia;
	RLONG t, bb;
	if ( !na || !nb ) { *nc = 0; return(0); }
	if ( na < 0 ) { na = -na; sgn = -sgn; }
	if ( nb < 0 ) { nb = -nb; sgn = -sgn; }
	*nc = i = na + nb;
	if ( i > (UWORD)(AM.MaxTal+1) ) goto MulLov;
	ic = c;
	do { *ic++ = 0; } while ( --i > 0 );
	do {
		ia = a;
		ic = c++;
		t = 0;
		i = na;
		bb = (RLONG)(*b++);
		do {
			t = (*ia++) * bb + t + *ic;
			*ic++ = (WORD)t;
			t >>= BITSINWORD;		/* should actually be a swap */
		} while ( --i > 0 );
		if ( t ) *ic = (UWORD)t;
	} while ( --nb > 0 );
	if ( !*ic ) (*nc)--;
	if ( *nc > AM.MaxTal ) goto MulLov;
	if ( sgn < 0 ) *nc = -(*nc);
	return(0);
MulLov:
	LOCK(ErrorMessageLock);
	MesPrint("Overflow in Multiplication");
	UNLOCK(ErrorMessageLock);
	return(-1);
}

/*
 		#] MulLong : 
 		#[ BigLong :		WORD BigLong(a,na,b,nb)

	Returns > 0 if a > b, < 0 if b > a and 0 if a == b

*/

WORD
BigLong ARG4(UWORD *,a,WORD,na,UWORD *,b,WORD,nb)
{
	a += na;
	b += nb;
	while ( na && !*--a ) na--;
	while ( nb && !*--b ) nb--;
	if ( nb < na ) return(1);
	if ( nb > na ) return(-1);
	while ( --na >= 0 ) {
		if ( *a > *b ) return(1);
		else if ( *b > *a ) return(-1);
		a--; b--;
	}
	return(0);
}

/*
 		#] BigLong : 
 		#[ DivLong :		WORD DivLong(a,na,b,nb,c,nc,d,nd)

	This is the long division which knows a couple of exceptions.
	It uses therefore a recursive call for the renormalization.
	The quotient comes in c and the remainder in d.
	d may be overlapping with b. It may also be identical to a.
	c should not overlap with a, but it can overlap with b.

*/

WORD
DivLong ARG8(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c
			,WORD *,nc,UWORD *,d,WORD *,nd)
{
	WORD sgn = 1, ne, nf, ng, nh;
	WORD i, ni;
	UWORD *w1, *w2;
	RLONG t, v;
	UWORD *e, *f, *g, norm, estim;
	RLONG esthelp;
	if ( !nb ) {
		LOCK(ErrorMessageLock);
		MesPrint("Division by zero");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	if ( !na ) { *nc = *nd = 0; return(0); }
	if ( na < 0 ) { sgn = -sgn; na = -na; }
	if ( nb < 0 ) { sgn = -sgn; nb = -nb; }
	if ( na < nb ) {
		for ( i = 0; i < na; i++ ) *d++ = *a++;
		*nd = na;
		*nc = 0;
	}
	else if ( nb == na && ( i = BigLong(b,nb,a,na) ) >= 0 ) {
		if ( i > 0 ) {
			for ( i = 0; i < na; i++ ) *d++ = *a++;
			*nd = na;
			*nc = 0;
		}
		else {
			*c = 1;
			*nc = 1;
			*nd = 0;
		}
	}
	else if ( nb == 1 ) {
		if ( *b == 1 ) {
			for ( i = 0; i < na; i++ ) *c++ = *a++;
			*nc = na;
			*nd = 0;
		}
		else {
			w1 = a+na;
			*nc = ni = na;
			*nd = 1;
			w2 = c+ni;
			v = (RLONG)(*b);
			t = (RLONG)(*--w1);
			while ( --ni >= 0 ) {
				*--w2 = t / v;
				t -= v * (*w2);
				if ( ni ) {
					t <<= BITSINWORD;
					t += *--w1;
				}
			}
			if ( ( *d = (UWORD)t ) == 0 ) *nd = 0;
			if ( !*(c+na-1) ) (*nc)--;
		}
	}
	else {
		GETIDENTITY

		/* Start with normalization operation */

		if ( AN.DLscrat9 == 0 ) {
			AN.DLscrat9 = (UWORD *)Malloc1(3*(AM.MaxTal+2)*sizeof(UWORD),"DivLong");
			AN.DLscratA = AN.DLscrat9 + AM.MaxTal+2;
			AN.DLscratB = AN.DLscratA + AM.MaxTal+2;
		}
		e = AN.DLscratB; f = AN.DLscratA; g = AN.DLscrat9;
		if ( b[nb-1] == (FULLMAX-1) ) norm = 1;
		else {
			norm = (UWORD)(((ULONG)FULLMAX) / (ULONG)((b[nb-1]+1L)));
		}
		f[na] = 0;
		if(MulLong(b,nb,&norm,1,e,&ne))return(-1);
		if(MulLong(a,na,&norm,1,f,&nf))return(-1);
		if ( BigLong(f+nf-ne,ne,e,ne) >= 0 ) {
			SubPLon(f+nf-ne,ne,e,ne,f+nf-ne,&nh);
			w1 = c + (nf-ne);
			*nc = nf-ne+1;
		}
		else {
			nh = ne;
			*nc = nf-ne;
			w1 = 0;
		}
		w2 = c; i = *nc; do { *w2++ = 0; } while ( --i > 0 );
		nf = na;
		ni = nf-ne;
		esthelp = (RLONG)(e[ne-1]) + 1L;
		while ( nf >= ne ) {
			if ( (WORD)esthelp == 0 ) {
				estim = (WORD)(((((RLONG)(f[nf]))<<BITSINWORD)+f[nf-1])>>BITSINWORD);
			}
			else {
				estim = (WORD)(((((RLONG)(f[nf]))<<BITSINWORD)+f[nf-1])/esthelp);
			}
			/* This estimate may be up to two too small */
			if ( estim ) {
				MulLong(e,ne,&estim,1,g,&ng);
				nh = ne + 1; if ( !f[ni+ne] ) nh--;
				SubPLon(f+ni,nh,g,ng,f+ni,&nh);
			}
			else {
				w2 = f+ni+ne; nh = ne+1;
				while ( !*w2 && ( nh > 0 ) ) { nh--; w2--; }
			}
			if ( BigLong(f+ni,nh,e,ne) >= 0 ) {
				estim++;
				SubPLon(f+ni,nh,e,ne,f+ni,&nh);
				if ( BigLong(f+ni,nh,e,ne) >= 0 ) {
					estim++;
					SubPLon(f+ni,nh,e,ne,f+ni,&nh);
					if ( BigLong(f+ni,nh,e,ne) >= 0 ) {
						LOCK(ErrorMessageLock);
						MesPrint("Problems in DivLong");
						AO.OutSkip = 3;
						FiniLine();
						i = na;
						while ( --i >= 0 ) { TalToLine((UWORD)(*a++)); TokenToLine((UBYTE *)"  "); }
						FiniLine();
						i = nb;
						while ( --i >= 0 ) { TalToLine((UWORD)(*b++)); TokenToLine((UBYTE *)"  "); }
						AO.OutSkip = 0;
						FiniLine();
						UNLOCK(ErrorMessageLock);
						return(-1);
					}
				}
			}
			c[ni] = estim;
			nf--;
			ni--;
		}
		if ( w1 ) *w1 = 1;

		/* Finish with the renormalization operation */

		if ( nh > 0 ) {
			if ( norm == 1 ) {
				*nd = i = nh;
				NCOPY(d,f,i);
			}
			else {
				w1 = f+nh;
				*nd = ni = nh;
				w2 = d+ni;
				v = norm;
				t = (RLONG)(*--w1);
				while ( --ni >= 0 ) {
					*--w2 = t / v;
					t -= v * (*w2);
					if ( ni ) {
						t <<= BITSINWORD;
						t += *--w1;
					}
				}
				if ( t ) {
					LOCK(ErrorMessageLock);
					MesPrint("Error in DivLong");
					UNLOCK(ErrorMessageLock);
					return(-1);
				}
				if ( !*(d+nh-1) ) (*nd)--;
			}
		}
		else { *nd = 0; }
	}
	if ( sgn < 0 ) { *nc = -(*nc); *nd = -(*nd); }
	return(0);
}

/*
 		#] DivLong : 
 		#[ RaisPow :		WORD RaisPow(a,na,b)

	Raises a to the power b. a is a Long integer and b >= 0.
	The method that is used works with a bitdecomposition of b.

*/

WORD
RaisPow BARG3(UWORD *,a,WORD *,na,UWORD,b)
{
	GETBIDENTITY
	WORD i, nu;
	UWORD *it, *iu, c;
	UWORD *is;
	WORD ns, nt, nmod;
	nmod = ABS(AC.ncmod);
	if ( AN.RPscratA == 0 ) {
		AN.RPscratA = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"RaisPow");
		AN.RPscratB = AN.RPscratA + AM.MaxTal+2;
	}
	is = AN.RPscratB;
	if ( !*na || ( ( *na == 1 ) && ( *a == 1 ) ) ) return(0);
	for ( i = 0; i < *na; i++ ) *is++ = a[i];
	ns = *na;
	c = b;
	for ( i = 0; i < BITSINWORD; i++ ) {
		if ( !c ) break;
		c >>= 1;
	}
	i--;
	c = 1 << i;
	is = AN.RPscratB;
	it = AN.RPscratA;
	while ( --i >= 0 ) {
		c >>= 1;
		if(MulLong(is,ns,is,ns,it,&nt)) goto RaisOvl;
		if ( b & c ) {
			if ( MulLong(it,nt,a,*na,is,&ns) ) goto RaisOvl;
		}
		else {
			iu = is; is = it; it = iu;
			nu = ns; ns = nt; nt = nu;
		}
		if ( nmod != 0 ) {
			if ( DivLong(is,ns,(UWORD *)AC.cmod,nmod,it,&nt,is,&ns) ) goto RaisOvl;
		}
	}
	if ( ( *na = i = ns ) > 0 ) NCOPY(a,is,i);
	return(0);
RaisOvl:
	LOCK(ErrorMessageLock);
	MesCall("RaisPow");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] RaisPow : 
 		#[ Product :		WORD Product(a,na,b)

	Multiplies the Long number in a with the WORD b.

*/

WORD
Product ARG3(UWORD *,a,WORD *,na,WORD,b)
{
	WORD i, sgn = 1;
	RLONG t, u;
	if ( *na < 0 ) { *na = -(*na); sgn = -sgn; }
	if ( b < 0 ) { b = -b; sgn = -sgn; }
	t = 0;
	u = (RLONG)b;
	for ( i = 0; i < *na; i++ ) {
		t += *a * u;
		*a++ = (UWORD)t;
		t >>= BITSINWORD;
	}
	if ( t > 0 ) {
		if ( ++(*na) > AM.MaxTal ) {
			LOCK(ErrorMessageLock);
			MesPrint("Overflow in Product");
			UNLOCK(ErrorMessageLock);
			return(-1);
		}
		*a = (UWORD)t;
	}
	if ( sgn < 0 ) *na = -(*na);
	return(0);
}

/*
 		#] Product : 
 		#[ Quotient :		UWORD Quotient(a,na,b)

		Routine divides the long number a by b with the assumption that
		there is no remainder (like while computing binomials).

*/

UWORD
Quotient ARG3(UWORD *,a,WORD *,na,WORD,b)
{
	RLONG v, t;
	WORD i, j, sgn = 1;
	if ( ( i = *na ) < 0 ) { sgn = -1; i = -i; }
	if ( b < 0 ) { b = -b; sgn = -sgn; }
	if ( i == 1 ) {
		if ( ( *a /= (UWORD)b ) == 0 ) *na = 0;
		if ( sgn < 0 ) *na = -*na;
		return(0);
	}
	a += i;
	j = i;
	v = (RLONG)b;
	t = (RLONG)(*--a);
	while ( --i >= 0 ) {
		*a = t / v;
		t -= v * (*a);
		if ( i ) {
			t <<= BITSINWORD;
			t += *--a;
		}
	}
	a += j - 1;
	if ( !*a ) j--;
	if ( sgn < 0 ) j = -j;
	*na = j;
	return(0);
}

/*
 		#] Quotient : 
 		#[ Remain10 :		WORD Remain10(a,na)

	Routine devides a by 10 and gives the remainder as return value.
	The value of a will be the quotient! a must be positive.

*/

WORD
Remain10 ARG2(UWORD *,a,WORD *,na)
{
	WORD i;
	RLONG t, u;
	UWORD *b;
	i = *na;
	t = 0;
	b = a + i - 1;
	while ( --i >= 0 ) {
		t += *b;
		*b-- = u = t / 10;
		t -= u * 10;
		if ( i > 0 ) t <<= BITSINWORD;
	}
	if ( ( *na > 0 ) && !a[*na-1] ) (*na)--;
	return((WORD)t);
}

/*
 		#] Remain10 : 
 		#[ Remain4 :		WORD Remain4(a,na)

	Routine devides a by 10000 and gives the remainder as return value.
	The value of a will be the quotient! a must be positive.

*/

WORD
Remain4 ARG2(UWORD *,a,WORD *,na)
{
	WORD i;
	RLONG t, u;
	UWORD *b;
	i = *na;
	t = 0;
	b = a + i - 1;
	while ( --i >= 0 ) {
		t += *b;
		*b-- = u = t / 10000;
		t -= u * 10000;
		if ( i > 0 ) t <<= BITSINWORD;
	}
	if ( ( *na > 0 ) && !a[*na-1] ) (*na)--;
	return((WORD)t);
}

/*
 		#] Remain4 : 
 		#[ PrtLong :		VOID PrtLong(a,na,s)

	Puts the long positive number a in string s.

*/

VOID
PrtLong ARG3(UWORD *,a,WORD,na,UBYTE *,s)
{
	GETIDENTITY
	WORD q, i;
	UBYTE *sa, *sb;
	UBYTE c;
	UWORD *bb, *b;
	if ( AN.PLscratA == 0 ) {
		AN.PLscratA = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"PrtLong");
	}
	b = AN.PLscratA;
	bb = b;
	i = na; while ( --i >= 0 ) *bb++ = *a++;
	a = b;
	if ( na > 2 ) {
		sa = s;
		do {
			q = Remain4(a,&na);
			*sa++ = '0' + (q%10);
			q /= 10;
			*sa++ = '0' + (q%10);
			q /= 10;
			*sa++ = '0' + (q%10);
			q /= 10;
			*sa++ = '0' + (q%10);
		} while ( na );
		while ( sa[-1] == '0' ) sa--;
		sb = s;
		s = sa;
		sa--;
		while ( sa > sb ) { c = *sa; *sa = *sb; *sb = c; sa--; sb++; }
	}
	else if ( na ) {
		sa = s;
		do {
			q = Remain10(a,&na);
			*sa++ = '0' + q;
		} while ( na );
		sb = s;
		s = sa;
		sa--;
		while ( sa > sb ) { c = *sa; *sa = *sb; *sb = c; sa--; sb++; }
	}
	else *s++ = '0';
	*s = '\0';
}

/*
 		#] PrtLong : 
 		#[ GetLong :		WORD GetLong(s,a,na)

	Reads a long number from a string.
	The string is zero terminated and contains only digits!

	New algorithm: try to read 4 digits together before the result
	is accumulated.
*/

WORD
GetLong ARG3(UBYTE *,s,UWORD *,a,WORD *,na)
{
/*
	UWORD digit;
	*a = 0;
	*na = 0;
	while ( FG.cTable[*s] == 1 ) {
		digit = *s++ - '0';
		if ( *na && Product(a,na,(WORD)10) ) return(-1);
		if ( digit && AddLong(a,*na,&digit,(WORD)1,a,na) ) return(-1);
	}
	return(0);
*/
	UWORD digit, x = 0, y = 0;
	*a = 0;
	*na = 0;
	while ( FG.cTable[*s] == 1 ) {
		x = *s++ - '0';
		if ( FG.cTable[*s] != 1 ) { y = 10; break; }
		x = 10*x + *s++ - '0';
		if ( FG.cTable[*s] != 1 ) { y = 100; break; }
		x = 10*x + *s++ - '0';
		if ( FG.cTable[*s] != 1 ) { y = 1000; break; }
		x = 10*x + *s++ - '0';
		if ( *na && Product(a,na,(WORD)10000) ) return(-1);
		if ( ( digit = x ) != 0 && AddLong(a,*na,&digit,(WORD)1,a,na) )
			return(-1);
		y = 0;
	}
	if ( y ) {
		if ( *na && Product(a,na,(WORD)y) ) return(-1);
		if ( ( digit = x ) != 0 && AddLong(a,*na,&digit,(WORD)1,a,na) )
			return(-1);
	}
	return(0);
}

/*
 		#] GetLong : 
 		#[ GCD :			WORD GCD(a,na,b,nb,c,nc)

	Algorithm to compute the GCD of two long numbers.
	See Knuth, sec 4.5.2 algorithm L.

	We assume that both numbers are positive
*/

#ifdef ANSI
VOID GCD(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
ULONG GCD2(ULONG,ULONG);
#else
VOID GCD();
ULONG GCD2();
#endif

#ifdef EXTRAGCD

#define Convert(ia,aa,naa) \
	if ( (LONG)ia < 0 ) {        \
		ia = (ULONG)(-(LONG)ia); \
		aa[0] = ia;              \
		if ( ( aa[1] = ia >> BITSINWORD ) != 0 ) naa = -2; \
		else naa = -1;           \
	}                            \
	else if ( ia == 0 ) { aa[0] = 0; naa = 0; } \
	else {                       \
		aa[0] = ia;              \
		if ( ( aa[1] = ia >> BITSINWORD ) != 0 ) naa = 2; \
		else naa = 1;            \
	}

VOID
GCD ARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	int ja = 0, jb = 0, j;
	UWORD *r,*t;
	UWORD *x1, *x2, *x3;
	WORD nd,naa,nbb;
	ULONG ia,ib,ic,id,u,v,w,q,T;
	UWORD aa[2], bb[2];
/*
	First eliminate easy powers of 2^...
*/
	while ( a[0] == 0 ) { na--; ja++; a++; }
	while ( b[0] == 0 ) { nb--; jb++; b++; }
	if ( ja > jb ) ja = jb;
	if ( ja > 0 ) {
		j = ja;
		do { *c++ = 0; } while ( --j > 0 );
	}
/*
	Now arrange things such that a >= b
*/
	if ( na < nb ) {
		jb = na; na = nb; nb = jb;
exch:
		r = a; a = b; b = r;
	}
	else if ( na == nb ) {
		r = a+na;
		t = b+nb;
		j = na;
		while ( --j >= 0 ) {
			if ( *--r > *--t ) break;
			if ( *r < *t ) goto exch;
		}
		if ( j < 0 ) {
out:
			j = nb;
			NCOPY(c,b,j);
			*nc = nb+ja;
			return;
		}
	}
/*
	{
		LOCK(ErrorMessageLock);
		MesPrint("Ordered input, ja = %d",(WORD)ja);
		AO.OutSkip = 3;
		FiniLine();
		j = na; r = a;
		while ( --j >= 0 ) { TalToLine((UWORD)(*r++)); TokenToLine((UBYTE *)"  "); }
		FiniLine();
		j = nb; r = b;
		while ( --j >= 0 ) { TalToLine((UWORD)(*r++)); TokenToLine((UBYTE *)"  "); }
		AO.OutSkip = 0;
		FiniLine();
		UNLOCK(ErrorMessageLock);
	}
*/
/*
	We have now that A > B
	The loop recognizes the case that na-nb >= 1
	In that case we just have to divide!
*/
	if ( AN.GCscrat6 == 0 ) {
		AN.GCscrat6 = (UWORD *)Malloc1(3*(AM.MaxTal+2)*sizeof(UWORD),"GCD");
		AN.GCscrat7 = AN.GCscrat6 + AM.MaxTal+2;
		AN.GCscrat8 = AN.GCscrat7 + AM.MaxTal+2;
	}
	r = x1 = AN.GCscrat6; t = x2 = AN.GCscrat7; x3 = AN.GCscrat8;
	j = na;
	NCOPY(r,a,j);
	j = nb;
	NCOPY(t,b,j);

	for(;;) {

		while ( na > nb ) {
toobad:
			DivLong(x1,na,x2,nb,c,nc,x3,&nd);
			if ( nd == 0 ) { b = x2; goto out; }
			t = x1; x1 = x2; x2 = x3; x3 = t; na = nb; nb = nd;
			if ( na == 2 ) break;
		}
/*
	Here we can use the shortcut.
*/
	if ( na == 2 ) {
		v = x1[0] + ( ((ULONG)x1[1]) << BITSINWORD );
		w = x2[0];
		if ( nb == 2 ) w += ((ULONG)x2[1]) << BITSINWORD;
#ifdef EXTRAGCD2
		v = GCD2(v,w);
#else
		do { u = v%w; v = w; w = u; } while ( w );
#endif
		c[0] = (UWORD)v;
		if ( ( c[1] = (UWORD)(v >> BITSINWORD) ) != 0 ) *nc = 2+ja;
		else *nc = 1+ja;
		return;
	}
	if ( na == 1 ) {
		UWORD ui, uj;
		ui = x1[0]; uj = x2[0];
#ifdef EXTRAGCD2
		ui = (UWORD)GCD2((ULONG)ui,(ULONG)uj);
#else
		do { nd = ui%uj; ui = uj; uj = nd; } while ( nd );
#endif
		c[0] = ui;
		*nc = 1 + ja;
		return;
	}
	ia = 1; ib = 0; ic = 0; id = 1;
	u = ( ((ULONG)x1[na-1]) << BITSINWORD ) + x1[na-2];
	v = ( ((ULONG)x2[nb-1]) << BITSINWORD ) + x2[nb-2];

	while ( v+ic != 0 && v+id != 0 &&
		 ( q = (u+ia)/(v+ic) ) == (u+ib)/(v+id) ) {
		T = ia-q*ic; ia = ic; ic = T;
		T = ib-q*id; ib = id; id = T;
		T = u - q*v; u = v; v = T;
	}
	if ( ib == 0 ) goto toobad;
	Convert(ia,aa,naa);
	Convert(ib,bb,nbb);
	MulLong(x1,na,aa,naa,x3,&nd);
	MulLong(x2,nb,bb,nbb,c,nc);
	AddLong(x3,nd,c,*nc,c,nc);
	Convert(ic,aa,naa);
	Convert(id,bb,nbb);
	MulLong(x1,na,aa,naa,x3,&nd);
	t = c; na = j = *nc; r = x1;
	NCOPY(r,t,j);
	MulLong(x2,nb,bb,nbb,c,nc);
	AddLong(x3,nd,c,*nc,x2,&nb);
	}
}

#endif

/*
 		#] GCD : 
 		#[ GcdLong :		WORD GcdLong(a,na,b,nb,c,nc)

	Returns the Greatest Common Divider of a and b in c.
	If a and or b are zero an error message will be returned.
	The answer is always positive.
	In principle a and c can be the same.
*/

#ifndef NEWTRICK

WORD
GcdLong BARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	GETBIDENTITY
	if ( !na || !nb ) {
		LOCK(ErrorMessageLock);
		MesPrint("Cannot take gcd");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	if ( na < 0 ) na = -na;
	if ( nb < 0 ) nb = -nb;
	if ( AN.GLscrat6 == 0 ) {
		AN.GLscrat6 = (UWORD *)Malloc1(3*(AM.MaxTal+2)*sizeof(UWORD),"GcdLong");
		AN.GLscrat7 = AN.GLscrat6 + AM.MaxTal+2;
		AN.GLscrat8 = AN.GLscrat7 + AM.MaxTal+2;
	}
	if ( na == 1 && nb == 1 ) {
#ifdef EXTRAGCD2
		*c = (UWORD)GCD2((ULONG)*a,(ULONG)*b);
#else
		UWORD x,y,z;
		x = *a;
		y = *b;
		do { z = x % y; x = y; } while ( ( y = z ) != 0 );
		*c = x;
#endif
		*nc = 1;
	}
	else if ( na <= 2 && nb <= 2 ) {
		RLONG lx,ly,lz;
		if ( na == 2 ) { lx = (((RLONG)(a[1]))<<BITSINWORD) + *a; }
		else { lx = *a; }
		if ( nb == 2 ) { ly = (((RLONG)(b[1]))<<BITSINWORD) + *b; }
		else { ly = *b; }
#ifdef LONGWORD
#ifdef EXTRAGCD2
		lx = GCD2(lx,ly);
#else
		do { lz = lx % ly; lx = ly; } while ( ( ly = lz ) != 0 );
#endif
#else
          if ( lx < ly ) { lz = lx; lx = ly; ly = lz; }
		do {
			lz = lx % ly; lx = ly;
		} while ( ( ly = lz ) != 0 && ( lx & AWORDMASK ) != 0 );
		if ( ly ) {
			do { *c = ((UWORD)lx)%((UWORD)ly); lx = ly; } while ( ( ly = *c ) != 0 );
			*c = (UWORD)lx;
			*nc = 1;
		}
		else
#endif
		{
			*c++ = (UWORD)lx;
			if ( ( *c = (UWORD)(lx >> BITSINWORD) ) != 0 ) *nc = 2;
			else *nc = 1;
		}
	}
	else {
#ifdef EXTRAGCD
		GCD(a,na,b,nb,c,nc);
#else
#ifdef NEWGCD
		UWORD *x3,*x1,*x2;
		WORD n1,n2,n3,n4;
		WORD i, j;
		x1 = c; x3 = a; n1 = i = na;
		NCOPY(x1,x3,i);
		x2 = AN.GLscrat8; x3 = b; n2 = i = nb;
		NCOPY(x2,x3,i);
		x1 = c; i = 0;
		while ( x1[0] == 0 ) { i += BITSINWORD; x1++; n1--; }
		while ( ( x1[0] & 1 ) == 0 ) { i++; SCHUIF(x1,n1) }
		x2 = AN.GLscrat8; j = 0;
		while ( x2[0] == 0 ) { j += BITSINWORD; x2++; n2--; }
		while ( ( x2[0] & 1 ) == 0 ) { j++; SCHUIF(x2,n2) }
		if ( j > i ) j = i;		/* powers of two in GCD */
		for(;;){
			if ( n1 > n2 ) {
firstbig:
				SubPLon(x1,n1,x2,n2,x1,&n3);
				n1 = n3;
				if ( n1 == 0 ) {
					x1 = c;
					n1 = i = n2; NCOPY(x1,x2,i);
					break;
				}
				while ( ( x1[0] & 1 ) == 0 ) SCHUIF(x1,n1)
				if ( n1 == 1 ) {
					if ( DivLong(x2,n2,x1,n1,AN.GLscrat7,&n3,x2,&n4) ) goto GcdErr;
					n2 = n4;
					if ( n2 == 0 ) {
						i = n1; x2 = c; NCOPY(x2,x1,i);
						break;
					}
#ifdef EXTRAGCD2
					*c = (UWORD)GCD2((ULONG)x1[0],(ULONG)x2[0]);
#else
					{
						UWORD x,y,z;
						x = x1[0];
						y = x2[0];
						do { z = x % y; x = y; } while ( ( y = z ) != 0 );
						*c = x;
					}
#endif
					n1 = 1;
					break;
				}
			}
			else if ( n1 < n2 ) {
lastbig:
				SubPLon(x2,n2,x1,n1,x2,&n3);
				n2 = n3;
				if ( n2 == 0 ) {
					i = n1; x2 = c; NCOPY(x2,x1,i);
					break;
				}
				while ( ( x2[0] & 1 ) == 0 ) SCHUIF(x2,n2)
				if ( n2 == 1 ) {
					if ( DivLong(x1,n1,x2,n2,AN.GLscrat7,&n3,x1,&n4) ) goto GcdErr;
					n1 = n4;
					if ( n1 == 0 ) {
						x1 = c;
						n1 = i = n2; NCOPY(x1,x2,i);
						break;
					}
#ifdef EXTRAGCD2
					*c = (UWORD)GCD2((ULONG)x2[0],(ULONG)x1[0]);
#else
					{
						UWORD x,y,z;
						x = x2[0];
						y = x1[0];
						do { z = x % y; x = y; } while ( ( y = z ) != 0 );
						*c = x;
					}
#endif
					n1 = 1;
					break;
				}
			}
			else {
				for ( i = n1-1; i >= 0; i-- ) {
					if ( x1[i] > x2[i] ) goto firstbig;
					else if ( x1[i] < x2[i] ) goto lastbig;
				}
				i = n1; x2 = c; NCOPY(x2,x1,i);
				break;
			}
		}
/*
		Now the GCD is in c but still needs j powers of 2.
*/
		x1 = c;
		while ( j >= BITSINWORD ) {
			for ( i = n1; i > 0; i-- ) x1[i] = x1[i-1];
			x1[0] = 0; n1++;
			j -= BITSINWORD;
		}
		if ( j > 0 ) {
			ULONG a1,a2 = 0;
			for ( i = 0; i < n1; i++ ) {
				a1 = x1[i]; a1 <<= j;
				a2 += a1;
				x1[i] = a2;
				a2 >>= BITSINWORD;
			}
			if ( a2 != 0 ) {
				x1[n1++] = a2;
			}
		}
		*nc = n1;
#else
		UWORD *x1,*x2,*x3,*x4;
		WORD n1,n2,n3,n4,i;
		x1 = c; x3 = a; n1 = i = na;
		NCOPY(x1,x3,i);
		x2 = AN.GLscrat8; x3 = b; n2 = i = nb;
		NCOPY(x2,x3,i);
		x1 = c; x2 = AN.GLscrat8; x3 = AN.GLscrat7; x4 = AN.GLscrat6;
		for(;;){
			if ( DivLong(x1,n1,x2,n2,x4,&n4,x3,&n3) ) goto GcdErr;
			if ( !n3 ) { x1 = x2; n1 = n2; break; }
			if ( DivLong(x2,n2,x3,n3,x4,&n4,x1,&n1) ) goto GcdErr;
			if ( !n1 ) { x1 = x3; n1 = n3; break; }
			if ( DivLong(x3,n3,x1,n1,x4,&n4,x2,&n2) ) goto GcdErr;
			if ( !n2 ) { *nc = n1; return(0); }
		}
		*nc = i = n1;
		NCOPY(c,x1,i);
#endif
#endif
	}
	return(0);
GcdErr:
	LOCK(ErrorMessageLock);
	MesCall("GcdLong");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

#else

/*
	New routine for GcdLong that uses smart shortcuts.
	Algorithm by J. Vermaseren 15-nov-2006.
	It runs faster for very big numbers but only by a fixed factor.
	There is no improvement in the power behaviour.
	Improvement on the whole of hf9 (multiple zeta values at weight 9):
		Better than a factor 2 on a 32 bits architecture and 2.76 on a
		64 bits architecture.

	If we have two long numbers (na,nb > GCDMAX) we will work in a
	truncated way. At the moment of writing (15-nov-2006) it isn't
	clear whether this algorithm is an invention or a reinvention.
	A short search on the web didn't show anything.

	Algorithm

	1: while ( na > nb || nb < GCDMAX ) {
		if ( nb == 0 ) { result in a }
		c = a % b;
		a = b;
		b = c;
	   }
	2: Make the truncated values in which a and b are the combinations
	   of the top two words of a and b. The whole numbers are aa and bb now.
	3: ma1 = 1; ma2 = 0; mb1 = 0; mb2 = 1;
	4: A = a; B = b; m = a/b; c = a - m*b;
	   c = ma1*a+ma2*b-m*(mb1*a+mb2*b) = (ma1-m*mb1)*a+(ma2-m*mb2)*b
	   mc1 = ma1-m*mb1; mc2 = ma2-m*mb2;
	5: a = b; ma1 = mb1; ma2 = mb2;
	   b = c; mb1 = mc1; mb2 = mc2;
	6: if ( b != 0 && nb >= FULLMAX ) goto 4;
	7: Now construct the new quantities
		ma1*aa+ma2*bb and mb1*aa+mb2*bb
	8: goto 1;

	The essence of the above algorithm is that we do the divisions only
	on relatively short numbers. Also usually there are many steps 4&5
	for each step 7. This eliminates many operations.
	The termination at FULLMAX is that we make errors by not considering
	the tail of the number. If we run b down all the way, the errors combine
	in such a way that the new numbers may be of the same order as the old
	numbers. By stopping halfway we don't get the error beyond halfway
	either. Unfortunately this means that a >= FULLMAX and hence na > nb
	which means that next we will have a complete division. But just once.
	Running the steps 4-6 till a < FULLMAX runs already into problems.
	It may be necessary to experiment a bit to obtain the optimum value
	of GCDMAX.
*/

WORD
GcdLong BARG6(UWORD *,a,WORD,na,UWORD *,b,WORD,nb,UWORD *,c,WORD *,nc)
{
	GETBIDENTITY
	UWORD x,y,z;
	UWORD *x1,*x2,*x3,*x4,*x5,*d;
	WORD n1,n2,n3,n4,n5,i;
	RLONG lx,ly,lz;
	LONG ma1, ma2, mb1, mb2, mc1, mc2, m;
#ifdef WITHEXTRAPASS
	int pass;
#endif
	if ( !na || !nb ) {
		LOCK(ErrorMessageLock);
		MesPrint("Cannot take gcd");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	if ( na < 0 ) na = -na;
	if ( nb < 0 ) nb = -nb;
	if ( AN.GLscrat6 == 0 ) {
		AN.GLscrat6 = (UWORD *)Malloc1(5*(AM.MaxTal+2)*sizeof(UWORD),"GcdLong");
		AN.GLscrat7 = AN.GLscrat6 + AM.MaxTal+2;
		AN.GLscrat8 = AN.GLscrat7 + AM.MaxTal+2;
		AN.GLscrat9 = AN.GLscrat8 + AM.MaxTal+2;
		AN.GLscrat10 = AN.GLscrat9 + AM.MaxTal+2;
	}
restart:;
	if ( na == 1 && nb == 1 ) {
		x = *a;
		y = *b;
		do { z = x % y; x = y; } while ( ( y = z ) != 0 );
		*c = x;
		*nc = 1;
	}
	else if ( na <= 2 && nb <= 2 ) {
		if ( na == 2 ) { lx = (((RLONG)(a[1]))<<BITSINWORD) + *a; }
		else { lx = *a; }
		if ( nb == 2 ) { ly = (((RLONG)(b[1]))<<BITSINWORD) + *b; }
		else { ly = *b; }
		if ( lx < ly ) { lz = lx; lx = ly; ly = lz; }
		do {
			lz = lx % ly; lx = ly;
		} while ( ( ly = lz ) != 0 && ( lx & AWORDMASK ) != 0 );
		if ( ly ) {
			x = (UWORD)lx; y = (UWORD)ly;
			do { *c = x % y; x = y; } while ( ( y = *c ) != 0 );
			*c = x;
			*nc = 1;
		}
		else
		{
			*c++ = (UWORD)lx;
			if ( ( *c = (UWORD)(lx >> BITSINWORD) ) != 0 ) *nc = 2;
			else *nc = 1;
		}
	}
	else if ( na < GCDMAX || nb < GCDMAX || na != nb ) {
		if ( na < nb ) {
			x2 = AN.GLscrat8; x3 = a; n2 = i = na;
			NCOPY(x2,x3,i);
			x1 = c; x3 = b; n1 = i = nb;
			NCOPY(x1,x3,i);
		}
		else {
			x1 = c; x3 = a; n1 = i = na;
			NCOPY(x1,x3,i);
			x2 = AN.GLscrat8; x3 = b; n2 = i = nb;
			NCOPY(x2,x3,i);
		}
		x1 = c; x2 = AN.GLscrat8; x3 = AN.GLscrat7; x4 = AN.GLscrat6;
		for(;;){
			if ( DivLong(x1,n1,x2,n2,x4,&n4,x3,&n3) ) goto GcdErr;
			if ( !n3 ) { x1 = x2; n1 = n2; break; }
			if ( n2 <= 2 ) { a = x2; b = x3; na = n2; nb = n3; goto restart; }
			if ( n3 >= GCDMAX && n2 == n3 ) {
				a = AN.GLscrat9; b = AN.GLscrat10; na = n2; nb = n3;
				for ( i = 0; i < na; i++ ) a[i] = x2[i];
				for ( i = 0; i < nb; i++ ) b[i] = x3[i];
				goto newtrick;
			}
			if ( DivLong(x2,n2,x3,n3,x4,&n4,x1,&n1) ) goto GcdErr;
			if ( !n1 ) { x1 = x3; n1 = n3; break; }
			if ( n3 <= 2 ) { a = x3; b = x1; na = n3; nb = n1; goto restart; }
			if ( n1 >= GCDMAX && n1 == n3 ) {
				a = AN.GLscrat9; b = AN.GLscrat10; na = n3; nb = n1;
				for ( i = 0; i < na; i++ ) a[i] = x3[i];
				for ( i = 0; i < nb; i++ ) b[i] = x1[i];
				goto newtrick;
			}
			if ( DivLong(x3,n3,x1,n1,x4,&n4,x2,&n2) ) goto GcdErr;
			if ( !n2 ) { *nc = n1; return(0); }
			if ( n1 <= 2 ) { a = x1; b = x2; na = n1; nb = n2; goto restart; }
			if ( n2 >= GCDMAX && n2 == n1 ) {
				a = AN.GLscrat9; b = AN.GLscrat10; na = n1; nb = n2;
				for ( i = 0; i < na; i++ ) a[i] = x1[i];
				for ( i = 0; i < nb; i++ ) b[i] = x2[i];
				goto newtrick;
			}
		}
		*nc = i = n1;
		NCOPY(c,x1,i);
	}
	else {
/*
		This is the new algorithm starting at step 3.

	3: ma1 = 1; ma2 = 0; mb1 = 0; mb2 = 1;
	4: A = a; B = b; m = a/b; c = a - m*b;
	   c = ma1*a+ma2*b-m*(mb1*a+mb2*b) = (ma1-m*mb1)*a+(ma2-m*mb2)*b
	   mc1 = ma1-m*mb1; mc2 = ma2-m*mb2;
	5: a = b; ma1 = mb1; ma2 = mb2;
	   b = c; mb1 = mc1; mb2 = mc2;
	6: if ( b != 0 ) goto 4;
*/
newtrick:;
#ifdef WITHEXTRAPASS
		pass = 0;
#endif
		ma1 = 1; ma2 = 0; mb1 = 0; mb2 = 1;
		lx = (((RLONG)(a[na-1]))<<BITSINWORD) + a[na-2];
		ly = (((RLONG)(b[nb-1]))<<BITSINWORD) + b[nb-2];
		if ( ly > lx ) { lz = lx; lx = ly; ly = lz; d = a; a = b; b = d; }
#ifdef WITHEXTRAPASS
retry:;
#endif
		do {
			m = lx/ly;
			mc1 = ma1-m*mb1; mc2 = ma2-m*mb2;
			ma1 = mb1; ma2 = mb2; mb1 = mc1; mb2 = mc2;
			lz = lx - m*ly; lx = ly; ly = lz;
		} while ( ly >= FULLMAX );
/*
		Next the construction of the two new numbers

	7: Now construct the new quantities
		a = ma1*aa+ma2*bb and b = mb1*aa+mb2*bb
*/
		x1 = AN.GLscrat6;
		x2 = AN.GLscrat7;
		x3 = AN.GLscrat8;
		x5 = AN.GLscrat10;
		if ( ma1 < 0 ) {
			ma1 = -ma1;
			x1[0] = (UWORD)ma1;
			x1[1] = (UWORD)(ma1 >> BITSINWORD);
			if ( x1[1] ) n1 = -2;
			else         n1 = -1;
		}
		else {
			x1[0] = (UWORD)ma1;
			x1[1] = (UWORD)(ma1 >> BITSINWORD);
			if ( x1[1] ) n1 = 2;
			else         n1 = 1;
		}
		if ( MulLong(a,na,x1,n1,x2,&n2) ) goto GcdErr;
		if ( ma2 < 0 ) {
			ma2 = -ma2;
			x1[0] = (UWORD)ma2;
			x1[1] = (UWORD)(ma2 >> BITSINWORD);
			if ( x1[1] ) n1 = -2;
			else         n1 = -1;
		}
		else {
			x1[0] = (UWORD)ma2;
			x1[1] = (UWORD)(ma2 >> BITSINWORD);
			if ( x1[1] ) n1 = 2;
			else         n1 = 1;
		}
		if ( MulLong(b,nb,x1,n1,x3,&n3) ) goto GcdErr;
		if ( AddLong(x2,n2,x3,n3,c,&n4) ) goto GcdErr;
		if ( mb1 < 0 ) {
			mb1 = -mb1;
			x1[0] = (UWORD)mb1;
			x1[1] = (UWORD)(mb1 >> BITSINWORD);
			if ( x1[1] ) n1 = -2;
			else         n1 = -1;
		}
		else {
			x1[0] = (UWORD)mb1;
			x1[1] = (UWORD)(mb1 >> BITSINWORD);
			if ( x1[1] ) n1 = 2;
			else         n1 = 1;
		}
		if ( MulLong(a,na,x1,n1,x2,&n2) ) goto GcdErr;
		if ( mb2 < 0 ) {
			mb2 = -mb2;
			x1[0] = (UWORD)mb2;
			x1[1] = (UWORD)(mb2 >> BITSINWORD);
			if ( x1[1] ) n1 = -2;
			else         n1 = -1;
		}
		else {
			x1[0] = (UWORD)mb2;
			x1[1] = (UWORD)(mb2 >> BITSINWORD);
			if ( x1[1] ) n1 = 2;
			else         n1 = 1;
		}
		if ( MulLong(b,nb,x1,n1,x3,&n3) ) goto GcdErr;
		if ( AddLong(x2,n2,x3,n3,x5,&n5) ) goto GcdErr;
		a = c; na = n4; b = x5; nb = n5;
		if ( nb == 0 ) { *nc = n4; return(0); }
		x4 = AN.GLscrat9; 
		for ( i = 0; i < na; i++ ) x4[i] = a[i];
		a = x4;
		if ( na < 0 ) na = -na;
		if ( nb < 0 ) nb = -nb;

		if ( nb >= GCDMAX && na == nb+1 && b[nb-1] >= HALFMAX && b[nb-1] > a[na-1] ) {
			lx = (((RLONG)(a[na-1]))<<BITSINWORD) + a[na-2];
			x1[0] = lx/b[nb-1]; n1 = 1;
			MulLong(b,nb,x1,n1,x2,&n2);
			n2 = -n2;
			AddLong(a,na,x2,n2,x4,&n4);
			if ( n4 == 0 ) {
				*nc = nb;
				for ( i = 0; i < nb; i++ ) c[i] = b[i];
				return(0);
			}
			if ( n4 < 0 ) n4 = -n4;
			a = b; na = nb; b = x4; nb = n4;
		}

#ifdef WITHEXTRAPASS
		if ( pass == 0 && nb >= GCDMAX && na == nb+1 && b[nb-1] >= HALFMAX ) {
			pass = 1;
			ma1 = 1; ma2 = 0; mb1 = 0; mb2 = 1;
			lx = (((RLONG)(a[na-1]))<<BITSINWORD) + a[na-2];
			ly = (RLONG)(b[nb-1]);
			goto retry;
		}
#endif
		goto restart;
	}
	return(0);
GcdErr:
	LOCK(ErrorMessageLock);
	MesCall("GcdLong");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

#endif

/*
 		#] GcdLong :
 		#[ GetBinom :		WORD GetBinom(a,na,i1,i2)
*/

WORD
GetBinom ARG4(UWORD *,a,WORD *,na,WORD,i1,WORD,i2)
{
	GETIDENTITY
	WORD j, k, l;
	if ( i1-i2 < i2 ) i2 = i1-i2;
	if ( i2 == 0 ) { *a = 1; *na = 1; return(0); }
	if ( i2 > i1 ) { *a = 0; *na = 0; return(0); }
	*a = i1; *na = 1;
	if ( AN.GBscrat3 == 0 ) {
		AN.GBscrat3 = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"GetBinom");
		AN.GBscrat4 = AN.GBscrat3 + AM.MaxTal+2;
	}
	for ( j = 2; j <= i2; j++ ) {
		AN.GBscrat3[0] = i1+1-j;
		if ( MulLong(a,*na,AN.GBscrat3,(WORD)1,AN.GBscrat4,&k) ) {
			LOCK(ErrorMessageLock);
			MesCall("GetBinom");
			UNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
		AN.GBscrat3[0] = j;
		if ( DivLong(AN.GBscrat4,k,AN.GBscrat3,(WORD)1,a,na,AN.GBscrat3,&l) ) {
			LOCK(ErrorMessageLock);
			MesCall("GetBinom");
			UNLOCK(ErrorMessageLock);
			SETERROR(-1)
		}
	}
	return(0);
}

/*
 		#] GetBinom : 
 		#[ TakeLongRoot:

	Takes the 'power'-root of the long number in a.
	If the root could be taken the return value is zero.
	If the root could not be taken, the return value is 1.
	The root will be in a if it could be taken, otherwise there will be garbage
	Algorithm: (assume b is guess of root, b' better guess)
		b' = (a-(power-1)*b^power)/(n*b^(power-1))
	Note: power should be positive!
*/

int TakeLongRoot ARG3(UWORD *,a,WORD *,n,WORD,power)
{
	GETIDENTITY
	int numbits, guessbits, i;
	UWORD x, *b, *c, *d, *e;
	WORD na, nb, nc, nd, ne;
	if ( *n < 0 && ( power & 1 ) == 0 ) return(1);
	if ( power == 1 ) return(0);
	if ( *n < 0 ) { na = -*n; }
	else          { na =  *n; }
	if ( na == 1 ) {
/*			Special cases that are the most frequent */
		if ( a[0] == 1 ) return(0);
		if ( power < BITSINWORD && na == 1 && a[0] == (1<<power) ) {
			a[0] = 2; return(0);
		}
		if ( 2*power < BITSINWORD && na == 1 && a[0] == (1<<(2*power)) ) {
			a[0] = 4; return(0);
		}
	}
/*
	Step 1: make a guess. We count the number of bits.
	numbits will be the 1+2log(a)
*/
	numbits = BITSINWORD*(na-1);
	x = a[na-1];
	while ( ( x >> 8 ) != 0 ) { numbits += 8; x >>= 8; }
	if ( ( x >> 4 ) != 0 ) { numbits += 4; x >>= 4; }
	if ( ( x >> 2 ) != 0 ) { numbits += 2; x >>= 2; }
	if ( ( x >> 1 ) != 0 ) numbits++;
	guessbits = numbits / power;
	if ( guessbits <= 0 ) return(1); /* root < 2 and 1 we did already */
	nb = guessbits/BITSINWORD;
	if ( AN.TLscrat1 == 0 ) {
		AN.TLscrat1 = (UWORD *)Malloc1(4*(AM.MaxTal+2)*sizeof(UWORD),"TakeLongRoot");
		AN.TLscrat2 = AN.TLscrat1 + AM.MaxTal + 2;
		AN.TLscrat3 = AN.TLscrat2 + AM.MaxTal + 2;
		AN.TLscrat4 = AN.TLscrat3 + AM.MaxTal + 2;
	}
/*
	The recursion is:
	(b'-b) = (a/b^(power-1)-b)/n
	       = (a/c-b)/n
	       = (d-b)/n    (remainder of a/c is e)
	       = c/n  (we reuse the scratch array c)
	Termination can be tricky. When a/c has no remainder and = b we have a root.
	When d = b but the remainder of a/c != 0, there is definitely no root.
*/
	b = AN.TLscrat1; c = AN.TLscrat2; d = AN.TLscrat3; e = AN.TLscrat4; 
	for ( i = 0; i < nb; i++ ) { b[i] = 0; }
	b[nb] = 1 << (guessbits%BITSINWORD);
	nb++;
	for(;;) {
		nc = nb;
		for ( i = 0; i < nb; i++ ) c[i] = b[i];
		if ( RaisPow(BHEAD c,&nc,power-1) ) goto TLcall;
		if ( DivLong(a,na,c,nc,d,&nd,e,&ne) ) goto TLcall;
		nb = -nb;
		if ( AddLong(d,nd,b,nb,c,&nc) ) goto TLcall;
		nb = -nb;
		if ( nc == 0 ) {
			if ( ne == 0 ) break;
			else return(1);
		}
		DivLong(c,nc,(UWORD *)(&power),1,d,&nd,e,&ne);
		if ( nd == 0 ) {
			return(1);
/*
			This code tries b+1 as a final possibility.
			We believe this is not needed
			UWORD one = 1;
			if ( AddLong(b,nb,&one,1,c,&nc) ) goto TLcall;
			if ( RaisPow(BHEAD c,&nc,power-1) ) goto TLcall;
			if ( DivLong(a,na,c,nc,d,&nd,e,&ne) ) goto TLcall;
			if ( ne != 0 ) return(1);
			nb = -nb;
			if ( SubLong(d,nd,b,nb,c,&nc) ) goto TLcall;
			nb = -nb;
			if ( nc != 0 ) return(1);
			break;
*/
		}
		if ( AddLong(b,nb,d,nd,b,&nb) ) goto TLcall;
	}
	for ( i = 0; i < nb; i++ ) a[i] = b[i];
	if ( *n < 0 ) *n = -nb;
	else          *n =  nb;
	return(0);
TLcall:
	LOCK(ErrorMessageLock);
	MesCall("TakeLongRoot");
	UNLOCK(ErrorMessageLock);
	Terminate(-1);
	return(-1);
}

/*
 		#] TakeLongRoot: 
  	#] RekenLong :
  	#[ RekenTerms :
 		#[ CompCoef :		WORD CompCoef(term1,term2)

	Compares the coefficients of term1 and term2 by subtracting them.
	This does more work than needed but this routine is only called
	when sorting functions and function arguments.
	(and comparing values 
*/
/* #define 64SAVE */

WORD
CompCoef ARG2(WORD *,term1,WORD *,term2)
{
	GETIDENTITY
	UWORD *c;
	WORD n1,n2,n3,*a;
	GETCOEF(term1,n1);
	GETCOEF(term2,n2);
	if ( AN.CCscratE == 0 ) {
		AN.CCscratE = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"CompCoef");
	}
	c = AN.CCscratE;
	if ( term1[1] == 0 && n1 == 1 ) {
		if ( term2[1] == 0 && n2 == 1 ) return(0);
		if ( n2 < 0 ) return(1);
		return(-1);
	}
	else if ( term2[1] == 0 && n2 == 1 ) {
		if ( n1 < 0 ) return(-1);
		return(1);
	}
	if ( n1 > 0 ) {
		if ( n2 < 0 ) return(1);
	}
	else {
		if ( n2 > 0 ) return(-1);
		a = term1; term1 = term2; term2 = a;
		n3 = -n1; n1 = -n2; n2 = n3;
	}
	if ( term1[1] == 1 && term2[1] == 1 && n1 == 1 && n2 == 1 ) {
		if ( (UWORD)*term1 > (UWORD)*term2 ) return(1);
		else if ( (UWORD)*term1 < (UWORD)*term2 ) return(-1);
		else return(0);
	}

/*
	The next call should get dedicated code, as AddRat does more than
	strictly needed. Also more attention should be given to overflow.
*/
	if ( AddRat(BHEAD (UWORD *)term1,n1,(UWORD *)term2,-n2,c,&n3) ) {
		LOCK(ErrorMessageLock);
		MesCall("CompCoef");
		UNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	return(n3);
}

/*
 		#] CompCoef : 
 		#[ Modulus :		WORD Modulus(term)

	Routine takes the coefficient of term modulus b. The answer
	is in term again and the length of term is adjusted.

*/

WORD
Modulus ARG1(WORD *,term)
{
	WORD *t;
	WORD n1;
	t = term;
	GETCOEF(t,n1);
	if ( TakeModulus((UWORD *)t,&n1,AC.cmod,AC.ncmod,0) ) {
		LOCK(ErrorMessageLock);
		MesCall("Modulus");
		UNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	if ( !n1 ) {
		*term = 0;
	}
	else {
		n1 <<= 1;
		t += n1;		/* Note that n1 >= 0 */
		n1++;
		*t++ = n1;
		*term = WORDDIF(t,term);
	}
	return(0);
}

/*
 		#] Modulus : 
 		#[ TakeModulus :	WORD TakeModulus(a,na,par)

		Routine gets the rational number in a with reduced length na.
		It is called when AC.ncmod != 0 and the number in AC.cmod is the
		number wrt which we need the modulus.
		The result is returned in a and na again.

*/

WORD
TakeModulus ARG5(UWORD *,a,WORD *,na,WORD *,cmodvec,WORD,ncmod,WORD,par)
{
	GETIDENTITY
	UWORD *c, *d, *e, *f, *g, *h;
	UWORD *x4,*x2;
	UWORD *x3,*x1,*x5,*x6,*x7,*x8;
	WORD y3,y1,y5,y6;
	WORD n1, i, y2, y4;
	WORD nh, tdenom, tnumer, nmod;
	if ( ncmod == 0 ) return(0);		/* No modulus operation */
	nmod = ABS(ncmod);
	n1 = *na;
	if ( !par ) UnPack(a,n1,&tdenom,&tnumer);
	else { tnumer = n1; }
	if ( AT.TMscrat1 == 0 ) {
		AT.TMscrat1 = (UWORD *)Malloc1(6*(AM.MaxTal+2)*sizeof(UWORD),"TakeModulus");
		AT.TMscrat2 = AT.TMscrat1 + AM.MaxTal+2;
		AT.TMscrat3 = AT.TMscrat2 + AM.MaxTal+2;
		AT.TMscrat4 = AT.TMscrat3 + AM.MaxTal+2;
		AT.TMscrat5 = AT.TMscrat4 + AM.MaxTal+2;
		AT.TMscrat6 = AT.TMscrat5 + AM.MaxTal+2;
	}
	c = AT.TMscrat1; d = AT.TMscrat2; e = AT.TMscrat3; f = AT.TMscrat4; g = AT.TMscrat5; h = AT.TMscrat6;
	n1 = ABS(n1);
	if ( DivLong(a,tnumer,(UWORD *)cmodvec,nmod,
		c,&nh,a,&tnumer) ) goto ModErr;
	if ( par ) { *na = tnumer; return(0); }
	if ( DivLong(a+n1,tdenom,(UWORD *)cmodvec,nmod,c,&nh,a+n1,&tdenom) ) goto ModErr;
	if ( !tdenom ) {
		LOCK(ErrorMessageLock);
		MesPrint("Division by zero in modulus arithmetic");
		if ( AP.DebugFlag ) {
			AO.OutSkip = 3;
			FiniLine();
			i = *na;
			if ( i < 0 ) i = -i;
			while ( --i >= 0 ) { TalToLine((UWORD)(*a++)); TokenToLine((UBYTE *)"  "); }
			i = *na;
			if ( i < 0 ) i = -i;
			while ( --i >= 0 ) { TalToLine((UWORD)(*a++)); TokenToLine((UBYTE *)"  "); }
			TalToLine((UWORD)(*na));
			AO.OutSkip = 0;
			FiniLine();
		}
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	x2 = (UWORD *)cmodvec; x1 = c; i = nmod; while ( --i >= 0 ) *x1++ = *x2++;
	x1 = c; x2 = a+n1; x3 = d; x4 = e; x5 = f; x6 = g;
	y1 = nmod; y2 = tdenom; y4 = 0; y5 = 1; *x5 = 1;
	for(;;) {
		if ( DivLong(x1,y1,x2,y2,h,&nh,x3,&y3) ) goto ModErr;
		if ( MulLong(x5,y5,h,nh,x6,&y6) ) goto ModErr;
		if ( AddLong(x4,y4,x6,-y6,x6,&y6) ) goto ModErr;
		if ( !y3 ) {
			if ( y2 != 1 || *x2 != 1 ) {
				LOCK(ErrorMessageLock);
				MesPrint("Inverse in modulus arithmetic doesn't exist");
				MesPrint("Denominator and modulus are not relative prime");
				UNLOCK(ErrorMessageLock);
				goto ModErr;
			}
			break;
		}
		x7 = x1; x1 = x2; y1 = y2; x2 = x3; y2 = y3; x3 = x7;
		x8 = x4; x4 = x5; y4 = y5; x5 = x6; y5 = y6; x6 = x8;
	}
	if ( y5 < 0 && AddLong((UWORD *)cmodvec,nmod,x5,y5,x5,&y5) ) goto ModErr;
	if ( MulLong(a,tnumer,x5,y5,c,&y3) ) goto ModErr;
	if ( DivLong(c,y3,(UWORD *)cmodvec,nmod,d,&y5,a,&tdenom) ) goto ModErr;
	if ( !tdenom ) { *na = 0; return(0); }
	if ( tdenom < 0 ) SubPLon((UWORD *)cmodvec,nmod,a,-tdenom,a,&tdenom);
	*na = i = tdenom;
	a += i;
	*a++ = 1;
	while ( --i > 0 ) *a++ = 0;
	return(0);
ModErr:
	LOCK(ErrorMessageLock);
	MesCall("TakeModulus");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] TakeModulus : 
 		#[ MakeModTable :	WORD MakeModTable()
*/

WORD
MakeModTable()
{
	LONG size, i, j, n;
	n = ABS(AC.ncmod);
	if ( AC.modpowers ) M_free(AC.modpowers,"AC.modpowers");
	if ( n > 2 ) {
		LOCK(ErrorMessageLock);
		MesPrint("&No memory for modulus generator power table");
		UNLOCK(ErrorMessageLock);
		Terminate(-1);
	}
	if ( n == 0 ) return(0);
	size = (LONG)(*AC.cmod);
	if ( n == 2 ) size += (((LONG)AC.cmod[1])<<BITSINWORD);
	AC.modpowers = (UWORD *)Malloc1(size*n*sizeof(UWORD),"table for powers of modulus");
	if ( n == 1 ) {
		j = 1;
		for ( i = 0; i < size; i++ ) AC.modpowers[i] = 0;
		for ( i = 0; i < size; i++ ) {
			AC.modpowers[j] = (WORD)i;
			j *= *AC.powmod;
			j %= *AC.cmod;
		}
		for ( i = 2; i < size; i++ ) {
			if ( AC.modpowers[i] == 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("&improper generator for this modulus");
				UNLOCK(ErrorMessageLock);
				M_free(AC.modpowers,"AC.modpowers");
				return(-1);
			}
		}
		AC.modpowers[1] = 0;
	}
	else {
		GETIDENTITY
		WORD nScrat, n2;
		if ( AN.MMscrat7 == 0 ) {
			AN.MMscrat7 = (UWORD *)Malloc1(2*(AM.MaxTal+2)*sizeof(UWORD),"MakeModTable");
			AN.MMscratC = AN.MMscrat7 + AM.MaxTal+2;
		}
		*AN.MMscratC = 1;
		nScrat = 1;
		j = size << 1;
		for ( i = 0; i < j; i+=2 ) { AC.modpowers[i] = 0; AC.modpowers[i+1] = 0; }
		for ( i = 0; i < size; i++ ) {
			j = *AN.MMscratC + (((LONG)AN.MMscratC[1])<<BITSINWORD);
			j <<= 1;
			AC.modpowers[j] = (WORD)(i & WORDMASK);
			AC.modpowers[j+1] = (WORD)(i >> BITSINWORD);
			MulLong((UWORD *)AN.MMscratC,nScrat,(UWORD *)AC.powmod,
			AC.npowmod,(UWORD *)AN.MMscrat7,&n2);
			TakeModulus(AN.MMscrat7,&n2,AC.cmod,AC.ncmod,1);
			*AN.MMscratC = *AN.MMscrat7; AN.MMscratC[1] = AN.MMscrat7[1]; nScrat = n2;
		}
		j = size << 1;
		for ( i = 4; i < j; i+=2 ) {
			if ( AC.modpowers[i] == 0 && AC.modpowers[i+1] == 0 ) {
				LOCK(ErrorMessageLock);
				MesPrint("&improper generator for this modulus");
				UNLOCK(ErrorMessageLock);
				M_free(AC.modpowers,"AC.modpowers");
				return(-1);
			}
		}
		AC.modpowers[2] = AC.modpowers[3] = 0;
	}
	return(0);
}

/*
 		#] MakeModTable : 
  	#] RekenTerms : 
  	#[ Functions :
 		#[ Factorial :		WORD Factorial(n,a,na)

	Starts with only the value of fac_(0).
	Builds up what is needed and remembers it for the next time.

	We have:
		AT.nfac: the number of the highest stored factorial
		AT.pfac: the array of locations in the array of stored factorials
		AT.factorials: the array with stored factorials
*/

int
Factorial BARG3(WORD,n,UWORD *,a,WORD *,na)
{
	GETBIDENTITY
	UWORD *b, *c;
	WORD nc;
	int i, j;
	long ii;
	if ( n > AT.nfac ) {
		if ( AT.factorials == 0 ) {
			AT.nfac = 0; AT.mfac = 50; AT.sfact = 400;
			AT.pfac = (long *)Malloc1((AT.mfac+2)*sizeof(long),"factorials");
			AT.factorials = (UWORD *)Malloc1(AT.sfact*sizeof(UWORD),"factorials");
			AT.factorials[0] = 1; AT.pfac[0] = 0; AT.pfac[1] = 1;
		}
		b = a;
		c = AT.factorials+AT.pfac[AT.nfac];
		nc = i = AT.pfac[AT.nfac+1] - AT.pfac[AT.nfac];
		while ( --i >= 0 ) *b++ = *c++;
		for ( j = AT.nfac+1; j <= n; j++ ) {
			Product(a,&nc,j);
			if ( nc > AM.MaxTal ) {
				LOCK(ErrorMessageLock);
				MesPrint("Overflow in factorial. MaxTal = %d",AM.MaxTal);
				MesPrint("Increase MaxTerm in %s",setupfilename);
				UNLOCK(ErrorMessageLock);
				return(-1);
			}
			if ( j > AT.mfac ) {  /* double the pfac buffer */
				long *p;
				p = (long *)Malloc1((AT.mfac*2+2)*sizeof(long),"factorials");
				i = AT.mfac;
				for ( i = AT.mfac+1; i >= 0; i-- ) p[i] = AT.pfac[i];
				M_free(AT.pfac,"factorial pointers"); AT.pfac = p; AT.mfac *= 2;
			}
			if ( AT.pfac[j] + nc >= AT.sfact ) { /* double the factorials buffer */
				UWORD *f;
				f = (UWORD *)Malloc1(AT.sfact*2*sizeof(UWORD),"factorials");
				ii = AT.sfact;
				c = AT.factorials; b = f;
				while ( --ii >= 0 ) *b++ = *c++;
				M_free(AT.factorials,"factorials");
				AT.factorials = f;
				AT.sfact *= 2;
			}
			b = a; c = AT.factorials + AT.pfac[j]; i = nc;
			while ( --i >= 0 ) *c++ = *b++;
			AT.pfac[j+1] = AT.pfac[j] + nc;
		}
		*na = nc;
		AT.nfac = n;
	}
	else if ( n == 0 ) {
		*a = 1; *na = 1;
	}
	else {
		*na = i = AT.pfac[n+1] - AT.pfac[n];
		b = AT.factorials + AT.pfac[n];
		while ( --i >= 0 ) *a++ = *b++;
	}
	return(0);
}

/*
 		#] Factorial : 
 		#[ Bernoulli :		WORD Bernoulli(n,a,na)

	Starts with only the value of bernoulli_(0).
	Builds up what is needed and remembers it for the next time.
	b_0 = 1
	(n+1)*b_n = -b_{n-1}-sum_(i,1,n-1,b_i*b_{n-i})
	The n-1 playes only a role for b_2.
	We have hard coded b_0,b_1,b_2 and b_odd. After that:
	(2n+1)*b_2n = -sum_(i,1,n-1,b_2i*b_{2n-2i})

	We have:
		AT.nBer: the number of the highest stored Bernoulli number
		AT.pBer: the array of locations in the array of stored Bernoulli numbers
		AT.bernoullis: the array with stored Bernoulli numbers
*/

int
Bernoulli ARG3(WORD,n,UWORD *,a,WORD *,na)
{
	GETIDENTITY
	UWORD *b, *c, *scrib, *ntop, *ntop1;
	WORD i, i1, i2, nhalf, nqua, nscrib, nntop, nntop1, *oldworkpointer;
	UWORD twee = 2, twonplus1;
	int j;
	long ii;
	if ( n <= 1 ) {
		if ( n == 0 ) { a[0] = a[1] = 1; *na = 3; }
		else if ( n == 1 ) { a[0] = 1; a[1] = 2; *na = 3; }
		return(0);
	}
	if ( ( n & 1 ) != 0 ) { a[0] = a[1] = 0; *na = 0; return(0); }
	nhalf = n/2;
	if ( nhalf > AT.nBer ) {
		oldworkpointer = AT.WorkPointer;
		if ( AT.bernoullis == 0 ) {
			AT.nBer = 1; AT.mBer = 50; AT.sBer = 400;
			AT.pBer = (long *)Malloc1((AT.mBer+2)*sizeof(long),"bernoullis");
			AT.bernoullis = (UWORD *)Malloc1(AT.sBer*sizeof(UWORD),"bernoullis");
			AT.pBer[1] = 0; AT.pBer[2] = 3;
			AT.bernoullis[0] = 3; AT.bernoullis[1] = 1; AT.bernoullis[2] = 12;
			if ( nhalf == 1 ) {
				a[0] = 1; a[1] = 12; *na = 3; return(0);
			}
		}
		while ( nhalf > AT.mBer ) {
			long *p;
			p = (long *)Malloc1((AT.mBer*2+1)*sizeof(long),"bernoullis");
			i = AT.mBer;
			for ( i = AT.mBer; i >= 0; i-- ) p[i] = AT.pBer[i];
			M_free(AT.pBer,"factorial pointers"); AT.pBer = p; AT.mBer *= 2;
		}
		for ( n = AT.nBer+1; n <= nhalf; n++ ) {
			scrib = (UWORD *)(AT.WorkPointer);
			nqua = n/2;
			if ( ( n & 1 ) == 1 ) {
				nscrib = 0; ntop = scrib;
			}
			else {
				b = AT.bernoullis + AT.pBer[nqua];
				nscrib = *b++;
				i = (WORD)(REDLENG(nscrib));
				MulRat(BHEAD b,i,b,i,scrib,&nscrib);
				ntop = scrib + 2*nscrib;
				nscrib = nscrib;
				nqua--;
			}
			for ( j = 1; j <= nqua; j++ ) {
				b = AT.bernoullis + AT.pBer[j];
				c = AT.bernoullis + AT.pBer[n-j];
				i1 = (WORD)(*b); i2 = (WORD)(*c);
				i1 = REDLENG(i1);
				i2 = REDLENG(i2);
				MulRat(BHEAD b+1,i1,c+1,i2,ntop,&nntop);
				Mully(BHEAD ntop,&nntop,&twee,1);
				if ( nscrib ) {
					i = (WORD)nntop; if ( i < 0 ) i = -i;
					ntop1 = ntop + 2*i;
					AddRat(BHEAD ntop,nntop,scrib,nscrib,ntop1,&nntop1);
				}
				else {
					ntop1 = ntop; nntop1 = nntop;
				}
				nscrib = i1 = (WORD)nntop1;
				if ( i1 < 0 ) i1 = - i1;
				i1 = 2*i1;
				for ( i = 0; i < i1; i++ ) scrib[i] = ntop1[i];
				ntop = scrib + i1; 
			}
			twonplus1 = 2*n+1;
			Divvy(BHEAD scrib,&nscrib,&twonplus1,-1);
			i1 = INCLENG(nscrib);
			i2 = i1; if ( i2 < 0 ) i2 = -i2;
			i = (WORD)(AT.bernoullis[AT.pBer[n-1]]);
			if ( i < 0 ) i = -i;
			AT.pBer[n] = AT.pBer[n-1]+i;
			if ( AT.pBer[n] + i2 >= AT.sBer ) {
				UWORD *f;
				f = (UWORD *)Malloc1(AT.sBer*2*sizeof(UWORD),"bernoullis");
				ii = AT.sBer;
				c = AT.bernoullis; b = f;
				while ( --ii >= 0 ) *b++ = *c++;
				M_free(AT.bernoullis,"bernoullis");
				AT.bernoullis = f;
				AT.sBer *= 2;
			}
			c = AT.bernoullis + AT.pBer[n]; b = scrib;
			*c++ = i1;
			for ( i = 1; i < i2; i++ ) *c++ = *b++;
		}
		AT.nBer = nhalf;
		AT.WorkPointer = oldworkpointer;
	}
	b = AT.bernoullis + AT.pBer[nhalf];
	*na = i = (WORD)(*b++);
	if ( i < 0 ) i = -i;
	i--;
	while ( --i >= 0 ) *a++ = *b++;
	return(0);
}

/*
 		#] Bernoulli : 
  	#] Functions : 
*/

/* temporary commentary for forcing cvs merge */
