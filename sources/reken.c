/** @file reken.c
 * 
 *  This file contains the numerical routines.
 *  The arithmetic in FORM is normally over the rational numbers.
 *	Hence there are routines for dealing with integers and with rational
 *	of 'arbitrary precision' (within limits)
 *	There are also routines for that calculus modulus an integer.
 *	In addition there are the routines for factorials and bernoulli numbers.
 *	The random number function is currently only for internal purposes.
 */
/* #[ License : */
/*
 *   Copyright (C) 1984-2017 J.A.M. Vermaseren
 *   When using this file you are requested to refer to the publication
 *   J.A.M.Vermaseren "New features of FORM" math-ph/0010025
 *   This is considered a matter of courtesy as the development was paid
 *   for by FOM the Dutch physics granting agency and we would like to
 *   be able to track its scientific use to convince FOM of its value
 *   for the community.
 *
 *   This file is part of FORM.
 *
 *   FORM is free software: you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any later
 *   version.
 *
 *   FORM is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 *   details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with FORM.  If not, see <http://www.gnu.org/licenses/>.
 */
/* #] License : */ 
/*
  	#[ Includes : reken.c
*/

#include "form3.h"
#include <math.h>

#ifdef WITHGMP
#include <gmp.h>
#define GMPSPREAD (GMP_LIMB_BITS/BITSINWORD)
#endif
 
#define GCDMAX 3

#define NEWTRICK 1
/*
  	#] Includes : 
  	#[ RekenRational :
 		#[ Pack :			VOID Pack(a,na,b,nb)

	Packs the contents of the numerator a and the denominator b into
	one normalized fraction a.

*/

VOID Pack(UWORD *a, WORD *na, UWORD *b, WORD nb)
{
	WORD c, sgn = 1, i;
	UWORD *to,*from;
	if ( (c = *na) == 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Caught a zero in Pack");
		MUNLOCK(ErrorMessageLock);
		return;
	}
	if ( nb == 0 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Division by zero in Pack");
		MUNLOCK(ErrorMessageLock);
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

VOID UnPack(UWORD *a, WORD na, WORD *denom, WORD *numer)
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

WORD Mully(PHEAD UWORD *a, WORD *na, UWORD *b, WORD nb)
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
	d = NumberMalloc("Mully"); e = NumberMalloc("Mully");
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
	NumberFree(d,"Mully"); NumberFree(e,"Mully");
	return(0);
MullyEr:
	MLOCK(ErrorMessageLock);
	MesCall("Mully");
	MUNLOCK(ErrorMessageLock);
	NumberFree(d,"Mully"); NumberFree(e,"Mully");
	SETERROR(-1)
}

/*
 		#] Mully : 
 		#[ Divvy :			WORD Divvy(a,na,b,nb)

	Divides the rational a by the Long b.

*/

WORD Divvy(PHEAD UWORD *a, WORD *na, UWORD *b, WORD nb)
{
	GETBIDENTITY
	UWORD *d,*e;
	WORD i, sgn = 1;
	WORD nd, ne, adenom, anumer;
	if ( !nb ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Division by zero in Divvy");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	d = NumberMalloc("Divvy"); e = NumberMalloc("Divvy");
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
	NumberFree(d,"Divvy"); NumberFree(e,"Divvy");
	return(0);
DivvyEr:
	MLOCK(ErrorMessageLock);
	MesCall("Divvy");
	MUNLOCK(ErrorMessageLock);
	NumberFree(d,"Divvy"); NumberFree(e,"Divvy");
	SETERROR(-1)
}

/*
 		#] Divvy : 
 		#[ AddRat :			WORD AddRat(a,na,b,nb,c,nc)
*/

WORD AddRat(PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
{
	GETBIDENTITY
	UWORD *d, *e, *f, *g;
	WORD nd, ne, nf, ng, adenom, anumer, bdenom, bnumer;
	if ( !na ) {
		WORD i;
		*nc = nb;
		if ( nb < 0 ) nb = -nb;
		nb *= 2;
		for ( i = 0; i < nb; i++ ) *c++ = *b++;
		return(0);
	}
	else if ( !nb ) {
		WORD i;
		*nc = na;
		if ( na < 0 ) na = -na;
		na *= 2;
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
		d = NumberMalloc("AddRat");
		d[0] = (UWORD)t3;
		if ( ( d[1] = (UWORD)(t3 >> BITSINWORD) ) != 0 ) nd = 2;
		else nd = 1;
		if ( Simplify(BHEAD c,nc,d,&nd) ) goto AddRer1;
	}
/*
	else if ( a[na] == 1 && b[nb] == 1 && adenom == 1 && bdenom == 1 ) {
		if ( AddLong(a,na,b,nb,c,&nc) ) goto AddRer2;
		i = ABS(nc); d = c + i; *d++ = 1;
		while ( --i > 0 ) *d++ = 0 ;
		return(0);
	}
*/
	else {
		d = NumberMalloc("AddRat"); e = NumberMalloc("AddRat");
		f = NumberMalloc("AddRat"); g = NumberMalloc("AddRat");
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
		if ( !*nc ) {
			NumberFree(g,"AddRat"); NumberFree(f,"AddRat");
			NumberFree(e,"AddRat"); NumberFree(d,"AddRat");
			return(0);
		}
		if ( nd ) {
			if ( Simplify(BHEAD c,nc,d,&nd) ) goto AddRer;
			if ( MulLong(e,ne,d,nd,g,&ng) ) goto AddRer;
			if ( MulLong(g,ng,f,nf,d,&nd) ) goto AddRer;
		}
		else {
			if ( MulLong(a+na,adenom,b+nb,bdenom,d,&nd) ) goto AddRer;
		}
		NumberFree(g,"AddRat"); NumberFree(f,"AddRat"); NumberFree(e,"AddRat");
	}
	Pack(c,nc,d,nd);
	NumberFree(d,"AddRat");
	return(0);
AddRer:
	NumberFree(g,"AddRat"); NumberFree(f,"AddRat"); NumberFree(e,"AddRat");
AddRer1:
	NumberFree(d,"AddRat");
/* AddRer2: */
	MLOCK(ErrorMessageLock);
	MesCall("AddRat");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
 		#] AddRat : 
 		#[ MulRat :			WORD MulRat(a,na,b,nb,c,nc)

	Multiplies the rationals a and b. The Gcd of the individual
	pieces is divided out first to minimize the chances of spurious
	overflows.

*/

WORD MulRat(PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
{
	WORD i;
	WORD sgn = 1;
	if ( *b == 1 && b[1] == 1 ) {
		if ( nb == 1 ) {
			*nc = na;
			i = ABS(na); i *= 2;
			while ( --i >= 0 ) *c++ = *a++;
			return(0);
		}
		else if ( nb == -1 ) {
			*nc = - na;
			i = ABS(na); i *= 2;
			while ( --i >= 0 ) *c++ = *a++;
			return(0);
		}
	}
	if ( *a == 1 && a[1] == 1 ) {
		if ( na == 1 ) {
			*nc = nb;
			i = ABS(nb); i *= 2;
			while ( --i >= 0 ) *c++ = *b++;
			return(0);
		}
		else if ( na == -1 ) {
			*nc = - nb;
			i = ABS(nb); i *= 2;
			while ( --i >= 0 ) *c++ = *b++;
			return(0);
		}
	}
	if ( na < 0 ) { na = -na; sgn = -sgn; }
	if ( nb < 0 ) { nb = -nb; sgn = -sgn; }
	if ( !na || !nb ) { *nc = 0; return(0); }
	if ( na != 1 || nb != 1 ) {
		GETBIDENTITY
		UWORD *xd,*xe, *xf,*xg;
		WORD dden, dnumr, eden, enumr;
		UnPack(a,na,&dden,&dnumr);
		UnPack(b,nb,&eden,&enumr);
		xd = NumberMalloc("MulRat"); xf = NumberMalloc("MulRat");
		for ( i = 0; i < dnumr; i++ ) xd[i] = a[i];
		a += na;
		for ( i = 0; i < dden; i++ ) xf[i] = a[i];
		xe = NumberMalloc("MulRat"); xg = NumberMalloc("MulRat");
		for ( i = 0; i < enumr; i++ ) xe[i] = b[i];
		b += nb;
		for ( i = 0; i < eden; i++ ) xg[i] = b[i];
		if ( Simplify(BHEAD xd,&dnumr,xg,&eden) ||
		     Simplify(BHEAD xe,&enumr,xf,&dden) ||
		     MulLong(xd,dnumr,xe,enumr,c,nc) ||
		     MulLong(xf,dden,xg,eden,xd,&dnumr) ) {
			MLOCK(ErrorMessageLock);
			MesCall("MulRat");
			MUNLOCK(ErrorMessageLock);
			NumberFree(xd,"MulRat"); NumberFree(xe,"MulRat"); NumberFree(xf,"MulRat"); NumberFree(xg,"MulRat");
			SETERROR(-1)
		}
		Pack(c,nc,xd,dnumr);
		NumberFree(xd,"MulRat"); NumberFree(xe,"MulRat"); NumberFree(xf,"MulRat"); NumberFree(xg,"MulRat");
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
}

/*
 		#] MulRat : 
 		#[ DivRat :			WORD DivRat(a,na,b,nb,c,nc)

	Divides the rational a by the rational b.

*/

WORD DivRat(PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
{
	GETBIDENTITY
	WORD i, j;
	UWORD *xd,*xe,xx;
	if ( !nb ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Rational division by zero");
		MUNLOCK(ErrorMessageLock);
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

WORD Simplify(PHEAD UWORD *a, WORD *na, UWORD *b, WORD *nb)
{
	GETBIDENTITY
	UWORD *x1,*x2,*x3;
	UWORD *x4;
	WORD n1,n2,n3,n4,sgn = 1;
	WORD i;
	UWORD *Siscrat5, *Siscrat6, *Siscrat7, *Siscrat8;
	if ( *na < 0 ) { *na = -*na; sgn = -sgn; }
	if ( *nb < 0 ) { *nb = -*nb; sgn = -sgn; }
	Siscrat5 = NumberMalloc("Simplify"); Siscrat6 = NumberMalloc("Simplify"); 
	Siscrat7 = NumberMalloc("Simplify"); Siscrat8 = NumberMalloc("Simplify"); 
	x1 = Siscrat8; x2 = Siscrat7;
	if ( *nb == 1 ) {
		x3 = Siscrat6;
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
		x4 = Siscrat5;
		x2 = Siscrat6;
		x3 = Siscrat7;
		if ( GcdLong(BHEAD Siscrat8,n1,Siscrat7,n2,x2,&n3) ) goto SimpErr;
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
		x4 = Siscrat5;
		n1 = i = *na; x3 = a;
		NCOPY(x1,x3,i);
		x3 = b; n2 = i = *nb;
		NCOPY(x2,x3,i);
		x1 = Siscrat8; x2 = Siscrat7; x3 = Siscrat6;
		for(;;){
			if ( DivLong(x1,n1,x2,n2,x4,&n4,x3,&n3) ) goto SimpErr;
			if ( !n3 ) break;
			if ( n2 == 1 ) {
				while ( ( *x1 = (*x2) % (*x3) ) != 0 ) { *x2 = *x3; *x3 = *x1; }
				*x2 = *x3;
				break;
			}
			if ( DivLong(x2,n2,x3,n3,x4,&n4,x1,&n1) ) goto SimpErr;
			if ( !n1 ) { x2 = x3; n2 = n3; x3 = Siscrat7; break; }
			if ( n3 == 1 ) {
				while ( ( *x2 = (*x3) % (*x1) ) != 0 ) { *x3 = *x1; *x1 = *x2; }
				*x2 = *x1;
				n2 = 1;
				break;
			}
			if ( DivLong(x3,n3,x1,n1,x4,&n4,x2,&n2) ) goto SimpErr;
			if ( !n2 ) { x2 = x1; n2 = n1; x1 = Siscrat7; break; }
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
	NumberFree(Siscrat5,"Simplify"); NumberFree(Siscrat6,"Simplify");
	NumberFree(Siscrat7,"Simplify"); NumberFree(Siscrat8,"Simplify");
	return(0);
SimpErr:
	MLOCK(ErrorMessageLock);
	MesCall("Simplify");
	MUNLOCK(ErrorMessageLock);
	NumberFree(Siscrat5,"Simplify"); NumberFree(Siscrat6,"Simplify");
	NumberFree(Siscrat7,"Simplify"); NumberFree(Siscrat8,"Simplify");
	SETERROR(-1)
}

/*
 		#] Simplify : 
 		#[ AccumGCD :		WORD AccumGCD(PHEAD a,na,b,nb)

		Routine takes the rational GCD of the fractions in a and b and
		replaces a by the GCD of the two.
		The rational GCD is defined as the rational that consists of
		the GCD of the numerators divided by the GCD of the denominators
*/

WORD AccumGCD(PHEAD UWORD *a, WORD *na, UWORD *b, WORD nb)
{
	GETBIDENTITY
	WORD nna,nnb,numa,numb,dena,denb,numc,denc;
	UWORD *GCDbuffer = NumberMalloc("AccumGCD");
	int i;
	nna = *na; if ( nna < 0 ) nna = -nna; nna = (nna-1)/2;
	nnb = nb;  if ( nnb < 0 ) nnb = -nnb; nnb = (nnb-1)/2;
	UnPack(a,nna,&dena,&numa);
	UnPack(b,nnb,&denb,&numb);
	if ( GcdLong(BHEAD a,numa,b,numb,GCDbuffer,&numc) ) goto AccErr;
	numa = numc;
	for ( i = 0; i < numa; i++ ) a[i] = GCDbuffer[i];
	if ( GcdLong(BHEAD a+nna,dena,b+nnb,denb,GCDbuffer,&denc) ) goto AccErr;
	dena = denc;
	for ( i = 0; i < dena; i++ ) a[i+nna] = GCDbuffer[i];
	Pack(a,&numa,a+nna,dena);
	*na = INCLENG(numa);
	NumberFree(GCDbuffer,"AccumGCD");
	return(0);
AccErr:
	MLOCK(ErrorMessageLock);
	MesCall("AccumGCD");
	MUNLOCK(ErrorMessageLock);
	NumberFree(GCDbuffer,"AccumGCD");
	SETERROR(-1)
}

/*
 		#] AccumGCD : 
 		#[ TakeRatRoot:
*/

int TakeRatRoot(UWORD *a, WORD *n, WORD power)
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

WORD AddLong(UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
{
	WORD sgn, res;
	if ( na < 0 ) {
		if ( nb < 0 ) {
			if ( AddPLon(a,-na,b,-nb,c,nc) ) return(-1);
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
		else { return( AddPLon(a,na,b,nb,c,nc) ); }
	}
	if ( ( res = BigLong(a,na,b,nb) ) > 0 ) {
		SubPLon(a,na,b,nb,c,nc);
		if ( sgn < 0 ) *nc = -*nc;
	}
	else if ( res < 0 ) {
		SubPLon(b,nb,a,na,c,nc);
		if ( sgn > 0 ) *nc = -*nc;
	}
	else {
		*nc = 0;
		*c = 0;
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

WORD AddPLon(UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
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
			MLOCK(ErrorMessageLock);
			MesPrint("Overflow in addition");
			MUNLOCK(ErrorMessageLock);
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

VOID SubPLon(UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
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
	while ( nd && !*--c ) { nd--; }
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

WORD MulLong(UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
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
/*
  	#[ GMP stuff :
*/
#ifdef WITHGMP
	if (na > 3 && nb > 3) {
/*		mp_limb_t res;  */
		UWORD *to, *from;
		int j;
		GETIDENTITY
		UWORD *DLscrat9 = NumberMalloc("MulLong"), *DLscratA = NumberMalloc("MulLong"), *DLscratB = NumberMalloc("MulLong");
#if ( GMPSPREAD != 1 )
		if ( na & 1 ) {
			from = a; a = to = DLscrat9; j = na; NCOPY(to, from, j);
			a[na++] = 0;
			++*nc;
		} else
#endif
		if ( (LONG)a & (sizeof(mp_limb_t)-1) ) {
			from = a; a = to = DLscrat9; j = na; NCOPY(to, from, j);
		}

#if ( GMPSPREAD != 1 )
		if ( nb & 1 ) {
			from = b; b = to = DLscratA; j = nb; NCOPY(to, from, j);
			b[nb++] = 0;
			++*nc;
		} else
#endif
		if ( (LONG)b & (sizeof(mp_limb_t)-1) ) {
			from = b; b = to = DLscratA; j = nb; NCOPY(to, from, j);
		}

		if ( ( *nc > (WORD)i ) || ( (LONG)c & (LONG)(sizeof(mp_limb_t)-1) ) ) {
			ic = DLscratB;
		}
		if ( na < nb ) {
			/* res = */
			mpn_mul((mp_ptr)ic, (mp_srcptr)b, nb/GMPSPREAD, (mp_srcptr)a, na/GMPSPREAD);
		} else {
			/* res = */
			mpn_mul((mp_ptr)ic, (mp_srcptr)a, na/GMPSPREAD, (mp_srcptr)b, nb/GMPSPREAD);
		}
		while ( ic[i-1] == 0 ) i--;
		*nc = i;
/*
		if ( res == 0 ) *nc -= GMPSPREAD;
		else if ( res <= WORDMASK ) --*nc;
*/
		if ( ic != c ) {
			j = *nc; NCOPY(c, ic, j);
		}
		if ( sgn < 0 ) *nc = -(*nc);
		NumberFree(DLscrat9,"MulLong"); NumberFree(DLscratA,"MulLong"); NumberFree(DLscratB,"MulLong");
		return(0);
	}
#endif
/*
  	#] GMP stuff : 
*/
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
	MLOCK(ErrorMessageLock);
	MesPrint("Overflow in Multiplication");
	MUNLOCK(ErrorMessageLock);
	return(-1);
}

/*
 		#] MulLong : 
 		#[ BigLong :		WORD BigLong(a,na,b,nb)

	Returns > 0 if a > b, < 0 if b > a and 0 if a == b

*/

WORD BigLong(UWORD *a, WORD na, UWORD *b, WORD nb)
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

WORD DivLong(UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c,
             WORD *nc, UWORD *d, WORD *nd)
{
	WORD sgna = 1, sgnb = 1, ne, nf, ng, nh;
	WORD i, ni;
	UWORD *w1, *w2;
	RLONG t, v;
	UWORD *e, *f, *ff, *g, norm, estim;
#ifdef WITHGMP
	UWORD *DLscrat9, *DLscratA, *DLscratB, *DLscratC;
#endif
	RLONG esthelp;
	if ( !nb ) {
		MLOCK(ErrorMessageLock);
		MesPrint("Division by zero");
		MUNLOCK(ErrorMessageLock);
		return(-1);
	}
	if ( !na ) { *nc = *nd = 0; return(0); }
	if ( na < 0 ) { sgna = -sgna; na = -na; }
	if ( nb < 0 ) { sgnb = -sgnb; nb = -nb; }
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
/*
 		#[ GMP stuff :

		We start with copying a and b.
		Then we make space for c and d.
		Next we call mpn_tdiv_qr
		We adjust sizes and copy to c and d if needed.
		Finally the signs are settled.
*/
#ifdef WITHGMP
		if ( na > 4 && nb > 3 ) {
		  UWORD *ic, *id, *to, *from;
		  int j = na - nb;
		  DLscrat9 = NumberMalloc("DivLong"); DLscratA = NumberMalloc("DivLong");
		  DLscratB = NumberMalloc("DivLong"); DLscratC = NumberMalloc("DivLong");

#if ( GMPSPREAD != 1 )
		  if ( na & 1 ) {
			from = a; a = to = DLscrat9; i = na; NCOPY(to, from, i);
			a[na++] = 0;
		  } else
#endif
		  if ( (LONG)a & (sizeof(mp_limb_t)-1) ) {
			from = a; a = to = DLscrat9; i = na; NCOPY(to, from, i);
		  }

#if ( GMPSPREAD != 1 )
		  if ( nb & 1 ) {
			from = b; b = to = DLscratA; i = nb; NCOPY(to, from, i);
			b[nb++] = 0;
		  } else
#endif
		  if ( ( (LONG)b & (sizeof(mp_limb_t)-1) ) != 0 ) {
			from = b; b = to = DLscratA; i = nb; NCOPY(to, from, i);
		  }
		  if ( ( (LONG)c & (sizeof(mp_limb_t)-1) ) != 0 ) ic = DLscratB;
		  else                                            ic = c;

		  if ( ( (LONG)d & (sizeof(mp_limb_t)-1) ) != 0 ) id = DLscratC;
		  else                                            id = d;
		  mpn_tdiv_qr((mp_limb_t *)ic,(mp_limb_t *)id,(mp_size_t)0,
			(const mp_limb_t *)a,(mp_size_t)(na/GMPSPREAD),
			(const mp_limb_t *)b,(mp_size_t)(nb/GMPSPREAD));
		  while ( j >= 0 && ic[j] == 0 ) j--;
		  j++; *nc = j;
		  if ( c != ic ) { NCOPY(c,ic,j); }
		  j = nb-1;
		  while ( j >= 0 && id[j] == 0 ) j--;
		  j++; *nd = j;
		  if ( d != id ) { NCOPY(d,id,j); }
		  if ( sgna < 0 ) { *nc = -(*nc); *nd = -(*nd); }
		  if ( sgnb < 0 ) { *nc = -(*nc); }
		  NumberFree(DLscrat9,"DivLong"); NumberFree(DLscratA,"DivLong");
		  NumberFree(DLscratB,"DivLong"); NumberFree(DLscratC,"DivLong");
		  return(0);
		}
#endif
/*
 		#] GMP stuff : 
*/
		/* Start with normalization operation */
 
		e = NumberMalloc("DivLong"); f = NumberMalloc("DivLong"); g = NumberMalloc("DivLong");
		if ( b[nb-1] == (FULLMAX-1) ) norm = 1;
		else {
			norm = (UWORD)(((ULONG)FULLMAX) / (ULONG)((b[nb-1]+1L)));
		}
		f[na] = 0;
		if ( MulLong(b,nb,&norm,1,e,&ne) ||
		     MulLong(a,na,&norm,1,f,&nf) ) {
			NumberFree(e,"DivLong"); NumberFree(f,"DivLong"); NumberFree(g,"DivLong");
			return(-1);
		}
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
				while ( ( nh > 0 ) && !*w2 ) { nh--; w2--; }
			}
			if ( BigLong(f+ni,nh,e,ne) >= 0 ) {
				estim++;
				SubPLon(f+ni,nh,e,ne,f+ni,&nh);
				if ( BigLong(f+ni,nh,e,ne) >= 0 ) {
					estim++;
					SubPLon(f+ni,nh,e,ne,f+ni,&nh);
					if ( BigLong(f+ni,nh,e,ne) >= 0 ) {
						MLOCK(ErrorMessageLock);
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
						MUNLOCK(ErrorMessageLock);
						NumberFree(e,"DivLong"); NumberFree(f,"DivLong"); NumberFree(g,"DivLong");
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
				*nd = i = nh; ff = f;
				NCOPY(d,ff,i);
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
					MLOCK(ErrorMessageLock);
					MesPrint("Error in DivLong");
					MUNLOCK(ErrorMessageLock);
					NumberFree(e,"DivLong"); NumberFree(f,"DivLong"); NumberFree(g,"DivLong");
					return(-1);
				}
				if ( !*(d+nh-1) ) (*nd)--;
			}
		}
		else { *nd = 0; }
		NumberFree(e,"DivLong"); NumberFree(f,"DivLong"); NumberFree(g,"DivLong");
	}
	if ( sgna < 0 ) { *nc = -(*nc); *nd = -(*nd); }
	if ( sgnb < 0 ) { *nc = -(*nc); }
	return(0);
}

/*
 		#] DivLong : 
 		#[ RaisPow :		WORD RaisPow(a,na,b)

	Raises a to the power b. a is a Long integer and b >= 0.
	The method that is used works with a bitdecomposition of b.
*/

WORD RaisPow(PHEAD UWORD *a, WORD *na, UWORD b)
{
	GETBIDENTITY
	WORD i, nu;
	UWORD *it, *iu, c;
	UWORD *is, *iss;
	WORD ns, nt, nmod;
	nmod = ABS(AN.ncmod);
	if ( !*na || ( ( *na == 1 ) && ( *a == 1 ) ) ) return(0);
	if ( !b ) {	*na=1; *a=1; return(0); }
	is = NumberMalloc("RaisPow");
	it = NumberMalloc("RaisPow");
	for ( i = 0; i < ABS(*na); i++ ) is[i] = a[i];
	ns = *na;
	c = b;
	for ( i = 0; i < BITSINWORD; i++ ) {
		if ( !c ) break;
		c /= 2;
	}
	i--;
	c = 1 << i;
	while ( --i >= 0 ) {
		c /= 2;
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
	if ( ( nmod != 0 ) && ( ( AC.modmode & POSNEG ) != 0 ) ) {
		NormalModulus(is,&ns);
	}
	if ( ( *na = i = ns ) != 0 ) { iss = is; i=ABS(i); NCOPY(a,iss,i); }
	NumberFree(is,"RaisPow"); NumberFree(it,"RaisPow");
	return(0);
RaisOvl:
	MLOCK(ErrorMessageLock);
	MesCall("RaisPow");
	MUNLOCK(ErrorMessageLock);
	NumberFree(is,"RaisPow"); NumberFree(it,"RaisPow");
	SETERROR(-1)
}

/*
 		#] RaisPow : 
 		#[ RaisPowCached :
*/

/**  Computes power x^n and caches the value
 *
 *   Description
 *   ===========
 *   Calculates the power x^n and stores the results for caching
 *   purposes. The pointer c (i.e., the pointer, and not what it
 *   points to) is overwritten. What it points to should not be
 *   overwritten in the calling function.
 *
 *   Notes
 *   =====
 *   - Caching is done in AT.small_power[]. This array is extended
 *     if necessary.
 */
VOID RaisPowCached (PHEAD WORD x, WORD n, UWORD **c, WORD *nc) {

	int i,j;
	WORD new_small_power_maxx, new_small_power_maxn, ID;
	WORD *new_small_power_n;
	UWORD **new_small_power;
	
	/* check whether to extend the array */
	if (x>=AT.small_power_maxx || n>=AT.small_power_maxn) {

		new_small_power_maxx = AT.small_power_maxx;
		if (x>=AT.small_power_maxx)
			new_small_power_maxx = MaX(2*AT.small_power_maxx, x+1);
		
		new_small_power_maxn = AT.small_power_maxn;
		if (n>=AT.small_power_maxn)
			new_small_power_maxn = MaX(2*AT.small_power_maxn, n+1);
		
		new_small_power_n = (WORD*) Malloc1(new_small_power_maxx*new_small_power_maxn*sizeof(WORD),"RaisPowCached");
		new_small_power = (UWORD **) Malloc1(new_small_power_maxx*new_small_power_maxn*sizeof(UWORD *),"RaisPowCached");

		for (i=0; i<new_small_power_maxx * new_small_power_maxn; i++) {
			new_small_power_n[i] = 0;
			new_small_power  [i] = NULL;
		}
					 
		for (i=0; i<AT.small_power_maxx; i++) 
			for (j=0; j<AT.small_power_maxn; j++) {
				new_small_power_n[i*new_small_power_maxn+j] =	AT.small_power_n[i*AT.small_power_maxn+j];
				new_small_power  [i*new_small_power_maxn+j] = AT.small_power  [i*AT.small_power_maxn+j];
			}

		if (AT.small_power_n != NULL) {
			M_free(AT.small_power_n,"RaisPowCached");
			M_free(AT.small_power,"RaisPowCached");
		}

		AT.small_power_maxx = new_small_power_maxx;
		AT.small_power_maxn = new_small_power_maxn;
		AT.small_power_n    = new_small_power_n;
		AT.small_power      = new_small_power;				
	}

	/* check whether the results is already calculated */
	ID = x * AT.small_power_maxn + n;

	if (AT.small_power[ID] == NULL) {
#ifdef OLDRAISPOWCACHED
        AT.small_power[ID] = NumberMalloc("RaisPowCached");
        AT.small_power_n[ID] = 1;
        AT.small_power[ID][0] = x;
        RaisPow(BHEAD AT.small_power[ID],&AT.small_power_n[ID],n);
#else
        UWORD *c = NumberMalloc("RaisPowCached");
        WORD i, k = 1;
		c[0] = x;
        RaisPow(BHEAD c,&k,n);
/*
        And now get the proper amount.
*/
        if ( AT.InNumMem < k ) {    /* We should start a new buffer */
            AT.InNumMem = 5*AM.MaxTal;
            AT.NumMem = (UWORD *)Malloc1(AT.InNumMem*sizeof(UWORD),"RaisPowCached");
/*
			MesPrint("  Got an extra %l UWORDS in RaisPowCached",AT.InNumMem);
*/
        }
        for ( i = 0; i < k; i++ ) AT.NumMem[i] = c[i];
        AT.small_power[ID] = AT.NumMem;
        AT.small_power_n[ID] = k;
        AT.NumMem += k;
        AT.InNumMem -= k;
        NumberFree(c,"RaisPowCached");
#endif
	}

	/* return the result */
	*c  = AT.small_power[ID];
	*nc = AT.small_power_n[ID];
}

/*
 		#] RaisPowCached : 
	 	#[ RaisPowMod :
		
   Computes the power x^n mod m
 */
WORD RaisPowMod (WORD x, WORD n, WORD m) {
	LONG y=1, z=x;
	while (n) {
		if (n&1) { y*=z; y%=m; }
		z*=z; z%=m;
		n /= 2;
	}
	return (WORD)y;
}

/*
  	#] RaisPowMod : 
 		#[ NormalModulus :  int NormalModulus(UWORD *a,WORD *na)
*/
/**
 *	Brings a modular representation in the range -p/2 to +p/2
 *	The return value tells whether anything was done.
 *	Routine made in the general modulus revamp of July 2008 (JV).
 */

int NormalModulus(UWORD *a,WORD *na)
{
	WORD n;
	if ( AC.halfmod == 0 ) {
	  LOCK(AC.halfmodlock);
	  if ( AC.halfmod == 0 ) {
		UWORD two[1],remain[1];
		WORD dummy;
		two[0] = 2;
		AC.halfmod = (UWORD *)Malloc1((ABS(AC.ncmod))*sizeof(UWORD),"halfmod");
		DivLong((UWORD *)AC.cmod,(ABS(AC.ncmod)),two,1
				,(UWORD *)AC.halfmod,&(AC.nhalfmod),remain,&dummy);
	  }
	  UNLOCK(AC.halfmodlock);
	}
	n = ABS(*na);
	if ( BigLong(a,n,AC.halfmod,AC.nhalfmod) > 0 ) {
		SubPLon((UWORD *)AC.cmod,(ABS(AC.ncmod)),a,n,a,&n);
		if ( *na > 0 ) { *na = -n; }
		else { *na = n; }
		return(1);
	}
	return(0);
}

/*
 		#] NormalModulus : 
 		#[ MakeInverses :
*/
/**
 *	Makes a table of inverses in modular calculus
 *	The modulus is in AC.cmod and AC.ncmod
 *	One should notice that the table of inverses can only be made if
 *	the modulus fits inside a single FORM word. Otherwise the table lookup
 *	becomes too difficult and the table too long.
 */

int MakeInverses()
{
	WORD n = AC.cmod[0], i, inv2;
	if ( AC.ncmod != 1 ) return(1);
	if ( AC.modinverses == 0 ) {
	  LOCK(AC.halfmodlock);
	  if ( AC.modinverses == 0 ) {
		AC.modinverses = (UWORD *)Malloc1(n*sizeof(UWORD),"modinverses");
		AC.modinverses[0] = 0;
		AC.modinverses[1] = 1;
		for ( i = 2; i < n; i++ ) {
			if ( GetModInverses(i,n,
						(WORD *)(&(AC.modinverses[i])),&inv2) ) {
				SETERROR(-1)
			}
		}
	  }
	  UNLOCK(AC.halfmodlock);
	}
	return(0);
}

/*
 		#] MakeInverses : 
 		#[ GetModInverses :
*/
/**
 *	Input m1 and m2, which are relative prime.
 *	determines a*m1+b*m2 = 1  (and 1 is the gcd of m1 and m2)
 *	then a*m1 = 1 mod m2 and hence im1 = a.
 *	and  b*m2 = 1 mod m1 and hence im2 = b.
 *	Set m1 = 0*m1+1*m2 = a1*m1+b1*m2
 *	    m2 = 1*m1+0*m2 = a2*m1+b2*m2
 *	If everything is OK, the return value is zero
 */

int GetModInverses(WORD m1, WORD m2, WORD *im1, WORD *im2)
{
	WORD a1, a2, a3;
	WORD b1, b2, b3;
	WORD x = m1, y, c, d = m2;
	if ( x < 1 || d <= 1 ) goto somethingwrong;
	a1 = 0; a2 = 1;
	b1 = 1; b2 = 0;
	for(;;) {
		c = d/x; y = d%x; /* a good compiler makes this faster than y=d-c*x */
		if ( y == 0 ) break;
		a3 = a1-c*a2; a1 = a2; a2 = a3;
		b3 = b1-c*b2; b1 = b2; b2 = b3;
		d = x; x = y;
	}
	if ( x != 1 ) goto somethingwrong;
	if ( a2 < 0 ) a2 += m2;
	if ( b2 < 0 ) b2 += m1;
	if (im1!=NULL) *im1 = a2;
	if (im2!=NULL) *im2 = b2;
	return(0);
somethingwrong:
	MLOCK(ErrorMessageLock);
	MesPrint("Error trying to determine inverses in GetModInverses");
	MUNLOCK(ErrorMessageLock);
	return(-1);
}
/*
 		#] GetModInverses : 
 		#[ GetLongModInverses :
*/

int GetLongModInverses(PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *ia, WORD *nia, UWORD *ib, WORD *nib) {

	UWORD *s, *t, *sa, *sb, *ta, *tb, *x, *y, *swap1;
	WORD ns, nt, nsa, nsb, nta, ntb, nx, ny, swap2;
		 
	s = NumberMalloc("GetLongModInverses");
	ns = na;
	WCOPY(s, a, ABS(ns));

	t = NumberMalloc("GetLongModInverses");
	nt = nb;
	WCOPY(t, b, ABS(nt));

	sa = NumberMalloc("GetLongModInverses");
	nsa = 1;
	sa[0] = 1;

	sb = NumberMalloc("GetLongModInverses");
	nsb = 0;

	ta = NumberMalloc("GetLongModInverses");
	nta = 0;

	tb = NumberMalloc("GetLongModInverses");
	ntb = 1;
	tb[0] = 1;

	x = NumberMalloc("GetLongModInverses");
	y = NumberMalloc("GetLongModInverses");

	while (nt != 0) {
		DivLong(s,ns,t,nt,x,&nx,y,&ny);
		swap1=s; s=y; y=swap1;
		ns=ny;
		MulLong(x,nx,ta,nta,y,&ny);
		AddLong(sa,nsa,y,-ny,sa,&nsa);
		MulLong(x,nx,tb,ntb,y,&ny);
		AddLong(sb,nsb,y,-ny,sb,&nsb);

		swap1=s; s=t; t=swap1;
		swap2=ns; ns=nt; nt=swap2;
		swap1=sa; sa=ta; ta=swap1;
		swap2=nsa; nsa=nta; nta=swap2;
		swap1=sb; sb=tb; tb=swap1;
		swap2=nsb; nsb=ntb; ntb=swap2;
	}

	if (ia!=NULL) {
		*nia = nsa*ns;
		WCOPY(ia,sa,ABS(*nia));
	}

	if (ib!=NULL) {
		*nib = nsb*ns;
		WCOPY(ib,sb,ABS(*nib));
	}
	
	NumberFree(s,"GetLongModInverses");
	NumberFree(t,"GetLongModInverses");
	NumberFree(sa,"GetLongModInverses");
	NumberFree(sb,"GetLongModInverses");
	NumberFree(ta,"GetLongModInverses");
	NumberFree(tb,"GetLongModInverses");
	NumberFree(x,"GetLongModInverses");
	NumberFree(y,"GetLongModInverses");

	return 0;
}

/*
 		#] GetLongModInverses : 
 		#[ Product :		WORD Product(a,na,b)

	Multiplies the Long number in a with the WORD b.

*/

WORD Product(UWORD *a, WORD *na, WORD b)
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
			MLOCK(ErrorMessageLock);
			MesPrint("Overflow in Product");
			MUNLOCK(ErrorMessageLock);
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

UWORD Quotient(UWORD *a, WORD *na, WORD b)
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

WORD Remain10(UWORD *a, WORD *na)
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

WORD Remain4(UWORD *a, WORD *na)
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

	Puts the long number a in string s.

*/

VOID PrtLong(UWORD *a, WORD na, UBYTE *s)
{
	GETIDENTITY
	WORD q, i;
	UBYTE *sa, *sb;
	UBYTE c;
	UWORD *bb, *b;

	if ( na < 0 ) {
		*s++ = '-';
		na = -na;
	}

	b = NumberMalloc("PrtLong");
	bb = b;
	i = na; while ( --i >= 0 ) *bb++ = *a++;
	a = b;
	if ( na > 2 ) {
		sa = s;
		do {
			q = Remain4(a,&na);
			*sa++ = (UBYTE)('0' + (q%10));
			q /= 10;
			*sa++ = (UBYTE)('0' + (q%10));
			q /= 10;
			*sa++ = (UBYTE)('0' + (q%10));
			q /= 10;
			*sa++ = (UBYTE)('0' + (q%10));
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
			*sa++ = (UBYTE)('0' + q);
		} while ( na );
		sb = s;
		s = sa;
		sa--;
		while ( sa > sb ) { c = *sa; *sa = *sb; *sb = c; sa--; sb++; }
	}
	else *s++ = '0';
	*s = '\0';
	NumberFree(b,"PrtLong");
}

/*
 		#] PrtLong : 
 		#[ GetLong :		WORD GetLong(s,a,na)

	Reads a long number from a string.
	The string is zero terminated and contains only digits!

	New algorithm: try to read 4 digits together before the result
	is accumulated.
*/

WORD GetLong(UBYTE *s, UWORD *a, WORD *na)
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

	NOTE!!!!!. NumberMalloc gets called and it may not be freed
*/

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

VOID GCD(UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
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
		MLOCK(ErrorMessageLock);
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
		MUNLOCK(ErrorMessageLock);
	}
*/
/*
	We have now that A > B
	The loop recognizes the case that na-nb >= 1
	In that case we just have to divide!
*/
	r = x1 = NumberMalloc("GCD"); t = x2 = NumberMalloc("GCD"); x3 = NumberMalloc("GCD");
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
		NumberFree(x1,"GCD"); NumberFree(x2,"GCD"); NumberFree(x3,"GCD");
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
		NumberFree(x1,"GCD"); NumberFree(x2,"GCD"); NumberFree(x3,"GCD");
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
/*
  	#[ Old Routine :
*/

WORD GcdLong(PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
{
	GETBIDENTITY
	if ( !na || !nb ) {
		if ( !na && !nb ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Cannot take gcd");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		
		if ( !na ) {
			*nc = abs(nb);
			NCOPY(c,b,*nc);
			*nc = abs(nb);
			return(0);
		}
		
		*nc = abs(na);
		NCOPY(c,a,*nc);
		*nc = abs(na);
		return(0);
	}
	if ( na < 0 ) na = -na;
	if ( nb < 0 ) nb = -nb;
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
		UWORD *x3,*x1,*x2, *GLscrat7, *GLscrat8;
		WORD n1,n2,n3,n4;
		WORD i, j;
		x1 = c; x3 = a; n1 = i = na;
		NCOPY(x1,x3,i);
		GLscrat7 = NumberMalloc("GcdLong"); GLscrat8 = NumberMalloc("GcdLong");
		x2 = GLscrat8; x3 = b; n2 = i = nb;
		NCOPY(x2,x3,i);
		x1 = c; i = 0;
		while ( x1[0] == 0 ) { i += BITSINWORD; x1++; n1--; }
		while ( ( x1[0] & 1 ) == 0 ) { i++; SCHUIF(x1,n1) }
		x2 = GLscrat8; j = 0;
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
					if ( DivLong(x2,n2,x1,n1,GLscrat7,&n3,x2,&n4) ) goto GcdErr;
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
					if ( DivLong(x1,n1,x2,n2,GLscrat7,&n3,x1,&n4) ) goto GcdErr;
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
		NumberFree(GLscrat7,"GcdLong"); NumberFree(GLscrat8,"GcdLong");
#else
		UWORD *x1,*x2,*x3,*x4,*c1,*c2;
		WORD n1,n2,n3,n4,i;
		x1 = c; x3 = a; n1 = i = na;
		NCOPY(x1,x3,i);
		x1 = c; c1 = x2 = NumberMalloc("GcdLong"); x3 = NumberMalloc("GcdLong"); x4 = NumberMalloc("GcdLong");
		c2 = b; n2 = i = nb;
		NCOPY(c1,c2,i);
		for(;;){
			if ( DivLong(x1,n1,x2,n2,x4,&n4,x3,&n3) ) goto GcdErr;
			if ( !n3 ) { x1 = x2; n1 = n2; break; }
			if ( DivLong(x2,n2,x3,n3,x4,&n4,x1,&n1) ) goto GcdErr;
			if ( !n1 ) { x1 = x3; n1 = n3; break; }
			if ( DivLong(x3,n3,x1,n1,x4,&n4,x2,&n2) ) goto GcdErr;
			if ( !n2 ) {
				*nc = n1;
				NumberFree(x2,"GcdLong"); NumberFree(x3,"GcdLong"); NumberFree(x4,"GcdLong");
				return(0);
			}
		}
		*nc = i = n1;
		NCOPY(c,x1,i);
		NumberFree(x2,"GcdLong"); NumberFree(x3,"GcdLong"); NumberFree(x4,"GcdLong");
#endif
#endif
	}
	return(0);
GcdErr:
	MLOCK(ErrorMessageLock);
	MesCall("GcdLong");
	MUNLOCK(ErrorMessageLock);
	SETERROR(-1)
}
/*
  	#] Old Routine : 
*/
#else

/*
	New routine for GcdLong that uses smart shortcuts.
	Algorithm by J. Vermaseren 15-nov-2006.
	It runs faster for very big numbers but only by a fixed factor.
	There is no improvement in the power behaviour.
	Improvement on the whole of hf9 (multiple zeta values at weight 9):
		Better than a factor 2 on a 32 bits architecture and 2.76 on a
		64 bits architecture.
	On hf10 (MZV's at weight 10), 64 bits architecture: factor 7.

	If we have two long numbers (na,nb > GCDMAX) we will work in a
	truncated way. At the moment of writing (15-nov-2006) it isn't
	clear whether this algorithm is an invention or a reinvention.
	A short search on the web didn't show anything.

	31-jul-2007:
	A better search shows that this is an adaptation of the Lehmer-Euclid
	algorithm, already described in Knuth. Here we can work without upper
	and lower limit because we are only interested in the GCD, not the
	extra numbers. Also it takes already some features of the double
	digit Lehmer-Euclid algorithm of Jebelean it seems.

	Maybe this can be programmed slightly better and we can get another
	few percent speed increase. Further improvements for the assymptotic
	case come from splitting the calculation as in Karatsuba and working
	with FFT divisions and multiplications etc. But this is when hundreds
	of words are involved at the least.

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

WORD GcdLong(PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
{
	GETBIDENTITY
	UWORD x,y,z;
	UWORD *x1,*x2,*x3,*x4,*x5,*d;
	UWORD *GLscrat6, *GLscrat7, *GLscrat8, *GLscrat9, *GLscrat10;
	WORD n1,n2,n3,n4,n5,i;
	RLONG lx,ly,lz;
	LONG ma1, ma2, mb1, mb2, mc1, mc2, m;
	if ( !na || !nb ) {
		if ( !na && !nb ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Cannot take gcd");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		
		if ( !na ) {
			*nc = abs(nb);
			NCOPY(c,b,*nc);
			*nc = abs(nb);
			return(0);
		}
		
		*nc = abs(na);
		NCOPY(c,a,*nc);
		*nc = abs(na);
		return(0);
	}
	if ( na < 0 ) na = -na;
	if ( nb < 0 ) nb = -nb;
/*
  	#[ GMP stuff :
*/
#ifdef WITHGMP
	if ( na > 3 && nb > 3 ) {
		int ii;
		mp_limb_t *upa, *upb, *upc, xx;
		UWORD *uw, *u1, *u2;
		unsigned int tcounta, tcountb, tcounta1, tcountb1;
		mp_size_t ana, anb, anc;

		u1 = uw = NumberMalloc("GcdLong");
		upa = (mp_limb_t *)u1;
		ana = na; tcounta1 = 0;
		while ( a[0] == 0 ) { a++; ana--; tcounta1++; }
		for ( ii = 0; ii < ana; ii++ ) { *uw++ = *a++; }
		if ( ( ana & 1 ) != 0 ) { *uw = 0; ana++; }
		ana /= 2;

		u2 = uw = NumberMalloc("GcdLong");
		upb = (mp_limb_t *)u2;
		anb = nb; tcountb1 = 0;
		while ( b[0] == 0 ) { b++; anb--; tcountb1++; }
		for ( ii = 0; ii < anb; ii++ ) { *uw++ = *b++; }
		if ( ( anb & 1 ) != 0 ) { *uw = 0; anb++; }
		anb /= 2;

		xx = upa[0]; tcounta = 0;
		while ( ( xx & 15 ) == 0 ) { tcounta += 4; xx >>= 4; }
		while ( ( xx &  1 ) == 0 ) { tcounta += 1; xx >>= 1; }
		xx = upb[0]; tcountb = 0;
		while ( ( xx & 15 ) == 0 ) { tcountb += 4; xx >>= 4; }
		while ( ( xx &  1 ) == 0 ) { tcountb += 1; xx >>= 1; }

		if ( tcounta ) {
			mpn_rshift(upa,upa,ana,tcounta);
			if ( upa[ana-1] == 0 ) ana--;
		}
		if ( tcountb ) {
			mpn_rshift(upb,upb,anb,tcountb);
			if ( upb[anb-1] == 0 ) anb--;
		}

		upc = (mp_limb_t *)(NumberMalloc("GcdLong"));
		if ( ( ana > anb ) || ( ( ana == anb ) && ( upa[ana-1] >= upb[ana-1] ) ) ) {
			anc = mpn_gcd(upc,upa,ana,upb,anb);
		}
		else {
			anc = mpn_gcd(upc,upb,anb,upa,ana);
		}

		tcounta = tcounta1*BITSINWORD + tcounta;
		tcountb = tcountb1*BITSINWORD + tcountb;
		if ( tcountb > tcounta ) tcountb = tcounta;
		tcounta = tcountb/BITSINWORD;
		tcountb = tcountb%BITSINWORD;

		if ( tcountb ) {
			xx = mpn_lshift(upc,upc,anc,tcountb);
			if ( xx ) { upc[anc] = xx; anc++; }
		}

		uw = (UWORD *)upc; anc *= 2;
		while ( uw[anc-1] == 0 ) anc--;
		for ( ii = 0; ii < (int)tcounta; ii++ ) *c++ = 0;
		for ( ii = 0; ii < anc; ii++ ) *c++ = *uw++;
		*nc = anc + tcounta;
		NumberFree(u1,"GcdLong"); NumberFree(u2,"GcdLong"); NumberFree((UWORD *)(upc),"GcdLong");
		return(0);
	}
#endif
/*
  	#] GMP stuff : 
*/
/*
  	#[ Easy cases :
*/
	if ( na == 1 && nb == 1 ) {
		x = *a;
		y = *b;
		do { z = x % y; x = y; } while ( ( y = z ) != 0 );
		*c = x;
		*nc = 1;
		return(0);
	}
	else if ( na <= 2 && nb <= 2 ) {
		if ( na == 2 ) { lx = (((RLONG)(a[1]))<<BITSINWORD) + *a; }
		else { lx = *a; }
		if ( nb == 2 ) { ly = (((RLONG)(b[1]))<<BITSINWORD) + *b; }
		else { ly = *b; }
		if ( lx < ly ) { lz = lx; lx = ly; ly = lz; }
#if ( BITSINWORD == 16 )
		do {
			lz = lx % ly; lx = ly;
		} while ( ( ly = lz ) != 0 );
#else
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
#endif
		{
			*c++ = (UWORD)lx;
			if ( ( *c = (UWORD)(lx >> BITSINWORD) ) != 0 ) *nc = 2;
			else *nc = 1;
		}
		return(0);
	}
/*
  	#] Easy cases : 
*/
	GLscrat6 = NumberMalloc("GcdLong"); GLscrat7 = NumberMalloc("GcdLong");
	GLscrat8 = NumberMalloc("GcdLong");
	GLscrat9 = NumberMalloc("GcdLong"); GLscrat10 = NumberMalloc("GcdLong");
restart:;
/*
  	#[ Easy cases :
*/
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
#if ( BITSINWORD == 16 )
		do {
			lz = lx % ly; lx = ly;
		} while ( ( ly = lz ) != 0 );
#else
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
#endif
		{
			*c++ = (UWORD)lx;
			if ( ( *c = (UWORD)(lx >> BITSINWORD) ) != 0 ) *nc = 2;
			else *nc = 1;
		}
	}
/*
  	#] Easy cases : 
  	#[ Original code :
*/
	else if ( na < GCDMAX || nb < GCDMAX || na != nb ) {
		if ( na < nb ) {
			x2 = GLscrat8; x3 = a; n2 = i = na;
			NCOPY(x2,x3,i);
			x1 = c; x3 = b; n1 = i = nb;
			NCOPY(x1,x3,i);
		}
		else {
			x1 = c; x3 = a; n1 = i = na;
			NCOPY(x1,x3,i);
			x2 = GLscrat8; x3 = b; n2 = i = nb;
			NCOPY(x2,x3,i);
		}
		x1 = c; x2 = GLscrat8; x3 = GLscrat7; x4 = GLscrat6;
		for(;;){
			if ( DivLong(x1,n1,x2,n2,x4,&n4,x3,&n3) ) goto GcdErr;
			if ( !n3 ) { x1 = x2; n1 = n2; break; }
			if ( n2 <= 2 ) { a = x2; b = x3; na = n2; nb = n3; goto restart; }
			if ( n3 >= GCDMAX && n2 == n3 ) {
				a = GLscrat9; b = GLscrat10; na = n2; nb = n3;
				for ( i = 0; i < na; i++ ) a[i] = x2[i];
				for ( i = 0; i < nb; i++ ) b[i] = x3[i];
				goto newtrick;
			}
			if ( DivLong(x2,n2,x3,n3,x4,&n4,x1,&n1) ) goto GcdErr;
			if ( !n1 ) { x1 = x3; n1 = n3; break; }
			if ( n3 <= 2 ) { a = x3; b = x1; na = n3; nb = n1; goto restart; }
			if ( n1 >= GCDMAX && n1 == n3 ) {
				a = GLscrat9; b = GLscrat10; na = n3; nb = n1;
				for ( i = 0; i < na; i++ ) a[i] = x3[i];
				for ( i = 0; i < nb; i++ ) b[i] = x1[i];
				goto newtrick;
			}
			if ( DivLong(x3,n3,x1,n1,x4,&n4,x2,&n2) ) goto GcdErr;
			if ( !n2 ) { *nc = n1; goto normalend; }
			if ( n1 <= 2 ) { a = x1; b = x2; na = n1; nb = n2; goto restart; }
			if ( n2 >= GCDMAX && n2 == n1 ) {
				a = GLscrat9; b = GLscrat10; na = n1; nb = n2;
				for ( i = 0; i < na; i++ ) a[i] = x1[i];
				for ( i = 0; i < nb; i++ ) b[i] = x2[i];
				goto newtrick;
			}
		}
		*nc = i = n1;
		NCOPY(c,x1,i);
	}
/*
  	#] Original code : 
  	#[ New code :
*/
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
		ma1 = 1; ma2 = 0; mb1 = 0; mb2 = 1;
		lx = (((RLONG)(a[na-1]))<<BITSINWORD) + a[na-2];
		ly = (((RLONG)(b[nb-1]))<<BITSINWORD) + b[nb-2];
		if ( ly > lx ) { lz = lx; lx = ly; ly = lz; d = a; a = b; b = d; }
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
		x1 = GLscrat6;
		x2 = GLscrat7;
		x3 = GLscrat8;
		x5 = GLscrat10;
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
		if ( nb == 0 ) { *nc = n4; goto normalend; }
		x4 = GLscrat9; 
		for ( i = 0; i < na; i++ ) x4[i] = a[i];
		a = x4;
		if ( na < 0 ) na = -na;
		if ( nb < 0 ) nb = -nb;
/*
		The typical case now is that in a we have the last step to go
		to loose the leading word, while in b we have lost the leading word.
		We could go to DivLong now but we can also add an extra step that
		is less wasteful.
		In the case that the new leading word of b is extrememly short (like 1)
		we make a rather large error of course. In the worst case the whole
		will be intercepted by DivLong after all, but that is so rare that
		it shouldn't influence any timing in a measurable way.
*/
		if ( nb >= GCDMAX && na == nb+1 && b[nb-1] >= HALFMAX && b[nb-1] > a[na-1] ) {
			lx = (((RLONG)(a[na-1]))<<BITSINWORD) + a[na-2];
			x1[0] = lx/b[nb-1]; n1 = 1;
			MulLong(b,nb,x1,n1,x2,&n2);
			n2 = -n2;
			AddLong(a,na,x2,n2,x4,&n4);
			if ( n4 == 0 ) {
				*nc = nb;
				for ( i = 0; i < nb; i++ ) c[i] = b[i];
				goto normalend;
			}
			if ( n4 < 0 ) n4 = -n4;
			a = b; na = nb; b = x4; nb = n4;
		}
		goto restart;
/*
  	#] New code : 
*/
	}
normalend:
	NumberFree(GLscrat6,"GcdLong"); NumberFree(GLscrat7,"GcdLong"); NumberFree(GLscrat8,"GcdLong");
	NumberFree(GLscrat9,"GcdLong"); NumberFree(GLscrat10,"GcdLong");
	return(0);
GcdErr:
	MLOCK(ErrorMessageLock);
	MesCall("GcdLong");
	MUNLOCK(ErrorMessageLock);
	NumberFree(GLscrat6,"GcdLong"); NumberFree(GLscrat7,"GcdLong"); NumberFree(GLscrat8,"GcdLong");
	NumberFree(GLscrat9,"GcdLong"); NumberFree(GLscrat10,"GcdLong");
	SETERROR(-1)
}

#endif

/*
 		#] GcdLong : 
 		#[ GetBinom :		WORD GetBinom(a,na,i1,i2)
*/

WORD GetBinom(UWORD *a, WORD *na, WORD i1, WORD i2)
{
	GETIDENTITY
	WORD j, k, l;
	UWORD *GBscrat3, *GBscrat4;
	if ( i1-i2 < i2 ) i2 = i1-i2;
	if ( i2 == 0 ) { *a = 1; *na = 1; return(0); }
	if ( i2 > i1 ) { *a = 0; *na = 0; return(0); }
	*a = i1; *na = 1;
	GBscrat3 = NumberMalloc("GetBinom"); GBscrat4 = NumberMalloc("GetBinom");
	for ( j = 2; j <= i2; j++ ) {
		GBscrat3[0] = i1+1-j;
		if ( MulLong(a,*na,GBscrat3,(WORD)1,GBscrat4,&k) ) goto CalledFrom;
		GBscrat3[0] = j;
		if ( DivLong(GBscrat4,k,GBscrat3,(WORD)1,a,na,GBscrat3,&l) ) goto CalledFrom;
	}
	NumberFree(GBscrat3,"GetBinom"); NumberFree(GBscrat4,"GetBinom");
	return(0);
CalledFrom:
	MLOCK(ErrorMessageLock);
	MesCall("GetBinom");
	MUNLOCK(ErrorMessageLock);
	NumberFree(GBscrat3,"GetBinom"); NumberFree(GBscrat4,"GetBinom");
	SETERROR(-1)
}

/*
 		#] GetBinom : 
 		#[ LcmLong :		WORD LcmLong(a,na,b,nb)

		Computes the LCM of the long numbers a and b and puts the result
		in c. c is allowed to be equal to a.
*/

WORD LcmLong(PHEAD UWORD *a, WORD na, UWORD *b, WORD nb, UWORD *c, WORD *nc)
{
	WORD error = 0;
	UWORD *d = NumberMalloc("LcmLong");
	UWORD *e = NumberMalloc("LcmLong");
	UWORD *f = NumberMalloc("LcmLong");
	WORD nd, ne, nf;
	GcdLong(BHEAD a, na, b, nb, d, &nd);
	DivLong(a,na,d,nd,e,&ne,f,&nf);
	if ( MulLong(b,nb,e,ne,c,nc) ) {
		MLOCK(ErrorMessageLock);
		MesCall("LcmLong");
		MUNLOCK(ErrorMessageLock);
		error = -1;
	}
	NumberFree(f,"LcmLong");
	NumberFree(e,"LcmLong");
	NumberFree(d,"LcmLong");
	return(error);
}

/*
 		#] LcmLong : 
 		#[ TakeLongRoot:	int TakeLongRoot(a,n,power)

	Takes the 'power'-root of the long number in a.
	If the root could be taken the return value is zero.
	If the root could not be taken, the return value is 1.
	The root will be in a if it could be taken, otherwise there will be garbage
	Algorithm: (assume b is guess of root, b' better guess)
		b' = (a-(power-1)*b^power)/(n*b^(power-1))
	Note: power should be positive!
*/

int TakeLongRoot(UWORD *a, WORD *n, WORD power)
{
	GETIDENTITY
	int numbits, guessbits, i, retval = 0;
	UWORD x, *b, *c, *d, *e;
	WORD na, nb, nc, nd, ne;
	if ( *n < 0 && ( power & 1 ) == 0 ) return(1);
	if ( power == 1 ) return(0);
	if ( *n < 0 ) { na = -*n; }
	else          { na =  *n; }
	if ( na == 1 ) {
/*			Special cases that are the most frequent */
		if ( a[0] == 1 ) return(0);
		if ( power < BITSINWORD && na == 1 && a[0] == (UWORD)(1<<power) ) {
			a[0] = 2; return(0);
		}
		if ( 2*power < BITSINWORD && na == 1 && a[0] == (UWORD)(1<<(2*power)) ) {
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
/*
	The recursion is:
	(b'-b) = (a/b^(power-1)-b)/n
	       = (a/c-b)/n
	       = (d-b)/n    (remainder of a/c is e)
	       = c/n  (we reuse the scratch array c)
	Termination can be tricky. When a/c has no remainder and = b we have a root.
	When d = b but the remainder of a/c != 0, there is definitely no root.
*/
	b = NumberMalloc("TakeLongRoot"); c = NumberMalloc("TakeLongRoot");
	d = NumberMalloc("TakeLongRoot"); e = NumberMalloc("TakeLongRoot"); 
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
			retval = 1; break;
/*
			else {
				NumberFree(b,"TakeLongRoot"); NumberFree(c,"TakeLongRoot");
				NumberFree(d,"TakeLongRoot"); NumberFree(e,"TakeLongRoot");
				return(1);
			}
*/
		}
		DivLong(c,nc,(UWORD *)(&power),1,d,&nd,e,&ne);
		if ( nd == 0 ) {
			retval = 1;
			break;
/*
			NumberFree(b,"TakeLongRoot"); NumberFree(c,"TakeLongRoot");
			NumberFree(d,"TakeLongRoot"); NumberFree(e,"TakeLongRoot");
			return(1);
*/
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
			if ( nc != 0 ) {
				NumberFree(b,"TakeLongRoot"); NumberFree(c,"TakeLongRoot");
				NumberFree(d,"TakeLongRoot"); NumberFree(e,"TakeLongRoot");
				return(1);
			}
			break;
*/
		}
		if ( AddLong(b,nb,d,nd,b,&nb) ) goto TLcall;
	}
	for ( i = 0; i < nb; i++ ) a[i] = b[i];
	if ( *n < 0 ) *n = -nb;
	else          *n =  nb;
	NumberFree(b,"TakeLongRoot"); NumberFree(c,"TakeLongRoot");
	NumberFree(d,"TakeLongRoot"); NumberFree(e,"TakeLongRoot");
	return(retval);
TLcall:
	MLOCK(ErrorMessageLock);
	MesCall("TakeLongRoot");
	MUNLOCK(ErrorMessageLock);
	NumberFree(b,"TakeLongRoot"); NumberFree(c,"TakeLongRoot");
	NumberFree(d,"TakeLongRoot"); NumberFree(e,"TakeLongRoot");
	Terminate(-1);
	return(-1);
}

/*
 		#] TakeLongRoot: 
 		#[ MakeRational:

		Makes the integer a mod m into a traction b/c with |b|,|c| < sqrt(m)
		For the algorithm, see MakeLongRational.
*/

int MakeRational(WORD a,WORD m, WORD *b, WORD *c)
{
	LONG x1,x2,x3,x4,y1,y2;
	if ( a < 0 ) { a = a+m; }
	if ( a <= 1 ) {
		if ( a > m/2 ) a = a-m;
		*b = a; *c = 1; return(0);
	}
	x1 = m; x2 = a;
	if ( x2*x2 >= m ) {
		y1 = x1/x2; y2 = x1%x2; x3 = 1; x4 = -y1; x1 = x2; x2 = y2;
		while ( x2*x2 >= m ) {
			y1 = x1/x2; y2 = x1%x2; x1 = x2; x2 = y2; y2 = x3-y1*x4; x3 = x4; x4 = y2;
		}
	}
	else x4 = 1;
	if ( x2 == 0 ) { return(1); }
	if ( x2 > m/2 ) *b = x2-m;
	else            *b = x2;
	if ( x4 > m/2 ) { *c = x4-m; *c = -*c; *b = -*b; }
	else if ( x4 <= -m/2 ) { x4 += m; *c = x4; }
	else if ( x4 < 0 ) { x4 = -x4; *c = x4; *b = -*b; }
	else            *c = x4;
	return(0);
}

/*
 		#] MakeRational: 
 		#[ MakeLongRational:

		Converts the long number a mod m into the fraction b
		One of the properties of b is that num,den < sqrt(m)
		The algorithm: Start with:   m 0
		                             a 1
		Make now c=m%a, c1=m/a       c c2=0-c1*1
		Make now d=a%c  d1=a/c       d d2=1-d1*c2
		Make now e=c%d  e1=c/d       e e2=1-e1*d2
			etc till in the first column we get a number < sqrt(m)
			We have then f,f2 and the fraction is f/f2.
		If at any moment we get a zero, m contained an unlucky prime.

		Note that this can be made a lot faster when we make the same
		improvements as in the GCD routine. That is something for later.
#ifdef WITHMAKERATIONAL
*/

#define COPYLONG(x1,nx1,x2,nx2) { int i; for(i=0;i<ABS(nx2);i++)x1[i]=x2[i];nx1=nx2; }

int MakeLongRational(PHEAD UWORD *a, WORD na, UWORD *m, WORD nm, UWORD *b, WORD *nb)
{
	UWORD *root = NumberMalloc("MakeRational");
	UWORD *x1 = NumberMalloc("MakeRational");
	UWORD *x2 = NumberMalloc("MakeRational");
	UWORD *x3 = NumberMalloc("MakeRational");
	UWORD *x4 = NumberMalloc("MakeRational");
	UWORD *y1 = NumberMalloc("MakeRational");
	UWORD *y2 = NumberMalloc("MakeRational");
	WORD nroot,nx1,nx2,nx3,nx4,ny1,ny2,retval = 0;
	WORD sign = 1;
/*
	Step 1: Take the square root of m
*/
	COPYLONG(root,nroot,m,nm)
	TakeLongRoot(root,&nroot,2);
/*
	Step 2: Set the start values
*/
	if ( na < 0 ) { na = -na; sign = -sign; }
	COPYLONG(x1,nx1,m,nm)
	COPYLONG(x2,nx2,a,na)
/*
	x3[0] = 0, nx3 = 0;
	x4[0] = 1, nx4 = 1;
*/
/*
	The start operation needs some special attention because of the zero.
*/
	if ( BigLong(x2,nx2,root,nroot) <= 0 ) {
		x4[0] = 1, nx4 = 1;
		goto gottheanswer;
	}
	DivLong(x1,nx1,x2,nx2,y1,&ny1,y2,&ny2);
	if ( ny2 == 0 ) { retval = 1; goto cleanup; }
	COPYLONG(x1,nx1,x2,nx2)
	COPYLONG(x2,nx2,y2,ny2)
	x3[0] = 1; nx3 = 1;
	COPYLONG(x4,nx4,y1,ny1)
	nx4 = -nx4;
/*
	Now the loop.
*/
	while ( BigLong(x2,nx2,root,nroot) > 0 ) {
		DivLong(x1,nx1,x2,nx2,y1,&ny1,y2,&ny2);
		if ( ny2 == 0 ) { retval = 1; goto cleanup; }
		COPYLONG(x1,nx1,x2,nx2)
		COPYLONG(x2,nx2,y2,ny2)
		MulLong(y1,ny1,x4,nx4,y2,&ny2);
		ny2 = -ny2;
		AddLong(x3,nx3,y2,ny2,y1,&ny1);
		COPYLONG(x3,nx3,x4,nx4)
		COPYLONG(x4,nx4,y1,ny1)
	}
/*
	Now we have the answer. It is x2/x4. It has to be packed into b.
*/
gottheanswer:
	if ( nx4 < 0 ) { sign = -sign; nx4 = -nx4; }
	COPYLONG(b,*nb,x2,nx2)
	Pack(b,nb,x4,nx4);
	if ( sign < 0 ) *nb = -*nb;
cleanup:
	NumberFree(y2,"MakeRational");
	NumberFree(y1,"MakeRational");
	NumberFree(x4,"MakeRational");
	NumberFree(x3,"MakeRational");
	NumberFree(x2,"MakeRational");
	NumberFree(x1,"MakeRational");
	NumberFree(root,"MakeRational");
	return(retval);
}

/*
#endif
 		#] MakeLongRational: 
 		#[ ChineseRemainder:
*/
/**
 *		Routine takes a1 mod m1 and a2 mod m2 and returns a mod m1*m2 with
 *		a mod m1 = a1 and a mod m2 = a2
 *	Chinese remainder:
 *		a%(m1*m2) = q1*m1+a1
 *		a%(m1*m2) = q2*m2+a2
 *	Compute n1 such that (n1*m1)%m2 is one
 *	Compute n2 such that (n2*m2)%m1 is one
 *	Then (a1*n2*m2+a2*n1*m1)%(m1*m2) is a%(m1*m2)
 *
 */
#ifdef WITHCHINESEREMAINDER

int ChineseRemainder(PHEAD MODNUM *a1, MODNUM *a2, MODNUM *a)
{
	UWORD *inv1 = NumberMalloc("ChineseRemainder");
	UWORD *inv2 = NumberMalloc("ChineseRemainder");
	UWORD *fac1 = NumberMalloc("ChineseRemainder");
	UWORD *fac2 = NumberMalloc("ChineseRemainder");
	UWORD two[1];
	WORD ninv1, ninv2, nfac1, nfac2;
	if ( a1->na < 0 ) {
		AddLong(a1->a,a1->na,a1->m,a1->nm,a1->a,&(a1->na));
	}
	if ( a2->na < 0 ) {
		AddLong(a2->a,a2->na,a2->m,a2->nm,a2->a,&(a2->na));
	}
	MulLong(a1->m,a1->nm,a2->m,a2->nm,a->m,&(a->nm));

	GetLongModInverses(BHEAD a1->m,a1->nm,a2->m,a2->nm,inv1,&ninv1,inv2,&ninv2);
	MulLong(inv1,ninv1,a1->m,a1->nm,fac1,&nfac1);
	MulLong(inv2,ninv2,a2->m,a2->nm,fac2,&nfac2);

	MulLong(fac1,nfac1,a2->a,a2->na,inv1,&ninv1);
	MulLong(fac2,nfac2,a1->a,a1->na,inv2,&ninv2);
	AddLong(inv1,ninv1,inv2,ninv2,a->a,&(a->na));

	two[0] = 2;
	MulLong(a->a,a->na,two,1,fac1,&nfac1);
	if ( BigLong(fac1,nfac1,a->m,a->nm) > 0 ) {
		a->nm = -a->nm;
		AddLong(a->a,a->na,a->m,a->nm,a->a,&(a->na));
		a->nm = -a->nm;
	}
	NumberFree(fac2,"ChineseRemainder");
	NumberFree(fac1,"ChineseRemainder");
	NumberFree(inv2,"ChineseRemainder");
	NumberFree(inv1,"ChineseRemainder");
	return(0);
}

#endif

/*
 		#] ChineseRemainder: 
  	#] RekenLong : 
  	#[ RekenTerms :
 		#[ CompCoef :		WORD CompCoef(term1,term2)

	Compares the coefficients of term1 and term2 by subtracting them.
	This does more work than needed but this routine is only called
	when sorting functions and function arguments.
	(and comparing values 
*/
/* #define 64SAVE */

WORD CompCoef(WORD *term1, WORD *term2)
{
	GETIDENTITY
	UWORD *c;
	WORD n1,n2,n3,*a;
	GETCOEF(term1,n1);
	GETCOEF(term2,n2);
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
	c = NumberMalloc("CompCoef");
	if ( AddRat(BHEAD (UWORD *)term1,n1,(UWORD *)term2,-n2,c,&n3) ) {
		MLOCK(ErrorMessageLock);
		MesCall("CompCoef");
		MUNLOCK(ErrorMessageLock);
		NumberFree(c,"CompCoef");
		SETERROR(-1)
	}
	NumberFree(c,"CompCoef");
	return(n3);
}

/*
 		#] CompCoef : 
 		#[ Modulus :		WORD Modulus(term)

	Routine takes the coefficient of term modulus b. The answer
	is in term again and the length of term is adjusted.

*/

WORD Modulus(WORD *term)
{
	WORD *t;
	WORD n1;
	t = term;
	GETCOEF(t,n1);
	if ( TakeModulus((UWORD *)t,&n1,AC.cmod,AC.ncmod,UNPACK) ) {
		MLOCK(ErrorMessageLock);
		MesCall("Modulus");
		MUNLOCK(ErrorMessageLock);
		SETERROR(-1)
	}
	if ( !n1 ) {
		*term = 0;
		return(0);
	}
	else if ( n1 > 0 ) {
		n1 *= 2;
		t += n1;		/* Note that n1 >= 0 */
		n1++;
	}
	else if ( n1 < 0 ) {
		n1 *= 2;
		t += -n1;
		n1--;
	}
	*t++ = n1;
	*term = WORDDIF(t,term);
	return(0);
}

/*
 		#] Modulus : 
 		#[ TakeModulus :	WORD TakeModulus(a,na,cmodvec,ncmod,par)

		Routine gets the rational number in a with reduced length na.
		It is called when AC.ncmod != 0 and the number in AC.cmod is the
		number wrt which we need the modulus.
		The result is returned in a and na again.

		If par == NOUNPACK we only do a single number, not a fraction.
		In addition we don't do fancy. We want a positive number and
		the input was supposed to be positive.
		We don't pack the result. The calling routine is responsible for that.
		This may not be a good idea. To be checked.
*/

WORD TakeModulus(UWORD *a, WORD *na, UWORD *cmodvec, WORD ncmod, WORD par)
{
	GETIDENTITY
	UWORD *c, *d, *e, *f, *g, *h;
	UWORD *x4,*x2;
	UWORD *x3,*x1,*x5,*x6,*x7,*x8;
	WORD y3,y1,y5,y6;
	WORD n1, i, y2, y4;
	WORD nh, tdenom, tnumer, nmod;
	LONG x;
	if ( ncmod == 0 ) return(0);		/* No modulus operation */
	nmod = ABS(ncmod);
	n1 = *na;
	if ( ( par & UNPACK ) != 0 ) UnPack(a,n1,&tdenom,&tnumer);
	else { tnumer = n1; }
/*
	We fish out the special case that the coefficient is short as well. 
	There is no need to make lots of calls etc
*/
	if ( ( ( par & UNPACK ) == 0 ) && nmod == 1 && ( n1 == 1 || n1 == -1 ) ) {
		goto simplecase;
	}
	else if ( nmod == 1 && ( n1 == 1 || n1 == -1 ) ) {
		if ( a[1] != 1 ) {
			a[1] = a[1] % cmodvec[0];
			if ( a[1] == 0 ) {
				MesPrint("Division by zero in short modulus arithmetic");
				return(-1);
			}
			y1 = 0;
			if ( ( AC.modinverses != 0 ) && ( ( par & NOINVERSES ) == 0 ) ) {
				y1 = AC.modinverses[a[1]];
			}
			else {
				GetModInverses(a[1],cmodvec[0],&y1,&y2);
			}
			x = a[0];
			a[0] = (x*y1) % cmodvec[0];
			a[1] = 1;
		}
		else {
simplecase:
			a[0] = a[0] % cmodvec[0];
		}
		if ( a[0] == 0 ) { *na = 0; return(0); }
		if ( ( AC.modmode & POSNEG ) != 0 ) {
			if ( a[0] > (UWORD)(cmodvec[0]/2) ) {
				a[0] =  cmodvec[0] - a[0];
				*na = -*na;
			}
		}
		else if ( *na < 0 ) {
			*na = 1; a[0] = cmodvec[0] - a[0];
		}
		return(0);
	}
	c = NumberMalloc("TakeModulus"); d = NumberMalloc("TakeModulus"); e = NumberMalloc("TakeModulus");
	f = NumberMalloc("TakeModulus"); g = NumberMalloc("TakeModulus"); h = NumberMalloc("TakeModulus");
	n1 = ABS(n1);
	if ( DivLong(a,tnumer,(UWORD *)cmodvec,nmod,
		c,&nh,a,&tnumer) ) goto ModErr;
	if ( tnumer == 0 ) { *na = 0; goto normalreturn; }
	if ( ( par & UNPACK ) == 0 ) {
		if ( ( AC.modmode & POSNEG ) != 0 ) {
			NormalModulus(a,&tnumer);
		}
		else if ( tnumer < 0 ) {
			SubPLon((UWORD *)cmodvec,nmod,a,-tnumer,a,&tnumer);
		}
		*na = tnumer;
		goto normalreturn;
	}
	if ( tdenom == 1 && a[n1] == 1 ) {
		if ( ( AC.modmode & POSNEG ) != 0 ) {
			NormalModulus(a,&tnumer);
		}
		else if ( tnumer < 0 ) {
			SubPLon((UWORD *)cmodvec,nmod,a,-tnumer,a,&tnumer);
		}
		*na = tnumer;
		i = ABS(tnumer);
		a += i;
		*a++ = 1;
		while ( --i > 0 ) *a++ = 0;
		goto normalreturn;
	}
	if ( DivLong(a+n1,tdenom,(UWORD *)cmodvec,nmod,c,&nh,a+n1,&tdenom) ) goto ModErr;
	if ( !tdenom ) {
		MLOCK(ErrorMessageLock);
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
		MUNLOCK(ErrorMessageLock);
		NumberFree(c,"TakeModulus"); NumberFree(d,"TakeModulus"); NumberFree(e,"TakeModulus");
		NumberFree(f,"TakeModulus"); NumberFree(g,"TakeModulus"); NumberFree(h,"TakeModulus");
		return(-1);
	}
	if ( ( AC.modinverses != 0 ) && ( ( par & NOINVERSES ) == 0 )
	&& ( tdenom == 1 || tdenom == -1 ) ) {
		*d = AC.modinverses[a[n1]]; y1 = 1; y2 = tdenom;
		if ( MulLong(a,tnumer,d,y1,c,&y3) ) goto ModErr;
		if ( DivLong(c,y3,(UWORD *)cmodvec,nmod,d,&y5,a,&tdenom) ) goto ModErr;
		if ( y2 < 0 ) tdenom = -tdenom;
	}
	else {
	  x2 = (UWORD *)cmodvec; x1 = c; i = nmod; while ( --i >= 0 ) *x1++ = *x2++;
	  x1 = c; x2 = a+n1; x3 = d; x4 = e; x5 = f; x6 = g;
	  y1 = nmod; y2 = tdenom; y4 = 0; y5 = 1; *x5 = 1;
	  for(;;) {
		if ( DivLong(x1,y1,x2,y2,h,&nh,x3,&y3) ) goto ModErr;
		if ( MulLong(x5,y5,h,nh,x6,&y6) ) goto ModErr;
		if ( AddLong(x4,y4,x6,-y6,x6,&y6) ) goto ModErr;
		if ( !y3 ) {
			if ( y2 != 1 || *x2 != 1 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("Inverse in modulus arithmetic doesn't exist");
				MesPrint("Denominator and modulus are not relative prime");
				MUNLOCK(ErrorMessageLock);
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
	}
	if ( !tdenom ) { *na = 0; goto normalreturn; }
	if ( ( ( AC.modmode & POSNEG ) != 0 ) && ( ( par & FROMFUNCTION ) == 0 ) ) {
		NormalModulus(a,&tdenom);
	}
	else if ( tdenom < 0 ) {
		SubPLon((UWORD *)cmodvec,nmod,a,-tdenom,a,&tdenom);
	}
	*na = tdenom;
	i = ABS(tdenom);
	a += i;
	*a++ = 1;
	while ( --i > 0 ) *a++ = 0;
normalreturn:
	NumberFree(c,"TakeModulus"); NumberFree(d,"TakeModulus"); NumberFree(e,"TakeModulus");
	NumberFree(f,"TakeModulus"); NumberFree(g,"TakeModulus"); NumberFree(h,"TakeModulus");
	return(0);
ModErr:
	MLOCK(ErrorMessageLock);
	MesCall("TakeModulus");
	MUNLOCK(ErrorMessageLock);
	NumberFree(c,"TakeModulus"); NumberFree(d,"TakeModulus"); NumberFree(e,"TakeModulus");
	NumberFree(f,"TakeModulus"); NumberFree(g,"TakeModulus"); NumberFree(h,"TakeModulus");
	SETERROR(-1)
}

/*
 		#] TakeModulus : 
 		#[ TakeNormalModulus :  WORD TakeNormalModulus(a,na,par)

		added by Jan [01-09-2010]
*/

WORD TakeNormalModulus (UWORD *a, WORD *na, UWORD *c, WORD nc, WORD par)
{
	WORD n;
	WORD nhalfc;
	UWORD *halfc;

	GETIDENTITY;
	
	/* determine c/2 by right shifting */
	halfc = NumberMalloc("TakeNormalModulus");
	nhalfc=nc;
	WCOPY(halfc,c,nc);

	for (n=0; n<nhalfc; n++) {
		halfc[n] /= 2;
		if (n+1<nc) halfc[n] |= c[n+1] << (BITSINWORD-1);
	}

	if (halfc[nhalfc-1]==0)
		nhalfc--;
				 
	/* takes care of the number never expanding, e.g., -1(mod 100) -> 99 -> -1 */
	if (BigLong(a,ABS(*na),halfc,nhalfc) > 0) {
	
		TakeModulus(a,na,c,nc,par);
	
		n = ABS(*na);
		if (BigLong(a,n,halfc,nhalfc) > 0) {
			SubPLon(c,nc,a,n,a,&n);
			*na = (*na > 0 ? -n : n);
		}
	}

	NumberFree(halfc,"TakeNormalModulus");
	return(0);
}

/*
 		#] TakeNormalModulus : 
 		#[ MakeModTable :	WORD MakeModTable()
*/

WORD MakeModTable()
{
	LONG size, i, j, n;
	n = ABS(AC.ncmod);
	if ( AC.modpowers ) {
		M_free(AC.modpowers,"AC.modpowers");
		AC.modpowers = NULL;
	}
	if ( n > 2 ) {
		MLOCK(ErrorMessageLock);
		MesPrint("&No memory for modulus generator power table");
		MUNLOCK(ErrorMessageLock);
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
				MLOCK(ErrorMessageLock);
				MesPrint("&improper generator for this modulus");
				MUNLOCK(ErrorMessageLock);
				M_free(AC.modpowers,"AC.modpowers");
				return(-1);
			}
		}
		AC.modpowers[1] = 0;
	}
	else {
		GETIDENTITY
		WORD nScrat, n2;
		UWORD *MMscrat7 = NumberMalloc("MakeModTable"), *MMscratC = NumberMalloc("MakeModTable");
		*MMscratC = 1;
		nScrat = 1;
		j = size * 2;
		for ( i = 0; i < j; i+=2 ) { AC.modpowers[i] = 0; AC.modpowers[i+1] = 0; }
		for ( i = 0; i < size; i++ ) {
			j = *MMscratC + (((LONG)MMscratC[1])<<BITSINWORD);
			j *= 2;
			AC.modpowers[j] = (WORD)(i & WORDMASK);
			AC.modpowers[j+1] = (WORD)(i >> BITSINWORD);
			MulLong((UWORD *)MMscratC,nScrat,(UWORD *)AC.powmod,
			AC.npowmod,(UWORD *)MMscrat7,&n2);
			TakeModulus(MMscrat7,&n2,AC.cmod,AC.ncmod,NOUNPACK);
			*MMscratC = *MMscrat7; MMscratC[1] = MMscrat7[1]; nScrat = n2;
		}
		NumberFree(MMscrat7,"MakeModTable"); NumberFree(MMscratC,"MakeModTable");
		j = size * 2;
		for ( i = 4; i < j; i+=2 ) {
			if ( AC.modpowers[i] == 0 && AC.modpowers[i+1] == 0 ) {
				MLOCK(ErrorMessageLock);
				MesPrint("&improper generator for this modulus");
				MUNLOCK(ErrorMessageLock);
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

int Factorial(PHEAD WORD n, UWORD *a, WORD *na)
{
	GETBIDENTITY
	UWORD *b, *c;
	WORD nc;
	int i, j;
	LONG ii;
	if ( n > AT.nfac ) {
		if ( AT.factorials == 0 ) {
			AT.nfac = 0; AT.mfac = 50; AT.sfact = 400;
			AT.pfac = (LONG *)Malloc1((AT.mfac+2)*sizeof(LONG),"factorials");
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
				MLOCK(ErrorMessageLock);
				MesPrint("Overflow in factorial. MaxTal = %d",AM.MaxTal);
				MesPrint("Increase MaxTerm in %s",setupfilename);
				MUNLOCK(ErrorMessageLock);
				return(-1);
			}
			if ( j > AT.mfac ) {  /* double the pfac buffer */
				LONG *p;
				p = (LONG *)Malloc1((AT.mfac*2+2)*sizeof(LONG),"factorials");
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

int Bernoulli(WORD n, UWORD *a, WORD *na)
{
	GETIDENTITY
	UWORD *b, *c, *scrib, *ntop, *ntop1;
	WORD i, i1, i2, nhalf, nqua, nscrib, nntop, nntop1, *oldworkpointer;
	UWORD twee = 2, twonplus1;
	int j;
	LONG ii;
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
			AT.pBer = (LONG *)Malloc1((AT.mBer+2)*sizeof(LONG),"bernoullis");
			AT.bernoullis = (UWORD *)Malloc1(AT.sBer*sizeof(UWORD),"bernoullis");
			AT.pBer[1] = 0; AT.pBer[2] = 3;
			AT.bernoullis[0] = 3; AT.bernoullis[1] = 1; AT.bernoullis[2] = 12;
			if ( nhalf == 1 ) {
				a[0] = 1; a[1] = 12; *na = 3; return(0);
			}
		}
		while ( nhalf > AT.mBer ) {
			LONG *p;
			p = (LONG *)Malloc1((AT.mBer*2+1)*sizeof(LONG),"bernoullis");
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
 		#[ NextPrime :
*/
/**
 *	Gives the next prime number in the list of prime numbers.
 *
 *	If the list isn't long enough we expand it.
 *	For ease in ParForm and because these lists shouldn't be very big
 *	we let each worker keep its own list.
 *
 *	The list is cut off at MAXPOWER, because we don't want to get into
 *	trouble that the power of a variable gets larger than the prime number.
 */

#if ( BITSINWORD == 32 )

void StartPrimeList(PHEAD0)
{
	int i, j;
	AR.PrimeList[AR.numinprimelist++] = 3;
	for ( i = 5; i < 46340; i += 2 ) {
		for ( j = 0; j < AR.numinprimelist && AR.PrimeList[j]*AR.PrimeList[j] <= i; j++ ) {
			if ( i % AR.PrimeList[j] == 0 ) goto nexti;
		}
		AR.PrimeList[AR.numinprimelist++] = i;
nexti:;
	}
	AR.notfirstprime = 1;
}

#endif

WORD NextPrime(PHEAD WORD num)
{
	int i, j;
	WORD *newpl;
	LONG newsize, x;
#if ( BITSINWORD == 32 )
	if ( AR.notfirstprime == 0 ) StartPrimeList(BHEAD0);
#endif
	if ( num > AT.inprimelist ) {
		while ( AT.inprimelist < num ) {
			if ( num >= AT.sizeprimelist ) {
				if ( AT.sizeprimelist == 0 ) newsize = 32;
				else newsize = 2*AT.sizeprimelist;
				while ( num >= newsize ) newsize = newsize*2;
				newpl = (WORD *)Malloc1(newsize*sizeof(WORD),"NextPrime");
				for ( i = 0; i < AT.sizeprimelist; i++ ) {
					newpl[i] = AT.primelist[i];
				}
				if ( AT.sizeprimelist > 0 ) {
					M_free(AT.primelist,"NextPrime");
				}
				AT.sizeprimelist = newsize;
				AT.primelist = newpl;
			}
			if ( AT.inprimelist < 0 ) { i = MAXPOSITIVE; }
			else { i = AT.primelist[AT.inprimelist]; }
			while ( i > MAXPOWER ) {
				i -= 2; x = i;
#if ( BITSINWORD == 32 )
				for ( j = 0; j < AR.numinprimelist && AR.PrimeList[j]*(LONG)(AR.PrimeList[j]) <= x; j++ ) {
					if ( x % AR.PrimeList[j] == 0 ) goto nexti;
				}
#else
				for ( j = 3; j*((LONG)j) <= x; j += 2 ) {
					if ( x % j == 0 ) goto nexti;
				}
#endif
				AT.inprimelist++;
				AT.primelist[AT.inprimelist] = i;
				break;
nexti:;
			}
			if ( i < MAXPOWER ) {
				MLOCK(ErrorMessageLock);
				MesPrint("There are not enough short prime numbers for this calculation");
				MesPrint("Try to use a computer with a %d-bits architecture",
					(int)(BITSINWORD*4));
				MUNLOCK(ErrorMessageLock);
				Terminate(-1);
			}
		}
	}
	return(AT.primelist[num]);
}
 
/*
 		#] NextPrime : 
 		#[ wranf :

		A random number generator that generates random WORDs with a very
		long sequence. It is based on the Knuth generator.

		We take some care that each thread can run its own, but each
		uses its own startup. Hence the seed includes the identity of
		the thread.

		For NPAIR1, NPAIR2 we can use any pair from the table on page 28.
		Candidates are 24,55 (the example on the pages 171,172)
		or (33,97) or (38,89)
		These values are defined in fsizes.h and used in startup.c and threads.c
*/

#define WARMUP 6

static void wranfnew(PHEAD0)
{
	int i;
	LONG j;
	for ( i = 0; i < AR.wranfnpair1; i++ ) {
		j = AR.wranfia[i] - AR.wranfia[i+(AR.wranfnpair2-AR.wranfnpair1)];
		if ( j < 0 ) j += (LONG)1 << (2*BITSINWORD-2);
		AR.wranfia[i] = j;
	}
	for ( i = AR.wranfnpair1; i < AR.wranfnpair2; i++ ) {
		j = AR.wranfia[i] - AR.wranfia[i-AR.wranfnpair1];
		if ( j < 0 ) j += (LONG)1 << (2*BITSINWORD-2);
		AR.wranfia[i] = j;
	}
}

void iniwranf(PHEAD0)
{
	int imax = AR.wranfnpair2-1;
	ULONG i, ii, seed = AR.wranfseed;
	LONG j, k;
	ULONG offset = 12345;
#ifdef PARALLELCODE
	int id;
#if defined(WITHPTHREADS)
	id = AT.identity;
#elif defined(WITHMPI)
	id = PF.me;
#endif
	seed += id;
	i = id + 1;
	if ( i > 1 ) {
		ULONG pow, accu;
		pow = offset; accu = 1;
		while ( i ) {
			if ( ( i & 1 ) != 0 ) accu *= pow;
			i /= 2; pow = pow*pow;
		}
		offset = accu;
	}
#endif
	if ( seed < ((LONG)1<<(BITSINWORD-1)) ) {
		j = ( (seed+31459L) << (BITSINWORD-2))+offset;
	}
	else if ( seed < ((LONG)1<<(BITSINWORD+10-1)) ) {
		j = ( (seed+31459L) << (BITSINWORD-10-2))+offset;
	}
	else {
		j = ( (seed+31459L) << 1)+offset;
	}
	if ( ( seed & 1 ) == 1 ) seed++;
	j += seed;
	AR.wranfia[imax] = j;
	k = 1;
	for ( i = 0; i <= (ULONG)(imax); i++ ) {
		ii = (AR.wranfnpair1*i)%AR.wranfnpair2;
		AR.wranfia[ii] = k;
		k = ULongToLong((ULONG)j - (ULONG)k);
		if ( k < 0 ) k += (LONG)1 << (2*BITSINWORD-2);
		j = AR.wranfia[ii];
	}
	for ( i = 0; i < WARMUP; i++ ) wranfnew(BHEAD0);
	AR.wranfcall = 0;
}

UWORD wranf(PHEAD0)
{
	UWORD wval;
	if ( AR.wranfia == 0 ) {
		AR.wranfia = (ULONG *)Malloc1(AR.wranfnpair2*sizeof(ULONG),"wranf");
		iniwranf(BHEAD0);
	}
	if ( AR.wranfcall >= AR.wranfnpair2) {
		wranfnew(BHEAD0);
		AR.wranfcall = 0;
	}
	wval = (UWORD)(AR.wranfia[AR.wranfcall++]>>(BITSINWORD-1));
	return(wval);
}

/*
	Returns a random UWORD in the range (0,...,imax-1)
*/

UWORD iranf(PHEAD UWORD imax)
{
	UWORD i;
	ULONG x = (LONG)1 << BITSINWORD, xmax = x - x%imax;
	while ( ( i = wranf(BHEAD0) ) >= xmax ) {}
	return(i%imax);
}

/*
 		#] wranf : 
 		#[ PreRandom :

		The random number generator of the preprocessor.
		This one is completely different from the execution time generator
		random_(number). In the preprocessor we generate a floating point
		number in a string according to a distribution.
		Currently allowed are:
			RANDOM_(log,min,max)
			RANDOM_(lin,min,max)
		The return value is a string with the floating point number.
*/

UBYTE *PreRandom(UBYTE *s)
{
	GETIDENTITY
	UBYTE *mode,*mins = 0,*maxs = 0, *outval;
	float num;
	double minval, maxval, value = 0;
	int linlog = -1;
	mode = s;
	while ( FG.cTable[*s] <= 1 ) s++;
	if ( *s == ',' ) { *s = 0; s++; }
	mins = s;
	while ( *s && *s != ',' ) s++;
	if ( *s == ',' ) { *s = 0; s++; }
	maxs = s;
	while ( *s && *s != ',' ) s++;
	if ( *s || *maxs == 0 || *mins == 0 ) {
		MesPrint("@Illegal arguments in macro RANDOM_");
		Terminate(-1);
	}
	if ( StrICmp(mode,(UBYTE *)"lin") == 0 ) {
		linlog = 0;
	}
	else if ( StrICmp(mode,(UBYTE *)"log") == 0 ) {
		linlog = 1;
	}
	else {
		MesPrint("@Illegal mode argument in macro RANDOM_");
		Terminate(-1);
	}

	sscanf((char *)mins,"%f",&num); minval = num;
	sscanf((char *)maxs,"%f",&num); maxval = num;

	/*
	 * Note on ParFORM: we should use the same random number on all the
	 * processes in the complication phase. The random number is generated
	 * on the master and broadcast to the other processes.
	 */
	{
		UWORD x;
		double xx;
#ifdef WITHMPI
		x = 0;
		if ( PF.me == MASTER ) {
			x = wranf(BHEAD0);
		}
		x = (UWORD)PF_BroadcastNumber((LONG)x);
#else
		x = wranf(BHEAD0);
#endif
		xx = x/pow(2.0,(double)(BITSINWORD-1));
		if ( linlog == 0 ) {
			value = minval + (maxval-minval)*xx;
		}
		else if ( linlog == 1 ) {
			value = minval * pow(maxval/minval,xx);
		}
	}

	outval = (UBYTE *)Malloc1(64,"PreRandom");
	if ( ABS(value) < 0.00001 || ABS(value) > 1000000. ) {
		sprintf((char *)outval,"%e",value);
	}
	else if ( ABS(value) < 0.0001 ) { sprintf((char *)outval,"%10f",value); }
	else if ( ABS(value) < 0.001 ) { sprintf((char *)outval,"%9f",value); }
	else if ( ABS(value) < 0.01 ) { sprintf((char *)outval,"%8f",value); }
	else if ( ABS(value) < 0.1 ) { sprintf((char *)outval,"%7f",value); }
	else if ( ABS(value) < 1. ) { sprintf((char *)outval,"%6f",value); }
	else if ( ABS(value) < 10. ) { sprintf((char *)outval,"%5f",value); }
	else if ( ABS(value) < 100. ) { sprintf((char *)outval,"%4f",value); }
	else if ( ABS(value) < 1000. ) { sprintf((char *)outval,"%3f",value); }
	else if ( ABS(value) < 10000. ) { sprintf((char *)outval,"%2f",value); }
	else { sprintf((char *)outval,"%1f",value); }
	return(outval);
}

/*
 		#] PreRandom : 
  	#] Functions : 
*/
