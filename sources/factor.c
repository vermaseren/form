/*
  	#[ Includes : factor.c
*/

#include "form3.h"

/*
  	#] Includes :
  	#[ ModulusGCD1 :

	For experimentation
	The command
		ModulusGCD,m,x,f1,f2;
	This gives in f2 the GCD of all occurrences of f1 mod m with the variable x.
	It leaves the functions f1.
	In ModulusGCD1 we assume that m fits inside a WORD.
*/

static WORD *mgscr1 = 0, *mgscr2 = 0, *mgscr3 = 0, nmgscr = 0;

int ModulusGCD1 ARG5(WORD,modu,WORD,fun1,WORD,fun2,WORD *,term,WORD,sym)
{
	WORD *t, *tstop, *t1 = 0, n1 = 0, n2, *m1, *m2, i, x1, offset;
	LONG y, y1, y2;
	tstop = term + *term; tstop -= ABS(tstop[-1]);
	t = term + 1;
	while ( t < tstop ) {
		if ( *t == fun1 ) {
			if ( t1 == 0 ) {
				n1 = MakeMono(modu,t,0,sym);
				if ( n1 >= 0 ) {
					t1 = t;
				}
			}
			else {
				n2 = MakeMono(modu,t,n1+1,sym);
				if ( n2 >= 0 ) {
					t1 = t;
				}
/*
				We have loaded the arrays. Now the works.
*/
				if ( n1 < n2 ) {
					m1 = mgscr1; mgscr1 = mgscr2; mgscr2 = m1; x1 = n1; n1 = n2; n2 = x1;
				}
				for(;;) {
					y1 = mgscr1[n1]; y2 = mgscr2[n2];
					mgscr1[n1] = 0; offset = n1-n2;
					for ( i = n2 - 1; i >= 0; i-- ) {
						y = ( mgscr1[i+offset] * y2 - mgscr2[i] * y1 ) % modu;
						if ( y < 0 ) y += modu;
						mgscr1[i+offset] = y;
					}
					for ( i = offset-1; i >= 0; i-- ) {
						y = (mgscr1[i]*y2) % modu;
						if ( y < 0 ) y += modu;
						mgscr1[i] = y;
					}
					while ( n1 > 0 && mgscr1[n1] == 0 ) n1--;
					if ( n1 == 0 && mgscr1[n1] == 0 ) break;
					if ( n1 < n2 ) {
						m1 = mgscr1; mgscr1 = mgscr2; mgscr2 = m1; x1 = n1; n1 = n2; n2 = x1;
					}
				}
				m1 = mgscr1; mgscr1 = mgscr2; mgscr2 = m1; x1 = n1; n1 = n2; n2 = x1;
			}
		}
		t += t[1];
	}
/*
		The answer resides now in mgscr1. Paste it into term after the last t1.
*/
	if ( t1 ) {
		if ( n1 == 0 ) {
			offset = FUNHEAD+2;
		}
		else if ( n1 ==  1 && mgscr1[0] == 0 ) {
			offset = FUNHEAD+2;
		}
		else {
			offset = FUNHEAD + ARGHEAD;
			if ( mgscr1[0] != 0 ) offset += 4;
			for ( i = 1; i <= n1; i++ ) {
				if ( mgscr1[i] != 0 ) offset += 8;
			}
		}
		t = t1 + t1[1];
		m1 = term + *term;
		m2 = m1 + offset;
		*term +=  offset;
		while ( m1 > t ) *--m2 = *--m1;
		*t++ = fun2; *t++ = offset; *t++ = 1; FILLFUN3(t);
		if ( n1 == 0 ) {
			*t++ = -SNUMBER; *t++ = mgscr1[0];
		}
		else if ( n1 == 1 && mgscr1[0] == 0 ) {
			*t++ = -SYMBOL;
			*t++ = sym;
		}
		else {
			*t++ = offset - FUNHEAD; *t++ = 1; FILLARG(t);
			if ( mgscr1[0] != 0 ) {
				*t++ = 4; *t++ = mgscr1[0]; *t++ = 1; *t++ = 3;
			}
			for ( i = 1; i <= n1; i++ ) {
				if ( mgscr1[i] != 0 ) {
					*t++ = 8; *t++ = SYMBOL; *t++ = 4; *t++ = sym; *t++ = i;
					*t++ = mgscr1[i]; *t++ = 1; *t++ = 3;
				}
			}
		}
	}
	return(0);
}

/*
  	#] ModulusGCD1 :
  	#[ MakeMono :
*/

int MakeMono ARG4(WORD,modu,WORD,*t,WORD,whichbuffer,WORD,sym)
{
	GETIDENTITY;
	WORD *tstop = t + t[1], *tt, *ttt, cs, maxpow, i, n, *m, *w1, *w2, rl;
	WORD oldncmod, oldcmod;
	if ( nmgscr == 0 ) {
		nmgscr = 40;
		mgscr3 = mgscr1 = (WORD *)Malloc1(2*(nmgscr+1)*sizeof(WORD),"MakeMono");
		mgscr2 = mgscr1 + nmgscr + 1;
	}
	if ( whichbuffer == 0 ) m = mgscr1;
	else                    m = mgscr2;
/*
	First the special cases
*/
	t += FUNHEAD;
	if ( *t == -SNUMBER && t+2 == tstop ) {
		if ( t[1] < 0 ) {
			mgscr2[0] = -((-t[1]) % modu);
			if ( mgscr2[0] < 0 ) mgscr2[0] += modu;
		}
		else {
			mgscr2[0] = t[1] % modu;
		}
		return(0);
	}
	else if ( *t == -SYMBOL && t+2 == tstop && t[1] == sym ) {
		mgscr2[0] = 0; mgscr2[1] = 1; return(1);
	}
	else if ( t + *t != tstop ) return(-1);
/*
	Here we have to scan the function and find the highest power
*/
	maxpow = -1;
	tt = t + ARGHEAD;
	while ( tt < tstop ) {
		ttt = tt + *tt;
		cs = ABS(ttt[-1]);
		if ( *tt == cs+1 ) {
			if ( maxpow < 0 ) maxpow = 0;
		}
		else if ( ( ( tt + tt[2] ) == ( ttt - cs - 1 ) ) && ( tt[1] == SYMBOL )
		&& ( tt[2] == 4 ) && ( tt[3] == sym ) && ( tt[4] > 0 ) ) {
			if ( tt[4] > maxpow ) maxpow = tt[4];
		}
		else return(-1);
		tt = ttt;
	}
/*
	The function has passed the first test
	Now we prepare the output
*/
	oldncmod = AC.ncmod; oldcmod = AC.cmod[0];
	AC.ncmod = 1; AC.cmod[0] = modu;
	if ( maxpow > nmgscr ) {	/* extend the buffer? */
		WORD *m1;
		nmgscr = maxpow;
		m1 = (WORD *)Malloc1(2*(nmgscr+1)*sizeof(WORD),"MakeMono");
		mgscr2 = m1 + nmgscr + 1;
		if ( whichbuffer > 0 ) {
			for ( i = 0; i < whichbuffer; i++ ) m1[i] = mgscr1[i];
		}
		M_free(mgscr3,"ModulusGCD1");
		mgscr1 = m1;
		if ( whichbuffer == 0 ) m = mgscr1;
		else                    m = mgscr2;
	}
	for ( i = 0; i <= maxpow; i++ ) m[i] = 0;
	tt = t + ARGHEAD;
	while ( tt < tstop ) {
		ttt = tt + *tt;
		cs = ABS(ttt[-1]);
		if ( *tt == cs+1 ) { n = 0; }
		else { n = tt[4]; }
		rl = (cs-1)/2;
		w1 = AT.WorkPointer; w2 = ttt - cs;
		for ( i = 0; i < rl; i++ ) { *w1++ = *w2++; *w1++ = *w2++; }
		if ( TakeModulus((UWORD *)(AT.WorkPointer),&rl,0) < 0 ) {
			AC.ncmod = oldncmod; AC.cmod[0] = oldcmod;
			return(-1);
		}
		m[n] = *(AT.WorkPointer);
		tt = ttt;
	}
	AC.ncmod = oldncmod; AC.cmod[0] = oldcmod;
	return(maxpow);
}

/*
  	#] MakeMono :
  	#[ ChinRem :

	We have two input arrays: pp with a list of (short) primes
	and rr with a list of remainders. Length of both arrays is npp.
	We return the positive constant x (length nx) with the property
	x < prod(pp[i])
	x%pp[i] = rr[i]
	Algorithm:
		P = prod(pp[i])
		x = sum_(i,0,npp-1,(P/pp[i])*(((P/pp[i])mod(pp[i]))^-1*rr[i])mod(pp[i])
	When par == 1 we put the number between -P/2 and +P/2
*/

#ifdef CHINREM

static UWORD *CRscrat1 = 0, *CRscrat2 = 0, *CRscrat3 = 0;
static WORD nCRscrat1, nCRscrat2, nCRscrat3;

int
ChinRem ARG6(UWORD *,pp, UWORD *,rr, WORD, npp, UWORD *,x, WORD *,nx,int,par)
{
	UWORD *x1, *x2, *x3, z1, z2, i;
	WORD y1, y2;
	if ( CRscrat1 == 0 ) {
		CRscrat1 = (UWORD *)Malloc1(3*(AM.MaxTal+2)*sizeof(UWORD),"ChinRem");
		CRscrat2 = CRscrat1 + AM.MaxTal+2;
		CRscrat3 = CRscrat2 + AM.MaxTal+2;
	}
	x1 = CRscrat1; x2 = CRscrat2;
	x1[0] = pp[0]; y1 = 1;
	for ( i = 1; i < npp; i++ ) {
		if ( MulLong(x1,y1,pp+i,1,x2,&y2) ) goto ChinErr;
		x3 = x1; x1 = x2; x2 = x3; y1 = y2;
	}

	for ( i = 0; i < npp; i++ ) {
		DivShort(x1,y1,pp[i],x2,&y2);
		z1 = DivMod(x2,y2,pp[i]);
		z1 = InvMod(z1,pp[i]);
		z2 = (((ULONG)z1)*rr[i])%pp[i];
		if ( i == 0 ) {
			*nx = 1; x[0] = z2;
		}
		else {
			if ( MulLong(x2,y2,&z2,1,CRscrat3,&nCRscrat3) ) goto ChinErr;
			if ( AddLong(x,*nx,CRscrat3,nCRscrat3,x,nx) ) goto ChinErr;
		}
	}
	while ( BigLong(x1,y1,x,*nx) <= 0 ) {
		SubPLon(x,*nx,x1,y1,x,nx);
	}
	if ( par == 1 ) {
		SubPLon(x1,y1,x,*nx,x2,&y2);
		if ( BigLong(x,*nx,x2,y2) > 0 ) ) {
			for ( i = 0; i < y2; i++ ) x[i] = x2[i];
			*nx = -y2;
		}
	}
	return(0);
ChinErr:
	LOCK(ErrorMessageLock);
	MesCall("ChinRem");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
  	#] ChinRem :
  	#[ ChinRema :

	Use of the Chinese Remainder theorem.
	We assume here that the smallest one (pb) of the (relative) primes
	fits inside a FORM word.
	x = pa*a+ra = pb*b+rb.
	with
	a = x/pa, ra = x%pa, b = x/pb, rb = x%pb.
	Output is x and its length.
	We use:
		a = (((rb-ra))/(pa%pb))mod(pb)
*/

int
ChinRema ARG8(UWORD*,pa,WORD,na,UWORD*,ra,WORD,nra,UWORD,pb,UWORD,rb,UWORD*,x,WORD*,nx)
{
	UWORD nn,pp,pd;
	ULONG xx;
	if ( CRscrat1 == 0 ) {
		CRscrat1 = (UWORD *)Malloc1(3*(AM.MaxTal+2)*sizeof(UWORD),"ChinRem");
		CRscrat2 = CRscrat1 + AM.MaxTal+2;
		CRscrat3 = CRscrat2 + AM.MaxTal+2;
	}
	if ( AddLong(&rb,1,ra,-nra,CRscrat1,&nCRscrat1) ) goto ChinErr;
	nn = DivMod(CRscrat1,nCRscrat1,pb);
	pp = DivMod(pa,na,pb);
	pd = InvMod(pp,pb);
	if ( pd == 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Problems with inverse in modulus calculation");
		UNLOCK(ErrorMessageLock);
		goto ChinErr;
	}
	xx = ((ULONG)nn)*pd;
	pd = xx%pb;
/*
	Now the object we are after is pd*pa+ra
*/
	if ( MulLong(pa,na,&pd,1,x,nx) || AddLong(x,*nx,ra,nra,x,nx) ) goto ChinErr;
	return(0);
ChinErr:
	LOCK(ErrorMessageLock);
	MesCall("ChinRema");
	UNLOCK(ErrorMessageLock);
	SETERROR(-1)
}

/*
  	#] ChinRema :
  	#[ DivMod :

	Takes the modulus a%b and returns it. We assume that b fits inside a word.
*/

UWORD DivMod ARG3(UWORD *,a,WORD,na,UWORD,b)
{
	int la;
	long x = 0;
	la = ABS(na);
	while ( la > 0 ) { x = ((x << BITSINWORD) + a[--la]) % b; }
	if ( na < 0 && x != 0 ) x = b - x;
	return ( (UWORD)x );
}

/*
  	#] DivMod :
  	#[ DivShort :

	Divides the long integer a by the short word b. Result in c.
	The remainder is returned.
*/

WORD DivShort ARG5(UWORD *,a,WORD,na,UWORD,b,UWORD *,c,WORD *,nc)
{
	int la, lb;
	long x = 0, y;
	lb = la = ABS(na);
	while ( --la > 0 ) {
		y = (x << BITSINWORD) + a[la]);
		x = y % b;
		c[la] = y/b;
	}
	*nc = lb--;
	if ( c[lb] == 0 ) (*nc)--;
	if ( na < 0 ) {
		if ( x != 0 ) x = b - x;
		*nc = -*nc;
	}
	return ( (UWORD)x );
}

/*
  	#] DivShort :
  	#[ InvMod :

	Takes the inverse of A mod B. Assumes of course that a has an inverse,
	or in other words: it assumes that a and b are relative prime.
	If not, the return value is zero.
	There are two possible algorithms:
	a:  x = a^(p-2)
	b:	determine x*a+y*b = 1. Then x is the inverse.
	We assume that a < b.
	Of course, when speed is very important, and b is not too big, we
	could build a table for this.
*/

UWORD InvMod ARG2(UWORD,A,UWORD,B)
{
	UWORD x1,x3,n,c,a=A,b=B;
	int sign = 1;
	x1 = 0; x3 = 1;
/*
	x2 = 1; x4 = 0;
        b = x2*B-x1*A;
		a = x3*A-x4*B;
*/
	for(;;) {
		n = b/a; c = b%a;
		if ( c == 0 ) break;
/*
		c = b-n*a = (x2+n*x4)*B-(x1+n*x3)*A;
		of course we need only the coefficient of A.
*/		
		y = n*x3; x3 = x1+y; x1 = y; b = a; a = c; sign = -sign;
	}
	if ( a != 1 ) return(0);
	if ( sign < 0 ) x3 = B - x3;
	return(x3);
}

/*
  	#] InvMod :
  	#[ MakePrimes :

	Routine creates (or extends) a list of short primes, starting at the
	maximum positive short prime and going downward.
	It stores these primes in a list and keeps with it a list of the
	2-log of the products of all the primes to this point.
*/

int CheckPrime ARG1(int,p)
{
	int i;
	if ( ( p & 1 ) == 0 ) return(0); /* is not prime */
	for ( i = 3; i < p; i += 2 ) {
		if ( i*i > p ) break;
		if ( p % i == 0 ) return(0);
	}
	return(1);
}

int TwoLog ARG2(UWORD *,a,WORD,na)
{
	int j, k;
	UWORD m,nn;
	if ( na < 0 ) na = -na;
	for ( j = 0, nn = a[na-1]; j < BITSINWORD; j++, nn >>= 1 ) {
		if ( nn == 0 ) break;
	}
	return(na*BITSINWORD + j);
}

UWORD *mkprimescrat = 0;
WORD mkprimenscrat = 0;
WORD startprimesat = MAXPOSITIVE;
UWORD *primelist = 0;
int *logprimelist = 0;
int nprimelist = 0;
int primelistsize = 0;

int MakePrimes ARG2(UWORD *,a,WORD,na)
{
	int i;
	UWORD k;
	if ( mkprimescrat == 0 ) {
		mkprimescrat = (UWORD *)Malloc1((AM.MaxTal+2)*sizeof(UWORD),"MakePrimes");
		mkprimescrat[0] = 1; mkprimenscrat = 1;
	}
	for ( i = startprimesat; i > 1; i -= 2 ) {
		if ( CheckPrime(i) ) {
			if ( nprimelist >= primelistsize ) {
				UWORD *p1;
				int *p2, ns, j;
				ns = 2*primelistsize;
				if ( ns <= 0 ) ns = 12;
				p1 = (UWORD *)Malloc1(sizeof(UWORD)*ns,"MakePrimes1");
				p2 = (int *)Malloc1(sizeof(int)*ns,"MakePrimes2");
				for ( j = 0; j < primelistsize; j++ ) {
					p1[j] = primelist[j];
					p2[j] = logprimelist[j];
				}
				if ( primelist ) M_free(primelist,"MakePrimes1");
				if ( logprimelist ) M_free(logprimelist,"MakePrimes2");
				primelist = p1; logprimelist = p2; primelistsize = ns;
			}
			primelist[nprimelist] = i;
			k = i;
			MulLong(mkprimescrat,mkprimenscrat,&k,1,mkprimescrat,&mkprimenscrat);
			logprimelist[nprimelist++] = TwoLog(mkprimescrat,mkprimenscrat);
			if ( BigLong(mkprimescrat,mkprimenscrat,a,na) > 0 ) {
				startprimesat = i-2;
				return(0);
			}
		}
	}
	LOCK(ErrorMessageLock);
	MesPrint("Input in MakePrimes too large to work with all short primes");
	UNLOCK(ErrorMessageLock);
	return(1);
}

#endif

/*
  	#] MakePrimes :
*/

