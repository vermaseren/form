/** @file factor.c
 *
 *	The routines for finding (one term) factors in dollars and expressions.
 *	In addition there are some (unused?) experimental routines.
 */
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

int ModulusGCD1(WORD modu, WORD fun1, WORD fun2, WORD *term, WORD sym)
{
	GETIDENTITY
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
					m1 = AN.mgscr1; AN.mgscr1 = AN.mgscr2; AN.mgscr2 = m1; x1 = n1; n1 = n2; n2 = x1;
				}
				for(;;) {
					y1 = AN.mgscr1[n1]; y2 = AN.mgscr2[n2];
					AN.mgscr1[n1] = 0; offset = n1-n2;
					for ( i = n2 - 1; i >= 0; i-- ) {
						y = ( AN.mgscr1[i+offset] * y2 - AN.mgscr2[i] * y1 ) % modu;
						if ( y < 0 ) y += modu;
						AN.mgscr1[i+offset] = y;
					}
					for ( i = offset-1; i >= 0; i-- ) {
						y = (AN.mgscr1[i]*y2) % modu;
						if ( y < 0 ) y += modu;
						AN.mgscr1[i] = y;
					}
					while ( n1 > 0 && AN.mgscr1[n1] == 0 ) n1--;
					if ( n1 == 0 && AN.mgscr1[n1] == 0 ) break;
					if ( n1 < n2 ) {
						m1 = AN.mgscr1; AN.mgscr1 = AN.mgscr2; AN.mgscr2 = m1; x1 = n1; n1 = n2; n2 = x1;
					}
				}
				m1 = AN.mgscr1; AN.mgscr1 = AN.mgscr2; AN.mgscr2 = m1; x1 = n1; n1 = n2; n2 = x1;
			}
		}
		t += t[1];
	}
/*
		The answer resides now in AN.mgscr1. Paste it into term after the last t1.
*/
	if ( t1 ) {
		if ( n1 == 0 ) {
			offset = FUNHEAD+2;
		}
		else if ( n1 ==  1 && AN.mgscr1[0] == 0 ) {
			offset = FUNHEAD+2;
		}
		else {
			offset = FUNHEAD + ARGHEAD;
			if ( AN.mgscr1[0] != 0 ) offset += 4;
			for ( i = 1; i <= n1; i++ ) {
				if ( AN.mgscr1[i] != 0 ) offset += 8;
			}
		}
		t = t1 + t1[1];
		m1 = term + *term;
		m2 = m1 + offset;
		*term +=  offset;
		while ( m1 > t ) *--m2 = *--m1;
		*t++ = fun2; *t++ = offset; *t++ = 1; FILLFUN3(t);
		if ( n1 == 0 ) {
			*t++ = -SNUMBER; *t++ = AN.mgscr1[0];
		}
		else if ( n1 == 1 && AN.mgscr1[0] == 0 ) {
			*t++ = -SYMBOL;
			*t++ = sym;
		}
		else {
			*t++ = offset - FUNHEAD; *t++ = 1; FILLARG(t);
			if ( AN.mgscr1[0] != 0 ) {
				*t++ = 4; *t++ = AN.mgscr1[0]; *t++ = 1; *t++ = 3;
			}
			for ( i = 1; i <= n1; i++ ) {
				if ( AN.mgscr1[i] != 0 ) {
					*t++ = 8; *t++ = SYMBOL; *t++ = 4; *t++ = sym; *t++ = i;
					*t++ = AN.mgscr1[i]; *t++ = 1; *t++ = 3;
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

int MakeMono(WORD modu, WORD *t, WORD whichbuffer, WORD sym)
{
	GETIDENTITY
	WORD *tstop = t + t[1], *tt, *ttt, cs, maxpow, i, n, *m, *w1, *w2, rl;
	WORD ncmod, cmod;
	if ( AN.nmgscr == 0 ) {
		AN.nmgscr = 40;
		AN.mgscr3 = AN.mgscr1 = (WORD *)Malloc1(2*(AN.nmgscr+1)*sizeof(WORD),"MakeMono");
		AN.mgscr2 = AN.mgscr1 + AN.nmgscr + 1;
	}
	if ( whichbuffer == 0 ) m = AN.mgscr1;
	else                    m = AN.mgscr2;
/*
	First the special cases
*/
	t += FUNHEAD;
	if ( *t == -SNUMBER && t+2 == tstop ) {
		if ( t[1] < 0 ) {
			AN.mgscr2[0] = -((-t[1]) % modu);
			if ( AN.mgscr2[0] < 0 ) AN.mgscr2[0] += modu;
		}
		else {
			AN.mgscr2[0] = t[1] % modu;
		}
		return(0);
	}
	else if ( *t == -SYMBOL && t+2 == tstop && t[1] == sym ) {
		AN.mgscr2[0] = 0; AN.mgscr2[1] = 1; return(1);
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
	ncmod = 1; cmod = modu;
	if ( maxpow > AN.nmgscr ) {	/* extend the buffer? */
		WORD *m1;
		AN.nmgscr = maxpow;
		m1 = (WORD *)Malloc1(2*(AN.nmgscr+1)*sizeof(WORD),"MakeMono");
		AN.mgscr2 = m1 + AN.nmgscr + 1;
		if ( whichbuffer > 0 ) {
			for ( i = 0; i < whichbuffer; i++ ) m1[i] = AN.mgscr1[i];
		}
		M_free(AN.mgscr3,"ModulusGCD1");
		AN.mgscr1 = m1;
		if ( whichbuffer == 0 ) m = AN.mgscr1;
		else                    m = AN.mgscr2;
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
		if ( TakeModulus((UWORD *)(AT.WorkPointer),&rl,&cmod,ncmod,0) < 0 ) {
			return(-1);
		}
		m[n] = *(AT.WorkPointer);
		tt = ttt;
	}
	return(maxpow);
}

/*
  	#] MakeMono :
  	#[ DivMod :

	Takes the modulus a%b and returns it. We assume that b fits inside a word.
*/

WORD DivMod(UWORD *a, WORD na, WORD b)
{
	int la;
	long x = 0;
	la = ABS(na);
	while ( la > 0 ) { x = ((x << BITSINWORD) + a[--la]) % b; }
	if ( na < 0 && x != 0 ) x = b - x;
	return ( (WORD)x );
}

/*
  	#] DivMod :
  	#[ DivShort :

	Divides the long integer a by the short word b. Result in c.
	The remainder is returned.

WORD DivShort(UWORD *a, WORD na, UWORD b, UWORD *c, WORD *nc)
{
	int la, lb;
	long x = 0, y;
	lb = la = ABS(na);
	while ( --la > 0 ) {
		y = (x << BITSINWORD) + a[la];
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

  	#] DivShort :
  	#[ FactorIn :

	This routine tests for a factor in a dollar expression.

	Note that unlike with regular active or hidden expressions we cannot
	add memory as easily as dollars are rather volatile.
*/

int FactorIn(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *t, *tstop, *m, *mm, *oldwork, *mstop, *n1, *n2, *n3, *n4, *n1stop, *n2stop;
	WORD *r1, *r2, *r3, *r4, j, k, kGCD, kGCD2, kLCM, jGCD, kkLCM, jLCM, size;
	int fromwhere = 0, i;
	DOLLARS d;
	t = term; GETSTOP(t,tstop); t++;
	while ( ( t < tstop ) && ( *t != FACTORIN || ( ( *t == FACTORIN )
	 && ( t[FUNHEAD] != -DOLLAREXPRESSION || t[1] != FUNHEAD+2 ) ) ) ) t += t[1];
	if ( t >= tstop ) {
		LOCK(ErrorMessageLock);
		MesPrint("Internal error. Could not find proper factorin_ function.");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	oldwork = AT.WorkPointer;
	d = Dollars + t[FUNHEAD+1];
#ifdef WITHPTHREADS
	{
		int nummodopt, dtype = -1;
		if ( AS.MultiThreaded ) {
			for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
				if ( t[FUNHEAD+1] == ModOptdollars[nummodopt].number ) break;
			}
			if ( nummodopt < NumModOptdollars ) {
				dtype = ModOptdollars[nummodopt].type;
				if ( dtype == MODLOCAL ) {
					d = ModOptdollars[nummodopt].dstruct+AT.identity;
				}
			}
		}
	}
#endif

	if ( d->type == DOLTERMS ) {
		fromwhere = 1;
	}
	else if ( ( d = DolToTerms(t[FUNHEAD+1]) ) == 0 ) {
/*
		The variable cannot convert to an expression
		We replace the function by 1.
*/
		m = oldwork; n1 = term;
		while ( n1 < t ) *m++ = *n1++;
		n1 = t + t[1]; tstop = term + *term;
		while ( n1 < tstop ) *m++ = *n1++;
		*oldwork = m - oldwork;
		AT.WorkPointer = m;
		if ( Generator(BHEAD oldwork,level) ) return(-1);
		AT.WorkPointer = oldwork;
		return(0);
	}
	if ( d->where[0] == 0 ) {
		if ( fromwhere == 0 ) M_free(d,"Dollar in FactorIn_");
		return(0);
	}
/*
	Now we have an expression in d->where. Find the symbolic factor that
	divides the expression and the numerical factor that makes all
	coefficients integer.

	For the symbolic factor we make a copy of the first term, and then
	go through all terms, scratching in the copy the objects that do not
	occur in the terms.
*/
	m = oldwork;
	mm = d->where;
	k = *mm - ABS((mm[*mm-1]));
	for ( j = 0; j < k; j++ ) *m++ = *mm++;
	mstop = m;
	*oldwork = k;
/*
	The copy is in place. Now search through the terms. Start at the second term
*/
	mm = d->where + d->where[0];
	while ( *mm ) {
		m = oldwork+1;
		r2 = mm+*mm;
		r2 -= ABS(r2[-1]);
		r1 = mm+1;
		while ( m < mstop ) {
			while ( r1 < r2 ) {
				if ( *r1 != *m ) {
					r1 += r1[1]; continue;
				}
/*
				Now the various cases
			#[ SYMBOL :
*/
				if ( *m == SYMBOL ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 ) { n2 += 2; continue; }
							if ( n1[1] > 0 ) {
								if ( n2[1] < 0 ) { n2 += 2; continue; }
								if ( n2[1] < n1[1] ) n1[1] = n2[1];
							}
							else {
								if ( n2[1] > 0 ) { n2 += 2; continue; }
								if ( n2[1] > n1[1] ) n1[1] = n2[1];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n1 += 2;
					}
					break;
				}
/*
			#] SYMBOL :
			#[ DOTPRODUCT :
*/
				else if ( *m == DOTPRODUCT ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 || n1[1] != n2[1] ) { n2 += 3; continue; }
							if ( n1[2] > 0 ) {
								if ( n2[2] < 0 ) { n2 += 3; continue; }
								if ( n2[2] < n1[2] ) n1[2] = n2[2];
							}
							else {
								if ( n2[2] > 0 ) { n2 += 3; continue; }
								if ( n2[2] > n1[2] ) n1[2] = n2[2];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 5 ) goto scratch;
							m[1] -= 3;
							n3 = n1; n4 = n1+3;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 3; n1stop -= 3;
							continue;
						}
						n1 += 3;
					}
					break;
				}
/*
			#] DOTPRODUCT :
			#[ VECTOR :
*/
				else if ( *m == VECTOR ) {
/*
					Here we have to be careful if there is more than
					one of the same
*/
					n1 = m+2; n1stop = m+m[1];
					n2 = r1+2;n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						while ( n2 < n2stop ) {
							if ( *n1 == *n2 && n1[1] == n2[1] ) {
								n2 += 2; goto nextn1;
							}
							n2 += 2;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n2 = r1+2;
nextn1:					n1 += 2;
					}
					break;
				}
/*
			#] VECTOR :
			#[ REMAINDER :
*/
				else {
/*
					Again: watch for multiple occurrences of the same object
*/
					if ( m[1] != r1[1] ) { r1 += r1[1]; continue; }
					for ( j = 2; j < m[1]; j++ ) {
						if ( m[j] != r1[j] ) break;
					}
					if ( j < m[1] ) { r1 += r1[1]; continue; }
					r1 += r1[1]; /* to restart at the next potential match */
					goto nextm;  /* match */
				}
/*
			#] REMAINDER :
*/
			}
            if ( r1 >= r2 ) { /* no factor! */
scratch:;
				r3 = m + m[1]; r4 = m;
				while ( r3 < mstop ) *r4++ = *r3++;
				*oldwork = r4 - oldwork;
				if ( *oldwork == 1 ) goto nofactor;
				mstop = r4;
				r1 = mm + 1;
				continue;
			}
			r1 = mm + 1;
nextm:		m += m[1];
		}
		mm = mm + *mm;
	}

nofactor:;
/*
	For the coefficient we have to determine the LCM of the denominators
	and the GCD of the numerators.
*/
#ifdef INDIVIDUALALLOC
	if ( AN.GCDbuffer == 0 ) {
		AN.GCDbuffer  = (UWORD *)Malloc1(5*(AM.MaxTal+2)*sizeof(UWORD),"GCDbuffer");
		AN.GCDbuffer2 = AN.GCDbuffer + AM.MaxTal+2;
		AN.LCMbuffer  = AN.GCDbuffer2 + AM.MaxTal+2;
		AN.LCMb = AN.LCMbuffer + AM.MaxTal+2;
		AN.LCMc = AN.LCMb + AM.MaxTal+2;
	}
#endif
	r1 = d->where;
/*
	First take the first term to load up the LCM and the GCD
*/
	r2 = r1 + *r1;
	j = r2[-1];
	r3 = r2 - ABS(j);
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kGCD = 0; kGCD < k; kGCD++ ) AN.GCDbuffer[kGCD] = r3[kGCD];
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	r3 += k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kLCM = 0; kLCM < k; kLCM++ ) AN.LCMbuffer[kLCM] = r3[kLCM];
	r1 = r2;
/*
	Now go through the rest of the terms in this dollar buffer.
*/
	while ( *r1 ) {
		r2 = r1 + *r1;
		j = r2[-1];
		r3 = r2 - ABS(j);
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( AN.GCDbuffer[0] == 1 ) && ( kGCD == 1 ) ) ) {
/*
			GCD is already 1
*/
		}
		else if ( ( ( k != 1 ) || ( r3[0] != 1 ) ) ) {
			if ( GcdLong(BHEAD AN.GCDbuffer,kGCD,(UWORD *)r3,k,AN.GCDbuffer2,&kGCD2) ) {
				goto onerror;
			}
			kGCD = kGCD2;
			for ( i = 0; i < kGCD; i++ ) AN.GCDbuffer[i] = AN.GCDbuffer2[i];
		}
		else {
			kGCD = 1; AN.GCDbuffer[0] = 1;
		}
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		r3 += k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( AN.LCMbuffer[0] == 1 ) && ( kLCM == 1 ) ) ) {
			for ( kLCM = 0; kLCM < k; kLCM++ )
				AN.LCMbuffer[kLCM] = r3[kLCM];
		}
		else if ( ( k != 1 ) || ( r3[0] != 1 ) ) {
			if ( GcdLong(BHEAD AN.LCMbuffer,kLCM,(UWORD *)r3,k,AN.LCMb,&kkLCM) ) {
				goto onerror;
			}
			DivLong((UWORD *)r3,k,AN.LCMb,kkLCM,AN.LCMb,&kkLCM,AN.LCMc,&jLCM);
			MulLong(AN.LCMbuffer,kLCM,AN.LCMb,kkLCM,AN.LCMc,&jLCM);
			for ( kLCM = 0; kLCM < jLCM; kLCM++ )
				AN.LCMbuffer[kLCM] = AN.LCMc[kLCM];
		}
		else {} /* LCM doesn't change */
		r1 = r2;
	}
/*
	Now put the factor together: GCD/LCM
*/
	r3 = (WORD *)(AN.GCDbuffer);
	if ( kGCD == kLCM ) {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ )
			r3[jGCD+kGCD] = AN.LCMbuffer[jGCD];
		k = kGCD;
	}
	else if ( kGCD > kLCM ) {
		for ( jGCD = 0; jGCD < kLCM; jGCD++ )
			r3[jGCD+kGCD] = AN.LCMbuffer[jGCD];
		for ( jGCD = kLCM; jGCD < kGCD; jGCD++ )
			r3[jGCD+kGCD] = 0;
		k = kGCD;
	}
	else {
		for ( jGCD = kGCD; jGCD < kLCM; jGCD++ )
			r3[jGCD] = 0;
		for ( jGCD = 0; jGCD < kLCM; jGCD++ )
			r3[jGCD+kLCM] = AN.LCMbuffer[jGCD];
		k = kLCM;
	}
	j = 2*k+1;
	mm = m = oldwork + oldwork[0];
/*
	Now compose the new term
*/
	n1 = term;
	while ( n1 < t ) *m++ = *n1++;
	n1 += n1[1];
	n2 = oldwork+1;
	while ( n2 < mm ) *m++ = *n2++;
	while ( n1 < tstop ) *m++ = *n1++;
/*
	And the coefficient
*/
	size = term[*term-1];
	size = REDLENG(size);
	if ( MulRat(BHEAD (UWORD *)tstop,size,(UWORD *)r3,k,
								(UWORD *)m,&size) ) goto onerror;
	size = INCLENG(size);
	k = size < 0 ? -size: size;
	m[k-1] = size;
	m += k;
	*mm = (WORD)(m - mm);
	AT.WorkPointer = m;
	if ( Generator(BHEAD mm,level) ) return(-1);
	AT.WorkPointer = oldwork;
	if ( fromwhere == 0 ) M_free(d,"Dollar in FactorIn");
	return(0);
onerror:
	AT.WorkPointer = oldwork;
	LOCK(ErrorMessageLock);
	MesCall("FactorIn");
	UNLOCK(ErrorMessageLock);
	return(-1);
}

/*
  	#] FactorIn :
  	#[ FactorInExpr :

	This routine tests for a factor in an active or hidden expression.

	The factor from the last call is stored in a cache.
	Main problem here is whether the cache is global or private to each thread.
	A global cache gives most likely the same traffic jam for the computation
	as a local cache. The local cache may stay valid longer.
	In the future we may make it such that threads can look at the cache
	of the others, or even whether a certain factor is under construction.
*/

int FactorInExpr(PHEAD WORD *term, WORD level)
{
	GETBIDENTITY
	WORD *t, *tstop, *m, *oldwork, *mstop, *n1, *n2, *n3, *n4, *n1stop, *n2stop;
	WORD *r1, *r2, *r3, *r4, j, k, kGCD, kGCD2, kLCM, jGCD, kkLCM, jLCM, size, sign;
	WORD *newterm, expr = 0;
	WORD olddeferflag = AR.DeferFlag, oldgetfile = AR.GetFile, oldhold = AR.KeptInHold;
	WORD newgetfile, newhold;
	int i;
	EXPRESSIONS e;
	FILEHANDLE *file = 0;
	POSITION position, oldposition, startposition;
	WORD *oldcpointer = AR.CompressPointer;
	t = term; GETSTOP(t,tstop); t++;
	while ( t < tstop ) {
		if ( *t == FACTORIN && t[1] == FUNHEAD+2 && t[FUNHEAD] == -EXPRESSION ) {
			expr = t[FUNHEAD+1];
			break;
		}
		t += t[1];
	}
	if ( t >= tstop ) {
		LOCK(ErrorMessageLock);
		MesPrint("Internal error. Could not find proper factorin_ function.");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	oldwork = AT.WorkPointer;
	if ( AT.previousEfactor && ( expr == AT.previousEfactor[0] ) ) {
/*
		We have a hit in the cache. Construct the new term.
		At the moment AT.previousEfactor[1] is reserved for future flags
*/
		goto PutTheFactor;
	}
/*
	No hit. We have to do the work. We start with constructing the factor
	in the WorkSpace. Later we will move it to the cache.
	Finally we will jump to PutTheFactor.
*/
	e = Expressions + expr;
	switch ( e->status ) {
		case LOCALEXPRESSION:
		case SKIPLEXPRESSION:
		case DROPLEXPRESSION:
		case GLOBALEXPRESSION:
		case SKIPGEXPRESSION:
		case DROPGEXPRESSION:
/*
			Expression is to be found in the input Scratch file.
			Set the file handle and the position.
			The rest is done by GetTerm.
*/
			newhold = 0;
			newgetfile = 1;
			file = AR.infile;
			break;
		case HIDDENLEXPRESSION:
		case HIDDENGEXPRESSION:
		case HIDELEXPRESSION:
		case HIDEGEXPRESSION:
		case DROPHLEXPRESSION:
		case DROPHGEXPRESSION:
		case UNHIDELEXPRESSION:
		case UNHIDEGEXPRESSION:
/*
			Expression is to be found in the hide Scratch file.
			Set the file handle and the position.
			The rest is done by GetTerm.
*/
			newhold = 0;
			newgetfile = 2;
			file = AR.hidefile;
			break;
		case STOREDEXPRESSION:
/*
			This is an 'illegal' case
*/
			LOCK(ErrorMessageLock);
			MesPrint("Error: factorin_ cannot determine factors in stored expressions.");
			UNLOCK(ErrorMessageLock);
			return(-1);
		case DROPPEDEXPRESSION:
/*
			We replace the function by 1.
*/
			m = oldwork; n1 = term;
			while ( n1 < t ) *m++ = *n1++;
			n1 = t + t[1]; tstop = term + *term;
			while ( n1 < tstop ) *m++ = *n1++;
			*oldwork = m - oldwork;
			AT.WorkPointer = m;
			if ( Generator(BHEAD oldwork,level) ) return(-1);
			AT.WorkPointer = oldwork;
			return(0);
		default:
			LOCK(ErrorMessageLock);
			MesPrint("Error: Illegal expression in factorinexpr.");
			UNLOCK(ErrorMessageLock);
			return(-1);
	}
/*
	Before we start with the file we set the buffers for the coefficient
	For the coefficient we have to determine the LCM of the denominators
	and the GCD of the numerators.
*/
#ifdef INDIVIDUALALLOC
	if ( AN.GCDbuffer == 0 ) {
		AN.GCDbuffer  = (UWORD *)Malloc1(5*(AM.MaxTal+2)*sizeof(UWORD),"GCDbuffer");
		AN.GCDbuffer2 = AN.GCDbuffer + AM.MaxTal+2;
		AN.LCMbuffer  = AN.GCDbuffer2 + AM.MaxTal+2;
		AN.LCMb = AN.LCMbuffer + AM.MaxTal+2;
		AN.LCMc = AN.LCMb + AM.MaxTal+2;
	}
#endif
	position = AS.OldOnFile[expr];
	AR.DeferFlag = 0; AR.KeptInHold = newhold; AR.GetFile = newgetfile;
	SeekScratch(file,&oldposition);
	SetScratch(file,&position);
	if ( GetTerm(BHEAD oldwork) <= 0 ) {
		LOCK(ErrorMessageLock);
		MesPrint("Expression %d has problems in scratchfile",expr);
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	SeekScratch(file,&startposition);
	SeekScratch(file,&position);
/*
	Load the first term in the workspace
*/
	if ( GetTerm(BHEAD oldwork) == 0 ) {
		SetScratch(file,&oldposition); /* We still need this untill Processor is clean */
		AR.DeferFlag = olddeferflag;
		oldwork[0] = 4; oldwork[1] = 1; oldwork[2] = 1; oldwork[3] = 3; 
		goto Complete;
	}
	SeekScratch(file,&position);
	AR.DeferFlag = olddeferflag; AR.KeptInHold = oldhold; AR.GetFile = oldgetfile;

	r2 = m = oldwork + *oldwork;
	j = m[-1];
	m -= ABS(j);
	*oldwork = (WORD)(m-oldwork);
	AT.WorkPointer = newterm = mstop = m;
/*
	Now take the coefficient of the first term to load up the LCM and the GCD
*/
	r3 = m;
	k = REDLENG(j);
	if ( k < 0 ) { k = -k; sign = -1; }
	else { sign = 1; }
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kGCD = 0; kGCD < k; kGCD++ ) AN.GCDbuffer[kGCD] = r3[kGCD];
	k = REDLENG(j);
	if ( k < 0 ) k = -k;
	r3 += k;
	while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
	for ( kLCM = 0; kLCM < k; kLCM++ ) AN.LCMbuffer[kLCM] = r3[kLCM];
/*
	The copy and the coefficient are in place. Now search through the terms.
*/
	for (;;) {
		AR.DeferFlag = 0; AR.KeptInHold = newhold; AR.GetFile = newgetfile;
		SetScratch(file,&position);
		size = GetTerm(BHEAD newterm);
		SeekScratch(file,&position);
		AR.DeferFlag = olddeferflag; AR.KeptInHold = oldhold; AR.GetFile = oldgetfile;
		if ( size == 0 ) break;
		m = oldwork+1;
		r2 = newterm + *newterm;
		r2 -= ABS(r2[-1]);
		r1 = newterm+1;
		while ( m < mstop ) {
			while ( r1 < r2 ) {
				if ( *r1 != *m ) {
					r1 += r1[1]; continue;
				}
/*
				Now the various cases
			#[ SYMBOL :
*/
				if ( *m == SYMBOL ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 ) { n2 += 2; continue; }
							if ( n1[1] > 0 ) {
								if ( n2[1] < 0 ) { n2 += 2; continue; }
								if ( n2[1] < n1[1] ) n1[1] = n2[1];
							}
							else {
								if ( n2[1] > 0 ) { n2 += 2; continue; }
								if ( n2[1] > n1[1] ) n1[1] = n2[1];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch symbol */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n1 += 2;
					}
					break;
				}
/*
			#] SYMBOL :
			#[ DOTPRODUCT :
*/
				else if ( *m == DOTPRODUCT ) {
					n1 = m+2; n1stop = m+m[1];
					n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						n2 = r1+2;
						while ( n2 < n2stop ) {
							if ( *n1 != *n2 || n1[1] != n2[1] ) { n2 += 3; continue; }
							if ( n1[2] > 0 ) {
								if ( n2[2] < 0 ) { n2 += 3; continue; }
								if ( n2[2] < n1[2] ) n1[2] = n2[2];
							}
							else {
								if ( n2[2] > 0 ) { n2 += 3; continue; }
								if ( n2[2] > n1[2] ) n1[2] = n2[2];
							}
							break;
						}
						if ( n2 >= n2stop ) {	/* scratch dotproduct */
							if ( m[1] == 5 ) goto scratch;
							m[1] -= 3;
							n3 = n1; n4 = n1+3;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 3; n1stop -= 3;
							continue;
						}
						n1 += 3;
					}
					break;
				}
/*
			#] DOTPRODUCT :
			#[ VECTOR :
*/
				else if ( *m == VECTOR ) {
/*
					Here we have to be careful if there is more than
					one of the same
*/
					n1 = m+2; n1stop = m+m[1];
					n2 = r1+2;n2stop = r1+r1[1];
					while ( n1 < n1stop ) {
						while ( n2 < n2stop ) {
							if ( *n1 == *n2 && n1[1] == n2[1] ) {
								n2 += 2; goto nextn1;
							}
							n2 += 2;
						}
						if ( n2 >= n2stop ) {	/* scratch vector */
							if ( m[1] == 4 ) goto scratch;
							m[1] -= 2;
							n3 = n1; n4 = n1+2;
							while ( n4 < mstop ) *n3++ = *n4++;
							*oldwork = n3 - oldwork;
							mstop -= 2; n1stop -= 2;
							continue;
						}
						n2 = r1+2;
nextn1:					n1 += 2;
					}
					break;
				}
/*
			#] VECTOR :
			#[ REMAINDER :
*/
				else {
/*
					Again: watch for multiple occurrences of the same object
*/
					if ( m[1] != r1[1] ) { r1 += r1[1]; continue; }
					for ( j = 2; j < m[1]; j++ ) {
						if ( m[j] != r1[j] ) break;
					}
					if ( j < m[1] ) { r1 += r1[1]; continue; }
					r1 += r1[1]; /* to restart at the next potential match */
					goto nextm;  /* match */
				}
/*
			#] REMAINDER :
*/
			}
            if ( r1 >= r2 ) { /* no factor! */
scratch:;
				r3 = m + m[1]; r4 = m;
				while ( r3 < mstop ) *r4++ = *r3++;
				*oldwork = r4 - oldwork;
				if ( *oldwork == 1 ) goto nofactor;
				mstop = r4;
				r1 = newterm + 1;
				continue;
			}
			r1 = newterm + 1;
nextm:		m += m[1];
		}
nofactor:;
/*
		Now the coefficient
*/
		r2 = newterm + *newterm;
		j = r2[-1];
		r3 = r2 - ABS(j);
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( AN.GCDbuffer[0] == 1 ) && ( kGCD == 1 ) ) ) {
/*
			GCD is already 1
*/
		}
		else if ( ( ( k != 1 ) || ( r3[0] != 1 ) ) ) {
			if ( GcdLong(BHEAD AN.GCDbuffer,kGCD,(UWORD *)r3,k,AN.GCDbuffer2,&kGCD2) ) {
				goto onerror;
			}
			kGCD = kGCD2;
			for ( i = 0; i < kGCD; i++ ) AN.GCDbuffer[i] = AN.GCDbuffer2[i];
		}
		else {
			kGCD = 1; AN.GCDbuffer[0] = 1;
		}
		k = REDLENG(j);
		if ( k < 0 ) k = -k;
		r3 += k;
		while ( ( k > 1 ) && ( r3[k-1] == 0 ) ) k--;
		if ( ( ( AN.LCMbuffer[0] == 1 ) && ( kLCM == 1 ) ) ) {
			for ( kLCM = 0; kLCM < k; kLCM++ )
				AN.LCMbuffer[kLCM] = r3[kLCM];
		}
		else if ( ( k != 1 ) || ( r3[0] != 1 ) ) {
			if ( GcdLong(BHEAD AN.LCMbuffer,kLCM,(UWORD *)r3,k,AN.LCMb,&kkLCM) ) {
				goto onerror;
			}
			DivLong((UWORD *)r3,k,AN.LCMb,kkLCM,AN.LCMb,&kkLCM,AN.LCMc,&jLCM);
			MulLong(AN.LCMbuffer,kLCM,AN.LCMb,kkLCM,AN.LCMc,&jLCM);
			for ( kLCM = 0; kLCM < jLCM; kLCM++ )
				AN.LCMbuffer[kLCM] = AN.LCMc[kLCM];
		}
		else {} /* LCM doesn't change */
	}
	SetScratch(file,&oldposition); /* Needed until Processor is thread safe */
	AR.DeferFlag = olddeferflag;
/*
	Now put the term together in oldwork: GCD/LCM
	We have already the algebraic contents there.
*/
	r3 = (WORD *)(AN.GCDbuffer);
	r4 = (WORD *)(AN.LCMbuffer);
	r1 = oldwork + *oldwork;
	if ( kGCD == kLCM ) {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r3++;
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r4++;
		k = 2*kGCD+1;
	}
	else if ( kGCD > kLCM ) {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r3++;
		for ( jGCD = 0; jGCD < kLCM; jGCD++ ) *r1++ = *r4++;
		for ( jGCD = kLCM; jGCD < kGCD; jGCD++ ) *r1++ = 0;
		k = 2*kGCD+1;
	}
	else {
		for ( jGCD = 0; jGCD < kGCD; jGCD++ ) *r1++ = *r3++;
		for ( jGCD = kGCD; jGCD < kLCM; jGCD++ ) *r1++ = 0;
		for ( jGCD = 0; jGCD < kLCM; jGCD++ ) *r1++ = *r4++;
		k = 2*kLCM+1;
	}
	if ( sign < 0 ) *r1++ = -k;
	else *r1++ = k;
	*oldwork = (WORD)(r1-oldwork);
/*
	Now put the new term in the cache
*/
Complete:;
	if ( AT.previousEfactor ) M_free(AT.previousEfactor,"Efactor cache");
	AT.previousEfactor = (WORD *)Malloc1((*oldwork+2)*sizeof(WORD),"Efactor cache");
	AT.previousEfactor[0] = expr;
	r1 = oldwork; r2 = AT.previousEfactor + 2; k = *oldwork;
	NCOPY(r2,r1,k)
	AT.previousEfactor[1] = 0;
/*
	Now we construct the new term in the workspace.
*/
PutTheFactor:;
	if ( AT.WorkPointer + AT.previousEfactor[2] >= AT.WorkTop ) {
		LOCK(ErrorMessageLock);
		MesWork();
		MesPrint("Called from factorin_");
		UNLOCK(ErrorMessageLock);
		return(-1);
	}
	n1 = oldwork; n2 = term; while ( n2 < t ) *n1++ = *n2++;
	n2 = AT.previousEfactor+2; GETSTOP(n2,n2stop); n3 = n2 + *n2;
	n2++; while ( n2 < n2stop ) *n1++ = *n2++;
	n2 = t + t[1]; while ( n2 < tstop ) *n1++ = *n2++;
	size = term[*term-1];
	size = REDLENG(size);
	k = n3[-1]; k = REDLENG(k);
	if ( MulRat(BHEAD (UWORD *)tstop,size,(UWORD *)n2stop,k,
							(UWORD *)n1,&size) ) goto onerror;
	size = INCLENG(size);
	k = size < 0 ? -size: size;
       n1 += k; n1[-1] = size;
	*oldwork = n1 - oldwork;
	AT.WorkPointer = n1;
	if ( Generator(BHEAD oldwork,level) ) return(-1);
	AT.WorkPointer = oldwork;
	AR.CompressPointer = oldcpointer;
	return(0);
onerror:
	AT.WorkPointer = oldwork;
	AR.CompressPointer = oldcpointer;
	LOCK(ErrorMessageLock);
	MesCall("FactorInExpr");
	UNLOCK(ErrorMessageLock);
	return(-1);
}

/*
  	#] FactorInExpr :
*/

