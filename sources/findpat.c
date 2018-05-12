/** @file findpat.c
 * 
 *  Pattern matching of symbols and dotproducts.
 *  There are various routines because of the options in the id-statements
 *	like once, only, multi and many.
 *	These are amoung the oldest routines in FORM and that can be noticed,
 *	because the interplay with the function matching is not complete.
 *	When we match functions and halfway we fail we can backtrack properly.
 *	With the symbols, the dotproducts and the vectors (in pattern.c) there
 *	is no proper backtracking. Hence the routines here need still quite
 *	some work or may even have to be rewritten.
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
  	#[ Includes : findpat.c
*/

#include "form3.h"

/*
  	#] Includes : 
	#[ Patterns :
 		#[ FindOnly :			WORD FindOnly(term,pattern)

	The current version doesn't scan function arguments yet. 10-Apr-1988

	This routine searches for an exact match. This means in particular:
	1:	x^#	must match exactly.
	2:	x^n? must have a single value for n that cannot be addapted.

	When setp != 0 it points to a collection of sets
	A match can occur only if no object will be left that belongs
	to any of these sets.
*/

WORD FindOnly(PHEAD WORD *term, WORD *pattern)
{
	GETBIDENTITY
	WORD *t, *m;
	WORD *tstop, *mstop;
	WORD *xstop, *ystop, *setp = AN.ForFindOnly;
	WORD n, nt, *p, nq;
	WORD older[NORMSIZE], *q, newval1, newval2, newval3;
	AN.UsedOtherFind = 0;
	m = pattern;
	mstop = m + *m;
	m++;
	t = term;
	t += *term - 1;
	tstop = t - ABS(*t) + 1;
 	t = term;
	t++;
	while ( t < tstop && *t > DOTPRODUCT ) t += t[1];
	while ( m < mstop && *m > DOTPRODUCT ) m += m[1];
	if ( m < mstop ) { do {
/*
			#[ SYMBOLS :
*/
		if ( *m == SYMBOL ) {
			ystop = m + m[1];
			m += 2;
			n = 0;
			p = older;
			if ( t < tstop ) while ( *t != SYMBOL ) {
				t += t[1];
				if ( t >= tstop ) {
OnlyZer1:
					do {
						if ( *m >= 2*MAXPOWER ) return(0);
						if ( m[1] >= 2*MAXPOWER ) nt = m[1];
						else if ( m[1] <= -2*MAXPOWER ) nt = -m[1];
						else return(0);
						nt -= 2*MAXPOWER;
						if ( CheckWild(BHEAD nt,SYMTONUM,0,&newval3) ) return(0);
						AddWild(BHEAD nt,SYMTONUM,0);
						m += 2;
					} while ( m < ystop );
					goto EndLoop;
				}
			}
			else goto OnlyZer1;
			xstop = t + t[1];
			t += 2;
			do {
				if ( *m == *t && t < xstop ) {
					if ( m[1] == t[1] ) { m += 2; t += 2; }
					else if ( m[1] >= 2*MAXPOWER ) {
						nt = t[1];
						nq = m[1];
						goto OnlyL2;
					}
					else if ( m[1] <= -2*MAXPOWER ) {
						nt = -t[1];
						nq = -m[1];
OnlyL2:					nq -= 2*MAXPOWER;
						if ( CheckWild(BHEAD nq,SYMTONUM,nt,&newval3) ) return(0);
						AddWild(BHEAD nq,SYMTONUM,nt);
						m += 2;
						t += 2;
					}
					else {
						*p++ = *t++; *p++ = *t++; n += 2;
					}
				}
				else if ( *m >= 2*MAXPOWER ) {
					while ( t < xstop ) { *p++ = *t++; *p++ = *t++; n += 2; }
					nq = n;
					p = older;
					while ( nq > 0 ) {
						if ( !CheckWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,*p,&newval1) ) {
							if ( m[1] == p[1] ) {
								AddWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,newval1);
								break;
							}
							else if ( m[1] >= 2*MAXPOWER && m[1] != *m ) {
								if ( !CheckWild(BHEAD m[1]-2*MAXPOWER,SYMTONUM,p[1],&newval3) ) {
									AddWild(BHEAD m[1]-2*MAXPOWER,SYMTONUM,p[1]);
									AddWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,newval1);
									break;
								}
							}
							else if ( m[1] <= -2*MAXPOWER && m[1] != -(*m) ) {
								if ( !CheckWild(BHEAD -m[1]-2*MAXPOWER,SYMTONUM,-p[1],&newval3) ) {
									AddWild(BHEAD -m[1]-2*MAXPOWER,SYMTONUM,-p[1]);
									AddWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,newval1);
									break;
								}
							}
						}
						nq -= 2;
						p += 2;
					}
					if ( nq <= 0 ) return(0);
					nq -= 2;
					n -= 2;
					q = p + 2;
					while ( --nq >= 0 ) *p++ = *q++;
					m += 2;
				}
				else {
					if ( t >= xstop || *m < *t ) {
						if ( m[1] >= 2*MAXPOWER ) nt = m[1];
						else if ( m[1] <= -2*MAXPOWER ) nt = -m[1];
						else return(0);
						nt -= 2*MAXPOWER;
						if ( CheckWild(BHEAD nt,SYMTONUM,0,&newval3) ) return(0);
						AddWild(BHEAD nt,SYMTONUM,0);
						m += 2;
					}
					else {
						*p++ = *t++; *p++ = *t++; n += 2;
					}
				}
			} while ( m < ystop );
			if ( setp ) {
				while ( t < xstop ) { *p++ = *t++; *p++ = *t++; n+= 2; }
				p = older;
				while ( n > 0 ) {
					nq = setp[1] - 2;
					m = setp + 2;
					while ( --nq >= 0 ) {
						if ( Sets[*m].type != CSYMBOL ) { m++; continue; }
						t = SetElements + Sets[*m].first;
						tstop = SetElements + Sets[*m].last;
						while ( t < tstop ) {
							if ( *t++ == *p ) return(0);
						}
						m++;
					}
					n -= 2;
					p += 2;
				}
			}
			return(1);
		}
/*
			#] SYMBOLS : 
			#[ DOTPRODUCTS :
*/
		else if ( *m == DOTPRODUCT ) {
			ystop = m + m[1];
			m += 2;
			n = 0;
			p = older;
			if ( t < tstop ) {
				if ( *t < DOTPRODUCT ) goto OnlyZer2;
				while ( *t > DOTPRODUCT ) {
				t += t[1];
				if ( t >= tstop || *t < DOTPRODUCT ) {
OnlyZer2:
					do {
						if ( *m >= (AM.OffsetVector+WILDOFFSET)
						|| m[1] >= (AM.OffsetVector+WILDOFFSET) ) return(0);
						if ( m[2] >= 2*MAXPOWER ) nq = m[2];
						else if ( m[2] <= -2*MAXPOWER ) nq = -m[2];
						else return(0);
						nq -= 2*MAXPOWER;
						if ( CheckWild(BHEAD nq,SYMTONUM,0,&newval3) ) return(0);
						AddWild(BHEAD nq,SYMTONUM,0);
						m += 3;
					} while ( m < ystop );
					goto EndLoop;
				}
				}
			}
			else goto OnlyZer2;
			xstop = t + t[1];
			t += 2;
			do {
				if ( *m == *t && m[1] == t[1] && t < xstop ) {
					if ( t[2] != m[2] ) {
						if ( m[2] >= 2*MAXPOWER ) {
							nq = m[2];
							nt = t[2];
						}
						else if ( m[2] <= -2*MAXPOWER ) {
							nq = -m[2];
							nt = -t[2];
						}
						else return(0);
						nq -= 2*MAXPOWER;
						if ( CheckWild(BHEAD nq,SYMTONUM,nt,&newval3) ) return(0);
						AddWild(BHEAD nq,SYMTONUM,nt);
					}
					t += 3; m += 3;
				}
				else if ( *m >= (AM.OffsetVector+WILDOFFSET) ) {
					while ( t < xstop ) {
						*p++ = *t++; *p++ = *t++; *p++ = *t++; n += 3;
					}
					nq = n;
					p = older;
					while ( nq > 0 ) {
						if ( *m == m[1] ) {
							if ( *p != p[1] ) goto NextInDot;
						}
						if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*p,&newval1) &&
							 !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,p[1],&newval2) ) {
							if ( p[2] == m[2] ) {
OnlyL9:							AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval2);
								AddWild(BHEAD *m-WILDOFFSET,VECTOVEC,newval1);
								break;
							}
							if ( m[2] >= 2*MAXPOWER ) {
								if ( !CheckWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,p[2],&newval3) ) {
									AddWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,newval3);
									goto OnlyL9;
								}
							}
							else if ( m[2] <= -2*MAXPOWER ) {
								if ( !CheckWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2],&newval3) ) {
									AddWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2]);
									goto OnlyL9;
								}
							}
						}
						if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,p[1],&newval1) &&
							 !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,*p,&newval2) ) {
							if ( p[2] == m[2] ) {
OnlyL10:						AddWild(BHEAD *m-WILDOFFSET,VECTOVEC,newval1);
								AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval2);
								break;
							}
							if ( m[2] >= 2*MAXPOWER ) {
								if ( !CheckWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,p[2],&newval3) ) {
									AddWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,p[2]);
									goto OnlyL10;
								}
							}
							else if ( m[2] <= -2*MAXPOWER ) {
								if ( !CheckWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2],&newval3) ) {
									AddWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2]);
									goto OnlyL10;
								}
							}
						}
NextInDot:
						p += 3; nq -= 3;
					}
					if ( nq <= 0 ) return(0);
					q = p+3;
					nq -= 3;
					n -= 3;
					while ( --nq >= 0 ) *p++ = *q++;
					m += 3;
				}
				else if ( m[1] >= (AM.OffsetVector+WILDOFFSET) ) {
					while ( *m >= *t && t < xstop ) {
						*p++ = *t++; *p++ = *t++; *p++ = *t++; n += 3;
					}
					nq = n;
					p = older;
					while ( nq > 0 ) {
						if ( *m == *p && !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,p[1],&newval1) ) {
							if ( p[2] == m[2] ) {
								AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
								break;
							}
							else if ( m[2] >= 2*MAXPOWER ) {
								if ( !CheckWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,p[2],&newval3) ) {
									AddWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,p[2]);
									AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
									break;
								}
							}
							else if ( m[2] <= -2*MAXPOWER ) {
								if ( !CheckWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2],&newval3) ) {
									AddWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2]);
									AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
									break;
								}
							}
						}
						if ( *m == p[1] && !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,*p,&newval1) ) {
							if ( p[2] == m[2] ) {
								AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
								break;
							}
							if ( m[2] >= 2*MAXPOWER ) {
								if ( !CheckWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,p[2],&newval3) ) {
									AddWild(BHEAD m[2]-2*MAXPOWER,SYMTONUM,p[2]);
									AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
									break;
								}
							}
							else if ( m[2] <= -2*MAXPOWER ) {
								if ( !CheckWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2],&newval3) ) {
									AddWild(BHEAD -m[2]-2*MAXPOWER,SYMTONUM,-p[2]);
									AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
									break;
								}
							}
						}
						p += 3; nq -= 3;
					}
					if ( nq <= 0 ) return(0);
					q = p+3;
					nq -= 3;
					n -= 3;
					while ( --nq >= 0 ) *p++ = *q++;
					m += 3;
				}
				else {
					if ( t >= xstop || *m < *t || ( *m == *t && m[1] < t[1] ) ) {
						if ( m[2] > 2*MAXPOWER ) nt = m[2];
						else if ( m[2] <= -2*MAXPOWER ) nt = -m[2];
						else return(0);
						nt -= 2*MAXPOWER;
						if ( CheckWild(BHEAD nt,SYMTONUM,0,&newval3) ) return(0);
						AddWild(BHEAD nt,SYMTONUM,0);
						m += 3;
					}
					else {
						*p++ = *t++; *p++ = *t++; *p++ = *t++; n += 3;
					}
				}
			} while ( m < ystop );
			t = xstop;
		}
/*
			#] DOTPRODUCTS : 
*/
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Error in pattern(1)");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
EndLoop:;
	} while ( m < mstop ); }
	if ( setp ) {
/*
		while ( t < tstop && *t > SYMBOL ) t += t[1];
		if ( t < tstop && setp[1] > 2 ) return(0);
*/
						/* There were nonempty sets */
						/* Empty sets are rejected by the compiler */
	}
	return(1);
}

/*
 		#] FindOnly : 
 		#[ FindOnce :			WORD FindOnce(term,pattern)

	Searches for a single match in term. The difference with multi
	lies mainly in the fact that here functions may occur.
	The functions have not been implemented yet. (10-Apr-1988)
	Wildcard powers are adjustable. The value closer to zero is taken.
	Positive and negative gives (o surprise) zero.

*/

WORD FindOnce(PHEAD WORD *term, WORD *pattern)
{
	GETBIDENTITY
	WORD *t, *m;
	WORD *tstop, *mstop;
	WORD *xstop, *ystop;
	WORD n, nt, *p, nq, mt, ch;
	WORD older[2*NORMSIZE], *q, newval1, newval2, newval3;
	AN.UsedOtherFind = 0;
	m = pattern;
	mstop = m + *m;
	m++;
	t = term;
	t += *term - 1;
	tstop = t - ABS(*t) + 1;
 	t = term;
	t++;
	while ( t < tstop && *t > DOTPRODUCT ) t += t[1];
	while ( m < mstop && *m > DOTPRODUCT ) m += m[1];
	if ( m < mstop ) { do {
/*
			#[ SYMBOLS :
*/
		if ( *m == SYMBOL ) {
			ystop = m + m[1];
			m += 2;
			n = 0;
			p = older;
			if ( t < tstop ) while ( *t != SYMBOL ) {
				t += t[1];
				if ( t >= tstop ) {
TryZero:
					do {
						if ( *m >= 2*MAXPOWER ) return(0);
						if ( m[1] >= 2*MAXPOWER ) nt = m[1];
						else if ( m[1] <= -2*MAXPOWER ) nt = -m[1];
						else return(0);
						nt -= 2*MAXPOWER;
						if ( ( ch = CheckWild(BHEAD nt,SYMTONUM,0,&newval3) ) != 0 ) {
							if ( ch > 1 ) return(0);
							if ( AN.oldtype != SYMTONUM ) return(0);
							if ( *AN.MaskPointer == 2 ) return(0);
						}
						AddWild(BHEAD nt,SYMTONUM,0);
						m += 2;
					} while ( m < ystop );
					goto EndLoop;
				}
			}
			else goto TryZero;
			xstop = t + t[1];
			t += 2;
			do {
				if ( *m == *t && t < xstop ) {
					nt = t[1];
					mt = m[1];
					if ( ( mt > 0 && mt <= nt ) ||
						 ( mt < 0 && mt >= nt ) ) { m += 2; t += 2; }
					else if ( mt >= 2*MAXPOWER ) goto OnceL2;
					else if ( mt <= -2*MAXPOWER ) {
						nt = -nt;
						mt = -mt;
OnceL2:					mt -= 2*MAXPOWER;
						if ( ( ch = CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) != 0 ) {
							if ( ch > 1 ) return(0);
							if ( AN.oldtype != SYMTONUM ) return(0);
							if ( AN.oldvalue <= 0 ) {
								if ( nt < AN.oldvalue ) nt = AN.oldvalue;
								else {
									if ( *AN.MaskPointer == 2 ) return(0);
									if ( nt > 0 ) nt = 0;
								}
							}
							if ( AN.oldvalue >= 0 ) {
								if ( nt > AN.oldvalue ) nt = AN.oldvalue;
								else {
									if ( *AN.MaskPointer == 2 ) return(0);
									if ( nt < 0 ) nt = 0;
								}
							}
						}
						AddWild(BHEAD mt,SYMTONUM,nt);
						m += 2;
						t += 2;
					}
					else {
						*p++ = *t++; *p++ = *t++; n += 2;
					}
				}
				else if ( *m >= 2*MAXPOWER ) {
					while ( t < xstop ) { *p++ = *t++; *p++ = *t++; n += 2; }
					nq = n;
					p = older;
					while ( nq > 0 ) {
						nt = p[1];
						if ( !CheckWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,*p,&newval1) ) {
							mt = m[1];
							if ( ( mt > 0 && mt <= nt ) ||
							     ( mt < 0 && mt >= nt ) ) {
								AddWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,newval1);
								break;
							}
							else if ( mt >= 2*MAXPOWER && mt != *m ) {
OnceL4a:						mt -= 2*MAXPOWER;
								if ( ( ch = CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) != 0 ) {
									if ( ch > 1 ) return(0);
									if ( AN.oldtype == SYMTONUM ) {
										if ( AN.oldvalue >= 0 ) {
											if ( nt > AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt < 0 ) nt = 0;
											}
										}
										else {
											if ( nt < AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt > 0 ) nt = 0;
											}
										}
										AddWild(BHEAD mt,SYMTONUM,nt);
										AddWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,newval1);
										break;
									}
								}
								else {
									AddWild(BHEAD mt,SYMTONUM,nt);
									AddWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,newval1);
									break;
								}
							}
							else if ( mt <= -2*MAXPOWER && mt != -(*m) ) {
								nt = -nt;
								mt = -mt;
								goto OnceL4a;
							}
						}
						nq -= 2;
						p += 2;
					}
					if ( nq <= 0 ) return(0);
					nq -= 2;
					n -= 2;
					q = p + 2;
					while ( --nq >= 0 ) *p++ = *q++;
					m += 2;
				}
				else {
					if ( t >= xstop || *m < *t ) {
						if ( m[1] >= 2*MAXPOWER ) nt = m[1];
						else if ( m[1] <= -2*MAXPOWER ) nt = -m[1];
						else return(0);
						nt -= 2*MAXPOWER;
						if ( ( ch = CheckWild(BHEAD nt,SYMTONUM,0,&newval3) ) != 0 ) {
							if ( ch > 1 ) return(0);
							if ( AN.oldtype != SYMTONUM ) return(0);
							if ( *AN.MaskPointer == 2 ) return(0);
						}
						AddWild(BHEAD nt,SYMTONUM,0);
						m += 2;
					}
					else {
						*p++ = *t++; *p++ = *t++; n += 2;
					}
				}
			} while ( m < ystop );
		}
/*
			#] SYMBOLS : 
			#[ DOTPRODUCTS :
*/
		else if ( *m == DOTPRODUCT ) {
			ystop = m + m[1];
			m += 2;
			n = 0;
			p = older;
			if ( t < tstop ) {
				if ( *t < DOTPRODUCT ) goto OnceOp;
				while ( *t > DOTPRODUCT ) {
				t += t[1];
				if ( t >= tstop || *t < DOTPRODUCT ) {
OnceOp:
					do {
						if ( *m >= (AM.OffsetVector+WILDOFFSET)
						|| m[1] >= (AM.OffsetVector+WILDOFFSET) ) return(0);
						if ( m[2] >= 2*MAXPOWER ) {
							nq = m[2] - 2*MAXPOWER;
						}
						else if ( m[2] <= -2*MAXPOWER ) {
							nq = -m[2] - 2*MAXPOWER;
						}
						else return(0);
						if ( CheckWild(BHEAD nq,SYMTONUM,(WORD)0,&newval3) ) {
							if ( AN.oldtype != SYMTONUM ) return(0);
							if ( *AN.MaskPointer == 2 ) return(0);
						}
						AddWild(BHEAD nq,SYMTONUM,(WORD)0);
						m += 3;
					} while ( m < ystop );
					goto EndLoop;
				}
				}
			}
			else goto OnceOp;
			xstop = t + t[1];
			t += 2;
			do {
				if ( *m == *t && m[1] == t[1] && t < xstop ) {
					nt = t[2];
					mt = m[2];
/*
					if ( ( nt > 0 && nt < mt ) ||
					     ( nt < 0 && nt > mt ) ) {
						if ( mt <= -2*MAXPOWER ) {
							mt = -mt;
							nt = -nt;
						}
						else if ( mt < 2*MAXPOWER ) return(0);
						mt -= 2*MAXPOWER;
						if ( CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) {
							if ( AN.oldtype != SYMTONUM ) return(0);
							if ( AN.oldvalue <= 0 ) {
								if ( nt < AN.oldvalue ) nt = AN.oldvalue;
								else {
									if ( *AN.MaskPointer == 2 ) return(0);
									if ( nt > 0 ) nt = 0;
								}
							}
							if ( AN.oldvalue >= 0 ) {
								if ( nt > AN.oldvalue ) nt = AN.oldvalue;
								else {
									if ( *AN.MaskPointer == 2 ) return(0);
									if ( nt < 0 ) nt = 0;
								}
							}
						}
						AddWild(BHEAD mt,SYMTONUM,nt);
						m += 3; t += 3;
					}
					else if ( ( nt > 0 && nt >= mt && mt > -2*MAXPOWER )
					|| ( nt < 0 && nt <= mt && mt < 2*MAXPOWER ) ) {
						m += 3; t += 3;
					}
*/
					if ( ( mt > 0 && mt <= nt ) ||
						 ( mt < 0 && mt >= nt ) ) { m += 3; t += 3; }
					else if ( mt >= 2*MAXPOWER ) goto OnceL7;
					else if ( mt <= -2*MAXPOWER ) {
						nt = -nt;
						mt = -mt;
OnceL7:					mt -= 2*MAXPOWER;
						if ( CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) {
							if ( AN.oldtype != SYMTONUM ) return(0);
							if ( AN.oldvalue <= 0 ) {
								if ( nt < AN.oldvalue ) nt = AN.oldvalue;
								else {
									if ( *AN.MaskPointer == 2 ) return(0);
									if ( nt > 0 ) nt = 0;
								}
							}
							if ( AN.oldvalue >= 0 ) {
								if ( nt > AN.oldvalue ) nt = AN.oldvalue;
								else {
									if ( *AN.MaskPointer == 2 ) return(0);
									if ( nt < 0 ) nt = 0;
								}
							}
						}
						AddWild(BHEAD mt,SYMTONUM,nt);
						m += 3;
						t += 3;
					}
					else {
						*p++ = *t++; *p++ = *t++; *p++ = *t++; n += 3;
					}
				}
				else if ( *m >= (AM.OffsetVector+WILDOFFSET) ) {
					while ( t < xstop ) {
						*p++ = *t++; *p++ = *t++; *p++ = *t++; n += 3;
					}
					nq = n;
					p = older;
					while ( nq > 0 ) {
						if ( *m == m[1] ) {
							if ( *p != p[1] ) goto NextInDot;
						}
						if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*p,&newval1) &&
							 !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,p[1],&newval2) ) {
							nt = p[2];
							mt = m[2];
							if ( ( mt > 0 && nt >= mt ) ||
								 ( mt < 0 && nt <= mt ) ) {
OnceL9:							AddWild(BHEAD *m-WILDOFFSET,VECTOVEC,newval1);
								AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval2);
								break;
							}
							if ( mt >= 2*MAXPOWER ) {
OnceL9a:						mt -= 2*MAXPOWER;
								if ( CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) {
									if ( AN.oldtype == SYMTONUM ) {
										if ( AN.oldvalue >= 0 ) {
											if ( nt > AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt < 0 ) nt = 0;
											}
										}
										else {
											if ( nt < AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt > 0 ) nt = 0;
											}
										}
										AddWild(BHEAD mt,SYMTONUM,nt);
										goto OnceL9;
									}
								}
								else {
									AddWild(BHEAD mt,SYMTONUM,nt);
									goto OnceL9;
								}
							}
							else if ( mt <= -2*MAXPOWER ) {
								mt = -mt;
								nt = -nt;
								goto OnceL9a;
							}
						}
						if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,p[1],&newval1) &&
							 !CheckWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,*p,&newval2) ) {
							nt = p[2];
							mt = m[2];
							if ( ( mt > 0 && nt >= mt ) ||
								 ( mt < 0 && nt <= mt ) ) {
OnceL10:						AddWild(BHEAD *m-WILDOFFSET,VECTOVEC,newval1);
								AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval2);
								break;
							}
							if ( mt >= 2*MAXPOWER ) {
OnceL10a:						mt -= 2*MAXPOWER;
								if ( CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) {
									if ( AN.oldtype == SYMTONUM ) {
										if ( AN.oldvalue >= 0 ) {
											if ( nt > AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt < 0 ) nt = 0;
											}
										}
										else {
											if ( nt < AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt > 0 ) nt = 0;
											}
										}
										AddWild(BHEAD mt,SYMTONUM,nt);
										goto OnceL10;
									}
								}
								else {
									AddWild(BHEAD mt,SYMTONUM,nt);
									goto OnceL10;
								}
							}
							else if ( mt <= -2*MAXPOWER ) {
								mt = -mt;
								nt = -nt;
								goto OnceL10a;
							}
						}
NextInDot:
						p += 3; nq -= 3;
					}
					if ( nq <= 0 ) return(0);
					else {
						q = p+3;
						nq -= 3;
						n -= 3;
						while ( --nq >= 0 ) *p++ = *q++;
					}
					m += 3;
				}
				else if ( m[1] >= (AM.OffsetVector+WILDOFFSET) ) {
					while ( *m >= *t && t < xstop ) {
						*p++ = *t++; *p++ = *t++; *p++ = *t++; n += 3;
					}
					nq = n;
					p = older;
					while ( nq > 0 ) {
						if ( *m == *p && !CheckWild(BHEAD m[1]-WILDOFFSET,
						VECTOVEC,p[1],&newval1) ) {
							nt = p[2];
							mt = m[2];
							if ( ( mt > 0 && nt >= mt ) ||
							     ( mt < 0 && nt <= mt ) ) {
								AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
								break;
							}
							else if ( mt >= 2*MAXPOWER ) {
OnceL7a:						mt -= 2*MAXPOWER;
								if ( CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) {
									if ( AN.oldtype == SYMTONUM ) {
										if ( AN.oldvalue >= 0 ) {
											if ( nt > AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt < 0 ) nt = 0;
											}
										}
										else {
											if ( nt < AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt > 0 ) nt = 0;
											}
										}
										AddWild(BHEAD mt,SYMTONUM,nt);
										AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
										break;
									}
								}
								else {
									AddWild(BHEAD mt,SYMTONUM,nt);
									AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
									break;
								}
							}
							else if ( mt <= -2*MAXPOWER ) {
								mt = -mt;
								nt = -nt;
								goto OnceL7a;
							}
						}
						if ( *m == p[1] && !CheckWild(BHEAD m[1]-WILDOFFSET,
						VECTOVEC,*p,&newval1) ) {
							nt = p[2];
							mt = m[2];
							if ( ( mt > 0 && nt >= mt ) ||
							     ( mt < 0 && nt <= mt ) ) {
								AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
								break;
							}
							if ( mt >= 2*MAXPOWER ) {
OnceL8a:						mt -= 2*MAXPOWER;
								if ( CheckWild(BHEAD mt,SYMTONUM,nt,&newval3) ) {
									if ( AN.oldtype == SYMTONUM ) {
										if ( AN.oldvalue >= 0 ) {
											if ( nt > AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt < 0 ) nt = 0;
											}
										}
										else {
											if ( nt < AN.oldvalue ) nt = AN.oldvalue;
											else {
												if ( *AN.MaskPointer == 2 ) return(0);
												if ( nt > 0 ) nt = 0;
											}
										}
										AddWild(BHEAD mt,SYMTONUM,nt);
										AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
										break;
									}
								}
								else {
									AddWild(BHEAD mt,SYMTONUM,nt);
									AddWild(BHEAD m[1]-WILDOFFSET,VECTOVEC,newval1);
									break;
								}
							}
							else if ( mt < -2*MAXPOWER ) {
								mt = -mt;
								nt = -nt;
								goto OnceL8a;
							}
						}
						p += 3; nq -= 3;
					}
					if ( nq <= 0 ) return(0);
					q = p+3;
					nq -= 3;
					n -= 3;
					while ( --nq >= 0 ) *p++ = *q++;
					m += 3;
				}
				else {
					if ( t >= xstop || *m < *t || ( *m == *t && m[1] < t[1] ) ) {
						if ( m[2] >= 2*MAXPOWER ) nt = m[2];
						else if ( m[2] <= -2*MAXPOWER ) nt = -m[2];
						else return(0);
						nt -= 2*MAXPOWER;
						if ( CheckWild(BHEAD nt,SYMTONUM,0,&newval3) ) {
							if ( AN.oldtype != SYMTONUM ) return(0);
							if ( *AN.MaskPointer == 2 ) return(0);
						}
						AddWild(BHEAD nt,SYMTONUM,0);
						m += 3;
					}
					else {
						*p++ = *t++; *p++ = *t++; *p++ = *t++; n += 3;
					}
				}
			} while ( m < ystop );
			t = xstop;
		}
/*
			#] DOTPRODUCTS : 
*/
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Error in pattern(2)");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
EndLoop:;
	} while ( m < mstop ); }
	else {
		return(-1);
	}
	return(1);
}

/*
 		#] FindOnce : 
 		#[ FindMulti :			WORD FindMulti(term,pattern)

	Note that multi cannot deal with wildcards. Those patterns revert
	to many which gives subsequent calls to once.

*/

WORD FindMulti(PHEAD WORD *term, WORD *pattern)
{
	GETBIDENTITY
	WORD *t, *m, *p;
	WORD *tstop, *mstop;
	WORD *xstop, *ystop;
	WORD mt, power, n, nq;
	WORD older[2*NORMSIZE], *q, newval1;
	AN.UsedOtherFind = 0;
	m = pattern;
	mstop = m + *m;
	m++;
	t = term;
	t += *term - 1;
	tstop = t - ABS(*t) + 1;
 	t = term;
	t++;
	while ( t < tstop && *t > DOTPRODUCT ) t += t[1];
	while ( m < mstop && *m > DOTPRODUCT ) m += m[1];
	power = -1;				/* No power yet */
	if ( m < mstop ) { do {
/*
			#[ SYMBOLS :
*/
		if ( *m == SYMBOL ) {
			ystop = m + m[1];
			m += 2;
			if ( t >= tstop ) return(0);
			while ( *t != SYMBOL ) { t += t[1]; if ( t >= tstop ) return(0); }
			xstop = t + t[1];
			t += 2;
			p = older;
			n = 0;
			do {
				if ( *m >= 2*MAXPOWER ) {
					while ( t < xstop ) { *p++ = *t++; *p++ = *t++; n += 2; }
					nq = n;
					p = older;
					while ( nq > 0 ) {
						if ( !CheckWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,*p,&newval1) ) {
							mt = p[1]/m[1];
							if ( mt > 0 ) {
								if ( power < 0 || mt < power ) power = mt;
								AddWild(BHEAD *m-2*MAXPOWER,SYMTOSYM,newval1);
								break;
							}
						}
						nq -= 2;
						p += 2;
					}
					if ( nq <= 0 ) return(0);
					nq -= 2;
					n -= 2;
					q = p + 2;
					while ( --nq >= 0 ) *p++ = *q++;
					m += 2;
				}
				else if ( t >= xstop ) return(0);
				else if ( *m == *t ) {
					if ( ( mt = t[1]/m[1] ) <= 0 ) return(0);
					if ( power < 0 || mt < power ) power = mt;
					m += 2;
					t += 2;
				}
				else if ( *m < *t ) return(0);
				else { *p++ = *t++; *p++ = *t++; n += 2; }
			} while ( m < ystop );
		}
/*
			#] SYMBOLS : 
			#[ DOTPRODUCTS :
*/
		else if ( *m == DOTPRODUCT ) {
			ystop = m + m[1];
			m += 2;
			if ( t >= tstop ) return(0);
			while ( *t != DOTPRODUCT ) { t += t[1]; if ( t >= tstop ) return(0); }
			xstop = t + t[1];
			t += 2;
			do {
				if ( t >= xstop ) return(0);
				if ( *t == *m ) {
					if ( t[1] == m[1] ) {
						if ( ( mt = t[2]/m[2] ) <= 0 ) return(0);
						if ( power < 0 || mt < power ) power = mt;
						m += 3;
					}
					else if ( t[1] > m[1] ) return(0);
				}
				else if ( *t > *m ) return(0);
				t += 3;
			} while ( m < ystop );
			t = xstop;
		}
/*
			#] DOTPRODUCTS : 
*/
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Error in pattern(3)");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	} while ( m < mstop ); }
	if ( power < 0 ) power = 0;
	return(power);
}

/*
 		#] FindMulti : 
 		#[ FindRest :			WORD FindRest(term,pattern)

	This routine scans for anything but dotproducts and symbols.

*/

WORD FindRest(PHEAD WORD *term, WORD *pattern)
{
	GETBIDENTITY
	WORD *t, *m, *tt, wild, regular;
	WORD *tstop, *mstop;
	WORD *xstop, *ystop;
	WORD n, *p, nq;
	WORD older[NORMSIZE], *q, newval1, newval2;
	int i, ntwa;
	AN.UsedOtherFind = 0;
	AN.findTerm = term; AN.findPattern = pattern;
	m = AN.WildValue;
	i = (m[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;
	ntwa = 0;
	while ( i > 0 ) {
		if ( m[0] == ARGTOARG ) ntwa++;
		m += m[1];
		i--;
	}
	t = term;
	t += *term - 1;
	tstop = t - ABS(*t) + 1;
 	t = term;
	t++; p = t;
	while ( t < tstop && *t > DOTPRODUCT ) t += t[1];
	tstop = t;
	t = p;
	m = pattern;
	mstop = m + *m;
	m++;
	p = m;
	while ( m < mstop && *m > DOTPRODUCT ) m += m[1];
	mstop = m;
	m = p;
	if ( m < mstop ) {
	do {
/*
			#[ FUNCTIONS :
*/
		if ( *m >= FUNCTION ) {
			if ( *mstop > 5 && !MatchIsPossible(pattern,term) ) return(0);
			ystop = m;
			n = 0;
			do {
				m += m[1]; n++;
			} while ( m < mstop && *m >= FUNCTION );
			AT.WorkPointer += n;
			while ( t < tstop && *t == SUBEXPRESSION ) t += t[1];
			tt = xstop = t;
			nq = 0;
			while ( t < tstop && ( *t >= FUNCTION || *t == SUBEXPRESSION ) ) {
				if ( *t != SUBEXPRESSION ) {
					nq++;
					if ( functions[*t-FUNCTION].commute ) tt = t + t[1];
				}
				t += t[1];
			}
			if ( nq < n ) return(0);
			AN.terstart = term;
			AN.terstop = t;
			AN.terfirstcomm = tt;
			AN.patstop = m;
			AN.NumTotWildArgs = ntwa;
			if ( !ScanFunctions(BHEAD ystop,xstop,0) ) return(0);
		}
/*
			#] FUNCTIONS : 
			#[ VECTORS :
*/
		else if ( *m == VECTOR ) {
			while ( t < tstop && *t != VECTOR ) t += t[1];
			if ( t >= tstop ) return(0);
			xstop = t + t[1];
			ystop = m + m[1];
			t += 2;
			m += 2;
			n = 0;
			p = older;
			do {
				if ( *m == *t && m[1] == t[1] && t < xstop ) {
					m += 2; t += 2;
				}
				else if ( *m >= (AM.OffsetVector+WILDOFFSET) ) {
					if ( t < xstop ) {
						p = older + n;
						do { *p++ = *t++; n++; } while ( t < xstop );
					}
					p = older;
					nq = n;
					if ( ( m[1] < (AM.OffsetIndex+WILDOFFSET) )
					|| ( m[1] >= (AM.OffsetIndex+2*WILDOFFSET) ) ) {
						while ( nq > 0 ) {
							if ( m[1] == p[1] ) {
								if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*p,&newval1) ) {
RestL11:							AddWild(BHEAD *m-WILDOFFSET,VECTOVEC,newval1);
									break;
								}
							}
							p += 2;
							nq -= 2;
						}
					}
					else {		/* Double wildcard */
						while ( nq > 0 ) {
							if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*p,&newval1) &&
								 !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,p[1],&newval2) ) {
								AddWild(BHEAD m[1]-WILDOFFSET,INDTOIND,newval2);
								goto RestL11;
							}
							p += 2;
							nq -= 2;
						}
					}
					if ( nq > 0 ) {
						nq -= 2; q = p + 2; n -= 2;
						while ( --nq >= 0 ) *p++ = *q++;
					}
					else return(0);
					m += 2;
				}
				else if ( ( *m <= *t )
				&& ( m[1] >= (AM.OffsetIndex + WILDOFFSET) )
				&& ( m[1] <  (AM.OffsetIndex + 2*WILDOFFSET) ) ) {
					if ( *m == *t && t < xstop ) {
						p = older;
						p += n;
						*p++ = *t++;
						*p++ = *t++;
						n += 2;
					}
					p = older;
					nq = n;
					while ( nq > 0 ) {
						if ( *m == *p ) {
							if ( !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,p[1],&newval1) ) {
								AddWild(BHEAD m[1]-WILDOFFSET,INDTOIND,newval1);
								break;
							}
						}
						p += 2;
						nq -= 2;
					}
					if ( nq > 0 ) {
						nq -= 2; q = p + 2; n -= 2;
						while ( --nq >= 0 ) *p++ = *q++;
					}
					else return(0);
					m += 2;
				}
				else {
					if ( t >= xstop ) return(0);
					*p++ = *t++; *p++ = *t++; n += 2;
				}
			} while ( m < ystop );
		}
/*
			#] VECTORS : 
			#[ INDICES :
*/
		else if ( *m == INDEX ) {
/*
			This needs only to say that there is a match, after matching
			a 'wildcard'. This has to be prepared in TestMatch. The C->rhs
			should provide the replacement inside the prototype!
			Next question: id,p=q/2+r/2
*/
			while ( *t != INDEX ) { t += t[1]; if ( t >= tstop ) return(0); }
			xstop = t + t[1];
			ystop = m + m[1];
			t += 2;
			m += 2;
			n = 0;
			p = older;
			do {
				if ( *m == *t && t < xstop && m < ystop ) {
					t++; m++;
				}
				else if ( ( *m >= (AM.OffsetIndex+WILDOFFSET) )
				&& ( *m < (AM.OffsetIndex+2*WILDOFFSET) ) ) {
					while ( t < xstop ) {
						*p++ = *t++; n++;
					}
					if ( !n ) return(0);
					nq = n;
					q = older;
					do {
						if ( !CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,*q,&newval1) ) {
							AddWild(BHEAD *m-WILDOFFSET,INDTOIND,newval1);
							break;
						}
						q++;
						nq--;
					} while ( nq > 0 );
					if ( nq <= 0 ) return (0);
					n--;
					nq--;
					p = q + 1;
					while ( nq > 0 ) { *q++ = *p++; nq--; }
					p--;
					m++;
				}
				else if ( ( *m >= (AM.OffsetVector+WILDOFFSET) )
				&& ( *m < (AM.OffsetVector+2*WILDOFFSET) ) ) {
					while ( t < xstop ) {
						*p++ = *t++; n++;
					}
					if ( !n ) return(0);
					nq = n;
					q = older;
					do {
						if ( !CheckWild(BHEAD *m-WILDOFFSET,VECTOVEC,*q,&newval1) ) {
							AddWild(BHEAD *m-WILDOFFSET,VECTOVEC,newval1);
							break;
						}
						q++;
						nq--;
					} while ( nq > 0 );
					if ( nq <= 0 ) return (0);
					n--;
					nq--;
					p = q + 1;
					while ( nq > 0 ) { *q++ = *p++; nq--; }
					p--;
					m++;
				}
				else {
					if ( t >= xstop ) return(0);
					*p++ = *t++; n++;
				}
			} while ( m < ystop );

/*
			return(0);
*/
		}
/*
			#] INDICES : 
			#[ DELTAS :
*/
		else if ( *m == DELTA ) {
			while ( *t != DELTA ) { t += t[1]; if ( t >= tstop ) return(0); }
			xstop = t + t[1];
			ystop = m + m[1];
			t += 2;
			m += 2;
			n = 0;
			p = older;
			do {
				if ( *t == *m && t[1] == m[1] && t < xstop ) {
					m += 2;
					t += 2;
				}
				else if ( ( *m >= (AM.OffsetIndex+WILDOFFSET) )
				&& ( *m < (AM.OffsetIndex+2*WILDOFFSET) )
				&& ( m[1] >= (AM.OffsetIndex+WILDOFFSET) )
				&& ( m[1] < (AM.OffsetIndex+2*WILDOFFSET) ) ) { /* Two dummies */
					while ( t < xstop ) {
						*p++ = *t++; *p++ = *t++; n += 2;
					}
					if ( !n ) return(0);
					nq = n;
					q = older;
					do {
						if ( !CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,*q,&newval1) &&
						     !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,q[1],&newval2) ) {
							AddWild(BHEAD *m-WILDOFFSET,INDTOIND,newval1);
							AddWild(BHEAD m[1]-WILDOFFSET,INDTOIND,newval2);
							break;
						}
						if ( !CheckWild(BHEAD *m-WILDOFFSET,INDTOIND,q[1],&newval1) &&
						     !CheckWild(BHEAD m[1]-WILDOFFSET,INDTOIND,*q,&newval2) ) {
							AddWild(BHEAD *m-WILDOFFSET,INDTOIND,newval1);
							AddWild(BHEAD m[1]-WILDOFFSET,INDTOIND,newval2);
							break;
						}
						q += 2;
						nq -= 2;
					} while ( nq > 0 );
					if ( nq <= 0 ) return(0);
					n -= 2;
					nq -= 2;
					p = q + 2;
					while ( nq > 0 ) { *q++ = *p++; nq--; }
					p -= 2;
					m += 2;
				}
				else if ( ( m[1] >= (AM.OffsetIndex+WILDOFFSET) )
				&& ( m[1] < (AM.OffsetIndex+2*WILDOFFSET) ) ) {
					wild = m[1]; regular = *m;
OneWild:
					while ( ( regular == *t || regular == t[1] ) && t < xstop ) {
						*p++ = *t++; *p++ = *t++; n += 2;
					}
					if ( !n ) return(0);
					nq = n;
					q = older;
					do {
						if ( regular == *q && !CheckWild(BHEAD wild-WILDOFFSET,INDTOIND,q[1],&newval1) ) {
							AddWild(BHEAD wild-WILDOFFSET,INDTOIND,newval1);
							break;
						}
						if ( regular == q[1] && !CheckWild(BHEAD wild-WILDOFFSET,INDTOIND,*q,&newval1) ) {
							AddWild(BHEAD wild-WILDOFFSET,INDTOIND,newval1);
							break;
						}
						q += 2;
						nq -= 2;
					} while ( nq > 0 );
					if ( nq <= 0 ) return(0);
					n -= 2;
					nq -= 2;
					p = q + 2;
					while ( nq > 0 ) { *q++ = *p++; nq--; }
					p -= 2;
					m += 2;
				}
				else if ( ( *m >= (AM.OffsetIndex+WILDOFFSET) )
				&& ( *m < (AM.OffsetIndex+2*WILDOFFSET) ) ) {
					wild = *m; regular = m[1];
					goto OneWild;
				}
				else {
					if ( t >= tstop || *m < *t || ( *m == *t && m[1] < t[1] ) )
						return(0);
					*p++ = *t++; *p++ = *t++; n += 2;
				}
			} while ( m < ystop );
		}
/*
			#] DELTAS : 
*/
		else {
			MLOCK(ErrorMessageLock);
			MesPrint("Pattern not yet implemented");
			MUNLOCK(ErrorMessageLock);
			Terminate(-1);
		}
	} while ( m < mstop );
	return(1);
	}
	else return(-1);
}

/*
 		#] FindRest : 
	#] Patterns :
*/
