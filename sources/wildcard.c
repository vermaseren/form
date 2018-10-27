/** @file wildcard.c
 * 
 *  Contains the functions that deal with the wildcards.
 *	During the pattern matching there are two steps:
 *	1: check that a wildcard substitution is correct (if there was already
 *	   an assignment for this variable, it is the same; it is part of the
 *	   proper set; it is the proper type of variables, etc.)
 *	2: make the assignment
 *	In addition we have to be able to clear assignments.
 *	During execution we have to make the actual replacements (WildFill)
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
  	#[ Includes : wildcard.c
*/

#include "form3.h"

#define DEBUG(x)

/*
#define DEBUG(x) x

  	#] Includes : 
 	#[ Wildcards :
 		#[ WildFill :			WORD WildFill(to,from,sub)

		Takes the term in from and puts it into to while
		making wildcard substitutions.
		The return value is the number of words put in to.
		The length as the first word of from is not copied.

		There are two possible algorithms:
		1:	For each element in `from': scan sub.
		2:	For each wildcard in sub replace elements in term.
		The original algorithm used 1:

*/

WORD WildFill(PHEAD WORD *to, WORD *from, WORD *sub)
{
	GETBIDENTITY
	WORD i, j, *s, *t, *m, len, dflag, odirt, adirt;
	WORD *r, *u, *v, *w, *z, *zst, *zz, *subs, *accu, na, dirty = 0, *tstop;
	WORD *temp = 0, *uu, *oldcpointer, sgn;
	WORD subcount, setflag, *setlist = 0, si;
	accu = oldcpointer = AR.CompressPointer;
	t = sub;
	t += sub[1];
	s = sub + SUBEXPSIZE;
	i = 0;
	while ( s < t && *s != FROMBRAC ) {
		i++; s += s[1];
	}
	if ( !i ) {			/* No wildcards -> done quickly */
		j = i = *from;
		NCOPY(to,from,i);
		if ( dirty ) AN.WildDirt = dirty;
		return(j);
	}
	sgn = 0;
	subs = sub + SUBEXPSIZE;
	t = from;
	GETSTOP(t,r);
	t++;
	m = to + 1;
	if ( t < r ) do {
		uu = u = t + t[1];
		setflag = 0;
ReSwitch:
		switch ( *t ) {
			case SYMBOL:
/*
			#[ SYMBOLS :
*/
				z = accu;
				*m++ = *t++;
				*m++ = *t++;
				v = m;
				while ( t < u ) {
					*m = *t;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) goto sspow;
					}
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( *t == s[2] ) {
							if ( *s == SYMTOSYM ) {
								*m = s[3]; dirty = 1;
								break;
							}
							else if ( *s == SYMTONUM ) {
								dirty = 1;
								zst = z;
								*z++ = SNUMBER;
								*z++ = 4;
								*z++ = s[3];
								w = z;
								*z++ = *++t;
								if ( ABS(*t) >= 2*MAXPOWER) {
DoPow:							s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( ( *s == SYMTONUM ) &&
									( ABS(*t) - 2*MAXPOWER ) == s[2] ) {
										dirty = 1;
										*w = s[3];
										if ( *t < 0 ) *w = -*w;
										break;
									}
									if ( ( *s == SYMTOSYM ) &&
									( ABS(*t) - 2*MAXPOWER ) == s[2] ) {
										dirty = 1;
										zz = z;
										while ( --zz >= zst ) {
											zz[1+FUNHEAD+ARGHEAD] = *zz;
										}
										w += 1+FUNHEAD+ARGHEAD;
										*zst = EXPONENT;
										zst[2] = DIRTYFLAG;
										zst[FUNHEAD+ARGHEAD] = WORDDIF(z,zst)+4;
										zst[1+FUNHEAD] = 1;
										zst[FUNHEAD] = WORDDIF(z,zst)+4+ARGHEAD;
										z += FUNHEAD+ARGHEAD+1;
										*w = 1;	/* exponent -> 1 */
										*z++ = 1;
										*z++ = 1;
										*z++ = 3;
										if ( *t > 0 ) {
											*z++ = -SYMBOL;
											*z++ = s[3];
										}
										else {
											*z++ = ARGHEAD+8;
											*z++ = 1;
											*z++ = 8;
											*z++ = SYMBOL;
											*z++ = 4;
											*z++ = s[3];
											*z++ = 1;
											*z++ = 1;
											*z++ = 1;
											*z++ = -3;
										}
										zst[1] = WORDDIF(z,zst);
										break;
									}
									if ( *s == SYMTOSUB &&
									( ABS(*t) - 2*MAXPOWER ) == s[2] ) {
MakeExp:								dirty = 1;
										zz = z;
										while ( --zz >= zst ) {
											zz[1+FUNHEAD+ARGHEAD] = *zz;
										}
										w += 1+FUNHEAD+ARGHEAD;
										*zst = EXPONENT;
										zst[2] = DIRTYFLAG;
										zst[FUNHEAD+ARGHEAD] = WORDDIF(z,zst)+4;
										zst[1+FUNHEAD] = 1;
										zst[FUNHEAD] = WORDDIF(z,zst)+4+ARGHEAD;
										z += FUNHEAD+ARGHEAD+1;
										*w = 1;	/* exponent -> 1 */
										*z++ = 1;
										*z++ = 1;
										*z++ = 3;
										*z++ = 4+SUBEXPSIZE+ARGHEAD;
										*z++ = 1;
										*z++ = 4+SUBEXPSIZE;
										*z++ = SUBEXPRESSION;
										*z++ = SUBEXPSIZE;
										*z++ = s[3];
										*z++ = 1;
										*z++ = AT.ebufnum;
										FILLSUB(z)
										*z++ = 1;
										*z++ = 1;
										*z++ = *t > 0 ? 3: -3;
										zst[1] = WORDDIF(z,zst);
										break;
									}
									s += s[1];
								}
								}
								if ( !*w ) z = w - 3;
								t++;
								goto Seven;
							}
							else if ( *s == SYMTOSUB ) {
								dirty = 1;
								zst = z;
								*z++ = SUBEXPRESSION;
								*z++ = SUBEXPSIZE;
								*z++ = s[3];
								w = z;
								*z++ = *++t;
								*z++ = AT.ebufnum;
								FILLSUB(z)
								goto DoPow;
							}
						}
						s += s[1];
					}
sspow:
					s = subs;
					*++m = *++t;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) {
							t++; m++;
							goto Seven;
						}
					}
					for ( j = 0; j < i; j++ ) {
						if ( ( ABS(*t) - 2*MAXPOWER ) == s[2] ) {
							if ( *s == SYMTONUM ) {
								dirty = 1;
								*m = s[3];
								if ( *t < 0 ) *m = -*m;
								break;
							}
							else if ( *s == SYMTOSYM ) {
								dirty = 1;
								*z++ = EXPONENT;
								if ( *t < 0 ) *z++ = FUNHEAD+ARGHEAD+10;
								else *z++ = 4+FUNHEAD;
								*z++ = 0;
								FILLFUN3(z)
								*z++ = -SYMBOL;
								*z++ = m[-1];
								if ( *t < 0 ) {
									*z++ = ARGHEAD+8;
									*z++ = 0;
									*z++ = 8;
									*z++ = SYMBOL;
									*z++ = 4;
									*z++ = s[3];
									*z++ = 1;
									*z++ = 1;
									*z++ = 1;
									*z = -3;
								}
								else {
									*z++ = -SYMBOL;
									*z++ = s[3];
								}
								m -= 2;
								break;
							}
							else if ( *s == SYMTOSUB ) {
								zst = z;
								*z++ = SYMBOL;
								*z++ = 4;
								*z++ = *--m;
								w = z;
								*z++ = *t;
								goto MakeExp;
							}
						}
						s += s[1];
					}
					t++;
					if ( *m ) m++;
					else m--;
Seven:;
				}
				j = WORDDIF(m,v);
				if ( !j ) m -= 2;
				else v[-1] = j + 2;
				s = accu;
				while ( s < z ) *m++ = *s++;
				break;
/*
			#] SYMBOLS : 
*/
			case DOTPRODUCT:
/*
			#[ DOTPRODUCTS :
*/
				*m++ = *t++;
				*m++ = *t++;
				v = m;
				z = accu;
				while ( t < u ) {
					*m = *t;
					subcount = 0;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) goto ss2;
					}
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( *t == s[2] ) {
							if ( *s == VECTOVEC ) {
								*m = s[3]; dirty = 1; break;
							}
							if ( *s == VECTOMIN ) {
								*m = s[3]; dirty = 1; sgn += t[2]; break;
							}
							if ( *s == VECTOSUB ) {
								*m = s[3]; dirty = 1; subcount = 1; break;
							}
						}
						s += s[1];
					}
ss2:
					*++m = *++t;
					s = subs;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) goto ss3;
					}
					for ( j = 0; j < i; j++ ) {
						if ( *t == s[2] ) {
							if ( *s == VECTOVEC ) {
								*m = s[3]; dirty = 1; break;
							}
							if ( *s == VECTOMIN ) {
								*m = s[3]; dirty = 1; sgn += t[1]; break;
							}
							if ( *s == VECTOSUB ) {
								*m = s[3]; dirty = 1; subcount += 2; break;
							}
						}
						s += s[1];
					}
ss3:				*++m = *++t;
					if ( ( ABS(*t) - 2*MAXPOWER ) < 0 ) goto RegPow;
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( ( ABS(*t) - 2*MAXPOWER ) == s[2] ) {
							if ( *s == SYMTONUM ) {
								*m = s[3];
								if ( *t < 0 ) *m = -*m;
								dirty = 1;
								break;
							}
							if ( *s <= SYMTOSUB ) {
/*
				Here we put together a power function with the proper
				arguments. Note that a p?.q? resolves to a single power.
*/
								m -= 2;
								*z++ = EXPONENT;
								w = z;
								if ( subcount == 0 ) {
									*z++ = 17+FUNHEAD+2*ARGHEAD;
									*z++ = DIRTYFLAG;
									FILLFUN3(z)
									*z++ = 9+ARGHEAD;
									*z++ = 0;
									FILLARG(z)
									*z++ = 9;
									*z++ = DOTPRODUCT;
									*z++ = 5;
									*z++ = *m;
									*z++ = m[1];
									*z++ = 1;
									*z++ = 1;
									*z++ = 1;
									*z++ = 3;
									if ( *s == SYMTOSYM ) {
										*z++ = 8+ARGHEAD;
										*z++ = 0;
										FILLARG(z)
										*z++ = 8;
										*z++ = SYMBOL;
										*z++ = 4;
										*z++ = s[3];
										*z++ = 1;
									}
									else {
										*z++ = 4+SUBEXPSIZE+ARGHEAD;
										*z++ = 1;
										FILLARG(z)
										*z++ = 4+SUBEXPSIZE;
										*z++ = SUBEXPRESSION;
										*z++ = SUBEXPSIZE;
										*z++ = s[3];
										*z++ = 1;
										*z++ = AT.ebufnum;
										FILLSUB(z)
									}
									*z++ = 1; *z++ = 1;
									*z++ = ( s[2] > 0 ) ? 3: -3;
								}
								else if ( subcount == 3 ) {
									*z++ = 20+2*SUBEXPSIZE+FUNHEAD+2*ARGHEAD;
									*z++ = DIRTYFLAG;
									FILLFUN3(z)
									*z++ = 12+2*SUBEXPSIZE+ARGHEAD;
									*z++ = 1;
									*z++ = 12+2*SUBEXPSIZE;
									*z++ = SUBEXPRESSION;
									*z++ = 4+SUBEXPSIZE;
									*z++ = *m + 1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = ++AR.CurDum;

									*z++ = SUBEXPRESSION;
									*z++ = 4+SUBEXPSIZE;
									*z++ = m[1] + 1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = AR.CurDum;
									*z++ = 1; *z++ = 1; *z++ = 3;
								}
								else {
									if ( subcount == 2 ) {
										j = *m; *m = m[1]; m[1] = j;
									}
									*z++ = 16+SUBEXPSIZE+FUNHEAD+2*ARGHEAD;
									*z++ = DIRTYFLAG;
									FILLFUN3(z)
									*z++ = 8+SUBEXPSIZE+ARGHEAD;
									*z++ = 1;
									*z++ = 8+SUBEXPSIZE;
									*z++ = SUBEXPRESSION;
									*z++ = 4+SUBEXPSIZE;
									*z++ = *m + 1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = m[1];
									*z++ = 1; *z++ = 1; *z++ = 3;
								}
								if ( *s == SYMTOSYM ) {
									if ( s[2] > 0 ) {
										*z++ = -SYMBOL;
										*z++ = s[3];
										t++;
										*w = z-w+1;
										goto NextDot;
									}
									*z++ = 8+ARGHEAD;
									*z++ = 0;
									*z++ = 8;
									*z++ = SYMBOL;
									*z++ = 4;
									*z++ = s[3];
									*z++ = 1;
								}
								else {
									*z++ = 4+SUBEXPSIZE+ARGHEAD;
									*z++ = 1;
									*z++ = 4+SUBEXPSIZE;
									*z++ = SUBEXPRESSION;
									*z++ = SUBEXPSIZE;
									*z++ = s[3];
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
								}
								*z++ = 1; *z++ = 1;
								*z++ = ( s[2] > 0 ) ? 3: -3;
								t++;
								*w = z-w+1;
								goto NextDot;
							}
						}
						s += s[1];
					}
RegPow:				if ( *m ) m++;
					else { m -= 2; subcount = 0; }
					t++;
					if ( subcount ) {
						m -= 3;
						if ( subcount == 3 ) {
							if ( m[2] < 0 ) {
								j = (-m[2]) * (2*SUBEXPSIZE+8);
								*z++ = DENOMINATOR;
								*z++ = j + 8 + FUNHEAD + ARGHEAD;
								*z++ = DIRTYFLAG;
								FILLFUN3(z)
								*z++ = j + 8 + ARGHEAD;
								*z++ = 1;
								*z++ = j + 8;
								while ( m[2] < 0 ) {
									(m[2])++;
									*z++ = SUBEXPRESSION;
									*z++ = 4+SUBEXPSIZE;
									*z++ = *m + 1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = ++AR.CurDum;
									*z++ = SUBEXPRESSION;
									*z++ = 8+SUBEXPSIZE;
									*z++ = m[1] + 1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = AR.CurDum;
									*z++ = SYMTOSYM;	/* Needed to avoid */
									*z++ = 4;			/* problems with   */
									*z++ = 1000;		/* conversion to   */
									*z++ = 1000;		/* square of subexp*/
								}
								*z++ = 1; *z++ = 1; *z++ = 3;
							}
							else {
								while ( m[2] > 0 ) {
									(m[2])--;
									*z++ = SUBEXPRESSION;
									*z++ = 4+SUBEXPSIZE;
									*z++ = *m + 1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = ++AR.CurDum;
									*z++ = SUBEXPRESSION;
									*z++ = 4+SUBEXPSIZE;
									*z++ = m[1] + 1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = AR.CurDum;
								}
							}
						}
						else {
							if ( subcount == 2 ) {
								j = *m; *m = m[1]; m[1] = j;
							}
							if ( m[2] < 0 ) {
								*z++ = DENOMINATOR;
								*z++ = 8+SUBEXPSIZE+FUNHEAD+ARGHEAD;
								*z++ = DIRTYFLAG;
								FILLFUN3(z)
								*z++ = 8+SUBEXPSIZE+ARGHEAD;
								*z++ = 1;
								*z++ = 8+SUBEXPSIZE;
							}
							*z++ = SUBEXPRESSION;
							*z++ = 4+SUBEXPSIZE;
							*z++ = *m + 1;
							*z++ = ABS(m[2]);
							*z++ = AT.ebufnum;
							FILLSUB(z)
							*z++ = INDTOIND;
							*z++ = 4;
							*z++ = FUNNYVEC;
							*z++ = m[1];
							if ( m[2] < 0 ) {
								*z++ = 1; *z++ = 1; *z++ = 3;
							}
						}
					}
NextDot:;
				}
				if ( m <= v ) m = v - 2;
				else v[-1] = WORDDIF(m,v) + 2;
				if ( z > accu ) {
					j = WORDDIF(z,accu);
					z = accu;
					NCOPY(m,z,j);
				}
				break;
/*
			#] DOTPRODUCTS : 
*/
			case SETSET:
/*
			#[ SETS :
*/
				temp = accu + (((AR.ComprTop - accu)>>1)&(-2));
				if ( ResolveSet(BHEAD t,temp,sub) ) {
					Terminate(-1);
				}
				setlist = t + 2 + t[3];
				setflag = t[1] - 2 - t[3];	/* Number of elements * 2 */
				t = temp; u = t + t[1];
				goto ReSwitch;
/*
			#] SETS : 
*/
			case VECTOR:
/*
			#[ VECTORS :
*/
				*m++ = *t++;
				*m++ = *t++;
				v = m;
				z = accu;
				while ( t < u ) {
					*m = *t;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) goto ss4;
					}
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( *t == s[2] ) {
							if ( *s == INDTOIND || *s == VECTOVEC ) {
								*m = s[3]; dirty = 1; break;
							}
							if ( *s == VECTOMIN ) {
								*m = s[3]; dirty = 1; sgn++; break;
							}
							else if ( *s == VECTOSUB ) {
								*z++ = SUBEXPRESSION;
								*z++ = 4+SUBEXPSIZE;
								*z++ = s[3]+1;
								*z++ = 1;
								*z++ = AT.ebufnum;
								FILLSUB(z)
								*z++ = VECTOVEC;
								*z++ = 4;
								*z++ = FUNNYVEC;
								*z++ = *++t;
								m--;
								s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( z[-1] == s[2] ) {
										if ( *s == INDTOIND || *s == VECTOVEC ) {
											z[-1] = s[3];
											break;
										}
										if ( *s == INDTOSUB || *s == VECTOSUB ) {
											z[-1] = ++AR.CurDum;
											*z++ = SUBEXPRESSION;
											*z++ = 4+SUBEXPSIZE;
											*z++ = s[3]+1;
											*z++ = 1;
											*z++ = AT.ebufnum;
											FILLSUB(z)
											if ( *s == INDTOSUB ) *z++ = INDTOIND;
											else *z++ = VECTOSUB;
											*z++ = 4;
											*z++ = FUNNYVEC;
											*z++ = AR.CurDum;
											break;
										}
									}
									s += s[1];
								}
								dirty = 1;
								break;
							}
							else if ( *s == INDTOSUB ) {
								*z++ = SUBEXPRESSION;
								*z++ = 4+SUBEXPSIZE;
								*z++ = s[3]+1;
								*z++ = 1;
								*z++ = AT.ebufnum;
								FILLSUB(z)
								*z++ = INDTOIND;
								*z++ = 4;
								*z++ = FUNNYVEC;
								m -= 2;
								*z++ = m[1];
								dirty = 1;
								t++;
								break;
							}
						}
						s += s[1];
					}
ss4:				m++; t++;
				}
				if ( m <= v ) m = v-2;
				else v[-1] = WORDDIF(m,v)+2;
				if ( z > accu ) {
					j = WORDDIF(z,accu); z = accu;
					NCOPY(m,z,j);
				}
				break;
/*
			#] VECTORS : 
*/
			case INDEX:
/*
			#[ INDEX :
*/
				*m++ = *t++;
				*m++ = *t++;
				v = m;
				z = accu;
				while ( t < u ) {
					*m = *t;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) goto ss5;
					}
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( *t == s[2] ) {
							if ( *s == INDTOIND || *s == VECTOVEC )
								{ *m = s[3]; dirty = 1; break; }
							if ( *s == VECTOMIN )
								{ *m = s[3]; dirty = 1; sgn++; break; }
							else if ( *s == VECTOSUB || *s == INDTOSUB ) {
								*z++ = SUBEXPRESSION;
								*z++ = SUBEXPSIZE;
								*z++ = s[3];
								*z++ = 1;
								*z++ = AT.ebufnum;
								FILLSUB(z)
								m--;
								dirty = 1;
								break;
							}
						}
						s += s[1];
					}
ss5:				m++; t++;
				}
				if ( m <= v ) m = v-2;
				else v[-1] = WORDDIF(m,v)+2;
				if ( z > accu ) {
					j = WORDDIF(z,accu); z = accu;
					NCOPY(m,z,j);
				}
				break;
/*
			#] INDEX : 
*/
			case DELTA:
			case LEVICIVITA:
			case GAMMA:
/*
			#[ SPECIALS :
*/
				v = m;
				*m++ = *t++;
				*m++ = *t++;
#if FUNHEAD > 2
				if ( t[-2] != DELTA ) *m++ = *t++;
#endif
Tensors:
				COPYFUN3(m,t)
				z = accu;
				while ( t < u ) {
					*m = *t;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) goto ss6;
					}
					s = subs;
					if ( *m == FUNNYWILD ) {
						CBUF *C = cbuf+AT.ebufnum;
						t++;
						for ( j = 0; j < i; j++ ) {
							if ( *s == ARGTOARG && *t == s[2] ) {
								v[2] |= DIRTYFLAG;
								if ( s[3] < 0 ) { /* empty */
									t++; break;
								}
								w = C->rhs[s[3]];
DEBUG(MesPrint("Thread %w(a): s[3] = %d, w=(%d,%d,%d,%d)",s[3],w[0],w[1],w[2],w[3]);)
								j = *w++;
								if ( j > 0 ) {
									NCOPY(m,w,j);
								}
								else {
									while ( *w ) {
										if ( *w == -INDEX || *w == -VECTOR
										|| *w == -MINVECTOR
										|| ( *w == -SNUMBER && w[1] >= 0
										&& w[1] < AM.OffsetIndex ) ) {
											if ( *w == -MINVECTOR ) sgn++;
											w++;
											*m++ = *w++;
										}
										else {
											MLOCK(ErrorMessageLock);
DEBUG(MesPrint("Thread %w(aa): *w = %d",*w);)
											MesPrint("Illegal substitution of argument field in tensor");
											MUNLOCK(ErrorMessageLock);
											SETERROR(-1)
										}
									}
								}
								t++;
								break;
							}
							s += s[1];
						}
					}
					else {
						for ( j = 0; j < i; j++ ) {
							if ( *t == s[2] ) {
								if ( *s == INDTOIND || *s == VECTOVEC )
									{ *m = s[3]; dirty = 1; break; }
								if ( *s == VECTOMIN )
									{ *m = s[3]; dirty = 1; sgn++; break; }
								else if ( *s == VECTOSUB || *s == INDTOSUB ) {
									*m = ++AR.CurDum;
									*z++ = SUBEXPRESSION;
									*z++ = 4+SUBEXPSIZE;
									*z++ = s[3]+1;
									*z++ = 1;
									*z++ = AT.ebufnum;
									FILLSUB(z)
									*z++ = INDTOIND;
									*z++ = 4;
									*z++ = FUNNYVEC;
									*z++ = AR.CurDum;
									dirty = 1;
									break;
								}
							}
							s += s[1];
						}
						if ( j < i && *v != DELTA ) v[2] |= DIRTYFLAG;
ss6:					m++; t++;
					}
				}
				v[1] = WORDDIF(m,v);
				if ( z > accu ) {
					j = WORDDIF(z,accu); z = accu;
					NCOPY(m,z,j);
				}
				break;
/*
			#] SPECIALS : 
*/
			case SUBEXPRESSION:
/*
			#[ SUBEXPRESSION :
*/
				dirty = 1;
				tstop = t + t[1];
				*m++ = *t++;
				*m++ = *t++;
				*m++ = *t++;
				*m++ = *t++;
				if ( t[-1] >= 2*MAXPOWER || t[-1] <= -2*MAXPOWER ) {
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( *s == SYMTONUM &&
								( ABS(t[-1]) - 2*MAXPOWER ) == s[2] ) {
							m[-1] = s[3];
							if ( t[-1] < 0 ) m[-1] = -m[-1];
							break;
						}
						s += s[1];
					}
				}
				*m++ = *t++;
				COPYSUB(m,t)
				while ( t < tstop ) {
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] - 2 ) goto ss7;
					}
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( s[2] == t[2] ) {
							if ( ( *s <= SYMTOSUB && *t <= SYMTOSUB )
							|| ( *s == *t && *s < FROMBRAC )
							|| ( *s == VECTOVEC && ( *t == VECTOSUB || *t == VECTOMIN ) )
							|| ( *s == VECTOSUB && ( *t == VECTOVEC || *t == VECTOMIN ) )
							|| ( *s == VECTOMIN && ( *t == VECTOSUB || *t == VECTOVEC ) )
							|| ( *s == INDTOIND && *t == INDTOSUB )
							|| ( *s == INDTOSUB && *t == INDTOIND ) ) {
								WORD *vv = m;
/*								*t = *s;  Wrong!!! Overwrites compiler buffer  */
                                j = t[1];
								NCOPY(m,t,j);
								vv[0] = s[0];
								vv[3] = s[3];
								goto sr7;
							}
						}
						s += s[1];
					}
ss7:				j = t[1];
					NCOPY(m,t,j);
sr7:;
				}
				break;
/*
			#] SUBEXPRESSION : 
*/
			case EXPRESSION:
/*
			#[ EXPRESSION :
*/
				dirty = 1;
				tstop = t + t[1];
				v = m;
				*m++ = *t++;
				*m++ = *t++;
				*m++ = *t++;
				*m++ = *t++;
				s = subs;
				for ( j = 0; j < i; j++ ) {
					if ( ( ABS(t[-1]) - 2*MAXPOWER ) == s[2] ) {
						if ( *s == SYMTONUM ) {
							m[-1] = s[3];
							if ( t[-1] < 0 ) m[-1] = -m[-1];
							break;
						}
						else if ( *s <= SYMTOSUB ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Wildcard power of expression should be a number");
							MUNLOCK(ErrorMessageLock);
							SETERROR(-1)
						}
					}
					s += s[1];
				}
				*m++ = *t++;
				COPYSUB(m,t)
				while ( t < tstop && *t != WILDCARDS ) {
					j = t[1];
					NCOPY(m,t,j);
				}
				if ( t < tstop && *t == WILDCARDS ) {
					*m++ = *t;
					s = sub;
					j = s[1];
					*m++ = j+2;
					NCOPY(m,s,j);
					t += t[1];
				}
				if ( t < tstop && *t == FROMBRAC ) {
					w = m;
					*m++ = *t;
					*m++ = t[1];
					if ( WildFill(BHEAD m,t+2,sub) < 0 ) {
						MLOCK(ErrorMessageLock);
						MesCall("WildFill");
						MUNLOCK(ErrorMessageLock);
						SETERROR(-1)
					}
					m += *m;
					w[1] = m - w;
					t += t[1];
				}
				while ( t < tstop ) {
					j = t[1];
					NCOPY(m,t,j);
				}
				v[1] = m-v;
				break;
/*
			#] EXPRESSION : 
*/
			default:
/*
			#[ FUNCTIONS :
*/
				if ( *t >= FUNCTION ) {
					dflag = 0;
					na = 0;
					*m = *t;
					for ( si = 0; si < setflag; si += 2 ) {
						if ( t == temp + setlist[si] ) {
							dflag = DIRTYFLAG;	goto ss8;
						}
					}
					s = subs;
					for ( j = 0; j < i; j++ ) {
						if ( *s == FUNTOFUN && *t == s[2] )
							{ *m = s[3]; dirty = 1; dflag = DIRTYFLAG; break; }
						s += s[1];
					}
ss8:				v = m;
					if ( *t >= FUNCTION && functions[*t-FUNCTION].spec
					>= TENSORFUNCTION ) {
						if ( *m < FUNCTION || functions[*m-FUNCTION].spec
						< TENSORFUNCTION ) {
							MLOCK(ErrorMessageLock);
							MesPrint("Illegal wildcarding of regular function to tensorfunction");
							MUNLOCK(ErrorMessageLock);
							SETERROR(-1)
						}
						m++; t++;
						*m++ = *t++;
						*m++ = *t++ | dflag;
						goto Tensors;
					}
					m++; t++;
					*m++ = *t++;
					*m++ = *t++ | dflag;
					COPYFUN3(m,t)
					z = accu;
					while ( t < u ) {		/* do an argument */
						if ( *t < 0 ) {
/*
			#[ Simple arguments :
*/
							CBUF *C = cbuf+AT.ebufnum;
							for ( si = 0; si < setflag; si += 2 ) {
								if ( *t <= -FUNCTION ) {
									if ( t == temp + setlist[si] ) {
										v[2] |= DIRTYFLAG; goto ss10; }
								}
								else {
									if ( t == temp + setlist[si]-1 ) {
										v[2] |= DIRTYFLAG; goto ss9; }
								}
							}
							if ( *t == -ARGWILD ) {
								s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( *s == ARGTOARG && s[2] == t[1] ) break;
									s += s[1];
								}
								v[2] |= DIRTYFLAG;
								w = C->rhs[s[3]];
DEBUG(MesPrint("Thread %w(b): s[3] = %d, w=(%d,%d,%d,%d)",s[3],w[0],w[1],w[2],w[3]);)
								if ( *w == 0 ) {
									w++;
									while ( *w ) {
										if ( *w > 0 ) j = *w;
										else if ( *w <= -FUNCTION ) j = 1;
										else j = 2;
										NCOPY(m,w,j);
									}
								}
								else {
									j = *w++;
									while ( --j >= 0 ) {
										if ( *w < MINSPEC ) *m++ = -VECTOR;
										else if ( *w >= 0 && *w < AM.OffsetIndex )
											*m++ = -SNUMBER;
										else *m++ = -INDEX;
										*m++ = *w++;
									}
								}
								t += 2;
								dirty = 1;
								if ( ( *v == NUMARGSFUN || *v == NUMTERMSFUN )
								&& t >= u && m == v + FUNHEAD ) {
									m = v;
									*m++ = SNUMBER; *m++ = 3; *m++ = 0;
									break;
								}
							}
							else if ( *t <= -FUNCTION ) {
								*m = *t;
								s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( -*t == s[2] ) {
										if ( *s == FUNTOFUN )
											{ *m = -s[3]; dirty = 1; v[2] |= DIRTYFLAG; break; }
									}
									s += s[1];
								}
								m++; t++;
							}
							else if ( *t == -SYMBOL ) {
								*m++ = *t++;
								*m = *t;
								s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( *t == s[2] && *s <= SYMTOSUB ) {
										dirty = 1; v[2] |= DIRTYFLAG;
										if ( AR.PolyFunType == 2 && v[0] == AR.PolyFun )
											v[2] |= MUSTCLEANPRF;
										if ( *s == SYMTOSYM ) *m = s[3];
										else if ( *s == SYMTONUM ) {
											m[-1] = -SNUMBER;
											*m = s[3];
										}
										else if ( *s == SYMTOSUB ) {
ToSub:										m--;
											w = C->rhs[s[3]];
DEBUG(MesPrint("Thread %w(c): s[3] = %d, w=(%d,%d,%d,%d)",s[3],w[0],w[1],w[2],w[3]);)
											s = m;
											m += 2;
											while ( *w ) {
												j = *w;
												NCOPY(m,w,j);
											}
											*s = WORDDIF(m,s);
											s[1] = 0;
											*m = 0;
											if ( t[-1] == -MINVECTOR ) {
												w = s+2;
												while ( *w ) {
													w += *w;
													w[-1] = -w[-1];
												}
											}
											if ( ToFast(s,s) ) {
												if ( *s <= -FUNCTION ) m = s;
												else m = s + 1;
											}
											else m--;
										}
										break;
									}
									s += s[1];
								}
								m++; t++;
							}
							else if ( *t == -INDEX ) {
								*m++ = *t++;
								*m = *t;
								s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( *t == s[2] ) {
										if ( *s == INDTOIND || *s == VECTOVEC ) {
											*m = s[3];
											if ( *m < MINSPEC ) m[-1] = -VECTOR;
											else if ( *m >= 0 && *m < AM.OffsetIndex )
												m[-1] = -SNUMBER;
											else m[-1] = -INDEX;
										}
										else if ( *s == VECTOSUB || *s == INDTOSUB ) {
											m[-1] = -INDEX;
											*m = ++AR.CurDum;
											*z++ = SUBEXPRESSION;
											*z++ = 4+SUBEXPSIZE;
											*z++ = s[3]+1;
											*z++ = 1;
											*z++ = AT.ebufnum;
											FILLSUB(z)
											*z++ = INDTOIND;
											*z++ = 4;
											*z++ = FUNNYVEC;
											*z++ = AR.CurDum;
										}
										v[2] |= DIRTYFLAG; dirty = 1;
										break;
									}
									s += s[1];
								}
								m++; t++;
							}
							else if ( *t == -VECTOR || *t == -MINVECTOR ) {
								*m++ = *t++;
								*m = *t;
								s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( *t == s[2] ) {
										if ( *s == VECTOVEC ) *m = s[3];
										else if ( *s == VECTOMIN ) {
											*m = s[3];
											if ( t[-1] == -VECTOR )
												m[-1] = -MINVECTOR;
											else
												m[-1] = -VECTOR;
										}
										else if ( *s == VECTOSUB ) goto ToSub;
										dirty = 1; v[2] |= DIRTYFLAG;
										break;
									}
									s += s[1];
								}
								m++; t++;
							}
							else if ( *t == -SNUMBER ) {
								*m++ = *t++;
								*m = *t;
								s = subs;
								for ( j = 0; j < i; j++ ) {
									if ( *t == s[2] && *s >= NUMTONUM && *s <= NUMTOSUB ) {
										dirty = 1; v[2] |= DIRTYFLAG;
										if ( *s == NUMTONUM ) *m = s[3];
										else if ( *s == NUMTOSYM ) {
											m[-1] = -SYMBOL;
											*m = s[3];
										}
										else if ( *s == NUMTOIND ) {
											m[-1] = -INDEX;
											*m = s[3];
										}
										else if ( *s == NUMTOSUB ) goto ToSub;
										break;
									}
									s += s[1];
								}
								m++; t++;
							}
							else {
ss9:							*m++ = *t++;
ss10:							*m++ = *t++;
							}
							na = WORDDIF(z,accu);
/*
			#] Simple arguments : 
*/
						}
						else {
							w = m;
							zz = t;
							NEXTARG(zz)
							odirt = AN.WildDirt; AN.WildDirt = 0;
							AR.CompressPointer = accu + na;
							for ( j = 0; j < ARGHEAD; j++ ) *m++ = *t++;
							j = 0;
							adirt = 0;
							while ( t < zz ) {	/* do a term */
								if ( ( len = WildFill(BHEAD m,t,sub) ) < 0 ) {
									MLOCK(ErrorMessageLock);
									MesCall("WildFill");
									MUNLOCK(ErrorMessageLock);
									SETERROR(-1)
								}
								if ( AN.WildDirt ) {
									adirt = AN.WildDirt;
									AN.WildDirt = 0;
								}
								m += len;
								t += *t;
							}
							*w = WORDDIF(m,w);	/* Fill parameter length */
							if ( adirt ) {
								dirty = w[1] = 1; v[2] |= DIRTYFLAG;
								if ( AR.PolyFunType == 2 && v[0] == AR.PolyFun )
									v[2] |= MUSTCLEANPRF;
								AN.WildDirt = adirt;
							}
							else {
								AN.WildDirt = odirt;
							}
							if ( ToFast(w,w) ) {
								if ( *w <= -FUNCTION ) {
									if ( *w == NUMARGSFUN || *w == NUMTERMSFUN ) {
										*w = -SNUMBER; w[1] = 0; m = w + 2;
									}
									else m = w+1;
								}
								else m = w+2;
							}
							AR.CompressPointer = oldcpointer;
						}
					}
					v[1] = WORDDIF(m,v);		/* Fill function length */
					s = accu;
					NCOPY(m,s,na);
/*
                    Now some code to speed up a few special cases
*/
                    if ( v[0] == EXPONENT ) {
                        if ( v[1] == FUNHEAD+4 && v[FUNHEAD] == -SYMBOL &&
                        v[FUNHEAD+2] == -SNUMBER && v[FUNHEAD+3] < MAXPOWER
                        && v[FUNHEAD+3] > -MAXPOWER ) {
                            v[0] = SYMBOL;
                            v[1] = 4;
                            v[2] = v[FUNHEAD+1];
                            v[3] = v[FUNHEAD+3];
                            m = v+4;
                        }
						else if ( v[1] == FUNHEAD+ARGHEAD+11
						 && v[FUNHEAD] == ARGHEAD+9
						 && v[FUNHEAD+ARGHEAD] == 9
						 && v[FUNHEAD+ARGHEAD+1] == DOTPRODUCT
						 && v[FUNHEAD+ARGHEAD+8] == 3
						 && v[FUNHEAD+ARGHEAD+7] == 1
						 && v[FUNHEAD+ARGHEAD+6] == 1
						 && v[FUNHEAD+ARGHEAD+5] == 1
                		 && v[FUNHEAD+ARGHEAD+9] == -SNUMBER
						 && v[FUNHEAD+ARGHEAD+10] < MAXPOWER
        		         && v[FUNHEAD+ARGHEAD+10] > -MAXPOWER ) {
							v[0] = DOTPRODUCT;
							v[1] = 5;
							v[2] = v[FUNHEAD+ARGHEAD+3];
							v[3] = v[FUNHEAD+ARGHEAD+4];
							v[4] = v[FUNHEAD+ARGHEAD+10];
							m = v+5;
						}
                    }
				}
				else { while ( t < u ) *m++ = *t++; }
/*
			#] FUNCTIONS : 
*/
		}
		t = uu;
	} while ( t < r );
	t = from;			/* Copy coefficient */
	t += *t;
	if ( r < t ) do { *m++ = *r++; } while ( r < t );
	if ( ( sgn & 1 ) != 0 ) m[-1] = -m[-1];
	*to = WORDDIF(m,to);
	if ( dirty ) AN.WildDirt = dirty;
	return(*to);
}

/*
 		#] WildFill : 
 		#[ ResolveSet :			WORD ResolveSet(from,to,subs)

		The set syntax is:
		SET,length,subterm,where,whichmember[,where,whichmember]

		setlength is 2*n+1 with n the number of set substitutions.
		length = setlength + subtermlength + 2

		At `where' is the number of the set and `whichmember' is the
		number of the element. This is still a symbol/dollar and we
		have to find the substitution in the wildcards.
		The output is the subterm in which the setelements have been
		substituted. This is ready for further wildcard substitutions.
*/

WORD ResolveSet(PHEAD WORD *from, WORD *to, WORD *subs)
{
	GETBIDENTITY
	WORD *m, *s, *w, j, i, ii, i3, flag, num;
	DOLLARS d = 0;
#ifdef WITHPTHREADS
	int nummodopt, dtype = -1;
#endif
	m = to;						/* pointer in output */
	s = from + 2;
	w = s + s[1];
	while ( s < w ) *m++ = *s++;
	j = (from[1] - WORDDIF(w,from) ) >> 1;
	m = subs + subs[1];
	subs += SUBEXPSIZE;
	s = subs;
	i = 0;
	while ( s < m ) { i++; s += s[1]; }
	m = to;
	if ( *m >= FUNCTION && functions[*m-FUNCTION].spec
	>= TENSORFUNCTION ) flag = 0;
	else flag = 1;
	while ( --j >= 0 ) {
		if ( w[1] >= 0 ) {
			s = subs;
			for ( ii = 0; ii < i; ii++ ) {
				if ( *s == SYMTONUM && s[2] == w[1] ) { num = s[3]; goto GotOne; }
				s += s[1];
			}
			MLOCK(ErrorMessageLock);
			MesPrint(" Unresolved setelement during substitution");
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		else {	/* Dollar ! */
			d = Dollars - w[1];
#ifdef WITHPTHREADS
			if ( AS.MultiThreaded ) {
				for ( nummodopt = 0; nummodopt < NumModOptdollars; nummodopt++ ) {
					if ( -w[1] == ModOptdollars[nummodopt].number ) break;
				}
				if ( nummodopt < NumModOptdollars ) {
					dtype = ModOptdollars[nummodopt].type;
					if ( dtype == MODLOCAL ) {
						d = ModOptdollars[nummodopt].dstruct+AT.identity;
					}
					else {
						LOCK(d->pthreadslockread);
					}
				}
			}
#endif
			if ( d->type == DOLNUMBER || d->type == DOLTERMS ) {
				if ( d->where[0] == 4 && d->where[3] == 3 && d->where[2] == 1
				&& d->where[1] > 0 && d->where[4] == 0 ) {
					num = d->where[1]; goto GotOne;
				}
			}
			else if ( d->type == DOLINDEX ) {
				if ( d->index > 0 && d->index < AM.OffsetIndex ) {
					num = d->index; goto GotOne;
				}
			}
			else if ( d->type == DOLARGUMENT ) {
				if ( d->where[0] == -SNUMBER && d->where[1] > 0 ) {
					num = d->where[1]; goto GotOne;
				}
			}
			else if ( d->type == DOLWILDARGS ) {
				if ( d->where[0] == 1 && 
				d->where[1] > 0 && d->where[1] < AM.OffsetIndex ) {
					num = d->where[1]; goto GotOne;
				}
				if ( d->where[0] == 0 && d->where[1] < 0 && d->where[3] == 0 ) {
					if ( ( d->where[1] == -SNUMBER && d->where[2] > 0 )
					|| ( d->where[1] == -INDEX && d->where[2] > 0
					&& d->where[2] < AM.OffsetIndex ) ) {
						num = d->where[2]; goto GotOne;
					}
				}
			}
#ifdef WITHPTHREADS
			if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
			MLOCK(ErrorMessageLock);
			MesPrint("Unusable type of variable $%s in set substitution",
				AC.dollarnames->namebuffer+d->name);
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
GotOne:;
#ifdef WITHPTHREADS
		if ( dtype > 0 && dtype != MODLOCAL ) { UNLOCK(d->pthreadslockread); }
#endif
		ii = m[*w];
		if ( ii >= 2*MAXPOWER ) i3 = ii - 2*MAXPOWER;
		else if ( ii <= -2*MAXPOWER ) i3 = -ii - 2*MAXPOWER;
		else i3 = ( ii >= 0 ) ? ii: -ii - 1;

		if ( num > ( Sets[i3].last - Sets[i3].first ) || num <= 0 ) {
			MLOCK(ErrorMessageLock);
			MesPrint("Array bound check during set substitution");
			MesPrint("    value is %d",num);
			MUNLOCK(ErrorMessageLock);
			return(-1);
		}
		m[*w] = (SetElements+Sets[i3].first)[num-1];
		if ( Sets[i3].type == CSYMBOL && m[*w] > MAXPOWER ) {
			if ( ii >= 2*MAXPOWER ) m[*w] -= 2*MAXPOWER;
			else if ( ii <= -2*MAXPOWER ) m[*w] = -(m[*w] - 2*MAXPOWER);
			else {
				m[*w] -= MAXPOWER;
				if ( m[*w] < MAXPOWER ) m[*w] -= 2*MAXPOWER;
				if ( flag ) MakeDirty(m,m+*w,1);
			}
		}
		else if ( Sets[i3].type == CSYMBOL ) {
			if ( ii >= 2*MAXPOWER ) m[*w] += 2*MAXPOWER;
			else if ( ii <= -2*MAXPOWER ) m[*w] = -m[*w] - 2*MAXPOWER;
			else if ( ii < 0 ) m[*w] = - m[*w];
		}
		else if ( ii < 0 ) m[*w] = - m[*w];
		w += 2;
	}
	m = to;
	if ( *m >= FUNCTION && functions[*m-FUNCTION].spec
	>= TENSORFUNCTION ) {
		w = from + 2 + from[3];
		if ( *w == 0 ) {	/* We had function -> tensor */
			m = from + 2 + FUNHEAD; s = to + FUNHEAD;
			while ( m < w ) {
				if ( *m == -INDEX || *m == -VECTOR ) {}
				else if ( *m == -ARGWILD ) { *s++ = FUNNYWILD; }
				else {
					MLOCK(ErrorMessageLock);
					MesPrint("Illegal argument in tensor after set substitution");
					MUNLOCK(ErrorMessageLock);
					SETERROR(-1)
				}
				*s++ = m[1];
				m += 2;
			}
			to[1] = WORDDIF(s,to);
		}
	}
	return(0);
}

/*
 		#] ResolveSet : 
 		#[ ClearWild :			VOID ClearWild()

	Clears the current wildcard settings and makes them ready for
	CheckWild and AddWild.

*/

VOID ClearWild(PHEAD0)
{
	GETBIDENTITY
	WORD n, nn, *w;
	n = (AN.WildValue[-SUBEXPSIZE+1]-SUBEXPSIZE)/4;		/* Number of wildcards */
	AN.NumWild = nn = n;
	if ( n > 0 ) {
		w = AT.WildMask;
		do { *w++ = 0; } while ( --n > 0 );
		w = AN.WildValue;
		do {
			if ( *w == SYMTONUM ) *w = SYMTOSYM;
			w += w[1];
		} while ( --nn > 0 );
	}
}

/*
 		#] ClearWild : 
 		#[ AddWild :			WORD AddWild(oldnumber,type,newnumber)

 		Adds a wildcard assignment.
		Extra parameter in AN.argaddress;

*/

WORD AddWild(PHEAD WORD oldnumber, WORD type, WORD newnumber)
{
	GETBIDENTITY
	WORD *w, *m, n, k, i = -1;
	CBUF *C = cbuf+AT.ebufnum;
DEBUG(WORD *mm;)
	AN.WildReserve = 0;
	m = AT.WildMask;
	w = AN.WildValue;
	n = AN.NumWild;
	if ( n <= 0 ) { return(-1); }
	if ( type <= SYMTOSUB ) {
		do {
			if ( w[2] == oldnumber && *w <= SYMTOSUB ) {
				if ( n > 1 && w[4] == SETTONUM ) i = w[7];
				*w = type;
				if ( *m != 2 ) *m = 1;
				if ( type != SYMTOSUB ) {
					if ( type == SYMTONUM ) AN.MaskPointer = m;
					w[3] = newnumber;
					goto FlipOn;
				}
				m = AddRHS(AT.ebufnum,1);
				w[3] = C->numrhs;
				w = AN.argaddress;
DEBUG(mm = m;)
				n = *w - ARGHEAD;
				w += ARGHEAD;
				while ( (m + n + 10) > C->Top ) m = DoubleCbuffer(AT.ebufnum,m,4);
				while ( --n >= 0 ) *m++ = *w++;
				*m++ = 0;
				C->rhs[C->numrhs+1] = m;
DEBUG(MesPrint("Thread %w(d): m=(%d,%d,%d,%d)(%d)",mm[0],mm[1],mm[2],mm[3],C->numrhs);)
				C->Pointer = m;
				goto FlipOn;
			}
			m++; w += w[1];
		} while ( --n > 0 );
	}
	else if ( type == ARGTOARG ) {
		do {
			if ( w[2] == oldnumber && *w == ARGTOARG ) {
				*m = 1;
				m = AddRHS(AT.ebufnum,1);
				w[3] = C->numrhs;
				w = AN.argaddress;
DEBUG(mm=m;)
				if ( ( newnumber & EATTENSOR ) != 0 ) {
					n = newnumber & ~EATTENSOR;
					*m++ = n;
					w = AN.argaddress;
				}
				else {
					while ( --newnumber >= 0 ) { NEXTARG(w) }
					n = WORDDIF(w,AN.argaddress);
					w = AN.argaddress;
					*m++ = 0;
				}
				while ( (m + n + 10) > C->Top ) m = DoubleCbuffer(AT.ebufnum,m,5);
DEBUG(if ( mm != m-1 ) MesPrint("Thread %w(e): Alarm!"); mm = m-1;)
				while ( --n >= 0 ) *m++ = *w++;
				*m++ = 0;
				C->rhs[C->numrhs+1] = m;
				C->Pointer = m;
DEBUG(MesPrint("Thread %w(e): w=(%d,%d,%d,%d)(%d)",mm[0],mm[1],mm[2],mm[3],C->numrhs);)
				return(0);
			}
			m++; w += w[1];
		} while ( --n > 0 );
	}
	else if ( type == ARLTOARL ) {
		do {
			if ( w[2] == oldnumber && *w == ARGTOARG ) {
				WORD **a;
				*m = 1;
				m = AddRHS(AT.ebufnum,1);
				w[3] = C->numrhs;
DEBUG(mm=m;)
				a = (WORD **)(AN.argaddress); n = 0; k = newnumber;
				while ( --newnumber >= 0 ) {
					w = *a++;
					if ( *w > 0 ) n += *w;
					else if ( *w <= -FUNCTION ) n++;
					else n += 2;
				}
				*m++ = 0;
				while ( (m + n + 10) > C->Top ) m = DoubleCbuffer(AT.ebufnum,m,6);
DEBUG(if ( mm != m-1 ) MesPrint("Thread %w(f): Alarm!"); mm = m-1;)
				a = (WORD **)(AN.argaddress);
				while ( --k >= 0 ) {
					w = *a++;
					if ( *w > 0 ) { n = *w; NCOPY(m,w,n); }
					else if ( *w <= -FUNCTION ) *m++ = *w++;
					else { *m++ = *w++; *m++ = *w++; }
				}
				*m++ = 0;
				C->rhs[C->numrhs+1] = m;
DEBUG(MesPrint("Thread %w(f): w=(%d,%d,%d,%d)(%d)",mm[0],mm[1],mm[2],mm[3],C->numrhs);)
				C->Pointer = m;
				return(0);
			}
			m++; w += w[1];
		} while ( --n > 0 );
	}
	else if ( type == VECTOSUB || type == INDTOSUB ) {
		WORD *ss, *sstop, *tt, *ttstop, j, *v1, *v2 = 0;
		do {
			if ( w[2] == oldnumber && ( *w == type ||
			( type == VECTOSUB && ( *w == VECTOVEC || *w == VECTOMIN ) )
			|| ( type == INDTOSUB && *w == INDTOIND ) ) ) {
				if ( n > 1 && w[4] == SETTONUM ) i = w[7];
				*w = type;
				*m = 1;
				m = AddRHS(AT.ebufnum,1);
				w[3] = C->numrhs;
				w = AN.argaddress;
				n = *w - ARGHEAD;
				w += ARGHEAD;
				while ( (m + n + 10) > C->Top ) m = DoubleCbuffer(AT.ebufnum,m,7);
				while ( --n >= 0 ) *m++ = *w++;
				*m++ = 0;
				C->rhs[C->numrhs+1] = m;
				C->Pointer = m;
				m = AddRHS(AT.ebufnum,1);
				w = AN.argaddress;
				n = *w - ARGHEAD;
				w += ARGHEAD;
				while ( (m + n + 10) > C->Top ) m = DoubleCbuffer(AT.ebufnum,m,8);
				sstop = w + n;
				while ( w < sstop ) {			/* Run over terms */
					tt = w + *w; ttstop = tt - ABS(tt[-1]);
					ss = m; m++; w++;
					while ( w < ttstop ) {		/* Subterms */
						if ( *w != INDEX ) {
							j = w[1];
							NCOPY(m,w,j);
						}
						else {
							v1 = m;
							*m++ = *w++;
							*m++ = j = *w++;
							j -= 2;
							while ( --j >= 0 ) {
								if ( *w >= MINSPEC ) *m++ = *w++;
								else v2 = w++;
							}
							j = WORDDIF(m,v1);
							if ( j != v1[1] ) {
								if ( j <= 2 ) m -= 2;
								else v1[1] = j;
								*m++ = VECTOR;
								*m++ = 4;
								*m++ = *v2;
								*m++ = FUNNYVEC;
							}
						}
					}
					while ( w < tt ) *m++ = *w++;
					*ss = WORDDIF(m,ss);
				}
				*m++ = 0;
				C->rhs[C->numrhs+1] = m;
				C->Pointer = m;
				if ( m > C->Top ) {
					MLOCK(ErrorMessageLock);
					MesPrint("Internal problems with extra compiler buffer");
					MUNLOCK(ErrorMessageLock);
					Terminate(-1);
				}
				goto FlipOn;
			}
			m++; w += w[1];
		} while ( --n > 0 );
	}
	else {
		do {
			if ( w[2] == oldnumber && ( *w == type || ( type == VECTOVEC
			&& ( *w == VECTOMIN || *w == VECTOSUB ) ) || ( type == VECTOMIN
			&& ( *w == VECTOVEC || *w == VECTOSUB ) )
			|| ( type == INDTOIND && *w == INDTOSUB ) ) ) {
				if ( n > 1 && w[4] == SETTONUM ) i = w[7];
				*w = type;
				w[3] = newnumber;
				*m = 1;
				goto FlipOn;
			}
			m++; w += w[1];
		} while ( --n > 0 );
	}
	MLOCK(ErrorMessageLock);
	MesPrint("Bug in AddWild.");
	MUNLOCK(ErrorMessageLock);
	return(-1);
FlipOn:
	if ( i >= 0 ) {
		m = AT.WildMask;
		w = AN.WildValue;
		n = AN.NumWild;
		while ( --n >= 0 ) {
			if ( w[2] == i && *w == SYMTONUM ) {
				*m = 2;
				return(0);
			}
			m++; w += w[1];
		}
		MLOCK(ErrorMessageLock);
		MesPrint(" Bug in AddWild with passing set[i]");
		MUNLOCK(ErrorMessageLock);
/*
		For the moment we want to crash here. That is easier with debugging.
*/
#ifdef WITHPTHREADS
		{ WORD *s = 0;
			*s++ = 1;
		}
#endif
		Terminate(-1);
	}
	return(0);
}

/*
 		#] AddWild : 
 		#[ CheckWild :			WORD CheckWild(oldnumber,type,newnumber,newval)

 		Tests whether a wildcard assignment is allowed.
 		A return value of zero means that it is allowed (nihil obstat).
 		If the variable has been assigned already its existing
 		assignment is returned in AN.oldvalue and AN.oldtype, which are
 		global variables.

		Note the special problem with name?set[i]. Here we have to pass
		an extra assignment. This cannot be done via globals as we
		call CheckWild sometimes twice before calling AddWild.
		Trick: Check the assignment of the number and if OK put it
		in place, but don't alter the used flag (if needed).
		Then AddWild can alter the used flag but the value is there.
		As long as this trick is `hanging' we turn on the flag:
		`AN.WildReserve' which is either turned off by AddWild or by
		a failing call to CheckWild.

		With ARGTOARG the tensors give the number of arguments
		or-ed with EATTENSOR which is at least 8192.
*/

WORD CheckWild(PHEAD WORD oldnumber, WORD type, WORD newnumber, WORD *newval)
{
	GETBIDENTITY
	WORD *w, *m, *s, n, old2, inset;
	WORD n2, oldval, dirty, i, j, notflag = 0, retblock = 0;
	CBUF *C = cbuf+AT.ebufnum;
	m = AT.WildMask;
	w = AN.WildValue;
	n = AN.NumWild;
	if ( n <= 0 ) { AN.oldtype = -1; AN.WildReserve = 0; return(-1); }
	switch ( type ) {
		case SYMTONUM :
			*newval = newnumber;
			do {
				if ( w[2] == oldnumber && *w <= SYMTOSUB ) {
					old2 = *w;
					if ( !*m ) goto TestSet;
					AN.MaskPointer = m;
					if ( *w == SYMTONUM  && w[3] == newnumber ) {
						return(0);
					}
					AN.oldtype = old2; AN.oldvalue = w[3]; goto NoMatch;
				}
				m++; w += w[1];
			} while ( --n > 0 );
			break;
		case SYMTOSYM :
			*newval = newnumber;
			do {
				if ( w[2] == oldnumber && *w <= SYMTOSUB ) {
					old2 = *w;
					if ( *w == SYMTOSYM ) {
						if ( !*m ) goto TestSet;
						if ( newnumber >= 0 && (w+4) < AN.WildStop
						&& ( w[4] == FROMSET || w[4] == SETTONUM )
						&& w[7] >= 0 ) goto TestSet;
						if ( w[3] == newnumber ) return(0);
					}
					else {
						if ( !*m ) goto TestSet;
					}
					goto NoM;
				}
				m++; w += w[1];
			} while ( --n > 0 );
			break;
		case SYMTOSUB :
/*
			Now newval contains the pointer to the argument.
*/
			{
/*
				Search for vector or index nature. If so: reject.
*/
				WORD *ss, *sstop, *tt, *ttstop;
				ss = newval;
				sstop = ss + *ss;
				ss += ARGHEAD;
				while ( ss < sstop ) {
					tt = ss + *ss;
					ttstop = tt - ABS(tt[-1]);
					ss++;
					while ( ss < ttstop ) {
						if ( *ss == INDEX ) goto NoMatch;
						ss += ss[1];
					}
					ss = tt;
				}
			}
			do {
				if ( w[2] == oldnumber && *w <= SYMTOSUB ) {
					old2 = *w;
					if ( *w == SYMTONUM || *w == SYMTOSYM ) {
						if ( !*m ) {
							s = w + w[1];
							if ( s >= AN.WildStop || *s != SETTONUM )
									goto TestSet;
						}
					}
					else if ( *w == SYMTOSUB ) {
						if ( !*m ) {
							s = w + w[1];
							if ( s >= AN.WildStop || *s != SETTONUM )
									goto TestSet;
						}
						n = *newval - 2;
						newval += 2;
						m = C->rhs[w[3]];
						if ( (C->rhs[w[3]+1] - m - 1) == n ) {
							while ( n > 0 ) {
								if ( *m != *newval ) {
									m++; newval++; break;
								}
								m++; newval++;
								n--;
							}
							if ( n <= 0 ) return(0);
						}
					}
					AN.oldtype = old2; AN.oldvalue = w[3]; goto NoMatch;
				}
				m++; w += w[1];
			} while ( --n > 0 );
			break;
		case ARGTOARG :
			do {
				if ( w[2] == oldnumber && *w == ARGTOARG ) {
					if ( !*m ) return(0);		/* nihil obstat */
					m = C->rhs[w[3]];
					if ( ( newnumber & EATTENSOR ) != 0 ) {
						n = newnumber & ~EATTENSOR;
						if ( *m != 0 ) {
							if ( n == *m ) {
								m++;
								while ( --n >= 0 ) {
									if ( *m != *newval ) {
										m++; newval++; break;
									}
									m++; newval++;
								}
								if ( n < 0 ) return(0);
							}
						}
						else {
							m++;
							while ( --n >= 0 ) {
								if ( *newval != m[1] || ( *m != -INDEX
								&& *m != -VECTOR && *m != -SNUMBER ) ) break;
								m += 2;
								newval++;
							}
							if ( n < 0 && *m == 0 ) return(0);
						}
					}
					else {
						i = newnumber;
						if ( *m != 0 ) {	/* Tensor field */
							if ( *m == i ) {
								m++;
								while ( --i >= 0 ) {
									if ( *m != newval[1]
									|| ( *newval != -VECTOR
									  && *newval != -INDEX
									  && *newval != -SNUMBER ) ) break;
									newval += 2;
									m++;
								}
								if ( i < 0 ) return(0);
							}
						}
						else {
							m++;
							s = newval;
							while ( --i >= 0 ) { NEXTARG(s) }
							n = WORDDIF(s,newval);
							while ( --n >= 0 ) {
								if ( *m != *newval ) {
									m++; newval++; break;
								}
								m++; newval++;
							}
							if ( n < 0 && *m == 0 ) return(0);
						}
					}
					AN.oldtype = *w; AN.oldvalue = w[3]; goto NoMatch;
				}
				m++; w += w[1];
			} while ( --n > 0 );
			break;
		case ARLTOARL :
			do {
				if ( w[2] == oldnumber && *w == ARGTOARG ) {
					WORD **a;
					if ( !*m ) return(0);		/* nihil obstat */
					m = C->rhs[w[3]];
					i = newnumber;
					a = (WORD **)newval;
					if ( *m != 0 ) {	/* Tensor field */
						if ( *m == i ) {
							m++;
							while ( --i >= 0 ) {
								s = *a++;
								if ( *m != s[1]
								|| ( *s != -VECTOR
								  && *s != -INDEX
								  && *s != -SNUMBER ) ) break;
								m++;
							}
							if ( i < 0 ) return(0);
						}
					}
					else {
						m++;
						while ( --i >= 0 ) {
							s = *a++;
							if ( *s > 0 ) {
								n = *s;
								while ( --n >= 0 ) {
									if ( *s != *m ) {
										s++; m++; break;
									}
									s++; m++;
								}
								if ( n >= 0 ) break;
							}
							else if ( *s <= -FUNCTION ) {
								if ( *s != *m ) {
									s++; m++; break;
								}
								s++; m++;
							}
							else {
								if ( *s != *m ) {
									s++; m++; break;
								}
								s++; m++;
								if ( *s != *m ) {
									s++; m++; break;
								}
								s++; m++;
							}
						}
						if ( i < 0 && *m == 0 ) return(0);
					}
					AN.oldtype = *w; AN.oldvalue = w[3]; goto NoMatch;
				}
				m++; w += w[1];
			} while ( --n > 0 );
			break;
		case VECTOSUB :
		case INDTOSUB :
/*
			Now newval contains the pointer to the argument(s).
*/
			{
/*
				Search for vector or index nature. If not so: reject.
*/
				WORD *ss, *sstop, *tt, *ttstop, count, jt;
				ss = newval;
				sstop = ss + *ss;
				ss += ARGHEAD;
				while ( ss < sstop ) {
					tt = ss + *ss;
					ttstop = tt - ABS(tt[-1]);
					ss++;
					count = 0;
					while ( ss < ttstop ) {
						if ( *ss == INDEX ) {
							jt = ss[1] - 2; ss += 2;
							while ( --jt >= 0 ) {
								if ( *ss < MINSPEC ) count++;
								ss++;
							}
						}
						else ss += ss[1];
					}
					if ( count != 1 ) goto NoMatch;
					ss = tt;
				}
			}
			do {
				if ( w[2] == oldnumber ) {
					old2 = *w;
					if ( ( type == VECTOSUB && ( *w == VECTOVEC || *w == VECTOMIN ) )
					|| ( type == INDTOSUB && *w == INDTOIND ) ) {
						if ( !*m ) goto TestSet;
						AN.oldtype = old2; AN.oldvalue = w[3]; goto NoMatch;
					}
					else if ( *w == type ) {
						if ( !*m ) goto TestSet;
						if ( type != INDTOIND && type != INDTOSUB ) {	/* Prevent double index */
							n = *newval - 2;
							newval += 2;
							m = C->rhs[w[3]];
							if ( (C->rhs[w[3]+1] - m - 1) == n ) {
								while ( n > 0 ) {
									if ( *m != *newval ) {
										m++; newval++; break;
									}
									m++; newval++;
									n--;
								}
								if ( n <= 0 ) return(0);
							}
						}
						AN.oldtype = old2; AN.oldvalue = w[3]; goto NoMatch;
					}
				}
				m++; w += w[1];
			} while ( --n > 0 );
			break;
		default :
			*newval = newnumber;
			do {
				if ( w[2] == oldnumber ) {
					if ( *w == type ) {
						old2 = *w;
						if ( !*m ) goto TestSet;
						if ( newnumber >= 0 && (w+4) < AN.WildStop &&
							( w[4] == FROMSET || w[4] == SETTONUM )
							&& w[7] >= 0 ) goto TestSet;
						if ( newnumber < 0 && *w == VECTOVEC
							&& (w+4) < AN.WildStop && ( w[4] == FROMSET
							|| w[4] == SETTONUM ) && w[7] >= 0 ) goto TestSet;
/*
						The next statement kills multiple indices -> vector
*/
						if ( *w == INDTOIND && w[3] < 0 ) goto NoMatch;
						if ( w[3] == newnumber ) {
							if ( *w != FUNTOFUN || newnumber < FUNCTION
							|| functions[newnumber-FUNCTION].spec ==
							functions[oldnumber-FUNCTION].spec )
								return(0);
						}
						AN.oldtype = old2; AN.oldvalue = w[3]; goto NoMatch;
					}
					else if ( ( type == VECTOVEC &&
							( *w == VECTOSUB || *w == VECTOMIN ) )
					       || ( type == INDTOIND && *w == INDTOSUB ) ) {
						if ( *m ) goto NoMatch;
						old2 = *w;
						goto TestSet;
					}
					else if ( type == VECTOMIN &&
							( *w == VECTOSUB || *w == VECTOVEC ) ) {
						if ( *m ) goto NoMatch;
						old2 = *w;
						goto TestSet;
					}
				}
				m++; w += w[1];
				if ( n > 1 && ( *w == FROMSET
				|| *w == SETTONUM ) ) { n--; m++; w += w[1]; }
			} while ( --n > 0 );
			break;
	}
	AN.oldtype = -1;
	AN.oldvalue = -1;
	AN.WildReserve = 0;
	MLOCK(ErrorMessageLock);
	MesPrint("Inconsistency in Wildcard prototype.");
	MUNLOCK(ErrorMessageLock);
	return(-1);
NoMatch:
	AN.WildReserve = 0;
	return(1+retblock);
/*
	Here we test the compatibility with a set specification.
*/ 
TestSet:
	dirty = *m;
	oldval = w[3];
	w += w[1];
	if ( w < AN.WildStop && ( *w == FROMSET || *w == SETTONUM ) ) {
		WORD k;
		s = w;							
		j = w[2]; n2 = w[3];
/*
			if SETTONUM:  x?j[n2]
			if FROMSET:   x?j?n2    or x?j and n2 = -WOLDOFFSET.
*/
		if ( j > WILDOFFSET ) {
			j -= 2*WILDOFFSET;
			notflag = 1;
/*
		???????
*/
			AN.oldtype = -1;
			AN.oldvalue = -1;
		}
		if ( j < AM.NumFixedSets ) {	/* special set */
			retblock = 1;
			switch ( j ) {
				case POS_:
					if ( type != SYMTONUM ||
					newnumber <= 0 ) goto NoMnot;
					break;
				case POS0_:
					if ( type != SYMTONUM ||
					newnumber < 0 ) goto NoMnot;
					break;
				case NEG_:
					if ( type != SYMTONUM ||
					newnumber >= 0 ) goto NoMnot;
					break;
				case NEG0_:
					if ( type != SYMTONUM ||
					newnumber > 0 ) goto NoMnot;
					break;
				case EVEN_:
					if ( type != SYMTONUM ||
					( newnumber & 1 ) != 0 ) goto NoMnot;
					break;
				case ODD_:
					if ( type != SYMTONUM ||
					( newnumber & 1 ) == 0 ) goto NoMnot;
					break;
				case Z_:
					if ( type != SYMTONUM ) goto NoMnot;
					break;
				case SYMBOL_:
					if ( type != SYMTOSYM ) goto NoMnot;
					break;
				case FIXED_:
					if ( type != INDTOIND ||
					newnumber >= AM.OffsetIndex ||
					newnumber < 0 ) goto NoMnot;
					break;
				case INDEX_:
					if ( type != INDTOIND ||
					newnumber < 0 ) goto NoMnot;
					break;
				case Q_:
					if ( type == SYMTONUM ) break;
					if ( type == SYMTOSUB ) {
						WORD *ss, *sstop;
						ss = newval;
						sstop = ss + *ss;
						ss += ARGHEAD;
						if ( ss >= sstop ) break;
						if ( ss + *ss < sstop ) goto NoMnot;
						if ( ABS(sstop[-1]) == ss[0]-1 ) break;
					}
					goto NoMnot;
				case DUMMYINDEX_:
					if ( type != INDTOIND ||
					newnumber < AM.IndDum || newnumber >= AM.IndDum+MAXDUMMIES ) goto NoMnot;
					break;
				case VECTOR_:
					if ( type != VECTOVEC ) goto NoMnot;
					break;
				default:
					goto NoMnot;
			}
Mnot:
			if ( notflag ) goto NoM;
			return(0);
NoMnot:
			if ( !notflag ) goto NoM;
			return(0);
		}
		else if ( Sets[j].type == CRANGE ) {
			if ( ( type == SYMTONUM )
			|| ( type == INDTOIND && ( newnumber > 0
			 && newnumber <= AM.OffsetIndex ) ) ) {
				if ( Sets[j].first < MAXPOWER ) {
					if ( newnumber >= Sets[j].first ) goto NoMnot;
				}
				else if ( Sets[j].first < 3*MAXPOWER ) {
					if ( newnumber+2*MAXPOWER > Sets[j].first ) goto NoMnot;
				}
				if ( Sets[j].last > -MAXPOWER ) {
					if ( newnumber <= Sets[j].last ) goto NoMnot;
				}
				else if ( Sets[j].last > -3*MAXPOWER ) {
					if ( newnumber-2*MAXPOWER < Sets[j].last ) goto NoMnot;
				}
				goto Mnot;
			}
			goto NoMnot;
		}
/*
		Now we have to determine which set element
*/
		w = SetElements + Sets[j].first;
		m = SetElements + Sets[j].last;
		if ( ( Sets[j].flags & ORDEREDSET ) == ORDEREDSET ) {
/*
			We search first and ask questions later
*/
			i = BinarySearch(w,Sets[j].last-Sets[j].first,newnumber);
			if ( i < 0 ) {	/* no matter what, it is not in the set. */
				goto NoMnot;
			}
			else {
/*
				We can set the proper parameters now to make only the
				checks for the given set element.
				After that we jump into the appropriate loop.
*/
				w = m = SetElements + i;
				i++;
				if ( Sets[j].type == -1 || Sets[j].type == CNUMBER ) {
					goto insideloop1;
				}
				else {
					goto insideloop2;
				}
			}
		}
		i = 1;
		if ( Sets[j].type == -1 || Sets[j].type == CNUMBER ) {
		  do {
			insideloop1:
			if ( notflag ) {
				switch ( type ) {
					case SYMTOSYM:
						if ( Sets[j].type == CNUMBER ) {}
						else {
							if ( *w == newnumber ) goto NoMatch;
						}
						break;
					case SYMTONUM:
					case INDTOIND:
						if ( *w == newnumber ) goto NoMatch;
						break;
					default:
						break;
				}
			}
			else if ( type != SYMTONUM && type != INDTOIND
			&& type != SYMTOSYM ) goto NoMatch;
			else if ( type == SYMTOSYM && Sets[j].type == CNUMBER ) goto NoMatch;
			else if ( *w == newnumber ) {
				if ( *s == SETTONUM ) {
					if ( n2 == oldnumber && type
					<= SYMTOSUB ) goto NoMatch;
					m = AT.WildMask;
					w = AN.WildValue;
					n = AN.NumWild;
					while ( --n >= 0 ) {
						if ( w[2] == n2 && *w <= SYMTOSUB ) {
							if ( !*m ) {
								*w = SYMTONUM;
								w[3] = i;
								AN.WildReserve = 1;
								return(0);
							}
							if ( *w != SYMTONUM )
								goto NoMatch;
							if ( w[3] == i ) return(0);
							i = w[3];
							j = (SetElements + Sets[j].first)[i];
							if ( j == n2 ) return(0);
							goto NoMatch;
						}
						m++; w += w[1];
					}
				}
				else if ( n2 >= 0 ) {
					*newval = *(w - Sets[j].first + Sets[n2].first);
					if ( *newval > MAXPOWER ) *newval -= 2*MAXPOWER;
					if ( dirty && *newval != oldval ) {
						*newval = oldval; goto NoMatch;
					}
				}
				return(0);
			}
			i++;
		  } while ( ++w < m );
		}
		else {
		  do {
			insideloop2:
			inset = *w;
			if ( notflag ) {
			switch ( type ) {
				case SYMTONUM:
				case SYMTOSYM:
					if ( ( type == SYMTOSYM && *w == newnumber ) 
					|| ( type == SYMTONUM && *w-2*MAXPOWER == newnumber ) ) {
						goto NoMatch;
					}
					/* fall through */
				case SYMTOSUB:
					if ( *w < 0 ) {
						WORD *mm = AT.WildMask, *mmm, *part;
						WORD *ww = AN.WildValue;
						WORD nn = AN.NumWild;
						k = -*w;
						while ( --nn >= 0 ) {
							if ( *mm && ww[2] == k && ww[0] == type ) {
								if ( type != SYMTOSUB ) {
									if ( ww[3] == newnumber ) goto NoMatch;
								}
								else {
									mmm = C->rhs[ww[3]];
									nn = *newval-2;
									part = newval+2;
									if ( (C->rhs[ww[3]+1]-mmm-1) == nn ) {
										while ( --nn >= 0 ) {
											if ( *mmm != *part ) {
												mmm++; part++; break;
											}
											mmm++; part++;
										}
										if ( nn < 0 ) goto NoMatch;
									}
								}
								break;
							}
							mm++; ww += ww[1];
						}
					}
					break;
				case VECTOMIN:
					if ( type == VECTOMIN ) {
						if ( inset >= AM.OffsetVector ) { i++; continue; }
						inset += WILDMASK;
					}
					/* fall through */
				case VECTOVEC:
					if ( inset == newnumber ) goto NoMatch;
					/* fall through */
				case VECTOSUB:
					if ( inset - WILDOFFSET >= AM.OffsetVector ) {
						WORD *mm = AT.WildMask, *mmm, *part;
						WORD *ww = AN.WildValue;
						WORD nn = AN.NumWild;
						k = inset - WILDOFFSET;
						while ( --nn >= 0 ) {
							if ( *mm && ww[2] == k && ww[0] == type ) {
								if ( type == VECTOVEC ) {
									if ( ww[3] == newnumber ) goto NoMatch;
								}
								else {
									mmm = C->rhs[ww[3]];
									nn = *newval-2;
									part = newval+2;
									if ( (C->rhs[ww[3]+1]-mmm-1) == nn ) {
										while ( --nn >= 0 ) {
											if ( *mmm != *part ) {
												mmm++; part++; break;
											}
											mmm++; part++;
										}
										if ( nn < 0 ) goto NoMatch;
									}
								}
								break;
							}
							mm++; ww += ww[1];
						}
					}
					break;
				case INDTOIND:
					if ( *w == newnumber ) goto NoMatch;
					/* fall through */
				case INDTOSUB:
					if ( *w - (WORD)WILDMASK >= AM.OffsetIndex ) {
						WORD *mm = AT.WildMask, *mmm, *part;
						WORD *ww = AN.WildValue;
						WORD nn = AN.NumWild;
						k = *w - WILDMASK;
						while ( --nn >= 0 ) {
							if ( *mm && ww[2] == k && ww[0] == type ) {
								if ( type == INDTOIND ) {
									if ( ww[3] == newnumber ) goto NoMatch;
								}
								else {
									mmm = C->rhs[ww[3]];
									nn = *newval-2;
									part = newval+2;
									if ( (C->rhs[ww[3]+1]-mmm-1) == nn ) {
										while ( --nn >= 0 ) {
											if ( *mmm != *part ) {
												mmm++; part++; break;
											}
											mmm++; part++;
										}
										if ( nn < 0 ) goto NoMatch;
									}
								}
								break;
							}
							mm++; ww += ww[1];
						}
					}
					break;
				case FUNTOFUN:
					if ( *w == newnumber ) goto NoMatch;
					if ( ( type == FUNTOFUN &&
					( k = *w - WILDMASK ) > FUNCTION ) ) {
						WORD *mm = AT.WildMask;
						WORD *ww = AN.WildValue;
						WORD nn = AN.NumWild;
						while ( --nn >= 0 ) {
							if ( *mm && ww[2] == k && ww[0] == type ) {
								if ( ww[3] == newnumber ) goto NoMatch;
								break;
							}
							mm++; ww += ww[1];
						}
					}
				default:
					break;
			}
			}
			else {
			  if ( type == VECTOMIN ) {
				if ( inset >= AM.OffsetVector ) { i++; continue; }
				inset += WILDMASK;
			  }
			  if ( ( inset == newnumber && type != SYMTONUM ) ||
			  ( type == SYMTONUM && inset-2*MAXPOWER == newnumber ) ) {
				if ( *s == SETTONUM ) {
					if ( n2 == oldnumber && type
					<= SYMTOSUB ) goto NoMatch;
					m = AT.WildMask;
					w = AN.WildValue;
					n = AN.NumWild;
					while ( --n >= 0 ) {
						if ( w[2] == n2 && *w <= SYMTOSUB ) {
							if ( !*m ) {
								*w = SYMTONUM;
								w[3] = i;
								AN.WildReserve = 1;
								return(0);
							}
							if ( *w != SYMTONUM )
								goto NoMatch;
							if ( w[3] == i ) return(0);
							i = w[3];
							j = (SetElements + Sets[j].first)[i];
							if ( j == n2 ) return(0);
							goto NoMatch;
						}
						m++; w += w[1];
					}
				}
				else if ( n2 >= 0 ) {
					*newval = *(w - Sets[j].first + Sets[n2].first);
					if ( *newval > MAXPOWER ) *newval -= 2*MAXPOWER;
					if ( dirty && *newval != oldval ) {
						*newval = oldval; goto NoMatch;
					}
				}
				return(0);
			  }
			}
			i++;
		  } while ( ++w < m );
		}
		if ( notflag ) return(0);
		AN.oldtype = old2; AN.oldvalue = oldval; goto NoMatch;
	}
	else { return(0); }

NoM:
	AN.oldtype = old2; AN.oldvalue = w[3]; goto NoMatch;
}

/*
 		#] CheckWild : 
 	#] Wildcards :
  	#[ DenToFunction :

	Renames the denominator function into a function with the given number.
	For the syntax see   Denominators,function;
*/

int DenToFunction(WORD *term, WORD numfun)
{
	int action = 0;
	WORD *t, *tstop, *tnext, *arg, *argstop, *targ;
	t = term+1;
	tstop = term + *term; tstop -= ABS(tstop[-1]);
	while ( t < tstop ) {
		if ( *t == DENOMINATOR ) {
			*t = numfun; t[2] |= DIRTYFLAG; action = 1;
		}
		tnext = t + t[1];
		if ( *t >= FUNCTION && functions[*t-FUNCTION].spec == 0 ) {
			arg = t + FUNHEAD;
			while ( arg < tnext ) {
				if ( *arg > 0 ) {
					targ = arg + ARGHEAD; argstop = arg + *arg;
					while ( targ < argstop ) {
						if ( DenToFunction(targ,numfun) ) {
							arg[1] |= DIRTYFLAG; t[2] |= DIRTYFLAG; action = 1;
						}
						targ += *targ;
					}
					arg = argstop;
				}
				else if ( *arg <= -FUNCTION ) arg++;
				else arg += 2;
			}
		}
		t = tnext;
	}
	return(action);
}

/*
  	#] DenToFunction : 
*/
