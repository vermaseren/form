/*
  	#[ Includes:

	Routines for the optimization of FORTRAN or C output.
	We have to do a number of things.
	1: map all objects to a special type of scalars. In terms of these
	   scalars the terms are a bit simpler:
	   size numsca sca1,pow1,sca2,pow2,...,coeff
	   If size < 0 we have to skip -size words
	   A zero ends a subexpression, two zeroes the whole expression
	2: Aply the routines from the codegenerator of spider.
	The problem is where to leave the intermediate results.
	We put them in a buffer and try to be careful.
	Also the temporary assignments are put in a buffer.
	We need a system of pointers for this.
	The name of the vector with intermediate results should be selected
	as a variable with as few letters as possible. Try which 1 character
	name is not in use. Next try two character names etc. Assume z.
	After that all intermediate variables have the name z(i).
*/

#include "form3.h"

static char *useprop =
"Only Symbols, DotProducts, User-defined Functions and vectors with indices\n\
are allowed in optimized FORTRAN/C output";

typedef struct ScAlAr {
	WORD *buffer;
	WORD *pointer;
	WORD *top;
	LONG bufsize;
	LONG numterms;
} SCALAR;

static SCALAR *scabuffer = 0;
static LONG scasize = 0, scanumber = -1;

static LONG *objoffset = 0, objsize = 0; 
static WORD *objbuffer = 0, *objpointer = 0, *objtop = 0;
static int numobjects = 0, numobjoffsets = 0;
static int iniobjects = 0;

static UBYTE scratchname[10];
static WORD **helppointers = 0, **sortpointers = 0;
static LONG sizepointers = 0;

static LONG *multiplicities = 0, multiplicitysize = 0;

/*
  	#] Includes:
  	#[ Optimize:
*/

int Optimize ARG1(WORD,numexpr)
{
	int retval;
	WORD powmax;
	LONG j;
	if ( FindScratchName() ) return(-1);
	if ( LoadOpti(numexpr) < 0 ) return(-1);
	iniobjects = numobjects;
/*
	Now we have the terms in the buffer.
	We pass the various optimization steps.
*/
	HuntNumFactor(0L,0,0);
	powmax = 0;
/*
	{
	WORD i;
	for ( j = 0; j < scanumber; j++ ) {
		SortOpti(j);
		i = MaxPowerOpti(j);
		if ( i > powmax ) powmax = i;
	}
	for ( i = powmax; i > 0; i-- ) {
		for ( j = 0; j < scanumber; j++ ) {
			HuntPairs(j,i);
		}
	}
	}
*/
	for ( j = 0; j < scanumber; j++ ) HuntBrackets(j);
	CombiOpti();
/*
	Finally we print the results.
*/
	retval = PrintOptima(numexpr);
	CleanOptiBuffer();
	return(retval);
}

/*
  	#] Optimize:
  	#[ LoadOpti:
*/

int LoadOpti ARG1(WORD,numexpr)
{
	WORD *term, *t, *tstop, *m, *mstop, *oldwork = AR.WorkPointer, *tbuf, *w, nc;
	WORD pow = 0;
	LONG objnum = 0;
	int i;
	tbuf = oldwork;
	AR.WorkPointer = tbuf + 2*AM.MaxTer;
	term = AR.WorkPointer;
	while ( GetTerm(term) > 0 ) {
		AR.WorkPointer = term + *term;
		Normalize(term);
		AR.WorkPointer = term + *term;
/*
		First hunt objects. Some we don't accept!
		We do accept symbols, dotproducts, vectors, user defined functions.
		We do not accept indices, system functions and the rest.
*/
		w = tbuf + 1;
		GETSTOP(term,tstop);
		t = term + 1; mstop = term + *term - 1;
		nc = *mstop;
		*w++ = LNUMBER; *w++ = ABS(nc)+2; *w++ = nc; m = tstop;
		while ( m < mstop ) *w++ = *m++;
		while ( t < tstop ) {
			mstop = t + t[1];
			if ( *t == SYMBOL ) {
				m = t + 2;
				while ( m < mstop ) {
					objnum = PutObject(m,SYMBOL);
					m += 2; pow = m[-1];
					*w++ = objnum >> BITSINWORD;
					*w++ = objnum & WORDMASK;
					*w++ = pow;
				}
			}
			else if ( *t == DOTPRODUCT ) {
				m = t + 2;
				while ( m < mstop ) {
					objnum = PutObject(m,DOTPRODUCT);
					m += 3; pow = m[-1];
					*w++ = objnum >> BITSINWORD;
					*w++ = objnum & WORDMASK;
					*w++ = pow;
				}
			}
			else if ( *t == VECTOR ) {
				m = t + 2;
				while ( m < mstop ) {
					objnum = PutObject(m,VECTOR);
					m += 2;
					pow = 1;
					while ( m < mstop && m[0] == m[-2] && m[1] == m[-1] ) {
						pow++; m += 2;
					}
					*w++ = objnum >> BITSINWORD;
					*w++ = objnum & WORDMASK;
					*w++ = pow;
				}
			}
			else if ( *t > MAXBUILTINFUNCTION ) {
				objnum = PutObject(t,FUNCTION);
				pow = 1;
				while ( mstop < tstop ) {
					m = mstop;
					for ( i = 0; i < t[1]; i++ ) {
						if ( m[i] != t[i] ) break;
					}
					if ( i < t[1] ) break;
					pow++;
					t = mstop; mstop = t + t[1];
				}
				*w++ = objnum >> BITSINWORD;
				*w++ = objnum & WORDMASK;
				*w++ = pow;
			}
			else {
				MesPrint("Problems with expression %s:",EXPRNAME(numexpr));
				MesPrint(useprop);
				AR.WorkPointer = oldwork;
				return(-1);
			}
			t = mstop;
		}
		tbuf[0] = w-tbuf;
		AddToOpti(tbuf,0);
	}
	AR.WorkPointer = oldwork;
/*
	We sort the terms internally. This makes life easier later.
*/
	t = scabuffer->buffer;
	while ( t < scabuffer->pointer ) {
		NormOpti(t);
		t += *t;
	}
	return(0);
}

/*
  	#] LoadOpti:
  	#[ CleanOptiBuffer:
*/

void CleanOptiBuffer ARG0
{
	LONG i;
	if ( scanumber > 0 ) {
		for ( i = 0; i < scanumber; i++ ) {
			if ( scabuffer[i].buffer ) M_free(scabuffer[i].buffer,"scabuf2");
			scabuffer[i].buffer = scabuffer[i].pointer = scabuffer[i].top = 0;
			scabuffer[i].bufsize = scabuffer[i].numterms = 0;
		}
		if ( scabuffer ) M_free(scabuffer,"scabuffer");
		scabuffer = 0;
		scasize = 0; scanumber = -1;
	}
	if ( objbuffer ) M_free(objbuffer,"objbuffer");
	objbuffer = objtop = 0;
	objsize = 0;
	if ( objoffset ) M_free(objoffset,"objoffset");
	objoffset = 0;
	numobjoffsets = 0;
	if ( sortpointers ) M_free(sortpointers,"optisort");
	sortpointers = 0; helppointers = 0; sizepointers = 0;
	if ( multiplicities ) M_free(multiplicities,"multiplicities");
	multiplicities = 0;
	multiplicitysize = 0;
}

/*
  	#] CleanOptiBuffer:
  	#[ PutObject:
*/

int PutObject ARG2(WORD *,object,int,type)
{
	int i, k;
	WORD *obj, *o, *oo, *t, *newobjbuffer;
	LONG size = 2, j, newobjsize, *newoffsets;
	switch ( type ) {
		case SYMBOL:     size = 2; break;
		case DOTPRODUCT: size = 3; break;
		case VECTOR:     size = 3; break;
		case SUBEXPRESSION:
			t = object; while ( *t ) t += *t; size = t-object+2; break;
		case FUNCTION:   size = object[1]+1; break;
	}
	for ( i = 1; i <= numobjects; i++ ) {
		obj = objbuffer + objoffset[i];
		if ( *obj != type ) continue;
		switch ( type ) {
			case SYMBOL:     if ( obj[1] == *object ) return(i); break;
			case DOTPRODUCT:
			case VECTOR:
				if ( obj[1] == *object && obj[2] == object[1] ) return(i);
				break;
			case FUNCTION:
				k = object[1]; o = obj+1; oo = object;
				while ( --k >= 0 ) {
					if ( *o != *oo ) break;
					o++; oo++;
				}
				if ( k < 0 ) return(i);
				break;
			case SUBEXPRESSION:
				if ( size == objoffset[i+1]-objoffset[i] ) {
					o = obj + 1; oo = object; j = size-1;
					while ( --j >= 0 ) {
						if ( *o != *oo ) break;
						o++; oo++;
					}
					if ( j < 0 ) return(i);
				}
				break;
		}
	}
/*
	Here we should add an object. We have to check two buffers.
*/
	while ( objpointer + size >= objtop ) {
		if ( objsize == 0 ) newobjsize = 2*size;
		else newobjsize = 2*objsize;
		if ( newobjsize < 200 ) newobjsize = 200;
		newobjbuffer = (WORD *)Malloc1(newobjsize*sizeof(WORD),"objbuffer");
		if ( objsize ) {
			for ( j = 0; j < objsize; j++ ) newobjbuffer[j] = objbuffer[j];
			objpointer = (objpointer-objbuffer) + newobjbuffer;
			if ( objbuffer ) M_free(objbuffer,"objbuffer");
			objbuffer = newobjbuffer;
		}
		else {
			objpointer = objbuffer = newobjbuffer;
		}
		objsize = newobjsize;
		objtop = objbuffer + objsize;
	}
	if ( numobjects + 3 >= numobjoffsets ) {
		if ( numobjoffsets == 0 ) newobjsize = 50;
		else newobjsize = 2*numobjoffsets;
		newoffsets = (LONG *)Malloc1(newobjsize*sizeof(LONG),"newoffsets");
		if ( numobjects > 0 ) {
			for ( j = numobjects+1; j >= 0; j-- ) newoffsets[j] = objoffset[j];
			if ( objoffset ) M_free(objoffset,"objoffset");
		}
		else {
			newoffsets[0] = newoffsets[1] = newoffsets[2] = 0;
		}
		numobjoffsets = newobjsize; objoffset = newoffsets;
	}
	numobjects++;
	o = objbuffer + objoffset[numobjects]; *o++ = type;
	switch ( type ) {
		case SYMBOL:     *o++ = *object; break;
		case VECTOR:     
		case DOTPRODUCT: *o++ = *object++; *o++ = *object; break;
		case FUNCTION: k = object[1]; oo = object;
			while ( --k >= 0 ) *o++ = *oo++;
			break;
		case SUBEXPRESSION:
			oo = object; j = size;
			while ( --j >= 0 ) *o++ = *oo++;
			break;
	}
	objoffset[numobjects+1] = o - objbuffer;
	return(numobjects);
}

/*
  	#] PutObject:
  	#[ AddToOpti:
*/

int AddToOpti ARG2(WORD *,term,int,num)
{
	LONG newnumber, i;
	WORD *w;
	int j;
	SCALAR *newsca;
	if ( num >= scasize ) {	/* increase scalar buffer */
		if ( scasize <= 0 ) newnumber = 100;
		else newnumber = scasize * 2;
		newsca = (SCALAR *)Malloc1(newnumber * sizeof(SCALAR),"scabuffer");
		if ( scanumber > 0 ) {
			for ( i = 0; i < scanumber; i++ ) newsca[i] = scabuffer[i];
			for ( ; i < newnumber; i++ ) {
				newsca[i].buffer = newsca[i].pointer = newsca[i].top = 0;
				newsca[i].numterms = newsca[i].bufsize = 0;
			}
			if ( scabuffer ) M_free(scabuffer,"scabuffer");
		}
		scabuffer = newsca;
		scasize = newnumber;
	}
	if ( num >= scanumber ) {  /* create new entry */
		for ( i = scanumber; i <= num; i++ ) {
			newsca = scabuffer + i;
			if ( newsca->bufsize == 0 ) {
				newsca->bufsize = 40;
				newsca->buffer = (WORD *)Malloc1(newsca->bufsize*sizeof(WORD),"scabuf2");
				newsca->pointer = newsca->buffer;
				newsca->top = newsca->buffer + newsca->bufsize;
				newsca->numterms = 0;
			}
		}
		scanumber = num+1;
	}
	newsca = scabuffer + num;
	while ( newsca->pointer + term[0]+1 >= newsca->top ) {
		newnumber = newsca->bufsize * 2;
		w = (WORD *)Malloc1(newnumber*sizeof(WORD),"newscabuffer");
		i = newsca->pointer - newsca->buffer;
		while ( --i >= 0 ) w[i] = newsca->buffer[i];
		newsca->pointer = ( newsca->pointer - newsca->buffer ) + w;
		newsca->bufsize = newnumber;
		newsca->top = w + newsca->bufsize;
		if ( newsca->buffer ) M_free(newsca->buffer,"newscabuffer");
		newsca->buffer = w;
	}
	w = term;
	j = *term;
	while ( --j >= 0 ) *newsca->pointer++ = *w++;
	*(newsca->pointer) = 0;
	newsca->numterms++;
	return(0);
}

/*
  	#] AddToOpti:
  	#[ FindScratchName:
*/

int FindScratchName ARG0
{
	int i, j, k, l, m;
	WORD number;
	UBYTE sname[10];
/*
	First attempt. Single characters. Be carefull with the case.
*/
	scratchname[0] = 'z'; scratchname[1] = 0;
	sname[0] = 'Z'; sname[1] = 0;
	for ( i = 25; i >= 0; i--, scratchname[0]--, sname[0]-- ) {
		if ( ( GetName(AC.varnames,scratchname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.exprnames,scratchname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) )
			return(0);
	}
	scratchname[0] = 'z'; scratchname[2] = 0; sname[2] = 0;
	for ( i = 25; i >= 0; i--, scratchname[0]-- ) {
	  scratchname[1] = 'z'; 
	  for ( j = 25; j >= 0; j--, scratchname[1]-- ) {
		sname[0] = scratchname[0]; sname[1] = scratchname[1];
		if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) ) {
		  sname[0] = scratchname[0]; sname[1] = scratchname[1]-'a'+'A';
		  if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		  && ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) ) {
			sname[0] = scratchname[0]-'a'+'A'; sname[1] = scratchname[1]-'a'+'A';
			if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
			&& ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) ) {
			  sname[0] = scratchname[0]-'a'+'A'; sname[1] = scratchname[1];
			  if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
			  && ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) )
				return(0);
			}
		  }
		}
	  }
	  scratchname[1] = '9'; 
	  for ( j = 9; j >= 0; j--, scratchname[1]-- ) {
		sname[0] = scratchname[0]; sname[1] = scratchname[1];
		if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) ) {
		  sname[0] = scratchname[0]-'a'+'A';
		  if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		  && ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) )
			return(0);
		}
	  }
	}
	scratchname[0] = 'z'; scratchname[3] = 0; sname[3] = 0;
	for ( i = 25; i >= 0; i--, scratchname[0]-- ) {
	  scratchname[1] = '9'; 
	  for ( j = 9; j >= 0; j--, scratchname[1]-- ) {
	  scratchname[2] = '9'; 
	  for ( k = 9; k >= 0; k--, scratchname[2]-- ) {
		sname[0] = scratchname[0]; sname[1] = scratchname[1];
		sname[2] = scratchname[2];
		if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) ) {
		  sname[0] = scratchname[0]-'a'+'A';
		  if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		  && ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) )
			return(0);
		}
	  }
	  }
	}
	scratchname[0] = 'z'; scratchname[4] = 0; sname[4] = 0;
	for ( i = 25; i >= 0; i--, scratchname[0]-- ) {
	  scratchname[1] = '9'; 
	  for ( j = 9; j >= 0; j--, scratchname[1]-- ) {
	  scratchname[2] = '9'; 
	  for ( k = 9; k >= 0; k--, scratchname[2]-- ) {
	  scratchname[3] = '9'; 
	  for ( l = 9; l >= 0; l--, scratchname[3]-- ) {
		sname[0] = scratchname[0]; sname[1] = scratchname[1];
		sname[2] = scratchname[2]; sname[3] = scratchname[3];
		if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) ) {
		  sname[0] = scratchname[0]-'a'+'A';
		  if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		  && ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) )
			return(0);
		}
	  }
	  }
	  }
	}
	scratchname[0] = 'z'; scratchname[5] = 0; sname[5] = 0;
	for ( i = 25; i >= 0; i--, scratchname[0]-- ) {
	  scratchname[1] = '9'; 
	  for ( j = 9; j >= 0; j--, scratchname[1]-- ) {
	  scratchname[2] = '9'; 
	  for ( k = 9; k >= 0; k--, scratchname[2]-- ) {
	  scratchname[3] = '9'; 
	  for ( l = 9; l >= 0; l--, scratchname[3]-- ) {
	  scratchname[4] = '9'; 
	  for ( m = 9; m >= 0; m--, scratchname[4]-- ) {
		sname[0] = scratchname[0]; sname[1] = scratchname[1];
		sname[2] = scratchname[2]; sname[3] = scratchname[3];
		sname[4] = scratchname[4];
		if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		&& ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) ) {
		  sname[0] = scratchname[0]-'a'+'A';
		  if ( ( GetName(AC.varnames,sname,&number,NOAUTO) == NAMENOTFOUND )
		  && ( GetName(AC.exprnames,sname,&number,NOAUTO) == NAMENOTFOUND ) )
			return(0);
		}
	  }
	  }
	  }
	  }
	}
	MesPrint("Could not find a decent name for the scratch variable in Optimize");
	return(-1);
}

/*
  	#] FindScratchName:
  	#[ PrintOptima:
*/

int PrintOptima ARG1(WORD,numexpr)
{
	UBYTE obuffer[80];
	WORD *obj, stermbuf[10], *t, *m, n, oldskip = AO.OutSkip;
	int i, first, fsym, *used;
	SCALAR *sca;
	LONG num, totnum = numobjects + scanumber, j;
	AC.OutputMode = FORTRANMODE;
	used = (int *)Malloc1((totnum+1)*sizeof(int),"PrintOptima");
	for ( j = 0; j < totnum; j++ ) used[j] = 0;
/*
	First we print the objects for as far as they should be printed
	(we do not convert the symbols).
	Because these routines are called from the writing routines we can
	safely assume that the writing environment is active.
*/
	FiniLine();
	sprintf((char *)obuffer,"DOUBLE PRECISION %s(%ld)",(char *)scratchname
		,numobjects+scanumber-1);
	TokenToLine(obuffer);
	FiniLine();
	for ( i = 1; i <= iniobjects; i++ ) {
		obj = objbuffer + objoffset[i];
		switch ( *obj ) {
			case SYMBOL:
				sprintf((char *)obuffer,"%s(%d)=",(char *)scratchname,i);
				TokenToLine(obuffer);
				stermbuf[0] = SYMBOL;
				stermbuf[1] = 4;
				stermbuf[2] = obj[1];
				stermbuf[3] = 1;
				WriteSubTerm(stermbuf,1);
				break;
			case DOTPRODUCT:
				sprintf((char *)obuffer,"%s(%d)=",(char *)scratchname,i);
				TokenToLine(obuffer);
				stermbuf[0] = DOTPRODUCT;
				stermbuf[1] = 5;
				stermbuf[2] = obj[1];
				stermbuf[3] = obj[2];
				stermbuf[4] = 1;
				WriteSubTerm(stermbuf,1);
				break;
			case VECTOR:
				sprintf((char *)obuffer,"%s(%d)=",(char *)scratchname,i);
				TokenToLine(obuffer);
				stermbuf[0] = VECTOR;
				stermbuf[1] = 4;
				stermbuf[2] = obj[1];
				stermbuf[3] = obj[2];
				WriteSubTerm(stermbuf,1);
				break;
/*
			case LNUMBER:
				sprintf((char *)obuffer,"%s(%d)=",(char *)scratchname,i);
				TokenToLine(obuffer);
				n = REDLENG(obj[2]);
				if ( n < 0 ) { n = -n; TokenToLine((UBYTE *)" - "); }
				RatToLine((UWORD *)(obj+3),n);
				break;
*/
			case SUBEXPRESSION:
				break;
			case FUNCTION:
				sprintf((char *)obuffer,"%s(%d)=",(char *)scratchname,i);
				TokenToLine(obuffer);
				AO.OutSkip = 7;
				WriteSubTerm(obj+1,1);
				AO.OutSkip = oldskip;
				break;
		}
		FiniLine();
		used[i] = 1;
	}
	for (;;) {
	for ( i = scanumber-1; i >= 0; i-- ) {
		if ( used[i+numobjects] && i != 0 ) continue;
		sca = scabuffer+i;
		t = sca->buffer;
		while ( t < sca->pointer ) {
			m = t + t[2] + 1; t += *t;
			while ( m < t ) {
				num = ( (LONG)(m[0]) << BITSINWORD ) + m[1];
				if ( used[num] == 0 ) goto nexti;
				m += 3;
			}
		}

		if ( i == 0 ) {
			sprintf((char *)obuffer,"%s=",EXPRNAME(numexpr));
			TokenToLine(obuffer);
		}
		else {
			sprintf((char *)obuffer,"%s(%d)=",(char *)scratchname,i+numobjects);
			TokenToLine(obuffer);
			used[i+numobjects] = 1;
		}
		sca = scabuffer+i;
		t = sca->buffer; first = 1;
		AO.OutSkip = 7;
		while ( t < sca->pointer ) {
			m = t + 1; t += *t;
			n = REDLENG(m[2]);
			fsym = 1;
			if ( n < 0 ) { n = -n; TokenToLine((UBYTE *)"-"); }
			else if ( !first ) TokenToLine((UBYTE *)"+");
			first = 0;
			if ( n != 1 || m[3] != 1 || m[4] != 1 || t == m + m[1] ) {
				fsym = 0; RatToLine((UWORD *)(m+3),n); }
			m += m[1];
			while ( m < t ) {
				if ( fsym == 0 ) {
					if ( m[2] < 0 ) TokenToLine((UBYTE *)"/");
					else TokenToLine((UBYTE *)"*");
				}
				else if ( m[2] < 0 ) TokenToLine((UBYTE *)"1/");
				fsym = 0;
				num = ( (LONG)(m[0]) << BITSINWORD ) + m[1];
				sprintf((char *)obuffer,"%s(%ld)",(char *)scratchname,num);
				TokenToLine(obuffer);
				if ( m[2] > 1 ) {
					sprintf((char *)obuffer,"**%d",m[2]);
					TokenToLine(obuffer);
				}
				else if ( m[2] < -1 ) {
					sprintf((char *)obuffer,"**%d",-m[2]);
					TokenToLine(obuffer);
				}
				m += 3;
			}
		}
		AO.OutSkip = oldskip;
		FiniLine();
nexti:;
	}
	for ( j = 1; j < totnum; j++ ) {
		if ( used[j] == 0 ) break;
	}
	if ( j >= totnum ) break;
	}
	AC.OutputMode = VORTRANMODE;
	AO.OutSkip = oldskip;
	M_free(used,"PrintOptima");
	return(0);
}

/*
  	#] PrintOptima:
  	#[ MaxPowerOpti:
*/

WORD MaxPowerOpti ARG1(LONG,number)
{
	SCALAR *sca = scabuffer + number;
	WORD pow = 0, *t, *m;
	t = sca->buffer;
	while ( t < sca->pointer ) {
		m = t + t[2] + 1;
		t += *t;
		while ( m < t ) {
			if ( m[2] > pow ) pow = m[2];
			else if ( -m[2] > pow ) pow = -m[2];
			m += 3;
		}
	}
	return(pow);
}

/*
  	#] MaxPowerOpti:
  	#[ HuntNumFactor:
*/

WORD HuntNumFactor ARG3(LONG,number,WORD *,coef,int,par)
{
	SCALAR *sca = scabuffer + number, *ss;
	WORD *t, *tt, *ttt, *m, *mm, *coef2, ncoef, n, nn, n1, n2, nt;
	int i;
	LONG numnewsca, a;
	if ( par != 1 ) { coef = AR.WorkPointer + 4; }
	t = sca->buffer;
	m = t + 4; i = t[2]; mm = coef; ncoef = t[3];
	NCOPY(mm,m,i);
	if ( ncoef < 0 ) ncoef = -ncoef;
	t += *t;
	while ( t < sca->pointer ) {
		if ( AccumGCD((UWORD *)coef,&ncoef,(UWORD *)(t+4),t[3]) ) goto ExitHunt;
		if ( ncoef == 3 && coef[0] == 1 && coef[1] == 1 ) return(0);
		t += *t;
	}
/*
	We have now the gcd in coef. First test for triviality:
*/
	if ( ncoef < 0 ) ncoef = -ncoef;
	if ( ncoef == 3 && coef[0] == 1 && coef[1] == 1 ) return(0);
/*
	Next we divide out the gcd. First flip it so we can use MulRat.
*/
	n = (ncoef-1)/2; t = coef; m = coef+n;
	for ( i = 0; i < n; i++ ) { nn = *t; *t++ = *m; *m++ = nn; }
	t = m = sca->buffer;
	coef2 = coef + ncoef;
	while ( t < sca->pointer ) {
		nt = *t; ttt = t + t[2] + 1;
		n1 = REDLENG(t[3]);
		mm = m+1; *mm++ = LNUMBER;
		if ( MulRat((UWORD *)(t+4),n1,(UWORD *)coef,n,(UWORD *)coef2,&n2) ) goto ExitHunt;
		i = INCLENG(n2);
		*mm++ = ABS(i)+2;
		*mm++ = i; if ( i < 0 ) i = -i; i--;
		tt = coef2; while ( --i >= 0 ) *mm++ = *tt++;
		t += nt;
		while ( ttt < t ) *mm++ = *ttt++;
		*m = mm-m; m = mm;
	}
	*m = 0;
	sca->pointer = m;
/*
	Now we make a flip. We put the buffer of this scalar in a new scalar.
	In the buffer of the new scalar we put the factor and the pointer to
	the now reduced term.
*/
	n = (ncoef-1)/2; t = coef; m = coef+n;
	for ( i = 0; i < n; i++ ) { nn = *t; *t++ = *m; *m++ = nn; }
	if ( par == 1 ) return(ncoef);

	numnewsca = scanumber;
	AR.WorkPointer[1] = LNUMBER;
	AR.WorkPointer[2] = ncoef+2;
	AR.WorkPointer[3] = ncoef;
	m = AR.WorkPointer + ncoef + 3;
	*m++ = (numnewsca+numobjects) >> BITSINWORD;
	*m++ = (numnewsca+numobjects) & WORDMASK;
	*m++ = 1;
	AR.WorkPointer[0] = m - AR.WorkPointer;
	AddToOpti(AR.WorkPointer,numnewsca);
/*
	Note: AddToOpti can move the scabuffer. Hence we need to redefine sca
*/
	ss = scabuffer + numnewsca;
	sca = scabuffer + number;
	m = sca->buffer; sca->buffer = ss->buffer; ss->buffer = m;
	m = sca->pointer; sca->pointer = ss->pointer; ss->pointer = m;
	m = sca->top; sca->top = ss->top; ss->top = m;
	a = sca->numterms; sca->numterms = ss->numterms; ss->numterms = a;
	a = sca->bufsize; sca->bufsize = ss->bufsize; ss->bufsize = a;

	return(ncoef);
ExitHunt:
	MesCall("HuntNumFactor");
	Terminate(-1);
	return(0);
}

/*
  	#] HuntNumFactor:
  	#[ HuntFactor:

	Hunts for factors in scalar 'number'
	The action depends of the value of 'par'
	par = 0   Symbolic and numeric factors. Extra sca is made.
	par = 1   Symbolic and numeric factors. Factor is divided out.
	par = 2   Symbolic and numeric factors. Factor is only determined.
	par = -1  Symbolic factors only. Factor is divided out.
	par = -2  Symbolic factors only. Factor is only determined.
	The factor is returned as a regular term in 'factor'
	In the case of par = 0, one can put 'factor' to zero.
	If an extra sca is made, the original sca and the new one are exchanged.
	The return value is the size of the factor, or zero if the factor is 1.
*/

WORD HuntFactor ARG3(LONG,number,WORD *,factor,int,par)
{
	SCALAR *sca, *scb;
	WORD *t, *m, *ft, *fm, *fr, *frr, *fact, *coef, ncoef, retval;
	int i, size;
	LONG newnum, a;
	if ( factor == 0 && par != 0 ) {
		MesPrint("Internal error: wrong value (%d) of par in HuntFactor",
				(WORD)par);
		Terminate(-1);
	}
	if ( par >= 0 ) {	/* We need the coefficient also */
		if ( factor == 0 ) factor = AR.WorkPointer;
		coef = factor + 4;
		ncoef = HuntNumFactor(number,coef,1);
		if ( ncoef == 0 || ( ncoef == 3 && coef[0] == 1
					&& coef[1] == 1 ) ) {
			retval = 0; ncoef = 3; coef[0] = coef[1] = 1; }
		else retval = ABS(ncoef)+3;
	}
	else {
		coef = factor + 4; ncoef = 3; coef[0] = coef[1] = 1;
		retval = 6;
	}
	factor[1] = LNUMBER; factor[2] = ABS(ncoef)+2; factor[3] = ncoef;
	factor[0] = factor[2]+1;
	fact = factor + factor[0];
	sca = scabuffer+number;
	t = sca->buffer;
	m = t + t[2] + 1; t += *t;
	fm = fact;
	while ( m < t ) *fm++ = *m++;
	ft = fm; size = ft - fact;
	if ( size == 0 ) {	/* no extra factor */
		return(retval);
	}
	while ( t < sca->pointer ) {
		m = t + t[2] + 1; t += *t;
		fm = fact;
		while ( fm < ft && m < t ) {
			if ( fm[0] == m[0] && fm[1] == m[1] ) {
				if ( fm[2] == m[2] ) { m += 3; fm += 3; }
				else if ( ( fm[2] > 0 && m[2] < 0 )
				|| ( fm[2] < 0 && m[2] > 0 ) ) goto loosethis;
				else if ( ( m[2] > 0 && fm[2] > m[2] )
				|| ( m[2] < 0 && fm[2] < m[2] ) ) {
					fm[2] = m[2]; m += 3; fm += 3;
				}
				else { m += 3; fm += 3; }
			}
			else if ( fm[0] < m[0] || ( fm[0] == m[0] && fm[1] < m[1] ) ) {
loosethis:		fr = fm + 3; frr = fm;
				while ( fr < ft ) *frr++ = *fr++;
				ft = frr; size -= 3;
				if ( size == 0 ) {	/* no symbolic factor left */
					return(retval);
				}
				m += 3;
			}
			else m += 3;
		}
		if ( fm < ft ) {
			ft = fm; size = ft - fact;
			if ( size == 0 ) {	/* no symbolic factor left */
				return(retval);
			}
		}
	}
/*
	We have a factor now. We can divide it out if the parameter asks for it.
*/
	factor[0] += size;
	size = factor[0];
	if ( par > 1 || par < -1 ) return(size);
	fr = t = sca->buffer;
	while ( t < sca->pointer ) {
		frr = fr; i = t[2]+1; m = t; t += *t;
		while ( --i >= 0 ) *fr++ = *m++;	/* coefficient */
		fm = fact;
		while ( fm < ft && m < t ) {
			if ( fm[0] == m[0] && fm[1] == m[1] ) {
				if ( fm[2] == m[2] ) { m += 3; fm += 3; }
				else {
					*fr++ = *m++; *fr++ = *m++; *fr++ = *m++ - fm[2];
					fm += 3;
				}
			}
			else { *fr++ = *m++; *fr++ = *m++; *fr++ = *m++; }
		}
		while ( m < t ) *fr++ = *m++;
		*frr = fr - frr;
		NormOpti(frr);
	}
	sca->pointer = fr;
	if ( par == 1 || par == -1 ) return(size);
/*
	Now we create a term with the factor and put it as a scalar.
	We have to exchange the order of the scalars!
*/
	t = AR.WorkPointer;
	*t++ = size + 9; *t++ = LNUMBER; *t++ = 5; *t++ = 3; *t++ = 1; *t++ = 1;
	t += size;
	*t++ = ( scanumber + numobjects ) >> BITSINWORD;
	*t++ = ( scanumber + numobjects ) & WORDMASK;
	*t++ = 1;
	newnum = scanumber;
	AddToOpti(AR.WorkPointer,newnum);
	sca = scabuffer + number;
	scb = scabuffer + newnum;
	t = sca->buffer; sca->buffer = scb->buffer; scb->buffer = t;
	t = sca->pointer; sca->pointer = scb->pointer; scb->pointer = t;
	t = sca->top; sca->top = scb->top; scb->top = t;
	a = sca->numterms; sca->numterms = scb->numterms; scb->numterms = a;
	a = sca->bufsize; sca->bufsize = scb->bufsize; scb->bufsize = a;
	return(size);
}

/*
  	#] HuntFactor:
  	#[ HuntPairs:

	Routine looks for an object (x) to a power of at least 'power'.
	It looks then for terms with are similar except for x^power.
	It will also try to look for (x+y)^power.
	This is more complicated than it looks because y can involve the
	coefficient of the term.
*/

void HuntPairs ARG2(LONG,number,WORD,power)
{
	SCALAR *sca = scabuffer + number;
	WORD *t, *tt, *m, *mm, *w, *w1, *pattern, *p, *pp, *patstop,
		 *coef, ncoef, nf, nc2, nc, *newter;
	int patsize, i, first, pushback = 0, nons, action = 0;
	LONG numnewsca = 0, ns;
	pattern = AR.WorkPointer;
	t = sca->buffer;
	while ( t < sca->pointer ) {
		if ( *t < 0 ) { t -= *t; continue; }
		w1 = t; m = t + 1 + t[2]; t += *t;
		while ( m < t ) {
			if ( m[2] >= power || -m[2] >= power ) {
				w = w1 + 1 + w1[2];
				p = pattern;
				while ( w < t ) {
					if ( w == m ) {
						if ( power == m[2] ) { w += 3; }
						else if ( power == -m[2] ) { w += 3; }
						else { *p++ = *w++; *p++ = *w++;
							if ( m[2] < 0 ) *p++ = *w++ + power;
							else            *p++ = *w++ - power;
						}
					}
					else { *p++ = *w++; *p++ = *w++; *p++ = *w++; }
				}
				patsize = p - pattern;
				if ( patsize == 0 ) { pushback++; goto nextm; }
				AR.WorkPointer = patstop = p;
				first = 1;
/*
				Now collect all terms which contain this pattern and put
				them into a new subexpression. Do this however only
				once there are at least two terms!
*/
				tt = sca->buffer;
				while ( tt < sca->pointer ) {
					if ( *tt < 0 ) { tt -= *tt; continue; }
					w = tt; mm = tt + tt[2] + 1; tt += *tt;
					if ( w == w1 ) continue;
					p = pattern; pp = AR.WorkPointer + w[2] + 1;
					while ( p < patstop && mm < tt ) {
						if ( mm[0] == p[0] && mm[1] == p[1] ) {
							if ( ( p[2] > 0 && mm[2] >= p[2] )
							|| ( p[2] < 0 && mm[2] <= p[2] ) ) {
								if ( mm[2] == p[2] ) { mm += 3; p += 3; }
								else {
									*pp++ = *mm++; *pp++ = *mm++;
									*pp++ = *mm++ - p[2];
									p += 3;
								}
							}
							else goto nexttt;
						}
						else { *pp++ = *mm++; *pp++ = *mm++; *pp++ = *mm++; }
					}
					if ( p >= patstop ) {	/* match! */
						while ( mm < tt ) *pp++ = *mm++;
						p = AR.WorkPointer; mm = w+1; i = mm[1];
						*p++ = pp - AR.WorkPointer;
						while ( --i >= 0 ) *p++ = *mm++;
						if ( first ) {
							p = pp+1; mm = w1+1; i = mm[1];
							while ( --i >= 0 ) *p++ = *mm++;
							*p++ = m[0]; *p++ = m[1];
							if ( m[2] < 0 ) *p++ = -power;
							else            *p++ = power;
							*pp = p - pp;
							NormOpti(pp);
							numnewsca = scanumber;
							AddToOpti(pp,numnewsca);
							sca = scabuffer + number;
							first = 0;
							*w1 = -*w1;
						}
						NormOpti(AR.WorkPointer);
						AddToOpti(AR.WorkPointer,numnewsca);
						*w = -*w;
						sca->numterms--;
					}
nexttt:;
				}
				if ( *w1 > 0 ) { /* no second term! */
					goto nextm;
				}
				action++;
/*
				First simplify the expression
*/
				nf = HuntFactor(numnewsca,AR.WorkPointer,1);
				SortOpti(numnewsca);
/*
				Check whether we have this scalar already.
				The factor could still be a problem.
*/
				coef = AR.WorkPointer + 4 + nf;
				ns = TestNewSca(numnewsca,coef,&ncoef);
				if ( ns != numnewsca ) {
					nons = 1;
					scanumber--;
				}
				else nons = 0;
/*
				See whether we have to combine things
				After this we only look for nf != 0
*/
				if ( nons ) {
					if ( nf ) {
						newter = coef + 2*ABS(ncoef) + 2;
						ncoef = REDLENG(ncoef);
						nc2 = REDLENG(AR.WorkPointer[3]);
						if ( MulRat((UWORD *)coef,ncoef,(UWORD *)(AR.WorkPointer+4)
						,nc2,(UWORD *)(newter+4),&nc) ) {
							MesCall("HuntPairs");
							Terminate(-1);
						}
						nc = INCLENG(nc);
						p = AR.WorkPointer + AR.WorkPointer[2] + 1;
						t = AR.WorkPointer + nf;
						newter[2] = ABS(nc) + 2;
						newter[3] = nc;
						newter[1] = LNUMBER;
						m = newter + newter[2] + 1;
						while ( p < t ) *m++ = *p++;
						nf = i = newter[0] = m-newter;
						m = newter; t = AR.WorkPointer;
						while ( --i >= 0 ) *t++ = *m++;
					}
					else {
						nf = ABS(ncoef);
						AR.WorkPointer[0] = nf + 3;
						AR.WorkPointer[1] = LNUMBER;
						AR.WorkPointer[2] = nf + 2;
						AR.WorkPointer[3] = ncoef;
						nf += 3;
					}
				}
/*
				Compress the buffer and continue from the next term
*/
				m = t = sca->buffer; p = AR.WorkPointer + 1;
				while ( t < sca->pointer ) {
					if ( *t < 0 ) {
						if ( t == w1 ) {
							w = m;
							if ( nf == 0 ) {
								*p++ = LNUMBER; *p++ = 5; *p++ = 3; *p++ = 1;
								*p++ = 1;
							}
							else {
								p = AR.WorkPointer + nf;
							}
							mm = pattern;
							while ( mm < patstop ) *p++ = *mm++;
							*p++ = (ns+numobjects) >> BITSINWORD;
							*p++ = (ns+numobjects) & WORDMASK;
							*p++ = 1;
							*(AR.WorkPointer) = p - AR.WorkPointer;
							NormOpti(AR.WorkPointer);
						}
						t -= *t;
					}
					else if ( t > m ) {
						i = *t; while ( --i >= 0 ) *m++ = *t++;
					}
					else { t += *t; m += *m; }
				}
				i = *(AR.WorkPointer); p = AR.WorkPointer;
				while ( --i >= 0 ) *m++ = *p++;
				sca->pointer = m; *m = 0;
				t = w;
				goto nextterm;
			}
nextm:
			m += 3;
		}
nextterm:;
	}
	if ( action ) SortOpti(number);
/*
	Next we have to look in the new scalars for things of the type
	y^power + power*a*y^(power-1) + ... + a^power
	We trigger on three terms.
	We do this term by term and try to accumulate powers ie:
	(a+b+c)^3 = a^3+b^3+c^3+3*a^2*b+3*a^2*c+3*a*b^2+3*a*c^2+3*b^2*c+3*b*c^2
	+6*a*b*c -> (a^3+3*a^2*b  +b^3)
	 -> ab^3+c^3+3*a^2*c+3*a*c^2+3*b^2*c+3*b*c^2+6*a*b*c;
	Next try to find more ab combinations (in squares etc) ->
	ab^3+c^3+3*ab^2*c+3*ab*c^2
	and then we start from the beginning which gives directly
	abc^3
	If an object simplifies completely, we adjust the power where it is
	used so that more tricks will be possible later.
*/
	AR.WorkPointer = pattern;
}

/*
  	#] HuntPairs:
  	#[ HuntBrackets:

	Routine goes through a sca and tries to find the most popular object.
	Then breaks up the expression in the style a*x+b.
	It keeps doing this till there are no more objects like this.
*/

void HuntBrackets ARG1(LONG,number)
{
	SCALAR *sca = scabuffer + number;
	WORD *t, *m, *tt, *mm, *left = 0, mostpopular[2], *coef, nf, *newter;
	WORD ncoef, nc2, nc;
	LONG mostmultiple, n, i, num, newscanum, ns;
	int first, j, neg, nons;
	for (;;) {
	if ( sca->numterms <= 2 ) return;
	n = scanumber + numobjects;
	while ( 2*n >= multiplicitysize ) {
		if ( multiplicitysize == 0 ) multiplicitysize = 500;
		else multiplicitysize *= 2;
		if ( multiplicities ) M_free(multiplicities,"multiplicities");
		multiplicities = (LONG *)Malloc1(multiplicitysize*sizeof(LONG),"multiplicities");
	}
	for ( i = 0; i <= n; i++ ) {
		multiplicities[i] = 0; multiplicities[i+n] = 0;
	}
	t = sca->buffer;
	while ( t < sca->pointer ) {
		m = t + t[2] + 1; t += *t;
		while ( m < t ) {
			num = ( (LONG)(m[0]) << BITSINWORD ) + m[1];
			if ( m[2] > 0 ) multiplicities[num]++;
			else multiplicities[n+num]++;
			m += 3;
		}
	}
	for ( i = 0, num = -1, mostmultiple = 0; i <= n; i++ ) {
		if ( multiplicities[i] > mostmultiple ) {
			mostmultiple = multiplicities[i]; num = i;
		}
		if ( multiplicities[i+n] > mostmultiple ) {
			mostmultiple = multiplicities[i+n]; num = i+n;
		}
	}
	if ( mostmultiple <= 1 ) return;
	if ( num > n ) { neg = 1; num -= n; }
	else neg = 0;
	mostpopular[0] = num >> BITSINWORD;
	mostpopular[1] = num & WORDMASK;
/*
	Now all terms containing num will be taken out ( with one power of num less )
	and sent off to a new expression.
	First find the first to start the system.
*/
	t = sca->buffer; newscanum = scanumber; first = 1;
	while ( t < sca->pointer ) {
		m = t + t[2] + 1; tt = t; t += *t;
		while ( m < t ) {
			if ( m[0] == mostpopular[0] && m[1] == mostpopular[1] ) {
				if ( ( neg && m[2] > 0 ) || ( !neg && m[2] < 0 ) ) break;
				mm = AR.WorkPointer;
				left = tt;
				while ( tt < m ) *mm++ = *tt++;
				if ( neg ) {
					if ( m[2] < -1 ) {
						*mm++ = *tt++; *mm++ = *tt++; *mm++ = 1 + *tt++;
					}
					else tt += 3;
				}
				else {
					if ( m[2] > 1 ) {
						*mm++ = *tt++; *mm++ = *tt++; *mm++ = *tt++ - 1;
					}
					else tt += 3;
				}
				while ( tt < t ) *mm++ = *tt++;
				AR.WorkPointer[0] = mm - AR.WorkPointer;
				sca->numterms--;
				AddToOpti(AR.WorkPointer,newscanum);
				goto dorest;
			}
			m += 3;
		}
	}
dorest:
	sca = scabuffer+number;
	while ( t < sca->pointer ) {
		m = t + t[2] + 1; tt = t; t += *t;
		while ( m < t ) {
			if ( m[0] == mostpopular[0] && m[1] == mostpopular[1] ) {
				if ( ( neg && m[2] > 0 ) || ( !neg && m[2] < 0 ) ) break;
				mm = AR.WorkPointer;
				while ( tt < m ) *mm++ = *tt++;
				if ( neg ) {
					if ( m[2] < -1 ) {
						*mm++ = *tt++; *mm++ = *tt++; *mm++ = 1 + *tt++;
					}
					else tt += 3;
				}
				else {
					if ( m[2] > 1 ) {
						*mm++ = *tt++; *mm++ = *tt++; *mm++ = *tt++ - 1;
					}
					else tt += 3;
				}
				while ( tt < t ) *mm++ = *tt++;
				AR.WorkPointer[0] = mm - AR.WorkPointer;
				sca->numterms--;
				AddToOpti(AR.WorkPointer,newscanum);
				goto nextt;
			}
			m += 3;
		}
		while ( tt < t ) *left++ = *tt++;	/* leftovers */
nextt:;
	}
/*
	Now the remaining term contains the popular factor, the new sca
	and possibly factors from the new sca that we can take out.
	First find the factors.
*/
	nf = HuntFactor(newscanum,AR.WorkPointer,1);
	SortOpti(newscanum);
/*
	Check whether we have this scalar already.
	The factor could still be a problem.
*/
	coef = AR.WorkPointer + 4 + nf;
	ns = TestNewSca(newscanum,coef,&ncoef);
	if ( ns != newscanum ) {
		nons = 1;
		scanumber--;
	}
	else nons = 0;
/*
	See whether we have to combine things
	After this we only look for nf != 0
*/
	if ( nons ) {
		if ( nf ) {
			newter = coef + 2*ABS(ncoef) + 2;
			ncoef = REDLENG(ncoef);
			nc2 = REDLENG(AR.WorkPointer[3]);
			if ( MulRat((UWORD *)coef,ncoef,(UWORD *)(AR.WorkPointer+4),nc2
			,(UWORD *)(newter+4),&nc) ) {
				MesCall("HuntPairs");
				Terminate(-1);
			}
			nc = INCLENG(nc);
			tt = AR.WorkPointer + AR.WorkPointer[2] + 1;
			t = AR.WorkPointer + nf;
			newter[1] = LNUMBER;
			newter[2] = ABS(nc) + 2;
			newter[3] = nc;
			m = newter + newter[2] + 1;
			while ( tt < t ) *m++ = *tt++;
			nf = j = newter[0] = m-newter;
			m = newter; t = AR.WorkPointer;
			while ( --j >= 0 ) *t++ = *m++;
		}
		else {
			nf = ABS(ncoef);
			AR.WorkPointer[0] = nf + 3;
			AR.WorkPointer[1] = LNUMBER;
			AR.WorkPointer[2] = nf + 2;
			AR.WorkPointer[3] = ncoef;
			nf += 3;
		}
	}
	if ( nf == 0 ) {
		m = AR.WorkPointer + 1;
		*m++ = LNUMBER; *m++ = 5; *m++ = 3; *m++ = 1; *m++ = 1;
		*m++ = mostpopular[0];
		*m++ = mostpopular[1];
		if ( neg ) *m++ = -1;
		else *m++ = 1;
	}
	else {
		t = AR.WorkPointer + nf;
		m = AR.WorkPointer + AR.WorkPointer[2] + 1;
		while ( m < t ) {
			if ( m[0] == mostpopular[0] && m[1] == mostpopular[1] ) {
				if ( neg ) m[2]--;
				else       m[2]++;
				break;
			}
			m += 3;
		}
		if ( m >= t ) {
			*m++ = mostpopular[0];
			*m++ = mostpopular[1];
			if ( neg ) *m++ = -1;
			else *m++ = 1;
		}
		else m = t;
	}
	*m++ = (ns+numobjects) >> BITSINWORD;
	*m++ = (ns+numobjects) & WORDMASK;
	*m++ = 1;
	*(AR.WorkPointer) = m - AR.WorkPointer;
	NormOpti(AR.WorkPointer);
	j = *(AR.WorkPointer); m = AR.WorkPointer;
	while ( --j >= 0 ) *left++ = *m++;
	sca->pointer = left; *left = 0;
	sca->numterms++;
	SortOpti(number);
	}
}

/*
  	#] HuntBrackets:
  	#[ HuntNumBrackets:

	Hunts for terms with an identical coefficient
	Makes a bracket of them. in the style of 2*(a+b)+c.
*/

void HuntNumBrackets ARG1(LONG,number)
{
}

/*
  	#] HuntNumBrackets:
  	#[ HuntPowers:

	Tries to look for something of the type
	a^n + n*a^(n-1)*b + b^n
	This pattern will trigger the substitution
	a^n + n*a^(n-1)*b + b^n = (a+b)^n - rest
	a+b will become a new sca
*/

void HuntPowers ARG2(LONG,number,WORD,power)
{
	SCALAR *sca = scabuffer + number;
	WORD *t1, *m1, *r1, *t2, *m2, *r2, *t3, *m3, *r3, *quotient, *extra
	    , *q, n1, n2, n3, nq;
	int i;
	quotient = AR.WorkPointer;
	t1 = sca->buffer;
	while ( t1 < sca->pointer ) {
		m1 = t1 + t1[2] + 1; r1 = t1; t1 += *t1;
		while ( m1 < t1 ) {
			if ( m1[2] >= power ) {
/*
				Now we have to find a second term with the property
				that term1/term2 is a clean power (like a^n/b^n)
*/
				n1 = REDLENG(r1[3]); t2 = t1; m1 = r1 + r1[2] + 1;
				while ( t2 < sca->pointer ) {
					m2 = t2 + t2[2] + 1; r2 = t2; t2 += *t2;
					n2 = REDLENG(r2[3]);
					if ( DivRat((UWORD *)(r2+4),n2,(UWORD *)(r1+4),n1,
					(UWORD *)(quotient+4),&nq) ) goto callHP;
					if ( TakeRatRoot((UWORD *)(quotient+4),&nq,power) ) continue;
					n2 = INCLENG(nq);
					quotient[2] = ABS(n2)+2; quotient[3] = n2;
					quotient[1] = LNUMBER; q = quotient + quotient[2] + 1;
					while ( m2 < t2 && m1 < t1 ) {
						if ( *m1 == *m2 && m1[1] == m2[1] ) {
							if ( ((m2[2]-m1[2])%power) != 0 ) goto nextt2;
							*q++ = *m1++; *q++ = *m1++;
							*q++ = (m2[2]-*m1++)/power;
							m2 += 3;
						}
						else if ( *m1 < *m2 || ( *m1 == *m2 && m1[1] < m2[1] ) ) {
							if ( ( m1[2] % power ) != 0 ) goto nextt2;
							*q++ = *m1++; *q++ = *m1++; *q++ = -*m1++ / power;
						}
						else {
							if ( ( m2[2] % power ) != 0 ) goto nextt2;
							*q++ = *m2++; *q++ = *m2++; *q++ = *m2++ / power;
						}
					}
					while ( m1 < t1 ) {
						if ( ( m1[2] % power ) != 0 ) goto nextt2;
						*q++ = *m1++; *q++ = *m1++; *q++ = -*m1++ / power;
					}
					while ( m2 < t2 ) {
						if ( ( m2[2] % power ) != 0 ) goto nextt2;
						*q++ = *m2++; *q++ = *m2++; *q++ = *m2++ / power;
					}
					quotient[0] = q - quotient;
/*
					Now we have the a^power and the b^power
					+/- a/b is in quotient
					if the coefficient is negative it has to be negative.
					if it is positive and power	is odd it has to be positive.
					otherwise it can be either positive or negative.			
					Construct now the third term.
					It is number 1 divided by quotient times the power.
*/
					extra = quotient + quotient[0];
					if ( DivRat((UWORD *)(r1+4),n1,(UWORD *)(quotient+4),nq
						,(UWORD *)(extra+4),&n3) ) goto callHP;
					if ( Mully((UWORD *)(extra+4),&n3,(UWORD *)(&power),1) ) goto callHP;
					n2 = INCLENG(n3);
					extra[1] = LNUMBER; extra[2] = ABS(n2)+2; extra[3] = n2;
					m3 = extra + extra[2] + 1;
					m1 = r1 + r1[2] + 1;
					m2 = quotient + quotient[2] + 1;
					while ( m1 < t1 && m2 < q ) {
						if ( m1[0] == m2[0] && m1[1] == m2[1] ) {
							if ( m1[2] == m2[2] ) { m1+=3; }
							else {
								*m3++ = *m1++; *m3++ = *m1++; *m3++ = *m1++ - m2[2];
							}
							m2 += 3;
						}
						else if ( m1[0] < m2[0] || ( m1[0] == m2[0]
						&& m1[1] < m2[1] ) ) {
							*m3++ = *m1++; *m3++ = *m1++; *m3++ = *m1++;
						}
						else {
							*m3++ = *m2++; *m3++ = *m2++; *m3++ = *m2++;
						}
					}
					while ( m1 < t1 ) {
						*m3++ = *m1++; *m3++ = *m1++; *m3++ = *m1++;
					}
					while ( m2 < q ) {
						*m3++ = *m2++; *m3++ = *m2++; *m3++ = *m2++;
					}
					extra[0] = m3 - extra;
/*
					Now look for +/- extra
*/
					t3 = sca->buffer;
					while ( t3 < sca->pointer ) {
						r3 = t3; t3 += *t3;
						if ( *r3 != *extra ) continue;
						if ( r3[2] != extra[2] ) continue;
						for ( i = 4; i < *r3; i++ ) {
							if ( extra[i] != r3[i] ) break;
						}
						if ( i < *r3 ) continue;
/*
						Now we can finally clear up the sign.
*/
						if ( r3[3] < 0 ) {
							if ( ( power & 1 ) != 0 && quotient[3] > 0 ) goto nextt2;
							if ( quotient[3] > 0 ) quotient[3] = -quotient[3]; 
						}
						else if ( quotient[3] < 0 ) goto nextt2;
/*
						We have everything now
*/

					}
nextt2:;
				}
			}
			m1 += 3;
		}
	}
	return;
callHP:
	MesCall("HuntPowers");
	Terminate(-1);
	return;
}

/*
  	#] HuntPowers:
  	#[ CombiOpti:

	We try to express the bigger sca's into smaller sca's
*/

void CombiOpti ARG0
{
	LONG i1, i2;
	SCALAR *sca1, *sca2;
	WORD *t1, *m1, *tt1, *t2, *m2, *tt2, *fill;
	int sign = 1, j;
	for ( i1 = 0; i1 < scanumber; i1++ ) {
		sca1 = scabuffer + i1;
		for ( i2 = 0; i2 < scanumber; i2++ ) {
			sca2 = scabuffer + i2;
			if ( sca2->numterms <= sca1->numterms ) continue;
			t1 = sca1->buffer; t2 = sca2->buffer;
			tt1 = t1; t1 += *t1;
/*
			First find the first term and determine the factor
*/
			while ( t2 < sca2->pointer ) {
				if ( *tt1 != *t2 ) { t2 += *t2; continue; }
				m1 = tt1 + tt1[2] + 1;
				m2 = t2 + t2[2] + 1; tt2 = t2; t2 += *t2;
				while ( m1 < t1 && m2 < t2 ) {
					if ( *m1 != *m2 || m1[1] != m2[1] || m1[2] != m2[2] ) break;
					m1 += 3; m2 += 3;
				}
				if ( m1 >= t1 && m2 >= t2 ) { /* we do have a match */
/*
					For now we tolerate just a sign
*/
					if ( tt1[2] != tt2[2] ) goto nexti2;
					if ( tt1[3] > 0 ) {
						if ( tt2[3] > 0 ) sign = 1;
						else sign = -1;
					}
					else {
						if ( tt2[3] < 0 ) sign = 1;
						else sign = -1;
					}
					m1 = tt1 + tt1[2] + 1;
					tt2 += 4; tt1 += 4;
					while ( tt1 < m1 ) {
						if ( *tt1 != *tt2 ) goto nexti2;
						tt1++; tt2++;
					}
					break;
				}
			}
			if ( t2 >= sca2->pointer ) goto nexti2;
			while ( t1 < sca1->pointer && t2 < sca2->pointer ) {
				if ( *t1 != *t2 ) { t2 += *t2; continue; }
				tt1 = t1 + *t1; m1 = t1 + t1[2] + 1;
				tt2 = t2 + *t2; m2 = t2 + t2[2] + 1;
				while ( m1 < tt1 && m2 < tt2 ) {
					if ( *m1 != *m2 || m1[1] != m2[1] || m1[2] != m2[2] ) break;
					m1 += 3; m2 += 3;
				}
				if ( m1 >= tt1 && m2 >= tt2 ) {
					if ( t1[2] != t2[2] ) goto nexti2;
					if ( sign*t1[3] != t2[3] ) goto nexti2;
					m1 = t1 + t1[2] + 1; t1 += 4; t2 += 4;
					while ( t1 < m1 ) {
						if ( *t1 != *t2 ) goto nexti2;
						t1++; t2++;
					}
					t1 = tt1; t2 = tt2;
				}
				else t2 = tt2;
			}
			if ( t1 < sca1->pointer ) goto nexti2;
/*
			Now we made it. We have to remove the terms and add the new term.
*/
			t1 = sca1->buffer; fill = t2 = sca2->buffer;
			sca2->numterms = 0;
			while ( t1 < sca1->pointer && t2 < sca2->pointer ) {
				if ( *t1 != *t2 ) { t2 += *t2; continue; }
				m1 = t1 + t1[2] + 1; tt1 = t1 + *t1;
				m2 = t2 + t2[2] + 1; tt2 = t2 + *t2;
				while ( m1 < tt1 && m2 < tt2 ) {
					if ( *m1 != *m2 || m1[1] != m2[1] || m1[2] != m2[2] ) break;
					m1 += 3; m2 += 3;
				}
				if ( m1 >= tt1 && m2 >= tt2 ) {
					t1 = tt1; t2 = tt2;
				}
				else {
					while ( t2 < tt2 ) *fill++ = *t2++;
					sca2->numterms++;
				}
			}
			while ( t2 < sca2->pointer ) {
				j = *t2; while ( --j >= 0 ) *fill++ = *t2++;
				sca2->numterms++;
			}
			*fill++ = 9; *fill++ = LNUMBER; *fill++ = 5;
			*fill++ = sign*3; *fill++ = 1; *fill++ = 1;
			*fill++ = ( i1+numobjects ) >> BITSINWORD;
			*fill++ = ( i1+numobjects ) & WORDMASK; *fill++ = 1;
			*fill = 0;
			sca2->numterms++;
			sca2->pointer = fill;
			SortOpti(i2);
nexti2:;
		}
	}
}

/*
  	#] CombiOpti:
  	#[ TestNewSca:
*/

LONG TestNewSca ARG3(LONG,number,WORD *,coef,WORD *,ncoef)
{
	SCALAR *sca = scabuffer + number, *s;
	WORD *t1, *t2, *m1, *m2, *coef2, ncoef2, n1, n2;
	int no;
	LONG i, ii;
	for ( i = 0, s = scabuffer; i < scanumber; i++, s++ ) {
		if ( i == number ) continue;
		if ( sca->numterms != s->numterms ) continue;
/*
		Test 1: equal up to coefficient
*/
		t1 = sca->buffer;
		t2 = s->buffer;
		no = 0;
		while ( t1 < sca->pointer && t2 < s->pointer ) {
			m1 = t1 + t1[2] + 1; t1 += *t1;
	    	m2 = t2 + t2[2] + 1; t2 += *t2;
			while ( m1 < t1 && m2 < t2 ) {
				if ( *m1 != *m2 || m1[1] != m2[1] || m1[2] != m2[2] ) {
					no = 1; break; }
				m1 += 3; m2 += 3;
			}
			if ( m1 < t1 || m2 < t2 ) { no = 1; break; }
		}
		if ( !no ) {
/*
			Now we have to be more precise with the coefficients
*/
			t1 = sca->buffer;
			t2 = s->buffer;
			n1 = REDLENG(t1[3]); n2 = REDLENG(t2[3]);
			DivRat((UWORD *)(t1+4),n1,(UWORD *)(t2+4),n2,(UWORD *)coef,ncoef);
			t1 += *t1; t2 += *t2;
			coef2 = coef + 2*ABS(*ncoef)+2;
			while ( t1 < sca->pointer && t2 < s->pointer ) {
				n1 = REDLENG(t1[3]); n2 = REDLENG(t2[3]);
				DivRat((UWORD *)(t1+4),n1,(UWORD *)(t2+4),n2,(UWORD *)coef2,&ncoef2);
				if ( *ncoef != ncoef2 ) break;
				ii = 2*ABS(*ncoef);
				while ( --ii >= 0 ) {
					if ( coef[ii] != coef2[ii] ) break;
				}
				if ( ii >= 0 ) break;
				t1 += *t1; t2 += *t2;
			}
			if ( t1 >= sca->pointer && t2 >= s->pointer ) {
/*
				There is a match! And we can keep the factor.
*/
				sca->pointer = sca->buffer;
				sca->numterms = 0;
				*ncoef = INCLENG(*ncoef);
				return(i);
			}
		}
	}
	return(number);
}

/*
  	#] TestNewSca:
  	#[ NormOpti:
*/

void NormOpti ARG1(WORD *,term)
{
	WORD *t, *m, *w, *tt, a;
	tt = m = term + term[2] + 4; t = term + *term;
	while ( m < t ) {
		w = m;
		while ( w >= tt && ( w[0] < w[-3]
		 || ( w[0] == w[-3] && w[1] < w[-2] ) ) ) {
			a = w[0]; w[0] = w[-3]; w[-3] = a;
			a = w[1]; w[1] = w[-2]; w[-2] = a;
			a = w[2]; w[2] = w[-1]; w[-1] = a;
			w -= 3;
		}
		m += 3;
	}
}

/*
  	#] NormOpti:
  	#[ SortOpti:
*/

void SortOpti ARG1(LONG,number)
{
	SCALAR *sca = scabuffer + number;
	WORD *newbuffer, *t, *m, **p, j;
	LONG i, newsize, num = 0;
	if ( sca->numterms <= 1 ) return;
	if ( (sca->numterms+sca->numterms/2) > sizepointers ) {
		if ( sortpointers ) M_free(sortpointers,"optisort");
		sizepointers = sca->numterms + sca->numterms/2 + 10;
		sortpointers = (WORD **)Malloc1(sizepointers*sizeof(WORD *),"optisort");
	}
	p = sortpointers;
	t = sca->buffer;
	while ( t < sca->pointer ) { *p++ = t; t += *t; num++; }
	if ( num != sca->numterms ) {
		MesPrint("Help! sca->numterms = %l, actual number = %l",
			sca->numterms, num);
	}
	helppointers = p;
	SplitOpti(sortpointers,num);
	newsize = ( sca->pointer - sca->buffer ) + 2;
	newbuffer = (WORD *)Malloc1(newsize*sizeof(WORD),"optisort2");
	m = newbuffer;
	for ( i = 0; i < num; i++ ) {
		t = sortpointers[i]; j = *t;
		while ( --j >= 0 ) *m++ = *t++;
	}
	sca->numterms = num;
	*m = 0;
	M_free(sca->buffer,"optisort2");
	sca->buffer = newbuffer; sca->pointer = m; sca->top = sca->buffer + newsize;
	sca->bufsize = newsize;
}

/*
  	#] SortOpti:
  	#[ SplitOpti:

	SplitMerge for the SortOpti routine.
*/

void SplitOpti ARG2(WORD **,pointers,LONG,number)
{
	WORD *t1, *t2, *m1, *m2, **p;
	int n;
	LONG left, right, i, j;
	if ( number <= 1 ) return;
	if ( number == 2 ) {
		t1 = pointers[0]; t2 = pointers[1];
		m1 = t1 + t1[2] + 1; m2 = t2 + t2[2] + 1;
		t1 += *t1; t2 += *t2;
		n = 0;
		while ( m1 < t1 && m2 < t2 ) {
			if ( m1[0] > m2[0] ) { n = 1; break; }
			else if ( m1[0] < m2[0] ) return;
			if ( m1[1] > m2[1] ) { n = 1; break; }
			else if ( m1[1] < m2[1] ) return;
			if ( m1[2] < m2[2] ) { n = 1; break; }
			else if ( m1[2] > m2[2] ) return;
			m1 += 3; m2 += 3;
		}
		if ( n > 0 || m1 < t1 ) {
			t1 = pointers[0]; pointers[0] = pointers[1]; pointers[1] = t1;
		}
		return;
	}
	left = number / 2;
	right = number - left;
	SplitOpti(pointers,left);
	SplitOpti(pointers+left,right);
	for ( i = 0; i < left; i++ ) helppointers[i] = pointers[i];
	i = 0; j = left; p = pointers;
	while ( i < left && j < number ) {
		t1 = helppointers[i]; t2 = pointers[j];
		m1 = t1 + t1[2] + 1; m2 = t2 + t2[2] + 1;
		t1 += *t1; t2 += *t2;
		n = 0;
		while ( m1 < t1 && m2 < t2 ) {
			if ( m1[0] > m2[0] ) { n = 1; break; }
			else if ( m1[0] < m2[0] ) { n = -1; break; }
			if ( m1[1] > m2[1] ) { n = 1; break; }
			else if ( m1[1] < m2[1] ) { n = -1; break; }
			if ( m1[2] < m2[2] ) { n = 1; break; }
			else if ( m1[2] > m2[2] ) { n = -1; break; }
			m1 += 3; m2 += 3;
		}
		if ( n > 0 || ( n == 0 && m1 < t1 ) ) { *p++ = pointers[j++]; }
		else *p++ = helppointers[i++];
	}
	while ( i < left ) *p++ = helppointers[i++];
}

/*
  	#] SplitOpti:
*/

