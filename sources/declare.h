#ifndef __FDECLARE__
#define __FDECLARE__

/** @file declare.h
 *
 *  Contains macros and function declarations.
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
  	#[ Macro's :
*/

#define MaX(x,y) ((x) > (y) ? (x): (y))
#define MiN(x,y) ((x) < (y) ? (x): (y))
#define ABS(x) ( (x) < 0 ? -(x): (x) )
#define SGN(x) ( (x) > 0 ? 1 : (x) < 0 ? -1 : 0 )
#define REDLENG(x) ((((x)<0)?((x)+1):((x)-1))/2)
#define INCLENG(x) (((x)<0)?(((x)*2)-1):(((x)*2)+1))
#define GETCOEF(x,y) x += *x;y = x[-1];x -= ABS(y);y=REDLENG(y)
#define GETSTOP(x,y) y=x+(*x)-1;y -= ABS(*y)-1
#define StuffAdd(x,y)  (((x)<0?-1:1)*(y)+((y)<0?-1:1)*(x))
 
#define EXCHN(t1,t2,n) { WORD a,i; for(i=0;i<n;i++){a=t1[i];t1[i]=t2[i];t2[i]=a;} }
#define EXCH(x,y) { WORD a = (x); (x) = (y); (y) = a; }
 
#define TOKENTOLINE(x,y) if ( AC.OutputSpaces == NOSPACEFORMAT ) { \
		TokenToLine((UBYTE *)(y)); } else { TokenToLine((UBYTE *)(x)); }

#define UngetFromStream(stream,c) ((stream)->nextchar[(stream)->isnextchar++]=c)
#ifdef WITHRETURN
#define AddLineFeed(s,n) { (s)[(n)++] = CARRIAGERETURN; (s)[(n)++] = LINEFEED; }
#else
#define AddLineFeed(s,n) { (s)[(n)++] = LINEFEED; }
#endif
#define TryRecover(x) Terminate(-1)
#define UngetChar(c) { pushbackchar = c; }
#define ParseNumber(x,s) {(x)=0;while(*(s)>='0'&&*(s)<='9')(x)=10*(x)+*(s)++ -'0';}
#define ParseSign(sgn,s) {(sgn)=0;while(*(s)=='-'||*(s)=='+'){\
          if ( *(s)++ == '-' ) sgn ^= 1;}}
#define ParseSignedNumber(x,s) { int sgn; ParseSign(sgn,s)\
          ParseNumber(x,s) if ( sgn ) x = -x; }

#define NCOPY(s,t,n) while ( --n >= 0 ) *s++ = *t++;

/*#define NCOPY(s,t,n) { memcpy(s,t,n*sizeof(WORD)); s+=n; t+=n; n = -1; }*/
#define NCOPYI(s,t,n) while ( --n >= 0 ) *s++ = *t++;
#define NCOPYB(s,t,n) while ( --n >= 0 ) *s++ = *t++;
#define NCOPYI32(s,t,n) while ( --n >= 0 ) *s++ = *t++;
#define WCOPY(s,t,n) { int nn=n; WORD *ss=(WORD *)s, *tt=(WORD *)t; while ( --nn >= 0 ) *ss++=*tt++; }
#define NeedNumber(x,s,err) { int sgn = 1;                               \
		while ( *s == ' ' || *s == '\t' || *s == '-' || *s == '+' ) {    \
			if ( *s == '-' ) sgn = -sgn; s++; }                          \
		if ( chartype[*s] != 1 ) goto err;                               \
		ParseNumber(x,s)                                                 \
		if ( sgn < 0 ) (x) = -(x); while ( *s == ' ' || *s == '\t' ) s++;\
	}
#define SKIPBLANKS(s) { while ( *(s) == ' ' || *(s) == '\t' ) (s)++; }
#define FLUSHCONSOLE if ( AP.InOutBuf > 0 ) CharOut(LINEFEED)

#define SKIPBRA1(s) { int lev1=0; s++; while(*s) { if(*s=='[')lev1++; \
					else if(*s==']'&&--lev1<0)break; s++;} }
#define SKIPBRA2(s) { int lev2=0; s++; while(*s) { if(*s=='{')lev2++; \
					else if(*s=='}'&&--lev2<0)break; \
					else if(*s=='[')SKIPBRA1(s) s++;} }
#define SKIPBRA3(s) { int lev3=0; s++; while(*s) { if(*s=='(')lev3++; \
					else if(*s==')'&&--lev3<0)break; \
					else if(*s=='{')SKIPBRA2(s) \
					else if(*s=='[')SKIPBRA1(s) s++;} }
#define SKIPBRA4(s) { int lev4=0; s++; while(*s) { if(*s=='(')lev4++; \
					else if(*s==')'&&--lev4<0)break; \
					else if(*s=='[')SKIPBRA1(s) s++;} }
#define SKIPBRA5(s) { int lev5=0; s++; while(*s) { if(*s=='{')lev5++; \
					else if(*s=='}'&&--lev5<0)break; \
					else if(*s=='(')SKIPBRA4(s) \
					else if(*s=='[')SKIPBRA1(s) s++;} }

/*
#define CYCLE1(a,i) {WORD iX,jX; iX=*a; for(jX=1;jX<i;jX++)a[jX-1]=a[jX]; a[i-1]=iX;}
*/
#define CYCLE1(t,a,i) {t iX=*a; WORD jX; for(jX=1;jX<i;jX++)a[jX-1]=a[jX]; a[i-1]=iX;}

#define AddToCB(c,wx) if(c->Pointer>=c->Top) \
		DoubleCbuffer(c-cbuf,c->Pointer,21); \
		*(c->Pointer)++ = wx;

#define EXCHINOUT { FILEHANDLE *ffFi = AR.outfile; \
	AR.outfile = AR.infile; AR.infile = ffFi; }
#define BACKINOUT { FILEHANDLE *ffFi = AR.outfile; POSITION posi; \
	AR.outfile = AR.infile; AR.infile = ffFi; \
	SetEndScratch(AR.infile,&posi); }

#define CopyArg(to,from) { if ( *from > 0 ) { int ica = *from; NCOPY(to,from,ica) } \
		else if ( *from <= -FUNCTION ) *to++ = *from++;  \
		else { *to++ = *from++; *to++ = *from++; } }

#if ARGHEAD > 2
#define FILLARG(w) { int i = ARGHEAD-2; while ( --i >= 0 ) *w++ = 0; }
#define COPYARG(w,t) { int i = ARGHEAD-2; while ( --i >= 0 ) *w++ = *t++; }
#define ZEROARG(w) { int i; for ( i = 2; i < ARGHEAD; i++ ) w[i] = 0; }
#else
#define FILLARG(w)
#define COPYARG(w,t)
#define ZEROARG(w)
#endif

#if FUNHEAD > 2
#define FILLFUN(w) { *w++ = 0; FILLFUN3(w) }
#define COPYFUN(w,t) { *w++ = *t++; COPYFUN3(w,t) }
#else
#define FILLFUN(w)
#define COPYFUN(w,t)
#endif

#if FUNHEAD > 3
#define FILLFUN3(w) { int ie = FUNHEAD-3; while ( --ie >= 0 ) *w++ = 0; }
#define COPYFUN3(w,t) { int ie = FUNHEAD-3; while ( --ie >= 0 ) *w++ = *t++; }
#else
#define COPYFUN3(w,t)
#define FILLFUN3(w)
#endif

#if SUBEXPSIZE > 5
#define FILLSUB(w) { int ie = SUBEXPSIZE-5; while ( --ie >= 0 ) *w++ = 0; }
#define COPYSUB(w,ww) { int ie = SUBEXPSIZE-5; while ( --ie >= 0 ) *w++ = *ww++; }
#else
#define FILLSUB(w)
#define COPYSUB(w,ww)
#endif

#if EXPRHEAD > 4
#define FILLEXPR(w) { int ie = EXPRHEAD-4; while ( --ie >= 0 ) *w++ = 0; }
#else
#define FILLEXPR(w)
#endif

#define NEXTARG(x) if(*x>0) x += *x; else if(*x <= -FUNCTION)x++; else x += 2;
#define COPY1ARG(s1,t1) { int ica; if ( (ica=*t1) > 0 ) { NCOPY(s1,t1,ica) } \
		else if(*t1<=-FUNCTION){*s1++=*t1++;} else{*s1++=*t1++;*s1++=*t1++;} }

/**
 * Fills a buffer by zero in the range [begin,end).
 *
 * @param  w      The buffer.
 * @param  begin  The index for the beginning of the range.
 * @param  end    The index for the end of the range (exclusive).
 */
#define ZeroFillRange(w,begin,end) do { \
	int tmp_i; \
	for ( tmp_i = begin; tmp_i < end; tmp_i++ ) { (w)[tmp_i] = 0; } \
} while (0)

#define TABLESIZE(a,b) (((WORD)sizeof(a))/((WORD)sizeof(b)))
#define WORDDIF(x,y) (WORD)(x-y)
#define wsizeof(a) ((WORD)sizeof(a))
#define VARNAME(type,num) (AC.varnames->namebuffer+type[num].name)
#define DOLLARNAME(type,num) (AC.dollarnames->namebuffer+type[num].name)
#define EXPRNAME(num) (AC.exprnames->namebuffer+Expressions[num].name)
 
#define PREV(x) prevorder?prevorder:x

#define SETERROR(x) { Terminate(-1); return(-1); }

/* use this macro to avoid the unused parameter warning */
#define DUMMYUSE(x) (void)(x);

#ifdef _FILE_OFFSET_BITS
#if _FILE_OFFSET_BITS==64
/*:[19mar2004 mt]*/

#define ADDPOS(pp,x) (pp).p1 = ((pp).p1+(off_t)(x))
#define SETBASELENGTH(ss,x) (ss).p1 = (off_t)(x)
#define SETBASEPOSITION(pp,x) (pp).p1 = (off_t)(x)
#define ISEQUALPOSINC(pp1,pp2,x) ( (pp1).p1 == ((pp2).p1+(off_t)(x)) )
#define ISGEPOSINC(pp1,pp2,x) ( (pp1).p1 >= ((pp2).p1+(off_t)(x)) )
#define DIVPOS(pp,n) ( (pp).p1/(off_t)(n) )
#define MULPOS(pp,n) (pp).p1 *= (off_t)(n)

#else

#define ADDPOS(pp,x) (pp).p1 = ((pp).p1+(x))
#define SETBASELENGTH(ss,x) (ss).p1 = (x)
#define SETBASEPOSITION(pp,x) (pp).p1 = (x)
#define ISEQUALPOSINC(pp1,pp2,x) ( (pp1).p1 == ((pp2).p1+(LONG)(x)) )
#define ISGEPOSINC(pp1,pp2,x) ( (pp1).p1 >= ((pp2).p1+(LONG)(x)) )
#define DIVPOS(pp,n) ( (pp).p1/(n) )
#define MULPOS(pp,n) (pp).p1 *= (n)
#endif
#else

#define ADDPOS(pp,x) (pp).p1 = ((pp).p1+(LONG)(x))
#define SETBASELENGTH(ss,x) (ss).p1 = (LONG)(x)
#define SETBASEPOSITION(pp,x) (pp).p1 = (LONG)(x)
#define ISEQUALPOSINC(pp1,pp2,x) ( (pp1).p1 == ((pp2).p1+(LONG)(x)) )
#define ISGEPOSINC(pp1,pp2,x) ( (pp1).p1 >= ((pp2).p1+(LONG)(x)) )
#define DIVPOS(pp,n) ( (pp).p1/(LONG)(n) )
#define MULPOS(pp,n) (pp).p1 *= (LONG)(n)

#endif
#define DIFPOS(ss,pp1,pp2) (ss).p1 = ((pp1).p1-(pp2).p1)
#define DIFBASE(pp1,pp2) ((pp1).p1-(pp2).p1)
#define ADD2POS(pp1,pp2) (pp1).p1 += (pp2).p1
#define PUTZERO(pp) (pp).p1 = 0
#define BASEPOSITION(pp) ((pp).p1)
#define SETSTARTPOS(pp) (pp).p1 = -2
#define NOTSTARTPOS(pp) ( (pp).p1 > -2 )
#define ISMINPOS(pp) ( (pp).p1 == -1 )
#define ISEQUALPOS(pp1,pp2) ( (pp1).p1 == (pp2).p1 )
#define ISNOTEQUALPOS(pp1,pp2) ( (pp1).p1 != (pp2).p1 )
#define ISLESSPOS(pp1,pp2) ( (pp1).p1 < (pp2).p1 )
#define ISGEPOS(pp1,pp2) ( (pp1).p1 >= (pp2).p1 )
#define ISNOTZEROPOS(pp) ( (pp).p1 != 0 )
#define ISZEROPOS(pp) ( (pp).p1 == 0 )
#define ISPOSPOS(pp) ( (pp).p1 > 0 )
#define ISNEGPOS(pp) ( (pp).p1 < 0 )
extern VOID TELLFILE(int,POSITION *);

#define TOLONG(x) ((LONG)(x))

#define Add2Com(x) { WORD cod[2]; cod[0] = x; cod[1] = 2; AddNtoL(2,cod); }
#define Add3Com(x1,x2) { WORD cod[3]; cod[0] = x1; cod[1] = 3; cod[2] = x2; AddNtoL(3,cod); }
#define Add4Com(x1,x2,x3) { WORD cod[4]; cod[0] = x1; cod[1] = 4; \
   cod[2] = x2; cod[3] = x3; AddNtoL(4,cod); }
#define Add5Com(x1,x2,x3,x4) { WORD cod[5]; cod[0] = x1; cod[1] = 5; \
   cod[2] = x2; cod[3] = x3; cod[4] = x4; AddNtoL(5,cod); }

/*
	The temporary variable ppp is to avoid a compiler warning about strict aliassing
*/
#define WantAddPointers(x) while((AT.pWorkPointer+(x))>AR.pWorkSize){WORD ***ppp=&AT.pWorkSpace;\
	ExpandBuffer((void **)ppp,&AR.pWorkSize,(int)(sizeof(WORD *)));}
#define WantAddLongs(x) while((AT.lWorkPointer+(x))>AR.lWorkSize){LONG **ppp=&AT.lWorkSpace;\
	ExpandBuffer((void **)ppp,&AR.lWorkSize,sizeof(LONG));}
#define WantAddPositions(x) while((AT.posWorkPointer+(x))>AR.posWorkSize){POSITION **ppp=&AT.posWorkSpace;\
	ExpandBuffer((void **)ppp,&AR.posWorkSize,sizeof(POSITION));}

/* inline in form3.h (or config.h). */
#define FORM_INLINE inline

/*
	Macro's for memory management. This can be done by routines, but that
	would be slower. Inline routines could do this, but we don't want to
	leave this to the friendliness of the compiler(s).
	The routines can be found in the file tools.c
*/
#define MEMORYMACROS

#ifdef MEMORYMACROS

#define TermMalloc(x) ( (AT.TermMemTop <= 0 ) ? TermMallocAddMemory(BHEAD0), AT.TermMemHeap[--AT.TermMemTop]: AT.TermMemHeap[--AT.TermMemTop] )
#define NumberMalloc(x) ( (AT.NumberMemTop <= 0 ) ? NumberMallocAddMemory(BHEAD0), AT.NumberMemHeap[--AT.NumberMemTop]: AT.NumberMemHeap[--AT.NumberMemTop] )
#define CacheNumberMalloc(x) ( (AT.CacheNumberMemTop <= 0 ) ? CacheNumberMallocAddMemory(BHEAD0), AT.CacheNumberMemHeap[--AT.CacheNumberMemTop]: AT.CacheNumberMemHeap[--AT.CacheNumberMemTop] )
#define TermFree(TermMem,x) AT.TermMemHeap[AT.TermMemTop++] = (WORD *)(TermMem)
#define NumberFree(NumberMem,x) AT.NumberMemHeap[AT.NumberMemTop++] = (UWORD *)(NumberMem)
#define CacheNumberFree(NumberMem,x) AT.CacheNumberMemHeap[AT.CacheNumberMemTop++] = (UWORD *)(NumberMem)

#else

#define TermMalloc(x) TermMalloc2(BHEAD (char *)(x))
#define NumberMalloc(x) NumberMalloc2(BHEAD (char *)(x))
#define CacheNumberMalloc(x) CacheNumberMalloc2(BHEAD (char *)(x))
#define TermFree(x,y) TermFree2(BHEAD (WORD *)(x),(char *)(y))
#define NumberFree(x,y) NumberFree2(BHEAD (UWORD *)(x),(char *)(y))
#define CacheNumberFree(x,y) CacheNumberFree2(BHEAD (UWORD *)(x),(char *)(y))

#endif

/*
 * Macros for checking nesting levels in the compiler, used as follows:
 *
 *   AC.IfSumCheck[AC.IfLevel] = NestingChecksum();
 *   AC.IfLevel++;
 *
 *   AC.IfLevel--;
 *   if ( AC.IfSumCheck[AC.IfLevel] != NestingChecksum() ) {
 *     MesNesting();
 *   }
 *
 * Note that NestingChecksum() also contains AC.IfLevel and so in this case
 * using increment/decrement operators on it in the left-hand side may be
 * confusing.
 */
#define NestingChecksum() (AC.IfLevel + AC.RepLevel + AC.arglevel + AC.insidelevel + AC.termlevel + AC.inexprlevel + AC.dolooplevel +AC.SwitchLevel)
#define MesNesting() MesPrint("&Illegal nesting of if, repeat, argument, inside, term, inexpression and do")

#define MarkPolyRatFunDirty(T) {if(*T&&AR.PolyFunType==2){WORD *TP,*TT;TT=T+*T;TT-=ABS(TT[-1]);\
TP=T+1;while(TP<TT){if(*TP==AR.PolyFun){TP[2]|=(DIRTYFLAG|MUSTCLEANPRF);}TP+=TP[1];}}}

/*
	Macros for nesting input levels for #$name = ...; assign instructions.
	Note that the level should never go below zero.
*/
#define PUSHPREASSIGNLEVEL AP.PreAssignLevel++; { GETIDENTITY \
    if ( AP.PreAssignLevel >= AP.MaxPreAssignLevel ) { int i; \
		LONG *ap = (LONG *)Malloc1(2*AP.MaxPreAssignLevel*sizeof(LONG *),"PreAssignStack"); \
		for ( i = 0; i < AP.MaxPreAssignLevel; i++ ) ap[i] = AP.PreAssignStack[i]; \
		M_free(AP.PreAssignStack,"PreAssignStack"); \
		AP.MaxPreAssignLevel *= 2; AP.PreAssignStack = ap; \
	} \
	*AT.WorkPointer++ = AP.PreContinuation; AP.PreContinuation = 0; \
	AP.PreAssignStack[AP.PreAssignLevel] = AC.iPointer - AC.iBuffer; }

#define POPPREASSIGNLEVEL if ( AP.PreAssignLevel > 0 ) { GETIDENTITY \
	AC.iPointer = AC.iBuffer + AP.PreAssignStack[AP.PreAssignLevel--]; \
	AP.PreContinuation = *--AT.WorkPointer; \
	*AC.iPointer = 0; }

/*
	MesPrint("P-level popped to %d with %d",AP.PreAssignLevel,(WORD)(AC.iPointer - AC.iBuffer));

  	#] Macro's : 
  	#[ Inline functions :
*/

/*
 * The following three functions give the unsigned absolute value of a signed
 * integer even for the most negative integer. This is beyond the scope of
 * the standard abs() function and its family, whose return-values are signed.
 * In short, we should not use the unary minus operator with signed numbers
 * unless we are sure that there are no integer overflows. Instead, we rely on
 * two well-defined operations: (i) signed-to-unsigned conversion and
 * (ii) unary minus of unsigned operands.
 *
 * See also:
 *   https://stackoverflow.com/a/4536188   (Unary minus and signed-to-unsigned conversion)
 *   https://stackoverflow.com/q/8026694   (C: unary minus operator behavior with unsigned operands)
 *   https://stackoverflow.com/q/1610947   (Why does stdlib.h's abs() family of functions return a signed value?)
 *   https://blog.regehr.org/archives/226  (A Guide to Undefined Behavior in C and C++, Part 2)
 */
static inline unsigned int IntAbs(int x)
{
	if ( x >= 0 ) return x;
	return(-((unsigned int)x));
}

static inline UWORD WordAbs(WORD x)
{
	if ( x >= 0 ) return x;
	return(-((UWORD)x));
}

static inline ULONG LongAbs(LONG x)
{
	if ( x >= 0 ) return x;
	return(-((ULONG)x));
}

/*
 * The following functions provide portable unsigned-to-signed conversions
 * (to avoid the implementation-defined behaviour), which is expected to be
 * optimized to a no-op.
 *
 * See also:
 *   https://stackoverflow.com/a/13208789  (Efficient unsigned-to-signed cast avoiding implementation-defined behavior)
 */
static inline int UnsignedToInt(unsigned int x)
{
	extern void Terminate(int);
	if ( x <= INT_MAX ) return(x);
	if ( x >= (unsigned int)INT_MIN ) return((int)(x - INT_MIN) + INT_MIN);
	Terminate(1);
	return(0);
}

static inline WORD UWordToWord(UWORD x)
{
	extern void Terminate(int);
	if ( x <= WORD_MAX_VALUE ) return(x);
	if ( x >= (UWORD)WORD_MIN_VALUE ) return((WORD)(x - WORD_MIN_VALUE) + WORD_MIN_VALUE);
	Terminate(1);
	return(0);
}

static inline LONG ULongToLong(ULONG x)
{
	extern void Terminate(int);
	if ( x <= LONG_MAX_VALUE ) return(x);
	if ( x >= (ULONG)LONG_MIN_VALUE ) return((LONG)(x - LONG_MIN_VALUE) + LONG_MIN_VALUE);
	Terminate(1);
	return(0);
}

/*
  	#] Inline functions : 
  	#[ Thread objects :
*/

/**
 * NOTE: We have replaced LOCK(ErrorMessageLock) and UNLOCK(ErrorMessageLock)
 * by MLOCK(ErrorMessageLock) and MUNLOCK(ErrorMessageLock). They are used
 * for the synchronised output in ParFORM.
 * (TU 28 May 2011)
 */
#ifdef WITHPTHREADS

#define EXTERNLOCK(x) extern pthread_mutex_t x;
#define INILOCK(x)    pthread_mutex_t x = PTHREAD_MUTEX_INITIALIZER;
#define EXTERNRWLOCK(x) extern pthread_rwlock_t x;
#define INIRWLOCK(x)    pthread_rwlock_t x = PTHREAD_RWLOCK_INITIALIZER;
#ifdef DEBUGGINGLOCKS
#include <asm/errno.h>
#define LOCK(x)       while ( pthread_mutex_trylock(&(x)) == EBUSY ) {}
#define RWLOCKR(x)      while ( pthread_rwlock_tryrdlock(&(x)) == EBUSY ) {}
#define RWLOCKW(x)      while ( pthread_rwlock_trywrlock(&(x)) == EBUSY ) {}
#else
#define LOCK(x)       pthread_mutex_lock(&(x))
#define RWLOCKR(x)      pthread_rwlock_rdlock(&(x))
#define RWLOCKW(x)      pthread_rwlock_wrlock(&(x))
#endif
#define UNLOCK(x)     pthread_mutex_unlock(&(x))
#define UNRWLOCK(x)     pthread_rwlock_unlock(&(x))
#define MLOCK(x)      LOCK(x)
#define MUNLOCK(x)    UNLOCK(x)

#define GETBIDENTITY
#define GETIDENTITY   int identity = WhoAmI(); ALLPRIVATES *B = AB[identity];
#else

#define EXTERNLOCK(x)
#define INILOCK(x)
#define LOCK(x) 
#define UNLOCK(x)
#define EXTERNRWLOCK(x)
#define INIRWLOCK(x)
#define RWLOCKR(x)
#define RWLOCKW(x)
#define UNRWLOCK(x)
#ifdef WITHMPI
#define MLOCK(x)      do { if ( PF.me != MASTER ) PF_MLock(); } while (0)
#define MUNLOCK(x)    do { if ( PF.me != MASTER ) PF_MUnlock(); } while (0)
#else
#define MLOCK(x)
#define MUNLOCK(x)
#endif
#define GETIDENTITY
#define GETBIDENTITY

#endif

/*
  	#] Thread objects : 
  	#[ Declarations :
*/
 
#ifdef TERMMALLOCDEBUG
extern WORD **DebugHeap1, **DebugHeap2;
#endif

/**
 *	All functions (well, nearly all) are declared here.
 */

extern VOID   StartVariables();
extern VOID   setSignalHandlers(VOID);
extern UBYTE *CodeToLine(WORD,UBYTE *);
extern UBYTE *AddArrayIndex(WORD ,UBYTE *);
extern INDEXENTRY *FindInIndex(WORD,FILEDATA *,WORD,WORD);
extern INDEXENTRY *NextFileIndex(POSITION *);
extern WORD  *PasteTerm(PHEAD WORD,WORD *,WORD *,WORD,WORD);
extern UBYTE *StrCopy(UBYTE *,UBYTE *);
extern UBYTE *WrtPower(UBYTE *,WORD);
extern WORD   AccumGCD(PHEAD UWORD *,WORD *,UWORD *,WORD);
extern VOID   AddArgs(PHEAD WORD *,WORD *,WORD *);
extern WORD   AddCoef(PHEAD WORD **,WORD **);
extern WORD   AddLong(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   AddPLon(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   AddPoly(PHEAD WORD **,WORD **);
extern WORD   AddRat(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern VOID   AddToLine(UBYTE *);
extern WORD   AddWild(PHEAD WORD,WORD,WORD);
extern WORD   BigLong(UWORD *,WORD,UWORD *,WORD);
extern WORD   BinomGen(PHEAD WORD *,WORD,WORD **,WORD,WORD,WORD,WORD,WORD,UWORD *,WORD);
extern WORD   CheckWild(PHEAD WORD,WORD,WORD,WORD *);
extern WORD   Chisholm(PHEAD WORD *,WORD);
extern WORD   CleanExpr(WORD);
extern VOID   CleanUp(WORD);
extern VOID   ClearWild(PHEAD0);
extern WORD   CompareFunctions(WORD *,WORD *);
extern WORD   Commute(WORD *,WORD *);
extern WORD   DetCommu(WORD *);
extern WORD   DoesCommu(WORD *);
extern int    CompArg(WORD *,WORD *);
extern WORD   CompCoef(WORD *, WORD *);
extern WORD   CompGroup(PHEAD WORD,WORD **,WORD *,WORD *,WORD);
extern WORD   Compare1(WORD *,WORD *,WORD);
extern WORD   CountDo(WORD *,WORD *);
extern WORD   CountFun(WORD *,WORD *);
extern WORD   DimensionSubterm(WORD *);
extern WORD   DimensionTerm(WORD *);
extern WORD   DimensionExpression(PHEAD WORD *);
extern WORD   Deferred(PHEAD WORD *,WORD);
extern WORD   DeleteStore(WORD);
extern WORD   DetCurDum(PHEAD WORD *);
extern VOID   DetVars(WORD *,WORD);
extern WORD   Distribute(DISTRIBUTE *,WORD);
extern WORD   DivLong(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *,UWORD *,WORD *);
extern WORD   DivRat(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   Divvy(PHEAD UWORD *,WORD *,UWORD *,WORD);
extern WORD   DoDelta(WORD *);
extern WORD   DoDelta3(PHEAD WORD *,WORD);
extern WORD   TestPartitions(WORD *, PARTI *);
extern WORD   DoPartitions(PHEAD WORD *,WORD);
extern int    CoCanonicalize(UBYTE *);
extern int    DoCanonicalize(PHEAD WORD *, WORD *);
extern WORD   GenTopologies(PHEAD WORD *,WORD);
extern WORD   GenDiagrams(PHEAD WORD *,WORD);
extern int    DoTopologyCanonicalize(PHEAD WORD *,WORD,WORD,WORD *);
extern int    DoShattering(PHEAD WORD *,WORD *,WORD *,WORD);
extern WORD   GenerateTopologies(PHEAD WORD,WORD,WORD,WORD);
extern WORD   DoTableExpansion(WORD *,WORD);
extern WORD   DoDistrib(PHEAD WORD *,WORD);
extern WORD   DoShuffle(WORD *,WORD,WORD,WORD);
extern WORD   DoPermutations(PHEAD WORD *,WORD);
extern int    Shuffle(WORD *, WORD *, WORD *);
extern int    FinishShuffle(WORD *);
extern WORD   DoStuffle(WORD *,WORD,WORD,WORD);
extern int    Stuffle(WORD *, WORD *, WORD *);
extern int    FinishStuffle(WORD *);
extern WORD  *StuffRootAdd(WORD *, WORD *, WORD *);
extern WORD   TestUse(WORD *,WORD);
extern DBASE *FindTB(UBYTE *);
extern int    CheckTableDeclarations(DBASE *);
extern WORD   Apply(WORD *,WORD);
extern int    ApplyExec(WORD *,int,WORD);
extern WORD   ApplyReset(WORD);
extern WORD   TableReset(VOID);
extern VOID   ReWorkT(WORD *,WORD *,WORD);
extern WORD   GetIfDollarNum(WORD *, WORD *);
extern int    FindVar(WORD *,WORD *);
extern WORD   DoIfStatement(PHEAD WORD *,WORD *);
extern WORD   DoOnePow(PHEAD WORD *,WORD,WORD,WORD *,WORD *,WORD,WORD *);
extern void   DoRevert(WORD *,WORD *);
extern WORD   DoSumF1(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   DoSumF2(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   DoTheta(PHEAD WORD *);
extern LONG   EndSort(PHEAD WORD *,int);
extern WORD   EntVar(WORD,UBYTE *,WORD,WORD,WORD,WORD);
extern WORD   EpfCon(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   EpfFind(PHEAD WORD *,WORD *);
extern WORD   EpfGen(WORD,WORD *,WORD *,WORD *,WORD);
extern WORD   EqualArg(WORD *,WORD,WORD);
extern WORD   Evaluate(UBYTE **);
extern int    Factorial(PHEAD WORD,UWORD *,WORD *);
extern int    Bernoulli(WORD,UWORD *,WORD *);
extern int    FactorIn(PHEAD WORD *,WORD);
extern int    FactorInExpr(PHEAD WORD *,WORD);
extern WORD   FindAll(PHEAD WORD *,WORD *,WORD,WORD *);
extern WORD   FindMulti(PHEAD WORD *,WORD *);
extern WORD   FindOnce(PHEAD WORD *,WORD *);
extern WORD   FindOnly(PHEAD WORD *,WORD *);
extern WORD   FindRest(PHEAD WORD *,WORD *);
extern WORD   FindSpecial(WORD *);
extern WORD   FindrNumber(WORD,VARRENUM *);
extern VOID   FiniLine(VOID);
extern WORD   FiniTerm(PHEAD WORD *,WORD *,WORD *,WORD,WORD);
extern WORD   FlushOut(POSITION *,FILEHANDLE *,int);
extern VOID   FunLevel(PHEAD WORD *);
extern VOID   AdjustRenumScratch(PHEAD0);
extern VOID   GarbHand(VOID);
extern WORD   GcdLong(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   LcmLong(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern VOID   GCD(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern ULONG  GCD2(ULONG,ULONG);
extern WORD   Generator(PHEAD WORD *,WORD);
extern WORD   GetBinom(UWORD *,WORD *,WORD,WORD);
extern WORD   GetFromStore(WORD *,POSITION *,RENUMBER,WORD *,WORD);
extern WORD   GetLong(UBYTE *,UWORD *,WORD *);
extern WORD   GetMoreTerms(WORD *);
extern WORD   GetMoreFromMem(WORD *,WORD **);
extern WORD   GetOneTerm(PHEAD WORD *,FILEHANDLE *,POSITION *,int);
extern RENUMBER GetTable(WORD,POSITION *,WORD);
extern WORD   GetTerm(PHEAD WORD *);
extern WORD   Glue(PHEAD WORD *,WORD *,WORD *,WORD);
extern WORD   InFunction(PHEAD WORD *,WORD *);
extern VOID   IniLine(WORD);
extern WORD   IniVars(VOID);
extern VOID   Initialize(VOID);
extern WORD   InsertTerm(PHEAD WORD *,WORD,WORD,WORD *,WORD *,WORD);
extern VOID   LongToLine(UWORD *,WORD);
extern WORD   MakeDirty(WORD *,WORD *,WORD);
extern VOID   MarkDirty(WORD *,WORD);
extern VOID   PolyFunDirty(PHEAD WORD *);
extern VOID   PolyFunClean(PHEAD WORD *);
extern WORD   MakeModTable(VOID);
extern WORD   MatchE(PHEAD WORD *,WORD *,WORD *,WORD);
extern int    MatchCy(PHEAD WORD *,WORD *,WORD *,WORD);
extern int    FunMatchCy(PHEAD WORD *,WORD *,WORD *,WORD);
extern int    FunMatchSy(PHEAD WORD *,WORD *,WORD *,WORD);
extern int    MatchArgument(PHEAD WORD *,WORD *);
extern WORD   MatchFunction(PHEAD WORD *,WORD *,WORD *);
extern WORD   MergePatches(WORD);
extern WORD   MesCerr(char *, UBYTE *);
extern WORD   MesComp(char *, UBYTE *, UBYTE *);
extern WORD   Modulus(WORD *);
extern VOID   MoveDummies(PHEAD WORD *,WORD);
extern WORD   MulLong(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   MulRat(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   Mully(PHEAD UWORD *,WORD *,UWORD *,WORD);
extern WORD   MultDo(PHEAD WORD *,WORD *);
extern WORD   NewSort(PHEAD0);
extern WORD   ExtraSymbol(WORD,WORD,WORD,WORD *,WORD *);
extern WORD   Normalize(PHEAD WORD *);
extern WORD   BracketNormalize(PHEAD WORD *);
extern VOID   DropCoefficient(PHEAD WORD *);
extern VOID   DropSymbols(PHEAD WORD *);
extern int    PutInside(PHEAD WORD *, WORD *);
extern WORD   OpenTemp(VOID);
extern VOID   Pack(UWORD *,WORD *,UWORD *,WORD );
extern LONG   PasteFile(PHEAD WORD,WORD *,POSITION *,WORD **,RENUMBER,WORD *,WORD);
extern WORD   Permute(PERM *,WORD);
extern WORD   PermuteP(PERMP *,WORD);
extern WORD   PolyFunMul(PHEAD WORD *);
extern WORD   PopVariables(VOID);
extern WORD   PrepPoly(PHEAD WORD *,WORD);
extern WORD   Processor(VOID);
extern WORD   Product(UWORD *,WORD *,WORD);
extern VOID   PrtLong(UWORD *,WORD,UBYTE *);
extern VOID   PrtTerms(VOID);
extern VOID   PrintRunningTime(VOID);
extern LONG   GetRunningTime(VOID);
extern WORD   PutBracket(PHEAD WORD *);
extern LONG   PutIn(FILEHANDLE *,POSITION *,WORD *,WORD **,int);
extern WORD   PutInStore(INDEXENTRY *,WORD);
extern WORD   PutOut(PHEAD WORD *,POSITION *,FILEHANDLE *,WORD);
extern UWORD  Quotient(UWORD *,WORD *,WORD);
extern WORD   RaisPow(PHEAD UWORD *,WORD *,UWORD);
extern VOID   RaisPowCached (PHEAD WORD, WORD, UWORD **, WORD *);
extern WORD   RaisPowMod (WORD, WORD, WORD);
extern int    NormalModulus(UWORD *,WORD *);
extern int    MakeInverses(VOID);
extern int    GetModInverses(WORD,WORD,WORD *,WORD *);
extern int    GetLongModInverses(PHEAD UWORD *, WORD, UWORD *, WORD, UWORD *, WORD *, UWORD *, WORD *);
extern VOID   RatToLine(UWORD *,WORD);
extern WORD   RatioFind(PHEAD WORD *,WORD *);
extern WORD   RatioGen(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   ReNumber(PHEAD WORD *);
extern WORD   ReadSnum(UBYTE **);
extern WORD   Remain10(UWORD *,WORD *);
extern WORD   Remain4(UWORD *,WORD *);
extern WORD   ResetScratch(VOID);
extern WORD   ResolveSet(PHEAD WORD *,WORD *,WORD *);
extern WORD   RevertScratch(VOID);
extern WORD   ScanFunctions(PHEAD WORD *,WORD *,WORD);
extern VOID   SeekScratch(FILEHANDLE *,POSITION *);
extern VOID   SetEndScratch(FILEHANDLE *,POSITION *);
extern VOID   SetEndHScratch(FILEHANDLE *,POSITION *);
extern WORD   SetFileIndex(VOID);
extern WORD   Sflush(FILEHANDLE *);
extern WORD   Simplify(PHEAD UWORD *,WORD *,UWORD *,WORD *);
extern WORD   SortWild(WORD *,WORD);
extern FILE  *LocateBase(char **,char **);
extern LONG   SplitMerge(PHEAD WORD **,LONG);
extern WORD   StoreTerm(PHEAD WORD *);
extern VOID   SubPLon(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern VOID   Substitute(PHEAD WORD *,WORD *,WORD);
extern WORD   SymFind(PHEAD WORD *,WORD *);
extern WORD   SymGen(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   Symmetrize(PHEAD WORD *,WORD *,WORD,WORD,WORD);
extern int    FullSymmetrize(PHEAD WORD *,int);
extern WORD   TakeModulus(UWORD *,WORD *,UWORD *,WORD,WORD);
extern WORD   TakeNormalModulus(UWORD *,WORD *,UWORD *,WORD,WORD);
extern VOID   TalToLine(UWORD);
extern WORD   TenVec(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   TenVecFind(PHEAD WORD *,WORD *);
extern WORD   TermRenumber(WORD *,RENUMBER,WORD);
extern VOID   TestDrop(VOID);
extern VOID   PutInVflags(WORD);
extern WORD   TestMatch(PHEAD WORD *,WORD *);
extern WORD   TestSub(PHEAD WORD *,WORD);
extern LONG   TimeCPU(WORD);
extern LONG   TimeChildren(WORD);
extern LONG   TimeWallClock(WORD);
extern LONG   Timer(int);
extern int    GetTimerInfo(LONG **,LONG **);
extern void   WriteTimerInfo(LONG *,LONG *);
extern LONG   GetWorkerTimes(VOID);
extern WORD   ToStorage(EXPRESSIONS,POSITION *);
extern VOID   TokenToLine(UBYTE *);
extern WORD   Trace4(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   Trace4Gen(PHEAD TRACES *,WORD);
extern WORD   Trace4no(WORD,WORD *,TRACES *);
extern WORD   TraceFind(PHEAD WORD *,WORD *);
extern WORD   TraceN(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   TraceNgen(PHEAD TRACES *,WORD);
extern WORD   TraceNno(WORD,WORD *,TRACES *);
extern WORD   Traces(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   Trick(WORD *,TRACES *);
extern WORD   TryDo(PHEAD WORD *,WORD *,WORD);
extern VOID   UnPack(UWORD *,WORD,WORD *,WORD *);
extern WORD   VarStore(UBYTE *,WORD,WORD,WORD);
extern WORD   WildFill(PHEAD WORD *,WORD *,WORD *);
extern WORD   WriteAll(VOID);
extern WORD   WriteOne(UBYTE *,int,int,WORD);
extern VOID   WriteArgument(WORD *);
extern WORD   WriteExpression(WORD *,LONG);
extern WORD   WriteInnerTerm(WORD *,WORD);
extern VOID   WriteLists(VOID);
extern VOID   WriteSetup(VOID);
extern VOID   WriteStats(POSITION *,WORD);
extern WORD   WriteSubTerm(WORD *,WORD);
extern WORD   WriteTerm(WORD *,WORD *,WORD,WORD,WORD);
extern WORD   execarg(PHEAD WORD *,WORD);
extern WORD   execterm(PHEAD WORD *,WORD);
extern VOID   SpecialCleanup(PHEAD0);
extern void   SetMods();
extern void   UnSetMods();
 
/*---------------------------------------------------------------------*/

extern WORD   DoExecute(WORD,WORD);
extern VOID   SetScratch(FILEHANDLE *,POSITION *);
extern VOID   Warning(char *);
extern VOID   HighWarning(char *);
extern int    SpareTable(TABLES);

extern UBYTE *strDup1(UBYTE *,char *);
extern VOID  *Malloc(LONG);
extern VOID  *Malloc1(LONG,const char *);
extern int    DoTail(int,UBYTE **);
extern int    OpenInput(VOID);
extern int    PutPreVar(UBYTE *,UBYTE *,UBYTE *,int);
extern VOID   Error0(char *);
extern VOID   Error1(char *,UBYTE *);
extern VOID   Error2(char *,char *,UBYTE *);
extern UBYTE  ReadFromStream(STREAM *);
extern UBYTE  GetFromStream(STREAM *);
extern UBYTE  LookInStream(STREAM *);
extern STREAM *OpenStream(UBYTE *,int,int,int);
extern int    LocateFile(UBYTE **,int);
extern STREAM *CloseStream(STREAM *);
extern VOID   PositionStream(STREAM *,LONG);
extern int    ReverseStatements(STREAM *);
extern int    ProcessOption(UBYTE *,UBYTE *,int);
extern int    DoSetups(VOID);
extern VOID   Terminate(int);
extern NAMENODE *GetNode(NAMETREE *,UBYTE *);
extern int    AddName(NAMETREE *,UBYTE *,WORD,WORD,int *);
extern int    GetName(NAMETREE *,UBYTE *,WORD *,int);
extern UBYTE  *GetFunction(UBYTE *,WORD *);
extern UBYTE  *GetNumber(UBYTE *,WORD *);
extern int    GetLastExprName(UBYTE *,WORD *);
extern int    GetAutoName(UBYTE *,WORD *);
extern int    GetVar(UBYTE *,WORD *,WORD *,int,int);
extern int    MakeDubious(NAMETREE *,UBYTE *,WORD *);
extern int    GetOName(NAMETREE *,UBYTE *,WORD *,int);
extern VOID   DumpTree(NAMETREE *);
extern VOID   DumpNode(NAMETREE *,WORD,WORD);
extern VOID   LinkTree(NAMETREE *,WORD,WORD);
extern VOID   CopyTree(NAMETREE *,NAMETREE *,WORD,WORD);
extern int    CompactifyTree(NAMETREE *,WORD);
extern NAMETREE *MakeNameTree(VOID);
extern VOID   FreeNameTree(NAMETREE *);
extern int    AddExpression(UBYTE *,int,int);
extern int    AddSymbol(UBYTE *,int,int,int,int);
extern int    AddDollar(UBYTE *,WORD,WORD *,LONG);
extern int    ReplaceDollar(WORD,WORD,WORD *,LONG);
extern int    DollarRaiseLow(UBYTE *,LONG);
extern int    AddVector(UBYTE *,int,int);
extern int    AddDubious(UBYTE *);
extern int    AddIndex(UBYTE *,int,int);
extern UBYTE *DoDimension(UBYTE *,int *,int *);
extern int    AddFunction(UBYTE *,int,int,int,int,int,int,int);
extern int    CoCommuteInSet(UBYTE *);
extern int    CoFunction(UBYTE *,int,int);
extern int    TestName(UBYTE *);
extern int    AddSet(UBYTE *,WORD);
extern int    DoElements(UBYTE *,SETS,UBYTE *);
extern int    DoTempSet(UBYTE *,UBYTE *);
extern int    NameConflict(int,UBYTE *);
extern int    OpenFile(char *);
extern int    OpenAddFile(char *);
extern int    ReOpenFile(char *);
extern int    CreateFile(char *);
extern int    CreateLogFile(char *);
extern VOID   CloseFile(int);
extern int    CopyFile(char *, char *);
extern int    CreateHandle(VOID);
extern LONG   ReadFile(int,UBYTE *,LONG);
extern LONG   ReadPosFile(PHEAD FILEHANDLE *,UBYTE *,LONG,POSITION *);
extern LONG   WriteFileToFile(int,UBYTE *,LONG);
extern VOID   SeekFile(int,POSITION *,int);
extern LONG   TellFile(int);
extern void   FlushFile(int);
extern int    GetPosFile(int,fpos_t *);
extern int    SetPosFile(int,fpos_t *);
extern VOID   SynchFile(int);
extern VOID   TruncateFile(int);
extern int    GetChannel(char *,int);
extern int    GetAppendChannel(char *);
extern int    CloseChannel(char *);
extern VOID   inictable(VOID);
extern KEYWORD *findcommand(UBYTE *);
extern int    inicbufs(VOID);
extern VOID   StartFiles(VOID);
extern UBYTE *MakeDate(VOID);
extern VOID   PreProcessor(VOID);
extern VOID  *FromList(LIST *);
extern VOID  *From0List(LIST *);
extern VOID  *FromVarList(LIST *);
extern int    DoubleList(VOID ***,int *,int,char *);
extern int    DoubleLList(VOID ***,LONG *,int,char *);
extern void   DoubleBuffer(void **,void **,int,char *);
extern void   ExpandBuffer(void **,LONG *,int);
extern LONG   iexp(LONG,int);
extern int    IsLikeVector(WORD *);
extern int    AreArgsEqual(WORD *,WORD *);
extern int    CompareArgs(WORD *,WORD *);
extern UBYTE *SkipField(UBYTE *,int);
extern int    StrCmp(UBYTE *,UBYTE *);
extern int    StrICmp(UBYTE *,UBYTE *);
extern int    StrHICmp(UBYTE *,UBYTE *);
extern int    StrICont(UBYTE *,UBYTE *);
extern int    CmpArray(WORD *,WORD *,WORD);
extern int    ConWord(UBYTE *,UBYTE *);
extern int    StrLen(UBYTE *);
extern UBYTE *GetPreVar(UBYTE *,int);
extern void   ToGeneral(WORD *,WORD *,WORD);
extern WORD   ToPolyFunGeneral(PHEAD WORD *);
extern int    ToFast(WORD *,WORD *);
extern SETUPPARAMETERS *GetSetupPar(UBYTE *);
extern int    RecalcSetups(VOID);
extern int    AllocSetups(VOID);
extern SORTING *AllocSort(LONG,LONG,LONG,LONG,int,int,LONG);
extern VOID   AllocSortFileName(SORTING *);
extern UBYTE *LoadInputFile(UBYTE *,int);
extern UBYTE  GetInput(VOID);
extern VOID   ClearPushback(VOID);
extern UBYTE  GetChar(int);
extern VOID   CharOut(UBYTE);
extern VOID   UnsetAllowDelay(VOID);
extern VOID   PopPreVars(int);
extern VOID   IniModule(int);
extern VOID   IniSpecialModule(int);
extern int    ModuleInstruction(int *,int *);
extern int    PreProInstruction(VOID);
extern int    LoadInstruction(int);
extern int    LoadStatement(int);
extern KEYWORD *FindKeyWord(UBYTE *,KEYWORD *,int);
extern KEYWORD *FindInKeyWord(UBYTE *,KEYWORD *,int);
extern int    DoDefine(UBYTE *);
extern int    DoRedefine(UBYTE *);
extern int    TheDefine(UBYTE *,int);
extern int    TheUndefine(UBYTE *);
extern int    ClearMacro(UBYTE *);
extern int    DoUndefine(UBYTE *);
extern int    DoInclude(UBYTE *);
extern int    DoReverseInclude(UBYTE *);
extern int    Include(UBYTE *,int);
/*[14apr2004 mt]:*/
extern int    DoExternal(UBYTE *);
extern int    DoToExternal(UBYTE *);
extern int    DoFromExternal(UBYTE *);
extern int    DoPrompt(UBYTE *);
extern int    DoSetExternal(UBYTE *);
/*[10may2006 mt]:*/
extern int    DoSetExternalAttr(UBYTE *);
/*:[10may2006 mt]*/
extern int    DoRmExternal(UBYTE *);
/*:[14apr2004 mt]*/
extern int    DoFactDollar(UBYTE *);
extern WORD   GetDollarNumber(UBYTE **,DOLLARS);
extern int    DoSetRandom(UBYTE *);
extern int    DoOptimize(UBYTE *);
extern int    DoClearOptimize(UBYTE *);
extern int    DoSkipExtraSymbols(UBYTE *);
extern int    DoTimeOutAfter(UBYTE *);
extern int    DoMessage(UBYTE *);
extern int    DoPreOut(UBYTE *);
extern int    DoPreAppend(UBYTE *);
extern int    DoPreCreate(UBYTE *);
extern int    DoPreAssign(UBYTE *);
extern int    DoPreBreak(UBYTE *);
extern int    DoPreDefault(UBYTE *);
extern int    DoPreSwitch(UBYTE *);
extern int    DoPreEndSwitch(UBYTE *);
extern int    DoPreCase(UBYTE *);
extern int    DoPreShow(UBYTE *);
extern int    DoPreExchange(UBYTE *);
extern int    DoSystem(UBYTE *);
extern int    DoPipe(UBYTE *);
extern VOID   StartPrepro(VOID);
extern int    DoIfdef(UBYTE *,int);
extern int    DoIfydef(UBYTE *);
extern int    DoIfndef(UBYTE *);
extern int    DoElse(UBYTE *);
extern int    DoElseif(UBYTE *);
extern int    DoEndif(UBYTE *);
extern int    DoTerminate(UBYTE *);
extern int    DoIf(UBYTE *);
extern int    DoCall(UBYTE *);
extern int    DoDebug(UBYTE *);
extern int    DoDo(UBYTE *);
extern int    DoBreakDo(UBYTE *);
extern int    DoEnddo(UBYTE *);
extern int    DoEndprocedure(UBYTE *);
extern int    DoInside(UBYTE *);
extern int    DoEndInside(UBYTE *);
extern int    DoProcedure(UBYTE *);
extern int    DoPrePrintTimes(UBYTE *);
extern int    DoPreWrite(UBYTE *);
extern int    DoPreClose(UBYTE *);
extern int    DoPreRemove(UBYTE *);
extern int    DoCommentChar(UBYTE *);
extern int    DoPrcExtension(UBYTE *);
extern int    DoPreReset(UBYTE *);
extern VOID   WriteString(int,UBYTE *,int);
extern VOID   WriteUnfinString(int,UBYTE *,int);
extern UBYTE *AddToString(UBYTE *,UBYTE *,int);
extern UBYTE *PreCalc(VOID);
extern UBYTE *PreEval(UBYTE *,LONG *);
extern VOID   NumToStr(UBYTE *,LONG);
extern int    PreCmp(int,int,UBYTE *,int,int,UBYTE *,int);
extern int    PreEq(int,int,UBYTE *,int,int,UBYTE *,int);
extern UBYTE *pParseObject(UBYTE *,int *,LONG *);
extern UBYTE *PreIfEval(UBYTE *,int *);
extern int    EvalPreIf(UBYTE *);
extern int    PreLoad(PRELOAD *,UBYTE *,UBYTE *,int,char *);
extern int    PreSkip(UBYTE *,UBYTE *,int);
extern UBYTE *EndOfToken(UBYTE *);
extern VOID   SetSpecialMode(int,int);
extern VOID   MakeGlobal(VOID);
extern int    ExecModule(int);
extern int    ExecStore(VOID);
extern VOID   FullCleanUp(VOID);
extern int    DoExecStatement(VOID);
extern int    DoPipeStatement(VOID);
extern int    DoPolyfun(UBYTE *);
extern int    DoPolyratfun(UBYTE *);
extern int    CompileStatement(UBYTE *);
extern UBYTE *ToToken(UBYTE *);
extern int    GetDollar(UBYTE *);
extern int    MesWork(VOID);
extern int    MesPrint(const char *,...);
extern int    MesCall(char *);
extern UBYTE *NumCopy(WORD,UBYTE *);
extern char  *LongCopy(LONG,char *);
extern char  *LongLongCopy(off_t *,char *);
extern VOID   ReserveTempFiles(int);
extern VOID   PrintTerm(WORD *,char *);
extern VOID   PrintTermC(WORD *,char *);
extern VOID   PrintSubTerm(WORD *,char *);
extern VOID   PrintWords(WORD *,LONG);
extern void   PrintSeq(WORD *,char *);
extern int    ExpandTripleDots(int);
extern LONG   ComPress(WORD **,LONG *);
extern VOID   StageSort(FILEHANDLE *);

#define M_alloc(x)      malloc((size_t)(x))

extern void   M_free(VOID *,const char *);
extern void   ClearWildcardNames(VOID);
extern int    AddWildcardName(UBYTE *);
extern int    GetWildcardName(UBYTE *);
extern void   Globalize(int);
extern void   ResetVariables(int);
extern void   AddToPreTypes(int);
extern void   MessPreNesting(int);
extern LONG   GetStreamPosition(STREAM *);
extern WORD  *DoubleCbuffer(int,WORD *,int);
extern WORD  *AddLHS(int);
extern WORD  *AddRHS(int,int);
extern int    AddNtoL(int,WORD *);
extern int    AddNtoC(int,int,WORD *,int);
extern VOID   DoubleIfBuffers(VOID);
extern STREAM *CreateStream(UBYTE *);

extern int    setonoff(UBYTE *,int *,int,int);
extern int    DoPrint(UBYTE *,int);
extern int    SetExpr(UBYTE *,int,int);
extern void   AddToCom(int,WORD *);
extern int    Add2ComStrings(int,WORD *,UBYTE *,UBYTE *);
extern int    DoSymmetrize(UBYTE *,int);
extern int    DoArgument(UBYTE *,int);
extern int    ArgFactorize(PHEAD WORD *,WORD *);
extern WORD  *TakeArgContent(PHEAD WORD *, WORD *);
extern WORD  *MakeInteger(PHEAD WORD *,WORD *,WORD *);
extern WORD  *MakeMod(PHEAD WORD *,WORD *,WORD *);
extern WORD   FindArg(PHEAD WORD *);
extern WORD   InsertArg(PHEAD WORD *,WORD *,int);
extern int    CleanupArgCache(PHEAD WORD);
extern int    ArgSymbolMerge(WORD *, WORD *);
extern int    ArgDotproductMerge(WORD *, WORD *);
extern void   SortWeights(LONG *,LONG *,WORD);
extern int    DoBrackets(UBYTE *,int);
extern int    DoPutInside(UBYTE *,int);
extern WORD  *CountComp(UBYTE *,WORD *);
extern int    CoAntiBracket(UBYTE *);
extern int    CoAntiSymmetrize(UBYTE *);
extern int    DoArgPlode(UBYTE *,int);
extern int    CoArgExplode(UBYTE *);
extern int    CoArgImplode(UBYTE *);
extern int    CoArgument(UBYTE *);
extern int    CoInside(UBYTE *);
extern int    ExecInside(UBYTE *);
extern int    CoInExpression(UBYTE *);
extern int    CoInParallel(UBYTE *);
extern int    CoNotInParallel(UBYTE *);
extern int    DoInParallel(UBYTE *,int);
extern int    CoEndInExpression(UBYTE *);
extern int    CoBracket(UBYTE *);
extern int    CoPutInside(UBYTE *);
extern int    CoAntiPutInside(UBYTE *);
extern int    CoMultiBracket(UBYTE *);
extern int    CoCFunction(UBYTE *);
extern int    CoCTensor(UBYTE *);
extern int    CoCollect(UBYTE *);
extern int    CoCompress(UBYTE *);
extern int    CoContract(UBYTE *);
extern int    CoCycleSymmetrize(UBYTE *);
extern int    CoDelete(UBYTE *);
extern int    CoTableBase(UBYTE *);
extern int    CoApply(UBYTE *);
extern int    CoDenominators(UBYTE *);
extern int    CoDimension(UBYTE *);
extern int    CoDiscard(UBYTE *);
extern int    CoDisorder(UBYTE *);
extern int    CoDrop(UBYTE *);
extern int    CoDropCoefficient(UBYTE *);
extern int    CoDropSymbols(UBYTE *);
extern int    CoElse(UBYTE *);
extern int    CoElseIf(UBYTE *);
extern int    CoEndArgument(UBYTE *);
extern int    CoEndInside(UBYTE *);
extern int    CoEndIf(UBYTE *);
extern int    CoEndRepeat(UBYTE *);
extern int    CoEndTerm(UBYTE *);
extern int    CoEndWhile(UBYTE *);
extern int    CoExit(UBYTE *);
extern int    CoFactArg(UBYTE *);
extern int    CoFactDollar(UBYTE *);
extern int    CoFactorize(UBYTE *);
extern int    CoNFactorize(UBYTE *);
extern int    CoUnFactorize(UBYTE *);
extern int    CoNUnFactorize(UBYTE *);
extern int    DoFactorize(UBYTE *,int);
extern int    CoFill(UBYTE *);
extern int    CoFillExpression(UBYTE *);
extern int    CoFixIndex(UBYTE *);
extern int    CoFormat(UBYTE *);
extern int    CoGlobal(UBYTE *);
extern int    CoGlobalFactorized(UBYTE *);
extern int    CoGoTo(UBYTE *);
extern int    CoId(UBYTE *);
extern int    CoIdNew(UBYTE *);
extern int    CoIdOld(UBYTE *);
extern int    CoIf(UBYTE *);
extern int    CoIfMatch(UBYTE *);
extern int    CoIfNoMatch(UBYTE *);
extern int    CoIndex(UBYTE *);
extern int    CoInsideFirst(UBYTE *);
extern int    CoKeep(UBYTE *);
extern int    CoLabel(UBYTE *);
extern int    CoLoad(UBYTE *);
extern int    CoLocal(UBYTE *);
extern int    CoLocalFactorized(UBYTE *);
extern int    CoMany(UBYTE *);
extern int    CoMerge(UBYTE *);
extern int    CoStuffle(UBYTE *);
extern int    CoMetric(UBYTE *);
extern int    CoModOption(UBYTE *);
extern int    CoModuleOption(UBYTE *);
extern int    CoModulus(UBYTE *);
extern int    CoMulti(UBYTE *);
extern int    CoMultiply(UBYTE *);
extern int    CoNFunction(UBYTE *);
extern int    CoNPrint(UBYTE *);
extern int    CoNTensor(UBYTE *);
extern int    CoNWrite(UBYTE *);
extern int    CoNoDrop(UBYTE *);
extern int    CoNoSkip(UBYTE *);
extern int    CoNormalize(UBYTE *);
extern int    CoMakeInteger(UBYTE *);
extern int    CoFlags(UBYTE *,int);
extern int    CoOff(UBYTE *);
extern int    CoOn(UBYTE *);
extern int    CoOnce(UBYTE *);
extern int    CoOnly(UBYTE *);
extern int    CoOptimizeOption(UBYTE *);
extern int    CoOptimize(UBYTE *);
extern int    CoPolyFun(UBYTE *);
extern int    CoPolyRatFun(UBYTE *);
extern int    CoPrint(UBYTE *);
extern int    CoPrintB(UBYTE *);
extern int    CoProperCount(UBYTE *);
extern int    CoUnitTrace(UBYTE *);
extern int    CoRCycleSymmetrize(UBYTE *);
extern int    CoRatio(UBYTE *);
extern int    CoRedefine(UBYTE *);
extern int    CoRenumber(UBYTE *);
extern int    CoRepeat(UBYTE *);
extern int    CoSave(UBYTE *);
extern int    CoSelect(UBYTE *);
extern int    CoSet(UBYTE *);
extern int    CoSetExitFlag(UBYTE *);
extern int    CoSkip(UBYTE *);
extern int    CoProcessBucket(UBYTE *);
extern int    CoPushHide(UBYTE *);
extern int    CoPopHide(UBYTE *);
extern int    CoHide(UBYTE *);
extern int    CoIntoHide(UBYTE *);
extern int    CoNoHide(UBYTE *);
extern int    CoUnHide(UBYTE *);
extern int    CoNoUnHide(UBYTE *);
extern int    CoSort(UBYTE *);
extern int    CoSplitArg(UBYTE *);
extern int    CoSplitFirstArg(UBYTE *);
extern int    CoSplitLastArg(UBYTE *);
extern int    CoSum(UBYTE *);
extern int    CoSymbol(UBYTE *);
extern int    CoSymmetrize(UBYTE *);
extern int    DoTable(UBYTE *,int);
extern int    CoTable(UBYTE *);
extern int    CoTerm(UBYTE *);
extern int    CoNTable(UBYTE *);
extern int    CoCTable(UBYTE *);
extern void   EmptyTable(TABLES);
extern int    CoToTensor(UBYTE *);
extern int    CoToVector(UBYTE *);
extern int    CoTrace4(UBYTE *);
extern int    CoTraceN(UBYTE *);
extern int    CoChisholm(UBYTE *);
extern int    CoTransform(UBYTE *);
extern int    CoClearTable(UBYTE *);
extern int    DoChain(UBYTE *,int);
extern int    CoChainin(UBYTE *);
extern int    CoChainout(UBYTE *);
extern int    CoTryReplace(UBYTE *);
extern int    CoVector(UBYTE *);
extern int    CoWhile(UBYTE *);
extern int    CoWrite(UBYTE *);
extern int    CoAuto(UBYTE *);
extern int    CoSwitch(UBYTE *);
extern int    CoCase(UBYTE *);
extern int    CoBreak(UBYTE *);
extern int    CoDefault(UBYTE *);
extern int    CoEndSwitch(UBYTE *);
extern int    CoTBaddto(UBYTE *);
extern int    CoTBaudit(UBYTE *);
extern int    CoTBcleanup(UBYTE *);
extern int    CoTBcreate(UBYTE *);
extern int    CoTBenter(UBYTE *);
extern int    CoTBhelp(UBYTE *);
extern int    CoTBload(UBYTE *);
extern int    CoTBoff(UBYTE *);
extern int    CoTBon(UBYTE *);
extern int    CoTBopen(UBYTE *);
extern int    CoTBreplace(UBYTE *);
extern int    CoTBuse(UBYTE *);
extern int    CoTestUse(UBYTE *);
extern int    CoThreadBucket(UBYTE *);
extern int    AddComString(int,WORD *,UBYTE *,int);
extern int    CompileAlgebra(UBYTE *,int,WORD *);
extern int    IsIdStatement(UBYTE *);
extern UBYTE *IsRHS(UBYTE *,UBYTE);
extern int    ParenthesesTest(UBYTE *);
extern int    tokenize(UBYTE *,WORD);
extern void   WriteTokens(SBYTE *);
extern int    simp1token(SBYTE *);
extern int    simpwtoken(SBYTE *);
extern int    simp2token(SBYTE *);
extern int    simp3atoken(SBYTE *,int);
extern int    simp3btoken(SBYTE *,int);
extern int    simp4token(SBYTE *);
extern int    simp5token(SBYTE *,int);
extern int    simp6token(SBYTE *,int);
extern UBYTE *SkipAName(UBYTE *);
extern int    TestTables(VOID);
extern int    GetLabel(UBYTE *);
extern int    CoIdExpression(UBYTE *,int);
extern int    CoAssign(UBYTE *);
extern int    DoExpr(UBYTE *,int,int);
extern int    CompileSubExpressions(SBYTE *);
extern int    CodeGenerator(SBYTE *);
extern int    CompleteTerm(WORD *,UWORD *,UWORD *,WORD,WORD,int);
extern int    CodeFactors(SBYTE *s);
extern WORD   GenerateFactors(WORD,WORD);
extern int    InsTree(int,int);
extern int    FindTree(int,WORD *);
extern void   RedoTree(CBUF *,int);
extern void   ClearTree(int);
extern int    CatchDollar(int);
extern int    AssignDollar(PHEAD WORD *,WORD);
extern UBYTE *WriteDollarToBuffer(WORD,WORD);
extern UBYTE *WriteDollarFactorToBuffer(WORD,WORD,WORD);
extern void   AddToDollarBuffer(UBYTE *);
extern int    PutTermInDollar(WORD *,WORD);
extern void   TermAssign(WORD *);
extern void   WildDollars(PHEAD WORD *);
extern LONG   numcommute(WORD *,LONG *);
extern int    FullRenumber(PHEAD WORD *,WORD);
extern int    Lus(WORD *,WORD,WORD,WORD,WORD,WORD);
extern int    FindLus(int,int,int);
extern int    CoReplaceLoop(UBYTE *);
extern int    CoFindLoop(UBYTE *);
extern int    DoFindLoop(UBYTE *,int);
extern int    CoFunPowers(UBYTE *);
extern int    SortTheList(int *,int);
extern int    MatchIsPossible(WORD *,WORD *);
extern int    StudyPattern(WORD *);
extern WORD   DolToTensor(PHEAD WORD);
extern WORD   DolToFunction(PHEAD WORD);
extern WORD   DolToVector(PHEAD WORD);
extern WORD   DolToNumber(PHEAD WORD);
extern WORD   DolToSymbol(PHEAD WORD);
extern WORD   DolToIndex(PHEAD WORD);
extern LONG   DolToLong(PHEAD WORD);
extern int    DollarFactorize(PHEAD WORD);
extern int    CoPrintTable(UBYTE *);
extern int    CoDeallocateTable(UBYTE *);
extern void   CleanDollarFactors(DOLLARS);
extern WORD   *TakeDollarContent(PHEAD WORD *,WORD **);
extern WORD   *MakeDollarInteger(PHEAD WORD *,WORD **);
extern WORD   *MakeDollarMod(PHEAD WORD *,WORD **);
extern int    GetDolNum(PHEAD WORD *, WORD *);
extern void   AddPotModdollar(WORD);
 
extern int    Optimize(WORD, int);
extern int    ClearOptimize(VOID);
extern int    LoadOpti(WORD);
extern int    PutObject(WORD *,int);
extern void   CleanOptiBuffer(VOID);
extern int    PrintOptima(WORD);
extern int    FindScratchName(VOID);
extern WORD   MaxPowerOpti(LONG);
extern WORD   HuntNumFactor(LONG,WORD *,int);
extern WORD   HuntFactor(LONG,WORD *,int);
extern void   HuntPairs(LONG,WORD);
extern void   HuntBrackets(LONG);
extern int    AddToOpti(WORD *,int);
extern LONG   TestNewSca(LONG,WORD *,WORD *);
extern void   NormOpti(WORD *);
extern void   SortOpti(LONG);
extern void   SplitOpti(WORD **,LONG);
extern void   CombiOpti(VOID);
extern int    TakeLongRoot(UWORD *,WORD *,WORD);
extern int    TakeRatRoot(UWORD *,WORD *,WORD);
extern int    MakeRational(WORD ,WORD , WORD *, WORD *);
extern int    MakeLongRational(PHEAD UWORD *,WORD ,UWORD *,WORD ,UWORD *,WORD *);
extern void   HuntPowers(LONG,WORD);
extern void   HuntNumBrackets(LONG);
extern void   ClearTableTree(TABLES);
extern int    InsTableTree(TABLES,WORD *);
extern void   RedoTableTree(TABLES,int);
extern int    FindTableTree(TABLES,WORD *,int);
extern void   finishcbuf(WORD);
extern void   clearcbuf(WORD);
extern void   CleanUpSort(int);
extern FILEHANDLE *AllocFileHandle(WORD,char *);
extern VOID   DeAllocFileHandle(FILEHANDLE *);
extern VOID   LowerSortLevel(VOID);
extern WORD  *PolyRatFunSpecial(PHEAD WORD *, WORD *);
extern VOID   SimpleSplitMergeRec(WORD *,WORD,WORD *);
extern VOID   SimpleSplitMerge(WORD *,WORD);
extern WORD   BinarySearch(WORD *,WORD,WORD);
extern int    InsideDollar(PHEAD WORD *,WORD);
extern DOLLARS DolToTerms(PHEAD WORD);
extern WORD   EvalDoLoopArg(PHEAD WORD *,WORD);
extern int    SetExprCases(int,int,int);
extern int    TestSelect(WORD *,WORD *);
extern VOID   SubsInAll(PHEAD0);
extern VOID   TransferBuffer(int,int,int);
extern int    TakeIDfunction(PHEAD WORD *);
extern int    MakeSetupAllocs(VOID);
extern int    TryFileSetups(VOID);
extern void   ExchangeExpressions(int,int);
extern void   ExchangeDollars(int,int);
extern int    GetFirstBracket(WORD *,int);
extern int    GetFirstTerm(WORD *,int);
extern int    GetContent(WORD *,int);
extern int    CleanupTerm(WORD *);
extern WORD   ContentMerge(PHEAD WORD *,WORD *);
extern UBYTE *PreIfDollarEval(UBYTE *,int *);
extern LONG   TermsInDollar(WORD);
extern LONG   SizeOfDollar(WORD);
extern LONG   TermsInExpression(WORD);
extern LONG   SizeOfExpression(WORD);
extern WORD   *TranslateExpression(UBYTE *);
extern int    IsSetMember(WORD *,WORD);
extern int    IsMultipleOf(WORD *,WORD *);
extern int    TwoExprCompare(WORD *,WORD *,int);
extern void   UpdatePositions(VOID);
extern void   M_check(VOID);
extern void   M_print(VOID);
extern void   M_check1(VOID);
extern void   PrintTime(UBYTE *);

extern POSITION *FindBracket(WORD,WORD *);
extern VOID   PutBracketInIndex(PHEAD WORD *,POSITION *);
extern void   ClearBracketIndex(WORD);
extern VOID   OpenBracketIndex(WORD);
extern int    DoNoParallel(UBYTE *);
extern int    DoParallel(UBYTE *);
extern int    DoModSum(UBYTE *);
extern int    DoModMax(UBYTE *);
extern int    DoModMin(UBYTE *);
extern int    DoModLocal(UBYTE *);
extern UBYTE *DoModDollar(UBYTE *,int);
extern int    DoProcessBucket(UBYTE *);
extern int    DoinParallel(UBYTE *);
extern int    DonotinParallel(UBYTE *);

extern int    FlipTable(FUNCTIONS,int);
extern int    ChainIn(PHEAD WORD *,WORD);
extern int    ChainOut(PHEAD WORD *,WORD);
extern int    ArgumentImplode(PHEAD WORD *,WORD *);
extern int    ArgumentExplode(PHEAD WORD *,WORD *);
extern int    DenToFunction(WORD *,WORD);
 
extern WORD   HowMany(PHEAD WORD *,WORD *);
extern VOID   RemoveDollars(VOID);
extern LONG   CountTerms1(PHEAD0);
extern LONG   TermsInBracket(PHEAD WORD *,WORD);
extern int    Crash(VOID);

extern char  *str_dup(char *);
extern void   convertblock(INDEXBLOCK *,INDEXBLOCK *,int);
extern void   convertnamesblock(NAMESBLOCK *,NAMESBLOCK *,int);
extern void   convertiniinfo(INIINFO *,INIINFO *,int);
extern int    ReadIndex(DBASE *);
extern int    WriteIndexBlock(DBASE *,MLONG);
extern int    WriteNamesBlock(DBASE *,MLONG);
extern int    WriteIndex(DBASE *);
extern int    WriteIniInfo(DBASE *);
extern int    ReadIniInfo(DBASE *);
extern int    AddToIndex(DBASE *,MLONG);
extern DBASE *GetDbase(char *);
extern DBASE *OpenDbase(char *);
extern char  *ReadObject(DBASE *,MLONG,char *);
extern char  *ReadijObject(DBASE *,MLONG,MLONG,char *);
extern int    ExistsObject(DBASE *,MLONG,char *);
extern int    DeleteObject(DBASE *,MLONG,char *);
extern int    WriteObject(DBASE *,MLONG,char *,char *,MLONG);
extern MLONG  AddObject(DBASE *,MLONG,char *,char *);
extern int    Cleanup(DBASE *);
extern DBASE *NewDbase(char *,MLONG);
extern void   FreeTableBase(DBASE *);
extern int    ComposeTableNames(DBASE *);
extern int    PutTableNames(DBASE *);
extern MLONG  AddTableName(DBASE *,char *,TABLES);
extern MLONG  GetTableName(DBASE *,char *);
extern MLONG  FindTableNumber(DBASE *,char *);
extern int    TryEnvironment(VOID);

#ifdef WITHZLIB
extern int    SetupOutputGZIP(FILEHANDLE *);
extern int    PutOutputGZIP(FILEHANDLE *);
extern int    FlushOutputGZIP(FILEHANDLE *);
extern int    SetupAllInputGZIP(SORTING *);
extern LONG   FillInputGZIP(FILEHANDLE *,POSITION *,UBYTE *,LONG,int);
extern void   ClearSortGZIP(FILEHANDLE *f);
#endif

#ifdef WITHPTHREADS
extern VOID   BeginIdentities(VOID);
extern int    WhoAmI(VOID);
extern int    StartAllThreads(int);
extern void   StartHandleLock(VOID);
extern VOID   TerminateAllThreads(VOID);
extern int    GetAvailableThread(VOID);
extern int    ConditionalGetAvailableThread(VOID);
extern int    BalanceRunThread(PHEAD int,WORD *,WORD);
extern void   WakeupThread(int,int);
extern int    MasterWait(VOID);
extern int    InParallelProcessor(VOID);
extern int    ThreadsProcessor(EXPRESSIONS,WORD,WORD);
extern int    MasterMerge(VOID);
extern int    PutToMaster(PHEAD WORD *);
extern void   SetWorkerFiles(VOID);
extern int    MakeThreadBuckets(int,int);
extern int    SendOneBucket(int);
extern int    LoadOneThread(int,int,THREADBUCKET *,int);
extern void  *RunSortBot(void *);
extern void   MasterWaitAllSortBots(VOID);
extern int    SortBotMerge(PHEAD0);
extern int    SortBotOut(PHEAD WORD *);
extern void   DefineSortBotTree(VOID);
extern int    SortBotMasterMerge(VOID);
extern int    SortBotWait(int);
extern void   StartIdentity(VOID);
extern void   FinishIdentity(void *);
extern int    SetIdentity(int *);
extern ALLPRIVATES *InitializeOneThread(int);
extern void   FinalizeOneThread(int);
extern void   ClearAllThreads(VOID);
extern void  *RunThread(void *);
extern void   IAmAvailable(int);
extern int    ThreadWait(int);
extern int    ThreadClaimedBlock(int);
extern int    GetThread(int);
extern int    UpdateOneThread(int);
extern void   MasterWaitAll(VOID);
extern void   MasterWaitAllBlocks(VOID);
extern int    MasterWaitThread(int);
extern void   WakeupMasterFromThread(int,int);
extern int    LoadReadjusted(VOID);
extern int    IniSortBlocks(int);
extern int    TreatIndexEntry(PHEAD LONG);
extern WORD   GetTerm2(PHEAD WORD *);
extern void	SetHideFiles(VOID);

#endif

extern int    CopyExpression(FILEHANDLE *,FILEHANDLE *);

extern int    set_in(UBYTE, set_of_char);
extern one_byte set_set(UBYTE, set_of_char);
extern one_byte set_del(UBYTE, set_of_char);
extern one_byte set_sub (set_of_char, set_of_char, set_of_char);
extern int    DoPreAddSeparator(UBYTE *);
extern int    DoPreRmSeparator(UBYTE *);

/*See the file extcmd.c*/
extern int    openExternalChannel(UBYTE *,int,UBYTE *,UBYTE *);
extern int    initPresetExternalChannels(UBYTE *, int);
extern int    closeExternalChannel(int);
extern int    selectExternalChannel(int);
extern int    getCurrentExternalChannel(VOID);
extern VOID   closeAllExternalChannels(VOID);

typedef int (*WRITEBUFTOEXTCHANNEL)(char *,size_t);
typedef int (*GETCFROMEXTCHANNEL)(VOID);
typedef int (*SETTERMINATORFOREXTERNALCHANNEL)(char *);
typedef int (*SETKILLMODEFOREXTERNALCHANNEL)(int,int);
typedef LONG (*WRITEFILE)(int,UBYTE *,LONG);
typedef WORD (*GETTERM)(PHEAD WORD *);

#define CompareTerms ((COMPARE)AR.CompareRoutine)
#define FiniShuffle AN.SHvar.finishuf
#define DoShtuffle ((DO_UFFLE)AN.SHvar.do_uffle)

extern UBYTE *defineChannel(UBYTE*, HANDLERS*);
extern int    writeToChannel(int,UBYTE *,HANDLERS*);
#ifdef WITHEXTERNALCHANNEL
extern LONG   WriteToExternalChannel(int,UBYTE *,LONG);
#endif
extern int    writeBufToExtChannelOk(char *,size_t);
extern int    getcFromExtChannelOk(VOID);
extern int    setKillModeForExternalChannelOk(int,int);
extern int    setTerminatorForExternalChannelOk(char *);
extern int    getcFromExtChannelFailure(VOID);
extern int    setKillModeForExternalChannelFailure(int,int);
extern int    setTerminatorForExternalChannelFailure(char *);
extern int    writeBufToExtChannelFailure(char *,size_t);

extern int    ReleaseTB(VOID);

extern int    SymbolNormalize(WORD *);
extern int    TestFunFlag(PHEAD WORD *);
extern WORD   CompareSymbols(WORD *,WORD *,WORD);
extern WORD   CompareHSymbols(WORD *,WORD *,WORD);
extern WORD   NextPrime(PHEAD WORD);
extern UWORD  wranf(PHEAD0);
extern UWORD  iranf(PHEAD UWORD);
extern void   iniwranf(PHEAD0);
extern UBYTE *PreRandom(UBYTE *);

extern WORD *PolyNormPoly (PHEAD WORD);
extern WORD  *EvaluateGcd(PHEAD WORD *);
extern int TreatPolyRatFun(PHEAD WORD *);

extern WORD   ReadSaveHeader(VOID);
extern WORD   ReadSaveIndex(FILEINDEX *);
extern WORD   ReadSaveExpression(UBYTE *,UBYTE *,LONG *,LONG *);
extern UBYTE *ReadSaveTerm32(UBYTE *,UBYTE *,UBYTE **,UBYTE *,UBYTE *,int);
extern WORD   ReadSaveVariables(UBYTE *,UBYTE *,LONG *,LONG *,INDEXENTRY *,LONG *);
extern WORD   WriteStoreHeader(WORD);

extern void   InitRecovery(VOID);
extern int    CheckRecoveryFile(VOID);
extern void   DeleteRecoveryFile(VOID);
extern char  *RecoveryFilename(VOID);
extern int    DoRecovery(int *);
extern void   DoCheckpoint(int);

extern VOID NumberMallocAddMemory(PHEAD0);
extern VOID CacheNumberMallocAddMemory(PHEAD0);
extern VOID TermMallocAddMemory(PHEAD0);
#ifndef MEMORYMACROS
extern WORD *TermMalloc2(PHEAD char *text);
extern VOID TermFree2(PHEAD WORD *term,char *text);
extern UWORD *NumberMalloc2(PHEAD char *text);
extern UWORD *CacheNumberMalloc2(PHEAD char *text);
extern VOID NumberFree2(PHEAD UWORD *NumberMem,char *text);
extern VOID CacheNumberFree2(PHEAD UWORD *NumberMem,char *text);
#endif

extern void ExprStatus(EXPRESSIONS);
extern VOID iniTools(VOID);
extern int TestTerm(WORD *);

extern WORD RunTransform(PHEAD WORD *term, WORD *params);
extern WORD RunEncode(PHEAD WORD *fun, WORD *args, WORD *info);
extern WORD RunDecode(PHEAD WORD *fun, WORD *args, WORD *info);
extern WORD RunReplace(PHEAD WORD *fun, WORD *args, WORD *info);
extern WORD RunImplode(WORD *fun, WORD *args);
extern WORD RunExplode(PHEAD WORD *fun, WORD *args);
extern int TestArgNum(int n, int totarg, WORD *args);
extern WORD PutArgInScratch(WORD *arg,UWORD *scrat);
extern UBYTE *ReadRange(UBYTE *s, WORD *out, int par);
extern int  FindRange(PHEAD WORD *,WORD *,WORD *,WORD);
extern WORD RunPermute(PHEAD WORD *fun, WORD *args, WORD *info);
extern WORD RunReverse(PHEAD WORD *fun, WORD *args);
extern WORD RunCycle(PHEAD WORD *fun, WORD *args, WORD *info);
extern WORD RunAddArg(PHEAD WORD *fun, WORD *args);
extern WORD RunMulArg(PHEAD WORD *fun, WORD *args);
extern WORD RunIsLyndon(PHEAD WORD *fun, WORD *args, int par);
extern WORD RunToLyndon(PHEAD WORD *fun, WORD *args, int par);
extern WORD RunDropArg(PHEAD WORD *fun, WORD *args);
extern WORD RunSelectArg(PHEAD WORD *fun, WORD *args);
extern WORD RunDedup(PHEAD WORD *fun, WORD *args);

extern int NormPolyTerm(PHEAD WORD *);
extern WORD ComparePoly(WORD *, WORD *, WORD);
extern int ConvertToPoly(PHEAD WORD *, WORD *,WORD *,WORD);
extern int LocalConvertToPoly(PHEAD WORD *, WORD *, WORD,WORD);
extern int ConvertFromPoly(PHEAD WORD *, WORD *, WORD, WORD, WORD, WORD);
extern WORD FindSubterm(WORD *);
extern WORD FindLocalSubterm(PHEAD WORD *, WORD);
extern void PrintSubtermList(int,int);
extern void PrintExtraSymbol(int,WORD *,int);
extern WORD FindSubexpression(WORD *);

extern void UpdateMaxSize(VOID);

extern int CoToPolynomial(UBYTE *);
extern int CoFromPolynomial(UBYTE *);
extern int CoArgToExtraSymbol(UBYTE *);
extern int CoExtraSymbols(UBYTE *);
extern UBYTE *GetDoParam(UBYTE *, WORD **, int);
extern WORD *GetIfDollarFactor(UBYTE **, WORD *);
extern int CoDo(UBYTE *);
extern int CoEndDo(UBYTE *);
extern int ExtraSymFun(PHEAD WORD *,WORD);
extern int PruneExtraSymbols(WORD);
extern int IniFbuffer(WORD);
extern void IniFbufs(VOID);
extern int GCDfunction(PHEAD WORD *,WORD);
extern WORD *GCDfunction3(PHEAD WORD *,WORD *);
extern WORD *GCDfunction4(PHEAD WORD *,WORD *);
extern int ReadPolyRatFun(PHEAD WORD *);
extern int FromPolyRatFun(PHEAD WORD *, WORD **, WORD **);
extern void PRFnormalize(PHEAD WORD *);
extern WORD *PRFadd(PHEAD WORD *, WORD *);
extern WORD *PolyDiv(PHEAD WORD *,WORD *,char *);
extern WORD *PolyGCD(PHEAD WORD *,WORD *);
extern WORD *PolyAdd(PHEAD WORD *,WORD *);
extern void GCDclean(PHEAD WORD *, WORD *);
extern int RatFunNormalize(PHEAD WORD *);
extern WORD *TakeSymbolContent(PHEAD WORD *,WORD *);
extern int GCDterms(PHEAD WORD *,WORD *,WORD *);
extern WORD *PutExtraSymbols(PHEAD WORD *,WORD,int *);
extern WORD *TakeExtraSymbols(PHEAD WORD *,WORD);
extern WORD *MultiplyWithTerm(PHEAD WORD *, WORD *,WORD);
extern WORD *TakeContent(PHEAD WORD *, WORD *);
extern int MergeSymbolLists(PHEAD WORD *, WORD *, int);
extern int MergeDotproductLists(PHEAD WORD *, WORD *, int);
extern WORD *CreateExpression(PHEAD WORD);
extern int DIVfunction(PHEAD WORD *,WORD,int);
extern WORD *MULfunc(PHEAD WORD *, WORD *);
extern WORD *ConvertArgument(PHEAD WORD *,int *);
extern int ExpandRat(PHEAD WORD *);
extern int InvPoly(PHEAD WORD *,WORD,WORD);
extern WORD TestDoLoop(PHEAD WORD *,WORD);
extern WORD TestEndDoLoop(PHEAD WORD *,WORD);

extern WORD  *poly_gcd(PHEAD WORD *, WORD *, WORD);
extern WORD  *poly_div(PHEAD WORD *, WORD *, WORD);
extern WORD  *poly_rem(PHEAD WORD *, WORD *, WORD);
extern WORD  *poly_inverse(PHEAD WORD *, WORD *);
extern WORD  *poly_mul(PHEAD WORD *, WORD *);
extern WORD *poly_ratfun_add(PHEAD WORD *, WORD *);
extern int   poly_ratfun_normalize(PHEAD WORD *);
extern int   poly_factorize_argument(PHEAD WORD *, WORD *);
extern WORD *poly_factorize_dollar(PHEAD WORD *);
extern int   poly_factorize_expression(EXPRESSIONS);
extern int   poly_unfactorize_expression(EXPRESSIONS);
extern void  poly_free_poly_vars(PHEAD const char *);

extern VOID optimize_print_code (int);

#ifdef WITHPTHREADS
extern void find_Horner_MCTS_expand_tree();
extern void find_Horner_MCTS_expand_tree_threaded();
extern void optimize_expression_given_Horner();
extern void optimize_expression_given_Horner_threaded();
#endif
 
extern int DoPreAdd(UBYTE *s);
extern int DoPreUseDictionary(UBYTE *s);
extern int DoPreCloseDictionary(UBYTE *s);
extern int DoPreOpenDictionary(UBYTE *s);
extern void RemoveDictionary(DICTIONARY *dict);
extern void UnSetDictionary(VOID);
extern int SetDictionaryOptions(UBYTE *options);
extern int SelectDictionary(UBYTE *name,UBYTE *options);
extern int AddToDictionary(DICTIONARY *dict,UBYTE *left,UBYTE *right);
extern int AddDictionary(UBYTE *name);
extern int FindDictionary(UBYTE *name);
extern int IsExponentSign(VOID);
extern int IsMultiplySign(VOID);
extern VOID TransformRational(UWORD *a, WORD na);
extern void WriteDictionary(DICTIONARY *);
extern void ShrinkDictionary(DICTIONARY *);
extern void MultiplyToLine(VOID);
extern UBYTE *FindSymbol(WORD num);
extern UBYTE *FindVector(WORD num);
extern UBYTE *FindIndex(WORD num);
extern UBYTE *FindFunction(WORD num);
extern UBYTE *FindFunWithArgs(WORD *t);
extern UBYTE *FindExtraSymbol(WORD num);
extern LONG DictToBytes(DICTIONARY *dict,UBYTE *buf);
extern DICTIONARY *DictFromBytes(UBYTE *buf);
extern int CoCreateSpectator(UBYTE *inp);
extern int CoToSpectator(UBYTE *inp);
extern int CoRemoveSpectator(UBYTE *inp);
extern int CoEmptySpectator(UBYTE *inp);
extern int CoCopySpectator(UBYTE *inp);
extern int PutInSpectator(WORD *,WORD);
extern void ClearSpectators(WORD);
extern WORD GetFromSpectator(WORD *,WORD);
extern void FlushSpectators(VOID);

extern WORD *PreGCD(PHEAD WORD *, WORD *,int);
extern WORD *FindCommonVariables(PHEAD int,int);
extern VOID AddToSymbolList(PHEAD WORD);
extern int AddToListPoly(PHEAD0);
extern int InvPoly(PHEAD WORD *,WORD,WORD);

extern int ReadFromScratch(FILEHANDLE *,POSITION *,UBYTE *,POSITION *);
extern int AddToScratch(FILEHANDLE *,POSITION *,UBYTE *,POSITION *,int);

extern int DoPreAppendPath(UBYTE *);
extern int DoPrePrependPath(UBYTE *);

extern int DoSwitch(PHEAD WORD *, WORD *);
extern int DoEndSwitch(PHEAD WORD *, WORD *);
extern SWITCHTABLE *FindCase(WORD, WORD);
extern VOID SwitchSplitMergeRec(SWITCHTABLE *, WORD, SWITCHTABLE *);
extern VOID SwitchSplitMerge(SWITCHTABLE *, WORD);
extern int DoubleSwitchBuffers(VOID);


/*
  	#] Declarations : 
*/
#endif
