#ifndef __FDECLARE__

#define __FDECLARE__

/** @file declare.h
 *
 *  Contains macros and function declarations.
 */

/*
  	#[ ARG definitions :
*/

#ifdef ANSI
/**
 *	First the declaration macro's. They are to keep the code portable.
 *	They wouldn't be needed ordinarily, but there is such a thing as
 *	the "IBM interpretation of the ANSI standard".
 *	In the old days there was also the difference between K&R C and Ansi C.
 *	By using macro's for the definition of functions we ensure that we
 *	don't need all kinds of ifdef's for each function we define.
 *
 *	There are two types of macro's. The ones that are called ARG0, ARG1, ...
 *	and the ones that are called BARG0, BARG1, ...
 *	The last ones were introduced when TFORM was programmed. In the case of
 *	workers, each worker may need some private data. These can in principle
 *	be accessed by some posix calls but that is unnecessarily slow. The
 *	passing of a pointer to the complete data struct with private data will
 *	be much faster. And anyway, there would have to be a macro that either
 *	makes the posix call (TFORM) or doesn't (FORM). The solution by having
 *	macro's that either pass the pointer (TFORM) or don't pass it (FORM)
 *	is seen as the best solution.
 *
 *	In the declarations and the calling of the functions we have to use
 *	either the PHEAD or the BHEAD macro if the pointer is to be passed.
 *	These macro's contain the comma as well. Hence we need special macro's
 *	if there are no other arguments. These are called PHEAD0 and BHEAD0.
 */

#ifdef INTELCOMPILER
#define ARG0 ()
#else
#define ARG0 (VOID)
#endif
#define ARG1(x1,y1) (x1 y1)
#define ARG2(x1,y1,x2,y2) (x1 y1,x2 y2)
#define ARG3(x1,y1,x2,y2,x3,y3) (x1 y1,x2 y2,x3 y3)
#define ARG4(x1,y1,x2,y2,x3,y3,x4,y4) (x1 y1,x2 y2,x3 y3,x4 y4)
#define ARG5(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5) (x1 y1,x2 y2,x3 y3,x4 y4,x5 y5)
#define ARG6(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6)
#define ARG7(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7)
#define ARG8(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8)
#define ARG9(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8,x9 y9)
#define ARG10(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9,xa,ya) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8,x9 y9,xa ya)

#ifdef WITHPTHREADS
#define BARG0 (ALLPRIVATES *B)
#define BARG1(x1,y1) (ALLPRIVATES *B,x1 y1)
#define BARG2(x1,y1,x2,y2) (ALLPRIVATES *B,x1 y1,x2 y2)
#define BARG3(x1,y1,x2,y2,x3,y3) (ALLPRIVATES *B,x1 y1,x2 y2,x3 y3)
#define BARG4(x1,y1,x2,y2,x3,y3,x4,y4) (ALLPRIVATES *B,x1 y1,x2 y2,x3 y3,x4 y4)
#define BARG5(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5) (ALLPRIVATES *B,x1 y1,x2 y2,x3 y3,x4 y4,x5 y5)
#define BARG6(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6) \
			(ALLPRIVATES *B,x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6)
#define BARG7(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7) \
			(ALLPRIVATES *B,x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7)
#define BARG8(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8) \
			(ALLPRIVATES *B,x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8)
#define BARG9(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9) \
			(ALLPRIVATES *B,x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8,x9 y9)
#define BARG10(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9,xa,ya) \
			(ALLPRIVATES *B,x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8,x9 y9,xa ya)
#else
#define BARG0 ARG0
#define BARG1(x1,y1) (x1 y1)
#define BARG2(x1,y1,x2,y2) (x1 y1,x2 y2)
#define BARG3(x1,y1,x2,y2,x3,y3) (x1 y1,x2 y2,x3 y3)
#define BARG4(x1,y1,x2,y2,x3,y3,x4,y4) (x1 y1,x2 y2,x3 y3,x4 y4)
#define BARG5(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5) (x1 y1,x2 y2,x3 y3,x4 y4,x5 y5)
#define BARG6(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6)
#define BARG7(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7)
#define BARG8(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8)
#define BARG9(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8,x9 y9)
#define BARG10(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9,xa,ya) \
			(x1 y1,x2 y2,x3 y3,x4 y4,x5 y5,x6 y6,x7 y7,x8 y8,x9 y9,xa ya)
#endif
 
#else

#define ARG0 ()
#define ARG1(x1,y1) (y1) x1 y1;
#define ARG2(x1,y1,x2,y2) (y1,y2) x1 y1;x2 y2;
#define ARG3(x1,y1,x2,y2,x3,y3) (y1,y2,y3) x1 y1;x2 y2;x3 y3;
#define ARG4(x1,y1,x2,y2,x3,y3,x4,y4) (y1,y2,y3,y4) x1 y1;x2 y2;x3 y3;x4 y4;
#define ARG5(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5) (y1,y2,y3,y4,y5) \
			 x1 y1;x2 y2;x3 y3;x4 y4;x5 y5;
#define ARG6(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6) (y1,y2,y3,y4,y5,y6) \
			 x1 y1;x2 y2;x3 y3;x4 y4;x5 y5;x6 y6;
#define ARG7(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7) \
			(y1,y2,y3,y4,y5,y6,y7) \
			 x1 y1;x2 y2;x3 y3;x4 y4;x5 y5;x6 y6;x7 y7;
#define ARG8(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8) \
			(y1,y2,y3,y4,y5,y6,y7,y8) \
			 x1 y1;x2 y2;x3 y3;x4 y4;x5 y5;x6 y6;x7 y7;x8 y8;
#define ARG9(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9) \
			(y1,y2,y3,y4,y5,y6,y7,y8,y9) \
			 x1 y1;x2 y2;x3 y3;x4 y4;x5 y5;x6 y6;x7 y7;x8 y8;x9 y9;
#define ARG10(x1,y1,x2,y2,x3,y3,x4,y4,x5,y5,x6,y6,x7,y7,x8,y8,x9,y9,xa,ya) \
			(y1,y2,y3,y4,y5,y6,y7,y8,y9,ya) \
			 x1 y1;x2 y2;x3 y3;x4 y4;x5 y5;x6 y6;x7 y7;x8 y8;x9 y9;xa ya;

#endif
/*
  	#] ARG definitions :
  	#[ Macro's :
*/

#define MaX(x,y) ((x) > (y) ? (x): (y))
#define MiN(x,y) ((x) < (y) ? (x): (y))
#define ABS(x) ( (x) < 0 ? -(x): (x) )
#define REDLENG(x) ((((x)<0)?((x)+1):((x)-1))>>1)
#define INCLENG(x) (((x)<0)?(((x)<<1)-1):(((x)<<1)+1))
#define GETCOEF(x,y) x += *x;y = x[-1];x -= ABS(y);y=REDLENG(y)
#define GETSTOP(x,y) y=x+(*x)-1;y -= ABS(*y)-1
#define MULWWL(x,y) (x)*(y)
#define MULLONG1(b,nb,norm,e,ne,i,t,v) {                   \
		for ( i = 0, t = 0, v = (RLONG)norm; i < nb; i++ ) \
		  { t += b[i] * v; e[i] = t; t >>= BITSINWORD; }   \
		ne = nb; if ( t ) e[ne++] = t;     }
 
#define TOKENTOLINE(x,y) if ( AC.OutputSpaces == NOSPACEFORMAT ) { \
		TokenToLine((UBYTE *)(y)); } else { TokenToLine((UBYTE *)(x)); }

#define UngetFromStream(stream,c) ((stream)->nextchar[(stream)->isnextchar++]=c)
#define StreamPosition(stream) ((stream)->bufferposition + \
	((stream)->pointer - (stream)->buffer))
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

#define AddToCB(c,wx) if(c->Pointer>=c->Top) \
		DoubleCbuffer(c-cbuf,c->Pointer); \
		*(c->Pointer)++ = wx;

#define EXCHINOUT { FILEHANDLE *ffFi = AR.outfile; \
	AR.outfile = AR.infile; AR.infile = ffFi; }
#define BACKINOUT { FILEHANDLE *ffFi = AR.outfile; POSITION posi; \
	AR.outfile = AR.infile; AR.infile = ffFi; \
	SetEndScratch(AR.infile,&posi); }

#if ARGHEAD > 2
#define FILLARG(w) { int i = ARGHEAD-2; while ( --i >= 0 ) *w++ = 0; }
#define COPYARG(w,t) { int i = ARGHEAD-2; while ( --i >= 0 ) *w++ = *t++; }
#else
#define FILLARG(w)
#define COPYARG(w,t)
#endif

#if FUNHEAD > 2
#define FILLFUN(w) { *w++ = 0; FILLFUN3(w) }
#else
#define FILLFUN(w)
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
#define GETSETNUM(y) (((y)[2]==4)?((LONG)(y)[4]):\
	((((LONG)(y)[4])<<(BITSINWORD-1))+(LONG)(y)[5]))

#define TABLESIZE(a,b) (((WORD)sizeof(a))/((WORD)sizeof(b)))
#define WORDDIF(x,y) (WORD)(x-y)
#define POINDIF(x,y) (WORD)(x-y)
#define LONGDIF(x,y) (WORD)(x-y)
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
#define ISPOSPOS(pp) ( (pp).p1 > 0 )
#define ISNEGPOS(pp) ( (pp).p1 < 0 )
extern VOID TELLFILE(int,POSITION *);

#define TOLONG(x) ((LONG)(x))
#define StrIcmp(x,y) StrICont((UBYTE *)(y),(UBYTE *)(x))

#define Add2Com(x) { WORD cod[2]; cod[0] = x; cod[1] = 2; AddNtoL(2,cod); }
#define Add3Com(x1,x2) { WORD cod[3]; cod[0] = x1; cod[1] = 3; cod[2] = x2; AddNtoL(3,cod); }
#define Add4Com(x1,x2,x3) { WORD cod[4]; cod[0] = x1; cod[1] = 4; \
   cod[2] = x2; cod[3] = x3; AddNtoL(4,cod); }
#define Add5Com(x1,x2,x3,x4) { WORD cod[5]; cod[0] = x1; cod[1] = 5; \
   cod[2] = x2; cod[3] = x3; cod[4] = x4; AddNtoL(5,cod); }

#define WantAddPointers(x) while((AT.pWorkPointer+(x))>AR.pWorkSize)\
	ExpandBuffer((void **)(&AT.pWorkSpace),&AR.pWorkSize,sizeof(WORD *))
#define WantAddLongs(x) while((AT.lWorkPointer+(x))>AR.lWorkSize)\
	ExpandBuffer((void **)(&AT.lWorkSpace),&AR.lWorkSize,sizeof(LONG))
#define WantAddPositions(x) while((AT.posWorkPointer+(x))>AR.posWorkSize)\
	ExpandBuffer((void **)(&AT.posWorkSpace),&AR.posWorkSize,sizeof(POSITION))

/*
  	#] Macro's :
  	#[ Thread objects :
*/

#ifdef WITHPTHREADS

#define EXTERNLOCK(x) extern pthread_mutex_t x;
#define INILOCK(x)    pthread_mutex_t x = PTHREAD_MUTEX_INITIALIZER
#ifdef DEBUGGINGLOCKS
#include <asm/errno.h>
#define LOCK(x)       while ( pthread_mutex_trylock(&(x)) == EBUSY ) {}
#else
#define LOCK(x)       pthread_mutex_lock(&(x))
#endif
#define UNLOCK(x)     pthread_mutex_unlock(&(x))
#define GETBIDENTITY
#ifdef ITHREADS
#define GETIDENTITY   int identity = WhoAmI();
#else
#define GETIDENTITY   int identity = WhoAmI(); ALLPRIVATES *B = AB[identity];
#endif
#else

#define EXTERNLOCK(x)
#define INILOCK(x)
#define LOCK(x) 
#define UNLOCK(x)
#define GETIDENTITY
#define GETBIDENTITY

#endif

/*
  	#] Thread objects :
  	#[ Declarations :
*/

/**
 *	All functions (well, nearly all) are declared here.
 */

extern VOID   StartVariables();
extern VOID   setSignalHandlers();
extern UBYTE *CodeToLine(WORD,UBYTE *);
extern INDEXENTRY *FindInIndex(WORD,FILEDATA *,WORD);
extern UBYTE *GetLine(UBYTE *,LONG,WORD);
extern UBYTE *GetOneSet(UBYTE *,WORD *,WORD *,WORD *,WORD *,WORD);
extern INDEXENTRY *NextFileIndex(POSITION *);
extern UBYTE *NextWord(UBYTE *);
extern WORD  *PasteTerm(PHEAD WORD,WORD *,WORD *,WORD,WORD);
extern UBYTE *SkipExpression(UBYTE *,WORD);
extern UBYTE *SkipLine(UBYTE *);
extern UBYTE *SkipName(UBYTE *);
extern UBYTE *SkipOneName(UBYTE *);
extern UBYTE *SkipSet(UBYTE *);
extern UBYTE *StrCopy(UBYTE *,UBYTE *);
extern UBYTE *WrtPower(UBYTE *,WORD);
extern WORD   AccumGCD(UWORD *,WORD *,UWORD *,WORD);
extern VOID   AddArgs(PHEAD WORD *,WORD *,WORD *);
extern WORD   AddCoef(PHEAD WORD **,WORD **);
extern WORD   AddLong(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   AddPLon(UWORD *,WORD,UWORD *,WORD,UWORD *,UWORD *);
extern WORD   AddPoly(PHEAD WORD **,WORD **);
extern WORD   AddRat(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern VOID   AddToLine(UBYTE *);
extern WORD   AddWild(PHEAD WORD,WORD,WORD);
extern WORD   BigLong(UWORD *,WORD,UWORD *,WORD);
extern WORD   BinomGen(WORD *,WORD,WORD **,WORD,WORD,WORD,WORD,WORD,UWORD *,WORD);
extern VOID   Cconout(WORD);
extern WORD   CheckWild(PHEAD WORD,WORD,WORD,WORD *);
extern WORD   Chisholm(PHEAD WORD *,WORD);
extern WORD   CleanExpr(WORD);
extern VOID   CleanUp(WORD);
extern VOID   ClearWild(PHEAD0);
extern WORD   Commute(WORD *,WORD *);
extern WORD   DetCommu(WORD *);
extern int    CompArg(WORD *,WORD *);
extern WORD   CompCoef(WORD *, WORD *);
extern WORD   CompGroup(PHEAD WORD,WORD **,WORD *,WORD *,WORD);
extern WORD   CompWord(UBYTE *,char *);
extern WORD   Compare1(PHEAD WORD *,WORD *,WORD);
extern WORD   CopyToComp();
extern WORD   CountDo(WORD *,WORD *);
extern WORD   CountFun(WORD *,WORD *);
extern WORD   Deferred(PHEAD WORD *,WORD);
extern WORD   DeleteStore(WORD);
extern WORD   DetCurDum(WORD *);
extern VOID   DetVars(WORD *,WORD);
extern WORD   Distribute(DISTRIBUTE *,WORD);
extern WORD   DivLong(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *,UWORD *,WORD *);
extern WORD   DivRat(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   Divvy(PHEAD UWORD *,WORD *,UWORD *,WORD);
extern WORD   DoDelta(WORD *);
extern WORD   DoDelta3(WORD *,WORD);
extern WORD   DoTableExpansion(WORD *,WORD);
extern WORD   DoDistrib(PHEAD WORD *,WORD);
extern WORD   DoMerge(WORD *,WORD,WORD,WORD);
extern WORD   TestUse(WORD *,WORD);
extern DBASE *FindTB(UBYTE *);
extern int    CheckTableDeclarations(DBASE *);
extern WORD   Apply(WORD *,WORD);
extern int    ApplyExec(WORD *,int,WORD);
extern WORD   ApplyReset(WORD);
extern WORD   TableReset();
extern VOID   ReWorkT(WORD *,WORD *,WORD);
extern WORD   DoIfStatement(WORD *,WORD *);
extern WORD   DoModule();
extern WORD   DoOnePow(WORD *,WORD,WORD,WORD *,WORD *,WORD,WORD *);
extern void   DoRevert(WORD *,WORD *);
extern WORD   DoSumF1(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   DoSumF2(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   DoTheta(WORD *);
extern LONG   EndSort(WORD *,int);
extern WORD   EntVar(WORD,UBYTE *,WORD,WORD,WORD);
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
extern VOID   FiniLine();
extern WORD   FiniTerm(PHEAD WORD *,WORD *,WORD *,WORD,WORD);
extern VOID   FlushCon();
extern WORD   FlushOut(POSITION *,FILEHANDLE *,int);
extern VOID   FunLevel(PHEAD WORD *);
extern VOID   GarbHand();
extern WORD   GcdLong(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern VOID   GCD(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern ULONG  GCD2(ULONG,ULONG);
extern WORD   Generator(PHEAD WORD *,WORD);
extern LONG   Get1Long(UBYTE *);
extern WORD   GetBinom(UWORD *,WORD *,WORD,WORD);
extern WORD   GetFromStore(WORD *,POSITION *,RENUMBER,WORD *,WORD);
extern WORD   GetIchar();
extern WORD   GetIword(UBYTE *,WORD);
extern WORD   GetLong(UBYTE *,UWORD *,WORD *);
extern WORD   GetMoreTerms(WORD *);
extern WORD   GetMoreFromMem(WORD *,WORD **);
extern WORD   GetObject(UBYTE *,WORD *,WORD *,WORD);
extern WORD   GetOneTerm(PHEAD WORD *,FILEHANDLE *,POSITION *,int);
extern WORD   GetOption(UBYTE **);
extern WORD   GetParams();
extern RENUMBER GetTable(WORD,POSITION *);
extern WORD   GetTerm(PHEAD WORD *);
extern WORD   GetWithEcho();
extern WORD   Glue(PHEAD WORD *,WORD *,WORD *,WORD);
extern WORD   InFunction(WORD *,WORD *);
extern WORD   IncLHS();
extern WORD   IncRHS(WORD);
extern VOID   IniGlob();
extern VOID   IniLine();
extern WORD   IniVars();
extern VOID   Init2();
extern VOID   Initialize();
extern WORD   InsertTerm(PHEAD WORD *,WORD,WORD,WORD *,WORD *,WORD);
extern WORD   Kraak(UBYTE *,WORD **,WORD *,WORD,WORD);
extern VOID   LongToLine(UWORD *,WORD);
extern WORD   LookAhead();
extern WORD   MakeDirty(WORD *,WORD *,WORD);
extern VOID   MarkDirty(WORD *,WORD);
extern WORD   MakeModTable();
extern WORD   MatchE(WORD *,WORD *,WORD *,WORD);
extern int    MatchCy(WORD *,WORD *,WORD *,WORD);
extern int    FunMatchCy(WORD *,WORD *,WORD *,WORD);
extern int    FunMatchSy(WORD *,WORD *,WORD *,WORD);
extern int    MatchArgument(WORD *,WORD *);
extern WORD   MatchFunction(PHEAD WORD *,WORD *,WORD *);
extern WORD   MergePatches(WORD);
extern WORD   MesCerr(char *, UBYTE *);
extern WORD   MesComp(char *, UBYTE *, UBYTE *);
extern WORD   MesLong(char *);
extern WORD   MesPar(WORD);
extern WORD   MesSet(WORD);
extern WORD   Modulus(WORD *);
extern VOID   MoveDummies(PHEAD WORD *,WORD);
extern WORD   MulLong(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   MulRat(PHEAD UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern WORD   Mully(PHEAD UWORD *,WORD *,UWORD *,WORD);
extern WORD   MultDo(WORD *,WORD *);
extern WORD   NewSort();
extern WORD   ExtraSymbol(WORD,WORD,WORD,WORD *);
extern WORD   Normalize(PHEAD WORD *);
extern WORD   OpenTemp();
extern VOID   Pack(UWORD *,WORD *,UWORD *,WORD );
extern LONG   PasteFile(WORD,WORD *,POSITION *,WORD **,RENUMBER,WORD *,WORD);
extern WORD   Permute(PERM *,WORD);
extern WORD   PolyFunMul(PHEAD WORD *);
extern WORD   PopVariables();
extern WORD   PrepPoly(WORD *);
extern WORD   Processor();
extern WORD   Product(UWORD *,WORD *,WORD);
extern VOID   PrtLong(UWORD *,WORD,UBYTE *);
extern VOID   PrtTerms();
extern VOID   PrintRunningTime();
extern WORD   PutBracket(PHEAD WORD *);
extern LONG   PutIn(FILEHANDLE *,POSITION *,WORD *,WORD **,int);
extern WORD   PutInStore(INDEXENTRY *,WORD);
extern WORD   PutOut(PHEAD WORD *,POSITION *,FILEHANDLE *,WORD);
extern UWORD  Quotient(UWORD *,WORD *,WORD);
extern WORD   RaisPow(PHEAD UWORD *,WORD *,UWORD);
extern VOID   RatToLine(UWORD *,WORD);
extern WORD   RatioFind(PHEAD WORD *,WORD *);
extern WORD   RatioGen(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   ReNumber(PHEAD WORD *);
extern WORD   ReadLHS();
extern WORD   ReadRHS(WORD);
extern WORD   ReadSnum(UBYTE **);
extern WORD   Remain10(UWORD *,WORD *);
extern WORD   Remain4(UWORD *,WORD *);
extern WORD   ResetScratch();
extern WORD   ResolveSet(WORD *,WORD *,WORD *);
extern WORD   Reverse5(WORD *term,WORD level);
extern WORD   RevertScratch();
extern WORD   ScanFunctions(PHEAD WORD *,WORD *,WORD);
extern VOID   SeekScratch(FILEHANDLE *,POSITION *);
extern VOID   SetEndScratch(FILEHANDLE *,POSITION *);
extern VOID   SetEndHScratch(FILEHANDLE *,POSITION *);
extern WORD   SetFileIndex();
extern WORD   SetParams();
extern WORD   Sflush(FILEHANDLE *);
extern WORD   Simplify(PHEAD UWORD *,WORD *,UWORD *,WORD *);
extern WORD   SkipWhite();
extern WORD   SortWild(WORD *,WORD);
extern FILE  *LocateBase(char **,char **);
#ifdef NEWSPLITMERGE
extern LONG   SplitMerge(PHEAD WORD **,LONG);
#else
extern VOID   SplitMerge(PHEAD WORD **,LONG);
#endif
extern WORD   StoreTerm(PHEAD WORD *);
extern WORD   StrCcmp(UBYTE *, char *);
extern VOID   StrNcop(UBYTE *, UBYTE *, WORD);
extern WORD   SubEval(UBYTE **);
extern VOID   SubPLon(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *);
extern VOID   Substitute(PHEAD WORD *,WORD *,WORD);
extern WORD   SymFind(PHEAD WORD *,WORD *);
extern WORD   SymGen(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   Symmetrize(PHEAD WORD *,WORD *,WORD,WORD,WORD);
extern int    FullSymmetrize(WORD *,int);
extern WORD   TakeModulus(UWORD *,WORD *,WORD *,WORD,WORD);
extern VOID   TalToLine(UWORD);
extern WORD   TenVec(PHEAD WORD *,WORD *,WORD,WORD);
extern WORD   TenVecFind(PHEAD WORD *,WORD *);
extern WORD   TermRenumber(WORD *,RENUMBER,WORD);
extern VOID   TestDrop();
extern WORD   TestMatch(PHEAD WORD *,WORD *);
extern WORD   TestSub(PHEAD WORD *,WORD);
extern LONG   TimeCPU(WORD);
extern LONG   TimeChildren(WORD);
extern LONG   TimeWallClock(WORD);
extern LONG   Timer(int);
extern LONG   GetWorkerTimes();
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
extern WORD   TryDo(WORD *,WORD *,WORD);
extern VOID   UnDefine(WORD);
extern VOID   UnPack(UWORD *,WORD,WORD *,WORD *);
extern WORD   Unique(UBYTE *,UBYTE *,WORD);
extern WORD   VarStore(UBYTE *,WORD,WORD,WORD);
extern VOID   W_S_L(WORD,LONG);
extern VOID   W_S_S(WORD,UBYTE *);
extern WORD   WildFill(PHEAD WORD *,WORD *,WORD *);
extern WORD   WriteAll();
extern WORD   WriteOne(UBYTE *,int,int);
extern VOID   WriteArgument(WORD *);
extern WORD   WriteExpression(WORD *,LONG);
extern WORD   WriteInnerTerm(WORD *,WORD);
extern VOID   WriteLists();
extern VOID   WriteSetup();
extern VOID   WriteStats(POSITION *,WORD);
extern WORD   WriteSubTerm(WORD *,WORD);
extern WORD   WriteTerm(WORD *,WORD *,WORD,WORD,WORD);
extern WORD   execarg(WORD *,WORD);
extern WORD   execterm(WORD *,WORD);
extern WORD   execnorm(WORD *,WORD);
extern VOID   SpecialCleanup(PHEAD0);

typedef WORD (*WCW)(EXPRESSIONS,WORD);

extern UBYTE *strDup(UBYTE *);
extern WORD   AddSetElement(EXPRESSIONS,LONG);
extern WORD   AddToHide();
extern LONG   CloseFormFile(UBYTE *,WORD);
extern LONG   CreateFormFile(UBYTE *,WORD);
extern WORD   DoExecute(WORD,WORD);
extern WORD   DoSpolynomial(EXPRESSIONS,WORD,WORD,WORD *);
extern WORD   ElimOneVar(EXPRESSIONS,WORD);
extern WORD   FromHide();
extern WORD   Groebner(WORD);
extern WORD   LinSolve(WORD);
extern WORD   MakeCommercial(UBYTE *,LONG,LONG);
extern WORD   MakeSubs(WORD *,WORD,WORD);
extern WORD   MulSetBracket(WORD *,LONG,WORD *,WORD,LONG,LONG);
extern WORD   NormalForm();
extern LONG   OpenFormFile(UBYTE *,WORD);
extern VOID   SetScratch(FILEHANDLE *,POSITION *);
extern WORD   SortByWeight(EXPRESSIONS,WORD);
extern WORD   Spolynomial(EXPRESSIONS,LONG,LONG,LONG,LONG,WORD *,WORD *,WORD *,WORD);
extern WORD   StepExpressions(WORD,WORD);
extern WORD   StepGroebner(EXPRESSIONS,WORD);
extern WORD   StepNormalForm(EXPRESSIONS,WORD);
extern WORD   TestLinearWeights(UBYTE *);
extern WORD   TestOneLinearWeight(WORD);
extern WORD   ToHide();
extern WORD   ToTriangle(EXPRESSIONS,WORD);
extern VOID   Warning(char *);
extern VOID   HighWarning(char *);
extern LONG   WriteFormFile(UBYTE *,WORD);
extern int    SpareTable(TABLES);

extern UBYTE *strDup1(UBYTE *,char *);
extern VOID  *Malloc(LONG);
extern VOID  *Malloc1(LONG,char *);
extern int    DoTail(int,UBYTE **);
extern int    OpenInput();
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
extern int    ProcessOption(UBYTE *,UBYTE *,int);
extern int    DoSetups();
extern VOID   Terminate(int);
extern NAMENODE *GetNode(NAMETREE *,UBYTE *);
extern int    AddName(NAMETREE *,UBYTE *,WORD,WORD,int *);
extern int    GetName(NAMETREE *,UBYTE *,WORD *,int);
extern int    GetAutoName(UBYTE *,WORD *);
extern int    GetVar(UBYTE *,WORD *,WORD *,int,int);
extern int    MakeDubious(NAMETREE *,UBYTE *,WORD *);
extern int    GetOName(NAMETREE *,UBYTE *,WORD *,int);
extern VOID   DumpTree(NAMETREE *);
extern VOID   DumpNode(NAMETREE *,WORD,WORD);
extern VOID   LinkTree(NAMETREE *,WORD,WORD);
extern VOID   CopyTree(NAMETREE *,NAMETREE *,WORD);
extern int    CompactifyTree(NAMETREE *);
extern NAMETREE *MakeNameTree();
extern VOID   FreeNameTree(NAMETREE *);
extern int    AddExpression(UBYTE *,int,int);
extern int    AddSymbol(UBYTE *,int,int,int);
extern int    AddDollar(UBYTE *,WORD,WORD *,LONG);
extern int    ReplaceDollar(WORD,WORD,WORD *,LONG);
extern int    DollarRaiseLow(UBYTE *,LONG);
extern int    AddVector(UBYTE *,int);
extern int    AddDubious(UBYTE *);
extern int    AddIndex(UBYTE *,int,int);
extern UBYTE *DoDimension(UBYTE *,int *,int *);
extern int    AddFunction(UBYTE *,int,int,int,int);
extern int    CoFunction(UBYTE *,int,int);
extern int    AddSet(UBYTE *);
extern int    DoElements(UBYTE *,SETS,UBYTE *);
extern int    DoTempSet(UBYTE *,UBYTE *);
extern int    NameConflict(int,UBYTE *);
extern int    OpenFile(char *);
extern int    OpenAddFile(char *);
extern int    CreateFile(char *);
extern int    CreateLogFile(char *);
extern VOID   CloseFile(int);
extern int    CreateHandle();
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
extern int    GetChannel(char *);
extern int    GetAppendChannel(char *);
extern int    CloseChannel(char *);
extern VOID   inictable();
extern KEYWORD *findcommand(UBYTE *);
extern int    inicbufs();
extern VOID   StartFiles();
extern UBYTE *MakeDate();
extern VOID   PreProcessor();
extern VOID  *FromList(LIST *);
extern VOID  *From0List(LIST *);
extern VOID  *FromVarList(LIST *);
extern int    DoubleList(VOID ***,int *,int,char *);
extern int    DoubleLList(VOID ***,LONG *,int,char *);
extern void   DoubleBuffer(void **,void **,int,char *);
extern void   ExpandBuffer(void **,LONG *,int);
extern LONG   iexp(LONG,int);
extern int    IsLikeVector(WORD *);
extern int    CompareArgs(WORD *,WORD *);
extern UBYTE *SkipField(UBYTE *,int);
extern int    StrCmp(UBYTE *,UBYTE *);
extern int    StrICmp(UBYTE *,UBYTE *);
extern int    StrHICmp(UBYTE *,UBYTE *);
extern int    StrICont(UBYTE *,UBYTE *);
extern int    ConWord(UBYTE *,UBYTE *);
extern int    StrLen(UBYTE *);
extern UBYTE *GetPreVar(UBYTE *,int);
extern void   ToGeneral(WORD *,WORD *,WORD);
extern int    ToFast(WORD *,WORD *);
extern SETUPPARAMETERS *GetSetupPar(UBYTE *);
extern int    RecalcSetups();
extern int    AllocSetups();
extern SORTING *AllocSort(LONG,LONG,LONG,LONG,int,int,LONG);
extern int    AllocScratchBuffers();
extern VOID   AllocSortFileName(SORTING *);
extern UBYTE *LoadInputFile(UBYTE *,int);
extern UBYTE  GetInput();
extern VOID   ClearPushback();
extern UBYTE  GetChar(int);
extern VOID   CharOut(UBYTE);
extern VOID   UnsetAllowDelay();
extern VOID   PopPreVars(int);
extern VOID   IniModule(int);
extern VOID   IniSpecialModule(int);
extern int    ModuleInstruction(int *,int *);
extern int    PreProInstruction();
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
extern int    DoMessage(UBYTE *);
extern int    DoPreNormPoly(UBYTE *);
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
extern VOID   StartPrepro();
extern int    DoIfdef(UBYTE *,int);
extern int    DoElse(UBYTE *);
extern int    DoElseif(UBYTE *);
extern int    DoEndif(UBYTE *);
extern int    DoTerminate(UBYTE *);
extern int    DoIf(UBYTE *);
extern int    DoCall(UBYTE *);
extern int    DoDebug(UBYTE *);
extern int    DoDo(UBYTE *);
extern int    DoEnddo(UBYTE *);
extern int    DoEndprocedure(UBYTE *);
extern int    DoProcedure(UBYTE *);
extern int    DoPrePrint(UBYTE *);
extern int    DoPreWrite(UBYTE *);
extern int    DoPreClose(UBYTE *);
extern int    DoPreRemove(UBYTE *);
extern int    DoCommentChar(UBYTE *);
extern int    DoPrcExtension(UBYTE *);
extern VOID   WriteString(int,UBYTE *,int);
extern VOID   WriteUnfinString(int,UBYTE *,int);
extern UBYTE *PreCalc();
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
extern VOID   MakeGlobal();
extern int    ExecModule(int);
extern int    ExecStore();
extern VOID   FullCleanUp();
extern int    DoExecStatement();
extern int    DoPipeStatement();
extern int    DoPolyfun(UBYTE *);
extern int    DoPolyratfun(UBYTE *);
extern int    CompileStatement(UBYTE *);
extern UBYTE *ToToken(UBYTE *);
extern int    GetDollar(UBYTE *);
extern int    PutDollar(UBYTE *);
extern int    MesWork();
extern int    MesPrint(char *,...);
extern int    MesCall(char *);
extern int    Mes1arr(char *);
extern int    Mes2arr(char *);
extern UBYTE *NumCopy(WORD,UBYTE *);
extern char  *LongCopy(LONG,char *);
extern char  *LongLongCopy(off_t *,char *);
extern VOID   ReserveTempFiles(int);
extern VOID   PrintTerm(WORD *,char *);
extern VOID   PrintTermC(WORD *,char *);
extern VOID   PrintSubTerm(WORD *,char *);
extern VOID   PrintWords(WORD *,LONG);
extern int    ExpandTripleDots();
extern LONG   ComPress(WORD **,LONG *);
extern VOID   StageSort(FILEHANDLE *);

#define M_alloc(x)      malloc((size_t)(x))

extern void   M_free(VOID *,char *);
extern void   ClearWildcardNames();
extern int    AddWildcardName(UBYTE *);
extern int    GetWildcardName(UBYTE *);
extern void   Globalize(int);
extern void   ResetVariables(int);
extern void   AddToPreTypes(int);
extern void   MessPreNesting(int);
extern LONG   GetStreamPosition(STREAM *);
extern WORD  *DoubleCbuffer(int,WORD *);
extern WORD  *AddLHS(int);
extern WORD  *AddRHS(int,int);
extern int    AddNtoL(int,WORD *);
extern int    AddNtoC(int,WORD *);
extern VOID   DoubleIfBuffers();
extern STREAM *CreateStream(UBYTE *);

extern int    setonoff(UBYTE *,int *,int,int);
extern int    DoPrint(UBYTE *,int);
extern int    SetExpr(UBYTE *,int,int);
extern void   AddToCom(int,WORD *);
extern int    Add2ComStrings(int,WORD *,UBYTE *,UBYTE *);
extern int    DoSymmetrize(UBYTE *,int);
extern int    DoArgument(UBYTE *,int);
extern int    DoBrackets(UBYTE *,int);
extern WORD  *CountComp(UBYTE *,WORD *);
extern int    CoAntiBracket(UBYTE *);
extern int    CoAntiSymmetrize(UBYTE *);
extern int    DoArgPlode(UBYTE *,int);
extern int    CoArgExplode(UBYTE *);
extern int    CoArgImplode(UBYTE *);
extern int    CoArgument(UBYTE *);
extern int    CoInside(UBYTE *);
extern int    DoInside(UBYTE *);
extern int    CoInExpression(UBYTE *);
extern int    CoInParallel(UBYTE *);
extern int    CoNotInParallel(UBYTE *);
extern int    DoInParallel(UBYTE *,int);
extern int    CoEndInExpression(UBYTE *);
extern int    CoBracket(UBYTE *);
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
extern int    CoFill(UBYTE *);
extern int    CoFillExpression(UBYTE *);
extern int    CoFixIndex(UBYTE *);
extern int    CoFormat(UBYTE *);
extern int    CoGlobal(UBYTE *);
extern int    CoGoTo(UBYTE *);
extern int    CoId(UBYTE *);
extern int    CoIdNew(UBYTE *);
extern int    CoIdOld(UBYTE *);
extern int    CoIf(UBYTE *);
extern int    CoIfMatch(UBYTE *);
extern int    CoIndex(UBYTE *);
extern int    CoInsideFirst(UBYTE *);
extern int    CoKeep(UBYTE *);
extern int    CoLabel(UBYTE *);
extern int    CoLoad(UBYTE *);
extern int    CoLocal(UBYTE *);
extern int    CoMany(UBYTE *);
extern int    CoMerge(UBYTE *);
extern int    CoMetric(UBYTE *);
extern int    CoModOption(UBYTE *);
extern int    CoModuleOption(UBYTE *);
extern int    CoModulusGCD(UBYTE *);
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
extern int    CoOff(UBYTE *);
extern int    CoOn(UBYTE *);
extern int    CoOnce(UBYTE *);
extern int    CoOnly(UBYTE *);
extern int    CoPolyFun(UBYTE *);
extern int    CoPolyRatFun(UBYTE *);
extern int    CoPolyNorm(UBYTE *);
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
extern int    CoSlavePatch(UBYTE *);
extern int    CoPushHide(UBYTE *);
extern int    CoPopHide(UBYTE *);
extern int    CoHide(UBYTE *);
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
extern int    CoToTensor(UBYTE *);
extern int    CoToVector(UBYTE *);
extern int    CoTrace4(UBYTE *);
extern int    CoTraceN(UBYTE *);
extern int    CoChisholm(UBYTE *);
extern int    CoClearTable(UBYTE *);
extern int    DoChain(UBYTE *,int);
extern int    CoChainin(UBYTE *);
extern int    CoChainout(UBYTE *);
extern int    CoTryReplace(UBYTE *);
extern int    CoVector(UBYTE *);
extern int    CoWhile(UBYTE *);
extern int    CoWrite(UBYTE *);
extern int    CoAuto(UBYTE *);
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
extern UBYTE *SkipAName(UBYTE *);
extern int    TestTables();
extern int    GetLabel(UBYTE *);
extern int    CoIdExpression(UBYTE *,int);
extern int    CoAssign(UBYTE *);
extern int    DoExpr(UBYTE *,int);
extern int    CompileSubExpressions(SBYTE *);
extern int    CodeGenerator(SBYTE *);
extern int    CompleteTerm(WORD *,UWORD *,UWORD *,WORD,WORD,int);
extern int    InsTree(int);
extern void   RedoTree(CBUF *,int);
extern void   ClearTree(int);
extern int    CatchDollar(int);
extern int    AssignDollar(WORD *,WORD);
extern UBYTE *WriteDollarToBuffer(WORD,WORD);
extern void   AddToDollarBuffer(UBYTE *);
extern void   TermAssign(WORD *);
extern void   WildDollars();
extern LONG   numcommute(WORD *,LONG *);
extern int    FullRenumber(WORD *,WORD);
extern int    Lus(WORD *,WORD,WORD,WORD,WORD,WORD);
extern int    FindLus(int,int,int);
extern int    CoReplaceLoop(UBYTE *);
extern int    CoFindLoop(UBYTE *);
extern int    DoFindLoop(UBYTE *,int);
extern int    CoFunPowers(UBYTE *);
extern int    SortTheList(int *,int);
extern int    MatchIsPossible(WORD *,WORD *);
extern void   StudyPattern(WORD *);
extern WORD   DolToTensor(WORD);
extern WORD   DolToFunction(WORD);
extern WORD   DolToVector(WORD);
extern WORD   DolToNumber(WORD);
extern WORD   DolToSymbol(WORD);
extern WORD   DolToIndex(WORD);
extern int    CoPrintTable(UBYTE *);
extern int    CoDeallocateTable(UBYTE *);
 
extern int    Optimize(WORD);
extern int    LoadOpti(WORD);
extern int    PutObject(WORD *,int);
extern void   CleanOptiBuffer();
extern int    PrintOptima(WORD);
extern int    FindScratchName();
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
extern void   CombiOpti();
extern int    TakeLongRoot(UWORD *,WORD *,WORD);
extern int    TakeRatRoot(UWORD *,WORD *,WORD);
extern void   HuntPowers(LONG,WORD);
extern void   HuntNumBrackets(LONG);
extern void   ClearTableTree(TABLES);
extern int    InsTableTree(TABLES,WORD *);
extern void   RedoTableTree(TABLES,int);
extern int    FindTableTree(TABLES,WORD *,int);
extern void   finishcbuf(WORD);
extern void   clearcbuf(WORD);
extern void   CleanUpSort(int);
extern FILEHANDLE *AllocFileHandle();
extern VOID   DeAllocFileHandle(FILEHANDLE *);
extern VOID   LowerSortLevel();
extern int    InsideDollar(WORD *,WORD);
extern DOLLARS DolToTerms(WORD);
extern int    SetExprCases(int,int,int);
extern int    TestSelect(WORD *,WORD *);
extern int    MakeSetupAllocs();
extern int    TryFileSetups();
extern void   ExchangeExpressions(int,int);
extern void   ExchangeDollars(int,int);
extern int    GetFirstBracket(WORD *,int);
extern UBYTE *PreIfDollarEval(UBYTE *,int *);
extern LONG   TermsInDollar(WORD);
extern LONG   TermsInExpression(WORD);
extern WORD   *TranslateExpression(UBYTE *);
extern int    IsSetMember(WORD *,WORD);
extern int    IsMultipleOf(WORD *,WORD *);
extern int    TwoExprCompare(WORD *,WORD *,int);
extern void   UpdatePositions();
extern void   M_check();
extern void   M_print();
extern void   M_check1();
extern void   PrintTime();
extern WORD  *PolynoAdd(WORD *,WORD *);
extern WORD  *PolynoSub(WORD *,WORD *);
extern WORD  *PolynoMul(WORD *,WORD *);
extern WORD  *PolynoDiv(WORD *,WORD *,WORD **);
extern WORD  *Polyno1Div(WORD *,WORD *,WORD **);
extern WORD  *PolynoGCD(WORD *,WORD *);
extern WORD  *Polyno1GCD(WORD *,WORD *);
extern UBYTE *PolynoPrint(WORD *);
extern int    PolynoWrite(WORD *);
extern WORD  *PolynoNormalize(WORD *);
extern WORD  *PolynoUnify(WORD *,int);
extern WORD  *PolynoIntFac(WORD *);
extern void   PolynoPushBracket(WORD);
extern void   PolynoPopBracket();
extern void   PolynoStart();
extern void   PolynoFinish();
extern WORD   DoPolynomial(WORD *,WORD);
extern WORD   DoPolyGetRem(WORD *,WORD);
extern WORD  *CopyOfPolynomial(WORD *);
extern WORD  *PolynoCoefNorm(WORD *,WORD,WORD **,int);
extern WORD  *MakePolynomial(WORD,int,int *);
extern int    DoPolynoNorm(int,WORD,WORD,WORD);
/*
int IsProductOf(WORD *,WORD *);
*/
extern POSITION *FindBracket(EXPRESSIONS,WORD *);
extern VOID   PutBracketInIndex(WORD *,POSITION *);
extern void   ClearBracketIndex(WORD);
extern VOID   OpenBracketIndex(WORD);
extern int    DoNoParallel(UBYTE *);
extern int    DoParallel(UBYTE *);
extern int    DoModSum(UBYTE *);
extern int    DoModMax(UBYTE *);
extern int    DoModMin(UBYTE *);
extern int    DoModLocal(UBYTE *);
extern UBYTE *DoModDollar(UBYTE *,int);
extern int    DoSlavePatch(UBYTE *);
extern int    DoinParallel(UBYTE *);
extern int    DonotinParallel(UBYTE *);

extern int    FlipTable(FUNCTIONS,int);
extern int    ChainIn(WORD *,WORD);
extern int    ChainOut(WORD *,WORD);
extern int    PolyNorm(PHEAD WORD *,WORD,WORD,WORD);
extern int    ArgumentImplode(PHEAD WORD *,WORD *);
extern int    ArgumentExplode(PHEAD WORD *,WORD *);
extern int    DenToFunction(WORD *,WORD);
 
extern WORD   ReadElIf();
extern WORD   HowMany(WORD *,WORD *);
extern VOID   RemoveDollars();
extern LONG   CountTerms1(PHEAD0);
extern LONG   TermsInBracket(PHEAD WORD *,WORD);
extern int    Crash();

extern void  *mmalloc(size_t,char *);
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
extern char  *GetSubString(char *,char **);
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
extern int    TouchKey(DBASE *,char *,char *);
extern int    DumpContents(DBASE *,char *,MLONG,MLONG);
extern int    RunMake(int,char **,char ***,MLONG,char *);
extern int    handlefold(DBASE *,char *,char *,char *,char *);
extern int    SumStart(int,char *);
extern int    SumStep(int,char **,MLONG,char *);
extern int    SpecStep(int,char **,char **,MLONG,char *);
extern int    SumFinish(int,char *);
extern int    LinSumStart(int,char *);
extern int    LinSumStep(int,char **,MLONG,char *);
extern int    LinSumDecl(int,char **,MLONG,char *);
extern int    LinSumFinish(int,char *);
extern char  *SkipString(char *);
extern int    ModulusGCD1(WORD,WORD,WORD,WORD *,WORD);
extern int    MakeMono(WORD,WORD *,WORD,WORD);
extern int    TryEnvironment();

#ifdef WITHZLIB
extern int    SetupOutputGZIP(FILEHANDLE *);
extern int    PutOutputGZIP(FILEHANDLE *);
extern int    FlushOutputGZIP(FILEHANDLE *);
extern int    SetupAllInputGZIP(SORTING *);
extern int    SetupInputGZIP(FILEHANDLE *,int);
extern LONG   GetInputGZIP(FILEHANDLE *,int);
extern LONG   FillInputGZIP(FILEHANDLE *,POSITION *,UBYTE *,LONG,int);
#endif

#ifdef WITHPTHREADS
extern VOID   BeginIdentities();
extern int    WhoAmI();
extern int    StartAllThreads(int);
extern void   StartHandleLock();
extern VOID   TerminateAllThreads();
extern int    GetAvailableThread();
extern int    ConditionalGetAvailableThread();
extern int    BalanceRunThread(PHEAD int,WORD *,WORD);
extern void   WakeupThread(int,int);
extern int    MasterWait();
extern int    InParallelProcessor();
extern int    ThreadsProcessor(EXPRESSIONS,WORD);
extern int    ThreadsMerge();
extern int    MasterMerge();
extern int    PutToMaster(PHEAD WORD *);
extern void   SetWorkerFiles();
extern int    MakeThreadBuckets(int,int);
extern void   Test();
extern int    SendOneBucket();
extern int    LoadOneThread(int,int,THREADBUCKET *,int);
extern void  *RunSortBot(void *);
extern void   MasterWaitAllSortBots();
extern int    SortBotMerge(PHEAD0);
extern int    SortBotOut(PHEAD WORD *);
extern void   DefineSortBotTree();
extern int    SortBotMasterMerge();
extern int    SortBotWait(int);
extern void   StartIdentity();
extern void   FinishIdentity(void *);
extern int    SetIdentity(int *);
extern ALLPRIVATES *InitializeOneThread(int);
extern void   FinalizeOneThread(int);
extern void  *RunThread(void *);
extern void   IAmAvailable(int);
extern int    ThreadWait(int);
extern int    ThreadClaimedBlock(int);
extern int    GetThread(int);
extern int    UpdateOneThread(int);
extern void   AlsoAvailable(int);
extern void   AlsoRunning(int);
extern void   MasterWaitAll();
extern void   MasterWaitAllBlocks();
extern int    MasterWaitThread(int);
extern void   WakeupMasterFromThread(int,int);
extern int    LoadReadjusted();
extern int    IniSortBlocks(int);

#endif

extern int    CopyExpression(FILEHANDLE *,FILEHANDLE *);

/*[12dec2003 mt]:*/
extern int    set_in(UBYTE, set_of_char);
extern one_byte set_set(UBYTE, set_of_char);
extern one_byte set_del(UBYTE, set_of_char);
extern one_byte set_sub (set_of_char, set_of_char, set_of_char);
extern int    DoPreAddSeparator(UBYTE *);
extern int    DoPreRmSeparator(UBYTE *);
/*:[12dec2003 mt]*/

/*[14apr2004 mt]:*/
/*See the file extcmd.c*/
/*[08may2006 mt]:*/
/*
extern int openExternalChannel(char *);
*/
extern int    openExternalChannel(UBYTE *,int,UBYTE *,UBYTE *);
extern int    initPresetExternalChannels(UBYTE *, int);
/*:[08may2006 mt]*/
extern int    closeExternalChannel(int);
extern int    selectExternalChannel(int);
extern int    getCurrentExternalChannel();
extern VOID   closeAllExternalChannels();
/*:[14apr2004 mt]*/

/*[08may2006 mt]:*/
/*extern int writexactly(int,char *,size_t);*/
/*:[08may2006 mt]*/

/*[17nov2005 mt]:*/
typedef int (*WRITEBUFTOEXTCHANNEL)(char *,size_t);
typedef int (*GETCFROMEXTCHANNEL)();
typedef int (*SETTERMINATORFOREXTERNALCHANNEL)(char *);
/*[08may2006 mt]:*/
typedef int (*SETKILLMODEFOREXTERNALCHANNEL)(int,int);
/*:[08may2006 mt]*/
typedef LONG (*WRITEFILE)(int,UBYTE *,LONG);
typedef WORD (*COMPARE)(PHEAD WORD *,WORD *,WORD);

#define Compare ((COMPARE)AR.CompareRoutine)

#ifdef PARALLEL
extern LONG   PF_BroadcastNumberOfTerms(LONG);
extern int    PF_Processor(EXPRESSIONS,WORD,WORD);

#endif

extern UBYTE *defineChannel(UBYTE*, HANDLERS*);
extern int    writeToChannel(int,UBYTE *,HANDLERS*);
#ifdef WITHEXTERNALCHANNEL
extern LONG   WriteToExternalChannel(int,UBYTE *,LONG);
#endif
extern pid_t  getExternalChannelPid();
extern int    writeBufToExtChannelOk(char *,size_t);
extern int    getcFromExtChannelOk();
extern int    setKillModeForExternalChannelOk(int,int);
extern int    setTerminatorForExternalChannelOk(char *);
extern int    getcFromExtChannelFailure();
extern int    setKillModeForExternalChannelFailure(int,int);
extern int    setTerminatorForExternalChannelFailure(char *);
extern int    writeBufToExtChannelFailure(char *,size_t);

extern int    ReleaseTB();

extern int    SymbolNormalize(WORD *,WORD *,WORD);
extern int    CheckMinTerm(WORD *,WORD *);
extern int    ReOrderSymbols(WORD *,WORD *,WORD);
extern int    CompareSymbols(PHEAD WORD *,WORD *,WORD);
extern WORD  *PolyAdd(PHEAD WORD *,WORD *);
extern WORD  *PolyMul(PHEAD WORD *,WORD *);
extern WORD  *PolyDiv(PHEAD WORD *,WORD *);
extern WORD  *PolyDivI(PHEAD WORD *,WORD *);
extern WORD  *PolyMul0(PHEAD WORD *,WORD *);
extern WORD  *PolyDiv0(PHEAD WORD *,WORD *);
extern WORD  *PolyRatNorm(PHEAD WORD *,WORD *);
extern WORD  *PolyFunNorm(PHEAD WORD *,WORD);
extern WORD  *PolyFunAddRat(PHEAD WORD *,WORD *);
extern WORD  *PolyRemoveContent(PHEAD WORD *,WORD);
extern WORD  *PolyGCD(PHEAD WORD *,WORD *);
extern WORD  *PolyGCD1(PHEAD WORD *,WORD *);
extern WORD  *PolyGCD1a(PHEAD WORD *,WORD *);
extern WORD  *PolyGCD1b(PHEAD WORD *,WORD *);
extern WORD  *PolyGCD1c(PHEAD WORD *,WORD *);
extern WORD  *PolyGCD1d(PHEAD WORD *,WORD *);
extern WORD  *PolyDiv1(PHEAD WORD *,WORD *);
extern WORD  *PolyDiv1d(PHEAD WORD *,WORD *);
extern WORD  *PolyPseudoRem1(PHEAD WORD *,WORD *);
extern WORD  *GetNegPow(PHEAD WORD *);
extern WORD  *PolyNormPoly(PHEAD WORD *);
extern WORD   PolyRatFunMul(PHEAD WORD *);
extern WORD  *PolyTake(PHEAD WORD *,WORD);
extern WORD   PolyGetRenumbering(PHEAD WORD *,WORD *);
extern WORD   InvertModular(WORD,WORD);
extern WORD   InvertLongModular(PHEAD UWORD *,WORD,WORD,UWORD *,WORD *);
/*WORD *PolyModGCD(PHEAD WORD *,WORD *,WORD);*/
extern int    PolyModGCD(POLYMOD *,POLYMOD *);
extern int    PolyConvertToModulus(WORD *,POLYMOD *,WORD);
extern WORD  *PolyConvertFromModulus(PHEAD POLYMOD *,WORD);
extern WORD  *PolyChineseRemainder(PHEAD WORD *,WORD *,WORD *,WORD,WORD);
extern WORD   NextPrime(PHEAD WORD);
extern WORD   ModShortPrime(UWORD *,WORD,WORD);
extern int    AllocPolyModCoefs(POLYMOD *,WORD);
extern WORD   DivMod(UWORD *,WORD,WORD);
extern int    AccumTermGCD(WORD *,WORD *);
extern int    PolyTakeSqrt(PHEAD WORD *);
extern int    PolyTakeRoot(PHEAD WORD *,WORD);
extern WORD  *PolyPow(PHEAD WORD *,WORD);
extern WORD  *PolyInterpolation(PHEAD WORD *,WORD *,WORD);
extern WORD  *PolySubs(PHEAD WORD *,WORD,WORD);
extern WORD  *PolyNewton(PHEAD WORD **,WORD,WORD);
extern WORD  *PolyGetNewtonCoef(PHEAD WORD **,WORD);
extern WORD   ModPow(WORD,WORD,WORD);
extern WORD   PolyModSubsVector(PHEAD WORD *,WORD *,WORD,WORD,WORD,WORD,POLYMOD *);
extern WORD  *PolyGetSymbols(PHEAD WORD *,int *);
extern WORD  *PolyGetGCDPowers(PHEAD WORD *,WORD *,WORD *,WORD *);
extern WORD  *PolyGetConfig(PHEAD WORD);
extern WORD   wranf(PHEAD0);

extern WORD  *EvaluateGcd(PHEAD WORD *);

extern WORD   ReadSaveHeader();
extern WORD   ReadSaveIndex(FILEINDEX *);
extern WORD   ReadSaveExpression(UBYTE *,UBYTE *,LONG *,LONG *);
extern UBYTE *ReadSaveTerm32(UBYTE *,UBYTE *,UBYTE **,UBYTE *,UBYTE *,int);
extern WORD   ReadSaveVariables(UBYTE *,UBYTE *,LONG *,LONG *,INDEXENTRY *,LONG *);
extern WORD   WriteStoreHeader(WORD);

extern int    CheckRecoveryFile();
extern char  *RecoveryFilename();
extern int    DoRecovery();
extern void   DoCheckpoint();

/*
  	#] Declarations :
*/
#endif
