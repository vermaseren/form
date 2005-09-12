#ifndef __FDECLARE__

#define __FDECLARE__

#ifdef ANSI
/*
	First the declaration macro's. They are to keep the code portable.
	They wouldn't be needed ordinarily, but there is such a thing as
	the IBM interpretation of the ANSI standard.
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

#define DECLARE(x,y) x y ;
 
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
#define DECLARE(x,y) x();

#endif

/*
		First the macros
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
		TokenToLine((UBYTE *)y); } else { TokenToLine((UBYTE *)x); }


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

#ifdef NOALIGN
#define DOALIGN(x)
#define ALIGN(x) x
#define WALIGN(x) {}
#else
#define DOALIGN(x) x = (WORD *)(((((long)x)+sizeof(WORD *)-1))&~(sizeof(WORD *)-1))
#define ALIGN(x) (((((long)x)+sizeof(WORD *)-1))&~(sizeof(WORD *)-1))
#define WALIGN(x) x=(WORD *)(((((long)x)+sizeof(WORD)-1))&~(sizeof(WORD)-1))
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

#ifdef DEBUGVERSION
#define SETERROR(x) { AR.ErrorCondition = 1; if ( x ) return(x); }
#else
#define SETERROR(x) { Terminate(-1); return(-1); }
#endif

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
DECLARE(VOID TELLFILE,(int,POSITION *))

#define TOLONG(x) ((LONG)(x))
#define StrIcmp(x,y) StrICont((UBYTE *)(y),(UBYTE *)(x))

#define Add2Com(x) { WORD cod[2]; cod[0] = x; cod[1] = 2; AddNtoL(2,cod); }
#define Add3Com(x1,x2) { WORD cod[3]; cod[0] = x1; cod[1] = 3; cod[2] = x2; AddNtoL(3,cod); }
#define Add4Com(x1,x2,x3) { WORD cod[4]; cod[0] = x1; cod[1] = 4; \
   cod[2] = x2; cod[3] = x3; AddNtoL(4,cod); }
#define Add5Com(x1,x2,x3,x4) { WORD cod[5]; cod[0] = x1; cod[1] = 5; \
   cod[2] = x2; cod[3] = x3; cod[4] = x4; AddNtoL(5,cod); }

#define WantAddPointers(x) while((AR.pWorkPointer+(x))>AR.pWorkSize)\
	ExpandBuffer((void **)(&AR.pWorkSpace),&AR.pWorkSize,sizeof(WORD *))
#define WantAddLongs(x) while((AR.lWorkPointer+(x))>AR.lWorkSize)\
	ExpandBuffer((void **)(&AR.lWorkSpace),&AR.lWorkSize,sizeof(LONG))
#define WantAddPositions(x) while((AR.posWorkPointer+(x))>AR.posWorkSize)\
	ExpandBuffer((void **)(&AR.posWorkSpace),&AR.posWorkSize,sizeof(POSITION))

DECLARE(UBYTE *CodeToLine,(WORD,UBYTE *))
DECLARE(INDEXENTRY *FindInIndex,(WORD,FILEDATA *,WORD))
DECLARE(UBYTE *GetLine,(UBYTE *,LONG,WORD))
DECLARE(UBYTE *GetOneSet,(UBYTE *,WORD *,WORD *,WORD *,WORD *,WORD))
DECLARE(INDEXENTRY *NextFileIndex,(POSITION *))
DECLARE(UBYTE *NextWord,(UBYTE *))
DECLARE(WORD *PasteTerm,(WORD,WORD *,WORD *,WORD,WORD))
DECLARE(UBYTE *SkipExpression,(UBYTE *,WORD))
DECLARE(UBYTE *SkipLine,(UBYTE *))
DECLARE(UBYTE *SkipName,(UBYTE *))
DECLARE(UBYTE *SkipOneName,(UBYTE *))
DECLARE(UBYTE *SkipSet,(UBYTE *))
DECLARE(UBYTE *StrCopy,(UBYTE *,UBYTE *))
DECLARE(UBYTE *WrtPower,(UBYTE *,WORD))
DECLARE(WORD AccumGCD,(UWORD *,WORD *,UWORD *,WORD))
DECLARE(VOID AddArgs,(WORD *,WORD *,WORD *))
DECLARE(WORD AddCoef,(WORD **,WORD **))
DECLARE(WORD AddLong,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *))
DECLARE(WORD AddPLon,(UWORD *,WORD,UWORD *,WORD,UWORD *,UWORD *))
DECLARE(WORD AddPoly,(WORD **,WORD **))
DECLARE(WORD AddRat,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *))
DECLARE(VOID AddToLine,(UBYTE *))
DECLARE(WORD AddWild,(WORD,WORD,WORD))
DECLARE(WORD BigLong,(UWORD *,WORD,UWORD *,WORD))
DECLARE(WORD BinomGen,(WORD *,WORD,WORD **,WORD,WORD,WORD,WORD,WORD,UWORD *,WORD))
DECLARE(VOID Cconout,(WORD))
DECLARE(WORD CheckWild,(WORD,WORD,WORD,WORD *))
DECLARE(WORD Chisholm,(WORD *term,WORD level))
DECLARE(WORD CleanExpr,(WORD))
DECLARE(VOID CleanUp,(WORD))
DECLARE(VOID ClearWild,ARG0)
DECLARE(WORD Commute,(WORD *,WORD *))
DECLARE(int CompArg,(WORD *,WORD *))
DECLARE(WORD CompCoef,(WORD *, WORD *))
DECLARE(WORD CompGroup,(WORD,WORD **,WORD *,WORD *,WORD))
DECLARE(WORD CompWord,(UBYTE *,char *))
DECLARE(WORD Compare,(WORD *,WORD *,WORD))
DECLARE(WORD CopyToComp,ARG0)
DECLARE(WORD CountDo,(WORD *,WORD *))
DECLARE(WORD CountFun,(WORD *,WORD *))
DECLARE(WORD Deferred,(WORD *,WORD))
DECLARE(WORD DeleteStore,(WORD))
DECLARE(WORD DetCurDum,(WORD *))
DECLARE(VOID DetVars,(WORD *,WORD))
DECLARE(WORD Distribute,(DISTRIBUTE *,WORD))
DECLARE(WORD DivLong,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *,UWORD *,WORD *))
DECLARE(WORD DivRat,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *))
DECLARE(WORD Divvy,(UWORD *,WORD *,UWORD *,WORD))
DECLARE(WORD DoDelta,(WORD *))
DECLARE(WORD DoDelta3,(WORD *,WORD))
DECLARE(WORD DoTableExpansion,(WORD *,WORD))
DECLARE(WORD DoDistrib,(WORD *,WORD))
DECLARE(WORD DoMerge,(WORD *,WORD,WORD,WORD))
DECLARE(WORD TestUse,(WORD *,WORD))
DECLARE(WORD Apply,(WORD *,WORD))
DECLARE(int ApplyExec,(WORD *,int,WORD))
DECLARE(WORD ApplyReset,(WORD *,WORD))
DECLARE(WORD TableReset,ARG0)
DECLARE(VOID ReWorkT,(WORD *,WORD *,WORD))
DECLARE(WORD DoIfStatement,(WORD *,WORD *))
DECLARE(WORD DoModule,ARG0)
DECLARE(WORD DoOnePow,(WORD *,WORD,WORD,WORD *,WORD *,WORD,WORD *))
DECLARE(void DoRevert,(WORD *,WORD *))
DECLARE(WORD DoSumF1,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD DoSumF2,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD DoTheta,(WORD *))
DECLARE(LONG EndSort,(WORD *,int))
DECLARE(WORD EntVar,(WORD,UBYTE *,WORD,WORD,WORD))
DECLARE(WORD EpfCon,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD EpfFind,(WORD *,WORD *))
DECLARE(WORD EpfGen,(WORD,WORD *,WORD *,WORD *,WORD))
DECLARE(WORD EqualArg,(WORD *,WORD,WORD))
DECLARE(WORD Evaluate,(UBYTE **))
DECLARE(int Factorial,(WORD,UWORD *,WORD *))
DECLARE(int Bernoulli,(WORD,UWORD *,WORD *))
DECLARE(WORD FindAll,(WORD *,WORD *,WORD,WORD *))
DECLARE(WORD FindMulti,(WORD *,WORD *))
DECLARE(WORD FindOnce,(WORD *,WORD *))
DECLARE(WORD FindOnly,(WORD *,WORD *))
DECLARE(WORD FindRest,(WORD *,WORD *))
DECLARE(WORD FindSpecial,(WORD *))
DECLARE(WORD FindrNumber,(WORD,VARRENUM *))
DECLARE(VOID FiniLine,ARG0)
DECLARE(WORD FiniTerm,(WORD *,WORD *,WORD *,WORD,WORD))
DECLARE(VOID FlushCon,ARG0)
DECLARE(WORD FlushOut,(POSITION *,FILEHANDLE *,int))
DECLARE(VOID FunLevel,(WORD *))
DECLARE(VOID GarbHand,ARG0)
DECLARE(WORD GcdLong,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *))
DECLARE(WORD Generator,(WORD *,WORD))
DECLARE(LONG Get1Long,(UBYTE *))
DECLARE(WORD GetBinom,(UWORD *,WORD *,WORD,WORD))
DECLARE(WORD GetFromStore,(WORD *,POSITION *,RENUMBER,WORD *,WORD))
DECLARE(WORD GetIchar,ARG0)
DECLARE(WORD GetIword,(UBYTE *,WORD))
DECLARE(WORD GetLong,(UBYTE *,UWORD *,WORD *))
DECLARE(WORD GetMoreTerms,(WORD *))
DECLARE(WORD GetMoreFromMem,(WORD *,WORD **))
DECLARE(WORD GetObject,(UBYTE *,WORD *,WORD *,WORD))
DECLARE(WORD GetOneTerm,(WORD *,WORD))
DECLARE(WORD GetOption,(UBYTE **))
DECLARE(WORD GetParams,ARG0)
DECLARE(RENUMBER GetTable,(WORD,POSITION *))
DECLARE(WORD GetTerm,(WORD *))
DECLARE(WORD GetWithEcho,ARG0)
DECLARE(WORD Glue,(WORD *,WORD *,WORD *,WORD))
DECLARE(WORD InFunction,(WORD *,WORD *))
DECLARE(WORD IncLHS,ARG0)
DECLARE(WORD IncRHS,(WORD))
DECLARE(VOID IniGlob,ARG0)
DECLARE(VOID IniLine,ARG0)
DECLARE(WORD IniVars,ARG0)
DECLARE(VOID Init2,ARG0)
DECLARE(VOID Initialize,ARG0)
DECLARE(WORD InsertTerm,(WORD *,WORD,WORD,WORD *,WORD *,WORD))
DECLARE(WORD Kraak,(UBYTE *,WORD **,WORD *,WORD,WORD))
DECLARE(VOID LongToLine,(UWORD *,WORD))
DECLARE(WORD LookAhead,ARG0)
DECLARE(WORD MakeDirty,(WORD *,WORD *,WORD))
DECLARE(VOID MarkDirty,(WORD *,WORD))
DECLARE(WORD MakeModTable,ARG0)
DECLARE(WORD MatchE,(WORD *,WORD *,WORD *,WORD))
DECLARE(int MatchCy,(WORD *,WORD *,WORD *,WORD))
DECLARE(int FunMatchCy,(WORD *,WORD *,WORD *,WORD))
DECLARE(int FunMatchSy,(WORD *,WORD *,WORD *,WORD))
DECLARE(int MatchArgument,(WORD *,WORD *))
DECLARE(WORD MatchFunction,(WORD *,WORD *,WORD *))
DECLARE(WORD MergePatches,(WORD))
DECLARE(WORD MesCerr,(char *, UBYTE *))
DECLARE(WORD MesComp,(char *, UBYTE *, UBYTE *))
DECLARE(WORD MesLong,(char *))
DECLARE(WORD MesPar,(WORD))
DECLARE(WORD MesSet,(WORD))
DECLARE(WORD Modulus,(WORD *))
DECLARE(WORD MulLong,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *))
DECLARE(WORD MulRat,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *))
DECLARE(WORD Mully,(UWORD *,WORD *,UWORD *,WORD))
DECLARE(WORD MultDo,(WORD *,WORD *))
DECLARE(WORD NewSort,ARG0)
DECLARE(WORD ExtraSymbol,(WORD,WORD,WORD,WORD *))
DECLARE(WORD Normalize,(WORD *))
DECLARE(WORD OpenTemp,ARG0)
DECLARE(VOID Pack,(UWORD *,WORD *,UWORD *,WORD ))
DECLARE(LONG PasteFile,(WORD,WORD *,POSITION *,WORD **,RENUMBER,WORD *,WORD))
DECLARE(WORD Permute,(PERM *,WORD))
DECLARE(WORD PolyMul,(WORD *))
DECLARE(WORD PopVariables,ARG0)
DECLARE(WORD PrepPoly,(WORD *))
DECLARE(WORD Processor,ARG0)
DECLARE(WORD Product,(UWORD *,WORD *,WORD))
DECLARE(VOID PrtLong,(UWORD *,WORD,UBYTE *))
DECLARE(VOID PrtTerms,ARG0)
DECLARE(WORD PutBracket,(WORD *))
DECLARE(LONG PutIn,(FILEHANDLE *,POSITION *,WORD *,WORD **,int))
DECLARE(WORD PutInStore,(INDEXENTRY *,WORD))
DECLARE(WORD PutOut,(WORD *,POSITION *,FILEHANDLE *,WORD))
DECLARE(UWORD Quotient,(UWORD *,WORD *,WORD))
DECLARE(WORD RaisPow,(UWORD *,WORD *,UWORD))
DECLARE(VOID RatToLine,(UWORD *,WORD))
DECLARE(WORD RatioFind,(WORD *,WORD *))
DECLARE(WORD RatioGen,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD ReNumber,(WORD *))
DECLARE(WORD ReadLHS,ARG0)
DECLARE(WORD ReadRHS,(WORD))
DECLARE(WORD ReadSnum,(UBYTE **))
DECLARE(WORD Remain10,(UWORD *,WORD *))
DECLARE(WORD Remain4,(UWORD *,WORD *))
DECLARE(WORD ResetScratch,ARG0)
DECLARE(WORD ResolveSet,(WORD *,WORD *,WORD *))
DECLARE(WORD Reverse5,(WORD *term,WORD level))
DECLARE(WORD RevertScratch,ARG0)
DECLARE(WORD ScanFunctions,(WORD *,WORD *,WORD))
DECLARE(VOID SeekScratch,(FILEHANDLE *,POSITION *))
DECLARE(VOID SetEndScratch,(FILEHANDLE *,POSITION *))
DECLARE(VOID SetEndHScratch,(FILEHANDLE *,POSITION *))
DECLARE(WORD SetFileIndex,ARG0)
DECLARE(WORD SetParams,ARG0)
DECLARE(WORD Sflush,(FILEHANDLE *))
DECLARE(WORD Simplify,(UWORD *,WORD *,UWORD *,WORD *))
DECLARE(WORD SkipWhite,ARG0)
DECLARE(WORD SortWild,(WORD *,WORD))
#ifdef NEWSPLITMERGE
DECLARE(LONG SplitMerge,(WORD **,LONG))
#else
DECLARE(VOID SplitMerge,(WORD **,LONG))
#endif
DECLARE(WORD StoreTerm,(WORD *))
DECLARE(WORD StrCcmp,(UBYTE *, char *))
DECLARE(VOID StrNcop,(UBYTE *, UBYTE *, WORD))
DECLARE(WORD SubEval,(UBYTE **))
DECLARE(VOID SubPLon,(UWORD *,WORD,UWORD *,WORD,UWORD *,WORD *))
DECLARE(VOID Substitute,(WORD *,WORD *,WORD))
DECLARE(WORD SymFind,(WORD *,WORD *))
DECLARE(WORD SymGen,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD Symmetrize,(WORD *,WORD *,WORD,WORD,WORD,WORD))
DECLARE(int FullSymmetrize,(WORD *,int))
DECLARE(WORD TakeModulus,(UWORD *,WORD *,WORD))
DECLARE(VOID TalToLine,(UWORD))
DECLARE(WORD TenVec,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD TenVecFind,(WORD *,WORD *))
DECLARE(WORD TermRenumber,(WORD *,RENUMBER,WORD))
DECLARE(VOID TestDrop,ARG0)
DECLARE(WORD TestMatch,(WORD *,WORD *))
DECLARE(WORD TestSub,(WORD *,WORD))
DECLARE(LONG TimeCPU,(WORD))
DECLARE(LONG Timer,ARG0)
DECLARE(WORD ToStorage,(EXPRESSIONS,POSITION *))
DECLARE(VOID TokenToLine,(UBYTE *))
DECLARE(WORD Trace4,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD Trace4Gen,(TRACES *,WORD))
DECLARE(WORD Trace4no,(WORD,WORD *,TRACES *))
DECLARE(WORD TraceFind,(WORD *,WORD *))
DECLARE(WORD TraceN,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD TraceNgen,(TRACES *,WORD))
DECLARE(WORD TraceNno,(WORD,WORD *,TRACES *))
DECLARE(WORD Traces,(WORD *,WORD *,WORD,WORD))
DECLARE(WORD Trick,(WORD *,TRACES *))
DECLARE(WORD TryDo,(WORD *,WORD *,WORD))
DECLARE(VOID UnDefine,(WORD))
DECLARE(VOID UnPack,(UWORD *,WORD,WORD *,WORD *))
DECLARE(WORD Unique,(UBYTE *,UBYTE *,WORD))
DECLARE(WORD VarStore,(UBYTE *,WORD,WORD,WORD))
DECLARE(VOID W_S_L,(WORD,LONG))
DECLARE(VOID W_S_S,(WORD,UBYTE *))
DECLARE(WORD WildFill,(WORD *,WORD *,WORD *))
DECLARE(WORD WriteAll,ARG0)
DECLARE(WORD WriteOne,(UBYTE *,int,int))
DECLARE(VOID WriteArgument,(WORD *))
DECLARE(WORD WriteExpression,(WORD *,LONG))
DECLARE(WORD WriteInnerTerm,(WORD *,WORD))
DECLARE(VOID WriteLists,ARG0)
DECLARE(VOID WriteSetup,ARG0)
DECLARE(VOID WriteStats,(POSITION *,WORD))
DECLARE(WORD WriteSubTerm,(WORD *,WORD))
DECLARE(WORD WriteTerm,(WORD *,WORD *,WORD,WORD,WORD))
DECLARE(WORD execarg,(WORD *,WORD))
DECLARE(WORD execterm,(WORD *,WORD))
DECLARE(WORD execnorm,(WORD *,WORD))
DECLARE(int main,(int,char **))

DECLARE(typedef WORD (*WCW),(EXPRESSIONS,WORD))
DECLARE(UBYTE *strDup,(UBYTE *))
DECLARE(WORD AddSetElement,(EXPRESSIONS,LONG))
DECLARE(WORD AddToHide,ARG0)
DECLARE(LONG CloseFormFile,(UBYTE *,WORD))
DECLARE(LONG CreateFormFile,(UBYTE *,WORD))
DECLARE(WORD DoExecute,(WORD,WORD))
DECLARE(WORD DoSpolynomial,(EXPRESSIONS,WORD,WORD,WORD *))
DECLARE(WORD ElimOneVar,(EXPRESSIONS,WORD))
DECLARE(WORD FromHide,ARG0)
DECLARE(WORD Groebner,(WORD))
DECLARE(WORD LinSolve,(WORD))
DECLARE(WORD MakeCommercial,(UBYTE *,LONG,LONG))
DECLARE(WORD MakeSubs,(WORD *,WORD,WORD))
DECLARE(WORD MulSetBracket,(WORD *,LONG,WORD *,WORD,LONG,LONG))
DECLARE(WORD NormalForm,ARG0)
DECLARE(LONG OpenFormFile,(UBYTE *,WORD))
DECLARE(VOID SetScratch,(FILEHANDLE *,POSITION *))
DECLARE(WORD SortByWeight,(EXPRESSIONS,WORD)) 
DECLARE(WORD Spolynomial,(EXPRESSIONS,LONG,LONG,LONG,LONG,WORD *,WORD *,WORD *,WORD))
DECLARE(WORD StepExpressions,(WORD,WORD))
DECLARE(WORD StepGroebner,(EXPRESSIONS,WORD))
DECLARE(WORD StepNormalForm,(EXPRESSIONS,WORD))
DECLARE(WORD TestLinearWeights,(UBYTE *))
DECLARE(WORD TestOneLinearWeight,(WORD))
DECLARE(WORD ToHide,ARG0)
DECLARE(WORD ToTriangle,(EXPRESSIONS,WORD))
DECLARE(VOID Warning,(char *))
DECLARE(VOID HighWarning,(char *))
DECLARE(LONG WriteFormFile,(UBYTE *,WORD))
DECLARE(int SpareTable,(TABLES))

DECLARE(UBYTE *strDup1,(UBYTE *,char *))
DECLARE(VOID *Malloc,(LONG))
DECLARE(VOID *Malloc1,(LONG,char *))
DECLARE(int DoTail,(int,UBYTE **))
DECLARE(int OpenInput,ARG0)
DECLARE(int PutPreVar,(UBYTE *,UBYTE *,UBYTE *,int))
DECLARE(VOID Error0,(char *))
DECLARE(VOID Error1,(char *,UBYTE *))
DECLARE(VOID Error2,(char *,char *,UBYTE *))
DECLARE(UBYTE ReadFromStream,(STREAM *))
DECLARE(UBYTE GetFromStream,(STREAM *))
DECLARE(UBYTE LookInStream,(STREAM *))
DECLARE(STREAM *OpenStream,(UBYTE *,int,int,int))
DECLARE(int LocateFile,(UBYTE **,int))
DECLARE(STREAM *CloseStream,(STREAM *))
DECLARE(VOID PositionStream,(STREAM *,LONG))
DECLARE(int ProcessOption,(UBYTE *,UBYTE *,int))
DECLARE(int DoSetups,ARG0)
DECLARE(VOID Terminate,(int))
DECLARE(NAMENODE *GetNode,(NAMETREE *,UBYTE *))
DECLARE(int AddName,(NAMETREE *,UBYTE *,WORD,WORD,int *))
DECLARE(int GetName,(NAMETREE *,UBYTE *,WORD *,int))
DECLARE(int GetAutoName,(NAMETREE *,UBYTE *,WORD *))
DECLARE(int GetVar,(UBYTE *,WORD *,WORD *,int,int))
DECLARE(int MakeDubious,(NAMETREE *,UBYTE *,WORD *))
DECLARE(int GetOName,(NAMETREE *,UBYTE *,WORD *,int))
DECLARE(VOID DumpTree,(NAMETREE *))
DECLARE(VOID DumpNode,(NAMETREE *,WORD,WORD))
DECLARE(VOID LinkTree,(NAMETREE *,WORD,WORD))
DECLARE(VOID CopyTree,(NAMETREE *,NAMETREE *,WORD))
DECLARE(int CompactifyTree,(NAMETREE *))
DECLARE(NAMETREE *MakeNameTree,ARG0)
DECLARE(VOID FreeNameTree,(NAMETREE *))
DECLARE(int AddExpression,(UBYTE *,int,int,int))
DECLARE(int AddSymbol,(UBYTE *,int,int,int))
DECLARE(int AddDollar,(UBYTE *,WORD,WORD *,LONG))
DECLARE(int ReplaceDollar,(WORD,WORD,WORD *,LONG))
DECLARE(int DollarRaiseLow,(UBYTE *,LONG))
DECLARE(int AddVector,(UBYTE *,int))
DECLARE(int AddDubious,(UBYTE *))
DECLARE(int AddIndex,(UBYTE *,int,int))
DECLARE(UBYTE *DoDimension,(UBYTE *,int *,int *))
DECLARE(int AddFunction,(UBYTE *,int,int,int,int))
DECLARE(int CoFunction,(UBYTE *,int,int))
DECLARE(int AddSet,(UBYTE *))
DECLARE(int DoElements,(UBYTE *,SETS,UBYTE *))
DECLARE(int DoTempSet,(UBYTE *,UBYTE *))
DECLARE(int NameConflict,(int,UBYTE *))
DECLARE(int OpenFile,(char *))
DECLARE(int OpenAddFile,(char *))
DECLARE(int CreateFile,(char *))
DECLARE(int CreateLogFile,(char *))
DECLARE(VOID CloseFile,(int))
DECLARE(int CreateHandle,ARG0)
DECLARE(LONG ReadFile,(int,UBYTE *,LONG))
/*[15apr2004 mt]:*/
/*I introduced the variable WriteFile, see the file variable.h:*/
/*DECLARE(LONG WriteFile,(int,UBYTE *,LONG))*/
DECLARE(LONG WriteFileToFile,(int,UBYTE *,LONG))
/*:[15apr2004 mt]*/
DECLARE(VOID SeekFile,(int,POSITION *,int))
DECLARE(LONG TellFile,(int))
DECLARE(void FlushFile,(int))
DECLARE(int GetPosFile,(int,fpos_t *))
DECLARE(int SetPosFile,(int,fpos_t *))
DECLARE(int GetChannel,(char *))
DECLARE(int GetAppendChannel,(char *))
DECLARE(int CloseChannel,(char *))
DECLARE(VOID inictable,ARG0)
DECLARE(int inicbufs,ARG0)
DECLARE(VOID StartFiles,ARG0)
DECLARE(UBYTE *MakeDate,ARG0)
DECLARE(VOID PreProcessor,ARG0)
DECLARE(VOID *FromList,(LIST *))
DECLARE(VOID *From0List,(LIST *))
DECLARE(VOID *FromVarList,(LIST *))
DECLARE(int DoubleList,(VOID ***,int *,int,char *))
DECLARE(int DoubleLList,(VOID ***,LONG *,int,char *))
DECLARE(void DoubleBuffer,(void **,void **,int,char *))
DECLARE(void ExpandBuffer,(void **,LONG *,int))
DECLARE(LONG iexp,(LONG,int))
DECLARE(int IsLikeVector,(WORD *))
DECLARE(int CompareArgs,(WORD *,WORD *))
DECLARE(UBYTE *SkipField,(UBYTE *,int))
DECLARE(int StrCmp,(UBYTE *,UBYTE *))
DECLARE(int StrICmp,(UBYTE *,UBYTE *))
DECLARE(int StrHICmp,(UBYTE *,UBYTE *))
DECLARE(int StrICont,(UBYTE *,UBYTE *))
DECLARE(int ConWord,(UBYTE *,UBYTE *))
DECLARE(int StrLen,(UBYTE *))
DECLARE(UBYTE *GetPreVar,(UBYTE *,int))
DECLARE(void ToGeneral,(WORD *,WORD *,WORD))
DECLARE(int ToFast,(WORD *,WORD *))
DECLARE(SETUPPARAMETERS *GetSetupPar,(UBYTE *))
DECLARE(int RecalcSetups,ARG0)
DECLARE(int AllocSetups,ARG0)
DECLARE(SORTING *AllocSort,(LONG,LONG,LONG,LONG,int,int,LONG))
DECLARE(UBYTE *LoadInputFile,(UBYTE *,int))
DECLARE(UBYTE GetInput,ARG0)
DECLARE(VOID ClearPushback,ARG0)
DECLARE(UBYTE GetChar,(int))
DECLARE(VOID CharOut,(UBYTE))
DECLARE(VOID UnsetAllowDelay,ARG0)
DECLARE(VOID PopPreVars,(int))
DECLARE(VOID IniModule,(int))
DECLARE(VOID IniSpecialModule,(int))
DECLARE(int ModuleInstruction,(int *,int *))
DECLARE(int PreProInstruction,ARG0)
DECLARE(int LoadInstruction,(int))
DECLARE(int LoadStatement,(int))
DECLARE(KEYWORD *FindKeyWord,(UBYTE *,KEYWORD *,int))
DECLARE(KEYWORD *FindInKeyWord,(UBYTE *,KEYWORD *,int))
DECLARE(int DoDefine,(UBYTE *))
DECLARE(int DoRedefine,(UBYTE *))
DECLARE(int TheDefine,(UBYTE *,int))
DECLARE(int TheUndefine,(UBYTE *))
DECLARE(int ClearMacro,(UBYTE *))
DECLARE(int DoUndefine,(UBYTE *))
DECLARE(int DoInclude,(UBYTE *))
/*[14apr2004 mt]:*/
DECLARE(int DoExternal,(UBYTE *))
DECLARE(int DoToExternal,(UBYTE *))
DECLARE(int DoFromExternal,(UBYTE *))
DECLARE(int DoPrompt,(UBYTE *))
DECLARE(int DoSetExternal,(UBYTE *))
DECLARE(int DoRmExternal,(UBYTE *))
/*:[14apr2004 mt]*/
DECLARE(int DoMessage,(UBYTE *))
DECLARE(int DoPreNormPoly,(UBYTE *))
DECLARE(int DoPreOut,(UBYTE *))
DECLARE(int DoPreAppend,(UBYTE *))
DECLARE(int DoPreCreate,(UBYTE *))
DECLARE(int DoPreAssign,(UBYTE *))
DECLARE(int DoPreBreak,(UBYTE *))
DECLARE(int DoPreDefault,(UBYTE *))
DECLARE(int DoPreSwitch,(UBYTE *))
DECLARE(int DoPreEndSwitch,(UBYTE *))
DECLARE(int DoPreCase,(UBYTE *))
DECLARE(int DoPreShow,(UBYTE *))
DECLARE(int DoPreExchange,(UBYTE *))
DECLARE(int DoSystem,(UBYTE *))
DECLARE(int DoPipe,(UBYTE *))
DECLARE(VOID StartPrepro,ARG0)
DECLARE(int DoIfdef,(UBYTE *,int))
DECLARE(int DoElse,(UBYTE *))
DECLARE(int DoElseif,(UBYTE *))
DECLARE(int DoEndif,(UBYTE *))
DECLARE(int DoTerminate,(UBYTE *))
DECLARE(int DoIf,(UBYTE *))
DECLARE(int DoCall,(UBYTE *))
DECLARE(int DoDebug,(UBYTE *))
DECLARE(int DoDo,(UBYTE *))
DECLARE(int DoEnddo,(UBYTE *))
DECLARE(int DoEndprocedure,(UBYTE *))
DECLARE(int DoProcedure,(UBYTE *))
DECLARE(int DoPrePrint,(UBYTE *))
DECLARE(int DoPreWrite,(UBYTE *))
DECLARE(int DoPreClose,(UBYTE *))
DECLARE(int DoPreRemove,(UBYTE *))
DECLARE(int DoCommentChar,(UBYTE *))
DECLARE(VOID WriteString,(int,UBYTE *,int))
DECLARE(VOID WriteUnfinString,(int,UBYTE *,int))
DECLARE(UBYTE *PreCalc,ARG0)
DECLARE(UBYTE *PreEval,(UBYTE *,LONG *))
DECLARE(VOID NumToStr,(UBYTE *,LONG))
DECLARE(int PreCmp,(int,int,UBYTE *,int,int,UBYTE *,int))
DECLARE(int PreEq,(int,int,UBYTE *,int,int,UBYTE *,int))
DECLARE(UBYTE *pParseObject,(UBYTE *,int *,LONG *))
DECLARE(UBYTE *PreIfEval,(UBYTE *,int *))
DECLARE(int EvalPreIf,(UBYTE *))
DECLARE(int PreLoad,(PRELOAD *,UBYTE *,UBYTE *,int,char *))
DECLARE(UBYTE *EndOfToken,(UBYTE *))
DECLARE(VOID SetSpecialMode,(int,int))
DECLARE(VOID MakeGlobal,ARG0)
DECLARE(int ExecModule,(int))
DECLARE(int ExecStore,ARG0)
DECLARE(VOID FullCleanUp,ARG0)
DECLARE(int DoExecStatement,ARG0)
DECLARE(int DoPipeStatement,ARG0)
DECLARE(int DoPolyfun,(UBYTE *))
DECLARE(int CompileStatement,(UBYTE *))
DECLARE(UBYTE *ToToken,(UBYTE *))
DECLARE(int GetDollar,(UBYTE *))
DECLARE(int PutDollar,(UBYTE *))
DECLARE(int MesWork,ARG0)
DECLARE(int MesPrint,(char *,...))
DECLARE(int MesCall,(char *))
DECLARE(int Mes1arr,(char *))
DECLARE(int Mes2arr,(char *))
DECLARE(UBYTE *NumCopy,(WORD,UBYTE *))
DECLARE(char *LongCopy,(LONG,char *))
DECLARE(char *LongLongCopy,(off_t *,char *))
DECLARE(VOID ReserveTempFiles,ARG0)
DECLARE(VOID PrintTerm,(WORD *,char *))
DECLARE(VOID PrintSubTerm,(WORD *,char *))
DECLARE(int ExpandTripleDots,ARG0)

#define M_alloc(x)      malloc((size_t)(x))
DECLARE(void M_free,(VOID *,char *))
DECLARE(void ClearWildcardNames,ARG0)
DECLARE(int AddWildcardName,(UBYTE *))
DECLARE(int GetWildcardName,(UBYTE *))
DECLARE(void Globalize,(int))
DECLARE(void ResetVariables,(int))
DECLARE(void AddToPreTypes,(int))
DECLARE(void MessPreNesting,(int))
DECLARE(LONG GetStreamPosition,(STREAM *))
DECLARE(WORD *DoubleCbuffer,(int,WORD *))
DECLARE(WORD *AddLHS,(int))
DECLARE(WORD *AddRHS,(int,int))
DECLARE(int AddNtoL,(int,WORD *))
DECLARE(int AddNtoC,(int,WORD *))
DECLARE(VOID DoubleIfBuffers,ARG0)
DECLARE(STREAM *CreateStream,(UBYTE *))

DECLARE(int CoAntiBracket,(UBYTE *))
DECLARE(int CoAntiSymmetrize,(UBYTE *))
DECLARE(int CoArgument,(UBYTE *))
DECLARE(int CoInside,(UBYTE *))
DECLARE(int DoInside,(UBYTE *))
DECLARE(int CoInExpression,(UBYTE *))
DECLARE(int CoEndInExpression,(UBYTE *))
DECLARE(int CoBracket,(UBYTE *))
DECLARE(int CoCFunction,(UBYTE *))
DECLARE(int CoCTensor,(UBYTE *))
DECLARE(int CoCollect,(UBYTE *))
DECLARE(int CoCompress,(UBYTE *))
DECLARE(int CoContract,(UBYTE *))
DECLARE(int CoCycleSymmetrize,(UBYTE *))
DECLARE(int CoDelete,(UBYTE *))
DECLARE(int CoTableBase,(UBYTE *))
DECLARE(int CoApply,(UBYTE *))
DECLARE(int CoDimension,(UBYTE *))
DECLARE(int CoDiscard,(UBYTE *))
DECLARE(int CoDisorder,(UBYTE *))
DECLARE(int CoDrop,(UBYTE *))
DECLARE(int CoElse,(UBYTE *))
DECLARE(int CoElseIf,(UBYTE *))
DECLARE(int CoEndArgument,(UBYTE *))
DECLARE(int CoEndInside,(UBYTE *))
DECLARE(int CoEndIf,(UBYTE *))
DECLARE(int CoEndRepeat,(UBYTE *))
DECLARE(int CoEndTerm,(UBYTE *))
DECLARE(int CoEndWhile,(UBYTE *))
DECLARE(int CoExit,(UBYTE *))
DECLARE(int CoFactArg,(UBYTE *))
DECLARE(int CoFill,(UBYTE *))
DECLARE(int CoFillExpression,(UBYTE *))
DECLARE(int CoFixIndex,(UBYTE *))
DECLARE(int CoFormat,(UBYTE *))
DECLARE(int CoGlobal,(UBYTE *))
DECLARE(int CoGoTo,(UBYTE *))
DECLARE(int CoId,(UBYTE *))
DECLARE(int CoIdNew,(UBYTE *))
DECLARE(int CoIdOld,(UBYTE *))
DECLARE(int CoIf,(UBYTE *))
DECLARE(int CoIfMatch,(UBYTE *))
DECLARE(int CoIndex,(UBYTE *))
DECLARE(int CoInsideFirst,(UBYTE *))
DECLARE(int CoKeep,(UBYTE *))
DECLARE(int CoLabel,(UBYTE *))
DECLARE(int CoLoad,(UBYTE *))
DECLARE(int CoLocal,(UBYTE *))
DECLARE(int CoMany,(UBYTE *))
DECLARE(int CoMerge,(UBYTE *))
DECLARE(int CoMetric,(UBYTE *))
DECLARE(int CoModOption,(UBYTE *))
DECLARE(int CoModuleOption,(UBYTE *))
DECLARE(int CoModulusGCD,(UBYTE *))
DECLARE(int CoModulus,(UBYTE *))
DECLARE(int CoMulti,(UBYTE *))
DECLARE(int CoMultiply,(UBYTE *))
DECLARE(int CoNFunction,(UBYTE *))
DECLARE(int CoNPrint,(UBYTE *))
DECLARE(int CoNTensor,(UBYTE *))
DECLARE(int CoNWrite,(UBYTE *))
DECLARE(int CoNoDrop,(UBYTE *))
DECLARE(int CoNoSkip,(UBYTE *))
DECLARE(int CoNormalize,(UBYTE *))
DECLARE(int CoMakeInteger,(UBYTE *))
DECLARE(int CoOff,(UBYTE *))
DECLARE(int CoOn,(UBYTE *))
DECLARE(int CoOnce,(UBYTE *))
DECLARE(int CoOnly,(UBYTE *))
DECLARE(int CoPolyFun,(UBYTE *))
DECLARE(int CoPrint,(UBYTE *))
DECLARE(int CoPrintB,(UBYTE *))
DECLARE(int CoProperCount,(UBYTE *))
DECLARE(int CoUnitTrace,(UBYTE *))
DECLARE(int CoRCycleSymmetrize,(UBYTE *))
DECLARE(int CoRatio,(UBYTE *))
DECLARE(int CoRedefine,(UBYTE *))
DECLARE(int CoRenumber,(UBYTE *))
DECLARE(int CoRepeat,(UBYTE *))
DECLARE(int CoSave,(UBYTE *))
DECLARE(int CoSelect,(UBYTE *))
DECLARE(int CoSet,(UBYTE *))
DECLARE(int CoSetExitFlag,(UBYTE *))
DECLARE(int CoSkip,(UBYTE *))
DECLARE(int CoSlavePatch,(UBYTE *))
DECLARE(int CoPushHide,(UBYTE *))
DECLARE(int CoPopHide,(UBYTE *))
DECLARE(int CoHide,(UBYTE *))
DECLARE(int CoNoHide,(UBYTE *))
DECLARE(int CoUnHide,(UBYTE *))
DECLARE(int CoNoUnHide,(UBYTE *))
DECLARE(int CoSort,(UBYTE *))
DECLARE(int CoSplitArg,(UBYTE *))
DECLARE(int CoSplitFirstArg,(UBYTE *))
DECLARE(int CoSplitLastArg,(UBYTE *))
DECLARE(int CoSum,(UBYTE *))
DECLARE(int CoSymbol,(UBYTE *))
DECLARE(int CoSymmetrize,(UBYTE *))
DECLARE(int DoTable,(UBYTE *,int))
DECLARE(int CoTable,(UBYTE *))
DECLARE(int CoTerm,(UBYTE *))
DECLARE(int CoNTable,(UBYTE *))
DECLARE(int CoCTable,(UBYTE *))
DECLARE(int CoToTensor,(UBYTE *))
DECLARE(int CoToVector,(UBYTE *))
DECLARE(int CoTrace4,(UBYTE *))
DECLARE(int CoTraceN,(UBYTE *))
DECLARE(int CoChisholm,(UBYTE *))
DECLARE(int DoChain,(UBYTE *,int))
DECLARE(int CoChainin,(UBYTE *))
DECLARE(int CoChainout,(UBYTE *))
DECLARE(int CoTryReplace,(UBYTE *))
DECLARE(int CoVector,(UBYTE *))
DECLARE(int CoWhile,(UBYTE *))
DECLARE(int CoWrite,(UBYTE *))
DECLARE(int CoAuto,(UBYTE *))
DECLARE(int CoTBaddto,(UBYTE *))
DECLARE(int CoTBaudit,(UBYTE *))
DECLARE(int CoTBcleanup,(UBYTE *))
DECLARE(int CoTBcreate,(UBYTE *))
DECLARE(int CoTBenter,(UBYTE *))
DECLARE(int CoTBhelp,(UBYTE *))
DECLARE(int CoTBload,(UBYTE *))
DECLARE(int CoTBoff,(UBYTE *))
DECLARE(int CoTBon,(UBYTE *))
DECLARE(int CoTBopen,(UBYTE *))
DECLARE(int CoTBreplace,(UBYTE *))
DECLARE(int CoTBuse,(UBYTE *))
DECLARE(int CoTestUse,(UBYTE *))
DECLARE(int AddComString,(int,WORD *,UBYTE *,int))
DECLARE(int CompileAlgebra,(UBYTE *,int,WORD *))
DECLARE(int IsIdStatement,(UBYTE *))
DECLARE(UBYTE *IsRHS,(UBYTE *,UBYTE))
DECLARE(int ParenthesesTest,(UBYTE *))
DECLARE(int tokenize,(UBYTE *,WORD))
DECLARE(void WriteTokens,(SBYTE *))
DECLARE(int simp1token,(SBYTE *,int))
DECLARE(int simpwtoken,(SBYTE *,int))
DECLARE(int simp2token,(SBYTE *,int))
DECLARE(int simp3atoken,(SBYTE *,int))
DECLARE(int simp3btoken,(SBYTE *,int))
DECLARE(int simp4token,(SBYTE *,int))
DECLARE(int simp5token,(SBYTE *,int))
DECLARE(UBYTE *SkipAName,(UBYTE *))
DECLARE(int TestTables,ARG0)
DECLARE(int GetLabel,(UBYTE *))
DECLARE(int CoIdExpression,(UBYTE *,int))
DECLARE(int CoAssign,(UBYTE *))
DECLARE(int DoExpr,(UBYTE *,int))
DECLARE(int CompileSubExpressions,(SBYTE *))
DECLARE(int CodeGenerator,(SBYTE *))
DECLARE(int CompleteTerm,(WORD *,UWORD *,UWORD *,WORD,WORD,int))
DECLARE(int InsTree,(int))
DECLARE(void RedoTree,(CBUF *,int))
DECLARE(void ClearTree,(int))
DECLARE(int CatchDollar,(int))
DECLARE(int AssignDollar,(WORD *,WORD))
DECLARE(UBYTE *WriteDollarToBuffer,(WORD))
DECLARE(void AddToDollarBuffer,(UBYTE *))
DECLARE(void TermAssign,(WORD *))
DECLARE(void WildDollars,ARG0)
DECLARE(LONG numcommute,(WORD *,LONG *))
DECLARE(int FullRenumber,(WORD *,WORD))
DECLARE(int Lus,(WORD *,WORD,WORD,WORD,WORD,WORD))
DECLARE(int FindLus,(int,int,int))
DECLARE(int CoReplaceLoop,(UBYTE *))
DECLARE(int CoFindLoop,(UBYTE *))
DECLARE(int DoFindLoop,(UBYTE *,int))
DECLARE(int CoFunPowers,(UBYTE *))
DECLARE(int SortTheList,(int *,int))
DECLARE(int MatchIsPossible,(WORD *,WORD *))
DECLARE(void StudyPattern,(WORD *))
DECLARE(WORD DolToTensor,(WORD))
DECLARE(WORD DolToFunction,(WORD))
DECLARE(WORD DolToVector,(WORD))
DECLARE(WORD DolToNumber,(WORD))
DECLARE(WORD DolToSymbol,(WORD))
DECLARE(WORD DolToIndex,(WORD))
DECLARE(int CoPrintTable,(UBYTE *))
DECLARE(int CoDeallocateTable,(UBYTE *))
 
DECLARE(int Optimize,(WORD))
DECLARE(int LoadOpti,(WORD))
DECLARE(int PutObject,(WORD *,int))
DECLARE(void CleanOptiBuffer,ARG0)
DECLARE(int PrintOptima,(WORD))
DECLARE(int FindScratchName,ARG0)
DECLARE(WORD MaxPowerOpti,(LONG))
DECLARE(WORD HuntNumFactor,(LONG,WORD *,int))
DECLARE(WORD HuntFactor,(LONG,WORD *,int))
DECLARE(void HuntPairs,(LONG,WORD))
DECLARE(void HuntBrackets,(LONG))
DECLARE(int AddToOpti,(WORD *,int))
DECLARE(LONG TestNewSca,(LONG,WORD *,WORD *))
DECLARE(void NormOpti,(WORD *))
DECLARE(void SortOpti,(LONG))
DECLARE(void SplitOpti,(WORD **,LONG))
DECLARE(void CombiOpti,ARG0)
DECLARE(int TakeLongRoot,(UWORD *,WORD *,WORD))
DECLARE(int TakeRatRoot,(UWORD *,WORD *,WORD))
DECLARE(void HuntPowers,(LONG,WORD))
DECLARE(void HuntNumBrackets,(LONG))
DECLARE(void ClearTableTree,(TABLES))
DECLARE(int InsTableTree,(TABLES,WORD *))
DECLARE(void RedoTableTree,(TABLES,int))
DECLARE(int FindTableTree,(TABLES,WORD *,int))
DECLARE(void finishcbuf,(WORD))
DECLARE(void CleanUpSort,(int))
DECLARE(FILEHANDLE *AllocFileHandle,ARG0)
DECLARE(VOID DeAllocFileHandle,(FILEHANDLE *))
DECLARE(VOID LowerSortLevel,ARG0)
DECLARE(int InsideDollar,(WORD *,WORD))
DECLARE(DOLLARS DolToTerms,(WORD))
DECLARE(int SetExprCases,(int,int,int))
DECLARE(int TestSelect,(WORD *,WORD *))
DECLARE(int MakeSetupAllocs,ARG0)
DECLARE(int TryFileSetups,ARG0)
DECLARE(void ExchangeExpressions,(int,int))
DECLARE(void ExchangeDollars,(int,int))
DECLARE(int GetFirstBracket,(WORD *,int))
DECLARE(UBYTE *PreIfDollarEval,(UBYTE *,int *))
DECLARE(LONG TermsInDollar,(WORD))
DECLARE(LONG TermsInExpression,(WORD))
DECLARE(WORD *TranslateExpression,(UBYTE *))
DECLARE(int IsSetMember,(WORD *,WORD))
DECLARE(int IsMultipleOf,(WORD *,WORD *))
DECLARE(int TwoExprCompare,(WORD *,WORD *,int))
DECLARE(void UpdatePositions,ARG0)
DECLARE(void M_check,ARG0)
DECLARE(void M_print,ARG0)
DECLARE(void M_check1,ARG0)
DECLARE(void PrintTime,ARG0)
DECLARE(WORD *PolynoAdd,(WORD *,WORD *))
DECLARE(WORD *PolynoSub,(WORD *,WORD *))
DECLARE(WORD *PolynoMul,(WORD *,WORD *))
DECLARE(WORD *PolynoDiv,(WORD *,WORD *,WORD **))
DECLARE(WORD *Polyno1Div,(WORD *,WORD *,WORD **))
DECLARE(WORD *PolynoGCD,(WORD *,WORD *))
DECLARE(WORD *Polyno1GCD,(WORD *,WORD *))
DECLARE(UBYTE *PolynoPrint,(WORD *))
DECLARE(int PolynoWrite,(WORD *))
DECLARE(WORD *PolynoNormalize,(WORD *))
DECLARE(WORD *PolynoUnify,(WORD *,int))
DECLARE(WORD *PolynoIntFac,(WORD *))
DECLARE(void PolynoPushBracket,(WORD))
DECLARE(void PolynoPopBracket,ARG0)
DECLARE(void PolynoStart,ARG0)
DECLARE(void PolynoFinish,ARG0)
DECLARE(WORD DoPolynomial,(WORD *,WORD))
DECLARE(WORD *CopyOfPolynomial,(WORD *))
DECLARE(WORD *PolynoCoefNorm,(WORD *,WORD,WORD **,int))
DECLARE(WORD *MakePolynomial,(WORD,int,int *))
DECLARE(int DoPolynoNorm,(int,WORD,WORD,WORD))
/*
DECLARE(int IsProductOf,(WORD *,WORD *))
*/
DECLARE(POSITION *FindBracket,(EXPRESSIONS,WORD *))
DECLARE(VOID PutBracketInIndex,(WORD *,POSITION *))
DECLARE(void ClearBracketIndex,(WORD))
DECLARE(VOID OpenBracketIndex,(WORD))
DECLARE(int DoNoParallel,(UBYTE *))
DECLARE(int DoParallel,(UBYTE *))
DECLARE(int DoModSum,(UBYTE *))
DECLARE(int DoModMax,(UBYTE *))
DECLARE(int DoModMin,(UBYTE *))
DECLARE(int DoModNoKeep,(UBYTE *))
DECLARE(UBYTE *DoModDollar,(UBYTE *,int))
DECLARE(int DoSlavePatch,(UBYTE *))

DECLARE(int FlipTable,(FUNCTIONS,int))
DECLARE(int ChainIn,(WORD *,WORD,WORD))
DECLARE(int ChainOut,(WORD *,WORD,WORD))
#endif

void *mmalloc(size_t size,char *message);
char *str_dup(char *str);
void convertblock(INDEXBLOCK *in,INDEXBLOCK *out,int mode);
void convertnamesblock(NAMESBLOCK *in,NAMESBLOCK *out,int mode);
void convertiniinfo(INIINFO *in,INIINFO *out,int mode);
int ReadIndex(DBASE *d);
int WriteIndexBlock(DBASE *d,MLONG num);
int WriteNamesBlock(DBASE *d,MLONG num);
int WriteIndex(DBASE *d);
int WriteIniInfo(DBASE *d);
int ReadIniInfo(DBASE *d);
int AddToIndex(DBASE *d,MLONG number);
DBASE *GetDbase(char *filename);
DBASE *OpenDbase(char *filename);
char *ReadObject(DBASE *d,MLONG tablenumber,char *arguments);
char *ReadijObject(DBASE *d,MLONG i,MLONG j,char *arguments);
int ExistsObject(DBASE *d,MLONG tablenumber,char *arguments);
int DeleteObject(DBASE *d,MLONG tablenumber,char *arguments);
char *GetSubString(char *keyword,char **argv);
int WriteObject(DBASE *d,MLONG tablenumber,char *arguments,char *rhs,MLONG number);
MLONG AddObject(DBASE *d,MLONG tablenumber,char *arguments,char *rhs);
int Cleanup(DBASE *d);
DBASE *NewDbase(char *name,MLONG number);
void FreeTableBase(DBASE *d);
int ComposeTableNames(DBASE *d);
int PutTableNames(DBASE *d);
MLONG AddTableName(DBASE *d,char *name,TABLES T);
MLONG GetTableName(DBASE *d,char *name);
MLONG FindTableNumber(DBASE *d,char *name);
int TouchKey(DBASE *d,char *key,char *value);
int DumpContents(DBASE *d,char *filename,MLONG from,MLONG to);
int RunMake(int variety,char **inargv,char ***outargv,MLONG num,char *outvar);
int handlefold(DBASE *d,char *start,char *finish,char *name,char *varname);
int SumStart(int variety,char *outname);
int SumStep(int variety,char **argv,MLONG num,char *outname);
int SpecStep(int variety,char **argv1,char **argv2,MLONG num,char *outname);
int SumFinish(int variety,char *outname);
int LinSumStart(int variety,char *outname);
int LinSumStep(int variety,char **argv,MLONG num,char *outname);
int LinSumDecl(int variety,char **argv,MLONG num,char *outname);
int LinSumFinish(int variety,char *outname);
char *SkipString(char *);
int ModulusGCD1(WORD modu,WORD fun1,WORD fun2,WORD *term,WORD sym);
int MakeMono(WORD modu,WORD *t,WORD whichbuffer,WORD sym);
int TryEnvironment();

#ifdef ZWITHZLIB
DECLARE(int SetupOutputGZIP,(FILEHANDLE *))
DECLARE(int PutOutputGZIP,(FILEHANDLE *))
DECLARE(int FlushOutputGZIP,(FILEHANDLE *))
DECLARE(int SetupAllInputGZIP,(SORTING *))
DECLARE(int SetupInputGZIP,(FILEHANDLE *,int))
DECLARE(LONG GetInputGZIP,(FILEHANDLE *,int))
DECLARE(LONG FillInputGZIP,(FILEHANDLE *,POSITION *,UBYTE *,LONG,int))
#endif

/*[12dec2003 mt]:*/
DECLARE(int set_in,(UBYTE, set_of_char))
DECLARE(one_byte set_set,(UBYTE, set_of_char))
DECLARE(one_byte set_del,(UBYTE, set_of_char))
DECLARE(one_byte set_sub, (set_of_char, set_of_char, set_of_char))
DECLARE(int DoPreAddSeparator,(UBYTE *))
DECLARE(int DoPreRmSeparator,(UBYTE *))
/*:[12dec2003 mt]*/

/*[14apr2004 mt]:*/
/*See the file extcmd.c*/
DECLARE(int openExternalChannel,(char *))
DECLARE(int closeExternalChannel,(int))
DECLARE(int selectExternalChannel,(int))
DECLARE(int getCurrentExternalChannel,(VOID))
DECLARE(VOID closeAllExternalChannels,(VOID))
/*:[14apr2004 mt]*/

/* temporary commentary for forcing cvs merge */

