/** @file checkpoint.c
 *
 *  Contains all functions that deal with the recovery mechanism controlled and
 *  activated by the On Checkpoint switch.
 *
 *  TODO: short description of how to modify the recovery code in case structs.h
 *  gets altered.
 */
/*
  	#[ Includes :
*/

#include "form3.h"

#include <errno.h>

/*
  	#] Includes :
  	#[ filenames and system commands :
*/

/**
 *  basename of recovery files
 */
#define RECOVERYBASENAME "FORMrecv"

#define FNAME(X) RECOVERYBASENAME "." #X

/**
 *  filename for the recovery file
 */
static char *recoveryfile = FNAME(tmp);
/**
 *  filename for the intermediate recovery file. only if the write is
 *  completely successful, this file will be moved/renamed to the one
 *  named by recoveryfile. this offers atomicity for the snapshot generation.
 */
static char *intermedfile = FNAME(XXX);
/**
 *  filename of sort file copy
 */
static char *sortfile = FNAME(out);
/**
 *  filename of store file copy
 */
static char *storefile = FNAME(str);

static int syscmdinit = 0;
static char *syscmdstore;
static char *syscmdsort;

/*
  	#] filenames and system commands :
  	#[ CheckRecoveryFile :
*/

/**
 *  Checks whether a snapshot/recovery file exists.
 *  Returns 1 if it exists, 0 otherwise.
 */
int CheckRecoveryFile()
{
	FILE *fd;
	if ( (fd = fopen(recoveryfile, "r")) ) {
		fclose(fd);
		return(1);
	}
	return(0);
}

/*
  	#] CheckRecoveryFile :
  	#[ RecoveryFilename :
*/

/**
 *  Returns pointer to recovery filename. 
 */
char *RecoveryFilename()
{
	return(recoveryfile);
}

/*
  	#] RecoveryFilename :
  	#[ Debugging :
*/

#ifdef PRINTDEBUG

static void print_BYTE(void *p)
{
	UBYTE h = (UBYTE)(*((UBYTE*)p) >> 4);
	UBYTE l = (UBYTE)(*((UBYTE*)p) & 0x0F);
	if ( h > 9 ) h += 55; else h += 48;
	if ( l > 9 ) l += 55; else l += 48;
	printf("%c%c ", h, l);
}

static void print_STR(UBYTE *p)
{
	if ( p ) {
		printf("%s\n", (char*)p);
	}
	else {
		printf("NULL\n");
	}
}

static void print_WORDB(WORD *buf, WORD *top)
{
	int i = 0;
	while ( buf < top ) {
		++i;
		printf("%d ",*buf++);
		if ( !(i % 40) ) printf("\n");
	}
	if ( i % 40 ) printf("\n");
}

static void print_VOIDP(void *p, size_t size)
{
	size_t i;
	if ( p ) {
		for ( i=1; i<=size; ++i ) {
			print_BYTE(p);
			p = (char*)p + 1;
			if ( (i % 40) == 0 ) printf("\n");
		}
		if ( ((i-1) % 40) ) printf("\n");
	}
	else {
		printf("NULL\n");
	}
}

static void print_CHARS(UBYTE *p, size_t size)
{
	size_t i;
	for ( i=0; i<size; ++i ) {
		if ( *p < 32 ) printf("^");
		else  printf("%c", *p);
		p++;
	}
	printf("\n");
}

static void print_INTV(int *p, size_t size)
{
	size_t i;
	for ( i=1; i<=size; ++i ) {
		printf("%d ", *p++);
		if ( (i % 8) == 0 ) printf("\n");
	}
	if ( ((i-1) % 8) ) printf("\n");
}

static void print_PRELOAD(PRELOAD *l)
{
	if ( l->size ) {
		print_CHARS(l->buffer, l->size);
	}
	printf("%ld\n", l->size);
}

static void print_PREVAR(PREVAR *l)
{
	printf("%s\n", l->name);
	printf("%s\n", l->value);
	if ( l->nargs ) printf("%s\n", l->argnames);
	printf("%d\n", l->nargs);
	printf("%d\n", l->wildarg);
}

static void print_DOLLARS(DOLLARS l)
{
	print_VOIDP(l->where, l->size);
	printf("%ld\n", l->size);
	printf("%ld\n", l->name);
	printf("%d\n", l->type);
	printf("%d\n", l->node);
	printf("%d\n", l->index);
	printf("%d\n", l->zero);
	printf("%d\n", l->numdummies);
	printf("%d\n", l->reserved);
}

static void print_LIST(LIST *l)
{
	print_VOIDP(l->lijst, l->size);
	printf("%s\n", l->message);
	printf("%d\n", l->num);
	printf("%d\n", l->maxnum);
	printf("%d\n", l->size);
	printf("%d\n", l->numglobal);
	printf("%d\n", l->numtemp);
	printf("%d\n", l->numclear);
}

static void print_DOLOOP(DOLOOP *l)
{
	print_PRELOAD(&(l->p));
	printf("%s\n", l->name);
	printf("%s\n", l->vars);
	printf("%s\n", l->contents);
	if ( l->dollarname ) printf("%s\n", l->dollarname);
	printf("%ld\n", l->startlinenumber);
	printf("%ld\n", l->firstnum);
	printf("%ld\n", l->lastnum);
	printf("%ld\n", l->incnum);
	printf("%d\n", l->type);
	printf("%d\n", l->NoShowInput);
	printf("%d\n", l->errorsinloop);
	printf("%d\n", l->firstloopcall);
}

static void print_PROCEDURE(PROCEDURE *l)
{
	print_PRELOAD(&(l->p));
	printf("%s\n", l->name);
	printf("%d\n", l->loadmode);
}

static void print_NAMETREE(NAMETREE *t)
{
	int i;
	for ( i=0; i<t->nodefill; ++i ) {
		printf("%ld %d %d %d %d %d %d\n", t->namenode[i].name,
			 t->namenode[i].parent, t->namenode[i].left, t->namenode[i].right,
			 t->namenode[i].balance, t->namenode[i].type, t->namenode[i].number );
	}
	print_CHARS(t->namebuffer, t->namefill);
	printf("%ld\n", t->namesize);
	printf("%ld\n", t->namefill);
	printf("%ld\n", t->nodesize);
	printf("%ld\n", t->nodefill);
	printf("%ld\n", t->oldnamefill);
	printf("%ld\n", t->oldnodefill);
	printf("%ld\n", t->globalnamefill);
	printf("%ld\n", t->globalnodefill);
	printf("%ld\n", t->clearnamefill);
	printf("%ld\n", t->clearnodefill);
	printf("%d\n", t->headnode);
}

static void print_STREAM(STREAM *t)
{
	print_CHARS(t->buffer, t->buffersize);
	print_STR(t->FoldName);
	print_STR(t->name);
	if ( t->type == PREVARSTREAM || t->type == DOLLARSTREAM ) {
		print_STR(t->pname);
	}
	printf("%ld\n", (long)t->fileposition);
	printf("%ld\n", (long)t->linenumber);
	printf("%ld\n", (long)t->prevline);
	printf("%d\n", t->handle);
}

static void print_C()
{
	int i;
	printf("%%%% C_const\n");
	for ( i=0; i<32; ++i ) {
		printf("%d",AC.separators[i].bit_7);
		printf("%d",AC.separators[i].bit_6);
		printf("%d",AC.separators[i].bit_5);
		printf("%d",AC.separators[i].bit_4);
		printf("%d",AC.separators[i].bit_3);
		printf("%d",AC.separators[i].bit_2);
		printf("%d",AC.separators[i].bit_1);
		printf("%d ",AC.separators[i].bit_0);
	}
	printf("\n");
	print_NAMETREE(AC.dollarnames);
	print_NAMETREE(AC.exprnames);
	print_NAMETREE(AC.varnames);
	print_LIST(&AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		printf("%s %d\n", channels[i].name, channels[i].handle);
	}
	print_LIST(&AC.DubiousList);
	print_LIST(&AC.FunctionList);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		if ( functions[i].tabl ) {

		}
		printf("%ld\n", functions[i].symminfo);
		printf("%ld\n", functions[i].name);
		printf("%d\n", functions[i].namesize);
	}
	print_LIST(&AC.ExpressionList);
	print_LIST(&AC.IndexList);
	print_LIST(&AC.SetElementList);
	print_LIST(&AC.SetList);
	print_LIST(&AC.SymbolList);
	print_LIST(&AC.VectorList);
	print_LIST(&AC.PotModDolList);
	print_LIST(&AC.ModOptDolList);
	print_LIST(&AC.TableBaseList);

	print_LIST(&AC.cbufList);
	printf("%d\n", AC.cbufnum);

	print_LIST(&AC.AutoSymbolList);
	print_LIST(&AC.AutoIndexList);
	print_LIST(&AC.AutoVectorList);
	print_LIST(&AC.AutoFunctionList);

	print_NAMETREE(AC.autonames);

	print_LIST(AC.Symbols);
	print_LIST(AC.Indices);
	print_LIST(AC.Vectors);
	print_LIST(AC.Functions);

	print_NAMETREE(*AC.activenames);
	
	printf("%d\n", AC.AutoDeclareFlag);

	for ( i=0; i<AC.NumStreams; ++i ) {
		print_STREAM(AC.Streams+i);
	}
	print_STREAM(AC.CurrentStream);

	/* ... */

	print_CHARS((UBYTE*)AC.tokens, AC.toptokens-AC.tokens);

}

static void print_P()
{
	int i;
	printf("%%%% P_const\n");
	print_LIST(&AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		print_DOLLARS(&(Dollars[i]));
	}
	print_LIST(&AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		print_PREVAR(&(PreVar[i]));
	}
	print_LIST(&AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		print_DOLOOP(&(DoLoops[i]));
	}
	print_LIST(&AP.ProcList);
	for ( i=0; i<AP.ProcList.num; ++i ) {
		print_PROCEDURE(&(Procedures[i]));
	}
	print_LIST(&AP.ChDollarList);
	for ( i=0; i<=AP.PreSwitchLevel; ++i ) {
		print_STR(AP.PreSwitchStrings[i]);
	}
	printf("%p\n", AP.preStart);
	printf("%p\n", AP.preStop);
	printf("%p\n", AP.preFill);
	print_CHARS(AP.preStart, AP.pSize);
	printf("%s\n", AP.procedureExtension);
	printf("%s\n", AP.cprocedureExtension);
	print_INTV(AP.PreIfStack, AP.MaxPreIfLevel);
	print_INTV(AP.PreSwitchModes, AP.NumPreSwitchStrings+1);
	print_INTV(AP.PreTypes, AP.NumPreTypes+1);
	printf("%ld\n", AP.InOutBuf);
	printf("%ld\n", AP.pSize);
	printf("%d\n", AP.PreproFlag);
	printf("%d\n", AP.iBufError);
	printf("%d\n", AP.PreOut);
	printf("%d\n", AP.PreSwitchLevel);
	printf("%d\n", AP.NumPreSwitchStrings);
	printf("%d\n", AP.MaxPreTypes);
	printf("%d\n", AP.NumPreTypes);
	printf("%d\n", AP.DelayPrevar);
	printf("%d\n", AP.AllowDelay);
	printf("%d\n", AP.lhdollarerror);
	printf("%d\n", AP.eat);
	printf("%d\n", AP.gNumPre);
	printf("%d\n", AP.DebugFlag);
	printf("%d\n", AP.preError);
	printf("%c\n", AP.ComChar);
	printf("%c\n", AP.cComChar);
}

static void print_R()
{
	GETIDENTITY
	size_t i;
	printf("%s\n", AR.infile->name);
	printf("%s\n", AR.outfile->name);
	printf("%s\n", AR.hidefile->name);
	for ( i=0; i<3; ++i ) {
		print_WORDB(AR.Fscr[i].PObuffer, AR.Fscr[i].POfull);
	}
}

#endif /* ifdef PRINTDEBUG */

/*
  	#] Debugging :
  	#[ Helper Macros :
*/

/* some helper macros to streamline the code in DoSnapshot() and DoRecovery() */

/* freeing memory */

#define R_FREE(ARG) \
	if ( ARG ) M_free(ARG, #ARG);

#define R_FREE_NAMETREE(ARG) \
	R_FREE(ARG->namenode); \
	R_FREE(ARG->namebuffer); \
	R_FREE(ARG);

#define R_FREE_STREAM(ARG) \
	R_FREE(ARG.buffer); \
	R_FREE(ARG.FoldName); \
	R_FREE(ARG.name);

/* reading a single variable */

#define R_SET(VAR,TYPE) \
	VAR = *((TYPE*)p); p = (unsigned char*)p + sizeof(TYPE);

/* general buffer */

#define R_COPY_B(VAR,SIZE,CAST) \
	VAR = (CAST)Malloc1(SIZE,#VAR); \
	memcpy(VAR, p, SIZE); p = (unsigned char*)p + SIZE;

#define S_WRITE_B(BUF,LEN) \
	if ( fwrite(BUF, LEN, 1, fd) != 1 ) return(__LINE__);

/* character strings */

#define R_COPY_S(VAR,CAST) \
	if ( VAR ) { \
		VAR = (CAST)malloc(strlen(p)+1); \
		strcpy((char*)VAR, p); p = (unsigned char*)p + strlen(p) + 1; \
	}

#define S_WRITE_S(STR) \
	if ( STR ) { \
		l = strlen((char*)STR) + 1; \
		if ( fwrite(STR, l, 1, fd) != 1 ) return(__LINE__); \
	}

/* LIST */

#define R_COPY_LIST(ARG) \
	if ( ARG.maxnum ) { \
		R_COPY_B(ARG.lijst, ARG.size*ARG.maxnum, void*) \
	}

#define S_WRITE_LIST(LST) \
	if ( LST.maxnum ) { \
		S_WRITE_B(LST.lijst, LST.maxnum*LST.size) \
	}

/* NAMETREE */

#define R_COPY_NAMETREE(ARG) \
	R_COPY_B(ARG, sizeof(NAMETREE), NAMETREE*); \
	if ( ARG->namenode ) { \
		R_COPY_B(ARG->namenode, ARG->nodesize*sizeof(NAMENODE), NAMENODE*); \
	} \
	if ( ARG->namebuffer ) { \
		R_COPY_B(ARG->namebuffer, ARG->namesize, UBYTE*); \
	}

#define S_WRITE_NAMETREE(ARG) \
	S_WRITE_B(ARG, sizeof(NAMETREE)); \
	if ( ARG->namenode ) { \
		S_WRITE_B(ARG->namenode, ARG->nodesize * sizeof(struct NaMeNode)); \
	} \
	if ( ARG->namebuffer ) { \
		S_WRITE_B(ARG->namebuffer, ARG->namesize); \
	}

/* DOLLAR */

#define S_WRITE_DOLLAR(ARG) \
	if ( ARG.size ) { \
		S_WRITE_B(ARG.where, ARG.size*sizeof(WORD)) \
	}

/*
  	#] Helper Macros :
  	#[ DoRecovery :
*/

/**
 *  Reads from the recovery file and restores all necessary variables and
 *  states in FORM, so that the execution can recommence in preprocessor() as
 *  if no restart of FORM had occured.
 */
int DoRecovery()
{
	GETIDENTITY
	FILE *fd;
	POSITION pos;
	void *buf, *p;
	int size, i, j;
	void *org, *org2, *org3, *org4, *org5;
	int ofs;

	printf("Recovering ... "); fflush(0);

	if ( !(fd = fopen(recoveryfile, "r")) ) return(__LINE__);

	/* load the complete recovery file into a buffer */
	if ( fread(&pos, sizeof(POSITION), 1, fd) != 1 ) return(__LINE__);
	size = BASEPOSITION(pos) - sizeof(POSITION);
	buf = Malloc1(size, "recovery buffer");
	if ( fread(buf, size, 1, fd) != 1 ) return(__LINE__);

	/* pointer p will go through the buffer in the following */
	p = buf;

	/* #[ AM */

	/* only certain elements will be restored. the rest of AM should have gotten
	 * the correct values at startup. */

	R_SET(AM.hparallelflag, int);
	R_SET(AM.gparallelflag, int);
	R_SET(AM.gCodesFlag, int);
	R_SET(AM.gNamesFlag, int);
	R_SET(AM.gStatsFlag, int);
	R_SET(AM.gNoSpacesInNumbers, int);
	R_SET(AM.gIndentSpace, WORD);
	R_SET(AM.gUnitTrace, WORD);
	R_SET(AM.gDefDim, int);
	R_SET(AM.gDefDim4, int);
	R_SET(AM.gncmod, WORD);
	R_SET(AM.gnpowmod, WORD);
	R_SET(AM.gOutputMode, WORD);
	R_SET(AM.gOutputSpaces, WORD);
	R_SET(AM.gOutNumberType, WORD);
	R_SET(AM.gfunpowers, int);
	R_SET(AM.gPolyFun, WORD);
	R_SET(AM.gPolyFunType, WORD);
	R_SET(AM.gSlavePatchSize, LONG);
	R_SET(AM.gproperorderflag, int);
	R_SET(AM.gThreadBucketSize, LONG);
	R_SET(AM.gThreadStats, int);
	R_SET(AM.gFinalStats, int);
	R_SET(AM.gThreadsFlag, int);
	R_SET(AM.gThreadBalancing, int);
	R_SET(AM.gThreadSortFileSynch, int);
	R_SET(AM.gSortType, int);
	R_SET(AM.gShortStatsMax, WORD);

	/* #] AM */
	/* #[ AC */

	/* #[ AC free pointers */

	/* AC will be overwritten by data from the recovery file, therefore
	 * dynamically allocated memory must be freed first. */

	R_FREE_NAMETREE(AC.dollarnames);
	R_FREE_NAMETREE(AC.exprnames);
	R_FREE_NAMETREE(AC.varnames);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		R_FREE(channels[i].name);
	}
	R_FREE(AC.ChannelList.lijst);
	R_FREE(AC.DubiousList.lijst);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		TABLES T = functions[i].tabl;
		if ( T ) {
			R_FREE(T->buffers);
			R_FREE(T->mm);
			R_FREE(T->flags);
			R_FREE(T->prototype);
			R_FREE(T->tablepointers);
			if ( T->sparse ) {
				R_FREE(T->boomlijst);
				R_FREE(T->argtail);
			}
			if ( T->spare ) {
				R_FREE(T->spare->buffers);
				R_FREE(T->spare->mm);
				R_FREE(T->spare->flags);
				R_FREE(T->spare->tablepointers);
				if ( T->spare->sparse ) {
					R_FREE(T->spare->boomlijst);
				}
				R_FREE(T->spare);
			}
			R_FREE(T);
		}
	}
	R_FREE(AC.FunctionList.lijst);
	for ( i=0; i<AC.ExpressionList.num; ++i ) {
		if ( Expressions[i].renum ) {
			R_FREE(Expressions[i].renum->symb.lo);
			R_FREE(Expressions[i].renum);
		}
		if ( Expressions[i].bracketinfo ) {
			R_FREE(Expressions[i].bracketinfo->indexbuffer);
			R_FREE(Expressions[i].bracketinfo->bracketbuffer);
			R_FREE(Expressions[i].bracketinfo);
		}
		if ( Expressions[i].newbracketinfo ) {
			R_FREE(Expressions[i].newbracketinfo->indexbuffer);
			R_FREE(Expressions[i].newbracketinfo->bracketbuffer);
			R_FREE(Expressions[i].newbracketinfo);
		}
		if ( Expressions[i].renumlists != AN.dummyrenumlist ) { 
			R_FREE(Expressions[i].renumlists);
		}
		R_FREE(Expressions[i].inmem);
	}
	R_FREE(AC.ExpressionList.lijst);
	R_FREE(AC.IndexList.lijst);
	R_FREE(AC.SetElementList.lijst);
	R_FREE(AC.SetList.lijst);
	R_FREE(AC.SymbolList.lijst);
	R_FREE(AC.VectorList.lijst);
	R_FREE(AC.PotModDolList.lijst);
#ifdef WITHPTHREADS
	for ( i=0; i<AC.ModOptDolList.num; ++i ) {
		if ( ModOptdollars[i].dstruct ) {
			R_FREE(ModOptdollars[i].dstruct->where);
			R_FREE(ModOptdollars[i].dstruct);
		}
	}
#endif
	R_FREE(AC.ModOptDolList.lijst);
	for ( i=0; i<AC.TableBaseList.num; ++i ) {
		R_FREE(tablebases[i].iblocks);
		R_FREE(tablebases[i].nblocks);
		R_FREE(tablebases[i].name);
		R_FREE(tablebases[i].fullname);
		R_FREE(tablebases[i].tablenames);
	}
	R_FREE(AC.TableBaseList.lijst);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		R_FREE(cbuf[i].Buffer);
		R_FREE(cbuf[i].lhs);
		R_FREE(cbuf[i].rhs);
		R_FREE(cbuf[i].boomlijst);
	}
	R_FREE(AC.cbufList.lijst);
	R_FREE(AC.AutoSymbolList.lijst);
	R_FREE(AC.AutoIndexList.lijst);
	R_FREE(AC.AutoVectorList.lijst);
	/* Tables cannot be auto-declared, therefore no extra code here */
	R_FREE(AC.AutoFunctionList.lijst);
	R_FREE_NAMETREE(AC.autonames);
	for ( i=0; i<AC.NumStreams; ++i ) {
		R_FREE_STREAM(AC.Streams[i]);
	}
	R_FREE(AC.Streams);
	R_FREE(AC.termstack);
	R_FREE(AC.termsortstack);
	R_FREE(AC.cmod);
	R_FREE(AC.modpowers);
	R_FREE(AC.IfHeap);
	R_FREE(AC.IfCount);
	R_FREE(AC.iBuffer);
	for ( i=0; i<AC.NumLabels; ++i ) {
		R_FREE(AC.LabelNames[i]);
	}
	R_FREE(AC.LabelNames);
	R_FREE(AC.FixIndices);
	R_FREE(AC.termsumcheck);
	R_FREE(AC.WildcardNames);
	R_FREE(AC.tokens);
	R_FREE(AC.tokenarglevel);
#ifdef WITHPTHREADS
	R_FREE(AC.inputnumbers);
#endif
	R_FREE(AC.IfSumCheck);
	R_FREE(AC.CheckpointRunAfter);
	R_FREE(AC.CheckpointRunBefore);

	/* #] AC free pointers */

	/* first we copy AC as a whole and then restore the pointer structures step
	   by step. */

	AC = *((struct C_const*)p); p = (unsigned char*)p + sizeof(struct C_const);
	
	R_COPY_NAMETREE(AC.dollarnames);
	R_COPY_NAMETREE(AC.exprnames);
	R_COPY_NAMETREE(AC.varnames);

	R_COPY_LIST(AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		R_COPY_S(channels[i].name,char*);
		/* TODO restore correct handle for channel */
	}

	R_COPY_LIST(AC.FunctionList);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		if ( functions[i].tabl ) {
			/* TODO correcting pointers, e.g. prototype */
			TABLES tabl;
			R_COPY_B(functions[i].tabl, sizeof(struct TaBlEs), TABLES);
			tabl = functions[i].tabl;
			if ( tabl->tablepointers ) {
				if ( tabl->sparse ) {
					R_COPY_B(tabl->tablepointers,
						tabl->reserved*sizeof(WORD)*(tabl->numind+TABLEEXTENSION),
						WORD*);
				}
				else {
					R_COPY_B(tabl->tablepointers, 
						TABLEEXTENSION*sizeof(WORD)*(tabl->totind), WORD*);
				}
			}
#ifdef WITHPTHREADS			
			/* TODO */
#else
			R_COPY_B(tabl->prototype, tabl->prototypeSize, WORD*);
#endif			
			R_COPY_B(tabl->mm, tabl->numind*sizeof(MINMAX), MINMAX*);
			R_COPY_B(tabl->flags, tabl->numind*sizeof(WORD), WORD*);
			R_COPY_B(tabl->boomlijst, tabl->MaxTreeSize*sizeof(COMPTREE), COMPTREE*);
			R_COPY_S(tabl->argtail,UBYTE*);
			if ( tabl->spare ) {
				TABLES spare;
				R_COPY_B(tabl->spare, sizeof(struct TaBlEs), TABLES);
				spare = tabl->spare;
				if ( spare->tablepointers ) {
					if ( spare->sparse ) {
						R_COPY_B(spare->tablepointers,
							spare->reserved*sizeof(WORD)*(spare->numind+TABLEEXTENSION),
							WORD*);
					}
					else {
						R_COPY_B(spare->tablepointers,
							TABLEEXTENSION*sizeof(WORD)*(spare->totind), WORD*);
					}
				}
#ifdef WITHPTHREADS			
			/* TODO */
#else
				R_COPY_B(spare->prototype, spare->prototypeSize, WORD*);
#endif			
				R_COPY_B(spare->mm, spare->numind*sizeof(MINMAX), MINMAX*);
				R_COPY_B(spare->flags, spare->numind*sizeof(WORD), WORD*);
				R_COPY_B(spare->boomlijst, spare->MaxTreeSize*sizeof(COMPTREE), COMPTREE*);
				R_COPY_S(spare->argtail,UBYTE*);
				R_COPY_B(spare->buffers, spare->bufferssize*sizeof(WORD), WORD*);
			}
			R_COPY_B(tabl->buffers, tabl->bufferssize*sizeof(WORD), WORD*);
		}
	}

	R_COPY_LIST(AC.ExpressionList);
	for ( i=0; i<AC.ExpressionList.num; ++i ) {
		EXPRESSIONS ex = Expressions + i;
		if ( ex->renum ) {
			R_COPY_B(ex->renum, sizeof(struct ReNuMbEr), RENUMBER);
			org = ex->renum->symb.lo;
			R_SET(size, size_t);
			R_COPY_B(ex->renum->symb.lo, size, WORD*);
			ofs = ex->renum->symb.lo - (WORD*)org;
			ex->renum->symb.start += ofs;
			ex->renum->symb.hi += ofs;
			ex->renum->indi.lo += ofs;
			ex->renum->indi.start += ofs;
			ex->renum->indi.hi += ofs;
			ex->renum->vect.lo += ofs;
			ex->renum->vect.start += ofs;
			ex->renum->vect.hi += ofs;
			ex->renum->func.lo += ofs;
			ex->renum->func.start += ofs;
			ex->renum->func.hi += ofs;
			ex->renum->symnum += ofs;
			ex->renum->indnum += ofs;
			ex->renum->vecnum += ofs;
			ex->renum->funnum += ofs;
		}
		if ( ex->bracketinfo ) {
			R_COPY_B(ex->bracketinfo, sizeof(BRACKETINFO), BRACKETINFO*);
			R_COPY_B(ex->bracketinfo->indexbuffer, ex->bracketinfo->indexbuffersize, BRACKETINDEX*);
			R_COPY_B(ex->bracketinfo->bracketbuffer, ex->bracketinfo->bracketbuffersize, WORD*);
		}
		if ( ex->newbracketinfo ) {
			R_COPY_B(ex->newbracketinfo, sizeof(BRACKETINFO), BRACKETINFO*);
			R_COPY_B(ex->newbracketinfo->indexbuffer, ex->newbracketinfo->indexbuffersize, BRACKETINDEX*);
			R_COPY_B(ex->newbracketinfo->bracketbuffer, ex->newbracketinfo->bracketbuffersize, WORD*);
		}
		/* TODO restore ex->renumlists correctly */
		if ( ex->inmem ) {
			R_SET(size, size_t);
			R_COPY_B(ex->inmem, size, WORD*);
		}
	}

	R_COPY_LIST(AC.IndexList);
	R_COPY_LIST(AC.SetElementList);
	R_COPY_LIST(AC.SetList);
	R_COPY_LIST(AC.SymbolList);
	R_COPY_LIST(AC.VectorList);
	R_COPY_LIST(AC.PotModDolList);

	R_COPY_LIST(AC.ModOptDolList);
#ifdef WITHPTHREADS
	for ( i=0; i<AC.ModOptDolList.num; ++i ) {
		R_COPY_B(ModOptdollars[i].dstruct, sizeof(struct DoLlArS), DOLLARS);
		R_COPY_B(ModOptdollars[i].dstruct->where, ModOptdollars[i].dstruct->size*sizeof(WORD), WORD*);
		ModOptdollars[i].dstruct->pthreadslockread = dummylock;
		ModOptdollars[i].dstruct->pthreadslockwrite = dummylock;
	}
#endif /* ifdef WITHPTHREADS */

	R_COPY_LIST(AC.TableBaseList);
	for ( i=0; i<AC.TableBaseList.num; ++i ) {
		if ( tablebases[i].iblocks ) {
			R_COPY_B(tablebases[i].iblocks, tablebases[i].info.numberofindexblocks*sizeof(INDEXBLOCK*),INDEXBLOCK**);
			for ( j=0; j<tablebases[i].info.numberofindexblocks; ++j ) {
				if ( tablebases[i].iblocks[j] ) {
					R_COPY_B(tablebases[i].iblocks[j], sizeof(INDEXBLOCK), INDEXBLOCK*);
				}
			}
		}
		if ( tablebases[i].nblocks ) {
			R_COPY_B(tablebases[i].nblocks, tablebases[i].info.numberofnamesblocks*sizeof(NAMESBLOCK*),NAMESBLOCK**);
			for ( j=0; j<tablebases[i].info.numberofindexblocks; ++j ) {
				if ( tablebases[i].nblocks[j] ) {
					R_COPY_B(tablebases[i].nblocks[j], sizeof(NAMESBLOCK), NAMESBLOCK*);
				}
			}
		}
		R_COPY_S(tablebases[i].name,char*);
		R_COPY_S(tablebases[i].fullname,char*);
		R_COPY_S(tablebases[i].tablenames,char*);
	}

	R_COPY_LIST(AC.cbufList);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		org = cbuf[i].Buffer;
		R_COPY_B(cbuf[i].Buffer, cbuf[i].BufferSize*sizeof(WORD), WORD*);
		ofs = cbuf[i].Buffer - (WORD*)org;
		cbuf[i].Top += ofs;
		cbuf[i].Pointer += ofs;
		R_COPY_B(cbuf[i].lhs, cbuf[i].maxlhs*sizeof(WORD*), WORD**);
		R_COPY_B(cbuf[i].rhs, cbuf[i].maxrhs*(sizeof(WORD*)+2*sizeof(LONG)+sizeof(WORD)), WORD**);
		R_COPY_B(cbuf[i].boomlijst, cbuf[i].MaxTreeSize*sizeof(COMPTREE), COMPTREE*);
		/* TODO correct lhs, rhs */
		cbuf[i].CanCommu = (LONG*)((WORD*)cbuf[i].CanCommu + ofs);
		cbuf[i].NumTerms = (LONG*)((WORD*)cbuf[i].NumTerms + ofs);
		cbuf[i].numdum += ofs;
	}

	R_COPY_LIST(AC.AutoSymbolList);
	R_COPY_LIST(AC.AutoIndexList);
	R_COPY_LIST(AC.AutoVectorList);
	R_COPY_LIST(AC.AutoFunctionList);

	R_COPY_NAMETREE(AC.autonames);

	org = AC.Streams;
	R_COPY_B(AC.Streams, AC.MaxNumStreams*sizeof(STREAM), STREAM*);
	for ( i=0; i<AC.NumStreams; ++i ) {
		if ( AC.Streams[i].type != FILESTREAM ) {
			org2 = AC.Streams[i].buffer;
			R_COPY_B(AC.Streams[i].buffer, AC.Streams[i].buffersize, UBYTE*);
			ofs = AC.Streams[i].buffer - (UBYTE*)org2;
			AC.Streams[i].pointer += ofs;
			AC.Streams[i].top += ofs;
		}
		else {
			p = (unsigned char*)p + AC.Streams[i].buffersize;
		}
		R_COPY_S(AC.Streams[i].FoldName,UBYTE*);
		R_COPY_S(AC.Streams[i].name,UBYTE*);
		if ( AC.Streams[i].type == PREVARSTREAM || AC.Streams[i].type == DOLLARSTREAM ) {
			AC.Streams[i].pname = AC.Streams[i].name;
		}
		if ( AC.Streams[i].type == FILESTREAM ) {
			/* TODO check whether a FILESTREAM is already open or not!
			   for now we assume that it is ... BIG BUG! */
			org2 = AC.Streams[i].buffer;
			AC.Streams[i].buffer = (UBYTE*)Malloc1(AC.Streams[i].buffersize, "buffer");
			ofs = AC.Streams[i].buffer - (UBYTE*)org2;
			AC.Streams[i].pointer += ofs;
			AC.Streams[i].top += ofs;
			
			PUTZERO(pos);
			ADDPOS(pos, AC.Streams[i].bufferposition);
			SeekFile(AC.Streams[i].handle, &pos, SEEK_SET);

			AC.Streams[i].inbuffer = ReadFile(AC.Streams[i].handle, AC.Streams[i].buffer, AC.Streams[i].inbuffer);

			SETBASEPOSITION(pos, AC.Streams[i].fileposition);
			SeekFile(AC.Streams[i].handle, &pos, SEEK_SET);
		}
	}
	ofs = AC.Streams - (STREAM*)org;
	AC.CurrentStream += ofs;

	if ( AC.termstack ) {
		R_COPY_B(AC.termstack, AC.maxtermlevel*sizeof(LONG), LONG*);
	}

	if ( AC.termsortstack ) {
		R_COPY_B(AC.termsortstack, AC.maxtermlevel*sizeof(LONG), LONG*);
	}

	R_COPY_B(AC.cmod, AM.MaxTal*4*sizeof(UWORD), WORD*);

	if ( AC.modpowers ) {
		R_SET(size, size_t);
		R_COPY_B(AC.modpowers,size,UWORD*);
	}

	/* TODO reassign IfHeap, ... */

	org = AC.iBuffer;
	R_COPY_B(AC.iBuffer, AC.iBufferSize+1, UBYTE*);
	ofs = AC.iBuffer - (UBYTE*)org;
	AC.iPointer += ofs;
	AC.iStop += ofs;

	if ( AC.LabelNames ) {
		R_COPY_B(AC.LabelNames, AC.MaxLabels*(sizeof(UBYTE*)+sizeof(WORD)), UBYTE**);
		for ( i=0; i<AC.NumLabels; ++i ) {
			R_COPY_S(AC.LabelNames[i],UBYTE*);
		}
	}
	
	R_COPY_B(AC.FixIndices, AM.OffsetIndex*sizeof(WORD), WORD*);

	if ( AC.termsumcheck ) {
		R_COPY_B(AC.termsumcheck, AC.maxtermlevel*sizeof(WORD), WORD*);
	}

	R_COPY_B(AC.WildcardNames, AC.WildcardBufferSize, UBYTE*);

	size = AC.toptokens - AC.tokens;
	if ( size ) {
		R_COPY_B(AC.tokens, size, SBYTE*);
	}
	AC.toptokens = AC.tokens + size;

	R_COPY_B(AC.tokenarglevel, AM.MaxParLevel*sizeof(WORD), WORD*);

#ifdef WITHPTHREADS
	if ( AC.inputnumbers ) {
		org = AC.inputnumbers;
		R_COPY_B(AC.inputnumbers, AC.sizepfirstnum*(sizeof(WORD)+sizeof(LONG)), LONG*);
		ofs = AC.inputnumbers - (LONG*)org;
		AC.pfirstnum += ofs;
	}
#endif /* ifdef WITHPTHREADS */

	/* #] AC */
	/* #[ AP */

	/* #[ AP free pointers */

	/* AP will be overwritten by data from the recovery file, therefore
	 * dynamically allocated memory must be freed first. */

	for ( i=0; i<AP.DollarList.num; ++i ) {
		if ( Dollars[i].size ) {
			R_FREE(Dollars[i].where);
		}
	}
	if ( AP.DollarList.maxnum ) {
		R_FREE(AP.DollarList.lijst);
	}

	for ( i=0; i<AP.PreVarList.num; ++i ) {
		R_FREE(PreVar[i].name);
	}
	if ( AP.PreVarList.maxnum ) {
		R_FREE(AP.PreVarList.lijst);
	}

	for ( i=0; i<AP.LoopList.num; ++i ) {
		R_FREE(DoLoops[i].p.buffer);
		if ( DoLoops[i].dollarname ) {
			R_FREE(DoLoops[i].dollarname);
		}
	}
	if ( AP.LoopList.maxnum ) {
		R_FREE(AP.LoopList.lijst);
	}
	
	for ( i=0; i<AP.ProcList.num; ++i ) {
		R_FREE(Procedures[i].p.buffer);
		R_FREE(Procedures[i].name);
	}
	if ( AP.ProcList.maxnum ) {
		R_FREE(AP.ProcList.lijst);
	}

	if ( AP.ChDollarList.maxnum ) {
		R_FREE(AP.ChDollarList.lijst);
	}

	for ( i=0; i<=AP.PreSwitchLevel; ++i ) {
		R_FREE(AP.PreSwitchStrings[i]);
	}
	R_FREE(AP.PreSwitchStrings);
	R_FREE(AP.preStart);
	R_FREE(AP.procedureExtension);
	R_FREE(AP.cprocedureExtension);
	R_FREE(AP.PreIfStack);
	R_FREE(AP.PreSwitchModes);
	R_FREE(AP.PreTypes);

	/* #] AP free pointers */
	
	/* first we copy AP as a whole and then restore the pointer structures step
	   by step. */

	AP = *((struct P_const*)p); p = (unsigned char*)p + sizeof(struct P_const);
#ifdef WITHPTHREADS
	AP.PreVarLock = dummylock;
#endif

	R_COPY_LIST(AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		size = Dollars[i].size * sizeof(WORD);
		if ( size ) {
			R_COPY_B(Dollars[i].where, size, void*);
		}
#ifdef WITHPTHREADS
		Dollars[i].pthreadslockread = dummylock;
		Dollars[i].pthreadslockwrite = dummylock;
#endif
	}

	R_COPY_LIST(AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		R_SET(size, size_t);
		org = PreVar[i].name;
		R_COPY_B(PreVar[i].name, size, UBYTE*);
		ofs = PreVar[i].name - (UBYTE*)org;
		if ( PreVar[i].value ) {
			PreVar[i].value += ofs;
		}
		if ( PreVar[i].argnames ) {
			PreVar[i].argnames += ofs;
		}
	}

	R_COPY_LIST(AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		org = DoLoops[i].p.buffer;
		R_COPY_B(DoLoops[i].p.buffer, DoLoops[i].p.size, UBYTE*);
		ofs = DoLoops[i].p.buffer - (UBYTE*)org;
		DoLoops[i].name += ofs;
		DoLoops[i].vars += ofs;
		DoLoops[i].contents += ofs;
		R_COPY_S(DoLoops[i].dollarname,UBYTE*);
	}

	R_COPY_LIST(AP.ProcList);
	for ( i=0; i<AP.ProcList.num; ++i ) {
		R_COPY_B(Procedures[i].p.buffer, Procedures[i].p.size, UBYTE*);
		R_COPY_S(Procedures[i].name,UBYTE*);
	}

	R_COPY_LIST(AP.ChDollarList);

	size = (AP.NumPreSwitchStrings+1)*sizeof(UBYTE*);
	R_COPY_B(AP.PreSwitchStrings, size, UBYTE**);
	for ( i=0; i<=AP.PreSwitchLevel; ++i ) {
		R_COPY_S(AP.PreSwitchStrings[i],UBYTE*);
	}

	org = AP.preStart;
	R_COPY_B(AP.preStart, AP.pSize, UBYTE*);
	ofs = AP.preStart - (UBYTE*)org;
	if ( AP.preFill ) {
		AP.preFill += ofs;
	}
	AP.preStop += ofs;

	R_COPY_S(AP.procedureExtension,UBYTE*);
	R_COPY_S(AP.cprocedureExtension,UBYTE*);

	R_COPY_B(AP.PreIfStack, AP.MaxPreIfLevel*sizeof(int), int*);
	R_COPY_B(AP.PreSwitchModes, (AP.NumPreSwitchStrings+1)*sizeof(int), int*);
	R_COPY_B(AP.PreTypes, (AP.MaxPreTypes+1)*sizeof(int), int*);

	/* #] AP */
	/* #[ AR */

	/* TODO free stuff */

	org = AR.Fscr;
	org2 = AR.CompressBuffer;
	org3 = AR.ComprTop;
	org4 = AR.CompressPointer;
	org5 = AR.CompareRoutine;

	AR = *((struct R_const*)p); p = (unsigned char*)p + sizeof(struct R_const);

	ofs = AR.Fscr - (FILEHANDLE*)org;
	AR.infile += ofs;
	AR.outfile += ofs;
	AR.hidefile += ofs;

	AR.CompressBuffer = org2;
	AR.ComprTop = org3;
	AR.CompressPointer = org4;
	AR.CompareRoutine = org5;

	/* TODO correct restoring of FILEHANDLE */
	for ( i=0; i<3; ++i ) {
		org = AR.Fscr[i].PObuffer;
		size = AR.Fscr[i].POfull - AR.Fscr[i].PObuffer;
		if ( size ) {
			AR.Fscr[i].PObuffer = (WORD*)Malloc1(AR.Fscr[i].POsize, "PObuffer");
			R_COPY_B(AR.Fscr[i].PObuffer, size*sizeof(WORD), WORD*);
			ofs = AR.Fscr[i].PObuffer - (WORD*)org;
			AR.Fscr[i].POstop += ofs;
			AR.Fscr[i].POfill += ofs;
			AR.Fscr[i].POfull += ofs;
		}
		R_COPY_S(AR.Fscr[i].name,char*);
#ifdef WITHPTHREADS
		/* TODO
		 * restore wPObuffer ...
		 */
		AR.Fscr[i].pthreadslock = dummylock;
#endif
		/* TODO
		 *
		 * WITHZLIB data
	 	 */
	}

	/* TODO
	 *
	 * FoStage4 restore
	 */

	/* #] AR */
	/* #[ AX */

	/* TODO */

	/* #] AX */
	/* #[ Local static variables problem */

	/* TODO especially file handles ... */

	/* #] Local static variables problem */

	if ( fclose(fd) ) return(__LINE__);

	if ( AR.outfile->handle >= 0 ) {
		CloseFile(AR.outfile->handle);

		i = strlen(sortfile);
		syscmdsort = (char*)malloc(i+strlen(AR.outfile->name)+8);
		strcpy(syscmdsort, "cp -f ");
		strcpy(syscmdsort+6, sortfile);
		syscmdsort[6+i] = ' ';
		strcpy(syscmdsort+7+i, AR.outfile->name);
		system(syscmdsort);

		AR.outfile->handle = OpenFile(AR.outfile->name);

		free(syscmdsort);
	}

	if ( ISNOTZEROPOS(AR.StoreData.Fill) ) {
		CloseFile(AR.StoreData.Handle);

		i = strlen(storefile);
		syscmdstore = (char*)malloc(i+strlen(FG.fname)+8);
		strcpy(syscmdstore, "cp -f ");
		strcpy(syscmdstore+6, storefile);
		syscmdstore[6+i] = ' ';
		strcpy(syscmdstore+7+i, FG.fname);
		system(syscmdstore);

		AR.StoreData.Handle = (WORD)OpenFile(FG.fname);

		free(syscmdstore);
	}

	/* TODO UpdatePositions(), cares about data in S_const */

	printf("done.\n");

	return(0);
}

/*
  	#] DoRecovery :
  	#[ DoSnapshot :
*/

/**
 *  Writes all relevant information for a recovery to the recovery file. It
 *  writes first to an intermediate file and then only if everything went well
 *  it renames this intermediate file to the final recovery file. Then it copies
 *  the sort and store files if necessary.
 */
static int DoSnapshot()
{
	GETIDENTITY
	FILE *fd;
	POSITION pos;
	int i, j, l;
	WORD *w;

	printf("Saving recovery point ... "); fflush(0);

	if ( !(fd = fopen(intermedfile, "w")) ) return(__LINE__);

	/* reserve space in the file for a length field */
	if ( fwrite(&pos, sizeof(POSITION), 1, fd) != 1 ) return(__LINE__);

	/* #[ AM */

	/* since most values don't change during execution, AM doesn't need to be
	 * written as a whole. all values will be correctly set when starting up
	 * anyway. only the exceptions need to be taken care of. see MakeGlobal()
	 * and PopVariables() in execute.c. */

	S_WRITE_B(&AM.hparallelflag, sizeof(int));
	S_WRITE_B(&AM.gparallelflag, sizeof(int));
	S_WRITE_B(&AM.gCodesFlag, sizeof(int));
	S_WRITE_B(&AM.gNamesFlag, sizeof(int));
	S_WRITE_B(&AM.gStatsFlag, sizeof(int));
	S_WRITE_B(&AM.gNoSpacesInNumbers, sizeof(int));
	S_WRITE_B(&AM.gIndentSpace, sizeof(WORD));
	S_WRITE_B(&AM.gUnitTrace, sizeof(WORD));
	S_WRITE_B(&AM.gDefDim, sizeof(int));
	S_WRITE_B(&AM.gDefDim4, sizeof(int));
	S_WRITE_B(&AM.gncmod, sizeof(WORD));
	S_WRITE_B(&AM.gnpowmod, sizeof(WORD));
	S_WRITE_B(&AM.gOutputMode, sizeof(WORD));
	S_WRITE_B(&AM.gOutputSpaces, sizeof(WORD));
	S_WRITE_B(&AM.gOutNumberType, sizeof(WORD));
	S_WRITE_B(&AM.gfunpowers, sizeof(int));
	S_WRITE_B(&AM.gPolyFun, sizeof(WORD));
	S_WRITE_B(&AM.gPolyFunType, sizeof(WORD));
	S_WRITE_B(&AM.gSlavePatchSize, sizeof(LONG));
	S_WRITE_B(&AM.gproperorderflag, sizeof(int));
	S_WRITE_B(&AM.gThreadBucketSize, sizeof(LONG));
	S_WRITE_B(&AM.gThreadStats, sizeof(int));
	S_WRITE_B(&AM.gFinalStats, sizeof(int));
	S_WRITE_B(&AM.gThreadsFlag, sizeof(int));
	S_WRITE_B(&AM.gThreadBalancing, sizeof(int));
	S_WRITE_B(&AM.gThreadSortFileSynch, sizeof(int));
	S_WRITE_B(&AM.gSortType, sizeof(int));
	S_WRITE_B(&AM.gShortStatsMax, sizeof(WORD));

	/* #] AM */
	/* #[ AC */

	/* we write AC as a whole and then write all additional data step by step.
	 * AC.DubiousList doesn't need to be treated, because it should be empty. */

	S_WRITE_B(&AC, sizeof(struct C_const));

	S_WRITE_NAMETREE(AC.dollarnames);
	S_WRITE_NAMETREE(AC.exprnames);
	S_WRITE_NAMETREE(AC.varnames);
	
	S_WRITE_LIST(AC.ChannelList);
	for ( i=0; i<AC.ChannelList.num; ++i ) {
		S_WRITE_S(channels[i].name);
	}

	S_WRITE_LIST(AC.FunctionList);
	for ( i=0; i<AC.FunctionList.num; ++i ) {
		/* if the function is a table */
		if ( functions[i].tabl ) {
			TABLES tabl = functions[i].tabl;
			S_WRITE_B(tabl, sizeof(struct TaBlEs));
			if ( tabl->tablepointers ) {
				if ( tabl->sparse ) {
					/* sparse tables. reserved holds number of allocated
					 * elements. the size of an element is numind plus
					 * TABLEEXTENSION times the size of WORD. */
					S_WRITE_B(tabl->tablepointers,
						tabl->reserved*sizeof(WORD)*(tabl->numind+TABLEEXTENSION));
				}
				else {
					/* matrix like tables. */
					S_WRITE_B(tabl->tablepointers,
						TABLEEXTENSION*sizeof(WORD)*(tabl->totind));
				}
			}
			S_WRITE_B(tabl->prototype, tabl->prototypeSize);
			S_WRITE_B(tabl->mm, tabl->numind*sizeof(MINMAX));
			S_WRITE_B(tabl->flags, tabl->numind*sizeof(WORD));
			S_WRITE_B(tabl->boomlijst, tabl->MaxTreeSize*sizeof(COMPTREE));
			S_WRITE_S(tabl->argtail);
			if ( tabl->spare ) {
				TABLES spare = tabl->spare;
				S_WRITE_B(spare, sizeof(struct TaBlEs));
				if ( spare->tablepointers ) {
					if ( spare->sparse ) {
						/* sparse tables */
						S_WRITE_B(spare->tablepointers,
							spare->reserved*sizeof(WORD)*(spare->numind+TABLEEXTENSION));
					}
					else {
						/* matrix like tables */
						S_WRITE_B(spare->tablepointers,
							TABLEEXTENSION*sizeof(WORD)*(spare->totind));
					}
				}
				S_WRITE_B(spare->prototype, spare->prototypeSize);
				S_WRITE_B(spare->mm, spare->numind*sizeof(MINMAX));
				S_WRITE_B(spare->flags, spare->numind*sizeof(WORD));
				S_WRITE_B(spare->boomlijst, spare->MaxTreeSize*sizeof(COMPTREE));
				S_WRITE_S(spare->argtail);
				S_WRITE_B(spare->buffers, spare->bufferssize*sizeof(WORD));
			}
			S_WRITE_B(tabl->buffers, tabl->bufferssize*sizeof(WORD));
		}
	}

	S_WRITE_LIST(AC.ExpressionList);
	for ( i=0; i<AC.ExpressionList.num; ++i ) {
		EXPRESSIONS ex = Expressions + i;
		if ( ex->renum ) {
			S_WRITE_B(ex->renum, sizeof(struct ReNuMbEr));
			/* there is one dynamically allocated buffer for struct ReNuMbEr and
			 * symb.lo points to its beginning. the size of the buffer is not
			 * stored anywhere but we know it is 2*sizeof(WORD)*N, where N is
			 * the number of all vectors, indices, functions and symbols. since
			 * funum points into the buffer at a distance 2N-[Number of
			 * functions] from symb.lo (see GetTable() in store.c), we can
			 * calculate the buffer size by some pointer arithmetic. the size is
			 * then written to the file. */
			l = ex->renum->funnum - ex->renum->symb.lo;
			l += ex->renum->funnum - ex->renum->func.lo;
			S_WRITE_B(&l, sizeof(size_t));
			S_WRITE_B(ex->renum->symb.lo, l);
		}
		if ( ex->bracketinfo ) {
			S_WRITE_B(ex->bracketinfo, sizeof(BRACKETINFO));
			S_WRITE_B(ex->bracketinfo->indexbuffer, ex->bracketinfo->indexbuffersize);
			S_WRITE_B(ex->bracketinfo->bracketbuffer, ex->bracketinfo->bracketbuffersize);
		}
		if ( ex->newbracketinfo ) {
			S_WRITE_B(ex->newbracketinfo, sizeof(BRACKETINFO));
			S_WRITE_B(ex->newbracketinfo->indexbuffer, ex->newbracketinfo->indexbuffersize);
			S_WRITE_B(ex->newbracketinfo->bracketbuffer, ex->newbracketinfo->bracketbuffersize);
		}
		if ( ex->inmem ) {
			/* size of the inmem buffer has to be determined. we use the fact
			 * that the end of an expression is marked by a zero. */
			w = ex->inmem;
			while ( *w++ ) ;
			l = w - ex->inmem;
			S_WRITE_B(&l, sizeof(size_t));
			S_WRITE_B(ex->inmem, l);
		}
	}
	
	S_WRITE_LIST(AC.IndexList);
	S_WRITE_LIST(AC.SetElementList);
	S_WRITE_LIST(AC.SetList);
	S_WRITE_LIST(AC.SymbolList);
	S_WRITE_LIST(AC.VectorList);
	S_WRITE_LIST(AC.PotModDolList);
	S_WRITE_LIST(AC.ModOptDolList);
#ifdef WITHPTHREADS
	for ( i=0; i<AC.ModOptDolList.num; ++i ) {
		S_WRITE_B(ModOptdollars[i].dstruct, sizeof(struct DoLlArS));
		S_WRITE_DOLLAR((*ModOptdollars[i].dstruct));
	}
#endif /* ifdef WITHPTHREADS */

	S_WRITE_LIST(AC.TableBaseList);
	for ( i=0; i<AC.TableBaseList.num; ++i ) {
		/* see struct dbase in minos.h */
		if ( tablebases[i].iblocks ) {
			S_WRITE_B(tablebases[i].iblocks, tablebases[i].info.numberofindexblocks * sizeof(INDEXBLOCK*));
			for ( j=0; j<tablebases[i].info.numberofindexblocks; ++j ) {
				if ( tablebases[i].iblocks[j] ) {
					S_WRITE_B(tablebases[i].iblocks[j], sizeof(INDEXBLOCK));
				}
			}
		}
		if ( tablebases[i].nblocks ) {
			S_WRITE_B(tablebases[i].nblocks, tablebases[i].info.numberofnamesblocks * sizeof(NAMESBLOCK*));
			for ( j=0; j<tablebases[i].info.numberofnamesblocks; ++j ) {
				if ( tablebases[i].nblocks[j] ) {
					S_WRITE_B(tablebases[i].nblocks[j], sizeof(NAMESBLOCK));
				}
			}
		}
		S_WRITE_S(tablebases[i].name);
		S_WRITE_S(tablebases[i].fullname);
		S_WRITE_S(tablebases[i].tablenames);
	}

	S_WRITE_LIST(AC.cbufList);
	for ( i=0; i<AC.cbufList.num; ++i ) {
		S_WRITE_B(cbuf[i].Buffer, cbuf[i].BufferSize*sizeof(WORD));
		/* see inicbufs in comtool.c */
		S_WRITE_B(cbuf[i].lhs, cbuf[i].maxlhs*sizeof(WORD*));
		S_WRITE_B(cbuf[i].rhs, cbuf[i].maxrhs*(sizeof(WORD*)+2*sizeof(LONG)+sizeof(WORD)));
		S_WRITE_B(cbuf[i].boomlijst, cbuf[i].MaxTreeSize*sizeof(COMPTREE));
	}

	S_WRITE_LIST(AC.AutoSymbolList);
	S_WRITE_LIST(AC.AutoIndexList);
	S_WRITE_LIST(AC.AutoVectorList);
	S_WRITE_LIST(AC.AutoFunctionList);

	S_WRITE_NAMETREE(AC.autonames);

	S_WRITE_B(AC.Streams, AC.MaxNumStreams*sizeof(STREAM));
	for ( i=0; i<AC.NumStreams; ++i ) {
		S_WRITE_B(AC.Streams[i].buffer, AC.Streams[i].buffersize);
		S_WRITE_S(AC.Streams[i].FoldName);
		S_WRITE_S(AC.Streams[i].name);
	}

	if ( AC.termstack ) {
		S_WRITE_B(AC.termstack, AC.maxtermlevel*sizeof(LONG));
	}

	if ( AC.termsortstack ) {
		S_WRITE_B(AC.termsortstack, AC.maxtermlevel*sizeof(LONG));
	}

	S_WRITE_B(AC.cmod, AM.MaxTal*4*sizeof(UWORD));

	if ( AC.modpowers ) {
		/* size of modpowers is not stored anywhere, so we recalculate
		 * it after MakeModTable() in reken.c */
		LONG n, size;
		n = ABS(AC.ncmod);
		size = (LONG)(*AC.cmod);
		if ( n == 2 ) size += (((LONG)AC.cmod[1])<<BITSINWORD);
		l = size*n*sizeof(UWORD);
		S_WRITE_B(&l, sizeof(size_t));
		S_WRITE_B(AC.modpowers, l);
	}

	S_WRITE_B(AC.iBuffer, AC.iBufferSize+1);

	if ( AC.LabelNames ) {
		S_WRITE_B(AC.LabelNames, AC.MaxLabels*(sizeof(UBYTE*)+sizeof(WORD)));
		for ( i=0; i<AC.NumLabels; ++i ) {
			S_WRITE_S(AC.LabelNames[i]);
		}
	}

	S_WRITE_B(AC.FixIndices, AM.OffsetIndex*sizeof(WORD));

	if ( AC.termsumcheck ) {
		S_WRITE_B(AC.termsumcheck, AC.maxtermlevel*sizeof(WORD));
	}

	S_WRITE_B(AC.WildcardNames, AC.WildcardBufferSize);

	l = AC.toptokens - AC.tokens;
	if ( l ) {
		S_WRITE_B(AC.tokens, l);
	}

	S_WRITE_B(AC.tokenarglevel, AM.MaxParLevel*sizeof(WORD));
	
#ifdef WITHPTHREADS
	if ( AC.inputnumbers ) {
		S_WRITE_B(AC.inputnumbers, AC.sizepfirstnum*(sizeof(WORD)+sizeof(LONG)));
	}
#endif /* ifdef WITHPTHREADS */

	/* #] AC */
	/* #[ AP */

	/* we write AP as a whole and then write all additional data step by step. */

	S_WRITE_B(&AP, sizeof(struct P_const));

	S_WRITE_LIST(AP.DollarList);
	for ( i=0; i<AP.DollarList.num; ++i ) {
		S_WRITE_DOLLAR(Dollars[i]);
	}

	S_WRITE_LIST(AP.PreVarList);
	for ( i=0; i<AP.PreVarList.num; ++i ) {
		/* there is one dynamically allocated buffer in struct pReVaR holding
		 * the strings name, value and several argnames. the size of the buffer
		 * can be calculated by adding up their string lengths. */
		l = strlen((char*)PreVar[i].name) + 1;
		if ( PreVar[i].value ) {
			l += strlen((char*)(PreVar[i].name+l)) + 1;
		}
		for ( j=0; j<PreVar[i].nargs; ++j ) {
			l += strlen((char*)(PreVar[i].name+l)) + 1;
		}
		if ( fwrite(&l, sizeof(size_t), 1, fd) != 1 ) return(__LINE__);
		S_WRITE_B(PreVar[i].name, l);
	}

	S_WRITE_LIST(AP.LoopList);
	for ( i=0; i<AP.LoopList.num; ++i ) {
		S_WRITE_B(DoLoops[i].p.buffer, DoLoops[i].p.size);
		if ( DoLoops[i].type == ONEEXPRESSION ) {
			/* do loops with an expression keep this expression in a dynamically
			 * allocated buffer in dollarname */
			S_WRITE_S(DoLoops[i].dollarname);
		}
	}

	S_WRITE_LIST(AP.ProcList);
	for ( i=0; i<AP.ProcList.num; ++i ) {
		S_WRITE_B(Procedures[i].p.buffer, Procedures[i].p.size);
		S_WRITE_S(Procedures[i].name);
	}

	S_WRITE_LIST(AP.ChDollarList);
	
	S_WRITE_B(AP.PreSwitchStrings, (AP.NumPreSwitchStrings+1)*sizeof(UBYTE*));
	for ( i=0; i<=AP.PreSwitchLevel; ++i ) {
		S_WRITE_S(AP.PreSwitchStrings[i]);
	}

	S_WRITE_B(AP.preStart, AP.pSize);
	
	S_WRITE_S(AP.procedureExtension);
	S_WRITE_S(AP.cprocedureExtension);

	S_WRITE_B(AP.PreIfStack, AP.MaxPreIfLevel*sizeof(int));
	S_WRITE_B(AP.PreSwitchModes, (AP.NumPreSwitchStrings+1)*sizeof(int));
	S_WRITE_B(AP.PreTypes, (AP.MaxPreTypes+1)*sizeof(int));

	/* #] AP */
	/* #[ AR */

	/* we write AR as a whole and then write all additional data step by step.
	 * the additional data is basically the FILEHANLDE for the out- and
	 * hidefile. */

	S_WRITE_B(&AR, sizeof(struct R_const));

	/* outfile */
	i = AR.outfile - AR.Fscr;
	l = AR.Fscr[i].POfull - AR.Fscr[i].PObuffer;
	if ( l ) {
		S_WRITE_B(AR.Fscr[i].PObuffer, l*sizeof(WORD));
	}
	S_WRITE_S(AR.Fscr[i].name);

	/* hidefile */
	l = AR.Fscr[2].POfull - AR.Fscr[2].PObuffer;
	if ( l ) {
		S_WRITE_B(AR.Fscr[2].PObuffer, l*sizeof(WORD));
	}
	S_WRITE_S(AR.Fscr[2].name);

	/* #] AR */
	/* #[ AX */

	/* TODO */

	/* #] AX */
	/* #[ Local static variables */

	/* TODO
	 * for example local variables in tool.c might need to go into a new
	 * global struct.
	 */

	/* #] Local static variables */

	/* save length of data at the beginning of the file */
	SETBASEPOSITION(pos, (ftell(fd)));
	fseek(fd, 0, SEEK_SET);
	if ( fwrite(&pos, sizeof(POSITION), 1, fd) != 1 ) return(__LINE__);
	fseek(fd, BASEPOSITION(pos), SEEK_SET);

	if ( fclose(fd) ) return(__LINE__);

	/* prepare strings for system command */
	if ( !syscmdinit ) {
		l = strlen(AR.outfile->name);
		syscmdsort = (char*)Malloc1(l+strlen(sortfile)+8, "syscmdsort");
		strcpy(syscmdsort, "cp -f ");
		strcpy(syscmdsort+6, AR.outfile->name);
		syscmdsort[6+l] = ' ';
		strcpy(syscmdsort+7+l, sortfile);

		l = strlen(FG.fname);
		syscmdstore = (char*)Malloc1(l+strlen(storefile)+8, "syscmdstore");
		strcpy(syscmdstore, "cp -f ");
		strcpy(syscmdstore+6, FG.fname);
		syscmdstore[6+l] = ' ';
		strcpy(syscmdstore+7+l, storefile);

		syscmdinit = 1;
	}

	/* for copying we are using the system command. it is fast and has no
	 * additional memory requirements as a read/write approach would have.
	 * as a drawback we might get portability problems. */

	/* copy sort file if necessary */
	if ( AR.outfile->handle >= 0 ) {
		system(syscmdsort);
	}

	/* copy store file if necessary */
	if ( ISNOTZEROPOS(AR.StoreData.Fill) ) {
		system(syscmdstore);
	}

	/* make the intermediate file the recovery file */
	if ( rename(intermedfile, recoveryfile) ) return(__LINE__);

	printf("done.\n");

	return(0);
}

/*
  	#] DoSnapshot :
  	#[ DoCheckpoint :
*/

/**
 *  Checks whether a snapshot should be done. Calls DoSnapshot() to create the
 *  snapshot.
 */
void DoCheckpoint()
{
	int error;
	LONG timestamp = Timer(0);

	if ( timestamp - AC.CheckpointStamp > AC.CheckpointInterval ) {
		/* TODO call script */

		error = DoSnapshot();
		if ( error ) {
			printf("Error creating recovery files: %d\n", error); fflush(0);
		}

		/* TODO call script */
		/* TODO print some messages */
	}
	AC.CheckpointStamp = Timer(0);
}

/*
  	#] DoCheckpoint :
*/
