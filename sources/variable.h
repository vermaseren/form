#ifndef __VARIABLE__

#define __VARIABLE__

/** @file variable.h
 *
 *  Contains a number of defines to make the coding easier.
 *	Especially the defines for the use of the lists are very nice.
 *	And of course the AC for A.C and AT for either A.T of B->T
 *	are indispensible to keep FORM and TFORM in one set of sources.
 */

/*See the file extcmd.c*/

#ifdef REMOVEDBY_MT
extern int (*writeBufToExtChannel)(char *buf, size_t n);
extern int (*getcFromExtChannel)();
extern int (*setTerminatorForExternalChannel)(char *newterminator);
#endif
extern WRITEBUFTOEXTCHANNEL writeBufToExtChannel;
extern GETCFROMEXTCHANNEL getcFromExtChannel;
extern SETTERMINATORFOREXTERNALCHANNEL setTerminatorForExternalChannel;
extern SETKILLMODEFOREXTERNALCHANNEL setKillModeForExternalChannel;

/*
extern LONG (*WriteFile)(int handle, UBYTE *buffer, LONG number);
*/
extern WRITEFILE WriteFile;
/*:[17nov2005 mt]*/

extern ALLGLOBALS A;
#ifdef WITHPTHREADS
extern ALLPRIVATES **AB;
#endif

extern FIXEDGLOBALS FG;
extern FIXEDSET fixedsets[];

extern char *setupfilename;

EXTERNLOCK(ErrorMessageLock)
EXTERNLOCK(FileReadLock)
EXTERNLOCK(dummylock)

#ifdef VMS
#include <stdio.h>
extern FILE **FileStructs;
#endif

#define chartype FG.cTable

#define Procedures ((PROCEDURE *)(AP.ProcList.lijst))
#define NumProcedures AP.ProcList.num
#define MaxProcedures AP.ProcList.maxnum
#define DoLoops ((DOLOOP *)(AP.LoopList.lijst))
#define NumDoLoops AP.LoopList.num
#define MaxDoLoops AP.LoopList.maxnum
#define PreVar ((PREVAR *)(AP.PreVarList.lijst))
#define NumPre AP.PreVarList.num
#define MaxNumPre AP.PreVarList.maxnum
#define SetElements ((WORD *)(AC.SetElementList.lijst))
#define Sets ((SETS)(AC.SetList.lijst))
#define functions ((FUNCTIONS)(AC.FunctionList.lijst))
#define indices ((INDICES)(AC.IndexList.lijst))
#define symbols ((SYMBOLS)(AC.SymbolList.lijst))
#define vectors ((VECTORS)(AC.VectorList.lijst))
#define tablebases ((DBASE *)(AC.TableBaseList.lijst))
#define NumFunctions AC.FunctionList.num
#define NumIndices AC.IndexList.num
#define NumSymbols AC.SymbolList.num
#define NumVectors AC.VectorList.num
#define NumSets AC.SetList.num
#define NumSetElements AC.SetElementList.num
#define NumTableBases AC.TableBaseList.num
#define GlobalFunctions AC.FunctionList.numglobal
#define GlobalIndices AC.IndexList.numglobal
#define GlobalSymbols AC.SymbolList.numglobal
#define GlobalVectors AC.VectorList.numglobal
#define GlobalSets AC.SetList.numglobal
#define GlobalSetElements AC.SetElementList.numglobal
#define cbuf ((CBUF *)(AC.cbufList.lijst))
#define channels ((CHANNEL *)(AC.ChannelList.lijst))
#define NumOutputChannels AC.ChannelList.num
#define Dollars ((DOLLARS)(AP.DollarList.lijst))
#define NumDollars AP.DollarList.num
#define Dubious ((DUBIOUSV)(AC.DubiousList.lijst))
#define NumDubious AC.DubiousList.num
#define Expressions ((EXPRESSIONS)(AC.ExpressionList.lijst))
#define NumExpressions AC.ExpressionList.num
#define autofunctions ((FUNCTIONS)(AC.AutoFunctionList.lijst))
#define autoindices ((INDICES)(AC.AutoIndexList.lijst))
#define autosymbols ((SYMBOLS)(AC.AutoSymbolList.lijst))
#define autovectors ((VECTORS)(AC.AutoVectorList.lijst))

/*[28sep2005 mt]:*/
/*This code is never used*/
/*
#define PPchangeddollars ((WORD *)(AP.ChDollarList.lijst))
#define NumPPchangeddollars AP.ChDollarList.num
*/
/*:[28sep2005 mt]*/

#define PotModdollars ((WORD *)(AC.PotModDolList.lijst))
#define NumPotModdollars AC.PotModDolList.num
#define ModOptdollars ((MODOPTDOLLAR *)(AC.ModOptDolList.lijst))
#define NumModOptdollars AC.ModOptDolList.num

#define BUG A.bug;

#ifdef WITHPTHREADS
#define AC A.C
#define AM A.M
#define AO A.O
#define AP A.P
#define AS A.S
#define AX A.X
#define AN B->N
#define AR B->R
#define AT B->T
#define AN0 B0->N
#define AR0 B0->R
#define AT0 B0->T
#else
#define AC A.C
#define AM A.M
#define AN A.N
#define AO A.O
#define AP A.P
#define AR A.R
#define AS A.S
#define AT A.T
#define AX A.X
#endif

#endif
