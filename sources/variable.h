#ifndef __VARIABLE__

#define __VARIABLE__

/*[14apr2004 mt]:*/
/*See the file extcmd.c*/
extern int (*writeBufToExtChannel) ARG2(char *,/**/, size_t, /**/);
extern int (*getcFromExtChannel) ARG0 ;
extern int (*setTerminatorForExternalChannel) ARG1 (char *, /**/);
/*:[14apr2004 mt]*/

/*[15apr2004 mt]:*/
extern LONG (*WriteFile)  ARG3 (int,/**/,UBYTE *,/**/,LONG,/**/);
/*:[15apr2004 mt]*/

extern ALLGLOBALS A;
#ifdef WITHPTHREADS
extern ALLPRIVATES *AB;
#endif
extern WORD *currentTerm;
extern WORD *listinprint;
extern WORD numlistinprint;

extern FIXEDGLOBALS FG;
extern FIXEDSET fixedsets[];

extern char *setupfilename;

EXTERNLOCK(ErrorMessageLock);

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
/*
#define Descriptor ((DESCRIPTORLIST *)(A.DescriptorList.lijst))
#define NumDescr A.DescriptorList.num
#define MaxNumDescr A.DescriptorList.maxnum
*/
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

#define PPchangeddollars ((WORD *)(AP.ChDollarList.lijst))
#define NumPPchangeddollars AP.ChDollarList.num
#define PotModdollars ((WORD *)(AC.PotModDolList.lijst))
#define NumPotModdollars AC.PotModDolList.num
#define ModOptdollars ((MODOPTDOLLAR *)(AC.ModOptDolList.lijst))
#define NumModOptdollars AC.ModOptDolList.num

#define BUG A.bug;

#ifdef WITHPTHREADS
#define AC A.C
#define AM A.M
#define AN AB[identity].N
#define AO A.O
#define AP A.P
#define AR AB[identity].R
#define AS A.S
#define AT AB[identity].T
#else
#define AC A.C
#define AM A.M
#define AN A.N
#define AO A.O
#define AP A.P
#define AR A.R
#define AS A.S
#define AT A.T
#endif

#endif
