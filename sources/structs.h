#ifndef __STRUCTS__

#define __STRUCTS__
 
/*
  	#[ sav&store :
*/

typedef struct PoSiTiOn {
	off_t p1;
} POSITION;

typedef struct FiLePoSiTiOn {
	LONG	posiarray[2];		/* This way we have more than 32 bits */
} FILEPOSITION;

typedef struct BlOcKpOsItIoN {
	ULONG	blocknumber;		/* The number of the block */
	ULONG	blockoffset;		/* The offset inside the block after expansion */
} BLOCKPOSITION;
 
#define INFILEINDX 15

typedef struct {
	POSITION	nameposition;	/* Position of the name of the expression */
	POSITION	varposition;	/* Position of the list with variables */
	POSITION	varlength;		/* Length of the variable list */
	POSITION	exprposition;	/* Position of the expression itself */
/*	POSITION	exprlength;		   Length of the expression itself */
} INDXENTRY;

typedef struct {
	POSITION	nextindex;			/* Position of next FILEINDX if any */
	LONG		number;				/* Number of used entries in this index */
	LONG		reserved;			/* For the future, and for padding */
	INDXENTRY	entries[INFILEINDX];
} FILEINDX;

/*	Next are the index structs for stored and saved expressions */

typedef struct InDeXeNtRy {
	POSITION	position;		/* Position of the expression itself */
	POSITION	length;			/* Length of the expression itself */
	POSITION	variables;		/* Position of the list with variables */
	LONG	CompressSize;		/* size of buffer before compress */
	WORD	nsymbols;			/* Number of symbols in the list */
	WORD	nindices;			/* Number of indices in the list */
	WORD	nvectors;			/* Number of vectors in the list */
	WORD	nfunctions;			/* Number of functions in the list */
	WORD    size;				/* Size of variables field */
	SBYTE	name[MAXENAME+1];
	PADLONG(1,5,MAXENAME+1);
} INDEXENTRY;

/* We want sizeof(FILEINDEX) to be 512 or some other nice number */

#define INFILEINDEX ((512-sizeof(LONG)-sizeof(POSITION))/sizeof(INDEXENTRY))
#define EMPTYININDEX (512-sizeof(LONG)-sizeof(POSITION)-INFILEINDEX*sizeof(INDEXENTRY))

typedef struct FiLeInDeX {
	POSITION	next;			/* Position of next FILEINDEX if any */
	LONG	number;				/* Number of used entries in this index */
	INDEXENTRY expression[INFILEINDEX];

	SBYTE	empty[EMPTYININDEX];

} FILEINDEX;

typedef struct FiLeDaTa {
	FILEINDEX Index;
	POSITION Fill;
	POSITION Position;
	WORD Handle;
	WORD allignment;
	PADLONG(0,2,0);
} FILEDATA;

typedef struct VaRrEnUm {		/* Pointers to an array in which a */
	WORD		*start;			/* binary search will be performed */
	WORD		*lo;
	WORD		*hi;
} VARRENUM;

typedef struct ReNuMbEr {
	VARRENUM	symb;			/* First stage renumbering */
	VARRENUM	indi;
	VARRENUM	vect;
	VARRENUM	func;
	WORD		*symnum;		/* Second stage renumbering */
	WORD		*indnum;
	WORD		*vecnum;
	WORD		*funnum;
	POSITION	startposition;
	PADPOINTER(0,0,0,0);
} *RENUMBER;

typedef struct {
	WORD	nsymbols;			/* Number of symbols in the list */
	WORD	nindices;			/* Number of indices in the list */
	WORD	nvectors;			/* Number of vectors in the list */
	WORD	nfunctions;			/* Number of functions in the list */
} VARINFO;

/*
  	#] sav&store : 
  	#[ Variables :
*/

typedef struct {
	void *lijst;
	char *message;
	int num;
	int maxnum;
	int size;
	int numglobal;
	int numtemp;		/* At the moment only needed for sets and setstore */
	int numclear;		/* Only for the clear instruction */
	PADPOINTER(0,6,0,0);
} LIST;
 
typedef struct {
	char *name;
	TFUN func;
	int type;
	int flags;
} KEYWORD;

typedef struct NameLi {         /* For fast namesearching */
    WORD where;
    WORD type;
    WORD number;
} NAMELIST;
 
typedef struct NaMeNode {
	LONG name;
	WORD parent;
	WORD left;
	WORD right;
	WORD balance;
	WORD type;
	WORD number;
	PADLONG(0,6,0);
} NAMENODE;

typedef struct NaMeTree {
	NAMENODE *namenode;
	UBYTE    *namebuffer;
	LONG     namesize;
	LONG     namefill;
	LONG     nodesize;
	LONG     nodefill;
	LONG     oldnamefill;
	LONG     oldnodefill;
	LONG     globalnamefill;
	LONG     globalnodefill;
	LONG     clearnamefill;
	LONG     clearnodefill;
	WORD     headnode;
	PADPOINTER(10,0,1,0);
} NAMETREE;

typedef struct {
	char *keyword;
	CFUN compfunc;
	char *layout;
} COMPKEY;

typedef struct {
	UBYTE *contents;
	long size;				/* size of contents */
	int next;				/* number of the element in the array */
	int option;				/* same */
	int nextoption;			/* for multiple occurrence of option */
	PADPOINTER(1,3,0,0);
} DESCRIPTORLIST;
 
typedef struct tree {
	int parent;
	int left;    /* left child (if not -1) */
	int right;   /* right child (if not -1) */
	int value;   /* the object to be sorted and searched */
	int blnce;   /* balance factor */
} COMPTREE;
 
typedef struct MiNmAx {
    WORD mini;          /* minimum value */
    WORD maxi;          /* maximum value */
    WORD size;          /* value of one unit in this position */
} MINMAX;
 
typedef struct BrAcKeTiNdEx {	/* For indexing brackets in local expressions */
	POSITION start;				/* Place where bracket starts - start of expr */
	POSITION next;				/* Place of next indexed bracket in expr */
	LONG bracket;				/* Offset of position in bracketbuffer */
	PADPOINTER(1,0,0,0);
} BRACKETINDEX;

typedef struct BrAcKeTiNfO {
	BRACKETINDEX *indexbuffer;
	WORD *bracketbuffer;
	LONG bracketbuffersize;
	LONG indexbuffersize;
	LONG bracketfill;
	LONG indexfill;
	WORD SortType;				/* The sorting criterium used (like POWERFIRST etc) */
	PADPOINTER(4,0,1,0);
} BRACKETINFO;
 
typedef struct TaBlEs {
    WORD    *tablepointers; /* start in tablepointers table */
    WORD    *prototype;     /* The wildcard prototyping for arguments */
	MINMAX	*mm;		    /* Array bounds, dimension by dimension */
	WORD	*flags;         /* Is element in use ? etc */
    WORD    *pattern;       /* The pattern with which to match the arguments */
	COMPTREE *boomlijst;	/* Tree for searching in sparse tables */
	UBYTE	*argtail;       /* The arguments in characters. For tablebase
                               Starts with parenthesis to indicate tail */
	struct TaBlEs *spare;   /* For tablebase. Alternatingly stubs and real */
	WORD	*buffers;		/* When we use more than one compiler buffer */
    LONG    totind;         /* Total number requested */
    LONG    reserved;       /* total reservation in tablepointers for sparse */
	LONG	defined;		/* Number of table elements that are defined */
	LONG	mdefined;		/* same but after .global */
    int     numind;         /* Number of array indices */
    int     bounds;         /* array bounds check on/off. */
    int     strict;         /* >0: all must be defined. <0: undefined not substitute */
    int     sparse;         /* > 0 --> sparse table */
	int		numtree;		/* For the tree for sparse tables */
	int		rootnum;        /* For the tree for sparse tables */
	int		MaxTreeSize;    /* For the tree for sparse tables */
	WORD	bufnum;			/* Each table potentially its own buffer */
	WORD	bufferssize;    /* When we use more than one compiler buffer */
	WORD	buffersfill;	/* When we use more than one compiler buffer */
	WORD	tablenum;       /* For testing of tableuse */
	WORD	mode;			/* 0: normal, 1: stub */
	WORD	notusedyet;		/* For future considerations */
	PADPOINTER(4,7,6,0);
} *TABLES;

typedef struct ExPrEsSiOn {
	RENUMBER renum;			/* For Renumbering of global stored expressions */
	BRACKETINFO *bracketinfo;
	BRACKETINFO *newbracketinfo;
	WORD	*renumlists;
	WORD	*inmem;			/* If in memory like e.g. a polynomial */
	POSITION	onfile;
	POSITION	prototype;
	LONG	counter;
	LONG	name;
	WORD	hidelevel;
	WORD	vflags;			/* Various flags */
	WORD	printflag;
	WORD	status;
	WORD	replace;
	WORD	node;
	WORD	whichbuffer;
	WORD	namesize;
	PADPOINTER(4,0,8,0);
} *EXPRESSIONS;

typedef struct SyMbOl {			/* Don't change unless altering .sav too */
	LONG	name;				/* Location in names buffer */
	WORD	minpower;			/* Minimum power admissible */
	WORD	maxpower;			/* Maximum power admissible */
	WORD	complex;			/* Properties wrt complex conjugation */
	WORD	number;				/* Number when stored in file */
	WORD	flags;				/* Used to indicate usage when storing */
	WORD	node;
	WORD	namesize;
	PADLONG(0,7,0);
} *SYMBOLS;

typedef struct InDeX {			/* Don't change unless altering .sav too */
	LONG	name;				/* Location in names buffer */
	WORD	type;				/* Regular or dummy */
	WORD	dimension;			/* Value of d_(n,n) or -number of symbol */
	WORD	number;				/* Number when stored in file */
	WORD	flags;				/* Used to indicate usage when storing */
	WORD    nmin4;				/* Used for n-4 if dimension < 0 */
	WORD	node;
	WORD	namesize;
	PADLONG(0,7,0);
} *INDICES;

typedef struct VeCtOr {			/* Don't change unless altering .sav too */
	LONG	name;				/* Location in names buffer */
	WORD	complex;			/* Properties under complex conjugation */
	WORD	number;				/* Number when stored in file */
	WORD	flags;				/* Used to indicate usage when storing */
	WORD	node;
	WORD	namesize;
	PADLONG(0,5,0);
} *VECTORS;

typedef struct FuNcTiOn {		/* Don't change unless altering .sav too */
	TABLES	tabl;				/* For if redefined as table */
	LONG	symminfo;			/* Info regarding symm properties offset in buffer */
	LONG	name;				/* Location in names buffer */
	WORD	commute;			/* commutation properties */
	WORD	complex;			/* Properties under complex conjugation */
	WORD	number;				/* Number when stored in file */
	WORD	flags;				/* Used to indicate usage when storing */
	WORD    spec;				/* Regular, Tensor, etc */
	WORD	symmetric;			/* > 0 if symmetric properties */
	WORD	node;
	WORD	namesize;
	PADPOINTER(2,0,8,0);
} *FUNCTIONS;

typedef struct SeTs {
	LONG	name;				/* Location in names buffer */
	WORD	type;				/* Symbol, vector, index or function */
	WORD	first;				/* First element in setstore */
	WORD	last;				/* Last element in setstore */
	WORD	node;
	WORD	namesize;
	PADLONG(0,5,0);
} *SETS;

typedef struct DuBiOuS {		/* Undeclared objects. Just for compiler. */
	LONG	name;				/* Location in names buffer */
	WORD	node;
	WORD	dummy;
	PADLONG(0,2,0);
} *DUBIOUSV;

typedef struct DoLlArS {
	WORD	*where;				/* A pointer(!) to the object */
	LONG	size;				/* The number of words */
	LONG	name;
	WORD	type;
	WORD	node;
	WORD	index;
	WORD	zero;
	PADPOINTER(2,0,4,0);
} *DOLLARS;

typedef struct MoDoPtDoLlArS {
	WORD	number;
	WORD	type;
} MODOPTDOLLAR;

typedef struct fixedset {
	char *name;
	char *description;
	int type;
} FIXEDSET;

typedef struct TaBlEbAsEsUbInDeX {
	POSITION where;
	LONG size;
} TABLEBASESUBINDEX;

typedef struct TaBlEbAsE {
	UBYTE *name;
	int *tablenumbers;		/* Number of each table */
	TABLEBASESUBINDEX *subindex;		/* For each table */
	POSITION fillpoint;
	POSITION current;
	int numtables;
} TABLEBASE;
 
/*
  	#] Variables : 
  	#[ Files :
*/
 
typedef struct FiLe {
    WORD *PObuffer;             /* Address of the intermediate buffer */
    WORD *POstop;               /* End of the buffer */
    WORD *POfill;               /* Fill position of the buffer */
    WORD *POfull;               /* Full buffer when only cached */
	char *name;					/* name of the file */
	POSITION *blockpos;			/* Positions of blocks in the file */
	POSITION *fPatches;			/* Positions of patches if sort file */
	POSITION POposition;    	/* File position */
    POSITION OldPosOut;         /* Temporary for stage4 */
    POSITION filesize;          /* Because SEEK_END is unsafe on IBM */
	ULONG numblocks;			/* Number of blocks in file */
	ULONG inbuffer;				/* Block in the buffer */
	ULONG blockpossize;			/* Size of blockpos buffer */
    LONG POsize;                /* size of the buffer */
    int handle;					/* Our own handle */
	int active;					/* File is open or closed */
	WORD fPatchN;				/* Number of patches on file */
	PADPOINTER(3,2,1,0);
} FILEHANDLE;
 
typedef struct StreaM {
	UBYTE *buffer;
	UBYTE *pointer;
	UBYTE *top;
	UBYTE *FoldName;
	UBYTE *name;
	UBYTE *pname;
	long buffersize;
	off_t fileposition;
	long bufferposition;
	long inbuffer;
	off_t linenumber;
	off_t prevline;
	int previous;
	int handle;
	int type;
	int prevars;
	int previousNoShowInput;
	int eqnum;
	int afterwards;
	int olddelay;
	UBYTE isnextchar;
	UBYTE nextchar[2];
	PADPOINTER(0,8,0,3);
} STREAM;

/*
  	#] Files : 
  	#[ Traces :
*/

typedef struct TrAcEs {			/* For computing 4 dimensional traces */
	WORD		*accu;		/* NUMBER * 2 */
	WORD		*accup;
	WORD		*termp;
	WORD		*perm;		/* number */
	WORD		*inlist;	/* number */
	WORD		*nt3;		/* number/2 */
	WORD		*nt4;		/* number/2 */
	WORD		*j3;		/* number*2 */
	WORD		*j4;		/* number*2 */
	WORD		*e3;		/* number*2 */
	WORD		*e4;		/* number */
	WORD		*eers;		/* number/2 */
	WORD		*mepf;		/* number/2 */
	WORD		*mdel;		/* number/2 */
	WORD		*pepf;		/* number*2 */
	WORD		*pdel;		/* number*3/2 */
	WORD		sgn;
	WORD		stap;
	WORD		step1,kstep,mdum;
	WORD		gamm,ad,a3,a4,lc3,lc4;
	WORD		sign1,sign2,gamma5,num,level,factor,allsign;
	WORD		finalstep;
	PADPOINTER(0,0,19,0);
} TRACES;

typedef struct TrAcEn {			/* For computing n dimensional traces */
	WORD		*accu;		/* NUMBER */
	WORD		*accup;
	WORD		*termp;
	WORD		*perm;		/* number */
	WORD		*inlist;	/* number */
	WORD		sgn,num,level,factor,allsign;
	WORD		allignment;
	PADPOINTER(0,0,6,0);
} *TRACEN;

/*
  	#] Traces : 
  	#[ Preprocessor :
*/
 
typedef struct pReVaR {
	UBYTE *name;
	UBYTE *value;
	UBYTE *argnames;	/* names of arguments, zero separated */
	int nargs;			/* 0 = regular, >= 1: number of macro arguments */
	int wildarg;		/* The number of a potential ?var. If none: 0. */
	PADPOINTER(0,2,0,0);
} PREVAR;

typedef struct {
	UBYTE	*buffer;
    LONG	size;
	PADPOINTER(1,0,0,0);
} PRELOAD;

typedef struct {
	PRELOAD p;
	UBYTE	*name;
	int		loadmode;
	PADPOINTER(0,1,0,0);
} PROCEDURE;

typedef struct DoLoOp {
	PRELOAD p;			/* size, name and buffer */
	UBYTE *name;
	UBYTE *vars;
	UBYTE *contents;
	LONG startlinenumber;
	LONG firstnum;
	LONG lastnum;
	LONG incnum;
	int type;
	int NoShowInput;
	int errorsinloop;
	int firstloopcall;
	PADPOINTER(4,4,0,0);
} DOLOOP;

/*
  	#] Preprocessor : 
  	#[ Varia :
*/

typedef struct CbUf {
	WORD *Buffer;
	WORD *Top;
	WORD *Pointer;
	WORD **lhs;
	WORD **rhs;
	LONG *CanCommu;
	LONG *NumTerms;
	COMPTREE *boomlijst;
	LONG BufferSize;
	int numlhs;
	int numrhs;
	int maxlhs;
	int maxrhs;
	int mnumlhs;
	int mnumrhs;
	int numtree;
	int rootnum;
	int MaxTreeSize;
	PADPOINTER(1,9,0,0);
} CBUF;

typedef struct ChAnNeL {
	char *name;
	int handle;
	PADPOINTER(0,1,0,0);
} CHANNEL;
 
typedef struct {
	UBYTE *parameter;
	int type;
	int flags;
	long value;
} SETUPPARAMETERS;

typedef struct NeStInG {
	WORD *termsize;
	WORD *funsize;
	WORD *argsize;
} *NESTING;

typedef struct StOrEcAcHe {
	struct StOrEcAcHe *next;
	POSITION position;
	POSITION toppos;
	WORD buffer[2];
	PADPOINTER(2,0,2,0);
} *STORECACHE;

typedef struct PeRmUtE {
	WORD *objects;
	WORD sign;
	WORD n;
	WORD cycle[MAXMATCH];
	PADPOINTER(0,0,MAXMATCH+2,0);
} PERM;

typedef struct DiStRiBuTe {
	WORD *obj1;
	WORD *obj2;
	WORD *out;
	WORD sign;
	WORD n1;
	WORD n2;
	WORD n;
	WORD cycle[MAXMATCH];
} DISTRIBUTE;
 
typedef struct specobj {
	char *name;
	WORD type;
	WORD num1;
	WORD num2;
	PADPOINTER(0,0,3,0);
} SPECOBJ;

typedef struct sOrT {
	WORD *lBuffer;				/* The large buffer */
	WORD *lTop;					/* End of the large buffer */
	WORD *lFill;				/* The filling point of the large buffer */
	WORD *used;					/*  auxiliary during actual sort */
	WORD *sBuffer;				/* The small buffer */
	WORD *sTop;					/* End of the small buffer */
	WORD *sTop2;				/* End of the extension of the small buffer */
	WORD *sHalf;				/* Halfway point in the extension */
	WORD *sFill;				/* Filling point in the small buffer */
	WORD **sPointer;			/* Pointers to terms in the small buffer */
	WORD **PoinFill;			/* Filling point for pointers to the sm.buf */
	WORD **SplitScratch;		/* Excess pointer space for the merge sort */
	WORD *cBuffer;				/* Compress buffer (if it exists) */
	WORD **Patches;				/* Positions of patches in large buffer */
	WORD **pStop;				/* Ends of patches in the large buffer */
	WORD **poina;				/*  auxiliary during actual sort */
	WORD **poin2a;				/*  auxiliary during actual sort */
	WORD *ktoi;					/*  auxiliary during actual sort */
	WORD *tree;					/*  auxiliary during actual sort */
	POSITION *fPatches;			/* Positions of output file patches */
	POSITION *inPatches;		/* Positions of input file patches */
	FILEHANDLE *f;				/* The actual output file */
	FILEHANDLE file;			/* The own sort file */
	FILEHANDLE **ff;			/* Handles for a staged sort */
	LONG sTerms;				/* Terms in small buffer */
	LONG LargeSize;				/* Size of large buffer (in words) */
	LONG SmallSize;				/* Size of small buffer (in words) */
	LONG SmallEsize;			/* Size of small + extension (in words) */
	LONG TermsInSmall;			/* Maximum number of terms in small buffer */
	LONG Terms2InSmall;			/* with extension for polyfuns etc. */
	LONG GenTerms;				/* Number of generated terms */
	LONG TermsLeft;				/* Number of terms still in existence */
	LONG GenSpace;				/* Amount of space of generated terms */
	LONG SpaceLeft;				/* Space needed for still existing terms */
	LONG putinsize;				/* Size of buffer in putin */
	LONG ninterms;				/* Which input term ? */
	POSITION SizeInFile[3];		/* Sizes in the various files */
	int MaxPatches;				/* Maximum number of patches in large buffer */
	int MaxFpatches;			/* Maximum number of patches in one filesort */
	int type;					/* Main, function or sub(routine) */
	int lPatch;					/* Number of patches in the large buffer */
	int fPatchN1;				/* Number of patches in input file */
	int PolyWise;				/* Is there a polyfun and if so, where? */
	int PolyFlag;				/*  */
	int cBufferSize;			/* Size of the compress buffer */
	int maxtermsize;			/* Keeps track for buffer allocations */
	int newmaxtermsize;			/* Auxiliary for maxtermsize */
	int outputmode;				/* Tells where the output is going */
	int stagelevel;				/* In case we have a 'staged' sort */
	WORD fPatchN;				/* Number of patches on file */
	WORD stage4;				/* Are we using stage4? */
	PADPOINTER(15,12,2,0);
} SORTING;

/*
  	#] Varia : 
  	#[ A :
*/

#ifdef DEBUGGER
typedef struct DeBuGgInG {
	int	eflag;
	int	printflag;
	int logfileflag;
	int	stdoutflag;
	PADPOINTER(0,4,0,0);
} DEBUGGING;
#endif

/*
	The M struct is for global settings at startup or .clear
*/
struct M_const {
    SORTING *S0;                   /* (M) The main sort buffer */
    WORD    *gcmod;                /* (M) Global setting of modulus. Pointer to value */
    WORD    *gpowmod;              /* (M) Global setting printing as powers. Pointer. */
    WORD    *WorkSpace;            /* (M) */
    WORD    *WorkTop;              /* (M) */
    UBYTE   *TempDir;              /* (M) Path with where the temporary files go */
    WORD    *CompressBuffer;       /* (M) */
    WORD    *ComprTop;             /* (M) */
    UBYTE *IncDir;                 /* (M) Directory path for include files */
    UBYTE *InputFileName;          /* (M) */
    UBYTE *LogFileName;            /* (M) */
    UBYTE *OutBuffer;              /* (M) Output buffer in pre.c */
    UBYTE *Path;                   /* (M) */
    UBYTE *SetupDir;               /* (M) Directory with setup file */
    UBYTE *SetupFile;              /* (M) Name of setup file */
	UBYTE *Zip1Buffer;             /* (M) First Zip compression buffer */
	UBYTE *Zip2Buffer;             /* (M) Second Zip compression buffer */
    int *RepTop;                   /* (M) Top of RepCount buffer */
    int *RepCount;                 /* (M) Buffer for repeat nesting */
    WORD    *n_coef;               /* (M) Used by normal. local. */
    WORD    *n_llnum;              /* (M) Used by normal. local. */
	POSITION zeropos;              /* (M) is zero */
    LONG    WorkSize;              /* (M) Size of WorkSpace */
    LONG    MaxTer;                /* (M) Maximum term size. Fixed at setup. */
    LONG    CompressSize;          /* (M) Size of Compress buffer */
    LONG    ScratSize;             /* (M) Size of Fscr[] buffers */
    LONG    SizeStoreCache;        /* (M) Size of the chaches for reading global expr. */
    LONG    MaxStreamSize;         /* (M) Maximum buffer size in reading streams */
    LONG    SIOsize;               /* (M) Sort InputOutput buffer size */
    LONG    SLargeSize;            /* (M) */
    LONG    SSmallEsize;           /* (M) */
    LONG    SSmallSize;            /* (M) */
    LONG    STermsInSmall;         /* (M) */
	LONG	MaxBracketBufferSize;  /* (M) Max Size for B+ or AB+ per expression */
	LONG	ZipBufferSize;         /* (M) Size of buffer for gzip compression */
    LONG    hSlavePatchSize;       /* (M) */
    LONG    gSlavePatchSize;       /* (M) */
    LONG    shmWinSize;            /* (M) size for shared memory window used in communications */
    int     FileOnlyFlag;          /* (M) Writing only to file */
    int     Interact;              /* (M) Interactive mode flag */
    int     MaxParLevel;           /* (M) Maximum nesting of parantheses */
    int     OutBufSize;            /* (M) size of OutBuffer */
    int     SMaxFpatches;          /* (M) */
    int     SMaxPatches;           /* (M) */
    int     StdOut;                /* (M) Regular output channel */
    int     ginsidefirst;          /* (M) Not used yet */
    int     gDefDim;               /* (M) */
    int     gDefDim4;              /* (M) */
    int     NumFixedSets;          /* (M) Number of the predefined sets */
    int     NumFixedFunctions;     /* (M) Number of built in functions */
    int     rbufnum;               /* (M) startup compiler buffer */
    int     dbufnum;               /* (M) dollar variables */
    int     SkipClears;            /* (M) Number of .clear to skip at start */
    int     gfunpowers;            /* (M) */
    int     gStatsFlag;            /* (M) */
    int     gNamesFlag;            /* (M) */
    int     gCodesFlag;            /* (M) */
    int     gSortType;             /* (M) */
    int     gproperorderflag;      /* (M) */
    int     hparallelflag;         /* (M) */
    int     gparallelflag;         /* (M) */
    WORD    MaxTal;                /* (M) Maximum number of words in a number */
    WORD    IndDum;                /* (M) Basis value for dummy indices */
    WORD    DumInd;                /* (M) */
    WORD    WilInd;                /* (M) Offset for wildcard indices */
    WORD    gncmod;                /* (M) Global setting of modulus. size of gcmod */
    WORD    gnpowmod;              /* (M) Global printing as powers. size gpowmod */
    WORD    gUnitTrace;            /* (M) Global value of Tr[1] */
    WORD    gOutputMode;           /* (M) */
    WORD    gOutputSpaces;         /* (M) */
    WORD    gOutNumberType;        /* (M) */
    WORD    gUniTrace[4];          /* (M) */
    WORD    MaxWildcards;          /* (M) Maximum number of wildcards */
    WORD    mTraceDum;             /* (M) Position/Offset for generated dummies */
    WORD    OffsetIndex;           /* (M) */
    WORD    OffsetVector;          /* (M) */
    WORD    RepMax;                /* (M) Max repeat levels */
    WORD    LogType;               /* (M) Type of writing to log file */
    WORD    ggStatsFlag;           /* (M) */
    WORD    gLineLength;           /* (M) */
    WORD    qError;                /* (M) Only error checking {-c option} */
    WORD    FortranCont;           /* (M) Fortran Continuation character */
    WORD    HoldFlag;              /* (M) Exit on termination? */
    WORD    Ordering[15];          /* (M) Auxiliary for ordering wildcards */
    WORD    silent;                /* (M) Silent flag. Only results in output. */
    WORD    tracebackflag;         /* (M) For tracing errors */
    WORD    expnum;                /* (M) internal number of ^ function */
    WORD    denomnum;              /* (M) internal number of / function */
    WORD    facnum;                /* (M) internal number of fac_ function */
    WORD    invfacnum;             /* (M) internal number of invfac_ function */
    WORD    sumnum;                /* (M) internal number of sum_ function */
    WORD    sumpnum;               /* (M) internal number of sump_ function */
    WORD    OldOrderFlag;          /* (M) Flag for allowing old statement order */
    WORD    termfunnum;            /* (M) internal number of term_ function */
    WORD    matchfunnum;           /* (M) internal number of match_ function */
    WORD    countfunnum;           /* (M) internal number of count_ function */
    WORD    polyfunnum;            /* (M) internal number of poly_ function */
    WORD    polytopnum;            /* (M) internal number of maximum poly function */
	WORD	gPolyFun;              /* (M) global value of PolyFun */
	WORD	safetyfirst;           /* (M) for testing special versions */
	WORD	dollarzero;            /* (M) for dollars with zero value */
	UBYTE	SaveFileHeader[SFHSIZE];/*(M) Header written to .str and .sav files */
};
/*
	The P struct defines objects set by the preprocessor
*/
struct P_const {
    LIST LoopList;                 /* (P) List of do loops */
    LIST ProcList;                 /* (P) List of procedures */
    LIST ChDollarList;             /* (P) List of Dollars changed by PP in module */
    UBYTE   **PreSwitchStrings;    /* (P) The string in a switch */
    UBYTE *preStart;               /* (P) Preprocessor instruction buffer */
    UBYTE *preStop;                /* (P) end of preStart */
    UBYTE *preFill;                /* (P) Filling point in preStart */
    int *PreIfStack;               /* (P) Tracks nesting of #if */
    int *PreSwitchModes;           /* (P) Stack of switch status */
    int *PreTypes;                 /* (P) stack of #call, #do etc nesting */
    DOLOOP      DoLoops[MAXLOOPS+1];/* (P) */
    PROCEDURE   FirstProc;         /* (P) */
    PROCEDURE   CurProc;           /* (P) */
    LONG    InOutBuf;              /* (P) Characters in the output buf in pre.c */
    LONG    pSize;                 /* (P) size of preStart */
    int     PreproFlag;            /* (P) Internal use to mark work on prepro instr. */
    int     iBufError;             /* (P) Flag for errors with input buffer */
    int     PreOut;                /* (P) Flag for #+ #- */
    int     PreSwitchLevel;        /* (P) Nesting of #switch */
    int     NumPreSwitchStrings;   /* (P) Size of PreSwitchStrings */
    int     MaxPreTypes;           /* (P) Size of PreTypes */
    int     NumPreTypes;           /* (P) Number of nesting objects in PreTypes */
    int     DelayPrevar;           /* (P) Delaying prevar substitution */
	int		AllowDelay;            /* (P) Allow delayed prevar substitution */
    WORD    DebugFlag;             /* (P) For debugging purposes */
    WORD    preError;              /* (?) used but not defined */
    UBYTE   ComChar;               /* (P) Commentary character */
    UBYTE   cComChar;              /* (P) Old commentary character for .clear */
};
/*[12dec2003 mt]:*/

/*Here it is assumed that there are exactly 8 bits in one 
byte and character occupies exactly one byte.*/

/*Structure used to store character sets. Introduced in such a 
way to be more portable:*/
struct  bit_field {
	UINT bit_0        : 1;
	UINT bit_1        : 1;
	UINT bit_2        : 1;
	UINT bit_3        : 1;
	UINT bit_4        : 1;
	UINT bit_5        : 1;
	UINT bit_6        : 1;
	UINT bit_7        : 1;
};

typedef struct bit_field set_of_char[32];
typedef struct bit_field *one_byte;
/*:[12dec2003 mt]*/

/*
	The C struct defines objects changed by the compiler
*/
struct C_const {
	/*[12dec2003 mt]:*/
	/*separators used to delimit arguments in #call and #do:*/
	set_of_char separators;	
	/*:[12dec2003 mt]*/
	/*[06nov2003 mt]:*/
#ifdef PARALLEL
	WORD NumberOfRhsExprInModule;  /* (C) Number of RHS expressions*/
	/*[28nov2003 mt]:*/
	WORD NumberOfRedefsInModule;  /* (C) Number of redefined variables in the module*/
	/*:[28nov2003 mt]*/
#endif
	/*:[06nov2003 mt]*/
	/*[19nov2003 mt]:*/
	/*Note, here is better to use explicit c-type 'unsigned long' instead e.g. ULONG
	since this field will be converted by sprintf, see PreProcessor in pre.c*/
	unsigned long CModule;  /* (C) Counter of current module
									 incremented in IniModule (pre.c)*/
	/*:[19nov2003 mt]*/
    NAMETREE *dollarnames;         /* (C) Names of dollar variables */
    NAMETREE *exprnames;           /* (C) Names of expressions */
    NAMETREE *varnames;            /* (C) Names of regular variables */
    LIST ChannelList;              /* (C) Used for the #write. */
                                   /*     Later also for write? */
    LIST DubiousList;              /* (C) List of dubious variables
                                       If not empty -> no execution */
    LIST FunctionList;             /* (C) List of functions and properties */
    LIST ExpressionList;           /* (C) List of expressions, locations etc. */
    LIST IndexList;                /* (C) List of indices */
    LIST SetElementList;           /* (C) List of all elements of all sets */
    LIST SetList;                  /* (C) List of the sets */
    LIST SortList;                 /* (C) List of sort buffers */
    LIST SymbolList;               /* (C) List of the symbols and their properties */
    LIST VectorList;               /* (C) List of the vectors */
    LIST cbufList;                 /* (C) List of compiler buffers */
    LIST PotModDolList;            /* (C) Potentially changed dollars */
	LIST ModOptDolList;            /* (C) Module Option Dollars list */
	LIST TableBaseList;            /* (C) TableBase list */
/*
    Objects for auto declarations
*/
    LIST AutoSymbolList;           /* (C) */
    LIST AutoIndexList;            /* (C) */
    LIST AutoVectorList;           /* (C) */
    LIST AutoFunctionList;         /* (C) */
    NAMETREE *autonames;           /* (C) Names in autodeclare */

    LIST *Symbols;                 /* (C) Pointer for autodeclare. Which list is
                                       it searching. Later also for subroutines */
    LIST *Indices;                 /* (C) id. */
    LIST *Vectors;                 /* (C) id. */
    LIST *Functions;               /* (C) id. */
	LIST *TableBases;
    NAMETREE **activenames;        /* (C) id. */
    STREAM  *Streams;              /* (C) The input streams. */
    STREAM  *CurrentStream;        /* (C) The current input stream.
                                       Streams are: do loop, file, prevariable */
	TABLES	usedtables;            /* (C) For checking the use of tables */
    LONG    *termstack;            /* (C) Last term statement {offset} */
    LONG    *termsortstack;        /* (C) Last sort statement {offset} */
    WORD    *cmod;                 /* (C) Local setting of modulus. Pointer to value */
    WORD    *powmod;               /* (C) Local setting printing as powers. Pointer. */
    UWORD   *modpowers;            /* (C) The conversion table for mod-> powers. */
    WORD    *ProtoType;            /* (C) The subexpression prototype {wildcards} */
    WORD    *WildC;                /* (C) Filling point for wildcards. */
    LONG    *IfHeap;               /* (C) Keeps track of where to go in if */
    LONG    *IfCount;              /* (C) Keeps track of where to go in if */
    LONG    *IfStack;              /* (C) Keeps track of where to go in if */
    UBYTE   *iBuffer;              /* (C) Compiler input buffer */
    UBYTE   *iPointer;             /* (C) Running pointer in the compiler input buffer */
    UBYTE   **LabelNames;          /* (C) List of names in label statements */
    WORD    *FixIndices;           /* (C) Buffer of fixed indices */
    WORD    *ScratchBuf[3];        /* (C) The scratch buffers. Change for hide. */
    WORD    *termsumcheck;         /* (C) checking of nesting */
    UBYTE *iStop;                  /* (C) Top of iBuffer. */
    UBYTE *WildcardNames;          /* (C) Names of ?a variables */
    int *Labels;                   /* (C) Label information for during run. */
    SBYTE *tokens;                 /* (C) Array with tokens for tokenizer */
    SBYTE *toptokens;              /* (C) Top of tokens */
    SBYTE *endoftokens;            /* (C) end of the actual tokens */
    WORD *tokenarglevel;           /* (C) keeps track of function arguments */
	WORD	*tableuse;             /* (C) list of tables of which the use should be checked */
    LONG    argstack[MAXNEST];     /* (C) {contents} Stack for nesting of Argument */
    LONG    insidestack[MAXNEST];  /* (C) {contents} Stack for Argument or Inside. */
    LONG    inexprstack[MAXNEST];  /* (C) {contents} Stack for Argument or Inside. */
    LONG    iBufferSize;           /* (C) Size of the input buffer */
    LONG    TransEname;            /* (C) Used when a new definition overwrites
                                       an old expression. */
    LONG    SlavePatchSize;        /* (C) */
    LONG    mSlavePatchSize;       /* (C) */
    int     MaxPreIfLevel;         /* (C) Maximum number of nested #if. Dynamic */
    int     NoShowInput;           /* (C) No listing of input as in .prc, #do */
    int     PreDebug;              /* (C) */
    int     PreIfLevel;            /* (C) */
    int     ShortStats;            /* (C) */
    int     compiletype;           /* (C) type of statement {DECLARATION etc} */
    int     firstconstindex;       /* (C) flag for giving first error message */
    int     insidefirst;           /* (C) Not used yet */
    int     minsidefirst;          /* (?) Not used yet */
    int     wildflag;              /* (C) Flag for marking use of wildcards */
    int     NumLabels;             /* (C) Number of labels {in Labels} */
    int     MaxLabels;             /* (C) Size of Labels array */
    int     lDefDim;               /* (C) */
    int     lDefDim4;              /* (C) */
    int     NumWildcardNames;      /* (C) Number of ?a variables */
    int     WildcardBufferSize;    /* (C) size of WildcardNames buffer */
    int     MaxIf;                 /* (C) size of IfHeap, IfSumCheck, IfCount */
    int     NumStreams;            /* (C) */
    int     MaxNumStreams;         /* (C) */
    int     firstctypemessage;     /* (C) Flag for giving first st order error */
    int     tablecheck;            /* (C) For table chacking */
    int     idoption;              /* (C) */
    int     BottomLevel;           /* (C) For propercount. Not used!!! */
    int     CompileLevel;          /* (C) Subexpression level */
    int     TokensWriteFlag;       /* (C) */
    int     PreAssignFlag;         /* (C) Indicates #assign -> catch dollar */
    int     PreContinuation;       /* (C) Indicates whether the statement is new */
    int     AutoDeclareFlag;       /* (C) Mode of looking for names */
    int     UnsureDollarMode;      /* (C)?Controls error messages undefined $'s */
    int     outsidefun;            /* (C) Used for writing Tables to file */
    int     funpowers;             /* (C) */
    int     WarnFlag;              /* (C) */
    int     StatsFlag;             /* (C) */
    int     NamesFlag;             /* (C) */
    int     CodesFlag;             /* (C) */
    int     SetupFlag;             /* (C) */
    int     SortType;              /* (C) */
    int     lSortType;             /* (C) */
    int     BracketNormalize;      /* (C) Indicates whether the bracket st is normalized */
    int     maxtermlevel;          /* (C) Size of termstack */
    int     dumnumflag;            /* (C) Where there dummy indices in tokenizer? */
    int     bracketindexflag;      /* (C) Are brackets going to be indexed? */
    int     parallelflag;          /* (C) parallel allowed? */
    int     mparallelflag;         /* (C) parallel allowed in this module? */
	int		properorderflag;       /* (C) clean normalizing. */
	int     vetofilling;           /* (C) vetoes overwriting in tablebase stubs */
    int     tablefilling;          /* (C) to notify AddRHS we are filling a table */
    int     vetotablebasefill;     /* (C) For the load in tablebase */
    WORD    RepLevel;              /* (C) Tracks nesting of repeat. */
    WORD    arglevel;              /* (C) level of nested argument statements */
    WORD    insidelevel;           /* (C) level of nested inside statements */
    WORD    inexprlevel;           /* (C) level of nested inexpr statements */
    WORD    termlevel;             /* (C) level of nested term statements */
    WORD    argsumcheck[MAXNEST];  /* (C) Checking of nesting */
    WORD    insidesumcheck[MAXNEST];/* (C) Checking of nesting */
    WORD    inexprsumcheck[MAXNEST];/* (C) Checking of nesting */
    WORD    MustTestTable;         /* (C) Indicates whether tables have been changed */
    WORD    Eside;                 /* (C) Tells which side of = sign */
    WORD    DumNum;                /* (C) */
    WORD    ncmod;                 /* (C) Local setting of modulus. size of cmod */
    WORD    npowmod;               /* (C) Local printing as powers. size powmod */
    WORD    DirtPow;               /* (C) Flag for changes in printing mod powers */
    WORD    lUnitTrace;            /* (C) Local value of Tr[1] */
    WORD    NwildC;                /* (C) Wildcard counter */
    WORD    ComDefer;              /* (C) defer brackets */
    WORD    CollectFun;            /* (C) Collect function number */
    WORD    AltCollectFun;         /* (C) Alternate Collect function number */
    WORD    OutputMode;            /* (C) */
    WORD    OutputSpaces;          /* (C) */
    WORD    OutNumberType;         /* (C) Controls RATIONAL/FLOAT output */
    WORD    lUniTrace[4];          /* (C) */
    WORD    RepSumCheck[MAXREPEAT];/* (C) Checks nesting of repeat, if, argument */
    WORD    DidClean;              /* (C) Test whether nametree needs cleaning */
    WORD    IfLevel;               /* (C) */
    WORD    WhileLevel;            /* (C) */
    WORD    *IfSumCheck;           /* (C) Keeps track of where to go in if */
    WORD    LogHandle;             /* (C) The Log File */
    WORD    LineLength;            /* (C) */
    WORD    StoreHandle;           /* (C) Handle of .str file */
    WORD    HideLevel;             /* (C) Hiding indicator */
    WORD    PolyFun;               /* (C) Number of the PolyFun function */
	WORD	lPolyFun;              /* (C) local value of PolyFun */
    WORD    SymChangeFlag;         /* (C) */
	WORD	inusedtables;          /* (C) Number of elements in usedtables */
/* --------change 17-feb-2003  */
    WORD    CollectPercentage;     /* (C) Collect function percentage */
    UBYTE   Commercial[COMMERCIALSIZE+2]; /* (C) Message to be printed in statistics */
};
/*
	The S struct defines objects changed at the start of the run (Processor)
*/
struct S_const {
    FILEHANDLE *hidefile;          /* (S) Points to Fscr[2] */
    POSITION *OldOnFile;           /* (S) File positions of expressions */
    int     NumOldOnFile;          /* (S) Number of expressions in OldOnFile */
    WORD    CurExpr;               /* (S) Number of current expression */
    WORD    ExecMode;              /* (S) */
};
/*
	The R struct defines objects changed at run time
*/
struct R_const {
    LIST DollarList;               /* (R) Dollar variables. Contains pointers
                                       to contents of the variables.*/
    LIST PreVarList;               /* (R) List of preprocessor variables
                                       Points to contents. Can be changed */
    FILEHANDLE *infile;            /* (R) Points alternatingly to Fscr[0] or Fscr[1] */
    FILEHANDLE *outfile;           /* (R) Points alternatingly to Fscr[1] or Fscr[0] */
    SORTING *SS;                   /* (R) Current sort buffer */

    NESTING     Nest;              /* (R) Nesting of function levels etc. */
    NESTING     NestStop;          /* (R) */
    NESTING     NestPoin;          /* (R) */

    WORD    *EndNest;              /* (R) Nesting of function levels etc. */
    WORD    *TMaddr;               /* (R) buffer for TestSub */
    WORD    *Frozen;               /* (R) Bracket info */
    WORD    *FullProto;            /* (R) Prototype of a subexpression or table */
    WORD    **pWorkSpace;          /* (R) Workspace for pointers. Dynamic. */
    LONG    *lWorkSpace;           /* (R) WorkSpace for LONG. Dynamic. */
	POSITION *posWorkSpace;        /* (R) WorkSpace for file positions */
    WORD    *BrackBuf;             /* (R) Bracket buffer. Used by poly_ at runtime. */
    WORD    *CompressPointer;      /* (R) */
    WORD    *WorkPointer;          /* (R) Pointer in the WorkSpace heap. */
    WORD    *cTerm;                /* (R) Current term for coef_ and term_ */
    int *RepPoint;                 /* (R) Pointer in RepCount buffer. Tracks repeat */
    STORECACHE  StoreCache;        /* (R) Cache for picking up stored expr. */
    FILEHANDLE  Fscr[3];           /* (R) Dollars etc play with it too */
    FILEHANDLE  FoStage4[2];       /* (R) In Sort. Stage 4. */
    LONG    pWorkPointer;          /* (R) Offset-pointer in pWorkSpace */
    LONG    pWorkSize;             /* (R) Size of pWorkSpace */
    LONG    lWorkPointer;          /* (R) Offset-pointer in lWorkSpace */
    LONG    lWorkSize;             /* (R) Size of lWorkSpace */
    LONG    posWorkPointer;        /* (R) Offset-pointer in posWorkSpace */
    LONG    posWorkSize;           /* (R) Size of posWorkSpace */
    POSITION DefPosition;          /* (R) Deferred position of keep brackets. */
    POSITION OldPosIn;             /* (R) Used in sort. */
    POSITION OldPosOut;            /* (R) Used in sort */
    LONG    InInBuf;               /* (R) Characters in input buffer. Scratch files. */
    LONG    OldTime;               /* (R) Zero time. Needed in timer. */

    int     cbufnum;               /* (R) current compiler buffer */
    int     ebufnum;               /* (R) extra compiler buffer */
    int     NoCompress;            /* (R) Controls compression */
    int     ErrorInDollar;         /* (R) */
    int     lhdollarflag;          /* (R) left hand dollar present */
    int     lhdollarerror;         /* (R) */
#ifdef DEBUGVERSION
/*
        The following variables are for the debugger
*/
    int     ErrorCondition;
#endif
    WORD    CurDum;                /* (R) Current maximum dummy number */
    WORD    BracketOn;             /* (R) Intensly used in poly_ */
    WORD    TeInFun;               /* (R) Passing type of action */
    WORD    TeSuOut;               /* (R) Passing info. Local? */
    WORD    TMout[40];             /* (R) Passing info */
    WORD    DeferFlag;             /* (R) For defered brackets */
    WORD    TePos;                 /* (R) */
    WORD    RecFlag;               /* (R) */
    WORD    MaxBracket;            /* (R) Size of BrackBuf. Changed by poly_ */
    WORD    sLevel;                /* (R) Sorting level */
    WORD    stage4;                /* (R) Sorting only. */
    WORD    Stage4Name;            /* (R) Sorting only */
    WORD    KeptInHold;            /* (R) */
    WORD    PolyAct;               /* (R) Used for putting the PolyFun at end */
    WORD    WildArgs;              /* (R) */
    WORD    WildEat;               /* (R) */
    WORD    GetFile;               /* (R) Where to get the terms {like Hide} */
    WORD    GetOneFile;            /* (R) Getting from hide or regular */
    WORD    expchanged;            /* (R) Info about expression */
    WORD    expflags;              /* (R) Info about expression */
    WORD    PolyNorm;              /* (R) For polynomial arithmetic */
    WORD    exitflag;              /* (R) For the exit statement */
    WORD    TMbuff;                /* (R) Communication between TestSub and Genera */
	WORD	CollectOverFlag;       /* (R) Indicates overflow at Collect */
};
/*
	The T struct concerns variables changed during running, but kept
	locally. Hence they may not need sending.
*/
struct T_const {
    UWORD   *factorials;           /* (T) buffer of factorials. Dynamic. */
    UWORD   *bernoullis;           /* (T) The buffer with bernoulli numbers. Dynamic. */
    long    *pfac;                 /* (T) array of positions of factorials. Dynamic. */
    long    *pBer;                 /* (T) array of positions of Bernoulli's. Dynamic. */
    LONG    sBer;                  /* (T) Size of the bernoullis buffer */
    int     sfact;                 /* (T) size of the factorials buffer */
    int     mfac;                  /* (T) size of the pfac array. */
    WORD    nfac;                  /* (T) Number of highest stored factorial */
    WORD    nBer;                  /* (T) Number of highest bernoulli number. */
    WORD    mBer;                  /* (T) Size of buffer pBer. */
};
/*
	The N struct contains variables used in running information that is
	inside blocks that should not be split, like pattern matching, traces etc.
*/
struct N_const {
    WORD    *WildValue;            /* (N) Wildcard info during pattern matching */
    WORD    *WildStop;             /* (N) Wildcard info during pattern matching */
    WORD    *WildMask;             /* (N) Wildcard info during pattern matching */
    WORD    *argaddress;           /* (N) Used in pattern matching of arguments */
    WORD    *RepFunList;           /* (N) For pattern matching */
    WORD    *patstop;              /* (N) Used in pattern matching */
    WORD    *terstop;              /* (N) Used in pattern matching */
    WORD    *terstart;             /* (N) Used in pattern matching */
    WORD    *DumFound;             /* (N) For renumbering indices {make local?} */
    WORD    **DumPlace;            /* (N) For renumbering indices {make local?} */
    WORD    **DumFunPlace;         /* (N) For renumbering indices {make local?} */
    WORD    *UsedSymbol;           /* (N) When storing terms of a global expr. */
    WORD    *UsedVector;           /* (N) When storing terms of a global expr. */
    WORD    *UsedIndex;            /* (N) When storing terms of a global expr. */
    WORD    *UsedFunction;         /* (N) When storing terms of a global expr. */
    WORD    *MaskPointer;          /* (N) For wildcard pattern matching */
    WORD    *WildArgTaken;         /* (N) Stack for wildcard pattern matching */
    WORD    *ForFindOnly;          /* (N) For wildcard pattern matching */
    WORD    *findTerm;             /* (N) For wildcard pattern matching */
    WORD    *findPattern;          /* (N) For wildcard pattern matching */
    int     NumTotWildArgs;        /* (N) Used in pattern matching */
    int     UseFindOnly;           /* (N) Controls pattern routines */
    int     UsedOtherFind;         /* (N) Controls pattern routines */
    WORD    oldtype;               /* (N) WildCard info at pattern matching */
    WORD    oldvalue;              /* (N) WildCard info at pattern matching */
    WORD    NumWild;               /* (N) Used in Wildcard */
    WORD    MinVecArg[7+ARGHEAD];  /* (N) but should be more local */
    WORD    FunArg[4+ARGHEAD+FUNHEAD]; /* (N) but can be more local */
    WORD    RepFunNum;             /* (N) Used in pattern matching */
    WORD    DisOrderFlag;          /* (N) Disorder option? Used in pattern matching */
    WORD    WildDirt;              /* (N) dirty in wldcard substitution. */
    WORD    NumFound;              /* (N) in reshuf only. Local? */
    WORD    WildReserve;           /* (N) Used in the wildcards */
};

/*
	The O struct concerns output variables
*/
struct O_const {
    UBYTE   *OutputLine;           /* (O) Sits also in debug statements */
    UBYTE   *OutStop;              /* (O) Top of OutputLine buffer */
    UBYTE   *OutFill;              /* (O) Filling point in OutputLine buffer */
    WORD    *bracket;              /* (O) For writing brackets */
    WORD    *termbuf;              /* (O) For writing terms */
    UBYTE   *wpos;                 /* (O) Only when storing file {local?} */
    UBYTE   *wpoin;                /* (O) Only when storing file {local?} */
    UBYTE *DollarOutBuffer;        /* (O) Outputbuffer for Dollars */
    UBYTE *CurBufWrt;              /* (O) Name of currently written expr. */
    FILEDATA    SaveData;          /* (O) */
    FILEDATA    StoreData;         /* (O) */
#ifdef mBSD
#ifdef MICROTIME
    long    wrap;                  /* (O) For statistics time. wrap around */
    long    wrapnum;               /* (O) For statistics time. wrap around */
#endif
#endif
#ifdef WINDOWS
    long    wrap;                  /* (O) For statistics time. wrap around */
    long    wrapnum;               /* (O) For statistics time. wrap around */
#endif
    LONG    NumInBrack;            /* (O) For typing [] option in print */
    LONG    wlen;                  /* (O) Used to store files. */
    LONG    DollarOutSizeBuffer;   /* (O) Size of DollarOutBuffer */
    LONG    DollarInOutBuffer;     /* (O) Characters in DollarOutBuffer */
    int     OutInBuffer;           /* (O) Which routine does the writing */
    WORD    OutSkip;               /* (O) How many chars to skip in output line */
    WORD    IsBracket;             /* (O) Controls brackets */
    WORD    InFbrack;              /* (O) For writing only */
    WORD    PrintType;             /* (O) */
    WORD    FortFirst;             /* (O) Only in sch.c */
    WORD    DoubleFlag;            /* (O) Output in double precision */
    UBYTE   FortDotChar;           /* (O) */
};

typedef struct AllGlobals {
	struct M_const M;
	struct P_const P;
	struct C_const C;
	struct S_const S;
	struct R_const R;
	struct T_const T;
	struct N_const N;
	struct O_const O;
} ALLGLOBALS;

/*
  	#] A :
  	#[ FG :
*/

typedef struct FixedGlobals {
	WCN		Operation[8];
	WCN2	OperaFind[6];
	char	*VarType[10];
	char	*ExprStat[17];
	char	*FunNam[2];
	char	*swmes[3];
	char	*fname;
	UBYTE	*s_one;
	UINT	cTable[256];
} FIXEDGLOBALS;

/*
  	#] FG : 
*/

#endif
