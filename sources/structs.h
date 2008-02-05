#ifndef __STRUCTS__

#define __STRUCTS__

/** @file structs.h
 *
 *  Contains definitions for global structs.
 */
 
/*
  	#[ sav&store :
*/

typedef struct PoSiTiOn {
	off_t p1;
} POSITION;
 
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

/**
 *  Defines the structure of the file header for store-files and save-files.
 *
 *  The first 8 bytes serve as a unique mark to identity save-files that
 *  contain such a header. Older versions of FORM don't have this header and
 *  will write the POSITION of the next file index (struct FiLeInDeX) here,
 *  which is always different from this pattern.
 *
 *  It is always 512 bytes long.
 */
typedef struct {
	UBYTE headermark[8];  /**< Pattern for header identification. Old versions
	                           of FORM have a maximum sizeof(POSITION) of 8 */
	UBYTE lenWORD;        /**< Number of bytes for WORD */
	UBYTE lenLONG;        /**< Number of bytes for LONG */
	UBYTE lenPOS;         /**< Number of bytes for POSITION */
	UBYTE lenPOINTER;     /**< Number of bytes for void * */
	UBYTE endianness[16]; /**< Used to determine endianness, sizeof(int) should be <= 16 */
	UBYTE sSym;           /**< sizeof(struct SyMbOl)   */
	UBYTE sInd;           /**< sizeof(struct InDeX)    */
	UBYTE sVec;           /**< sizeof(struct VeCtOr)   */
	UBYTE sFun;           /**< sizeof(struct FuNcTiOn) */
	UBYTE maxpower[16];   /**< Maximum power, see #MAXPOWER */
	UBYTE wildoffset[16]; /**< #WILDOFFSET macro         */
	UBYTE revision;       /**< Revision number of save-file system  */
	UBYTE reserved[512-8-4-16-4-16-16-1]; /**< Padding to 512 bytes */
} STOREHEADER;

/**
 *  Defines the structure of an entry in a file index (see struct FiLeInDeX).
 *  
 *  It represents one expression in the file.
 *
 *  It is always 512 bytes long.
 */
typedef struct InDeXeNtRy {
	POSITION	position;		/**< Position of the expression itself */
	POSITION	length;			/**< Length of the expression itself */
	POSITION	variables;		/**< Position of the list with variables */
	LONG	CompressSize;		/**< Size of buffer before compress */
	WORD	nsymbols;			/**< Number of symbols in the list */
	WORD	nindices;			/**< Number of indices in the list */
	WORD	nvectors;			/**< Number of vectors in the list */
	WORD	nfunctions;			/**< Number of functions in the list */
	WORD    size;				/**< Size of variables field */
	SBYTE	name[MAXENAME+1];	/**< Name of expression */
	PADLONG(1,5,MAXENAME+1);
} INDEXENTRY;

/**
 *  Maximum number of entries (struct InDeXeNtRy) in a file index (struct
 *  FiLeInDeX). Number is calculated such that the size of a file index is no
 *  more than 512 bytes.
 */
#define INFILEINDEX ((512-sizeof(LONG)-sizeof(POSITION))/sizeof(INDEXENTRY))
/**
 *  Number of empty filling bytes for a file index (struct FiLeInDeX). It is
 *  calculated such that the size of a file index is always 512 bytes.
 */
#define EMPTYININDEX (512-sizeof(LONG)-sizeof(POSITION)-INFILEINDEX*sizeof(INDEXENTRY))

/**
 *  Defines the structure of a file index in store-files and save-files.
 *  
 *  It contains several entries (see struct InDeXeNtRy) up to a maximum of
 *  #INFILEINDEX.
 *
 *  It is always 512 bytes long.
 */
typedef struct FiLeInDeX {
	POSITION	next;			/**< Position of next FILEINDEX if any */
	LONG	number;				/**< Number of used entries in this index */
	INDEXENTRY expression[INFILEINDEX]; /**< File index entries */
	SBYTE	empty[EMPTYININDEX];		/**< Padding to 512 bytes */
} FILEINDEX;

typedef struct FiLeDaTa {
	FILEINDEX Index;
	POSITION Fill;
	POSITION Position;
	WORD Handle;
	WORD dirtyflag;
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
#ifdef WITHPTHREADS
    WORD    **prototype;     /* The wildcard prototyping for arguments */
    WORD    **pattern;       /* The pattern with which to match the arguments */
#else
    WORD    *prototype;     /* The wildcard prototyping for arguments */
    WORD    *pattern;       /* The pattern with which to match the arguments */
#endif
	MINMAX	*mm;		    /* Array bounds, dimension by dimension */
	WORD	*flags;         /* Is element in use ? etc */
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
	WORD	numdummies;		/*  */
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
	WORD	compression;
	WORD	numdummies;
#ifdef WITHPTHREADS
	WORD	partodo;		/* Whether to be done in parallel mode */
	PADPOINTER(2,0,11,0);
#else
	PADPOINTER(2,0,10,0);
#endif
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
	PADLONG(1,5,0);
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
#ifdef WITHPTHREADS
	pthread_mutex_t	pthreadslockread;
	pthread_mutex_t	pthreadslockwrite;
#endif
	WORD	type;
	WORD	node;
	WORD	index;
	WORD	zero;
	WORD	numdummies;
	WORD	reserved;
	PADPOINTER(2,0,6,0);
} *DOLLARS;

typedef struct MoDoPtDoLlArS {
	WORD	number;
	WORD	type;
#ifdef WITHPTHREADS
	DOLLARS	dstruct;	/* If local dollar: list of DOLLARS for each thread */
#endif
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
 
typedef struct {
	WORD *location;
	int numargs;
	int numfunnies;
	int numwildcards;
	int symmet;
	int tensor;
	int commute;
} FUN_INFO;
 
/*
  	#] Variables : 
  	#[ Files :
*/
 
typedef struct FiLe {
    WORD *PObuffer;             /* Address of the intermediate buffer */
    WORD *POstop;               /* End of the buffer */
    WORD *POfill;               /* Fill position of the buffer */
    WORD *POfull;               /* Full buffer when only cached */
#ifdef WITHPTHREADS
    WORD *wPObuffer;             /* Address of the intermediate worker buffer */
    WORD *wPOstop;               /* End of the worker buffer */
    WORD *wPOfill;               /* Fill position of the worker buffer */
    WORD *wPOfull;               /* Full buffer when only worker cached */
#endif
	char *name;					/* name of the file */
#ifdef WITHZLIB
	z_streamp zsp;				/* The pointer to the stream struct for gzip */
	Bytef *ziobuffer;			/* The output buffer for compression */
#endif
	POSITION *fPatches;			/* Positions of patches if sort file */
	POSITION POposition;    	/* File position */
    POSITION filesize;          /* Because SEEK_END is unsafe on IBM */
	ULONG numblocks;			/* Number of blocks in file */
	ULONG inbuffer;				/* Block in the buffer */
    LONG POsize;                /* size of the buffer */
#ifdef WITHZLIB
    LONG ziosize;               /* size of the zoutbuffer */
#endif
#ifdef WITHPTHREADS
    LONG wPOsize;                /* size of the worker buffer */
	pthread_mutex_t	pthreadslock;
#endif
    int handle;					/* Our own handle */
	int active;					/* File is open or closed. Not used. */
	WORD fPatchN;				/* Number of patches on file */
#ifdef WITHZLIB
	PADPOINTER(4,2,1,0);
#else
	PADPOINTER(3,2,1,0);
#endif
} FILEHANDLE;
 
typedef struct StreaM {
	UBYTE *buffer;
	UBYTE *pointer;
	UBYTE *top;
	UBYTE *FoldName;
	UBYTE *name;
	UBYTE *pname;
	off_t fileposition;
	off_t linenumber;
	off_t prevline;
	long buffersize;
	long bufferposition;
	long inbuffer;
	int previous;
	int handle;
	int type;
	int prevars;
	int previousNoShowInput;
	int eqnum;
	int afterwards;
	int olddelay;
	int oldnoshowinput;
	UBYTE isnextchar;
	UBYTE nextchar[2];
	UBYTE reserved;
	PADPOINTER(3,8,0,4);
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
	UBYTE *vars;        /* for {} or name of expression */
	UBYTE *contents;
	UBYTE *dollarname;  /* For loop over terms in expression */
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

struct  bit_field {	/* Assume 8 bits per byte */
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

typedef struct{
	WORD newlogonly;
	WORD newhandle;
	WORD oldhandle;
	WORD oldlogonly;
	WORD oldprinttype;
	WORD oldsilent;
}HANDLERS;

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
	WORD *numdum;
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
#ifdef WITHZLIB
	WORD *fpcompressed;			/*  is output filepatch compressed? */
	WORD *fpincompressed;		/*  is input filepatch compressed? */
	z_streamp zsparray;			/*  the array of zstreams for decompression */
#endif
	POSITION *fPatches;			/* Positions of output file patches */
	POSITION *inPatches;		/* Positions of input file patches */
	POSITION *fPatchesStop;		/* Positions of output file patches */
	POSITION *iPatches;			/* Input file patches, Points to fPatches or inPatches */
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
	WORD fPatchN;				/* Number of patches on file (output) */
	WORD inNum;					/* Number of patches on file (input) */
	WORD stage4;				/* Are we using stage4? */
	PADPOINTER(15,12,3,0);
} SORTING;

#ifdef WITHPTHREADS
typedef struct SoRtBlOcK {
    pthread_mutex_t *MasterBlockLock;
    WORD    **MasterStart;
    WORD    **MasterFill;
    WORD    **MasterStop;
    int     MasterNumBlocks;
    int     MasterBlock;
    int     FillBlock;
} SORTBLOCK;
#endif

#ifdef DEBUGGER
typedef struct DeBuGgInG {
    int eflag;
    int printflag;
    int logfileflag;
    int stdoutflag;
    PADPOINTER(0,4,0,0);
} DEBUGSTR;
#endif

#ifdef WITHPTHREADS

typedef struct ThReAdBuCkEt {
	POSITION *deferbuffer;      /* For Keep Brackets: remember position */
	WORD *threadbuffer;         /* Here are the (primary) terms */
	WORD *compressbuffer;       /* For keep brackets we need the compressbuffer */
	LONG threadbuffersize;      /* Number of words in threadbuffer */
	LONG ddterms;               /* Number of primary+secondary terms represented */
	LONG firstterm;				/* The number of the first term in the bucket */
	pthread_mutex_t lock;       /* For the load balancing phase */
	int  free;                  /* Status of the bucket */
	int  totnum;                /* Total number of primary terms */
	int  usenum;                /* Which is the term being used at the moment */
	int  busy;                  /*  */
} THREADBUCKET;

#endif

typedef struct {
	WORD	*coefs;				/* The array of coefficients */
	WORD	numsym;				/* The number of the symbol in the polynomial */
	WORD	arraysize;			/* The size of the allocation of coefs */
	WORD	polysize;			/* The maximum power in the polynomial */
	WORD	modnum;				/* The prime number of the modulus */
} POLYMOD;

typedef struct {
	POLYMOD	*ppoly;				/* An array of polynomials */
	WORD	*ncoefs;			/* How many powers of the prime in each coef */
	WORD	numsym;				/* The number of the symbol in the polynomial */
	WORD	padicsize;			/* Elements allocated in ppoly; */
	WORD	numpow;				/* Maximum power of modnum actually used */
	WORD	arraysize;			/* The size of the allocation of coefs */
	WORD	polysize;			/* The maximum power in the polynomial */
	WORD	modnum;				/* The prime number of the modulus */
} POLYPADIC;

/*
  	#] Varia :
    #[ A :
 		#[ M : The M struct is for global settings at startup or .clear
*/
struct M_const {
    SORTING *S0;                   /* (M) The main sort buffer */
    WORD    *gcmod;                /* (M) Global setting of modulus. Pointer to value */
    WORD    *gpowmod;              /* (M) Global setting printing as powers. Pointer. */
    UBYTE   *TempDir;              /* (M) Path with where the temporary files go */
    UBYTE *IncDir;                 /* (M) Directory path for include files */
    UBYTE *InputFileName;          /* (M) */
    UBYTE *LogFileName;            /* (M) */
    UBYTE *OutBuffer;              /* (M) Output buffer in pre.c */
    UBYTE *Path;                   /* (M) */
    UBYTE *SetupDir;               /* (M) Directory with setup file */
    UBYTE *SetupFile;              /* (M) Name of setup file */
    UBYTE *Zip1Buffer;             /* (M) First Zip compression buffer */
    UBYTE *Zip2Buffer;             /* (M) Second Zip compression buffer */
    POSITION zeropos;              /* (M) is zero */
#ifdef WITHPTHREADS
	pthread_mutex_t handlelock;    /* (M) */
	pthread_mutex_t storefilelock; /* (M) */
    LONG    ThreadScratSize;       /* (M) Size of Fscr[0/2] buffers of the workers */
    LONG    ThreadScratOutSize;    /* (M) Size of Fscr[1] buffers of the workers */
#endif
    LONG    MaxTer;                /* (M) Maximum term size. Fixed at setup. In Bytes!!!*/
    LONG    CompressSize;          /* (M) Size of Compress buffer */
    LONG    ScratSize;             /* (M) Size of Fscr[] buffers */
    LONG    SizeStoreCache;        /* (M) Size of the chaches for reading global expr. */
    LONG    MaxStreamSize;         /* (M) Maximum buffer size in reading streams */
    LONG    SIOsize;               /* (M) Sort InputOutput buffer size */
    LONG    SLargeSize;            /* (M) */
    LONG    SSmallEsize;           /* (M) */
    LONG    SSmallSize;            /* (M) */
    LONG    STermsInSmall;         /* (M) */
    LONG    MaxBracketBufferSize;  /* (M) Max Size for B+ or AB+ per expression */
    LONG    ZipBufferSize;         /* (M) Size of buffer for gzip compression */
    LONG    hSlavePatchSize;       /* (M) */
    LONG    gSlavePatchSize;       /* (M) */
    LONG    shmWinSize;            /* (M) size for shared memory window used in communications */
    LONG    OldChildTime;          /* (M) Zero time. Needed in timer. */
	LONG	OldSecTime;            /* (M) Zero time for measuring wall clock time */
	LONG	OldMilliTime;          /* (M) Same, but milli seconds */
    LONG    WorkSize;              /* (M) Size of WorkSpace */
    LONG    gThreadBucketSize;     /* (C) */
    LONG    ggThreadBucketSize;    /* (C) */
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
    int     totalnumberofthreads;  /* (M) */
    int     gThreadStats;
    int     ggThreadStats;
    int     gFinalStats;
    int     ggFinalStats;
    int     gThreadsFlag;
    int     ggThreadsFlag;
    int     gThreadBalancing;
    int     ggThreadBalancing;
    int     gThreadSortFileSynch;
    int     ggThreadSortFileSynch;
    int     maxFlevels;            /* () maximum function levels */
    int     resetTimeOnClear;      /* (M) */
    int     gcNumDollars;          /* () number of dollars for .clear */
    int     MultiRun;
    int     gNoSpacesInNumbers;    /*     For very long numbers */
    int     ggNoSpacesInNumbers;   /*     For very long numbers */
    int     polygcdchoice;
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
    WORD    polygetremnum;         /* (M) internal number of polygetrem_ function */
    WORD    polytopnum;            /* (M) internal number of maximum poly function */
    WORD    gPolyFun;              /* (M) global value of PolyFun */
    WORD    gPolyFunType;          /* (M) global value of PolyFun */
    WORD    safetyfirst;           /* (M) for testing special versions */
    WORD    dollarzero;            /* (M) for dollars with zero value */
    WORD    atstartup;             /* To protect against DATE_ ending in \n */
    WORD    exitflag;              /* (R) For the exit statement */
    WORD    NumStoreCaches;        /* () Number of storage caches per processor */
    WORD    gIndentSpace;          /* For indentation in output */
	WORD	ggIndentSpace;
    UBYTE   SaveFileHeader[SFHSIZE];/*(M) Header written to .str and .sav files */
};
/*
 		#] M : 
 		#[ P : The P struct defines objects set by the preprocessor
*/
struct P_const {
    LIST DollarList;               /* (R) Dollar variables. Contains pointers
                                       to contents of the variables.*/
    LIST PreVarList;               /* (R) List of preprocessor variables
                                       Points to contents. Can be changed */
    LIST LoopList;                 /* (P) List of do loops */
    LIST ProcList;                 /* (P) List of procedures */
    LIST ChDollarList;             /* (P) List of Dollars changed by PP in module */
    UBYTE **PreSwitchStrings;      /* (P) The string in a switch */
    UBYTE *preStart;               /* (P) Preprocessor instruction buffer */
    UBYTE *preStop;                /* (P) end of preStart */
    UBYTE *preFill;                /* (P) Filling point in preStart */
	UBYTE *procedureExtension;     /* (P) Extension for procedure files (prc) */
	UBYTE *cprocedureExtension;    /* (P) Extension after .clear */
    int *PreIfStack;               /* (P) Tracks nesting of #if */
    int *PreSwitchModes;           /* (P) Stack of switch status */
    int *PreTypes;                 /* (P) stack of #call, #do etc nesting */
    DOLOOP      DoLoops[MAXLOOPS+1];/* (P) */
    PROCEDURE   FirstProc;         /* (P) */
    PROCEDURE   CurProc;           /* (P) */
#ifdef WITHPTHREADS
	pthread_mutex_t PreVarLock;    /* (P) */
#endif
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
    int     AllowDelay;            /* (P) Allow delayed prevar substitution */
    int     lhdollarerror;         /* (R) */
	int		eat;                   /* () */
	int     gNumPre;               /* (P) Number of preprocessor variables for .clear */
    WORD    DebugFlag;             /* (P) For debugging purposes */
    WORD    preError;              /* (?) used but not defined */
    UBYTE   ComChar;               /* (P) Commentary character */
    UBYTE   cComChar;              /* (P) Old commentary character for .clear */
};

/*
 		#] P : 
 		#[ C : The C struct defines objects changed by the compiler
*/
struct C_const {
    set_of_char separators;        /* (C) Separators in #call and #do */
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
    TABLES  usedtables;            /* (C) For checking the use of tables */
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
    WORD    *termsumcheck;         /* (C) checking of nesting */
    UBYTE *iStop;                  /* (C) Top of iBuffer. */
    UBYTE *WildcardNames;          /* (C) Names of ?a variables */
    int *Labels;                   /* (C) Label information for during run. */
    SBYTE *tokens;                 /* (C) Array with tokens for tokenizer */
    SBYTE *toptokens;              /* (C) Top of tokens */
    SBYTE *endoftokens;            /* (C) end of the actual tokens */
    WORD *tokenarglevel;           /* (C) keeps track of function arguments */
    WORD    *tableuse;             /* (C) list of tables of which the use should be checked */
#ifdef WITHPTHREADS
	LONG    *inputnumbers;         /* For redefine */
	WORD    *pfirstnum;            /* For redefine */
#endif
    LONG    argstack[MAXNEST];     /* (C) {contents} Stack for nesting of Argument */
    LONG    insidestack[MAXNEST];  /* (C) {contents} Stack for Argument or Inside. */
    LONG    inexprstack[MAXNEST];  /* (C) {contents} Stack for Argument or Inside. */
    LONG    iBufferSize;           /* (C) Size of the input buffer */
    LONG    TransEname;            /* (C) Used when a new definition overwrites
                                       an old expression. */
    LONG    SlavePatchSize;        /* (C) */
    LONG    mSlavePatchSize;       /* (C) */
    LONG    CModule;               /* (C) Counter of current module */
    LONG    ThreadBucketSize;      /* (C) Roughly the maximum number of input terms */
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
    int     tablecheck;            /* (C) For table checking */
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
    int     ThreadStats;           /* (C) */
    int     FinalStats;            /* (C) */
    int     ThreadsFlag;
    int     ThreadBalancing;
    int     ThreadSortFileSynch;
    int     BracketNormalize;      /* (C) Indicates whether the bracket st is normalized */
    int     maxtermlevel;          /* (C) Size of termstack */
    int     dumnumflag;            /* (C) Where there dummy indices in tokenizer? */
    int     bracketindexflag;      /* (C) Are brackets going to be indexed? */
    int     parallelflag;          /* (C) parallel allowed? */
    int     mparallelflag;         /* (C) parallel allowed in this module? */
    int     properorderflag;       /* (C) clean normalizing. */
    int     vetofilling;           /* (C) vetoes overwriting in tablebase stubs */
    int     tablefilling;          /* (C) to notify AddRHS we are filling a table */
    int     vetotablebasefill;     /* (C) For the load in tablebase */
    int     exprfillwarning;       /* (C) Warning has been printed for expressions in fill statements */
    int     cbufnum;               /* (R) current compiler buffer */
    int     lhdollarflag;          /* (R) left hand dollar present */
    int     NoCompress;            /* (R) Controls native compression */
#ifdef WITHPTHREADS
	int     numpfirstnum;          /* For redefine */
	int     sizepfirstnum;         /* For redefine */
	int     numpartodo;
#endif
    WORD    RepLevel;              /* (C) Tracks nesting of repeat. */
    WORD    arglevel;              /* (C) level of nested argument statements */
    WORD    insidelevel;           /* (C) level of nested inside statements */
    WORD    inexprlevel;           /* (C) level of nested inexpr statements */
    WORD    termlevel;             /* (C) level of nested term statements */
    WORD    argsumcheck[MAXNEST];  /* (C) Checking of nesting */
    WORD    insidesumcheck[MAXNEST];/* (C) Checking of nesting */
    WORD    inexprsumcheck[MAXNEST];/* (C) Checking of nesting */
    WORD    MustTestTable;         /* (C) Indicates whether tables have been changed */
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
    WORD    lPolyFun;              /* (C) local value of PolyFun */
    WORD    lPolyFunType;          /* (C) local value of PolyFunType */
    WORD    SymChangeFlag;         /* (C) */
    WORD    inusedtables;          /* (C) Number of elements in usedtables */
    WORD    CollectPercentage;     /* (C) Collect function percentage */
#ifdef PARALLEL
    WORD NumberOfRhsExprInModule;  /* (C) Number of RHS expressions*/
    WORD NumberOfRedefsInModule;   /* (C) Number of redefined variables in the module*/
#endif
    UBYTE   Commercial[COMMERCIALSIZE+2]; /* (C) Message to be printed in statistics */
};
/*
 		#] C : 
 		#[ S : The S struct defines objects changed at the start of the run (Processor)
		       Basically only set by the master.
*/
struct S_const {
#ifdef WITHPTHREADS
	pthread_mutex_t	inputslock;
	pthread_mutex_t	outputslock;
#endif
    POSITION *OldOnFile;           /* (S) File positions of expressions */
    int     NumOldOnFile;          /* (S) Number of expressions in OldOnFile */
    int     MultiThreaded;         /* (S) Are we running multi-threaded? */
#ifdef WITHPTHREADS
    int     MasterSort;            /* Final stage of sorting to the master */
#endif
    int     Balancing;             /* For second stage loadbalancing */
    WORD    ExecMode;              /* (S) */

    WORD    CollectOverFlag;       /* (R) Indicates overflow at Collect */
#ifdef WITHPTHREADS
	WORD	sLevel;                /* Copy of AR0.sLevel because it can get messy */
#endif
};
/*
 		#] S : 
 		#[ R : The R struct defines objects changed at run time.
               They determine the environment that has to be transfered
               together with a term during multithreaded execution.
*/
struct R_const {
    FILEHANDLE *infile;            /* (R) Points alternatingly to Fscr[0] or Fscr[1] */
    FILEHANDLE *outfile;           /* (R) Points alternatingly to Fscr[1] or Fscr[0] */
    FILEHANDLE *hidefile;          /* (R) Points to Fscr[2] */
    FILEDATA    StoreData;         /* (O) */

    WORD    *CompressBuffer;       /* (M) */
    WORD    *ComprTop;             /* (M) */
    WORD    *CompressPointer;      /* (R) */
	VOID    *CompareRoutine;
    FILEHANDLE  Fscr[3];           /* (R) Dollars etc play with it too */
    FILEHANDLE  FoStage4[2];       /* (R) In Sort. Stage 4. */

    POSITION DefPosition;          /* (R) Deferred position of keep brackets. */
#ifdef WITHPOSIXCLOCK
	struct timespec timing;        /* Struct defined in time.h */
#endif
    LONG    OldTime;               /* (R) Zero time. Needed in timer. */
    LONG    InInBuf;               /* (R) Characters in input buffer. Scratch files. */
    LONG    pWorkSize;             /* (R) Size of pWorkSpace */
    LONG    lWorkSize;             /* (R) Size of lWorkSpace */
    LONG    posWorkSize;           /* (R) Size of posWorkSpace */
    int     NoCompress;            /* (R) Controls native compression */
    int     gzipCompress;          /* (R) Controls gzip compression */
    int     Cnumlhs;               /* Local copy of cbuf[rbufnum].numlhs */
#ifdef WITHPTHREADS
    int     exprtodo;              /* The expression to do in parallel mode */
#endif
    WORD    GetFile;               /* (R) Where to get the terms {like Hide} */
    WORD    KeptInHold;            /* (R) */
    WORD    BracketOn;             /* (R) Intensly used in poly_ */
    WORD    MaxBracket;            /* (R) Size of BrackBuf. Changed by poly_ */
    WORD    CurDum;                /* (R) Current maximum dummy number */
    WORD    DeferFlag;             /* (R) For defered brackets */
    WORD    TePos;                 /* (R) */
    WORD    sLevel;                /* (R) Sorting level */
    WORD    Stage4Name;            /* (R) Sorting only */
    WORD    GetOneFile;            /* (R) Getting from hide or regular */
    WORD    PolyFun;               /* (C) Number of the PolyFun function */
    WORD    PolyFunType;           /* () value of PolyFunType */
    WORD    Eside;                 /* () Tells which side of = sign */
	WORD	MaxDum;                /* Maximum dummy value in an expression */
	WORD	level;                 /* Running level in Generator */
    WORD    expchanged;            /* (R) Info about expression */
    WORD    expflags;              /* (R) Info about expression */
    WORD    CurExpr;               /* (S) Number of current expression */
	WORD    SortType;              /* A copy of AC.SortType to play with */
};

/*
 		#] R : 
 		#[ T : These are variables that stay in each thread during multi threaded execution.
*/
struct T_const {
#ifdef WITHPTHREADS
    SORTBLOCK SB;
#endif
    SORTING *S0;                   /* (-) The thread specific sort buffer */
    SORTING *SS;                   /* (R) Current sort buffer */
    NESTING     Nest;              /* (R) Nesting of function levels etc. */
    NESTING     NestStop;          /* (R) */
    NESTING     NestPoin;          /* (R) */
    WORD    *BrackBuf;             /* (R) Bracket buffer. Used by poly_ at runtime. */
    STORECACHE  StoreCache;        /* (R) Cache for picking up stored expr. */
                                   /*     Seems not to be active */
    WORD    **pWorkSpace;          /* (R) Workspace for pointers. Dynamic. */
    LONG    *lWorkSpace;           /* (R) WorkSpace for LONG. Dynamic. */
    POSITION *posWorkSpace;        /* (R) WorkSpace for file positions */
    WORD    *WorkSpace;            /* (M) */
    WORD    *WorkTop;              /* (M) */
    WORD    *WorkPointer;          /* (R) Pointer in the WorkSpace heap. */
    int     *RepCount;             /* (M) Buffer for repeat nesting */
    int     *RepTop;               /* (M) Top of RepCount buffer */
    WORD    *WildArgTaken;         /* (N) Stack for wildcard pattern matching */
    WORD    *n_coef;               /* (M) Used by normal. local. */
    WORD    *n_llnum;              /* (M) Used by normal. local. */
    UWORD   *factorials;           /* (T) buffer of factorials. Dynamic. */
    UWORD   *bernoullis;           /* (T) The buffer with bernoulli numbers. Dynamic. */
	WORD    *primelist;
    UWORD   *TMscrat1;             /* () Scratch for TakeModulus */
    UWORD   *TMscrat2;             /* () Scratch for TakeModulus */
    UWORD   *TMscrat3;             /* () Scratch for TakeModulus */
    UWORD   *TMscrat4;             /* () Scratch for TakeModulus */
    UWORD   *TMscrat5;             /* () Scratch for TakeModulus */
    UWORD   *TMscrat6;             /* () Scratch for TakeModulus */
    WORD    *lastpolyrem;          /* () Remainder after PolyDiv_ */
    long    *pfac;                 /* (T) array of positions of factorials. Dynamic. */
    long    *pBer;                 /* (T) array of positions of Bernoulli's. Dynamic. */
    WORD    *TMaddr;               /* (R) buffer for TestSub */
    WORD    *WildMask;             /* (N) Wildcard info during pattern matching */
    WORD    *previousEfactor;      /* () Cache for factors in expressions */
    LONG    sBer;                  /* (T) Size of the bernoullis buffer */
    LONG    pWorkPointer;          /* (R) Offset-pointer in pWorkSpace */
    LONG    lWorkPointer;          /* (R) Offset-pointer in lWorkSpace */
    LONG    posWorkPointer;        /* (R) Offset-pointer in posWorkSpace */
    int     sfact;                 /* (T) size of the factorials buffer */
    int     mfac;                  /* (T) size of the pfac array. */
    int     ebufnum;               /* (R) extra compiler buffer */
    int     WildcardBufferSize;    /* () local copy for updates */
#ifdef WITHPTHREADS
    int     identity;              /* () When we work with B->T */
    int     LoadBalancing;         /* Needed for synchronization */
#ifdef WITHSORTBOTS
    int     SortBotIn1;            /* Input stream 1 for a SortBot */
    int     SortBotIn2;            /* Input stream 2 for a SortBot */
#endif
#endif
    WORD    dummysubexp[SUBEXPSIZE+4]; /* () used in normal.c */
    WORD    onesympol[9];          /* () Used in poly.c = {8,SYMBOL,4,1,1,1,1,3,0} */
    WORD    comsym[8];             /* () Used in tools.c = {8,SYMBOL,4,0,1,1,1,3} */
    WORD    comnum[4];             /* () Used in tools.c = { 4,1,1,3 } */
    WORD    comfun[FUNHEAD+4];     /* () Used in tools.c = {7,FUNCTION,3,0,1,1,3} */
                                   /*            or { 8,FUNCTION,4,0,0,1,1,3 } */
    WORD    comind[7];             /* () Used in tools.c = {7,INDEX,3,0,1,1,3} */
    WORD    MinVecArg[7+ARGHEAD];  /* (N) but should be more local */
    WORD    FunArg[4+ARGHEAD+FUNHEAD]; /* (N) but can be more local */
    WORD    zeropol[1];            /* () Polynomial with value zero */
    WORD    onepol[5];             /* () Polynomial with value one */
    WORD    locwildvalue[SUBEXPSIZE]; /* () Used in argument.c = {SUBEXPRESSION,SUBEXPSIZE,0,0,0} */
    WORD    mulpat[SUBEXPSIZE+5];  /* () Used in argument.c = {TYPEMULT, SUBEXPSIZE+3, 0, */
                                   /* SUBEXPRESSION, SUBEXPSIZE, 0, 1, 0, 0, 0 } */
    WORD    proexp[SUBEXPSIZE+5];  /* () Used in poly.c */
    WORD    TMout[40];             /* (R) Passing info */
    WORD    TMbuff;                /* (R) Communication between TestSub and Genera */
    WORD    nfac;                  /* (T) Number of highest stored factorial */
    WORD    nBer;                  /* (T) Number of highest bernoulli number. */
    WORD    mBer;                  /* (T) Size of buffer pBer. */
    WORD    PolyAct;               /* (R) Used for putting the PolyFun at end. ini at 0 */
    WORD    RecFlag;               /* (R) Used in TestSub. ini at zero. */
	WORD    inprimelist;
	WORD    sizeprimelist;
};
/*
 		#] T : 
 		#[ N : The N struct contains variables used in running information
               that is inside blocks that should not be split, like pattern
               matching, traces etc. They are local for each thread.
               They don't need initializations.
*/
struct N_const {
    LONG    *polybpstack;          /* () Used in poly */
    WORD    *EndNest;              /* (R) Nesting of function levels etc. */
    WORD    *Frozen;               /* (R) Bracket info */
    WORD    *FullProto;            /* (R) Prototype of a subexpression or table */
    WORD    *cTerm;                /* (R) Current term for coef_ and term_ */
    int     *RepPoint;             /* (R) Pointer in RepCount buffer. Tracks repeat */
    WORD    *WildValue;            /* (N) Wildcard info during pattern matching */
    WORD    *WildStop;             /* (N) Wildcard info during pattern matching */
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
    WORD    *ForFindOnly;          /* (N) For wildcard pattern matching */
    WORD    *findTerm;             /* (N) For wildcard pattern matching */
    WORD    *findPattern;          /* (N) For wildcard pattern matching */
    WORD    *brackbuf;             /* () Used in poly */
    WORD    *polybrackets;         /* () Used in poly */
	UWORD	*EAscrat;              /* () Used in execarg */
#ifdef WITHZLIB
	Bytef	**ziobufnum;           /* () Used in compress.c */
	Bytef	*ziobuffers;           /* () Used in compress.c */
#endif
	UWORD	*IfScrat1;             /* () Used in dollar.c */
	UWORD	*IfScrat2;             /* () Used in dollar.c */
	WORD	*dummyrenumlist;       /* () Used in execute.c and store.c */
	WORD	*mgscr1;               /* () Used in factor.c */
	WORD	*mgscr2;               /* () Used in factor.c */
	WORD	*mgscr3;               /* () Used in factor.c */
	int		*funargs;              /* () Used in lus.c */
	WORD	**funlocs;             /* () Used in lus.c */
	int		*funinds;              /* () Used in lus.c */
	UWORD	*NoScrat2;             /* () Used in normal.c */
	WORD	*ReplaceScrat;         /* () Used in normal.c */
/*
	WORD	*st;
	WORD	*sm;
	WORD	*sr;
	WORD	*sc;
*/
	TRACES	*tracestack;           /* () used in opera.c */
	WORD	*selecttermundo;       /* () Used in pattern.c */
	WORD	*patternbuffer;        /* () Used in pattern.c */
	UWORD	*GlScratC;             /* () Used in ratio.c */
	UWORD	*Myscrat1;             /* () Used in reken.c */
	UWORD   *Myscrat2;             /* () Used in reken.c */
	UWORD	*Dyscrat1;             /* () Used in reken.c */
	UWORD	*Dyscrat2;             /* () Used in reken.c */
	UWORD	*ARscrat1;             /* () Used in reken.c */
	UWORD	*ARscrat2;             /* () Used in reken.c */
	UWORD	*ARscrat3;             /* () Used in reken.c */
	UWORD	*ARscrat4;             /* () Used in reken.c */
	UWORD	*MRscrat1;             /* () Used in reken.c */
	UWORD	*MRscrat2;             /* () Used in reken.c */
	UWORD	*MRscrat3;             /* () Used in reken.c */
	UWORD	*MRscrat4;             /* () Used in reken.c */
	UWORD	*Siscrat5;             /* () Used in reken.c */
	UWORD	*Siscrat6;             /* () Used in reken.c */
	UWORD	*Siscrat7;             /* () Used in reken.c */
	UWORD	*Siscrat8;             /* () Used in reken.c */
	UWORD	*DLscrat9;             /* () Used in reken.c */
	UWORD	*DLscratA;             /* () Used in reken.c */
	UWORD	*DLscratB;             /* () Used in reken.c */
	UWORD	*DLscratC;             /* () Used in reken.c */
	UWORD	*RPscratA;             /* () Used in reken.c */
	UWORD	*RPscratB;             /* () Used in reken.c */
	UWORD	*PLscratA;             /* () Used in reken.c */
#ifdef EXTRAGCD
	UWORD	*GCscrat6;             /* () Used in reken.c */
	UWORD	*GCscrat7;             /* () Used in reken.c */
	UWORD	*GCscrat8;             /* () Used in reken.c */
#endif
	UWORD	*GLscrat6;             /* () Used in reken.c */
	UWORD	*GLscrat7;             /* () Used in reken.c */
	UWORD	*GLscrat8;             /* () Used in reken.c */
	UWORD	*GLscrat9;             /* () Used in reken.c */
	UWORD	*GLscrat10;            /* () Used in reken.c */
	UWORD	*GBscrat3;             /* () Used in reken.c */
	UWORD	*GBscrat4;             /* () Used in reken.c */
	UWORD	*TLscrat1;             /* () Used in reken.c */
	UWORD	*TLscrat2;             /* () Used in reken.c */
	UWORD	*TLscrat3;             /* () Used in reken.c */
	UWORD	*TLscrat4;             /* () Used in reken.c */
	UWORD	*CCscratE;             /* () Used in reken.c */
	UWORD	*MMscrat7;             /* () Used in reken.c */
	UWORD	*MMscratC;             /* () Used in reken.c */
	UWORD	*POscrat1;             /* () Used in polynito.c */
	UWORD	*POscrat2;             /* () Used in polynito.c */
	UWORD	*POscrat3;             /* () Used in polynito.c */
	UWORD	*POscrat4;             /* () Used in polynito.c */
	UWORD	*POscratg;             /* () Used in polynito.c */
	UWORD	*POscrath;             /* () Used in polynito.c */
	WORD	*PoinScratch[300];     /* () used in reshuf.c */
	WORD	*FunScratch[300];      /* () used in reshuf.c */
	FUN_INFO *FunInfo;             /* () Used in smart.c */
	WORD	**SplitScratch;        /* () Used in sort.c */
	SORTING **FunSorts;            /* () Used in sort.c */
	UWORD	*SoScratC;             /* () Used in sort.c */
	WORD	*listinprint;          /* () Used in proces.c and message.c */
	WORD	*currentTerm;          /* () Used in proces.c and message.c */
	UWORD	*GCDbuffer;            /* () Used in argument.c and factor.c */
	UWORD	*GCDbuffer2;           /* () Used in argument.c and factor.c */
	UWORD	*LCMbuffer;            /* () Used in argument.c and factor.c */
	UWORD	*LCMb;                 /* () Used in argument.c and factor.c */
	UWORD	*LCMc;                 /* () Used in argument.c and factor.c */
#ifdef CHINREM
	UWORD	*CRscrat1;             /* () Used in factor.c */
	UWORD	*CRscrat2;             /* () Used in factor.c */
	UWORD	*CRscrat3;             /* () Used in factor.c */
#endif
	WORD	**arglist;             /* () Used in function.c */
	UWORD	*DIscratC;             /* () Used in if.c */
	UWORD	*DIscratD;             /* () Used in if.c */
	UWORD	*DIscratE;             /* () Used in if.c */
	int		*tlistbuf;             /* () used in lus.c */
#ifdef WHICHSUBEXPRESSION
	UWORD	*BinoScrat;            /* () Used in proces.c */
#endif
	UWORD	*PIFscrat;             /* () Used in poly.c */
	UWORD	*PIFscrat1;            /* () Used in poly.c */
	UWORD	*PIFscrat2;            /* () Used in poly.c */
	WORD	*compressSpace;        /* () Used in sort.c */
	WORD	*poly1a;
	WORD	*poly2a;
#ifdef WITHPTHREADS
	THREADBUCKET *threadbuck;
#endif
	POLYMOD polymod1;              /* For use in PolyModGCD and calling routines */
	POLYMOD polymod2;              /* For use in PolyModGCD and calling routines */
    POSITION OldPosIn;             /* (R) Used in sort. */
    POSITION OldPosOut;            /* (R) Used in sort */
	POSITION theposition;          /* () Used in index.c */
    LONG    polybpointer;          /* () Used in poly */
    LONG    polybsize;             /* () Used in poly */
    LONG    polybpsize;            /* () Used in poly */
	LONG	deferskipped;          /* () Used in proces.c store.c and parallel.c */
	LONG	InScratch;             /* () Used in sort.c */
	LONG	SplitScratchSize;      /* () Used in sort.c */
	LONG	ninterms;              /* () Used in proces.c and sort.c */
#ifdef WITHPTHREADS
	LONG	inputnumber;           /* () For use in redefine */
#endif
#ifdef WHICHSUBEXPRESSION
	LONG	last2;                 /* () Used in proces.c */
	LONG	last3;                 /* () Used in proces.c */
#endif
    int     NumTotWildArgs;        /* (N) Used in pattern matching */
    int     UseFindOnly;           /* (N) Controls pattern routines */
    int     UsedOtherFind;         /* (N) Controls pattern routines */
    int     doingpoly;             /* () Used in poly */
    int     sorttype;              /* () Used in poly */
    int     ErrorInDollar;         /* (R) */
	int		numfargs;              /* () Used in lus.c */
	int		numflocs;              /* () Used in lus.c */
	int		nargs;                 /* () Used in lus.c */
	int		tohunt;                /* () Used in lus.c */
	int		numoffuns;             /* () Used in lus.c */
	int		funisize;              /* () Used in lus.c */
	int		RSsize;                /* () Used in normal.c */
	int		numtracesctack;        /* () used in opera.c */
	int		intracestack;          /* () used in opera.c */
	int		numfuninfo;            /* () Used in smart.c */
	int		NumFunSorts;           /* () Used in sort.c */
	int		MaxFunSorts;           /* () Used in sort.c */
	int		arglistsize;           /* () Used in function.c */
	int		tlistsize;             /* () used in lus.c */
	int		filenum;               /* () used in setfile.c */
	int		compressSize;          /* () Used in sort.c */
	int		polysortflag;
	WORD	RenumScratch[300];     /* () used in reshuf.c */
    WORD    oldtype;               /* (N) WildCard info at pattern matching */
    WORD    oldvalue;              /* (N) WildCard info at pattern matching */
    WORD    NumWild;               /* (N) Used in Wildcard */
    WORD    RepFunNum;             /* (N) Used in pattern matching */
    WORD    DisOrderFlag;          /* (N) Disorder option? Used in pattern matching */
    WORD    WildDirt;              /* (N) dirty in wldcard substitution. */
    WORD    NumFound;              /* (N) in reshuf only. Local? */
    WORD    WildReserve;           /* (N) Used in the wildcards */
    WORD    TeInFun;               /* (R) Passing type of action */
    WORD    TeSuOut;               /* (R) Passing info. Local? */
    WORD    WildArgs;              /* (R) */
    WORD    WildEat;               /* (R) */
    WORD    PolyNormFlag;          /* (R) For polynomial arithmetic */
    WORD    bracketon;             /* () Used in poly */
    WORD    maxbracket;            /* () Used in poly */
    WORD    polyblevel;            /* () Used in poly */
	WORD	nmgscr;                /* () Used in factor.c */
	WORD	sizeselecttermundo;    /* () Used in pattern.c */
	WORD	patternbuffersize;     /* () Used in pattern.c */
	WORD	numlistinprint;        /* () Used in process.c */
	WORD	getdivgcd;
	WORD	nPOscrata;
#ifdef CHINREM
	WORD	nCRscrat1;             /* () Used in factor.c */
	WORD	nCRscrat2;             /* () Used in factor.c */
	WORD	nCRscrat3;             /* () Used in factor.c */
#endif
#ifdef WHICHSUBEXPRESSION
	WORD	nbino;                 /* () Used in proces.c */
	WORD	last1;                 /* () Used in proces.c */
#endif
	WORD	PIFnscrat;             /* () Used in poly.c */
	WORD	PIFnscrat1;            /* () Used in poly.c */
	WORD	PIFnscrat2;            /* () Used in poly.c */
};

/*
 		#] N : 
 		#[ O : The O struct concerns output variables
*/
struct O_const {
    UBYTE   *OutputLine;           /* (O) Sits also in debug statements */
    UBYTE   *OutStop;              /* (O) Top of OutputLine buffer */
    UBYTE   *OutFill;              /* (O) Filling point in OutputLine buffer */
    WORD    *bracket;              /* (O) For writing brackets */
    WORD    *termbuf;              /* (O) For writing terms */
    UBYTE   *wpos;                 /* (O) Only when storing file {local?} */
    UBYTE   *wpoin;                /* (O) Only when storing file {local?} */
    UBYTE   *DollarOutBuffer;      /* (O) Outputbuffer for Dollars */
    UBYTE   *CurBufWrt;            /* (O) Name of currently written expr. */
    FILEDATA    SaveData;          /* (O) */
    STOREHEADER SaveHeader;        /* ()  System Independent save-Files */
    WORD    transFlag;             /* ()  >0 indicades that translations have to be done */
    WORD    powerFlag;             /* ()  >0 indicades that some exponents/powers had to be adjusted */
    WORD    resizeFlag;            /* ()  >0 indicades that something went wrong when resizing words */
    WORD    bufferedInd;           /* ()  Contains extra INDEXENTRIES, see ReadSaveIndex() for an explanation */
    VOID    (*FlipWORD)(UBYTE *);  /* ()  Function pointers for translations. Initialized by ReadSaveHeader() */
    VOID    (*FlipLONG)(UBYTE *);
    VOID    (*FlipPOS)(UBYTE *);
    VOID    (*FlipPOINTER)(UBYTE *);
    VOID    (*ResizeData)(UBYTE *,int,UBYTE *,int);
    VOID    (*ResizeWORD)(UBYTE *,UBYTE *);
    VOID    (*ResizeNCWORD)(UBYTE *,UBYTE *);
    VOID    (*ResizeLONG)(UBYTE *,UBYTE *);
    VOID    (*ResizePOS)(UBYTE *,UBYTE *);
    VOID    (*ResizePOINTER)(UBYTE *,UBYTE *);
    VOID    (*CheckPower)(UBYTE *);
    VOID    (*RenumberVec)(UBYTE *);
	UBYTE	*tensorList;           /* Dynamically allocated list with functions that are tensorial. */
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
    int     NoSpacesInNumbers;     /*     For very long numbers */
    int     BlockSpaces;           /*     For very long numbers */
    WORD    OutSkip;               /* (O) How many chars to skip in output line */
    WORD    IsBracket;             /* (O) Controls brackets */
    WORD    InFbrack;              /* (O) For writing only */
    WORD    PrintType;             /* (O) */
    WORD    FortFirst;             /* (O) Only in sch.c */
    WORD    DoubleFlag;            /* (O) Output in double precision */
    WORD    IndentSpace;           /* For indentation in output */
    UBYTE   FortDotChar;           /* (O) */
};
/*
 		#] O : 
 		#[ X : The X struct contains variables that deal with the external channel
*/
struct X_const {
	UBYTE	*currentPrompt;
	/*[08may2006 mt]:*/
	int timeout;				/*timeout to initialize preset channels.
										If timeout<0, the preset channels are 
										already initialized*/
	int killSignal;			/* signal number, SIGKILL by default*/
	int killWholeGroup;		/* if 0, the signal is sent only to a process, 
										if !=0 (default) is sent to a whole process group*/
	int daemonize;				/* if !=0 (default), start in a daemon mode */
	UBYTE *shellname;			/* if !=NULL (default is "/bin/sh -c"), start in 
										the specified	subshell*/
	UBYTE *stderrname;		/* If !=NULL (default if "/dev/null"), stderr is 
										redirected to the specified file*/
	/*:[08may2006 mt]*/
	int		currentExternalChannel;
};
/*
 		#] X : 
 		#[ Definitions :
*/

#ifdef WITHPTHREADS

typedef struct AllGlobals {
    struct M_const M;
    struct P_const P;
    struct C_const C;
    struct S_const S;
    struct O_const O;
	struct X_const X;
} ALLGLOBALS;

typedef struct AllPrivates {
    struct R_const R;
    struct T_const T;
    struct N_const N;
} ALLPRIVATES;

#else

typedef struct AllGlobals {
    struct M_const M;
    struct P_const P;
    struct C_const C;
    struct S_const S;
    struct R_const R;
    struct T_const T;
    struct N_const N;
    struct O_const O;
	struct X_const X;
} ALLGLOBALS;

#endif

/*
 		#] Definitions : 
    #] A :
  	#[ FG :
*/

#ifdef ANSI
typedef WORD (*WCN)(PHEAD WORD *,WORD *,WORD,WORD);
typedef WORD (*WCN2)(PHEAD WORD *,WORD *);
#else
typedef WORD (*WCN)();
typedef WORD (*WCN2)();
#endif

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
