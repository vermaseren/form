/** @file structs.h
 * 
 *  Contains definitions for global structs.
 *
 *  !!!CAUTION!!!
 *  Changes in this file will most likely have consequences for the recovery
 *  mechanism (see checkpoint.c). You need to care for the code in checkpoint.c
 *  as well and modify the code there accordingly!
 *
 *  The marker [D] is used in comments in this file to mark pointers to which
 *  dynamically allocated memory is assigned by a call to malloc() during
 *  runtime (in contrast to pointers that point into already allocated memory).
 *  This information is especially helpful if one needs to know which pointers
 *  need to be freed (cf. checkpoint.c).
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
 
#ifndef __STRUCTS__

#define __STRUCTS__
#ifdef _MSC_VER
#include <wchar.h>  /* off_t */
#endif
/*
  	#[ sav&store :
*/

/**
 *
 */

typedef struct PoSiTiOn {
	off_t p1;
} POSITION;

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

STATIC_ASSERT(sizeof(STOREHEADER) == 512);

/**
 *  Defines the structure of an entry in a file index (see struct FiLeInDeX).
 *  
 *  It represents one expression in the file.
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
	PADPOSITION(0,1,0,5,MAXENAME+1);
} INDEXENTRY;

/**
 *  Maximum number of entries (struct InDeXeNtRy) in a file index (struct
 *  FiLeInDeX). Number is calculated such that the size of a file index is no
 *  more than 512 bytes.
 */
#define INFILEINDEX ((512-2*sizeof(POSITION))/sizeof(INDEXENTRY))
/**
 *  Number of empty filling bytes for a file index (struct FiLeInDeX). It is
 *  calculated such that the size of a file index is always 512 bytes.
 */
#define EMPTYININDEX (512-2*sizeof(POSITION)-INFILEINDEX*sizeof(INDEXENTRY))

/**
 *  Defines the structure of a file index in store-files and save-files.
 *  
 *  It contains several entries (see struct InDeXeNtRy) up to a maximum of
 *  #INFILEINDEX.
 *
 *  The variable number has been made of type POSITION to avoid padding
 *  problems with some types of computers/OS and keep system independence
 *  of the .sav files.
 *
 *  This struct is always 512 bytes long.
 */
typedef struct FiLeInDeX {
	POSITION	next;			/**< Position of next FILEINDEX if any */
	POSITION	number;			/**< Number of used entries in this index */
	INDEXENTRY expression[INFILEINDEX]; /**< File index entries */
	SBYTE	empty[EMPTYININDEX];		/**< Padding to 512 bytes */
} FILEINDEX;

STATIC_ASSERT(sizeof(FILEINDEX) == 512);

/**
 *
 */

typedef struct FiLeDaTa {
	FILEINDEX Index;
	POSITION Fill;
	POSITION Position;
	WORD Handle;
	WORD dirtyflag;
	PADPOSITION(0,0,0,2,0);
} FILEDATA;

/**
 *
 *  Contains the pointers to an array in which a binary search will be
 *  performed.
 */

typedef struct VaRrEnUm {
	WORD  *start;  /**< Start point for search. Points inbetween lo and hi */
	WORD  *lo;     /**< Start of memory area */
	WORD  *hi;     /**< End of memory area */
} VARRENUM;

/**
 *
 *  Only symb.lo gets dynamically allocated. All other pointers points into this
 *  memory.
 */

typedef struct ReNuMbEr {
	POSITION   startposition;
	/* First stage renumbering */
	VARRENUM   symb;          /**< Symbols */
	VARRENUM   indi;          /**< Indices */
	VARRENUM   vect;          /**< Vectors */
	VARRENUM   func;          /**< Functions */
	/* Second stage renumbering */
	WORD       *symnum;       /**< Renumbered symbols */
	WORD       *indnum;       /**< Renumbered indices */
	WORD       *vecnum;       /**< Renumbered vectors */
	WORD       *funnum;       /**< Renumbered functions */
	PADPOSITION(4,0,0,0,sizeof(VARRENUM)*4);
} *RENUMBER;

/*
  	#] sav&store : 
  	#[ Variables :
*/

/**
 *  Much information is stored in arrays of which we can double the size
 *  if the array proves to be too small. Such arrays are controled by
 *  a variable of type #LIST. The routines that expand the lists are in the
 *  file tools.c
 */

typedef struct {
	void *lijst;       /**< [D] Holds space for "maxnum" elements of size "size" each */
	char *message;     /**< Text for Malloc1 when allocating lijst. Set to constant string. */
	int num;           /**< Number of elements in lijst. */
	int maxnum;        /**< Maximum number of elements in lijst. */
	int size;          /**< Size of one element in lijst. */
	int numglobal;     /**< Marker for position when .global is executed. */
	int numtemp;       /**< At the moment only needed for sets and setstore. */
	int numclear;      /**< Only for the clear instruction. */
	PADPOINTER(0,6,0,0);
} LIST;

/**
 *	The KEYWORD struct defines names of commands/statements and the routine
 *	to be called when they are encountered by the compiler or preprocessor.
 */

typedef struct {
	char *name;
	TFUN func;
	int type;
	int flags;
} KEYWORD;

/**
 *	The KEYWORDV struct defines names of commands/statements and the variable
 *	to be affected when they are encountered by the compiler or preprocessor.
 */

typedef struct {
	char *name;
	int *var;
	int type;
	int flags;
} KEYWORDV;

/**
 *  The names of variables are kept in an array. Elements of type #NAMENODE
 *  define a tree (that is kept balanced) that make it easy and fast to look for
 *  variables. See also #NAMETREE.
 */

typedef struct NaMeNode {
	LONG name;      /**< Offset into NAMETREE::namebuffer. */
	WORD parent;    /**< =-1 if no parent. */
	WORD left;      /**< =-1 if no child. */
	WORD right;     /**< =-1 if no child. */
	WORD balance;   /**< Used for the balancing of the tree. */
	WORD type;      /**< Type associated with the name. See @ref CompilerTypes "compiler types". */
	WORD number;    /**< Number of variable in #LIST's like for example C_const::SymbolList. */
	PADLONG(0,6,0);
} NAMENODE;

/**
 *  A struct of type #NAMETREE controls a complete (balanced) tree of names
 *  for the compiler. The compiler maintains several of such trees and the
 *  system has been set up in such a way that one could define more of them
 *  if we ever want to work with local name spaces.
 */

typedef struct NaMeTree {
	NAMENODE *namenode;      /**< [D] Vector of #NAMENODE's. Number of elements is #nodesize.
	                              =0 if no memory has been allocated. */
	UBYTE    *namebuffer;    /**< [D] Buffer that holds all the name strings refered to by the
	                              NAMENODE's. Allocation size is #namesize. =0 if no memory
	                              has been allocated. */
	LONG     nodesize;       /**< Maximum number of elements in #namenode. */
	LONG     nodefill;       /**< Number of currently used nodes in #namenode. */
	LONG     namesize;       /**< Allocation size of #namebuffer in bytes. */
	LONG     namefill;       /**< Number of bytes occupied. */
	LONG     oldnamefill;    /**< UNUSED */
	LONG     oldnodefill;    /**< UNUSED */
	LONG     globalnamefill; /**< Set by .global statement to the value of #namefill. When a .store
	                              command is processed, this value will be used to reset namefill.*/
	LONG     globalnodefill; /**< Same usage as #globalnamefill, but for nodefill. */
	LONG     clearnamefill;  /**< Marks the reset point used by the .clear statement. */
	LONG     clearnodefill;  /**< Marks the reset point used by the .clear statement. */
	WORD     headnode;       /**< Offset in #namenode of head node. =-1 if tree is empty. */
	PADPOINTER(10,0,1,0);
} NAMETREE;

/**
 *	The subexpressions in the compiler are kept track of in a (balanced) tree
 *	to reduce the need for subexpressions and hence save much space in
 *	large rhs expressions (like when we have xxxxxxx occurrences of objects
 *	like f(x+1,x+1) in which each x+1 becomes a subexpression.
 *	The struct that controls this tree is COMPTREE.
 */

typedef struct tree {
	int parent;  /**< Index of parent */
	int left;    /**< Left child (if not -1) */
	int right;   /**< Right child (if not -1) */
	int value;   /**< The object to be sorted and searched */
	int blnce;   /**< Balance factor */
	int usage;   /**< Number of uses in some types of trees */
} COMPTREE;

/**
 *
 */
 
typedef struct MiNmAx {
	WORD mini;          /**< Minimum value */
	WORD maxi;          /**< Maximum value */
	WORD size;          /**< Value of one unit in this position. */
} MINMAX;

/**
 *
 */
 
typedef struct BrAcKeTiNdEx {	/* For indexing brackets in local expressions */
	POSITION start;				/* Place where bracket starts - start of expr */
	POSITION next;				/* Place of next indexed bracket in expr */
	LONG bracket;				/* Offset of position in bracketbuffer */
	LONG termsinbracket;
	PADPOSITION(0,2,0,0,0);
} BRACKETINDEX;

/**
 *
 */

typedef struct BrAcKeTiNfO {
	BRACKETINDEX *indexbuffer; /**< [D] */
	WORD  *bracketbuffer;      /**< [D] */
	LONG  bracketbuffersize;
	LONG  indexbuffersize;
	LONG  bracketfill;
	LONG  indexfill;
	WORD  SortType;            /**< The sorting criterium used (like POWERFIRST etc) */
	PADPOINTER(4,0,1,0);
} BRACKETINFO;

/**
 *
 *  buffers, mm, flags, and prototype are always dynamically allocated,
 *  tablepointers only if needed (=0 if unallocated),
 *  boomlijst and argtail only for sparse tables.
 *
 *  Allocation is done for both the normal and the stub instance (spare),
 *  except for prototype and argtail which share memory.
 */

typedef struct TaBlEs {
	WORD    *tablepointers; /**< [D] Start in tablepointers table. */
#ifdef WITHPTHREADS
	WORD    **prototype;    /**< [D] The wildcard prototyping for arguments */
	WORD    **pattern;      /**< The pattern with which to match the arguments */
#else
	WORD    *prototype;     /**< [D] The wildcard prototyping for arguments */
	WORD    *pattern;       /**< The pattern with which to match the arguments */
#endif
	MINMAX  *mm;            /**< [D] Array bounds, dimension by dimension. # elements = numind. */
	WORD    *flags;         /**< [D] Is element in use ? etc. # elements = numind. */
	COMPTREE *boomlijst;    /**< [D] Tree for searching in sparse tables */
	UBYTE   *argtail;       /**< [D] The arguments in characters. Starts for tablebase
	                             with parenthesis to indicate tail */
	struct TaBlEs *spare;   /**< [D] For tablebase. Alternatingly stubs and real */
	WORD    *buffers;       /**< [D] When we use more than one compiler buffer. */
	LONG    totind;         /**< Total number requested */
	LONG    reserved;       /**< Total reservation in tablepointers for sparse */
	LONG    defined;        /**< Number of table elements that are defined */
	LONG    mdefined;       /**< Same as defined but after .global */
	int     prototypeSize;  /**< Size of allocated memory for prototype in bytes. */
	int     numind;         /**< Number of array indices */
	int     bounds;         /**< Array bounds check on/off. */
	int     strict;         /**< >0: all must be defined. <0: undefined not substitute */
	int     sparse;         /**< > 0 --> sparse table */
	int     numtree;        /**< For the tree for sparse tables */
	int     rootnum;        /**< For the tree for sparse tables */
	int     MaxTreeSize;    /**< For the tree for sparse tables */
	WORD    bufnum;         /**< Each table potentially its own buffer */
	WORD    bufferssize;    /**< When we use more than one compiler buffer */
	WORD    buffersfill;    /**< When we use more than one compiler buffer */
	WORD    tablenum;       /**< For testing of tableuse */
	WORD    mode;           /**< 0: normal, 1: stub */
	WORD    numdummies;     /**<  */
	PADPOINTER(4,8,6,0);
} *TABLES;

/**
 *
 */

typedef struct ExPrEsSiOn {
	POSITION	onfile;
	POSITION	prototype;
    POSITION    size;
	RENUMBER renum;			/* For Renumbering of global stored expressions */
	BRACKETINFO *bracketinfo;
	BRACKETINFO *newbracketinfo;
	WORD	*renumlists;    /**< Allocated only for threaded version if variables exist,
	                             else points to AN.dummyrenumlist */
	WORD	*inmem;			/* If in memory like e.g. a polynomial */
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
	WORD	numfactors;
	WORD	sizeprototype;
#ifdef PARALLELCODE
    WORD    partodo;        /* Whether to be done in parallel mode */
	PADPOSITION(5,2,0,13,0);
#else
	PADPOSITION(5,2,0,12,0);
#endif
} *EXPRESSIONS;

/**
 *
 */

typedef struct SyMbOl {			/* Don't change unless altering .sav too */
	LONG	name;				/* Location in names buffer */
	WORD	minpower;			/* Minimum power admissible */
	WORD	maxpower;			/* Maximum power admissible */
	WORD	complex;			/* Properties wrt complex conjugation */
	WORD	number;				/* Number when stored in file */
	WORD	flags;				/* Used to indicate usage when storing */
	WORD	node;
	WORD	namesize;
	WORD	dimension;			/* For dimensionality checks */
	PADLONG(0,8,0);
} *SYMBOLS;

/**
 *
 */

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

/**
 *
 */

typedef struct VeCtOr {			/* Don't change unless altering .sav too */
	LONG	name;				/* Location in names buffer */
	WORD	complex;			/* Properties under complex conjugation */
	WORD	number;				/* Number when stored in file */
	WORD	flags;				/* Used to indicate usage when storing */
	WORD	node;
	WORD	namesize;
	WORD	dimension;			/* For dimensionality checks */
	PADLONG(0,6,0);
} *VECTORS;

/**
 *  Contains all information about a function. Also used for tables.
 *  It is used in the #LIST elements of #AC.
 */

typedef struct FuNcTiOn {  /* Don't change unless altering .sav too */
	TABLES  tabl;          /**< Used if redefined as table. != 0 if function is a table */
	LONG    symminfo;      /**< Info regarding symm properties offset in buffer */
	LONG    name;          /**< Location in namebuffer of #NAMETREE */
	WORD    commute;       /**< Commutation properties */
	WORD    complex;       /**< Properties under complex conjugation */
	WORD    number;        /**< Number when stored in file */
	WORD    flags;         /**< Used to indicate usage when storing */
	WORD    spec;          /**< Regular, Tensor, etc. See @ref FunSpecs. */
	WORD    symmetric;     /**< > 0 if symmetric properties */
	WORD    node;          /**< Location in namenode of #NAMETREE */
	WORD    namesize;      /**< Length of the name */
	WORD    dimension;     /* For dimensionality checks */
	WORD    maxnumargs;
	WORD    minnumargs;
	PADPOINTER(2,0,11,0);
} *FUNCTIONS;

/**
 *
 */

typedef struct SeTs {
	LONG	name;				/* Location in names buffer */
	WORD	type;				/* Symbol, vector, index or function */
	WORD	first;				/* First element in setstore */
	WORD	last;				/* Last element in setstore (excluding) */
	WORD	node;
	WORD	namesize;
	WORD	dimension;			/* For dimensionality checks */
    WORD    flags;              /* Like: ordered */
	PADLONG(0,7,0);
} *SETS;

/**
 *
 */

typedef struct DuBiOuS {		/* Undeclared objects. Just for compiler. */
	LONG	name;				/* Location in names buffer */
	WORD	node;
	WORD	dummy;
	PADLONG(0,2,0);
} *DUBIOUSV;
 
typedef struct FaCdOlLaR {
	WORD	*where;				/* A pointer(!) to the content */
	LONG	size;
	WORD	type;				/* Type can be DOLNUMBER or DOLTERMS */
	WORD	value;				/* in case it is a (short) number */
	PADPOINTER(1,0,2,0);
} FACDOLLAR;

typedef struct DoLlArS {
	WORD	*where;				/* A pointer(!) to the object */
	FACDOLLAR *factors;			/* an array of factors. nfactors elements */
#ifdef WITHPTHREADS
	pthread_mutex_t	pthreadslockread;
	pthread_mutex_t	pthreadslockwrite;
#endif
	LONG	size;				/* The number of words */
	LONG	name;
	WORD	type;
	WORD	node;
	WORD	index;
	WORD	zero;
	WORD	numdummies;
	WORD	nfactors;
#ifdef WITHPTHREADS
	PADPOINTER(2,0,6,sizeof(pthread_mutex_t)*2);
#else
	PADPOINTER(2,0,6,0);
#endif
} *DOLLARS;

/**
 *
 */

typedef struct MoDoPtDoLlArS {
#ifdef WITHPTHREADS
	DOLLARS	dstruct;	/* If local dollar: list of DOLLARS for each thread */
#endif
	WORD	number;
	WORD	type;
#ifdef WITHPTHREADS
	PADPOINTER(0,0,2,0);
#endif
} MODOPTDOLLAR;

/**
 *
 */

typedef struct fixedset {
	char *name;
	char *description;
	int type;
	int dimension;
} FIXEDSET;

/**
 *
 */

typedef struct TaBlEbAsEsUbInDeX {
	POSITION where;
	LONG size;
	PADPOSITION(0,1,0,0,0);
} TABLEBASESUBINDEX;

/**
 *
 */

typedef struct TaBlEbAsE {
	POSITION fillpoint;
	POSITION current;
	UBYTE *name;
	int *tablenumbers;		/* Number of each table */
	TABLEBASESUBINDEX *subindex;		/* For each table */
	int numtables;
	PADPOSITION(3,0,1,0,0);
} TABLEBASE;

/**
 *	The struct FUN_INFO is used for information about functions in the file
 *	smart.c which is supposed to intelligently look for patterns in
 *	complicated wildcard situations involving symmetric functions.
 */

typedef struct {
	WORD *location;
	int numargs;
	int numfunnies;
	int numwildcards;
	int symmet;
	int tensor;
	int commute;
	PADPOINTER(0,6,0,0);
} FUN_INFO;
 
/*
  	#] Variables : 
  	#[ Files :
*/

/**
 *	The type FILEHANDLE is the struct that controls all relevant information
 *	of a file, whether it is open or not. The file may even not yet exist.
 *	There is a system of caches (PObuffer) and as long as the information
 *	to be written still fits inside the cache the file may never be
 *	created. There are variables that can store information about different
 *	types of files, like scratch files or sort files.
 *	Depending on what is available in the system we may also have information
 *	about gzip compression (currently sort file only) or locks (TFORM).
 */

typedef struct FiLe {
	POSITION POposition;    	/* File position */
    POSITION filesize;          /* Because SEEK_END is unsafe on IBM */
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
    int handle;					/**< Our own handle. Equal -1 if no file exists. */
	int active;					/* File is open or closed. Not used. */
#ifdef WITHPTHREADS
#ifdef WITHZLIB
	PADPOSITION(11,5,2,0,sizeof(pthread_mutex_t));
#else
	PADPOSITION(9,4,2,0,sizeof(pthread_mutex_t));
#endif
#else
#ifdef WITHZLIB
	PADPOSITION(7,4,2,0,0);
#else
	PADPOSITION(5,3,2,0,0);
#endif
#endif
} FILEHANDLE;

/**
 *	Input is read from 'streams' which are represented by objects of type
 *	STREAM. A stream can be a file, a do-loop, a procedure, the string value
 *	of a preprocessor variable .....
 *	When a new stream is opened we have to keep information about where
 *	to fall back in the parent stream to allow this to happen even in the
 *	middle of reading names etc as would be the case with a`i'b
 */
 
typedef struct StreaM {
	off_t fileposition;
	off_t linenumber;
	off_t prevline;
	UBYTE *buffer;        /**< [D] Size in buffersize */
	UBYTE *pointer;       /**< pointer into buffer memory */
	UBYTE *top;           /**< pointer into buffer memory */
	UBYTE *FoldName;      /**< [D] */
	UBYTE *name;          /**< [D] */
	UBYTE *pname;         /**< for DOLLARSTREAM and PREVARSTREAM it points always to name, else it
	                           is undefined */
	LONG buffersize;
	LONG bufferposition;
	LONG inbuffer;
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
	PADPOSITION(6,3,9,0,4);
} STREAM;

typedef struct SpecTatoR {
	POSITION position;   /* The place where we will be writing */
	POSITION readpos;    /* The place from which we read */
	FILEHANDLE *fh;
	char *name;          /* We identify the spectator by the name of the expression */
	WORD exprnumber;     /* During running we use the number. */
	WORD flags;          /* local, global? */
	PADPOSITION(2,0,0,2,0);
} SPECTATOR;

/*
  	#] Files : 
  	#[ Traces :
*/

/**
 *	The struct TRACES keeps track of the progress during the expansion
 *	of a 4-dimensional trace. Each time a term gets generated the expansion
 *	tree continues in the next statement. When it returns it has to know
 *	where to continue. The 4-dimensional traces are more complicated
 *	than the n-dimensional traces (see TRACEN) because of the extra tricks
 *	that can be used. They are responsible for the shorter final expressions.
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

/**
 *	The struct TRACEN keeps track of the progress during the expansion
 *	of a 4-dimensional trace. Each time a term gets generated the expansion
 *	tree continues in the next statement. When it returns it has to know
 *	where to continue.
 */

typedef struct TrAcEn {			/* For computing n dimensional traces */
	WORD		*accu;		/* NUMBER */
	WORD		*accup;
	WORD		*termp;
	WORD		*perm;		/* number */
	WORD		*inlist;	/* number */
	WORD		sgn,num,level,factor,allsign;
	PADPOINTER(0,0,5,0);
} *TRACEN;

/*
  	#] Traces : 
  	#[ Preprocessor :
*/

/**
 *	An element of the type PREVAR is needed for each preprocessor variable.
 */
 
typedef struct pReVaR {
	UBYTE *name;		/**< allocated */
	UBYTE *value;		/**< points into memory of name */
	UBYTE *argnames;	/**< names of arguments, zero separated. points into memory of name */
	int nargs;			/**< 0 = regular, >= 1: number of macro arguments. total number */
	int wildarg;		/**< The number of a potential ?var. If none: 0. wildarg<nargs */
	PADPOINTER(0,2,0,0);
} PREVAR;
 
/**
 *	Used for #inside
 */

typedef struct {
	WORD *buffer;
	int oldcompiletype;
	int oldparallelflag;
	int oldnumpotmoddollars;
	WORD size;
	WORD numdollars;
	WORD oldcbuf;
	WORD oldrbuf;
	WORD inscbuf;
	WORD oldcnumlhs;
	PADPOINTER(0,3,6,0);
} INSIDEINFO;

/**
 *	Used by the preprocessor to load the contents of a doloop or a procedure.
 *	The struct PRELOAD is used both in the DOLOOP and PROCEDURE structs.
 */

typedef struct {
	UBYTE	*buffer;
    LONG	size;
	PADPOINTER(1,0,0,0);
} PRELOAD;

/**
 *	An element of the type PROCEDURE is needed for each procedure in the system.
 */

typedef struct {
	PRELOAD p;
	UBYTE	*name;
	int		loadmode;
	PADPOINTER(0,1,0,0);
} PROCEDURE;

/**
 *	Each preprocessor do loop has a struct of type DOLOOP to keep track
 *	of all relevant parameters like where the beginning of the loop is,
 *	what the boundaries, increment and value of the loop parameter are, etc.
 *	Also we keep the whole loop inside a buffer of type PRELOAD
 */

typedef struct DoLoOp {
	PRELOAD p;          /**< size, name and buffer */
	UBYTE *name;        /**< pointer into PRELOAD buffer */
	UBYTE *vars;        /* for {} or name of expression */
	UBYTE *contents;
	UBYTE *dollarname;  /**< For loop over terms in expression. Allocated with Malloc1() */
	LONG startlinenumber;
	LONG firstnum;
	LONG lastnum;
	LONG incnum;
	int type;
	int NoShowInput;
	int errorsinloop;
	int firstloopcall;
	WORD firstdollar;   /* When >= 0 we have to get the value from a dollar */
	WORD lastdollar;    /* When >= 0 we have to get the value from a dollar */
	WORD incdollar;     /* When >= 0 we have to get the value from a dollar */
	WORD NumPreTypes;
	WORD PreIfLevel;
	WORD PreSwitchLevel;
	PADPOINTER(4,4,6,0);
} DOLOOP;

/**
 *	The struct bit_field is used by set_in, set_set, set_del and set_sub.
 *	They in turn are used in pre.c to toggle bits that indicate whether
 *	a character can be used as a separator of function arguments.
 *	This facility is used in the communication with external channels.
 */

struct  bit_field {	/* Assume 8 bits per byte */
    UBYTE bit_0        : 1;
    UBYTE bit_1        : 1;
    UBYTE bit_2        : 1;
    UBYTE bit_3        : 1;
    UBYTE bit_4        : 1;
    UBYTE bit_5        : 1;
    UBYTE bit_6        : 1;
    UBYTE bit_7        : 1;
/*
    UINT bit_0        : 1;
    UINT bit_1        : 1;
    UINT bit_2        : 1;
    UINT bit_3        : 1;
    UINT bit_4        : 1;
    UINT bit_5        : 1;
    UINT bit_6        : 1;
    UINT bit_7        : 1;
*/
};

/**
 *	Used in set_in, set_set, set_del and set_sub.
 */

typedef struct bit_field set_of_char[32];

/**
 *	Used in set_in, set_set, set_del and set_sub.
 */

typedef struct bit_field *one_byte;

/**
 *	The struct HANDLERS is used in the communication with external channels.
 */

typedef struct {
	WORD newlogonly;
	WORD newhandle;
	WORD oldhandle;
	WORD oldlogonly;
	WORD oldprinttype;
	WORD oldsilent;
} HANDLERS;

/*
  	#] Preprocessor : 
  	#[ Varia :
*/

typedef WORD (*FINISHUFFLE)(WORD *);
typedef WORD (*DO_UFFLE)(WORD *,WORD,WORD,WORD);
typedef WORD (*COMPARE)(WORD *,WORD *,WORD);

/**
 *  The CBUF struct is used by the compiler. It is a compiler buffer of which
 *  since version 3.0 there can be many.
 */

typedef struct CbUf {
    WORD *Buffer;         /**< [D] Size in BufferSize */
    WORD *Top;            /**< pointer to the end of the Buffer memory */
    WORD *Pointer;        /**< pointer into the Buffer memory */
    WORD **lhs;           /**< [D] Size in maxlhs. list of pointers into Buffer. */
    WORD **rhs;           /**< [D] Size in maxrhs. list of pointers into Buffer. */
    LONG *CanCommu;       /**< points into rhs memory behind WORD* area. */
    LONG *NumTerms;       /**< points into rhs memory behind CanCommu area */
    WORD *numdum;         /**< points into rhs memory behind NumTerms */
    WORD *dimension;      /**< points into rhs memory behind numdum */
    COMPTREE *boomlijst;  /**< [D] Number elements in MaxTreeSize */
    LONG BufferSize;      /**< Number of allocated WORD's in Buffer */
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

/**
 *  When we read input from text files we have to remember not only their
 *  handle but also their name. This is needed for error messages.
 *  Hence we call such a file a channel and reserve a struct of type
 *  #CHANNEL to allow to lay this link.
 */

typedef struct ChAnNeL {
    char *name;          /**< [D] Name of the channel */
    int handle;          /**< File handle */
    PADPOINTER(0,1,0,0);
} CHANNEL;

/**
 *  Each setup parameter has one element of the struct SETUPPARAMETERS
 *  assigned to it. By binary search in the array of them we can then
 *  locate the proper element by name.
 *  We have to assume that two ints make a long and either one or two longs
 *  make a pointer. The long before the ints and padding gives a problem
 *  in the initialization.
 */
 
typedef struct {
    UBYTE *parameter;
    int type;
    int flags;
    LONG value;
} SETUPPARAMETERS;

/**
 *  The NESTING struct is used when we enter the argument of functions and
 *  there is the possibility that we have to change something there.
 *  Because functions can be nested we have to keep track of all levels
 *  of functions in case we have to move the outer layers to make room
 *  for a larger function argument.
 */

typedef struct NeStInG {
    WORD *termsize;
    WORD *funsize;
    WORD *argsize;
} *NESTING;

/**
 *  The struct of type STORECACHE is used by a caching system for reading
 *  terms from stored expressions. Each thread should have its own system
 *  of caches.
 */

typedef struct StOrEcAcHe {
    POSITION position;
    POSITION toppos;
    struct StOrEcAcHe *next;
    WORD buffer[2];
    PADPOSITION(1,0,0,2,0);
} *STORECACHE;

/**
 *  The struct PERM is used to generate all permutations when the pattern
 *  matcher has to try to match (anti)symmetric functions.
 */

typedef struct PeRmUtE {
    WORD *objects;
    WORD sign;
    WORD n;
    WORD cycle[MAXMATCH];
    PADPOINTER(0,0,MAXMATCH+2,0);
} PERM;

/**
 *  Like struct PERM but works with pointers.
 */

typedef struct PeRmUtEp {
    WORD **objects;
    WORD sign;
    WORD n;
    WORD cycle[MAXMATCH];
    PADPOINTER(0,0,MAXMATCH+2,0);
} PERMP;

/**
 *  The struct DISTRIBUTE is used to help the pattern
 *  matcher when matching antisymmetric tensors.
 */

typedef struct DiStRiBuTe {
    WORD *obj1;
    WORD *obj2;
    WORD *out;
    WORD sign;
    WORD n1;
    WORD n2;
    WORD n;
    WORD cycle[MAXMATCH];
    PADPOINTER(0,0,(MAXMATCH+4),0);
} DISTRIBUTE;

/**
 *  The struct PARTI is used to help determining whether a partition_
 *  function can be replaced.
 */

typedef struct PaRtI {
	WORD *psize;  /* the sizes of the partitions */
	WORD *args;   /* the offsets of the arguments to be partitioned */
	WORD *nargs;  /* argument numbers (different number = different argument) */
	WORD *nfun;   /* the functions into which the partitions go */
	WORD numargs; /* the number of arguments to be partitioned */
	WORD numpart; /* the number of partitions */
	WORD where;   /* offset of the function in the term */
    PADPOINTER(0,0,3,0);
} PARTI;

/**
 *  The struct SORTING is used to control a sort operation.
 *  It includes a small and a large buffer and arrays for keeping track
 *  of various stages of the (merge) sorts.
 *  Each sort level has its own struct and different levels can have
 *  different sizes for its arrays.
 *  Also different threads have their own set of SORTING structs.
 */

typedef struct sOrT {
    FILEHANDLE file;            /* The own sort file */
    POSITION SizeInFile[3];     /* Sizes in the various files */
    WORD *lBuffer;              /* The large buffer */
    WORD *lTop;                 /* End of the large buffer */
    WORD *lFill;                /* The filling point of the large buffer */
    WORD *used;                 /*  auxiliary during actual sort */
    WORD *sBuffer;              /* The small buffer */
    WORD *sTop;                 /* End of the small buffer */
    WORD *sTop2;                /* End of the extension of the small buffer */
    WORD *sHalf;                /* Halfway point in the extension */
    WORD *sFill;                /* Filling point in the small buffer */
    WORD **sPointer;            /* Pointers to terms in the small buffer */
    WORD **PoinFill;            /* Filling point for pointers to the sm.buf */
    WORD **SplitScratch;        /* Excess pointer space for the merge sort */
    WORD *cBuffer;              /* Compress buffer (if it exists) */
    WORD **Patches;             /* Positions of patches in large buffer */
    WORD **pStop;               /* Ends of patches in the large buffer */
    WORD **poina;               /*  auxiliary during actual sort */
    WORD **poin2a;              /*  auxiliary during actual sort */
    WORD *ktoi;                 /*  auxiliary during actual sort */
    WORD *tree;                 /*  auxiliary during actual sort */
#ifdef WITHZLIB
    WORD *fpcompressed;         /*  is output filepatch compressed? */
    WORD *fpincompressed;       /*  is input filepatch compressed? */
    z_streamp zsparray;         /*  the array of zstreams for decompression */
#endif
    POSITION *fPatches;         /* Positions of output file patches */
    POSITION *inPatches;        /* Positions of input file patches */
    POSITION *fPatchesStop;     /* Positions of output file patches */
    POSITION *iPatches;         /* Input file patches, Points to fPatches or inPatches */
    FILEHANDLE *f;              /* The actual output file */
    FILEHANDLE **ff;            /* Handles for a staged sort */
    LONG sTerms;                /* Terms in small buffer */
    LONG LargeSize;             /* Size of large buffer (in words) */
    LONG SmallSize;             /* Size of small buffer (in words) */
    LONG SmallEsize;            /* Size of small + extension (in words) */
    LONG TermsInSmall;          /* Maximum number of terms in small buffer */
    LONG Terms2InSmall;         /* with extension for polyfuns etc. */
    LONG GenTerms;              /* Number of generated terms */
    LONG TermsLeft;             /* Number of terms still in existence */
    LONG GenSpace;              /* Amount of space of generated terms */
    LONG SpaceLeft;             /* Space needed for still existing terms */
    LONG putinsize;             /* Size of buffer in putin */
    LONG ninterms;              /* Which input term ? */
    int MaxPatches;             /* Maximum number of patches in large buffer */
    int MaxFpatches;            /* Maximum number of patches in one filesort */
    int type;                   /* Main, function or sub(routine) */
    int lPatch;                 /* Number of patches in the large buffer */
    int fPatchN1;               /* Number of patches in input file */
    int PolyWise;               /* Is there a polyfun and if so, where? */
    int PolyFlag;               /*  */
    int cBufferSize;            /* Size of the compress buffer */
    int maxtermsize;            /* Keeps track for buffer allocations */
    int newmaxtermsize;         /* Auxiliary for maxtermsize */
    int outputmode;             /* Tells where the output is going */
    int stagelevel;             /* In case we have a 'staged' sort */
    WORD fPatchN;               /* Number of patches on file (output) */
    WORD inNum;                 /* Number of patches on file (input) */
    WORD stage4;                /* Are we using stage4? */
#ifdef WITHZLIB
    PADPOSITION(28,12,12,3,0);
#else
    PADPOSITION(25,12,12,3,0);
#endif
} SORTING;

#ifdef WITHPTHREADS

/**
 *  The SORTBLOCK's are used by TFORM when the master has to merge the sorted
 *  results of each of the workers.
 */

typedef struct SoRtBlOcK {
    pthread_mutex_t *MasterBlockLock;
    WORD    **MasterStart;
    WORD    **MasterFill;
    WORD    **MasterStop;
    int     MasterNumBlocks;
    int     MasterBlock;
    int     FillBlock;
    PADPOINTER(0,3,0,0);
} SORTBLOCK;
#endif

#ifdef DEBUGGER
typedef struct DeBuGgInG {
    int eflag;
    int printflag;
    int logfileflag;
    int stdoutflag;
} DEBUGSTR;
#endif

#ifdef WITHPTHREADS

/**
 *  The THREADBUCKET struct defines one of the buckets used to pass terms
 *  from the master to the workers in TFORM.
 */

typedef struct ThReAdBuCkEt {
    POSITION *deferbuffer;      /* For Keep Brackets: remember position */
    WORD *threadbuffer;         /* Here are the (primary) terms */
    WORD *compressbuffer;       /* For keep brackets we need the compressbuffer */
    LONG threadbuffersize;      /* Number of words in threadbuffer */
    LONG ddterms;               /* Number of primary+secondary terms represented */
    LONG firstterm;             /* The number of the first term in the bucket */
    LONG firstbracket;          /* When doing complete brackets */
    LONG lastbracket;           /* When doing complete brackets */
    pthread_mutex_t lock;       /* For the load balancing phase */
    int  free;                  /* Status of the bucket */
    int  totnum;                /* Total number of primary terms */
    int  usenum;                /* Which is the term being used at the moment */
    int  busy;                  /*  */
    int  type;                  /* Doing brackets? */
    PADPOINTER(5,5,0,sizeof(pthread_mutex_t));
} THREADBUCKET;

#endif

/**
 *  The POLYMOD struct controls one univariate polynomial of which the
 *  coefficients have been taken modulus a (prime) number that fits inside
 *  a variable of type WORD. The polynomial is stored as an array of
 *  coefficients of size WORD.
 */

typedef struct {
    WORD    *coefs;             /* The array of coefficients */
    WORD    numsym;             /* The number of the symbol in the polynomial */
    WORD    arraysize;          /* The size of the allocation of coefs */
    WORD    polysize;           /* The maximum power in the polynomial */
    WORD    modnum;             /* The prime number of the modulus */
} POLYMOD;

typedef struct {
    WORD    *outterm;            /* Used in DoShuffle/Merge/FinishShuffle system */
    WORD    *outfun;
    WORD    *incoef;
    WORD    *stop1;
    WORD    *stop2;
    WORD    *ststop1;
    WORD    *ststop2;
    FINISHUFFLE finishuf;
    DO_UFFLE	do_uffle;
    LONG    combilast;
    WORD    nincoef;
    WORD    level;
    WORD    thefunction;
    WORD    option;
    PADPOINTER(1,0,4,0);
} SHvariables;

typedef struct {                /* Used for computing calculational cost in optim.c */
    LONG    add;
    LONG    mul;
    LONG    div;
    LONG    pow;
} COST;

typedef struct {
    UWORD   *a;     /* The number array */
    UWORD   *m;     /* The modulus array */
    WORD    na;     /* Size of the number */
    WORD    nm;     /* size of the number in the modulus array */
} MODNUM;

/*
    Struct for optimizing outputs. If changed, do not forget to change
    the padding information in the AO struct.
*/
typedef struct {
    union { /* we do this to allow padding */
        float fval;
        int ival[2]; /* This should be enough */
    } mctsconstant;
    int   horner;
    int   hornerdirection;
    int   method;
    int   mctstimelimit;
    int   mctsnumexpand;
    int   mctsnumkeep;
    int   mctsnumrepeat;
    int   greedytimelimit;
    int   greedyminnum;
    int   greedymaxperc;
    int   printstats;
    int   debugflags;
    int   schemeflags;
    int   mctsdecaymode;
    int   saIter; /* Simulated annealing updates */
    union {
        float fval;
        int ival[2];
    } saMaxT; /* Maximum temperature of SA */
    union {
        float fval;
        int ival[2];
    } saMinT; /* Minimum temperature of SA */
} OPTIMIZE;

typedef struct {
    WORD  *code;
    UBYTE *nameofexpr;  /* It is easier to remember an expression by name */
    LONG  codesize;     /* We need this for the checkpoints */
    WORD  exprnr;       /* Problem here is: we renumber them in execute.c */
    WORD  minvar;
    WORD  maxvar;
    
    PADPOSITION(2,1,0,3,0);
} OPTIMIZERESULT;
 
typedef struct {
    WORD *lhs;      /* Object to be replaced */
    WORD *rhs;      /* Depending on the type it will be UBYTE* or WORD* */
    int type;
    int size;       /* Size of the lhs */
} DICTIONARY_ELEMENT;

typedef struct {
    DICTIONARY_ELEMENT **elements;
    UBYTE *name;
    int sizeelements;
    int numelements;
    int numbers;        /* deal with numbers */
    int variables;      /* deal with single variables */
    int characters;     /* deal with special characters */
    int funwith;        /* deal with functions with arguments */
    int gnumelements;   /* if .global shrinks the dictionary */
    int ranges;
} DICTIONARY;

typedef struct {
	WORD ncase;
	WORD value;
	WORD compbuffer;
} SWITCHTABLE;

typedef struct {
	SWITCHTABLE *table;
	SWITCHTABLE defaultcase;
	SWITCHTABLE endswitch;
	WORD typetable;
	WORD maxcase;
	WORD mincase;
	WORD numcases;
	WORD tablesize;
	WORD caseoffset;
	WORD iflevel;
	WORD whilelevel;
	WORD nestingsum;
    WORD padding;
} SWITCH;

/*
	The next typedef comes originally from declare.h but because new compilers
	do not like casting from void * to a function address we have to put it here
*/

/*
  	#] Varia : 
    #[ A :
 		#[ M : The M struct is for global settings at startup or .clear
*/

/**
 *	The M_const struct is part of the global data and resides in the #ALLGLOBALS
 *	struct #A under the name #M.
 *	We see it used with the macro #AM as in AM.S0.
 *	It contains global settings at startup or .clear.
 */

struct M_const {
    POSITION zeropos;              /* (M) is zero */
    SORTING *S0;                   /**< [D] The main sort buffer */
    UWORD   *gcmod;                /**< Global setting of modulus. Uses AC.cmod's memory */
    UWORD   *gpowmod;              /**< Global setting printing as powers. Uses AC.cmod's memory */
    UBYTE   *TempDir;              /* (M) Path with where the temporary files go */
    UBYTE   *TempSortDir;          /* (M) Path with where the sort files go */
    UBYTE   *IncDir;               /* (M) Directory path for include files */
    UBYTE   *InputFileName;        /* (M) */
    UBYTE   *LogFileName;          /* (M) */
    UBYTE   *OutBuffer;            /* (M) Output buffer in pre.c */
    UBYTE   *Path;                 /* (M) */
    UBYTE   *SetupDir;             /* (M) Directory with setup file */
    UBYTE   *SetupFile;            /* (M) Name of setup file */
    UBYTE   *gFortran90Kind;
	UBYTE   *gextrasym;
	UBYTE   *ggextrasym;
    UBYTE   *oldnumextrasymbols;
	SPECTATOR *SpectatorFiles;
#ifdef WITHPTHREADS
    pthread_rwlock_t handlelock;   /* (M) */
    pthread_mutex_t storefilelock; /* (M) */
	pthread_mutex_t	sbuflock;      /* (M) Lock for writing in the AM.sbuffer */
    LONG    ThreadScratSize;       /* (M) Size of Fscr[0/2] buffers of the workers */
    LONG    ThreadScratOutSize;    /* (M) Size of Fscr[1] buffers of the workers */
#endif
    LONG    MaxTer;                /* (M) Maximum term size. Fixed at setup. In Bytes!!!*/
    LONG    CompressSize;          /* (M) Size of Compress buffer */
    LONG    ScratSize;             /* (M) Size of Fscr[] buffers */
    LONG    HideSize;              /* (M) Size of Fscr[2] buffer */
    LONG    SizeStoreCache;        /* (M) Size of the chaches for reading global expr. */
    LONG    MaxStreamSize;         /* (M) Maximum buffer size in reading streams */
    LONG    SIOsize;               /* (M) Sort InputOutput buffer size */
    LONG    SLargeSize;            /* (M) */
    LONG    SSmallEsize;           /* (M) */
    LONG    SSmallSize;            /* (M) */
    LONG    STermsInSmall;         /* (M) */
    LONG    MaxBracketBufferSize;  /* (M) Max Size for B+ or AB+ per expression */
    LONG    hProcessBucketSize;    /* (M) */
    LONG    gProcessBucketSize;    /* (M) */
    LONG    shmWinSize;            /* (M) size for shared memory window used in communications */
    LONG    OldChildTime;          /* (M) Zero time. Needed in timer. */
    LONG    OldSecTime;            /* (M) Zero time for measuring wall clock time */
    LONG    OldMilliTime;          /* (M) Same, but milli seconds */
    LONG    WorkSize;              /* (M) Size of WorkSpace */
    LONG    gThreadBucketSize;     /* (C) */
    LONG    ggThreadBucketSize;    /* (C) */
    LONG    SumTime;               /*     Used in .clear */
    LONG    SpectatorSize;         /*     Size of the buffer in bytes */
    LONG    TimeLimit;             /*     Limit in sec to the total real time */
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
    int     sbufnum;               /* (M) subterm variables */
    int     zbufnum;               /* (M) special values */
    int     SkipClears;            /* (M) Number of .clear to skip at start */
    int     gTokensWriteFlag;      /* (M) */
    int     gfunpowers;            /* (M) */
    int     gStatsFlag;            /* (M) */
    int     gNamesFlag;            /* (M) */
    int     gCodesFlag;            /* (M) */
    int     gSortType;             /* (M) */
    int     gproperorderflag;      /* (M) */
    int     hparallelflag;         /* (M) */
    int     gparallelflag;         /* (M) */
    int     totalnumberofthreads;  /* (M) */
    int     gSizeCommuteInSet;
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
    int     gProcessStats;
    int     ggProcessStats;
    int     gOldParallelStats;
    int     ggOldParallelStats;
    int     maxFlevels;            /* () maximum function levels */
    int     resetTimeOnClear;      /* (M) */
    int     gcNumDollars;          /* () number of dollars for .clear */
    int     MultiRun;
    int     gNoSpacesInNumbers;    /* For very long numbers */
    int     ggNoSpacesInNumbers;   /* For very long numbers */
    int     gIsFortran90;
	int		PrintTotalSize;
    int     fbuffersize;           /* Size for the AT.fbufnum factorization caches */
    int     gOldFactArgFlag;
    int     ggOldFactArgFlag;
    int     gnumextrasym;
    int     ggnumextrasym;
	int		NumSpectatorFiles;     /* Elements used in AM.spectatorfiles; */
	int		SizeForSpectatorFiles; /* Size in AM.spectatorfiles; */
    int     gOldGCDflag;
    int     ggOldGCDflag;
    int     gWTimeStatsFlag;
    int     ggWTimeStatsFlag;
    int     jumpratio;
    WORD    MaxTal;                /* (M) Maximum number of words in a number */
    WORD    IndDum;                /* (M) Basis value for dummy indices */
    WORD    DumInd;                /* (M) */
    WORD    WilInd;                /* (M) Offset for wildcard indices */
    WORD    gncmod;                /* (M) Global setting of modulus. size of gcmod */
    WORD    gnpowmod;              /* (M) Global printing as powers. size gpowmod */
    WORD    gmodmode;              /* (M) Global mode for modulus */
    WORD    gUnitTrace;            /* (M) Global value of Tr[1] */
    WORD    gOutputMode;           /* (M) */
    WORD    gOutputSpaces;         /* (M) */
    WORD    gOutNumberType;        /* (M) */
    WORD    gCnumpows;             /* (M) */
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
    WORD    gPolyFun;              /* (M) global value of PolyFun */
    WORD    gPolyFunInv;           /* (M) global value of Inverse of PolyFun */
    WORD    gPolyFunType;          /* (M) global value of PolyFun */
    WORD    gPolyFunExp;
    WORD    gPolyFunVar;
    WORD    gPolyFunPow;
    WORD    dollarzero;            /* (M) for dollars with zero value */
    WORD    atstartup;             /* To protect against DATE_ ending in \n */
    WORD    exitflag;              /* (R) For the exit statement */
    WORD    NumStoreCaches;        /* () Number of storage caches per processor */
    WORD    gIndentSpace;          /* For indentation in output */
    WORD    ggIndentSpace;
    WORD    gShortStatsMax;        /**< For  On FewerStatistics 10; */
    WORD    ggShortStatsMax;       /**< For  On FewerStatistics 10; */
    WORD    gextrasymbols;
    WORD    ggextrasymbols;
    WORD    zerorhs;
    WORD    onerhs;
    WORD    havesortdir;
    WORD    vectorzero;            /* p0_ */
    WORD    ClearStore;
    WORD    BracketFactors[8];
#ifdef WITHPTHREADS
	PADPOSITION(17,26,62,83,(sizeof(pthread_rwlock_t)+sizeof(pthread_mutex_t)*2));
#else
	PADPOSITION(17,24,62,83,0);
#endif
};
/*
 		#] M : 
 		#[ P : The P struct defines objects set by the preprocessor
*/
/**
 *  The P_const struct is part of the global data and resides in the
 *  ALLGLOBALS struct A under the name P
 *  We see it used with the macro AP as in AP.InOutBuf
 *  It contains objects that have dealings with the preprocessor.
 */

struct P_const {
    LIST DollarList;               /* (R) Dollar variables. Contains pointers
                                       to contents of the variables.*/
    LIST PreVarList;               /* (R) List of preprocessor variables
                                       Points to contents. Can be changed */
    LIST LoopList;                 /* (P) List of do loops */
    LIST ProcList;                 /* (P) List of procedures */
    INSIDEINFO inside;             /*     Information during #inside/#endinside */
    UBYTE **PreSwitchStrings;      /* (P) The string in a switch */
    UBYTE *preStart;               /* (P) Preprocessor instruction buffer */
    UBYTE *preStop;                /* (P) end of preStart */
    UBYTE *preFill;                /* (P) Filling point in preStart */
    UBYTE *procedureExtension;     /* (P) Extension for procedure files (prc) */
    UBYTE *cprocedureExtension;    /* (P) Extension after .clear */
    LONG  *PreAssignStack;         /* For nesting #$name assignments */
    int *PreIfStack;               /* (P) Tracks nesting of #if */
    int *PreSwitchModes;           /* (P) Stack of switch status */
    int *PreTypes;                 /* (P) stack of #call, #do etc nesting */
#ifdef WITHPTHREADS
    pthread_mutex_t PreVarLock;    /* (P) */
#endif
    LONG    StopWatchZero;         /* For `timer_' and #reset timer */
    LONG    InOutBuf;              /* (P) Characters in the output buf in pre.c */
    LONG    pSize;                 /* (P) size of preStart */
    int     PreAssignFlag;         /* (C) Indicates #assign -> catch dollar */
    int     PreContinuation;       /* (C) Indicates whether the statement is new */
    int     PreproFlag;            /* (P) Internal use to mark work on prepro instr. */
    int     iBufError;             /* (P) Flag for errors with input buffer */
    int     PreOut;                /* (P) Flag for #+ #- */
    int     PreSwitchLevel;        /* (P) Nesting of #switch */
    int     NumPreSwitchStrings;   /* (P) Size of PreSwitchStrings */
    int     MaxPreTypes;           /* (P) Size of PreTypes */
    int     NumPreTypes;           /* (P) Number of nesting objects in PreTypes */
    int     MaxPreIfLevel;         /* (C) Maximum number of nested #if. Dynamic */
    int     PreIfLevel;            /* (C) Current position if PreIfStack */
    int     PreInsideLevel;        /* (C) #inside active? */
    int     DelayPrevar;           /* (P) Delaying prevar substitution */
    int     AllowDelay;            /* (P) Allow delayed prevar substitution */
    int     lhdollarerror;         /* (R) */
    int     eat;                   /* () */
    int     gNumPre;               /* (P) Number of preprocessor variables for .clear */
    int     PreDebug;              /* (C) */
    int     OpenDictionary;
    int     PreAssignLevel;        /* For nesting #$name = ...; assignments */
    int     MaxPreAssignLevel;     /* For nesting #$name = ...; assignments */
    WORD    DebugFlag;             /* (P) For debugging purposes */
    WORD    preError;              /* (P) Blocks certain types of execution */
    UBYTE   ComChar;               /* (P) Commentary character */
    UBYTE   cComChar;              /* (P) Old commentary character for .clear */
    PADPOINTER(3,21,2,2);
};

/*
 		#] P : 
 		#[ C : The C struct defines objects changed by the compiler
*/

/**
 *  The C_const struct is part of the global data and resides in the #ALLGLOBALS
 *  struct #A under the name #C.
 *  We see it used with the macro #AC as in AC.exprnames.
 *  It contains variables that involve the compiler and objects set during
 *  compilation.
 */

struct C_const {
    set_of_char separators;        /**< Separators in #call and #do */
	POSITION StoreFileSize;        /* () Size of store file */
    NAMETREE *dollarnames;         /**< [D] Names of dollar variables */
    NAMETREE *exprnames;           /**< [D] Names of expressions */
    NAMETREE *varnames;            /**< [D] Names of regular variables */
    LIST    ChannelList;           /**< Used for the #write statement. Contains #CHANNEL */
                                   /*     Later also for write? */
    LIST    DubiousList;           /**< List of dubious variables. Contains #DUBIOUSV.
                                        If not empty -> no execution */
    LIST    FunctionList;          /**< List of functions and properties. Contains #FUNCTIONS */
    LIST    ExpressionList;        /**< List of expressions, locations etc. */
    LIST    IndexList;             /**< List of indices */
    LIST    SetElementList;        /**< List of all elements of all sets */
    LIST    SetList;               /**< List of the sets */
    LIST    SymbolList;            /**< List of the symbols and their properties */
    LIST    VectorList;            /**< List of the vectors */
    LIST    PotModDolList;         /**< Potentially changed dollars */
    LIST    ModOptDolList;         /**< Module Option Dollars list */
    LIST    TableBaseList;         /**< TableBase list */
/*
    Compile buffer variables
*/
    LIST    cbufList;              /**< List of compiler buffers */
/*
    Objects for auto declarations
*/
    LIST    AutoSymbolList;        /* (C) */
    LIST    AutoIndexList;         /* (C) */
    LIST    AutoVectorList;        /* (C) */
    LIST    AutoFunctionList;      /* (C) */
    NAMETREE *autonames;           /**< [D] Names in autodeclare */

    LIST    *Symbols;              /* (C) Pointer for autodeclare. Which list is
                                      it searching. Later also for subroutines */
    LIST    *Indices;              /* (C) id. */
    LIST    *Vectors;              /* (C) id. */
    LIST    *Functions;            /* (C) id. */
    NAMETREE **activenames;        /** (C) Pointer for AutoDeclare statement. Points either to
                                           varnames or autonames. */
    STREAM  *Streams;              /**< [D] The input streams. */
    STREAM  *CurrentStream;        /**< (C) The current input stream.
                                       Streams are: do loop, file, prevariable. points into Streams memory. */
    SWITCH  *SwitchArray;
    WORD    *SwitchHeap;
    LONG    *termstack;            /**< [D] Last term statement {offset} */
    LONG    *termsortstack;        /**< [D] Last sort statement {offset} */
    UWORD   *cmod;                 /**< [D] Local setting of modulus. Pointer to value. */
    UWORD   *powmod;               /**< Local setting printing as powers. Points into cmod memory */
    UWORD   *modpowers;            /**< [D] The conversion table for mod-> powers. */
	UWORD   *halfmod;              /* (C) half the modulus when not zero */
    WORD    *ProtoType;            /* (C) The subexpression prototype {wildcards} */
    WORD    *WildC;                /* (C) Filling point for wildcards. */
    LONG    *IfHeap;               /**< [D] Keeps track of where to go in if */
    LONG    *IfCount;              /**< [D] Keeps track of where to go in if */
    LONG    *IfStack;              /**< Keeps track of where to go in if. Points into IfHeap-memory */
    UBYTE   *iBuffer;              /**< [D] Compiler input buffer */
    UBYTE   *iPointer;             /**< Running pointer in the compiler input buffer */
    UBYTE   *iStop;                /**< Top of iBuffer */
    UBYTE   **LabelNames;          /**< [D] List of names in label statements */
    WORD    *FixIndices;           /**< [D] Buffer of fixed indices */
    WORD    *termsumcheck;         /**< [D] Checking of nesting */
    UBYTE   *WildcardNames;        /**< [D] Names of ?a variables */
    int     *Labels;               /**< Label information for during run. Pointer into LabelNames memory. */
    SBYTE   *tokens;               /**< [D] Array with tokens for tokenizer */
    SBYTE   *toptokens;            /**< Top of tokens */
    SBYTE   *endoftokens;          /**< End of the actual tokens */
    WORD    *tokenarglevel;        /**< [D] Keeps track of function arguments */
	UWORD   *modinverses;          /* Table for inverses of primes */
    UBYTE   *Fortran90Kind;        /* The kind of number in Fortran 90 as in _ki */
	WORD	**MultiBracketBuf;     /* Array of buffers for multi-level brackets */
	UBYTE   *extrasym;             /* Array with the name for extra symbols in ToPolynomial */
	WORD    *doloopstack;          /* To keep track of begin and end of doloops */
	WORD    *doloopnest;           /* To keep track of nesting of doloops etc */
    char    *CheckpointRunAfter;   /**< [D] Filename of script to be executed _before_ creating the
                                        snapshot. =0 if no script shall be executed. */
    char    *CheckpointRunBefore;  /**< [D] Filename of script to be executed _after_ having
                                        created the snapshot. =0 if no script shall be executed.*/
    WORD    *IfSumCheck;           /**< [D] Keeps track of if-nesting */
    WORD    *CommuteInSet;         /* groups of noncommuting functions that can commute */
    UBYTE   *TestValue;            /* For debugging */
#ifdef PARALLELCODE
    LONG    *inputnumbers;         /**< [D] For redefine */
    WORD    *pfirstnum;            /**< For redefine. Points into inputnumbers memory */
#endif
#ifdef WITHPTHREADS
    pthread_mutex_t halfmodlock;   /* () Lock for adding buffer for halfmod */
#endif
    LONG    argstack[MAXNEST];     /* (C) {contents} Stack for nesting of Argument */
    LONG    insidestack[MAXNEST];  /* (C) {contents} Stack for Argument or Inside. */
    LONG    inexprstack[MAXNEST];  /* (C) {contents} Stack for Argument or Inside. */
    LONG    iBufferSize;           /* (C) Size of the input buffer */
    LONG    TransEname;            /* (C) Used when a new definition overwrites
                                       an old expression. */
    LONG    ProcessBucketSize;     /* (C) */
    LONG    mProcessBucketSize;    /* (C) */
    LONG    CModule;               /* (C) Counter of current module */
    LONG    ThreadBucketSize;      /* (C) Roughly the maximum number of input terms */
    LONG    CheckpointStamp;       /**< Timestamp of the last created snapshot (set to Timer(0)).*/
    LONG    CheckpointInterval;    /**< Time interval in milliseconds for snapshots. =0 if
                                        snapshots shall be created at the end of _every_ module.*/
    int     cbufnum;               /**< Current compiler buffer */
    int     AutoDeclareFlag;       /** (C) Mode of looking for names. Set to NOAUTO (=0) or
                                           WITHAUTO (=2), cf. AutoDeclare statement */
    int     NoShowInput;           /* (C) No listing of input as in .prc, #do */
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
    int     OldParallelStats;      /* (C) */
    int     ThreadsFlag;
    int     ThreadBalancing;
    int     ThreadSortFileSynch;
    int     ProcessStats;          /* (C) */
    int     BracketNormalize;      /* (C) Indicates whether the bracket st is normalized */
    int     maxtermlevel;          /* (C) Size of termstack */
    int     dumnumflag;            /* (C) Where there dummy indices in tokenizer? */
    int     bracketindexflag;      /* (C) Are brackets going to be indexed? */
    int     parallelflag;          /* (C) parallel allowed? */
    int     mparallelflag;         /* (C) parallel allowed in this module? */
    int     inparallelflag;        /* (C) inparallel allowed? */
    int     partodoflag;           /* (C) parallel allowed? */
    int     properorderflag;       /* (C) clean normalizing. */
    int     vetofilling;           /* (C) vetoes overwriting in tablebase stubs */
    int     tablefilling;          /* (C) to notify AddRHS we are filling a table */
    int     vetotablebasefill;     /* (C) For the load in tablebase */
    int     exprfillwarning;       /* (C) Warning has been printed for expressions in fill statements */
    int     lhdollarflag;          /* (R) left hand dollar present */
    int     NoCompress;            /* (R) Controls native compression */
    int     IsFortran90;           /* Tells whether the Fortran is Fortran90 */
    int     MultiBracketLevels;    /* Number of elements in MultiBracketBuf */
    int     topolynomialflag;      /* To avoid ToPolynomial and FactArg together */
    int     ffbufnum;              /* Buffer number for user defined factorizations */
    int     OldFactArgFlag;
    int     MemDebugFlag;          /* Only used when MALLOCDEBUG in tools.c */
    int     OldGCDflag;
    int     WTimeStatsFlag;
	int     doloopstacksize;
	int     dolooplevel;
    int     CheckpointFlag;        /**< Tells preprocessor whether checkpoint code must executed.
                                        -1 : do recovery from snapshot, set by command line option;
                                        0 : do nothing; 1 : create snapshots, set by On checkpoint
                                        statement */
    int     SizeCommuteInSet;      /* Size of the CommuteInSet buffer */
#ifdef PARALLELCODE
    int     numpfirstnum;          /* For redefine */
    int     sizepfirstnum;         /* For redefine */
#endif
    int     origin;                /* Determines whether .sort or ModuleOption */
    int     vectorlikeLHS;
    WORD    argsumcheck[MAXNEST];  /* (C) Checking of nesting */
    WORD    insidesumcheck[MAXNEST];/* (C) Checking of nesting */
    WORD    inexprsumcheck[MAXNEST];/* (C) Checking of nesting */
    WORD    RepSumCheck[MAXREPEAT];/* (C) Checks nesting of repeat, if, argument */
    WORD    lUniTrace[4];          /* (C) */
    WORD    RepLevel;              /* (C) Tracks nesting of repeat. */
    WORD    arglevel;              /* (C) level of nested argument statements */
    WORD    insidelevel;           /* (C) level of nested inside statements */
    WORD    inexprlevel;           /* (C) level of nested inexpr statements */
    WORD    termlevel;             /* (C) level of nested term statements */
    WORD    MustTestTable;         /* (C) Indicates whether tables have been changed */
    WORD    DumNum;                /* (C) */
    WORD    ncmod;                 /* (C) Local setting of modulus. size of cmod */
    WORD    npowmod;               /* (C) Local printing as powers. size powmod */
    WORD    modmode;               /* (C) Mode for modulus calculus */
	WORD	nhalfmod;              /* relevant word size of halfmod when defined */
    WORD    DirtPow;               /* (C) Flag for changes in printing mod powers */
    WORD    lUnitTrace;            /* (C) Local value of Tr[1] */
    WORD    NwildC;                /* (C) Wildcard counter */
    WORD    ComDefer;              /* (C) defer brackets */
    WORD    CollectFun;            /* (C) Collect function number */
    WORD    AltCollectFun;         /* (C) Alternate Collect function number */
    WORD    OutputMode;            /* (C) */
    WORD    Cnumpows;
    WORD    OutputSpaces;          /* (C) */
    WORD    OutNumberType;         /* (C) Controls RATIONAL/FLOAT output */
    WORD    DidClean;              /* (C) Test whether nametree needs cleaning */
    WORD    IfLevel;               /* (C) */
    WORD    WhileLevel;            /* (C) */
    WORD    SwitchLevel;
    WORD    SwitchInArray;
    WORD    MaxSwitch;
    WORD    LogHandle;             /* (C) The Log File */
    WORD    LineLength;            /* (C) */
    WORD    StoreHandle;           /* (C) Handle of .str file */
    WORD    HideLevel;             /* (C) Hiding indicator */
    WORD    lPolyFun;              /* (C) local value of PolyFun */
    WORD    lPolyFunInv;           /* (C) local value of Inverse of PolyFun */
    WORD    lPolyFunType;          /* (C) local value of PolyFunType */
    WORD    lPolyFunExp;
    WORD    lPolyFunVar;
    WORD    lPolyFunPow;
    WORD    SymChangeFlag;         /* (C) */
    WORD    CollectPercentage;     /* (C) Collect function percentage */
    WORD    ShortStatsMax;         /* For  On FewerStatistics 10; */
	WORD	extrasymbols;          /* Flag for the extra symbsols output mode */
    WORD    PolyRatFunChanged;     /* Keeps track whether we changed in the compiler */
    WORD    ToBeInFactors;
    WORD    InnerTest;            /* For debugging */
#ifdef WITHMPI
    WORD    RhsExprInModuleFlag;   /* (C) Set by the compiler if RHS expressions exists. */
#endif
    UBYTE   Commercial[COMMERCIALSIZE+2]; /* (C) Message to be printed in statistics */
    UBYTE   debugFlags[MAXFLAGS+2];    /* On/Off Flag number(s) */
#if defined(WITHPTHREADS)
	PADPOSITION(49,8+3*MAXNEST,72,48+3*MAXNEST+MAXREPEAT,COMMERCIALSIZE+MAXFLAGS+4+sizeof(LIST)*17+sizeof(pthread_mutex_t));
#elif defined(WITHMPI)
	PADPOSITION(49,8+3*MAXNEST,72,49+3*MAXNEST+MAXREPEAT,COMMERCIALSIZE+MAXFLAGS+4+sizeof(LIST)*17);
#else
	PADPOSITION(47,8+3*MAXNEST,70,48+3*MAXNEST+MAXREPEAT,COMMERCIALSIZE+MAXFLAGS+4+sizeof(LIST)*17);
#endif
};
/*
 		#] C : 
 		#[ S : The S struct defines objects changed at the start of the run (Processor)
		       Basically only set by the master.
*/
/**
 *	The S_const struct is part of the global data and resides in the
 *	ALLGLOBALS struct A under the name S
 *	We see it used with the macro AS as in AS.ExecMode
 *	It has some variables used by the master in multithreaded runs
 */

struct S_const {
	POSITION MaxExprSize;          /* ( ) Maximum size of in/out/sort */
#ifdef WITHPTHREADS
	pthread_mutex_t	inputslock;
	pthread_mutex_t	outputslock;
	pthread_mutex_t	MaxExprSizeLock;
#endif
    POSITION *OldOnFile;           /* (S) File positions of expressions */
    WORD    *OldNumFactors;        /* ( ) NumFactors in (old) expression */
    WORD    *Oldvflags;            /* ( ) vflags in (old) expression */
    int     NumOldOnFile;          /* (S) Number of expressions in OldOnFile */
    int     NumOldNumFactors;      /* (S) Number of expressions in OldNumFactors */
    int     MultiThreaded;         /* (S) Are we running multi-threaded? */
#ifdef WITHPTHREADS
    int     MasterSort;            /* Final stage of sorting to the master */
#endif
#ifdef WITHMPI
    int     printflag;             /* controls MesPrint() on each slave */
#endif
    int     Balancing;             /* For second stage loadbalancing */
    WORD    ExecMode;              /* (S) */

    WORD    CollectOverFlag;       /* (R) Indicates overflow at Collect */
#ifdef WITHPTHREADS
	WORD	sLevel;                /* Copy of AR0.sLevel because it can get messy */
#endif
#if defined(WITHPTHREADS)
	PADPOSITION(3,0,5,3,sizeof(pthread_mutex_t)*3);
#elif defined(WITHMPI)
	PADPOSITION(3,0,5,2,0);
#else
	PADPOSITION(3,0,4,2,0);
#endif
};
/*
 		#] S : 
 		#[ R : The R struct defines objects changed at run time.
               They determine the environment that has to be transfered
               together with a term during multithreaded execution.
*/
/**
 *  The R_const struct is part of the global data and resides either in the
 *  ALLGLOBALS struct A, or the ALLPRIVATES struct B (TFORM) under the name R
 *  We see it used with the macro AR as in AR.infile
 *  It has the variables that define the running environment and that
 *  should be transferred with a term in a multithreaded run.
 */

struct R_const {
    FILEDATA    StoreData;         /* (O) */
    FILEHANDLE  Fscr[3];           /* (R) Dollars etc play with it too */
    FILEHANDLE  FoStage4[2];       /* (R) In Sort. Stage 4. */
    POSITION DefPosition;          /* (R) Deferred position of keep brackets. */
    FILEHANDLE *infile;            /* (R) Points alternatingly to Fscr[0] or Fscr[1] */
    FILEHANDLE *outfile;           /* (R) Points alternatingly to Fscr[1] or Fscr[0] */
    FILEHANDLE *hidefile;          /* (R) Points to Fscr[2] */

    WORD    *CompressBuffer;       /* (M) */
    WORD    *ComprTop;             /* (M) */
    WORD    *CompressPointer;      /* (R) */
    COMPARE CompareRoutine;
    ULONG   *wranfia;

    LONG    OldTime;               /* (R) Zero time. Needed in timer. */
    LONG    InInBuf;               /* (R) Characters in input buffer. Scratch files. */
    LONG    InHiBuf;               /* (R) Characters in hide buffer. Scratch file. */
    LONG    pWorkSize;             /* (R) Size of pWorkSpace */
    LONG    lWorkSize;             /* (R) Size of lWorkSpace */
    LONG    posWorkSize;           /* (R) Size of posWorkSpace */
	ULONG   wranfseed;
    int     NoCompress;            /* (R) Controls native compression */
    int     gzipCompress;          /* (R) Controls gzip compression */
    int     Cnumlhs;               /* Local copy of cbuf[rbufnum].numlhs */
	int     outtohide;             /* Indicates that output is directly to hide */
#ifdef WITHPTHREADS
    int     exprtodo;              /* The expression to do in parallel mode */
#endif
    int     wranfcall;
	int     wranfnpair1;
	int     wranfnpair2;
#if ( BITSINWORD == 32 )
    WORD    PrimeList[5000];
	WORD    numinprimelist;
    WORD    notfirstprime;
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
    WORD    PolyFunInv;            /* (C) Number of the Inverse of the PolyFun function */
    WORD    PolyFunType;           /* () value of PolyFunType */
    WORD    PolyFunExp;
    WORD    PolyFunVar;
    WORD    PolyFunPow;
    WORD    Eside;                 /* () Tells which side of = sign */
    WORD    MaxDum;                /* Maximum dummy value in an expression */
    WORD    level;                 /* Running level in Generator */
    WORD    expchanged;            /* (R) Info about expression */
    WORD    expflags;              /* (R) Info about expression */
    WORD    CurExpr;               /* (S) Number of current expression */
    WORD    SortType;              /* A copy of AC.SortType to play with */
    WORD    ShortSortCount;        /* For On FewerStatistics 10; */
#if ( BITSINWORD == 32 )
#ifdef WITHPTHREADS
	PADPOSITION(8,7,8,5026,0);
#else
	PADPOSITION(8,7,7,5026,0);
#endif
#else
#ifdef WITHPTHREADS
	PADPOSITION(8,7,8,24,0);
#else
	PADPOSITION(8,7,7,24,0);
#endif
#endif
};

/*
 		#] R : 
 		#[ T : These are variables that stay in each thread during multi threaded execution.
*/
/**
 *	The T_const struct is part of the global data and resides either in the
 *	ALLGLOBALS struct A, or the ALLPRIVATES struct B (TFORM) under the name T
 *	We see it used with the macro AT as in AT.WorkPointer
 *	It has variables that are private to each thread, most of which have
 *	to be defined at startup.
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
    STORECACHE  StoreCacheAlloc;   /* (R) Permanent address of StoreCache to keep valgrind happy */
    WORD    **pWorkSpace;          /* (R) Workspace for pointers. Dynamic. */
    LONG    *lWorkSpace;           /* (R) WorkSpace for LONG. Dynamic. */
    POSITION *posWorkSpace;        /* (R) WorkSpace for file positions */
    WORD    *WorkSpace;            /* (M) */
    WORD    *WorkTop;              /* (M) */
    WORD    *WorkPointer;          /* (R) Pointer in the WorkSpace heap. */
    int     *RepCount;             /* (M) Buffer for repeat nesting */
    int     *RepTop;               /* (M) Top of RepCount buffer */
    WORD    *WildArgTaken;         /* (N) Stack for wildcard pattern matching */
    UWORD   *factorials;           /* (T) buffer of factorials. Dynamic. */
  	WORD    *small_power_n;        /*     length of the number */
	  UWORD  **small_power;          /*     the number*/	
    UWORD   *bernoullis;           /* (T) The buffer with bernoulli numbers. Dynamic. */
	WORD    *primelist;
    LONG    *pfac;                 /* (T) array of positions of factorials. Dynamic. */
    LONG    *pBer;                 /* (T) array of positions of Bernoulli's. Dynamic. */
    WORD    *TMaddr;               /* (R) buffer for TestSub */
    WORD    *WildMask;             /* (N) Wildcard info during pattern matching */
    WORD    *previousEfactor;      /* () Cache for factors in expressions */
    WORD    **TermMemHeap;        /* For TermMalloc. Set zero in Checkpoint */
    UWORD    **NumberMemHeap;      /* For NumberMalloc. Set zero in Checkpoint */
    UWORD    **CacheNumberMemHeap; /* For CacheNumberMalloc. Set zero in Checkpoint */
	BRACKETINFO *bracketinfo;
    WORD    **ListPoly;
    WORD    *ListSymbols;
    UWORD   *NumMem;
    WORD    *TopologiesTerm;
    WORD    *TopologiesStart;
    PARTI   partitions;
    LONG    sBer;                  /* (T) Size of the bernoullis buffer */
    LONG    pWorkPointer;          /* (R) Offset-pointer in pWorkSpace */
    LONG    lWorkPointer;          /* (R) Offset-pointer in lWorkSpace */
    LONG    posWorkPointer;        /* (R) Offset-pointer in posWorkSpace */
    LONG    InNumMem;
    int     sfact;                 /* (T) size of the factorials buffer */
    int     mfac;                  /* (T) size of the pfac array. */
    int     ebufnum;               /* (R) extra compiler buffer */
    int     fbufnum;               /* extra compiler buffer for factorization cache */
    int     allbufnum;             /* extra compiler buffer for id,all */
    int     aebufnum;              /* extra compiler buffer for id,all */
    int     idallflag;             /* indicates use of id,all buffers */
    int     idallnum;
    int     idallmaxnum;
    int     WildcardBufferSize;    /* () local copy for updates */
#ifdef WITHPTHREADS
    int     identity;              /* () When we work with B->T */
    int     LoadBalancing;         /* Needed for synchronization */
#ifdef WITHSORTBOTS
    int     SortBotIn1;            /* Input stream 1 for a SortBot */
    int     SortBotIn2;            /* Input stream 2 for a SortBot */
#endif
#endif
    int     TermMemMax;            /* For TermMalloc. Set zero in Checkpoint */
    int     TermMemTop;            /* For TermMalloc. Set zero in Checkpoint */
    int     NumberMemMax;          /* For NumberMalloc. Set zero in Checkpoint */
    int     NumberMemTop;          /* For NumberMalloc. Set zero in Checkpoint */
    int     CacheNumberMemMax;     /* For CacheNumberMalloc. Set zero in Checkpoint */
    int     CacheNumberMemTop;     /* For CacheNumberMalloc. Set zero in Checkpoint */
    int     bracketindexflag;      /* Are brackets going to be indexed? */
    int     optimtimes;            /* Number of the evaluation of the MCTS tree */
    int     ListSymbolsSize;
    int     NumListSymbols;
    int     numpoly;
    int     LeaveNegative;
    int     TrimPower;             /* Indicates trimming in polyratfun expansion */
    WORD    small_power_maxx;      /*     size of the cache for small powers  */
    WORD    small_power_maxn;      /*     size of the cache for small powers */
    WORD    dummysubexp[SUBEXPSIZE+4]; /* () used in normal.c */
    WORD    comsym[8];             /* () Used in tools.c = {8,SYMBOL,4,0,1,1,1,3} */
    WORD    comnum[4];             /* () Used in tools.c = { 4,1,1,3 } */
    WORD    comfun[FUNHEAD+4];     /* () Used in tools.c = {7,FUNCTION,3,0,1,1,3} */
                                   /*            or { 8,FUNCTION,4,0,0,1,1,3 } */
    WORD    comind[7];             /* () Used in tools.c = {7,INDEX,3,0,1,1,3} */
    WORD    MinVecArg[7+ARGHEAD];  /* (N) but should be more local */
    WORD    FunArg[4+ARGHEAD+FUNHEAD]; /* (N) but can be more local */
    WORD    locwildvalue[SUBEXPSIZE]; /* () Used in argument.c = {SUBEXPRESSION,SUBEXPSIZE,0,0,0} */
    WORD    mulpat[SUBEXPSIZE+5];  /* () Used in argument.c = {TYPEMULT, SUBEXPSIZE+3, 0, */
                                   /* SUBEXPRESSION, SUBEXPSIZE, 0, 1, 0, 0, 0 } */
    WORD    proexp[SUBEXPSIZE+5];  /* () Used in poly.c */
    WORD    TMout[40];             /* (R) Passing info */
    WORD    TMbuff;                /* (R) Communication between TestSub and Genera */
	WORD	TMdolfac;              /* factor number for dollar */
    WORD    nfac;                  /* (T) Number of highest stored factorial */
    WORD    nBer;                  /* (T) Number of highest bernoulli number. */
    WORD    mBer;                  /* (T) Size of buffer pBer. */
    WORD    PolyAct;               /* (R) Used for putting the PolyFun at end. ini at 0 */
    WORD    RecFlag;               /* (R) Used in TestSub. ini at zero. */
	WORD    inprimelist;
	WORD    sizeprimelist;
    WORD    fromindex;             /* Tells the compare routine whether call from index */
    WORD    setinterntopo;         /* Set of internal momenta for topogen */
    WORD    setexterntopo;         /* Set of external momenta for topogen */
    WORD    TopologiesLevel;
    WORD    TopologiesOptions[2];
#ifdef WITHPTHREADS
#ifdef WITHSORTBOTS
	PADPOINTER(5,27,105+SUBEXPSIZE*4+FUNHEAD*2+ARGHEAD*2,0);
#else
	PADPOINTER(5,25,105+SUBEXPSIZE*4+FUNHEAD*2+ARGHEAD*2,0);
#endif
#else
	PADPOINTER(5,23,105+SUBEXPSIZE*4+FUNHEAD*2+ARGHEAD*2,0);
#endif
};
/*
 		#] T : 
 		#[ N : The N struct contains variables used in running information
               that is inside blocks that should not be split, like pattern
               matching, traces etc. They are local for each thread.
               They don't need initializations.
*/
/**
 *	The N_const struct is part of the global data and resides either in the
 *	ALLGLOBALS struct A, or the ALLPRIVATES struct B (TFORM) under the name N
 *	We see it used with the macro AN as in AN.RepFunNum
 *	It has variables that are private to each thread and are used as
 *	temporary storage during the expansion of the terms tree.
 */

struct N_const {
    POSITION OldPosIn;             /* (R) Used in sort. */
    POSITION OldPosOut;            /* (R) Used in sort */
	POSITION theposition;          /* () Used in index.c */
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
    WORD    *terfirstcomm;         /* (N) Used in pattern matching */
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
#ifdef WITHZLIB
	Bytef	**ziobufnum;           /* () Used in compress.c */
	Bytef	*ziobuffers;           /* () Used in compress.c */
#endif
	WORD	*dummyrenumlist;       /* () Used in execute.c and store.c */
	int		*funargs;              /* () Used in lus.c */
	WORD	**funlocs;             /* () Used in lus.c */
	int		*funinds;              /* () Used in lus.c */
	UWORD	*NoScrat2;             /* () Used in normal.c */
	WORD	*ReplaceScrat;         /* () Used in normal.c */
	TRACES	*tracestack;           /* () used in opera.c */
	WORD	*selecttermundo;       /* () Used in pattern.c */
	WORD	*patternbuffer;        /* () Used in pattern.c */
	WORD	*termbuffer;           /* () Used in pattern.c */
	WORD	**PoinScratch;         /* () used in reshuf.c */
	WORD	**FunScratch;          /* () used in reshuf.c */
	WORD	*RenumScratch;         /* () used in reshuf.c */
	FUN_INFO *FunInfo;             /* () Used in smart.c */
	WORD	**SplitScratch;        /* () Used in sort.c */
	WORD	**SplitScratch1;       /* () Used in sort.c */
	SORTING **FunSorts;            /* () Used in sort.c */
	UWORD	*SoScratC;             /* () Used in sort.c */
	WORD	*listinprint;          /* () Used in proces.c and message.c */
	WORD	*currentTerm;          /* () Used in proces.c and message.c */
	WORD	**arglist;             /* () Used in function.c */
	int		*tlistbuf;             /* () used in lus.c */
#ifdef WHICHSUBEXPRESSION
	UWORD	*BinoScrat;            /* () Used in proces.c */
#endif
	WORD	*compressSpace;        /* () Used in sort.c */
#ifdef WITHPTHREADS
	THREADBUCKET *threadbuck;
	EXPRESSIONS expr;
#endif
	UWORD	*SHcombi;
	WORD    *poly_vars;
    UWORD   *cmod;                 /* Local setting of modulus. Pointer to value. */
	SHvariables SHvar;
	LONG	deferskipped;          /* () Used in proces.c store.c and parallel.c */
	LONG	InScratch;             /* () Used in sort.c */
	LONG	SplitScratchSize;      /* () Used in sort.c */
	LONG	InScratch1;            /* () Used in sort.c */
	LONG	SplitScratchSize1;     /* () Used in sort.c */
	LONG	ninterms;              /* () Used in proces.c and sort.c */
#ifdef WITHPTHREADS
	LONG	inputnumber;           /* () For use in redefine */
	LONG	lastinindex;
#endif
#ifdef WHICHSUBEXPRESSION
	LONG	last2;                 /* () Used in proces.c */
	LONG	last3;                 /* () Used in proces.c */
#endif
	LONG	SHcombisize;
    int     NumTotWildArgs;        /* (N) Used in pattern matching */
    int     UseFindOnly;           /* (N) Controls pattern routines */
    int     UsedOtherFind;         /* (N) Controls pattern routines */
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
    int     nogroundlevel;         /* () Used to see whether pattern matching at groundlevel */
	int		subsubveto;            /* () Sabotage combining subexpressions in TestSub */
	WORD	MaxRenumScratch;       /* () used in reshuf.c */
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
    WORD    PolyFunTodo;           /* deals with expansions and multiplications */
    WORD    sizeselecttermundo;    /* () Used in pattern.c */
    WORD    patternbuffersize;     /* () Used in pattern.c */
    WORD    numlistinprint;        /* () Used in process.c */
    WORD    ncmod;                 /* () used as some type of flag to disable */
    WORD    ExpectedSign;          /** Used in pattern matching of antisymmetric functions */
    WORD    SignCheck;             /** Used in pattern matching of antisymmetric functions */
    WORD    IndDum;                /* Active dummy indices */
    WORD    poly_num_vars;
    WORD    idfunctionflag;
    WORD    poly_vars_type;        /* type of allocation. For free. */
    WORD    tryterm;               /* For EndSort(...,2) */
#ifdef WHICHSUBEXPRESSION
	WORD	nbino;                 /* () Used in proces.c */
	WORD	last1;                 /* () Used in proces.c */
#endif
#ifdef WITHPTHREADS
#ifdef WHICHSUBEXPRESSION
#ifdef WITHZLIB
	PADPOSITION(55,11,23,28,sizeof(SHvariables));
#else
	PADPOSITION(53,11,23,28,sizeof(SHvariables));
#endif
#else
#ifdef WITHZLIB
	PADPOSITION(54,9,23,26,sizeof(SHvariables));
#else
	PADPOSITION(52,9,23,26,sizeof(SHvariables));
#endif
#endif
#else
#ifdef WHICHSUBEXPRESSION
#ifdef WITHZLIB
	PADPOSITION(53,9,23,28,sizeof(SHvariables));
#else
	PADPOSITION(51,9,23,28,sizeof(SHvariables));
#endif
#else
#ifdef WITHZLIB
	PADPOSITION(52,7,23,26,sizeof(SHvariables));
#else
	PADPOSITION(50,7,23,26,sizeof(SHvariables));
#endif
#endif
#endif
};

/*
 		#] N : 
 		#[ O : The O struct concerns output variables
*/
/**
 *	The O_const struct is part of the global data and resides in the
 *	ALLGLOBALS struct A under the name O
 *	We see it used with the macro AO as in AO.OutputLine
 *	It contains variables that involve the writing of text output and
 *	save/store files.
 */

struct O_const {
    FILEDATA    SaveData;          /* (O) */
    STOREHEADER SaveHeader;        /* ()  System Independent save-Files */
	OPTIMIZERESULT OptimizeResult;
    UBYTE   *OutputLine;           /* (O) Sits also in debug statements */
    UBYTE   *OutStop;              /* (O) Top of OutputLine buffer */
    UBYTE   *OutFill;              /* (O) Filling point in OutputLine buffer */
    WORD    *bracket;              /* (O) For writing brackets */
    WORD    *termbuf;              /* (O) For writing terms */
    WORD    *tabstring;
    UBYTE   *wpos;                 /* (O) Only when storing file {local?} */
    UBYTE   *wpoin;                /* (O) Only when storing file {local?} */
    UBYTE   *DollarOutBuffer;      /* (O) Outputbuffer for Dollars */
    UBYTE   *CurBufWrt;            /* (O) Name of currently written expr. */
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
	DICTIONARY **Dictionaries;
	UBYTE	*tensorList;           /* Dynamically allocated list with functions that are tensorial. */
    WORD    *inscheme;             /* for feeding a Horner scheme to Optimize */
/*----Leave NumInBrack as first non-pointer. This is used by the checkpoints--*/
    LONG    NumInBrack;            /* (O) For typing [] option in print */
    LONG    wlen;                  /* (O) Used to store files. */
    LONG    DollarOutSizeBuffer;   /* (O) Size of DollarOutBuffer */
    LONG    DollarInOutBuffer;     /* (O) Characters in DollarOutBuffer */
#if defined(mBSD) && defined(MICROTIME)
    LONG    wrap;                  /* (O) For statistics time. wrap around */
    LONG    wrapnum;               /* (O) For statistics time. wrap around */
#endif
    OPTIMIZE Optimize;          
    int     OutInBuffer;           /* (O) Which routine does the writing */
    int     NoSpacesInNumbers;     /*     For very long numbers */
    int     BlockSpaces;           /*     For very long numbers */
    int     CurrentDictionary;
    int     SizeDictionaries;
    int     NumDictionaries;
    int     CurDictNumbers;
    int     CurDictVariables;
    int     CurDictSpecials;
    int     CurDictFunWithArgs;
    int     CurDictNumberWarning;
    int     CurDictNotInFunctions;
    int     CurDictInDollars;
    int     gNumDictionaries;
    WORD    schemenum;             /* for feeding a Horner scheme to Optimize */
    WORD    transFlag;             /* ()  >0 indicades that translations have to be done */
    WORD    powerFlag;             /* ()  >0 indicades that some exponents/powers had to be adjusted */
    WORD    mpower;                /*     For maxpower adjustment to larger value */
    WORD    resizeFlag;            /* ()  >0 indicades that something went wrong when resizing words */
    WORD    bufferedInd;           /* ()  Contains extra INDEXENTRIES, see ReadSaveIndex() for an explanation */
    WORD    OutSkip;               /* (O) How many chars to skip in output line */
    WORD    IsBracket;             /* (O) Controls brackets */
    WORD    InFbrack;              /* (O) For writing only */
    WORD    PrintType;             /* (O) */
    WORD    FortFirst;             /* (O) Only in sch.c */
    WORD    DoubleFlag;            /* (O) Output in double precision */
    WORD    IndentSpace;           /* For indentation in output */
    WORD    FactorMode;            /* When the output should be written as factors */
    WORD    FactorNum;             /* Number of factor currently treated */
    WORD    ErrorBlock;
    WORD    OptimizationLevel;     /* Level of optimization in the output */
    UBYTE   FortDotChar;           /* (O) */
/*
	For the padding, please count also the number of int's in the OPTIMIZE struct.
*/
#if defined(mBSD) && defined(MICROTIME)
	PADPOSITION(25,6,35,17,1);
#else
	PADPOSITION(25,4,35,17,1);
#endif
};
/*
 		#] O : 
 		#[ X : The X struct contains variables that deal with the external channel
*/
/**
 *	The X_const struct is part of the global data and resides in the
 *	ALLGLOBALS struct A under the name X
 *	We see it used with the macro AX as in AX.timeout
 *	It contains variables that involve communication with external programs
 */

struct X_const {
	UBYTE *currentPrompt;
	UBYTE *shellname;          /* if !=NULL (default is "/bin/sh -c"), start in 
	                              the specified subshell*/
	UBYTE *stderrname;         /* If !=NULL (default if "/dev/null"), stderr is 
	                              redirected to the specified file*/
	int timeout;               /* timeout to initialize preset channels.
	                              If timeout<0, the preset channels are 
	                              already initialized*/
	int killSignal;            /* signal number, SIGKILL by default*/
	int killWholeGroup;        /* if 0, the signal is sent only to a process, 
	                              if !=0 (default) is sent to a whole process group*/
	int daemonize;             /* if !=0 (default), start in a daemon mode */
	int	currentExternalChannel;
	PADPOINTER(0,5,0,0);
};
/*
 		#] X : 
 		#[ Definitions :
*/

#ifdef WITHPTHREADS

/**
 *	With pthreads (TFORM) the ALLGLOBALS struct has all the variables of which
 *	there is only a single copy.
 */

typedef struct AllGlobals {
    struct M_const M;
    struct C_const C;
    struct S_const S;
    struct O_const O;
    struct P_const P;
	struct X_const X;
	PADPOSITION(0,0,0,0,sizeof(struct P_const)+sizeof(struct X_const));
} ALLGLOBALS;

/**
 *	With pthreads (TFORM) the ALLPRIVATES struct has all the variables of which
 *	each thread must have its own (private) copy.
 */

typedef struct AllPrivates {
    struct R_const R;
    struct N_const N;
    struct T_const T;
	PADPOSITION(0,0,0,0,sizeof(struct T_const));
} ALLPRIVATES;

#else

/**
 *	Without pthreads (FORM) the ALLGLOBALS struct has all the global variables
 */

typedef struct AllGlobals {
    struct M_const M;
    struct C_const C;
    struct S_const S;
    struct R_const R;
    struct N_const N;
    struct O_const O;
    struct P_const P;
    struct T_const T;
	struct X_const X;
	PADPOSITION(0,0,0,0,sizeof(struct P_const)+sizeof(struct T_const)+sizeof(struct X_const));
} ALLGLOBALS;

#endif

/*
 		#] Definitions : 
    #] A : 
  	#[ FG :
*/

#ifdef WITHPTHREADS
#define PHEAD  ALLPRIVATES *B,
#define PHEAD0 ALLPRIVATES *B
#define BHEAD  B,
#define BHEAD0 B
#else
#define PHEAD
#define PHEAD0 VOID
#define BHEAD
#define BHEAD0
#endif

#ifdef ANSI
typedef WORD (*WCN)(PHEAD WORD *,WORD *,WORD,WORD);
typedef WORD (*WCN2)(PHEAD WORD *,WORD *);
#else
typedef WORD (*WCN)();
typedef WORD (*WCN2)();
#endif

/**
 *	The FIXEDGLOBALS struct is an anachronism. It started as the struct
 *	with global variables that needed initialization.
 *	It contains the elements Operation and OperaFind which define a very early
 *	way of automatically jumping to the proper operation. We find the results
 *	of it in parts of the file opera.c
 *	Later operations were treated differently in a more transparent way.
 *	We never changed the existing code. The most important part is currently
 *	the cTable which is used intensively in the compiler.
 */

typedef struct FixedGlobals {
	WCN		Operation[8];
	WCN2	OperaFind[6];
	char	*VarType[10];
	char	*ExprStat[21];
	char	*FunNam[2];
	char	*swmes[3];
	char	*fname;
	char	*fname2;
	UBYTE	*s_one;
	WORD	fnamebase;
	WORD	fname2base;
	UINT	cTable[256];
} FIXEDGLOBALS;

/*
  	#] FG : 
*/

#endif
