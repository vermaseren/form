
#define LINEFEED '\n'
#define CARRIAGERETURN 0x0D

#include <time.h>
#include <fcntl.h>
#include <sys/file.h>

#ifdef LINUX
#include <unistd.h>
#endif

#define WITHPIPE
#define WITHSYSTEM

#define P_term(code)    exit((int)(code<0?-code:code))

#define SEPARATOR '/'
#define ALTSEPARATOR '/'
#define WITH_ENV

#ifdef ALPHA
#define BITSINWORD 32
#define BITSINLONG 64
#define TOPBITONLY 0x080000000L
#define TOPLONGBITONLY 0x8000000000000000L
#define SPECMASK 0x80000000
#define WILDMASK 0x40000000
#define WORDMASK 0x0FFFFFFFFL
#define MAXPOSITIVE 0x07FFFFFFFL
#define FULLMAX  0x100000000L
#define AWORDMASK 0xFFFFFFFF00000000L
#define MAXLONG 0x7FFFFFFFFFFFFFFFL
#define PADPOINTER(lo,in,wo,by) UBYTE d_u_m_m_y[8-((8*(lo)+4*(in)+4*(wo)+(by))&7)]
#define PADLONG(in,wo,by) UBYTE d_u_m_m_y[8-((4*(wo)+(by))&7)]
#define PADINT(wo,by) UBYTE d_u_m_m_y[4-((4*(wo)+(by))&3)]
#define PADWORD(by) UBYTE d_u_m_m_y[4-((by)&1)]
/*
#define BITSINWORD 16
#define BITSINLONG 32
#define TOPBITONLY 0x08000L
#define TOPLONGBITONLY 0x80000000L
#define SPECMASK 0x8000
#define WILDMASK 0x4000
#define WORDMASK 0x0FFFFL
#define MAXPOSITIVE 0x07FFFL
#define FULLMAX  0x10000L
#define AWORDMASK 0xFFFF0000L
#define MAXLONG 0x7FFFFFFFL
#define PADPOINTER(lo,in,wo,by) UBYTE d_u_m_m_y[8-((4*(lo)+4*(in)+2*(wo)+(by))&7)]
#define PADLONG(in,wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADINT(wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADWORD(by) UBYTE d_u_m_m_y[2-((by)&1)]
*/
#else
#define BITSINWORD 16
#define BITSINLONG 32
#define TOPBITONLY 0x08000L
#define TOPLONGBITONLY 0x80000000L
#define SPECMASK 0x8000
#define WILDMASK 0x4000
#define WORDMASK 0x0FFFFL
#define MAXPOSITIVE 0x07FFFL
#define FULLMAX  0x10000L
#define AWORDMASK 0xFFFF0000L
#define MAXLONG 0x7FFFFFFFL
#define PADPOINTER(lo,in,wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADLONG(in,wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADINT(wo,by) UBYTE d_u_m_m_y[4-((2*(wo)+(by))&3)]
#define PADWORD(by) UBYTE d_u_m_m_y[2-((by)&1)]
#endif
