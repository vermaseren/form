
#define LINEFEED '\n'
#define CARRIAGERETURN 0x0D
#define WITHRETURN

#define WITHSYSTEM

#define P_term(code)    exit((int)(code<0?-code:code))

#define SEPARATOR '\\'
#define ALTSEPARATOR '/'
#define WITH_ENV

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


