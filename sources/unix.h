/** @file unix.h
 *
 *  Contains the definition of constants connected to the byte structure
 *	of the computer. Like WORD size etc. Used mainly on unix-like systems.
 */

/* #[ License : */
/*
 *   Copyright (C) 1984-2010 J.A.M. Vermaseren
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

/*[13jul2005 mt]:*/
/*With SAFESIGNAL defined, write() and read() syscalls are wrapped by
the errno checkup*/
/*#define SAFESIGNAL*/
/*:[13jul2005 mt]*/

/*[29apr2004 mt]:*/
#define WITHEXTERNALCHANNEL
#define TRAPSIGNALS
/*:[29apr2004 mt]*/

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
#ifdef OPTERON
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
#define PADLONG(in,wo,by) UBYTE d_u_m_m_y[8-((4*(in)+4*(wo)+(by))&7)]
#define PADINT(wo,by) UBYTE d_u_m_m_y[4-((4*(wo)+(by))&3)]
#define PADWORD(by) UBYTE d_u_m_m_y[4-((by)&1)]
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
#endif

#define MAXPOSITIVE2 (MAXPOSITIVE/2)
