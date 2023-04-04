/** @file unix.h
 *
 *  Settings for Unix-like systems.
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

#define LINEFEED '\n'
#define CARRIAGERETURN 0x0D

#define WITHPIPE
#define WITHSYSTEM

/*[13jul2005 mt]:*/
/*With SAFESIGNAL defined, write() and read() syscalls are wrapped by
the errno checkup*/
/*#define SAFESIGNAL*/
/*:[13jul2005 mt]*/

/*[29apr2004 mt]:*/
#define WITHEXTERNALCHANNEL
/*
*/
#define TRAPSIGNALS
/*:[29apr2004 mt]*/

#define P_term(code)    exit((int)(code<0?-code:code))

#define SEPARATOR '/'
#define ALTSEPARATOR '/'
#define PATHSEPARATOR ':'
#define WITH_ENV
