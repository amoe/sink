
These source files collectively form a Scheme program that interprets a
language that is almost, but not quite, Kernel.  The program is called
SINK, which is an acronym for Scheme-based Interpreter for Not-quite Kernel.

==== EDIT: ====

This version was ported to R6RS by David Banks <amoebae@gmail.com> in 2011.  I
have left the original README intact below.  Please report any bugs with this
version to me, not John Shutt!

There are an unknown amount of bugs.  Most of the tests seem to pass.  This
is very much only for those who want to mess around and just evaluate a few
expressions.

In this version the top level REPL is 'script.sps'.
This seems to work on the following implementations (and by "work", I mean that
the REPL loads):

  Ikarus r1870 (bzr)
    $ ikarus --r6rs-script script.sps
    Should dump you into the REPL.       

Racket 5.1.1, 6.7:

    $ PLTCOLLECTS=$(pwd): plt-r6rs script.sps

Should dump you into the REPL.  `PLTCOLLECTS` is needed to get Racket to look
in the current directory for libraries.

  Guile 2.0.0.196-0c81a0
    $ guile -L . -x .sls script.sps

  Ypsilon
    $ ypsilon --sitelib . script.sps
  Has the same REPL bug as Racket

What does not work:

  Mosh 0.2.7
    Has a debatable bug in READ.
    <http://groups.google.com/group/mosh-developer-discuss/browse_thread/thread/1c39756f4a2b00ae>

Patches welcome :)

==== END EDIT ====

The interpreter is meant to have been written in unextended R5RS Scheme.
Incidental file "subfiles/script.scm", which is not part of the interpreter
proper, uses extensions in MzScheme.


Copyright

  This program was designed and written by John N. Shutt, copyright 2009,
  and is distributed under the GNU General Public License version 2 or later.
  Each individual source file begins with a two-line notice:

   ; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
   ; Copyright (c) 2009 John N. Shutt

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public License
  as published by the Free Software Foundation; either version 2 of
  the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

Platform

  Because the program is written in R5RS Scheme, its Kernel-error-handling
  facilities rely on dynamic-wind to catch errors occurring in native Scheme
  procedures.  Unfortunately, this behavior is not actually spelled out in the
  R5RS, and some implementors of R5RS Scheme have seen fit not to integrate
  dynamic-wind gracefully with their error handling.  Consequently, under some
  implementations of R5RS Scheme an error in a primitive (such as dividing by
  zero) will cause the entire SINK interpreter to abend.  The program was
  developed under MzScheme, where dynamic-wind catches errors as expected (or
  at least it did, as of version 103 of MzScheme that was used during
  development).

Files in the home directory

  readme

    This file.

  rev-log

    A summary of the revision history of the program.

  sink.scm

    The primary source file for running SINK under Scheme interactive mode.
    Loading this file creates the interpreter by loading all secondary source
    files in the proper sequence, and outputs a startup message identifying
    the program and giving instructions on how to run it.

  sink.bat

    An MS-DOS batch file for running SINK directly under the MS-DOS prompt,
    bypassing Scheme interactive mode.

  subfiles/

    Directory containing all the secondary source files.

  test/

    Directory containing miscellaneous (not-quite-)Kernel source files for
    testing.  Source files that are pure Kernel have file type ".krn", those
    that are not (because they use "%" instead of "#") have file type ".snk".

  copyleft.gnu

    The GNU General Public License.
