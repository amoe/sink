@echo off

rem  This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
rem  Copyright (c) 2009 John N. Shutt
rem
rem This is the main file for using SINK from the MS-DOS system prompt,
rem bypassing the Scheme interactive level.  It assumes MzScheme.
rem If there is a system environment variable SINKDIR, it should contain the
rem path to the SINK home directory, and this batch file can then be called
rem from anywhere.  If there is no such variable, this batch file must be
rem called from the SINK home directory.
rem
rem The check for the system environment variable, and changes of current
rem directory, are handled by file "subfiles/script.scm" using features of
rem MzScheme; on other sensitivity of SINK to choice of Scheme implementation,
rem see file "subfiles/operative.scm" procedure naive->action.

mzscheme -r "%SINKDIR%subfiles\\script.scm"
