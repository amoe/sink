; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;;;;;;
; Scheme-based    ;
; Interpreter for ;
; Not-quite       ;
; Kernel          ;
;;;;;;;;;;;;;;;;;;;
;
; This is the main file for using SINK in Scheme interactive mode.
; It constructs SINK and displays instructions on how to run SINK.
; The programmer can then run SINK as many times as desired, and whenever
; SINK terminates the programmer is returned to the Scheme prompt.
;
; Using MzScheme, to load SINK from the SINK home directory, type either
;
;     mzscheme
;     (load "sink.scm")
; or
;     mzscheme -f sink.scm
;
; SINK was developed under MzScheme version 103.  Although the program is
; mostly limited to R5RS Scheme, some details of Kernel error handling won't
; work on some technically R5RS-compliant platforms (on which, see file
; "subfiles/operative.scm" procedure naive->action).
;

(load "subfiles/all.scm")

(display "    Start by calling interpreter with no arguments, thus:")(newline)
(newline)
(display "        (interpreter)")                                    (newline)
(newline)

'SINK-constructed
