; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;;;
; batch script ;
;;;;;;;;;;;;;;;;
;
; This file is a script that constructs SINK, runs it, and displays a sign-off
; message.  It is meant to be used by a batch file, such as the MS-DOS batch
; file "sink.bat".  If system environment variable SINKDIR is defined, it
; should be the path of the SINK home directory; this file saves the current
; directory, moves to the SINK home directory to load the SINK system, then
; moves back to the saved directory.  If system environment variable SINKDIR
; is undefined, this file assumes that the SINK home directory is the current
; directory.
;
; The language facilities used to access the system environment and current
; directory path are peculiar to MzScheme.  On sensitivity of SINK proper
; (distinct from this batch script) to choice of Scheme implementation, see
; file "subfiles/operative.scm" procedure naive->action.
;


(import (rnrs)
        (subfiles interpreter)
        (subfiles revision))

(newline)
(display "  SINK:  Scheme-based Interpreter for Not-quite Kernel.")   (newline)
(display "  (An interpreter for an approximate subset of Kernel.)")   (newline)
(newline)
(display "  Version ") (display (get-version)) (newline)
(display "  ") (display (get-revision-date)) (display ".") (newline)
(display "  Based on the R-1RK.") (newline)
(newline)
(display "    Use %ignore and %inert for Kernel #ignore and #inert.") (newline)
(display "    Major omissions: ")
(display      " complex numbers; bounds on inexact reals.")           (newline)
(display "    Various standard combiners haven't been defined.")
(newline)

(newline)
(display "Entering SINK.")
(newline)
(newline)
(newline)

(interpreter)

(newline)
(display "SINK terminated.")
(newline)
