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

(let ((sinkdir  (getenv "sinkdir")))
  (if (eq? sinkdir #f)
      (load "subfiles/all.scm")
      (let ((saved-path  (current-directory)))
        (current-directory sinkdir)
        (load "subfiles/all.scm")
        (current-directory saved-path))))

(newline)
(display "Entering SINK.")
(newline)
(newline)
(newline)

(interpreter)

(newline)
(display "SINK terminated.")
(newline)
