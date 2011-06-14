; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;;;;;;;
; SINK constructor ;
;;;;;;;;;;;;;;;;;;;;
;
; This file constructs SINK, by loading all the other defining files in
; an acceptable order, and displays a short introductory message about SINK.
;
; This file is not meant to be loaded directly by the user; two files are
; provided for that purpose in the SINK program's home directory (the parent of
; the directory where the current file, "all.scm", should reside).  Users who
; want to work from the Scheme interactive level should use file "sink.scm";
; those who want to bypass the Scheme interactive level entirely (and who have
; MzScheme) should use a batch file such as "sink.bat".
;

(load "revision.scm")      ; mechanism for tracking revision and date
(load "eval.scm")          ; evaluator central logic
(load "interpreter.scm")   ; interpreter top level
(load "boolean.scm")       ; booleans
(load "object.scm")        ; encapsulated objects
(load "applicative.scm")   ;   applicatives              (uses object)
(load "context.scm")       ;   continuations             (uses object)
(load "encapsulation.scm") ;   encapsulation types       (uses object)
(load "environment.scm")   ;   environments              (uses object)
(load "error.scm")         ;   error descriptors         (uses object)
(load "ignore.scm")        ;   ignore                    (uses object)
(load "inert.scm")         ;   inert                     (uses object)
(load "kernel-pair.scm")   ;   pairs                     (uses object)
(load "keyed.scm")         ;   keyed variables           (uses object)
(load "number.scm")        ;   number                    (uses object)
(load "operative.scm")     ;   operatives                (uses object)
(load "port.scm")          ;   ports             (uses dynamic,object)
(load "cycles.scm")        ; cyclic structures
(load "ground.scm")        ; ground environment (uses everything else)

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
