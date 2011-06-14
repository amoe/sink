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

(load "subfiles/revision.scm")      ; mechanism for tracking revision and date
(load "subfiles/eval.scm")          ; evaluator central logic
(load "subfiles/interpreter.scm")   ; interpreter top level
(load "subfiles/boolean.scm")       ; booleans
(load "subfiles/object.scm")        ; encapsulated objects
(load "subfiles/applicative.scm")   ;   applicatives              (uses object)
(load "subfiles/context.scm")       ;   continuations             (uses object)
(load "subfiles/encapsulation.scm") ;   encapsulation types       (uses object)
(load "subfiles/environment.scm")   ;   environments              (uses object)
(load "subfiles/error.scm")         ;   error descriptors         (uses object)
(load "subfiles/ignore.scm")        ;   ignore                    (uses object)
(load "subfiles/inert.scm")         ;   inert                     (uses object)
(load "subfiles/kernel-pair.scm")   ;   pairs                     (uses object)
(load "subfiles/keyed.scm")         ;   keyed variables           (uses object)
(load "subfiles/number.scm")        ;   number                    (uses object)
(load "subfiles/operative.scm")     ;   operatives                (uses object)
(load "subfiles/port.scm")          ;   ports             (uses dynamic,object)
(load "subfiles/cycles.scm")        ; cyclic structures
(load "subfiles/ground.scm")        ; ground environment (uses everything else)

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
