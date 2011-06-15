; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

;;;;;;;;;;
; ignore ;
;;;;;;;;;;
;
; The ignore value has type 'ignore and no attributes.
;

(define ignore (let ((name  (list #f)))
                 (lambda (message)
                   (case message
                     ((type) 'ignore)
                     ((name) name)))))

(define ignore? (make-object-type-predicate 'ignore))
