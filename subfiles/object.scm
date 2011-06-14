; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

;;;;;;;;;;;
; objects ;
;;;;;;;;;;;
;
; Encapsulated objects in the interpreted language are represented by
; procedures.  The representing procedure takes a symbol as argument, and
; returns one of several fields.  Internally, it's a dispatch procedure.
;
; The point of the exercise is to achieve guaranteed encapsulation of these
; objects *in the interpreted language*.  The interpreter source code is
; supposed to follow stylistic rules, which can be checked because the
; interpreter source code is finite, and it is then technically impossible
; for a Kernel program to violate the encapsulation of the objects.  The
; essential point is that a Kernel program cannot directly require that
; a procedure be called (procedure being a type in the Scheme meta-language,
; not in the interpreted Kernel language), and therefore the Kernel program
; is limited in its manipulations of encapsulated objects to those specific
; manipulations for which Kernel tools are provided.
;
; Every object has two fields, 'type and 'name.
;   'type is a symbol that names the object's type.
;   'name is a list, whose car is either #f, meaning the object is unnameable;
; or #t, meaning the object can be named but hasn't been yet; or a symbol or
; string, which is then the object's name.  The cdr of the list is initially
; nil, but could later be altered via designate-origin, which see.
; 
;
; For example, here's how one would idiomatically set up an object type 'foo
; that is nameable and has attributes 'bar and 'quux.
;
;     (define make-foo
;       (lambda (bar quux)
;         (let ((name  (list #t)))
;           (lambda (message)
;             (case message
;               ((type) 'foo)
;               ((name) name)
;               ((bar)  bar)
;               ((quux) quux))))))
;
;     (define foo? (make-object-type-predicate 'foo))
;
; Sufficient accessors should be provided that clients never have to know that
; encapsulated objects are dispatch procedures; for example, if clients should
; have the ability to access the 'bar and 'quux attributes of a foo,
;
;     (define get-foo-bar  (lambda (x) (x 'bar)))
;     (define get-foo-quux (lambda (x) (x 'quux)))
;

;
; Determines whether all its arguments are objects.
;
(define object? procedure?)

;
; Given some number of symbol arguments, constructs a predicate that takes
; an argument and determines whether it is an object whose type is amongst
; the given symbols.
;
(define make-object-type-predicate
  (lambda types
    (lambda (object)
      (and (object? object)
           (pair? (member (object 'type) types))))))

;
; If given object can be named but hasn't been yet, names it, and if the
; object's name record is linked to others, suggets the name to them, too.
;
; If the second argument is an object, the name of that object is used.
; If (after that substitution, if any) the second argument is a boolean,
; no action is taken.
;
(define suggest-object-name
  (lambda (object name)
    (let ((name  (if (object? name) (car (name 'name)) name)))
      (if (not (boolean? name))
          (letrec ((aux  (lambda (ls)
                           (if (and (pair? ls) (boolean? (car ls)))
                               (begin
                                 (if (eq? (car ls) #t)
                                     (set-car! ls name))
                                 (aux (cdr ls)))))))
            (aux (object 'name)))))))

;
; Designates a particular other object to which this object should suggest its
; name (if it ever acquires one).  The designation is singular and permanent,
; and should be another object that is in some sense the foundation for this
; object, such as the underlying combiner of an applicative.  (If there weren't
; already such a foundational relationship, the designation would prevent the
; other object from being garbage collected before this one.)  It often happens
; that the predecessor object is created just to found its successor, and is
; never directly bound by a symbol, so that this facility becomes the most
; likely way for the predecessor to receive a relevant name.
;
(define designate-name-inheritor!
  (lambda (object heir)
    (let ((oname  (object 'name))
          (hname  (heir 'name)))
      (if (null? (cdr oname))
          (begin
            (suggest-object-name heir (car oname))
            (set-cdr! oname hname))))))

;
; Generates a string describing given object.  Not for use on a kernel-pair.
;
(define describe-object
  (lambda (object)
    (let ((type  (symbol->string (object 'type)))
          (name  (car (object 'name))))
      (cond ((eq? name #f)  (string-append "#" type))
            ((string? name) (string-append "#[" type " " name "]"))
            ((symbol? name) (string-append "#[" type " " (symbol->string name)
                                                "]"))
            (else           (string-append "#[" type "]"))))))
