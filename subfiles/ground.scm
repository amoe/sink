; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.1 3))
(set-revision-date 2009 9 21)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Kernel ground environment ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; The ground environment contains bindings for all built-in combiners.
;

(define ground-environment (make-environment))

;
; Evaluates a sequence of expressions, and returns the last result.
; Used by both $vau and $sequence.
;
(define eval-sequence
  (lambda (operand-tree env context)
    (cond ((null? operand-tree)  inert)
          ((not (kernel-pair? operand-tree))
             (error-pass
                (make-error-descriptor
                  "Non-list operand-tree when calling #[operative $sequence]")
                context))
          ((null? (kernel-cdr operand-tree))
             (eval (kernel-car operand-tree) env context))
          (else
             (eval          (kernel-car operand-tree) env context)
             (eval-sequence (kernel-cdr operand-tree) env context)))))

;
; Predicates the combiner type.
;
(define combiner? (make-object-type-predicate 'operative 'applicative))

;
; Predicates anything.
;
(define any? (lambda x #t))

;
; The primitive bindings.
;
; Many are enumerated here, especially those that are imported to Kernel from
; Scheme and those that aren't strongly associated with one of the other files.
; Others are handled by initializer procedures that were defined elsewhere.
;
(add-bindings! ground-environment

; 'combiner?   (unary-predicate->applicative  combiner?)

  'char?       (unary-predicate->applicative  char?)
  'eof-object? (unary-predicate->applicative  eof-object?)
  'eq?         (binary-predicate->applicative eq?          any?)
  'null?       (unary-predicate->applicative  null?)
  'string?     (unary-predicate->applicative  string?)
  'symbol?     (unary-predicate->applicative  symbol?)

  'string=?     (binary-predicate->applicative  string=?      string?)
  'string<?     (binary-predicate->applicative  string<?      string?)
  'string>?     (binary-predicate->applicative  string>?      string?)
  'string<=?    (binary-predicate->applicative  string<=?     string?)
  'string>=?    (binary-predicate->applicative  string>=?     string?)
  'string-ci=?  (binary-predicate->applicative  string-ci=?   string?)
  'string-ci<?  (binary-predicate->applicative  string-ci<?   string?)
  'string-ci>?  (binary-predicate->applicative  string-ci>?   string?)
  'string-ci<=? (binary-predicate->applicative  string-ci<=?  string?)
  'string-ci>=? (binary-predicate->applicative  string-ci>=?  string?)

  'string-append
  (naive->checked-applicative
    (lambda (operand-tree)
      (apply string-append
             (copy-kernel-list->list operand-tree)))
    "string-append"
    0 -1 string?)

  'number->string
  (naive->checked-applicative
    (lambda (operand-tree)
      (let ((number  (kernel-car operand-tree)))
        (if (object? number)
            (string-copy (describe-object number))
            (number->string number))))
    "number->string"
    1 1 kernel-number?)

  'list->string
  (naive->checked-applicative
    (lambda (operand-tree)
      (list->string (kernel-list->list (kernel-car operand-tree))))
    "list->string"
    1 1 kernel-list?)

  'integer->char
  (naive->checked-applicative
    (lambda (operand-tree)
      (integer->char (kernel-car operand-tree)))
    "integer->char"
    1 1 integer?)

  'char->integer
  (naive->checked-applicative
    (lambda (operand-tree)
      (char->integer (kernel-car operand-tree)))
    "char->integer"
    1 1 char?)

;;;;;;;;;;;; doesn't look right
;  'assoc
;  (naive->checked-applicative
;    (lambda (operand-tree)
;      (let* ((key     (kernel-car operand-tree))
;             (alist   (kernel-cadr operand-tree))
;             (result  (assoc key (kernel-list->list alist))))
;        (if (pair? result)
;            result
;            ())))
;    "assoc"
;    2 2 any? kernel-list?)
;;;;;;;;;;;;

;  'assq
;  (naive->checked-applicative
;    (lambda (operand-tree)
;      (let* ((key     (kernel-car operand-tree))
;             (alist   (kernel-cadr operand-tree))
;             (result  (assq key (kernel-list->list alist))))
;        (if (pair? result)
;            result
;            ())))
;    "assq"
;    2 2 any? kernel-list?)

; '$sequence
; (action->operative eval-sequence)

  'load
  (action->checked-applicative
    (lambda (operand-tree env context)
      (let* ((filename  (kernel-car operand-tree))
             (kip       (open-kernel-input-file filename context)))
        (suggest-object-name kip (string-append "\"" filename "\""))
        (call-with-guarded-context
          (lambda (context)
            (letrec ((aux  (lambda (legacy)
                             (let ((object  (copy-es-immutable
                                              (kernel-read kip context))))
                               (if (eof-object? object)
                                   (begin
                                     (close-kernel-input-port kip context)
                                     legacy)
                                   (aux (eval object env context)))))))
              (aux inert)))
          context
          (list (cons ()
                      (lambda (v)
                        (error-pass
                          (make-error-descriptor
                            (list "Tried to reenter dynamic extent of load \""
                                  filename "\"")
                            (list "  Value sent: " (list v)))
                          context))))
          (list (cons ()
                      (lambda (v)
                        (close-kernel-input-port kip context)
                        v))))))
    1 1 string?)

  ;
  ; Finally, these bindings are added to the ground environment temporarily,
  ; for use in "subfiles/library.snk" so that it can contribute to the version
  ; number and date.  They are removed from the ground environment after the
  ; library has been loaded.
  ;

  '$set-version
  (naive->checked-operative
    (lambda (operand-tree)
      (apply set-version
        (map kernel-list->list (kernel-list->list operand-tree))))
    "$set-version"
    0 -1 (lambda (x) (and (kernel-pair? x) (kernel-pair? (kernel-cdr x)))))

  '$set-revision-date
  (naive->checked-operative
    (lambda (operand-tree)
      (apply set-revision-date (kernel-list->list operand-tree)))
    "$set-revision-date"
    3 3 integer?)

  )

(bind-applicative-primitives!      ground-environment)
(bind-boolean-primitives!          ground-environment)
(bind-context-primitives!          ground-environment)
(bind-cyclic-primitives!           ground-environment)
(bind-encapsulation-primitives!    ground-environment)
(bind-environment-primitives!      ground-environment)
(bind-error-descriptor-primitives! ground-environment)
(bind-ignore-primitives!           ground-environment)
(bind-inert-primitives!            ground-environment)
(bind-kernel-pair-primitives!      ground-environment)
(bind-keyed-variable-primitives!   ground-environment)
(bind-number-primitives!           ground-environment)
(bind-operative-primitives!        ground-environment)
(bind-port-primitives!             ground-environment)

;
; The library bindings.
;
; This code loads the Kernel library.  Since loading involves evaluation, it
; has to create a top-level context, and in case an error message must be
; generated during the load it also names the ground environment; the code
; is therefore rather similar to that of interpreter, in file
; "subfiles/interpreter.scm".  After loading the library, bindings for
; symbols "set-version" and "set-revision-date" are removed from the ground
; environment (since they aren't meant to be available to Kernel programs).
;

(let ((okay  #t))
  (suggest-object-name ground-environment 'the-ground-environment)
  (let ((context  (make-top-level-context
                    (lambda (x)
                      (report-error x)
                      (set! okay #f)))))
    (if okay
        (begin
          (eval (list->kernel-list '(load "subfiles/library.snk"))
                ground-environment context)
          (remove-bindings! ground-environment
                            '$set-version
                            '$set-revision-date)
          ))))
