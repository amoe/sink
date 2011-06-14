; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 2)
             (list 0.1 2))
(set-revision-date 2009 9 20)

;;;;;;;;;;;;;;;;;;;;
; error-descriptor ;
;;;;;;;;;;;;;;;;;;;;
;
; An error-descriptor has type 'error-descriptor, and attribute 'content whose
; value is a Scheme list of Scheme lists of line elements.  A line element is
; either a string, which is taken to be message text; or a Scheme list of one
; element, which is taken to be literal data.
;

(define make-error-descriptor
  (lambda content
    (let ((name     (list #t))
          (content  (map (lambda (x) (if (string? x) (list x) x)) content)))
      (lambda (message)
        (case message
          ((type)    'error-descriptor)
          ((name)    name)
          ((content) content))))))

(define error-descriptor? (make-object-type-predicate 'error-descriptor))

(define get-error-content (lambda (x) (x 'content)))

(define error-add-to-first-line!
  (lambda (ed . suffix)
    (let ((content  (get-error-content ed)))
      (set-car! content
                (append (car content) suffix)))))

;
; Describe an error.
;
; Since error descriptors can only be created directly by the interpreter,
; if the internal format is wrong, it's SINK's fault.
;
(define describe-error
  (lambda (error-descriptor)

    ; number of columns expended on strings on the current line
    (define column 0)

    ; single element on a line
    (define aux3
      (lambda (element)
        (cond ((string? element)
                 (let ((new-column  (+ column (string-length element))))
                   (if (< 79 new-column)
                       (begin
                         (newline)
                         (display " ;   ")
                         (set! column 5))
                       (set! column new-column)))
                 (display element))
              ((and (pair? element)
                    (null? (cdr element)))
                 (write-tree (car element) (current-output-port)))
              (else
                (display " [[")
                (write-tree element)
                (display "]] ")))))

    ; list of elements on a line
    (define aux2
      (lambda (ls)
        (if (pair? ls)
            (begin
              (aux3 (car ls))
              (aux2 (cdr ls))))))

    ; list of lines
    (define aux
      (lambda (lss)
        (if (pair? lss)
            (begin
              (display " ; ")
              (set! column 3)
              (aux2 (car lss))
              (newline)
              (aux (cdr lss))))))

    (aux (get-error-content error-descriptor))))

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the error-descriptor
; type.  It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the inert type.
;
(define bind-error-descriptor-primitives!
  (lambda (env)
    (add-bindings! env

      'error-descriptor?  (unary-predicate->applicative error-descriptor?))))
