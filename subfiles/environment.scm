; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 1)
             (list 0.1 0))
(set-revision-date 2007 8 5)

;;;;;;;;;;;;;;;;
; environments ;
;;;;;;;;;;;;;;;;
;
; An environment has type 'environment, and attributes 'frames and 'alist.
;
;   'frames is a nonempty list of frames; a frame is a list of length one whose
; sole element is a list of name/value pairs.  Lookup starts with the first
; frame.  There should never be any reason to work with frames outside this
; file.
;
;   'alist is a list of keyed-bindings, constructed by tools in file
; "subfiles/keyed.scm".
;

;
; private constructor/accessors
;

(define internal-make-environment
  (lambda (frames alist)
    (let ((name  (list #t)))
      (lambda (message)
        (case message
          ((type)    'environment)
          ((name)    name)
          ((frames)  frames)
          ((alist)   alist))))))

(define get-environment-frames (lambda (x) (x 'frames)))
(define get-environment-alist  (lambda (x) (x 'alist)))

;
; public constructor/accessors
;

(define make-environment
  (lambda parents
    (internal-make-environment
      (cons (make-empty-frame)
            (apply append
                   (map get-environment-frames
                        parents)))
      (apply merge-alists
             (map get-environment-alist
                  parents)))))

(define make-standard-environment
  (lambda ()
    (make-environment ground-environment)))

(define make-environment-with-keyed-binding
  (lambda (key value parent)
    (internal-make-environment
      (cons (make-empty-frame) (get-environment-frames parent))
      (make-alist (get-environment-alist parent) key value))))

(define environment? (make-object-type-predicate 'environment))

(define environment-keyed-lookup
  (lambda (key env context)
    (let ((binding  (alist-lookup key (get-environment-alist env))))
      (if (pair? binding)
          (cdr binding)
          (error-pass
            (make-error-descriptor
              "Attempted to look up an unbound keyed static variable"
              (list "in " (list env)))
            context)))))

;
; Returns the value bound to name if there is one, otherwise throws an error.
;
(define lookup
  (lambda (name env context)
    (let ((binding  (get-binding-from-frames
                      name (get-environment-frames env))))
      (if (eq? binding #f)
          (error-pass
            (make-error-descriptor
              (list "Unbound symbol: " (symbol->string name))
              (list "in " (describe-object env)))
            context)
          (cdr binding)))))

;
; Performs a series of local bind operations, mutating existing local bindings
; when possible, creating new local bindings otherwise.  Takes an odd number of
; arguments, of which the even arguments are names (i.e., symbols).  The first
; argument is the environment in which local bindings are to be performed.
; Each later odd argument is the value to be locally bound to the immediately
; preceding name.
;
(define add-bindings!
  (lambda (env . x)
    (apply add-bindings-to-frame!
           (car (get-environment-frames env))
           x)))

;
; Removes local bindings for given symbols.  The first argument is the
; environment from which local bindings are to be removed, and each later
; argument is a symbol whose local binding, if any, is to be removed.
;
; This facility is not available to Kernel, but is used in
; "subfiles/ground.scm" to allow certain facilities to be provided
; temporarily in the ground environment while the Kernel library is being
; loaded, and then removed from the ground environment before entering the
; read-eval-print loop.
;
(define remove-bindings!
  (lambda (env . x)
    (apply remove-bindings-from-frame!
           (car (get-environment-frames env))
           x)))

;
; Determines whether a parameter tree is valid.
;
; A parameter tree is valid if it is acyclic, it contains no duplicate symbols,
; and every leaf is either a symbol, nil, or ignore.
;
(define valid-ptree?
  (lambda (tree)

    (define aux ; returns symbols if valid, #f if invalid
      (lambda (tree symbols)
        (cond ((ignore? tree)  symbols)
              ((null? tree)    symbols)
              ((symbol? tree)  (if (pair? (member tree symbols))
                                   #f
                                   (cons tree symbols)))
              ((kernel-pair? tree)
                 (let ((symbols  (aux (kernel-car tree) symbols)))
                   (if (eq? symbols #f)
                       #f
                       (aux (kernel-cdr tree) symbols))))
              (else  #f))))

    (and (not (cyclic-tree? tree))
         (list? (aux tree ())))))

;
; Locally binds a parameter-tree to an object.
;
; On success, returns nil.  On failure (invalid  parameter-tree, or
; parameter-tree/object mismatch), returns an error-descriptor to whose
; first line " when calling ..." might reasonably be appended.
;
(define match!
  (lambda (env ptree object)

    (define emsg ()) ; repository for error-descriptor content

    ; returns arguments for add-bindings-to-frame!
    (define aux
      (lambda (ptree object args)
        (cond ((kernel-pair? ptree)
                 (if (kernel-pair? object)
                     (aux      (kernel-cdr ptree) (kernel-cdr object)
                          (aux (kernel-car ptree) (kernel-car object) args))
                     (set! emsg
                           (append emsg
                                   (list (list "  mismatch:  " (list ptree)
                                               "  " (list object)))))))
              ((symbol? ptree)  (cons ptree (cons object args)))
              ((null? ptree)   (if (null? object)
                                   args
                                   (set! emsg
                                         (append emsg
                                            (list (list "  mismatch:  ()  "
                                                        (list object)))))))
              (else args)))) ; must be ignore

    (if (not (valid-ptree? ptree))
        (make-error-descriptor "Invalid parameter tree"
                               (list "Parameter tree: " (list ptree)))
        (let ((args  (aux ptree object ())))
          (if (pair? emsg)
              (apply make-error-descriptor
                     "Definiend/object mismatch"
                     (list "Definiend:  " (list ptree))
                     (list "Object:     " (list object))
                     (list)
                     emsg)
              (begin
                (apply add-bindings-to-frame!
                       (car (get-environment-frames env))
                       args)
                ()))))))

;
; Constructs an empty frame.
;
(define make-empty-frame (lambda () (list ())))

;
; Performs a series of bind operations in given frame, mutating existing
; bindings when possible, creating new bindings otherwise.  Arguments as
; add-bindings!, except that the first argument is the local frame.
;
(define add-bindings-to-frame!
  (lambda (frame . x)
    (if (pair? x)
        (let ((name   (car  x))
              (value  (cadr x))
              (x      (cddr x)))
          (if (object? value)
              (suggest-object-name value name))
          (let ((binding  (get-binding-from-frame name frame)))
            (if (eq? binding #f)
                (set-car! frame (cons (cons name value) (car frame)))
                (set-cdr! binding value)))
          (apply add-bindings-to-frame! frame x)))))

;
; Deletes bindings for given symbols from given frame.  Arguments as
; remove-bindings!, except that the first argument is the local frame.
;
(define remove-bindings-from-frame!
  (lambda (frame . x)

    (define aux-cdr!
      (lambda (bindings) ; must be a pair whose car has already been checked
        (cond ((null? (cdr bindings)))
              ((pair? (member (caadr bindings) x))
                 (set-cdr! bindings (cddr bindings))
                 (aux-cdr! bindings))
              (#t
                 (aux-cdr! (cdr bindings))))))

    (define aux-car!
      (lambda (frame)
        (cond ((null? (car frame)))
              ((pair? (member (caaar frame) x))
                 (set-car! frame (cdar frame))
                 (aux-car! frame))
              (#t
                 (aux-cdr! (car frame))))))

    (aux-car! frame)))

;
; Returns the binding for name if there is one, otherwise returns #f.
;
(define get-binding-from-frames
  (lambda (name frames)
    (if (null? frames)
        #f
        (let ((binding  (get-binding-from-frame name (car frames))))
          (if (pair? binding)
              binding
              (get-binding-from-frames name (cdr frames)))))))

;
; Returns the binding for name if there is one, otherwise returns #f.
;
(define get-binding-from-frame
  (lambda (name frame)
    (assoc name (car frame))))

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the environment type.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the type.
;
(define bind-environment-primitives!
  (lambda (env)
    (add-bindings! env

      'environment?
      (unary-predicate->applicative  environment?)

      'eval
      (action->checked-applicative
        (lambda (operand-tree env context)
          (eval (kernel-car operand-tree) (kernel-cadr operand-tree) context))
        2 2 any? environment?)

      'make-environment
      (naive->checked-applicative
        (lambda (operand-tree)
          (apply make-environment
                 (copy-kernel-list->list operand-tree)))
        "make-environment"
        0 -1 environment?)

      '$define!
      (action->checked-operative
        (lambda (operand-tree env context)
           (let ((ed  (match! env (kernel-car operand-tree)
                                  (eval (kernel-cadr operand-tree)
                                        env context))))
             (if (error-descriptor? ed)
                 (begin
                   (error-add-to-first-line!  ed
                     " when calling #[operative $define!]")
                   (error-pass ed context))
                 inert)))
        2 2)

      )))
