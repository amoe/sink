; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.0 1)
             (list 0.1 1))
(set-revision-date 2009 9 1)

;;;;;;;;;
; pairs ;
;;;;;;;;;
;
; Pairs in the interpreted language ("Kernel") are a different data type from
; pairs in the meta-language (Scheme).  The interpreted-language type is
; called "kernel-pair".  Outside of this file, types pair and kernel-pair
; cannot be assumed to be identical, nor can they be assumed to be disjoint.
;
; The interpreted language has both mutable and immutable kernel-pairs.
; Their subtypes are called respectively "mutable" and "immutable".
; Private to this file, immutables are disjoint from pairs, while two
; different implementations are possible for mutables, one in which mutables
; are actually pairs (unbeknownst to the rest of the interpreter), the other
; in which they too are disjoint from pairs.
;
; When a procedure for kernel-pairs is cognate to one for pairs, it is named
; by adding prefix "kernel-" to the Scheme name:  kernel-cons, kernel-car,
; kernel-cdr, kernel-caar, etc.
;
; The only constructor of immutables is copy-es-immutable, which returns an
; immutable copy of a kernel-pair and all other kernel-pairs reachable from
; it without passing through any non-kernel-pair.  Consequently, it is not
; possible for an immutable to have a mutable as its kernel-car or kernel-cdr.
;
; Type kernel-list differs from type list both by using kernel-pairs instead of
; pairs, and by allowing cycles.  An "improper kernel-list" doesn't have to be
; null-terminated, therefore all Kernel values are improper kernel-lists.
; Similarly, every Kernel value is a kernel-tree; but Scheme trees aren't used
; in the interpreter, so kernel-trees are called simply "trees".
;
;
; Implementing mutables disjointly is more expensive than implementing
; immutables that way, because there is usually just one immutable copy of an
; algorithm, but during evaluation of that one copy, many mutable argument
; lists are constructed.  On the other hand, if mutables are represented by
; pairs, it is appallingly easy for code elsewhere in the interpreter to treat
; Kernel structures as if they were Scheme structures, without the bug being
; detected for a long time.  Both implementations have been provided, one in
; file "subfiles/kernel-pair-disjoint.scm" and the other in file
; "subfiles/kernel-pair-overlapping.scm"; the file loaded by the interpreter,
; called "subfiles/kernel-pair.scm", is a copy of one or the other version.
;
; This is the overlapping version of the type.  A mutable kernel-pair is a
; pair; an immutable kernel-pair is an object with type 'immutable and
; attributes 'kar and 'kdr.
;

(define mutable? pair?)

(define immutable? (make-object-type-predicate 'immutable))

(define kernel-pair?
  (lambda (x)
    (or (mutable? x)
        (immutable? x))))

(define kernel-car
  (lambda (x)
    (if (pair? x)
        (car x)
        (x 'kar))))

(define kernel-cdr
  (lambda (x)
    (if (pair? x)
        (cdr x)
        (x 'kdr))))

(define kernel-caar (lambda (x) (kernel-car (kernel-car x))))
(define kernel-cadr (lambda (x) (kernel-car (kernel-cdr x))))
(define kernel-cddr (lambda (x) (kernel-cdr (kernel-cdr x))))
(define kernel-caddr (lambda (x) (kernel-car (kernel-cdr (kernel-cdr x)))))
(define kernel-cadddr
  (lambda (x) (kernel-car (kernel-cdr (kernel-cdr (kernel-cdr x))))))

(define kernel-cons  cons)

(define kernel-list
  (lambda x
    (if (pair? x)
        (kernel-cons (car x) (apply kernel-list (cdr x)))
        x)))

(define kernel-set-car!  set-car!)
(define kernel-set-cdr!  set-cdr!)

;
; Constructs a procedure that takes as its sole argument a possibly-cyclic
; structure composed from some pair-like primitive data type, and returns a
; list of nodes of the structure (i.e., pair-like entities) whose revisits
; should be pruned during traversal of the structure.
;
; The precise condition that should be satisfied by the result is that the
; listed revisits are a minimal set sufficient to minimize a traversal of the
; structure.
;   "Sufficient to minimize a traversal" means that, if the structure were
; traversed, checking each node against the revisits-list; and at the first
; position where a listed node is visited, traversal would continue past it to
; its descendants, but at other positions where it occurs, traversal would not
; continue past it; then this traversal would visit every node of the
; structure at least once, and would revisit only nodes on the revisits-list.
;   "Minimal set" means that if any member of the revisits-list were removed,
; then it would no longer have this property, i.e., it would no longer be
; sufficient to minimize a traversal.
;
; The purpose of this condition is to all clients to preserve structural
; isomorphism.  This is a strictly more difficult task than merely preventing
; infinite traversal of cyclic structures.  For example, commands
;   ($define! x (list 1 2 3))
;   (set-car! x (cdr x))
; would produce acyclic structure  (#1=(2 3) . #1#)  whose revisit-list would
; be a singleton list of node #1#.  Merely to prevent infinite traversals,
; it would suffice to check each node against its ancestors; but that would
; not detect the redundancy in this example, so that any structural
; transformation based on such an algorithm could not be expected to produce
; a structurally isomorphic result.
;
; Arguments:
;   tree               --- the structure itself, composed of pair-like nodes
;   node?              --- predicate for the pair-like type
;   node-car, node-cdr --- accessors for the pair-like type
;
(define make-get-revisits
  (lambda (node? node-car node-cdr)

    (define aux
      (lambda (revisits all . trees)
        (if (null? trees)
            revisits
            (let ((tree   (car trees))
                  (trees  (cdr trees)))
              (cond ((or (not (node? tree))
                         (pair? (memq tree revisits)))
                       (apply aux revisits all trees))
                    ((pair? (memq tree all))
                       (apply aux (cons tree revisits) all trees))
                    (else
                       (apply aux revisits (cons tree all)
                                  (node-car tree) (node-cdr tree) trees)))))))

    ; get-revisits
    (lambda (tree)
      (aux () () tree))))

(define get-kernel-revisits
  (make-get-revisits kernel-pair? kernel-car kernel-cdr))

;
; Constructs a procedure that takes as its sole argument a possibly-cyclic
; structure composed from some pair-like primitive data type, and returns
; a structurally isomorphic copy of its evaluation structure, optionally
; performing some transformation on leaf nodes during the copy.
;
; There will be three such procedures constructed:
; copy-es-immutable, copy-es, and scheme-read-object->kernel.
;
; The evaluation structure of a value (under a given pair-like primitive type)
; is the structure whose start is the value itself, and whose members are all
; objects reachable from the start by following only car and cdr references
; (of the given pair-like primitive type).  If the value is not of the
; chosen pair-like type, then the value itself is the only object of the
; data structure.
;
; Arguments:
;     in-pair?       --- predicate for the input pair-like type
;     in-car, in-cdr --- accessors for the input pair-like type
;     make-record    --- constructs an alist record (see below)
;     out-cons       --- constructs a copy of a non-pruned parent node
;     xform-leaf     --- transformation to perform on leaves when copying
;
; First, compiles an alist whose keys are those in-pairs in the input
; structure whose cyclic revisiting must be pruned during traversal.  For
; each of these in-pairs, make-record constructs an alist record whose key
; is the in-pair, whose cadr is an out-pair, and whose cddr is a pair whose
; elements determine the out-car and out-cdr of the out-pair.  (Depending on
; representations, the cadr and cddr might actually be the same object.)  Then
; the in-pairs of the input structure are traversed a second time, creating
; out-pairs for non-pruned mutables using out-cons, and setting the elements
; of the previously constructed out-pairs for pruned in-pairs.  When the
; elements of a pruned out-pair are to be set, its content pair is separated
; out and the cddr of its record is set to nil, to prevent infinite recursion.
;
(define make-es-copier
  (lambda (in-pair? in-car in-cdr make-record out-cons xform-leaf)

    (define get-in-revisits (make-get-revisits in-pair? in-car in-cdr))

    ; es-copier
    (lambda (tree)

      (define alist (map make-record (get-in-revisits tree)))

      (define aux
        (lambda (tree)
          (if (not (in-pair? tree))
              (xform-leaf tree)
              (let ((record  (assq tree alist)))
                (if (pair? record)
                    (let ((content  (cddr record)))
                      (if (pair? content)
                          (begin
                            (set-cdr! (cdr record) ())
                            (set-car! content (aux (in-car tree)))
                            (set-cdr! content (aux (in-cdr tree)))))
                      (cadr record))
                    (out-cons (aux (in-car tree))
                              (aux (in-cdr tree))))))))

      (aux tree))))

;
; Given a Kernel value, returns an immutable copy of its evaluation structure.
;
(define copy-es-immutable
  (make-es-copier
    mutable? kernel-car kernel-cdr
    (let ((name  (list #f)))
      (lambda (key)
        (let ((content  (cons () ())))
          (let ((immutable  (lambda (message)
                              (case message
                                ((type) 'immutable)
                                ((name) name)
                                ((kar)  (car content))
                                ((kdr)  (cdr content))))))
            (cons key
                  (cons immutable content))))))
    (let ((name  (list #f)))
      (lambda (kar kdr)
        (lambda (message)
          (case message
            ((type) 'immutable)
            ((name) name)
            ((kar)  kar)
            ((kdr)  kdr)))))
    (lambda (x) x)))

;
; Given a Kernel value, returns a mutable copy of its evaluation structure.
;
(define copy-es
  (make-es-copier
    kernel-pair? kernel-car kernel-cdr
    (lambda (key)
      (let ((content  (cons () ())))
        (cons key (cons content content))))
    cons
    (lambda (x) x)))

;
; Given a scheme value presumed to have just been read, returns a mutable
; Kernel version of the value, by copying its evaluation structure and
; transforming certain symbols to their Kernel counterparts.
;
(define scheme-read-object->kernel
  (make-es-copier
    pair? car cdr
    (lambda (key)
      (let ((content  (cons () ())))
        (cons key (cons content content))))
    kernel-cons
    (lambda (x)
      (if (symbol? x)
          (case x
            ((%ignore) ignore)
            ((%inert)  inert)
            ((%e+infinity)  exact-positive-infinity)
            ((%e-infinity)  exact-negative-infinity)
            ((%i+infinity)  inexact-positive-infinity)
            ((%i-infinity)  inexact-negative-infinity)
            (else      x))
          x))))

;
; Given a kernel-list, returns a list with the same elements in the same order.
; The result is guaranteed to be a list (acyclic and made up of pairs), but is
; not guaranteed to be distinct from the given kernel-list:  if mutables are
; represented by pairs, the result may be the given kernel-list.  Therefore,
; this tool should only be used if the resultant list certainly will not be
; mutated (because mutating the result might mutate the original kernel-list).
;
; To guarantee that the result will be distinct from the argument,
; use  copy-kernel-list->list.
;
(define kernel-list->list
  (lambda (ls)
    (if (list? ls)
        ls
        (bounded-simple-map->list (car (get-list-metrics ls))
                                  (lambda (x) x)
                                  ls))))

;
; Given a list, returns a mutable kernel-list with the same elements in the
; same order.  The elements are assumed to be kernel values.  The result is
; not guaranteed to be distinct from the given list:  if mutables are
; represented by pairs, the result may be the given kernel-list.  Therefore,
; this tool should only be used if the given list won't be needed again
; (so that if it happens to be mutated, that won't be a problem).
;
(define list->kernel-list (lambda (x) x))

;
; Determines whether a tree (i.e., an arbitrary interpreted-language value)
; is cyclic.
;
(define cyclic-tree?
  (lambda (tree)

    (define aux
      (lambda (ancestors tree)
        (cond ((not (kernel-pair? tree))  #f)
              ((pair? (memq tree ancestors))  #t)
              (else
                (let ((ancestors  (cons tree ancestors)))
                  (or (aux ancestors (kernel-car tree))
                      (aux ancestors (kernel-cdr tree))))))))

    (aux () tree)))

;
; Given a tree of the interpreted language, output a representation of it to
; a given output-port, using a given procedure to output the non-object leaves.
; The latter takes as arguments the leaf and the output-port.  Either the third
; argument, or the second and third arguments, may be omitted.  If the third
; argument is omitted, write is used.  If the second argument is also omitted,
; the current output-port is used.
;
; Cyclicity is handled by keeping an alist of revisits (kernel-pairs that will
; be visited more than once and are to be expanded only on the first visit),
; where the cadr of each record is the position of the record in the alist,
; and the cddr of the record is #t or #f depending on whether that revisit has
; already been expanded once.
;
(define write-tree
  (lambda (x . options)
    (let ((outport     (if (pair? options)
                           (car options)
                           (current-output-port)))
          (write-leaf  (if (and (pair? options) (pair? (cdr options)))
                           (cadr options)
                           write))
          (table  (letrec ((aux  (lambda (ls k)
                                   (if (null? ls)
                                       ls
                                       (cons (cons (car ls) (cons k #f))
                                             (aux (cdr ls) (+ k 1)))))))
                    (aux (get-kernel-revisits x) 0))))

      (define write-visit
        (lambda (x rec)
          (display "#"        outport)
          (display (cadr rec) outport)
          (if (cddr rec)
              (display "#" outport)
              (begin
                (set-cdr! (cdr rec) #t)
                (display   "=(" outport)
                (write-car (kernel-car x))
                (write-cdr (kernel-cdr x))
                (display   ")" outport)))))

      (define write-cdr
        (lambda (x)
          (cond ((null? x))
                ((kernel-pair? x)
                   (let ((rec  (assq x table)))
                     (if (pair? rec)
                         (begin
                           (display     " . " outport)
                           (write-visit x rec))
                         (begin
                           (display   " " outport)
                           (write-car (kernel-car x))
                           (write-cdr (kernel-cdr x))))))
                (else
                   (display   " . " outport)
                   (write-car x)))))

      (define write-car
        (lambda (x)
          (cond ((kernel-pair? x)
                   (let ((rec  (assq x table)))
                     (if (pair? rec)
                         (write-visit x rec)
                         (begin
                           (display   "(" outport)
                           (write-car (kernel-car x))
                           (write-cdr (kernel-cdr x))
                           (display   ")" outport)))))
                ((object? x)  (display (describe-object x) outport))
                (else  (write-leaf x outport)))))

      (write-car x))))

;
; As write-tree, except that there must be exactly two arguments, and the
; non-object leaf output procedure is display rather than write.
;
(define display-tree
  (lambda (x outport)
    (write-tree x outport display)))

;
; Creates bindings for this type in a given environment.
;
; This code should not use any internal knowledge of the kernel-pair type.
; It appears in this file, rather than in "subfiles/ground.scm", simply
; because it is logically associated with the kernel-pair type.
;
(define bind-kernel-pair-primitives!
  (lambda (env)
    (add-bindings! env

      'pair? (unary-predicate->applicative  kernel-pair?)

      'cons
      (naive->checked-applicative
        (lambda (operand-tree)
          (kernel-cons (kernel-car operand-tree)
                       (kernel-cadr operand-tree)))
        "cons"
        2 2)

      'copy-es-immutable
      (naive->checked-applicative
        (lambda (operand-tree)
          (copy-es-immutable (kernel-car operand-tree)))
        "copy-es-immutable"
        1 1)

      'copy-es
      (naive->checked-applicative
        (lambda (operand-tree)
          (copy-es (kernel-car operand-tree)))
        "copy-es"
        1 1)

      'set-car!
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let ((x  (kernel-car operand-tree))
                (y  (kernel-cadr operand-tree)))
            (if (mutable? x)
                (kernel-set-car! x y)
                (error-pass (make-error-descriptor
                              (list "Operand #1 is immutable"
                                    " when calling primitive set-car!")
                              (list "Operand tree: " (list operand-tree)))
                            context)))
          inert)
        2 2 kernel-pair? any?)

      'set-cdr!
      (action->checked-applicative
        (lambda (operand-tree env context)
          (let ((x  (kernel-car operand-tree))
                (y  (kernel-cadr operand-tree)))
            (if (mutable? x)
                (kernel-set-cdr! x y)
                (error-pass (make-error-descriptor
                              (list "Operand #1 is immutable"
                                    " when calling primitive set-cdr!")
                              (list "Operand tree: " (list operand-tree)))
                            context)))
          inert)
        2 2 kernel-pair? any?)

      )))
