; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

(set-version (list 0.1 0))
(set-revision-date 2007 8 5)

;;;;;;;;;;;;;;;;;;;;;
; cyclic structures ;
;;;;;;;;;;;;;;;;;;;;;
;
; Kernel treats cyclic structures of kernel-pairs (such as cyclic lists) as
; first-class objects, by guaranteeing that each standard combiner will handle
; cyclic structures in finite time whenever that behavior is consistent with
; the intended semantics of the combiner.
;
; The tools in this file help SINK to handle cyclic structures.  However,
; despite the availability of these tools, at last check SINK did not (yet)
; handle cyclic structures in all cases required by Kernel.
;
; The tools here do not use any internal knowledge of the kernel-pair type.
; Cycle-related tools that do use internal knowledge of that type are in file
; "subfiles/kernel-pair.scm".
;
; A kernel-tree is called simply a "tree", because the interpreter has no
; occasion to use Scheme trees.  A kernel-pair may be visited more than once
; during normal (left-to-right, depth first) traversal of a tree without
; constituting a cycle, so long as the revisited kernel-pair isn't a
; descendant of itself.
;
; Even if all kernel-pairs were known to be represented by pairs (which, if it
; were true, should only be known in "subfiles/kernel-pair.scm"), it would be
; necessary to handle them with tongs in Scheme, because Scheme doesn't grant
; first-class status to cyclic pair-structures.  Notably, the argument list of
; a procedure cannot be cyclic.  In Kernel, of course, there is no difficulty
; in using a cyclic argument list (or operand list).
;

;
; Given improper kernel-list ls and nonnegative integer k, returns the (k)th
; kernel-cdr of ls.  Thus, if k=0, returns ls.
;
(define kernel-list-tail
  (lambda (ls k)
    (if (> k 0)
        (kernel-list-tail (kernel-cdr ls) (- k 1))
        ls)))

;
; Given improper kernel-list ls and nonnegative integer k, returns the
; kernel-car of the (k)th kernel-cdr of ls.  Thus, if k=0, returns the
; kernel-car of ls; if k=1, returns the kernel-cadr of ls; and so on.
;
(define kernel-list-ref
  (lambda (ls k)
    (kernel-car (kernel-list-tail ls k))))

;
; Given a value to be construed as an improper kernel-list, returns a list
; of the following four statistics about the value:
;
;   p = the number of kernel-pairs in the improper list.
;   n = the number of nils in the improper list
;         (1 if proper and finite, else 0).
;   a = the number of kernel-pairs in the acyclic prefix of the improper list.
;   c = the number of kernel-pairs in the cycle of the improper list
;         (0 if not a cyclic list).
;
; The algorithm here is linear-time, requiring two passes through the improper
; list, of which the first pass may traverse the improper list for up to twice
; its length, and the second pass traverses it for its length.
;  (1) In the first pass, aux, we determine either that the list is acyclic,
; or the length of its cycle; to detect a cycle, each kernel-pair at position n
; in the list is compared for eq?-ness to the kernel-pair at the most recent
; power-of-two index.  Each kernel-pair is compared to only one other, and we
; can't overshoot the beginning of the cycle by more than a factor of two, so
; this pass takes time linear in the number of kernel-pairs.  If there's no
; cycle, we're done.
;  (2) A second pass, aux2, determines where the cycle begins, by comparing
; each kernel-pair starting from the beginning of the list to the kernel-pair
; displaced to its right by exactly the cycle length (which we know, from the
; first pass).
;
; An alternative algorithm would be to use encapsulated integer marks on
; kernel-pairs; one would then require two passes of just the length of the
; improper list.  That algorithm would have to be in file
; "subfiles/kernel-pair.scm", since it would use private information about
; kernel-pair.  However, even if one did that, it would save less than fifty
; percent in traversal length, and the traversal steps could be significantly
; more expensive since they would involve modifying the encapsulated mark
; fields.  That's not even getting in to questions of reentrance and
; parallelization.
;
(define get-list-metrics
  (lambda (x)

    ; find the cycle length
    (define aux
      (lambda (current-x ; the item we're going to look at now
               current-n ; the number of kernel-pairs we've already passed
               old-x     ; an earlier kernel-pair to compare with this item
               old-n     ; the number of kernel-pairs preceding old-x
               next-n)   ; when to replace old-x
        (cond ((not (kernel-pair? current-x))
                 (list current-n (if (null? current-x) 1 0)
                       current-n 0))
              ((eq? current-x old-x)
                 (aux2 (- current-n old-n)))
              ((< current-n next-n)
                 (aux (kernel-cdr current-x) (+ 1 current-n)
                      old-x                  old-n
                                             next-n))
              (else
                 (aux (kernel-cdr current-x) (+ 1 current-n)
                      current-x              current-n
                                             (* 2 current-n))))))

    ; find the acyclic prefix length
    (define aux2
      (lambda (cycle-length)
        (let ((acyclic-prefix-length
                (aux3 x (kernel-list-tail x cycle-length) 0)))
          (list (+ acyclic-prefix-length cycle-length)
                0
                acyclic-prefix-length
                cycle-length))))

    ; find the acyclic prefix length
    (define aux3
      (lambda (x y n)
        (if (eq? x y)
            n
            (aux3 (kernel-cdr x) (kernel-cdr y) (+ 1 n)))))

    (if (kernel-pair? x)
        (aux (kernel-cdr x) 1 x 0 8)
        (list 0 (if (null? x) 1 0) 0 0))))

;
; Given a value, determines whether that value is a list in the Kernel sense,
; i.e., either a finite list terminated by nil, or a cyclic list.
;
(define kernel-list?
  (lambda (ls)
    (let* ((metrics  (get-list-metrics ls))
           (n  (cadr metrics))
           (c  (cadddr metrics)))
      (or (> n 0)
          (> c 0)))))

;
; Given a value, determines whether that value is a finite list.
;
(define finite-list?
  (lambda (ls)
    (> (cadr (get-list-metrics ls)) 0)))

;
; Given a value, determines whether that value is a cyclic list.
;
(define cyclic-list?
  (lambda (ls)
    (> (cadddr (get-list-metrics ls)) 0)))

;
; Given a cons-like procedure some-cons, returns a procedure that,
; given integer n-pairs, procedure proc, and kernel-list ls, where the length
; of ls is at least n-pairs, calls proc on each of the first n-pairs elements
; of ls, and returns a finite some-list of the results.
;
; This higher-order procedure captures the common structure of
; bounded-simple-map, which returns a finite mutable kernel-list, and
; bounded-simple-map->list, which returns a list.
;
(define make-bounded-simple-map
  (lambda (some-cons)
    (letrec ((mapper  (lambda (n-pairs proc ls)
                        (if (> n-pairs 0)
                            (some-cons (proc (kernel-car ls))
                                       (mapper (- n-pairs 1)
                                               proc
                                               (kernel-cdr ls)))
                            ()))))
      mapper)))

(define bounded-simple-map       (make-bounded-simple-map kernel-cons))
(define bounded-simple-map->list (make-bounded-simple-map cons))

;
; Given a kernel-list, returns a freshly allocated list with the same elements
; in the same order.
;
; If the resultant list certainly won't be mutated, use  kernel-list->list.
;
(define copy-kernel-list->list
  (lambda (ls)
    (bounded-simple-map->list (car (get-list-metrics ls))
                              (lambda (x) x)
                              ls)))

;
; Given mutable kernel-list ls, nonnegative integer a, and nonnegative integer
; c, where the number of kernel-pairs in ls is at least a+c, if c is zero does
; nothing, otherwise sets the kernel-cdr of the (a+c)th kernel-pair of ls to
; point to the (a+1)th kernel-pair of ls.  After mutation, ls has acyclic
; prefix length a and cycle length c.
;
(define kernel-encycle!
  (lambda (ls a c)
    (if (> c 0)
        (kernel-set-cdr! (kernel-list-tail ls (+ a c -1))
                         (kernel-list-tail ls a)))))

;
; Given procedure proc and kernel-list ls, calls proc on each element of ls
; (just once for each eq?-distinct position in ls), and returns
; a mutable kernel-list of the results structurally isomorphic to ls.
;
; For example, using curly braces for a kernel-list,
;
;       (simple-map (lambda (x) (+ x 5)) {1 2 #0={3 4 . #0#}})
;   ==>
;       {6 7 #0={8 9 . #0#}}
;
(define simple-map
  (lambda (proc ls)
    (let* ((metrics  (get-list-metrics ls))
           (p  (car metrics))
           (a  (caddr metrics))
           (c  (cadddr metrics)))
      (if (<= p 0)
          ()
          (let ((ls  (bounded-simple-map p proc ls)))
            (kernel-encycle! ls a c)
            ls)))))

;
; Given a nonempty kernel-list of kernel-lists lss, constructs a
; finite mutable kernel-list whose n^th element is a mutable kernel-list of
; the n^th elements of the kernel-lists in lss; each element of the resultant
; kernel-list has the topological structure of lss, and the length of the
; resultant kernel-list is just large enough to include every combination of
; elements from the kernel-lists in lss.  The result returned is a list whose
; first element is the resultant kernel-list, and whose second and third
; elements are the acyclic prefix length and cycle length that would encycle
; the resultant kernel-list to be traversal-isomorphic to the infinite
; transpose of lss.  If the kernel-lists in lss don't all have the same
; length, an error-message string is returned.
;
;       (transpose-lists '{{1 2 3} {4 5 6}})
;   ==>
;       ({{1 4} {2 5} {3 6}} 3 0)
;
;       (transpose-lists '#0={{1 #1={2 3 . #1#}} {4 5 #2={6 . #2#}} . #0#})
;   ==>
;       ({#0={1 4 . #0#} #1={2 5 . #1#} #2={3 6 . #2#} #3={2 6 . #3#}} 2 2)
;
(define transpose-lists
  (lambda (lss)

    (define get-results-metrics
      (lambda (so-far remaining)
        (if (null? remaining)
            so-far
            (let ((next       (car remaining))
                  (remaining  (cdr remaining)))
              (let ((sa  (caddr so-far))
                    (sc  (cadddr so-far))
                    (na  (caddr next))
                    (nc  (cadddr next)))
                (if (or (not (eq? (zero? sc) (zero? nc)))
                        (and (zero? sc) (zero? nc) (not (= sa na))))
                    "lists don't have the same length"
                    (let ((a  (max sa na))
                          (c  (if (zero? sc) sc (lcm sc nc))))
                      (get-results-metrics (list (+ a c) (cadr so-far) a c)
                                           remaining))))))))

    (define aux
      (lambda (k lss p a c)
        (if (<= k 0)
            ()
            (let ((x  (bounded-simple-map p kernel-car lss))
                  (y  (bounded-simple-map p kernel-cdr lss)))
              (kernel-encycle! x a c)
              (kernel-cons x (aux (- k 1) y p a c))))))

    (let* ((lss-metrics  (get-list-metrics lss))
           (lss-p        (car lss-metrics))
           (lss-a        (caddr lss-metrics))
           (lss-c        (cadddr lss-metrics)))
      (let* ((metrics-list  (bounded-simple-map->list
                              lss-p get-list-metrics lss))
             (results-metrics  (get-results-metrics (car metrics-list)
                                                    (cdr metrics-list))))
        (if (string? results-metrics)
            results-metrics
            (let ((rp  (car results-metrics))
                  (ra  (caddr results-metrics))
                  (rc  (cadddr results-metrics)))
              (list (aux rp lss lss-p lss-a lss-c)
                    ra rc)))))))

;
; Given procedure proc and nonempty kernel-list of kernel-lists lss,
; repeatedly calls proc with a single mutable kernel-list argument constructed
; by taking the n^th elements of each kernel-list in lss, for n from 1 to a
; large enough number to cover every combination of positions in the different
; kernel-lists.  Each argument has the topological structure of lss.  Returns
; a mutable kernel-list of the results of the calls to proc, with appropriate
; topological structure.  If the kernel-lists in lss have different lengths,
; the operation cannot be performed correctly, so an error-message string is
; returned instead.
;
;       (full-map (lambda (x) (apply + x)) '{{1 2 3} {4 5 6}})
;   ==>
;       {5 7 9}
;
;       (full-map (lambda (x) (apply + x))
;                 '{{1 #0={2 3 . #0#}} {4 5 #1={6 7 8 . #1#}}})
;   ==>
;       {5 7 #0={9 9 11 8 10 10 . #0#}}
;
(define full-map
  (lambda (proc lss)
    (let ((x  (transpose-lists lss)))
      (if (string? x)
          x
          (let ((ls  (car x))
                (a   (cadr x))
                (c   (caddr x)))
            (let ((ls  (bounded-simple-map (+ a c) proc ls)))
              (kernel-encycle! ls a c)
              ls))))))

;
; Given two structures x and y, either of which may be cyclic, determine
; whether they are equal?.
;
; A table is maintained to keep track of which constituents (kernel-pairs) of
; x do not have to be compared again to which constituents of y.  The table
; is a list; each element of this list is a pair, whose first element is a
; constituent of x, and whose subsequent elements are constituents of y that
; don't have to be recompared to it.
;
; There is no call for this tool to use encapsulated knowledge about the
; kernel-pair type, because marking individual kernel-pairs wouldn't help.
;
(define kernel-equal?
  (lambda (x y)

    (define table ())

    (define get-row
      (lambda (x)
        (let ((row  (assq x table)))
          (if (pair? row)
              row
              (let ((row  (list x)))
                (set! table (cons row table))
                row)))))

    (define is-in-row?
      (lambda (y row)
        (if (pair? (memq y row))
            #t
            (begin
              (set-cdr! row (cons y (cdr row)))
              #f))))

    (define aux
      (lambda (x y)
        (cond ((and (kernel-pair? x) (kernel-pair? y))
                 (if (is-in-row? y (get-row x))
                     #t
                     (and (aux (kernel-car x) (kernel-car y))
                          (aux (kernel-cdr x) (kernel-cdr y)))))
              ((and (kernel-number? x) (kernel-number? y))
                 (kernel-=? x y))
              ((and (string? x) (string? y))
                 (string=? x y))
              (else
                 (eq? x y)))))

    (aux x y)))

;
; Creates bindings for handling cyclic structures in a given environment.
;
(define bind-cyclic-primitives!
  (lambda (env)
    (add-bindings! env

    ; 'get-list-metrics
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (list->kernel-list (get-list-metrics (kernel-car operand-tree))))
    ;   1 1)

    ; 'finite-list?
    ; (unary-predicate->applicative finite-list?)

    ; 'countable-list?
    ; (unary-predicate->applicative kernel-list?)

    ; 'member?
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((object (kernel-car operand-tree))
    ;            (ls     (kernel-cadr operand-tree))
    ;            (p      (car (get-list-metrics ls))))
    ;       (letrec ((aux?  (lambda (k ls)
    ;                         (if (<= k 0)
    ;                             #f
    ;                             (or (kernel-equal? object (kernel-car ls))
    ;                                 (aux? (- k 1) (kernel-cdr ls)))))))
    ;         (aux? p ls))))
    ;   2 2 any? kernel-list?)

    ; 'memq?
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((object (kernel-car operand-tree))
    ;            (ls     (kernel-cadr operand-tree))
    ;            (p      (car (get-list-metrics ls))))
    ;       (letrec ((aux?  (lambda (k ls)
    ;                         (if (<= k 0)
    ;                             #f
    ;                             (or (eq? object (kernel-car ls))
    ;                                 (aux? (- k 1) (kernel-cdr ls)))))))
    ;         (aux? p ls))))
    ;   2 2 any? kernel-list?)

    ; 'list-tail
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((ls  (kernel-car operand-tree))
    ;            (k   (kernel-cadr operand-tree))
    ;            (p   (car (get-list-metrics ls))))
    ;       (if (< p k)
    ;           (error-pass (make-error-descriptor
    ;                         (list "List isn't long enough"
    ;                               " when calling #[operative list-tail]")
    ;                         (list "Operand tree: " (list operand-tree)))
    ;                       context)
    ;           (kernel-list-tail ls k))))
    ;   2 2 kernel-list? integer?)

    ; 'list-ref
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((ls  (kernel-car operand-tree))
    ;            (k   (kernel-cadr operand-tree))
    ;            (p   (car (get-list-metrics ls))))
    ;       (if (<= p k)
    ;           (error-pass (make-error-descriptor
    ;                         (list "List isn't long enough"
    ;                               " when calling #[operative list-ref]")
    ;                         (list "Operand tree: " (list operand-tree)))
    ;                       context)
    ;           (kernel-list-ref ls k))))
    ;   2 2 kernel-list? integer?)

    ; 'encycle!
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let* ((ls  (kernel-car operand-tree))
    ;            (a   (kernel-cadr operand-tree))
    ;            (c   (kernel-caddr operand-tree))
    ;            (p   (car (get-list-metrics ls))))
    ;       (cond ((< c 1)  ())
    ;             ((< p (+ a c))
    ;                (error-pass (make-error-descriptor
    ;                              (list "List isn't long enough"
    ;                                    " when calling #[operative encycle!]")
    ;                              (list "Operand tree: " (list operand-tree)))
    ;                            context))
    ;             ((immutable? (kernel-list-tail ls (+ a c -1)))
    ;                (error-pass (make-error-descriptor
    ;                              (list "Target is immutable"
    ;                                    " when calling #[operative encycle!]")
    ;                              (list "Operand tree: " (list operand-tree)))
    ;                            context))
    ;             (else  (kernel-encycle! ls a c))))
    ;     inert)
    ;   3 3 any? integer? integer?)

    ; 'map
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;     (let ((combiner  (unwrap (kernel-car operand-tree)))
    ;           (lss       (kernel-cdr operand-tree)))
    ;       (let ((result  (full-map (lambda (args)
    ;                                  (eval (kernel-cons combiner args)
    ;                                        env context))
    ;                                lss)))
    ;         (if (string? result)
    ;             (error-pass
    ;               (make-error-descriptor
    ;                 (list result ", when calling #[operative map]"))
    ;               context)
    ;             result))))
    ;   2 -1 applicative? kernel-list?)

    ;;;;;;;;;;;; doesn't look right
    ; 'append
    ; (action->checked-applicative
    ;   (lambda (operand-tree env context)
    ;
    ;     (define mutable-finite-list?
    ;       (lambda (ls)
    ;         (let* ((metrics  (get-list-metrics ls))
    ;                (p        (car metrics))
    ;                (c        (cadddr metrics)))
    ;           (and (zero? c)
    ;                (or (zero? p)
    ;                    (mutable? (kernel-list-tail ls (- p 1))))))))
    ;
    ;     (define check-operands
    ;       (lambda (n k operands)
    ;         (if (>= n k)
    ;             (if (mutable-finite-list? (kernel-car operands))
    ;                 (check-operands n (+ k 1) (kernel-cdr operands))
    ;                 (error-pass
    ;                   (make-error-descriptor
    ;                     (string-append "Operand #" (number->string k)
    ;                       (if (finite-list? (kernel-car operands))
    ;                           " is immutable"
    ;                           " has wrong type")
    ;                       " when calling #[operative append]")
    ;                     (list "Operand tree: " (list operand-tree)))
    ;                   context)))))
    ;
    ;     (define binary-append
    ;       (lambda (x y)
    ;         (if (null? x)
    ;             y
    ;             (cons (kernel-car x)
    ;                   (binary-append (kernel-cdr x) y)))))
    ;
    ;     (define bounded-append
    ;       (lambda (k lss)
    ;         (if (<= k 0)
    ;             ()
    ;             (binary-append (kernel-car lss)
    ;                            (bounded-append (- k 1) (kernel-cdr lss))))))
    ;
    ;     (define finite-append
    ;       (lambda (lss)
    ;         (if (null? lss)
    ;             ()
    ;             (let ((ls   (kernel-car lss))
    ;                   (lss  (kernel-cdr lss)))
    ;               (if (null? lss)
    ;                   ls
    ;                   (binary-append ls (finite-append lss)))))))
    ;
    ;     (define set-last!
    ;       (lambda (x y)
    ;         (if (null? (kernel-cdr x))
    ;             (kernel-set-cdr! x y)
    ;             (set-last! (kernel-cdr x) y))))
    ;
    ;     (let* ((metrics  (get-list-metrics operand-tree))
    ;            (p        (car metrics))
    ;            (a        (caddr metrics))
    ;            (c        (cadddr metrics)))
    ;       (if (zero? c)
    ;           (begin
    ;             (check-operands (- p 1) 1 operand-tree)
    ;             (finite-append operand-tree))
    ;           (begin
    ;             (check-operands p 1 operand-tree)
    ;             (let ((cycle  (bounded-append c (kernel-list-tail
    ;                                               operand-tree a))))
    ;               (set-last! cycle cycle)
    ;               (if (zero? a)
    ;                   cycle
    ;                   (let ((acyclic-prefix
    ;                           (bounded-append a operand-tree)))
    ;                     (set-last! acyclic-prefix cycle)
    ;                     acyclic-prefix)))))))
    ;   0 -1)
    ;;;;;;;;;;;;

      'equal?  (binary-predicate->applicative  kernel-equal?  any?)

      )))
