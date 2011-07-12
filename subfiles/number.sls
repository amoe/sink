#!r6rs
; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;
; numbers ;
;;;;;;;;;;;
;
; Infinities are incompletely supported.
;
; Exact and inexact, positive and negative, infinty have types
; 'e+infinity 'e-infinity 'i+infinity 'i-infinity, and no attributes.
;
; Complex numbers are not supported.  They would be a substantial addition,
; because of their interactions with infinities.
;

(library (subfiles number)
  (export exact-positive-infinity
          exact-negative-infinity
          inexact-positive-infinity
          inexact-negative-infinity
          kernel-number?
          kernel-=?
          kernel-real?
          kernel-exact?
          kernel-add
          kernel-binary-add
          kernel-negate
          kernel-multiply
          kernel-binary-multiply
          kernel-invert
          kernel-<?
          kernel-<=?
          kernel->?
          kernel->=?
          infinity?
          exact-positive-infinity?
          exact-negative-infinity?
          inexact-positive-infinity?
          inexact-negative-infinity?
          positive-infinity?
          kernel-zero?
          negative-infinity?)

  (import (rnrs)
          (subfiles object)
          (subfiles kernel-pair)
          (subfiles revision)
          (subfiles cycles))

(define exact-positive-infinity
  (let ((name  (list #f)))
    (lambda (message)
      (case message
        ((type) 'e+infinity)
        ((name) name)))))

(define exact-negative-infinity
  (let ((name  (list #f)))
    (lambda (message)
      (case message
        ((type) 'e-infinity)
        ((name) name)))))

(define inexact-positive-infinity
  (let ((name  (list #f)))
    (lambda (message)
      (case message
        ((type) 'i+infinity)
        ((name) name)))))

(define inexact-negative-infinity
  (let ((name  (list #f)))
    (lambda (message)
      (case message
        ((type) 'i-infinity)
        ((name) name)))))

(define exact-positive-infinity?    (make-object-type-predicate 'e+infinity))
(define exact-negative-infinity?    (make-object-type-predicate 'e-infinity))
(define inexact-positive-infinity?  (make-object-type-predicate 'i+infinity))
(define inexact-negative-infinity?  (make-object-type-predicate 'i-infinity))

(define positive-infinity?
  (make-object-type-predicate 'e+infinity 'i+infinity))

(define negative-infinity?
  (make-object-type-predicate 'e-infinity 'i-infinity))

(define exact-infinity?
  (make-object-type-predicate 'e+infinity 'e-infinity))

(define inexact-infinity?
  (make-object-type-predicate 'i+infinity 'i-infinity))

(define infinity?
  (make-object-type-predicate 'e+infinity 'e-infinity
                              'i+infinity 'i-infinity))

(define kernel-real?
  (lambda ls
    (or (null? ls)
        (and (or (infinity? (car ls))
                 (real? (car ls)))
             (apply kernel-real? (cdr ls))))))

(define kernel-number? kernel-real?)

(define kernel-<?
  (lambda (x y)
    (cond ((real? x)
             (if (real? y)
                 (< x y)
                 (positive-infinity? y)))
          ((negative-infinity? x)
             (not (negative-infinity? y)))
          (else
             #f))))

(define kernel-<=?
  (lambda (x y)
    (cond ((real? x)
             (if (real? y)
                 (<= x y)
                 (positive-infinity? y)))
          ((negative-infinity? x)
             #t)
          (else
             (positive-infinity? y)))))

(define kernel-=?
  (lambda (x y)
    (cond ((real? x)
             (if (real? y)
                 (= x y)
                 #f))
          ((negative-infinity? x)
             (negative-infinity? y))
          (else
             (positive-infinity? y)))))

(define kernel->?
  (lambda (x y)
    (cond ((real? x)
             (if (real? y)
                 (> x y)
                 (negative-infinity? y)))
          ((negative-infinity? x)
             #f)
          (else
             (not (positive-infinity? y))))))

(define kernel->=?
  (lambda (x y)
    (cond ((real? x)
             (if (real? y)
                 (>= x y)
                 (negative-infinity? y)))
          ((negative-infinity? x)
             (negative-infinity? y))
          (else
             #t))))

(define kernel-zero?
  (lambda (x)
    (and (real? x)
         (zero? x))))

(define kernel-exact?
  (lambda (x)
    (if (real? x)
        (exact? x)
        (exact-infinity? x))))

(define describe-kernel-number
  (lambda (x)
    (if (real? x)
        (number->string x)
        (describe-object x))))

;
; The arithmetic operations return a string on error.
;

(define kernel-negate
  (lambda (x)
    (cond ((string? x)  x)
          ((real? x)  (- x))
          ((exact-positive-infinity? x)  exact-negative-infinity)
          ((exact-negative-infinity? x)  exact-positive-infinity)
          ((inexact-positive-infinity? x)  inexact-negative-infinity)
          (else  inexact-positive-infinity))))

(define kernel-invert
  (lambda (x)
    (cond ((string? x)  x)
          ((real? x)  (if (zero? x)
                          "Division by zero"
                          (/ 1 x)))
          ((inexact-infinity? x)  0.0)
          (else 0))))

(define kernel-binary-multiply
  (lambda (x y)

    (define indeterminate
      (lambda ()
        (string-append "Indeterminate product of "
                       (describe-kernel-number x) " with "
                       (describe-kernel-number y))))

    (cond ((string? y)  y)
          ((string? x)  x)
          ((real? x)
             (cond ((real? y)  (* x y))
                   ((zero? x)  (indeterminate))
                   ((exact? x)  (if (> x 0) y (kernel-negate y)))
                   ((positive-infinity? y)
                      (if (> x 0) inexact-positive-infinity
                                  inexact-negative-infinity))
                   (else
                      (if (> x 0) inexact-negative-infinity
                                  inexact-positive-infinity))))
          ((real? y)
             (cond ((zero? y)  (indeterminate))
                   ((exact? y)  (if (> y 0) x (kernel-negate x)))
                   ((positive-infinity? x)
                      (if (> y 0) inexact-positive-infinity
                                  inexact-negative-infinity))
                   (else
                      (if (> y 0) inexact-negative-infinity
                                  inexact-positive-infinity))))
          ((exact-positive-infinity? x)  y)
          ((exact-negative-infinity? x)  (kernel-negate y))
          ((inexact-positive-infinity? x)
             (if (positive-infinity? y) inexact-positive-infinity
                                        inexact-negative-infinity))
          (else
             (if (positive-infinity? y) inexact-negative-infinity
                                        inexact-positive-infinity)))))

(define kernel-binary-add
  (lambda (x y)

    (define indeterminate
      (lambda ()
        (string-append "Indeterminate sum of "
                       (describe-kernel-number x) " with "
                       (describe-kernel-number y))))

    (cond ((string? y)  y)
          ((string? x)  x)
          ((real? x)
             (cond ((real? y)  (+ x y))
                   ((exact? x)  y)
                   ((positive-infinity? y)  inexact-positive-infinity)
                   (else                    inexact-negative-infinity)))
          ((positive-infinity? x)
             (cond ((real? y)  (if (exact? y) x inexact-positive-infinity))
                   ((negative-infinity? y)        (indeterminate))
                   ((exact-positive-infinity? y)  x)
                   (else  y)))
          (else
             (cond ((real? y)  (if (exact? y) x inexact-negative-infinity))
                   ((positive-infinity? y)        (indeterminate))
                   ((exact-negative-infinity? y)  x)
                   (else y))))))

(define kernel-bounded-reduce
  (lambda (k ls binary identity)
    (cond ((> k 1)  (binary (kernel-car ls)
                            (kernel-bounded-reduce
                              (- k 1) (kernel-cdr ls) binary identity)))
          ((= k 1)  (kernel-car ls))
          (else identity))))

(define kernel-bounded-test?
  (lambda (k ls unary?)
    (or (<= k 0)
        (and (unary? (kernel-car ls))
             (kernel-bounded-test? (- k 1) (kernel-cdr ls) unary?)))))

(define kernel-add
  (lambda (ls)

    (define bounded-sum
      (lambda (k ls)
        (kernel-bounded-reduce
          k ls kernel-binary-add 0)))

    (define bounded-zero?
      (lambda (k ls)
        (kernel-bounded-test? k ls kernel-zero?)))

    (let* ((metrics  (get-list-metrics ls))
           (a  (caddr metrics))
           (c  (cadddr metrics)))
      (kernel-binary-add
        (bounded-sum a ls)
        (if (zero? c)
            0
            (let* ((cycle  (kernel-list-tail ls a))
                   (sum-c  (bounded-sum c cycle)))
              (if (kernel-zero? sum-c)
                  (if (bounded-zero? c cycle)
                      sum-c
                      "Sum of cycle doesn't converge")
                  (kernel-binary-multiply
                    sum-c
                    exact-positive-infinity))))))))

(define kernel-multiply
  (lambda (ls)

    (define bounded-product
      (lambda (k ls)
        (kernel-bounded-reduce
          k ls kernel-binary-multiply 1)))

    (define bounded-nonnegative?
      (lambda (k ls)
        (kernel-bounded-test? k ls
          (lambda (x) (kernel->=? x 0)))))

    (define bounded-one?
      (lambda (k ls)
        (kernel-bounded-test? k ls
          (lambda (x) (kernel-=? x 1)))))

    (let* ((metrics  (get-list-metrics ls))
           (a  (caddr metrics))
           (c  (cadddr metrics)))
      (kernel-binary-multiply
        (bounded-product a ls)
        (if (zero? c)
            1
            (let* ((cycle      (kernel-list-tail ls a))
                   (product-c  (bounded-product c cycle)))
              (cond ((or (kernel-zero? product-c)
                         (bounded-one? c cycle))
                       product-c)
                    ((or (not (bounded-nonnegative? c cycle))
                         (kernel-=? product-c 1))
                       "Product of cycle doesn't converge")
                    (else
                       (kernel-binary-multiply
                         product-c
                         (if (kernel-<? product-c 1)
                             0
                             exact-positive-infinity))))))))))


(set-version (list 0.0 0)
             (list 0.1 0))
(set-revision-date 2007 8 4)

)
