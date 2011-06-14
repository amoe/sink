; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; revision and date tracking ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; This is just a simple device for coordinating decentralized information on
; revision numbers and dates, so that when modifying just one source file,
; resultant changes to revision information are localized within that file.
;
; Each source file should call procedures set-version and set-revision-date.
;
; set-revision-date simply takes a year number, month number (1--12), and day
; number (1--31).
;
; set-version takes any number of arguments, each of which is a list whose
; first element is a real, and whose second element is an integer.
;
;   The first, real element is a version number that the file supports.  The
; version number of the entire program is the largest version number that is
; supported by *all* of the source files (precisely, the largest version
; number supported by all of the calls to set-version).
;
;   The second, integer element is a modification count.  For each version
; number, the *sum* of all modification counts is maintained.  Whichever
; version is chosen as the largest supported by all files, its total
; modification count is reported as a suffix to the version number reported by
; procedure get-version.
;
;   When a source file is modified, increment the modification counts for all
; its supported versions.  Some earlier version numbers may no longer be
; supported by the file in its new form, in which case, delete those version
; numbers from the list.  When a newly supported version number is added, its
; modification count should be zero.
;   When a source file is certified for a later version without being modified,
; the new version number is added with modification count zero, without
; changing the modification counts for versions already supported.  In this
; case the revision date doesn't have to be changed.
;   When modifying a source file without adding any new version number to it,
; there is no need to increment the modification cound of any version number
; that the interpreter as a whole has yet to achieve.
;

(define set-version       ())
(define get-version       ())

(define set-revision-date ())
(define get-revision-date ())

(let ((first-flag     #t)   ; cleared by first call to set-version
      (known-versions ())   ; list of records of the form (version count)
      (known-year     2007)
      (known-month    8)
      (known-day      4))

  (define filter
    (lambda (proc? ls)
      (cond ((null? ls)        ls)
            ((proc? (car ls))  (cons (car ls) (filter proc? (cdr ls))))
            (#t                (filter proc? (cdr ls))))))

  (set! set-version
    (lambda versions ; list of records of the form (version count)
      (if first-flag
          (begin
            (set! first-flag #f)
            (set! known-versions versions))
          (set! known-versions
            (filter (lambda (rec)
                      (let ((rec2  (assoc (car rec) versions)))
                        (if (pair? rec2)
                            (begin
                              (set-car! (cdr rec) (+ (cadr rec) (cadr rec2)))
                              #t)
                            #f)))
                    known-versions)))))

  ;
  ; Set revision year, month, and day to be no earlier than given values.
  ;
  (set! set-revision-date
    (lambda (new-year new-month new-day)
      (cond ((< new-year known-year))
            ((> new-year known-year)
               (set! known-year  new-year)
               (set! known-month new-month)
               (set! known-day   new-day))
            ((< new-month known-month))
            ((> new-month known-month)
               (set! known-month new-month)
               (set! known-day   new-day))
            ((> new-day known-day)
               (set! known-day   new-day)))))

  ;
  ; Get a string describing the known version number, revision number, and
  ; revision letter.
  ;
  (set! get-version
    (lambda ()
      (if (null? known-versions)
          "unnormalized."
          (let* ((v  (apply max (map car known-versions)))
                 (c  (cadr (assoc v known-versions))))
            (if (zero? c)
                (string-append (number->string v))
                (string-append (number->string v)
                               " m "
                               (number->string c)))))))

  (set! get-revision-date
    (lambda ()
      (string-append
        (number->string known-day)
        " "
        (list-ref (list "January"   "February" "March"    "April"
                        "May"       "June"     "July"     "August"
                        "September" "October"  "November" "December")
                  (- known-month 1))
        " "
        (number->string known-year)))))
