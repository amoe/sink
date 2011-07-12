#!r6rs
; This file is part of SINK, a Scheme-based Interpreter for Not-quite Kernel
; Copyright (c) 2009 John N. Shutt

;;;;;;;;;;;;
; contexts ;
;;;;;;;;;;;;
;
; The name "context" is used within the interpreter for what the Kernel
; programmer calls a "continuation".  The word "continuation" is used for
; Scheme continuations.
;
; A context has type 'continuation (because that's the name that should be
; visible within Kernel), and attributes 'receiver, 'parent, 'entry-guards,
; 'exit-guards, 'error-context, 'terminal-context, 'alist, and 'mark.  None
; of the attributes is directly accessible to clients.
;
;   'receiver is a continuation.  When an abnormal pass arrives at its
; destination, the abnormally passed value goes to the destination's receiver.
;
;   'parent is either nil or a context.  The parent (even if nil) contains
; this context, or equivalently, is an ancestor of this context.  Ancestry
; and containment are improper unless otherwise stated, i.e., a context is
; an ancestor of/contains itself.
;
;   'entry-guards and 'exit-guards are each a list of context/procedure pairs.
; Each pair is called a "guard", its context a "selector", and its procedure
; an "interceptor".
;   When an abnormal pass is scheduled, a list is made of interceptors to be
; called along the way.  At most one exit interceptor is selected from each
; context to be exited, in the order they will be exited, and then at most one
; entry interceptor from each context to be entered, in the order they will be
; entered.  For each exited context, the first exit guard is selected whose
; selector contains the destination of the abnormal pass; for each entered
; context, the first entry guard is selected whose selector contains the source
; of the abnormal pass.
;   Once the interceptors for the abnormal pass have been selected, they are
; used in series to transform the abnormally passed value; i.e., the value is
; passed as an argument to the first interceptor, the output of the first is
; passed as an argument to the second, etc.  The output of the last interceptor
; goes to the destination's receiver.
;
;   'error-context is a context.  When an error occurs, a descriptor of the
; error is abnormally passed to the error-context.
;
;   'terminal-context is a context.  Abnormally passing any value to the
; terminal-context requests termination of the interpreter.
;
;   'alist is a list of keyed-bindings, constructed by tools in file
; "subfiles/keyed.scm".
; 
;   'mark is a pair, unique to this context, whose car is a boolean.  Its
; car is #f except during ancestry-determination algorithms.  A context
; whose mark's car is #t is said to be "marked".  Marking contexts allows
; ancestry-determination algorithms to run in linear rather than quadratic
; time.
;

(library (subfiles context)
  (export context-keyed-lookup
          call-with-keyed-context
          call-with-guarded-context
          make-context
          context?
          context->applicative)
  (import (rnrs)
          (rnrs mutable-pairs)
          (subfiles object)
          (subfiles keyed)
          (subfiles error)
          (subfiles revision)
          (subfiles proxy-2)
          (subfiles applicative))

(define make-context
  (lambda (receiver parent entry-guards exit-guards
                    error-context terminal-context alist)
    (let ((name  (list #t))
          (mark  (list #f)))
      (lambda (message)
        (case message
          ((type)             'continuation)
          ((name)             name)
          ((receiver)         receiver)
          ((parent)           parent)
          ((entry-guards)     entry-guards)
          ((exit-guards)      exit-guards)
          ((mark)             mark)
          ((error-context)    error-context)
          ((terminal-context) terminal-context)
          ((alist)            alist))))))

(define context? (make-object-type-predicate 'continuation))

;
; call-with-guarded-context takes as arguments a procedure, parent context, and
; entry and exit guards; and passes as the single argument to the procedure
; a new context whose receiver is the return continuation from the call to
; call-with-guarded-context, whose parent and guards are as specified, and
; whose error-context terminal-context and alist are inherited from the parent.
;

(define call-with-guarded-context
  (lambda (proc parent entry-guards exit-guards)
    (call-with-current-continuation
      (lambda (receiver)
        (let ((error-context     (parent 'error-context))
              (terminal-context  (parent 'terminal-context))
              (alist             (parent 'alist)))
          (proc (make-context receiver parent entry-guards exit-guards
                              error-context terminal-context alist)))))))

;
; call-with-keyed-context takes as arguments a procedure, parent context, and
; key and value for a keyed binding; and passes as the single argument to
; the procedure a new context whose receiver is the return continuation from
; the call to call-with-keyed-context, whose parent is as specified, whose
; guard lists are empty, whose error-context and terminal-context are inherited
; from the parent, and whose alist is inherited from the parent except for the
; given keyed binding.
;

(define call-with-keyed-context
  (lambda (proc parent key value)
    (call-with-current-continuation
      (lambda (receiver)
        (let ((error-context     (parent 'error-context))
              (terminal-context  (parent 'terminal-context))
              (alist             (make-alist (parent 'alist) key value)))
          (proc (make-context receiver parent '() '()
                              error-context terminal-context alist)))))))

;
; Given the internal key for a keyed variable, and a context, looks up the
; value of that keyed variable in that context's alist.  Returns the value
; if found, or signals an error.
;

(define context-keyed-lookup
  (lambda (key context)
    (let ((binding  (alist-lookup key (context 'alist))))
      (if (pair? binding)
          (cdr binding)
          (error-pass
            (make-error-descriptor
              "Attempted to look up an unbound keyed dynamic variable"
              (list "in " (list context)))
            context)))))

;
; Given a context, constructs an applicative that abnormally passes its
; argument tree to that context.
;
(define context->applicative
  (lambda (dest-context)
    (let ((this
            (action->applicative
              (lambda (operand-tree env source-context)
                (abnormally-pass operand-tree source-context dest-context)))))
      (designate-name-inheritor! (unwrap this) dest-context)
      this)))


(set-version (list 0.0 1)
             (list 0.1 1))
(set-revision-date 2009 9 7)

)
