;;;; syntax.scm - various useful macros


(define-syntax define-syntax-rule
  (syntax-rules ___ ()
    ((_ (name args ___) rule)
     (define-syntax name
       (syntax-rules ()
	 ((_ args ___) rule))))))

(define-syntax-rule (when x y z ...)
  (if x (begin y z ...)))

(define-syntax-rule (unless x y z ...)
  (if (not x) (begin y z ...)))

(define-syntax cut
  (syntax-rules (<> <...>)
    ;; construct fixed- or variable-arity procedure:
    ((_ "1" (slot-name ...) (proc arg ...))
     (lambda (slot-name ...) (proc arg ...)))
    ((_ "1" (slot-name ...) (proc arg ...) <...>)
     (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))
    ;; process one slot-or-expr
    ((_ "1" (slot-name ...)   (position ...)      <>  . se)
     (cut "1" (slot-name ... x) (position ... x)        . se))
    ((_ "1" (slot-name ...)   (position ...)      nse . se)
     (cut "1" (slot-name ...)   (position ... nse)      . se))
    ((_ . slots-or-exprs)
     (cut "1" () () . slots-or-exprs))) )

(define-syntax fluid-let
  (syntax-rules ()
    ((_ ((v1 e1) ...) b1 b2 ...)
     (fluid-let "temps" () ((v1 e1) ...) b1 b2 ...))
    ((_ "temps" (t ...) ((v1 e1) x ...) b1 b2 ...)
     (let ((temp e1))
       (fluid-let "temps" ((temp e1 v1) t ...) (x ...) b1 b2 ...)))
    ((_ "temps" ((t e v) ...) () b1 b2 ...)
     (let-syntax ((swap!
                   (syntax-rules ()
                     ((swap! a b)
                      (let ((tmp a))
                        (set! a b)
                        (set! b tmp))))))
       (dynamic-wind
        (lambda ()
          (swap! t v) ...)
        (lambda ()
          b1 b2 ...)
        (lambda ()
          (swap! t v) ...))))))

(define-syntax-rule (begin1 x1 x2 ...)
  (%call-with-saved-values
   (lambda () x1)
   (lambda () (begin x2 ...))))

(define-syntax-rule (syntax-error msg arg ...)
  (%syntax-error msg arg ...))

(define-syntax-rule (new class arg ...)
  (%new class arg ...))

(define-syntax define-entry-point
  (syntax-rules ()
    ((_ (name . llist) body ...) 
     (define-entry-point name (lambda llist body ...)))
    ((_ name x)
     (begin
       (define name x)
       (define (%host-set! 'name (callback name)))))))

(define-syntax-rule (define-native name ...)
  (begin
    (define-syntax name
      (native (%host-ref 'name)))
    ...))

(define-syntax-rule (define-native-method name ...)
  (begin
    (define-syntax name
      (native-method (%host-ref 'name)))
    ...))

(define-syntax set!
  (let-syntax ((primitive-set! set!))
    (syntax-rules ()
      ((_ (prop x) y)
       (%property-set! 'prop x y))
      ((_ var x) 
       (primitive-set! var x)))))

(define-syntax-rule (optional x y)
  (if (pair? x) (car x) y))
