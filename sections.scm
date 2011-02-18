;;;; sections.scm - section handling for internal xref database


;; this must be portable syntax-rules, since it runs in the
;; host implementation

(define-syntax define-library-section
  (syntax-rules (default strict depends define define-syntax define-inline)
    ((_ "walk" sec ()) 
     (void))
    ((_ "walk" sec ((default def ...) . more))
     (begin
       (define-library-section "walk-defs" sec (def ...))
       (define-library-section "walk" sec more)))
    ((_ "walk" sec ((strict def ...) . more))
     (begin
       (define-library-section "walk-defs" sec (def ...))
       (define-library-section "walk" sec more)))
    ((_ "walk" sec ((depends dep ...) . more))
     (begin
       (put! 'sec 'depends '(dep ...))
       (define-library-section "walk" sec more)))
    ((_ "walk" sec (clause . more))
     (define-library-section "walk" sec more))
    ((_ "walk-defs" sec ())
     (void))
    ((_ "walk-defs" sec ((define (name . llist) body ...) . more))
     (begin
       (put! 'name 'defined 'sec)
       (define-library-section "walk-defs" sec more)))
    ((_ "walk-defs" sec ((define name val) . more))
     (begin
       (put! 'name 'defined 'sec)
       (define-library-section "walk-defs" sec more)))
    ((_ "walk-defs" sec ((define-syntax name val) . more))
     (begin
       (put! 'name 'defined 'sec)
       (define-library-section "walk-defs" sec more)))
    ((_ "walk-defs" sec ((define-inline (name . llist) body ...) . more))
     (begin
       (put! 'name 'defined 'sec)
       (define-library-section "walk-defs" sec more)))
    ((_ "walk-defs" sec (form . more))
     (define-library-section "walk-defs" sec more))
    ((_ sec clauses ...)
     (define-library-section "walk" sec (clauses ...)))))
