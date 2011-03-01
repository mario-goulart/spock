;;;; opt.scm - optimizer


(define (optimize form state)
  (let-syntax ((result
		(syntax-rules ()
		  ((_ (a b c) x body ...)
		   (match-let ((#(a b c) x)) body ...)))))
    (let ((debug (test-option 'debug state))
	  (block (test-option 'block state))
	  (local-env #f))
      ;; (walk FORM ENV DEST LOC) -> #(FORM' VALUE SIDEEFFECT?)
      ;;
      ;;   ENV = ((<variable1> <value1> <used?>) ...)
      ;;   VALUE = '<const> | <variable> | #f | (%void)
      (define (walk form e dest loc)
	(define (return form val se?)
	  ;;(pp `(RETURN: ,form ,val ,se?))
	  (vector form val se?))
	;;(pp `(WALK: ,form))
	(match form
	  ;; * propagate variable value, if known
	  ((? symbol?)
	   (let ((px (propagate form form #f)))
	     (cond ((and (symbol? px) (assq px e)) =>
		    (lambda (a) (set-car! (cddr a) #t)))) ; mark as used
	     (return 
	      (if (eq? px form)
		  form
		  (note
		   loc state
		   px
		   (string-append
		    "replaced reference to `" (symbol->string form) "' with: ")
		   px))
	      form #f)))
	  ;; * "straighten" `let' forms
	  ;;
	  ;;XXX disabled, because it results in too deeply nested
	  ;;    functions (r4rstest.scm), even for v8
;; 	  (('let ((v1 ('let ((v2 x)) z))) y)
;; 	   (walk `(let ((,v2 ,x)) ; alpha-conversion should make this safe
;; 		    (let ((,v1 ,z)) ,y))
;; 		 e dest loc))
	  ;; * remove side-effect free unused bindings
	  (('let (('%unused x)) y)
	   (result
	    (x2 vx se) (walk x e #f loc)
	    (if se
		(result 
		 (y2 yv yse) (walk y e dest loc)
		 (return
		  `(let ((%unused ,x2))
		     ,y2)
		  yv #t))
		(note
		 loc state
		 (walk y e dest loc)
		 "removed side-effect free expression: " x2))))
	  ;; * remove unused bindings
	  (('let ((v x)) y)
	   (result
	    (x2 xv xse) (walk x e v loc)
	    (let ((b (list v xv #f)))
	      (fluid-let ((local-env (cons b local-env)))
		(result
		 (y2 yv yse) (walk y (cons b e) dest loc)
		 (return
		  (cond ((caddr b)	; variable used?
			 `(let ((,v ,x2))
			    ,y2))
			(xse		; unused, but bound value has side-effect?
			 `(let ((%unused ,x2))
			    ,y2))
			(else 
			 (note loc state y2 "removed unused binding: " v)))
		  yv
		  (or xse yse)))))))
	  (('quote c) (return form form #f))
	  ;; * remove self-assignment
	  (('set! v x)
	   (result
	    (x2 xv _) (walk x e v loc)
	    (cond ((eq? v x2)
		   (note
		    loc state
		    (return '(%void) '(%void) #f)
		    "removing self-assignment: " v))
		  (else
		   (cond ((and xv (assq v local-env)) =>
			  (lambda (a)	; assign new value, if local
			    (set-car! (cddr a) #t) ; mark as used
			    (set-car! (cdr a) xv)))
			 ((assq v e) =>	; otherwise invalidate
			  (lambda (a)
			    (set-car! (cddr a) #t)
			    (set-car! (cdr a) #f))))
		   (return `(set! ,v ,x2) '(%void) #t)))))
	  ;;
	  (('lambda llist body)
	   (match-let (((vars _) (parse-llist llist)))
	     (fluid-let ((local-env (map (cut list <> #f #f) vars)))
	       (result
		(body2 _ _) 
		(walk body (append local-env e) #f dest)
		(let ((form2 `(lambda ,llist ,body2)))
		  (return form2 #f #f))))))
	  ;;
	  (('%void) (return form form #f))
	  ;; * replace with constant, if argument known to be void or non-void
	  (('%void? x)
	   (result
	    (x2 xv se?) (walk x e #f loc)
	    (match xv
	      ('(%void) 
	       (note
		loc state
		(return
		 (if se? 
		     `(let ((%unused ,x2)) ''#t)
		     ''#t)
		 ''#t
		 se?)
		"removed voidness-test (true)"))
	      (('quote _) 
	       (note 
		loc state
		(return
		 (if se? 
		     `(let ((%unused ,x2)) ''#f)
		     ''#f)
		 ''#f
		 se?)
		"removed voidness-test (false)"))
	      (_ (return `(%void? ,x2) #f se?)))))
	  ;; * "straighten" binding inside condition
	  (('if ('let binding x) . more)
	   (let ((t (temp)))
	     (walk
	      `(let ,binding 
		 (let ((,t ,x))
		   (if ,t ,@more)))
	      e dest loc)))
	  ;; * replace side-effect free known condition and/or side-effect free branches
	  (('if x y z)
	   (result 
	    (x2 xv xse) (walk x e #f loc)
	    (cond ((and (pair? xv) (eq? 'quote (car xv))) ; constant condition?
		   (let ((b (if (cadr xv) y z)))
		     (note 
		      loc state
		      (if xse
			  (result ; execute condition, but ignore result
			   (b2 bv bse) (walk b e dest loc)
			   (return
			    `(let ((%unused ,x2)) ,b2)
			    b2 #t))
			  (note
			   loc state
			   (walk b e dest loc)	; drop alternative branch
			   "removed side-effect free conditional branch for: " x2))
		      "constant condition in conditional: " x2)))
		  (else
		   (result
		    (y2 yv yse) (walk y e dest loc)
		    (result
		     (z2 zv zse) (walk z e dest loc)
		     (return
		      `(if ,x2 ,y2 ,z2)
		      #f 
		      (or xse yse zse))))))))
	  ;;
	  (('%host-ref _) 
	   (return form #f debug))
	  ;;
	  (('%host-set! p x)
	   (result 
	    (x2 xv xse) (walk x e p loc)
	    (return `(%host-set! ,p ,x2) #f #t)))
	  ;;
	  (('%property-ref _)
	   (return form #f debug))
	  ;;
	  (('%check type x) 
	   ;;XXX remove unneeded `%check's (constant known value)
	   (result 
	    (x2 xv xse) (walk x e dest loc)
	    (return `(%check ,type ,x2) #f (or xse debug))))
	  ;; ((%property-ref PARTS) X) ~> (%property-ref PARTS X)
	  ((('%property-ref parts) x)
	   (walk `(%property-ref ,parts ,x) e dest loc))
	  ;;
	  (('%property-ref parts x) 
	   (result 
	    (x2 xv xse) (walk x e dest loc)
	    (return `(%property-ref ,parts ,x2) #f (or xse debug))))
	  ;;
	  (('%property-set! p x y)
	   (result 
	    (x2 _ xse) (walk x e #f loc)
	    (result 
	     (y2 _ yse) (walk y e p loc)
	     (return `(%property-set! ,p ,x2 ,y2) #f #t))))
	  ;;
	  (('%code . code)
	   (return form #f #t))
	  ;;
	  (('%native-lambda . code)
	   (return form #f #f))
	  ;;
	  (('%inline name args ...)
	   (result
	    (xs _ _) (walk-many args e loc)
	    (return `(%inline ,name ,@xs) #f #t)))
	  ;;
	  (('%new args ...)
	   (result
	    (xs _ _) (walk-many args e loc)
	    (return `(%new ,@xs) #f #t)))
	  ;;
	  (('%global-ref _)
	   (return form #f debug))
	  ;; * remove global, if unused, if in block-mode and value has no side-effects
	  ;; * remove self-assignment
	  (('%global-set! v x)		
	   (result
	    (x2 _ se) (walk x e v loc)
	    (cond ((eq? v x2)
		   (note 
		    loc state 
		    (return '(%void) '(%void) #f)
		    "removing global self-assignment: " v))
		  ((and block (not se)
			(not (memq v referenced)))
		   (set! dropped (cons v dropped))
		   (note
		    loc state
		    (return '(%void) '(%void) #f)
		    "dropped unused global variable assignment: "
		    v))
		  (else
		   (return `(%global-set! ,v ,x2) '(%void) #t)))))
	  ;;
	  (('%loop llist body)
	   (result 
	    (x2 xv xse) (walk body e dest loc)
	    (return `(%loop ,llist ,x2) xv xse)))
	  ;;
	  (('%continue . xs)
	   (result 
	    (xs2 _ se) (walk-many xs e loc)
	    (return `(%continue ,@xs2) #f se))) ;XXX se?
	  ;;
	  ((op args ...)
	   (for-each (lambda (a) (set-car! (cdr a) #f)) e) ; invalidate all variables in env
	   (result
	    (xs2 _ se) (walk-many form e loc) ;XXX should we re-walk if op changed?
	    (return xs2 #f #t)))
	  (_ (error "opt: invalid form" form))))
      (define (walk-many forms e loc)
	(let loop ((forms forms) (xs '()) (vs '()) (se #f))
	  (if (null? forms)
	      (vector (reverse xs) (reverse vs) se)
	      (result
	       (x2 xv xse) (walk (car forms) e #f loc)
	       (loop (cdr forms) 
		     (cons x2 xs)
		     (cons xv vs)
		     (or se xse))))))
      ;; replace expression with known value, if possible and no side effects are caused
      (define (propagate exp val se)
	(if se
	    exp
	    (let loop ((val val))
	      (cond ((and (pair? val) 
			  (or (and (eq? 'quote (car val))
				   ;; do not propagate complex constants
				   (not (pair? (cadr val)))
				   (not (vector? (cadr val))))
			      (eq? '%void (car val))))
		     val)
		    ((and (symbol? val) (assq val local-env)) =>
		     (lambda (a)
		       (if (cadr a)
			   (loop (cadr a))
			   val)))
		    (else exp)))))
      (result
       (form2 _ _) (walk form '() #f #f)
       form2))))
