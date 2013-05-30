;;;; bind.scm - highlevel foreign interface
;
; Binding syntax:
;
; ["function"] ID "(" [ARG {"," ARG}] ")" ["{" ... "}" | ";"]
; "var" ID ["=" TYPE] {"," ID ["=" TYPE]} [";"]
;
; ARG = TYPE [ "..." ]
;     | "..."
; TYPE = "number" | "string" | "function" | "object" | "null" | "undefined"
;      | "[" [ARG {"," ARG}] "]"
;      | "_" | "*"


;; needs honu.scm and match.scm

(define (method-name? str)
  (and (memv #\. (string->list str)) #t))

(define (method-name-split str)
  (if (char-upper-case? (string-ref str 0))
      str
      (let ((res (string-split str ".")))
         (if (= (length res) 1)
             (car res)
             (list (car res) (foldl (cut string-append <> "." <>)
                                    "" (cdr res)))))))

(define parse-bindings 
  (let ((semicolon (string->symbol ";"))
	(comma (string->symbol ","))
	(dot (string->symbol ".")))
    (lambda (str)
      (let ((tokens
	     (with-input-from-string str
	       (lambda ()
		 (let loop ((xs '()))
		   (let ((x (read-honu)))
		     (if (eof-object? x)
			 (reverse xs)
			 (loop (cons x xs))))))))
	    (code '()))
	(define (add expr)
	  (set! code (cons expr code)))
	(define (parse t)
	  (match t
	    (() (reverse code))
	    (('function . more) 
	     (parse (function more)))
	    (('var . more)
	     (parse (var more)))
	    (((? symbol?) . _) 
	     (parse (function t)))
	    (_ (error "invalid binding syntax" t))))
	(define (function t0)
	  (match-let (((name . t) (parse-name t0)))
	    (match t
	      ((('%parens . args) . t)
	       (let ((tl (typelist args)))
		 (match t
		   (((or (? semicolon?) ('%braces . _)) . t2)
		    (set! t t2))
		   (_ #f))
		 (add (generate-function-binding name tl))
		 t))
	      (_ (error "invalid function binding syntax" t0)))))
	(define (var t0)
	  (match-let (((name . t) (parse-name t0)))
	    (let ((type #f))
	      (match t
		(('= type2 . t2)
		 (set! type (parse-type type2))
		 (set! t t2))
		(_ #f))
	      (add (generate-variable-binding name type))
	      (match t
		(((? semicolon?) . t2) t2)
		(((? comma?) . t2) (var t2))
		(_ t)))))
	(define (semicolon? t) (eq? t semicolon))
	(define (comma? t) (eq? t comma))
	(define (dot? t) (eq? t dot))
	(define (dots? t) (eq? t '...))
	(define (parse-type type)
	  (case type
	    ((number string object null undefined function) type)
	    ((_ *) #f)
	    (else 
	     (match type
	       (('%brackets) '())
	       (('%brackets . args) (typelist args))
	       (_ (error "invalid binding type" type))))))
	(define (typelist lst)
	  (let loop ((lst lst) (tl '()))
	    (match lst
	      (() (reverse tl))
	      (((? dots?)) (reverse (cons '... tl)))
	      ((type (? dots?)) 
	       (reverse (cons (vector (parse-type type)) tl)))
	      ((type (? comma?) . lst)
	       (loop lst (cons (parse-type type) tl)))
	      ((type)
	       (reverse (cons (parse-type type) tl))))))
	(define (parse-name t)
	  (match t
	    (((? symbol? s) . t)
	     (let loop ((t t) (name (symbol->string s)))
	       (match t
		 (((? dot?) (? symbol? s2) . t)
		  (loop t (string-append name "." (symbol->string s2))))
		 (_ (cons name t)))))
	    (_ (error "invalid binding name" t))))
	(parse tokens)
	`(begin ,@(reverse code))))))

;; these must generate expanded code, as parsing of bind expressions happens
;; during canonicalization
(define (generate-function-binding name tl)
  ;;XXX doesn't handle methods (do we need to?)
  (let ((name (method-name-split name)))
    (define (finish wraps tmps rest rtmp)
      (let ((sname
	     (if (string? name)
		 name
		 (cadr name))))
	`(define-syntax ,(string->symbol sname)
	   (syntax-rules ()
	     ((_ ,@tmps ,@(if rest (list rtmp '...) '()))
	      (%inline 
	       ,sname
	       ,@wraps
	       ,@(cond ((eq? #t rest) (list rtmp '...))
		       (rest (list rest '...))
		       (else '()))))))	  ))
    (define (wrap type tmp)
      (case type
	((number function object null undefined)
	 `(%check ,(symbol->string type) ,tmp))
	((string)
	 `(%string->jstring ,tmp))
	(else
	 (if (pair? type)
	     (wrap-vector type tmp)
	     tmp))))
    (define (wrap-vector tl tmp)
      (let loop ((tl tl) (cs '()) (f #f) (i 0))
	(match tl
	  (()
	   (if f 
	       `(vector ,@(reverse cs))
	       tmp))
	  ;;XXX currently not handled
	  ((type (? (cut eq? '... <>)))
	   (loop '() cs f i))
	  (((? (cut eq? <> '...)))
	   (if f
	       `(%inline 
		 ".concat" 
		 `(vector ,(reverse cs))
		 (%inline ".slice" ,tmp ,i))
	       tmp))
	  ((type . tl) 
	   (let* ((wt `(vector-ref ,tmp ,i)) ;XXX nested access will be inefficient
		  (w (wrap type wt)))
	     (loop tl (cons w cs) (not (equal? w wt)) (+ i 1)))))))
    (let loop ((tl tl) (wraps '()) (tmps '()))
      ;; (write tl (current-error-port))
      (match tl
	(() (finish (reverse wraps) (reverse tmps) #f #f)) ; fixed
	(((? (cut eq? <> '...)))
	 (finish (reverse wraps) (reverse tmps) #t (temp))) ; N + rest
	((#(type))
	 (let ((tmp (temp)))
	   (finish (reverse wraps) (reverse tmps) (wrap type tmp) tmp))) ; N + rest with type
	((type . tl)
	 (let ((tmp (temp)))
	   (loop tl (cons (wrap type tmp) wraps) (cons tmp tmps))))))))
		       
(define (generate-variable-binding name type)
  (let ((sname (string->symbol name))
	(tmp (temp))
	(tmp2 (temp)))
    `(begin
       (define-syntax set!
	 (let-syntax ((old-set! set!))
	   (syntax-rules (,sname)
	     ((_ ,sname ,tmp) 
	      (%host-set! 
	       ,name 
	       ,(case type
		  ((number function object null undefined)
		   `(%check ,(symbol->string type) x))
		  ((string)
		   `(%string->jstring x))
		  (else 'x))))
	     ((_ ,tmp tmp2)
	      (old-set! ,tmp ,tmp2)))))
       (define-syntax ,sname (%host-ref ,name)))))
