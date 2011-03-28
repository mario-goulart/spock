;;;; run.scm - overly simplistic spock test


(use spock)

(print "\nGenerating some trivial JS:\n")

(display (<spock-header> debug: #t))
(display #`(print #^(+ 3 4)))

(print "\nUsing `bind':\n")
(spock 'bind "test-bind.js"
       'code '(begin 
		(print ($ "abc"))
		(print (Math.sin 42))
		(print (gurgle (vector "yes" "no") 1))
		(print yes)
		(set! one.two "ok")))
