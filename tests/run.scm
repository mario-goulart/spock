;;;; run.scm - overly simplistic spock test


(use spock)

(print "\nGenerating some trivial JS:\n")
(spock-initialize 'library-path "../spock")

(display (<script-header> debug: #t))

(display #`(print #^(+ 3 4)))
