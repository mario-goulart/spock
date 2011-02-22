;;;; run.scm - overly simplistic spock test


(use spock)

(print "\nGenerating some trivial JS:\n")

(display (<script-header> debug: #t))
(display #`(print #^(+ 3 4)))
