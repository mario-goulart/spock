;;;; test-end.scm - show statistics over all tests


;;XXX abstract away all those +columnX-width+ references

(display "\n----------------------------------------------------------------------\n")
(display (padr "tests succeded:" +column1-width+))
(display (padl (number->string tests-succeeded) +column2-width+))
(newline)
(display (padr "tests failed:" +column1-width+))
(display (padl (number->string tests-failed) +column2-width+))
(newline)
(display (padr "total number of tests:" +column1-width+))
(display (padl (number->string (+ tests-succeeded tests-failed)) +column2-width+))
(newline)
