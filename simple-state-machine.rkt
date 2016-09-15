;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname simple-state-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; State is one of the following: 'state0, 'state1, or 'state2

;;; state-machine takes a character and runs it through a state machine for the
;;; regular expression aa*b|c, provided that it is given the current state.

; State -> Char -> State
(define (state-machine state char)
  (cond [(symbol=? state 'state0)
         (cond [(char=? char #\a) 'state1]
               [(char=? char #\b) 'state0]
               [(char=? char #\c) 'state0]
               [else              'state0])]
        [(symbol=? state 'state1)
         (cond [(char=? char #\a) 'state1]
               [(char=? char #\b) 'state2]
               [(char=? char #\c) 'state2]
               [else              'state0])]
        [(symbol=? state 'state2) 'state2]))

(check-expect (state-machine 'state0 #\a) 'state1)
(check-expect (state-machine 'state0 #\b) 'state0)
(check-expect (state-machine 'state0 #\c) 'state0)
(check-expect (state-machine 'state0 #\d) 'state0)

(check-expect (state-machine 'state1 #\a) 'state1)
(check-expect (state-machine 'state1 #\b) 'state2)
(check-expect (state-machine 'state1 #\c) 'state2)
(check-expect (state-machine 'state0 #\d) 'state0)

(check-expect (state-machine 'state2 #\a) 'state2)
(check-expect (state-machine 'state2 #\b) 'state2)
(check-expect (state-machine 'state2 #\c) 'state2)
(check-expect (state-machine 'state2 #\d) 'state2)

;;; check-string checks if a given string matches the regular expression aa*b|c

; String -> Boolean
(define (check-string string)
  (symbol=? (foldl (lambda (char state)
                     (state-machine state char))
                   'state0 (string->list string))
            'state2))

(check-expect (check-string "ab") #true)
(check-expect (check-string "ac") #true)
(check-expect (check-string "aaaaab") #true)
(check-expect (check-string "aaaaac") #true)
(check-expect (check-string "adab") #true)
(check-expect (check-string "abad") #true)
(check-expect (check-string "abd") #true)
(check-expect (check-string "dabd") #true)

(check-expect (check-string "") #false)
(check-expect (check-string "a") #false)
(check-expect (check-string "adb") #false)