#lang racket

;;; A State is a (make-state ListOfTransitions)
(define-struct state (transitions))

;;; A Transition is a (make-transition Character State)
(define-struct transition (char to-state))

;;; A StateIndex is an Integer.

;;; An NFA is a State.

;;; A ListOfTransitions is one of:
;;; - empty
;;; - (cons Transition ListOfTransitions)

(define concat-nfa (letrec ([f (make-state (list (make-transition '() f)))]
                            [b (make-state (list (make-transition #\B f)))]
                            [a (make-state (list (make-transition #\A b)))])
                     a))