#lang racket

;;; A ListOfTransitions is one of:
;;; - empty
;;; - (cons Transition ListOfTransitions)

;;; An Input is either 'empty or Character

;;; A State is a (make-state ListOfTransitions)
(define-struct state (transitions))

;;; A Transition is a (make-transition Input (box State))
(define-struct transition (char to-state))

;;; An NFA is a (make-nfa (box State) (box State))
(define-struct nfa (start final))

;;; Test NFA matching regular expression "AB"
(define test-nfa (shared ([f (box (make-state (list (make-transition 'empty
                                                                     f))))]
                          [b (box (make-state (list (make-transition #\B
                                                                     f))))]
                          [a (box (make-state (list (make-transition #\A
                                                                     b))))])
                   (make-nfa a f)))