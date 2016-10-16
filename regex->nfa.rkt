#lang racket

(require "nfa-graph.rkt")

;; An NFASymbol is one of:
;; - 'empty
;; - Character

;; Creates NFA for a symbol.
;; symbol-nfa : NFASymbol -> NFA
(define (symbol-nfa symbol)
  (shared ([s2 (box (make-state (list (make-transition 'empty
                                                       s2))))]
           [s1 (box (make-state (list (make-transition symbol
                                                       s2))))])
    (make-nfa a f)))

;; Creates NFA for union of two NFAs.
;; union-nfa : NFA NFA -> NFA
(define (union-nfa a b)
  (shared ([s2 (box (make-state (list (make-transition 'empty
                                                       f))))]
           [s1 (box (make-state (list (make-transition 'empty
                                                       (nfa-start a))
                                      (make-transition 'empty
                                                       (nfa-start b)))))])
    (set-box! a (make-state (cons (make-transition 'empty s2)
                                  (nfa-transitions (unbox a)))))
    (set-box! b (make-state (cons (make-transition 'empty s2)
                                  (nfa-transitions (unbox b)))))                  
    (make-nfa s1 s2)))
  