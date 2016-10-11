#lang racket

(require "nfa.rkt")

;; An NFASymbol is one of:
;; - empty
;; - Character

;; Creates NFA for a symbol.
;; symbol-nfa : NFASymbol -> NFA
(define (symbol-nfa char)
  (list (make-state (list (make-transition 1 char)))
        (make-state (list (make-transition 1 empty)))))

;; Creates NFA for union of two NFAs.
;; union-nfa : NFA NFA -> NFA
(define (union-nfa a b)
  (list (make-state (list (make-transition 
  