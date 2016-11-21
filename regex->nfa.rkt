#lang racket

(require racket/hash)
(require "nfa-tuple-not-typed.rkt")

;; Creates NFA for an Input.
;; symbol-nfa : Input -> NFA
(define (input-nfa input)
  (let ([s0 (gensym)]
        [s1 (gensym)])
    (make-nfa (set s0 s1)
              (hash (cons s0 input) s1)
              s0
              (set s1))))

;; Creates NFA for union of two NFAs.
;; union-nfa : NFA NFA -> NFA
(define (union-nfa a b)
  (let ([s0 (gensym)]
        [s1 (gensym)])
    (make-nfa (set-union (set s0 s1)
                         (nfa-states a)
                         (nfa-states b))
              (hash-union (hash (cons s0 'ep) (set (nfa-initial a)
                                                   (nfa-initial b)))
                          (nfa-transitions a)
                          (nfa-transitions b))
              s0
              (set s1))))
  