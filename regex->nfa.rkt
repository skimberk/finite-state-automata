#lang racket

(require racket/hash)
(require "nfa-tuple-not-typed.rkt")

;; Creates NFA for an Input.
;; symbol-nfa : Input -> NFA
(define (input-nfa input)
  (let ([s0 (gensym)]
        [s1 (gensym)])
    (make-nfa (set s0 s1)
              (hash (cons s0 input) (set s1))
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
                                                   (nfa-initial b))
                                (cons (set-first (nfa-accepting a)) 'ep) (set s1)
                                (cons (set-first (nfa-accepting b)) 'ep) (set s1))
                          (nfa-transitions a)
                          (nfa-transitions b))
              s0
              (set s1))))

;;; Creates NFA for concatenation of two NFAs.
;;; concat-nfa : NFA NFA -> NFA
(define (concat-nfa a b)
  (make-nfa (set-union (nfa-states a)
                       (nfa-states b))
            (hash-union (hash (cons (set-first (nfa-accepting a)) 'ep)
                              (set (nfa-initial b)))
                        (nfa-transitions a)
                        (nfa-transitions b))
            (nfa-initial a)
            (nfa-accepting b)))

;;; Creates NFA for Kleene star expression with NFA.
;;; star-nfa : NFA -> NFA
(define (star-nfa x)
  (let ([s0 (gensym)]
        [s1 (gensym)])
    (make-nfa (set-union (set s0 s1)
                         (nfa-states x))
              (hash-union (hash (cons s0 'ep) (set (nfa-initial x) s1)
                                (cons (set-first (nfa-accepting x)) 'ep)
                                (set (nfa-initial x) s1))
                          (nfa-transitions x))
              s0
              (set s1))))

(define test-union-nfa (union-nfa (input-nfa #\A) (input-nfa #\B)))

(define test-concat-nfa (concat-nfa (input-nfa #\A) (input-nfa #\B)))

(define test-star-nfa (star-nfa (input-nfa #\A)))

(define test-complex-nfa (concat-nfa test-union-nfa test-concat-nfa))