#lang racket

(require "queue.rkt")

;;; A [State T] is a T
;;; A Input is a Char
;;; A [Condition T] is a [Pairof [State T] Input]

;;; A [NTransition T] is a [Pairof [Condition T] [Setof [State T]]]
;;; A [NTransitionTable T] is a [Hashof [Condition T] [Setof [State T]]]

;;; A [DTransition T] is a [Pairof [Condition T] [State T]]
;;; A [DTransitionTable T] is a [Hashof [Condition T] [State T]]

;;; A [NFA T] is a (make-nfa [Setof [State T]]
;;;                          [NTransitionTable T]
;;;                          [State T]
;;;                          [Setof [State T]])

;;; A [DFA T] is a (make-nfa [Setof [State T]]
;;;                          [DTransitionTable T]
;;;                          [State T]
;;;                          [Setof [State T]])

(define-struct nfa (states transitions initial accepting))

;;; An [NFA Symbol] equal to regular expression "AB"
(define test-nfa (let ([s0 'state0]
                       [s1 'state1]
                       [s2 'state2]
                       [s3 'state3])
                   (make-nfa (set s0 s1 s2 s3)
                             (hash (cons s0 #\A) (set s1)
                                   (cons s1 'ep) (set s2)
                                   (cons s2 #\B) (set s3))
                             s0
                             (set s3))))

;;; An [NFA Symbol] that is just a loop
(define loop-nfa (let ([s0 (gensym)]
                       [s1 (gensym)])
                   (make-nfa (set s0 s1)
                             (hash (cons s0 'ep) (set s1)
                                   (cons s1 'ep) (set s0))
                             s0
                             (set s1))))

;;; Union of list of sets
;;; [Listof [Setof T]] -> [Setof T]
(define (list-set-union l)
  (if (empty? l) '() (apply set-union l)))

;;; All transitions that match predicate.
;;; [NFA T] [[NTransition T] -> Boolean] -> [Listof [NTransition T]]
(define (pred-transitions nfa pred)
  (filter pred (hash->list (nfa-transitions nfa))))

;;; All states reachable from transitions matching predicate.
;;; [NFA T] [[NTransition T] -> Boolean] -> [Setof [State T]]
(define (pred-reachable nfa pred)
  (list-set-union (map cdr (pred-transitions nfa pred))))

;;; Complement of predicate.
;;; [Setof [State T]] -> [[Ntransition T] -> Boolean]
(define (not-pred pred)
  (λ (transition)
    (not (pred transition))))

;;; Predicate which matches transitions from set of states.
;;; [Setof [State T]] -> [[NTransition T] -> Boolean]
(define (from-pred from)
  (λ (transition)
    (set-member? from (caar transition))))

;;; Predicate which matches all epsilon transitions.
;;; [[NTransition T] -> Boolean]
(define epsilon-pred
  (λ (transition)
    (symbol? (cdar transition))))

;;; All states reachable via transitions matching predicate from initial states.
;;; Includes initial states.
;;; [NFA T] [Setof [State T]] [Setof [State T]] [[NTransition T] -> Boolean] -> [Setof [State T]]
(define (pred-reachable-from nfa from pred)
  (local [(define (step current visit)
            (cond [(empty? visit) current]
                  [else (step (set-union current visit)
                              (pred-reachable nfa
                                              (λ (t)
                                                (and ((not-pred (from-pred current)) t)
                                                     ((from-pred visit) t)
                                                     (pred t)))))]))]
    (step (set) from)))

;;; All reachable states within NFA.
;;; [NFA T] -> [Setof [State T]]
(define (all-reachable nfa)
  (pred-reachable-from nfa (set (nfa-initial nfa)) (λ (t) true)))

;;; Epsilon closure of states (all states that can be reached via
;;; epsilon transitions from given states).
;;; [NFA T] [Setof [State T]] -> [Setof [State T]]
(define (epsilon-closure nfa states)
  (pred-reachable-from nfa states epsilon-pred))