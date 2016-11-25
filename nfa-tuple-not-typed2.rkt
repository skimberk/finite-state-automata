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

;;; A [DFA T] is a (make-dfa [Setof [State T]]
;;;                          [DTransitionTable T]
;;;                          [State T]
;;;                          [Setof [State T]])

(define-struct nfa (states transitions initial accepting))
(define-struct dfa (states transitions initial accepting))

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

;;; An [NFA Symbol] equal to regular expression "A(B|C)"
(define test-nfa2 (let ([s0 'state0]
                       [s1 'state1]
                       [s2 'state2]
                       [s3 'state3])
                   (make-nfa (set s0 s1 s2 s3)
                             (hash (cons s0 #\A) (set s1 s2)
                                   (cons s1 #\B) (set s3)
                                   (cons s2 #\C) (set s3))
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

;;; Add a list of keys and values to a hash.
;;; [Hashof T U] [Listof [Pairof T U]] -> [Hashof T U]
(define (hash+list h l)
  (cond [(empty? l) h]
        [else (hash-set (hash+list h (rest l))
                        (car (first l))
                        (cdr (first l)))]))

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

;;; Predicate which is true if all given predicates are true.
;;; [[Ntransition T] -> Boolean] ... -> [[Ntransition T] -> Boolean]
(define (and-pred . preds)
  (λ (transition)
    (andmap (λ (pred)
              (pred transition))
            preds)))

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
                                              (and-pred (not-pred (from-pred current))
                                                        (from-pred visit)
                                                        pred)))]))]
    (step (set) from)))

;;; Epsilon closure of states (all states that can be reached via
;;; epsilon transitions from given states).
;;; [NFA T] [Setof [State T]] -> [Setof [State T]]
(define (epsilon-closure nfa states)
  (pred-reachable-from nfa states epsilon-pred))

;;; Converts from NFA to DFA using Powerset construction.
;;; [NFA T] -> [DFA [Setof T]]
(define (nfa->dfa nfa)
  (local [(define initial-state
            (epsilon-closure nfa (set (nfa-initial nfa))))
          (define (add-states current-dfa visit-first visit-rest)
            (local [(define transitions
                      (pred-transitions nfa
                                        (and-pred (from-pred visit-first)
                                                  (not-pred epsilon-pred))))
                    (define new-transitions
                      (map (λ (transition)
                             (cons (cons visit-first
                                         (cdar transition))
                                   (epsilon-closure nfa
                                                    (cdr transition))))
                           transitions))]
              (step (make-dfa (set-add (dfa-states current-dfa)
                                       visit-first)
                              (hash+list (dfa-transitions current-dfa)
                                         new-transitions)
                              (dfa-initial current-dfa)
                              (if (set-empty? (set-intersect (nfa-accepting nfa)
                                                             visit-first))
                                  (dfa-accepting current-dfa)
                                  (set-add (dfa-accepting current-dfa)
                                           visit-first)))
                    (enqueue-list visit-rest (map cdr new-transitions)))))
          (define (step current-dfa visit)
            (cond [(queue-empty? visit) current-dfa]
                  [else (local [(define dequeued (dequeue visit))
                                (define visit-first (car dequeued))
                                (define visit-rest (cdr dequeued))]
                          (cond [(set-member? (dfa-states current-dfa)
                                              visit-first)
                                 (step current-dfa visit-rest)]
                                [else (add-states current-dfa
                                                  visit-first
                                                  visit-rest)]))]))]
    (step (make-dfa (set)
                    (hash)
                    initial-state
                    (set))
          (enqueue empty-queue
                   initial-state))))