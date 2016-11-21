#lang racket

(require "queue.rkt")

;;; A State is a Symbol
;;; A Input is a Char
;;; A Condition is a [Pairof State Input]
;;; A Transition is a [Pairof Condition State]
;;; A TransitionTable is a [Hashof Condition State]

;;; A EInput is either a Input or 'ep
;;; A ECondition is a [Pairof State EInput]
;;; A ETransition is a [Pairof ECondition State]
;;; A ETransitionTable is a [Hashof ECondition State]

;;; A NFA is a (make-nfa [Setof State] TransitionTable State [Setof State])
;;; A ENFA is a (make-nfa [Setof State] ETransitionTable State [Setof State])
(define-struct nfa (states transitions initial accepting))

;;; An NFA equal to regular expression "AB"
(define test-nfa (let ([s0 (gensym)]
                       [s1 (gensym)]
                       [s2 (gensym)])
                   (make-nfa (set s0 s1 s2)
                             (hash (cons s0 #\A) s1
                                   (cons s1 #\B) s2)
                             s0
                             (set s2))))

;;; An NFA that is just a loop
(define loop-nfa (let ([s0 (gensym)]
                       [s1 (gensym)])
                   (make-nfa (set s0 s1)
                             (hash (cons s0 'ep) s1
                                   (cons s1 'ep) s0)
                             s0
                             (set s1))))

;;; Add a list of keys and values to a hash.
;;; [A B]   [Hashof A B] [Listof [Pairof A B]] -> [Hashof A B]
(define (hash+list h l)
  (cond [(empty? l) h]
        [else (hash-set (hash+list h (rest l))
                        (car (first l))
                        (cdr (first l)))]))

;;; All transitions for set of states (excluding ones on blacklist).
;;; ENFA [Setof State] [Setof State] -> [Listof Transition]
(define (state-transitions nfa from blacklist)
  (filter (λ (transition)
            (and (set-member? from (car (car transition)))
                 (not (set-member? blacklist (cdr transition)))))
          (hash->list (nfa-transitions nfa))))

;;; All epsilon transitions for a set of states (excluding ones on blacklist).
;;; ENFA [Setof State] -> [Listof Transition]
(define (state-transitions/epsilon nfa from blacklist)
  (filter (λ (transition)
            (symbol? (cdr (car transition))))
          (state-transitions nfa from blacklist)))

;;; All non-epsilon transitions for a set of states (excluding ones on blacklist).
;;; ENFA [Setof State] -> [Listof Transition]
(define (state-transitions/no-epsilon nfa from blacklist)
  (filter (λ (transition)
            (not (symbol? (cdr (car transition)))))
          (state-transitions nfa from blacklist)))

;;; All states reachable from state (excluding ones on blacklist).
;;; ENFA State [Setof State] -> [Listof State]
(define (reachable-states nfa from blacklist)
  (map cdr (state-transitions nfa
                              (set from)
                              blacklist)))

;;; All states reachable from state via epsilon transitions
;;; (excluding ones on blacklist).
;;; ENFA State [Setof State] -> [Listof State]
(define (reachable-states/epsilon nfa from blacklist)
  (map cdr (state-transitions/epsilon nfa
                                      (set from)
                                      blacklist)))

;;; All possible reachable states from state given reachability function.
;;; ENFA State [ENFA State [Setof State] -> [Listof State]] -> [Setof State]
(define (all-reachable nfa from reachable)
  (local [(define (step current visit)
            (cond [(queue-empty? visit) current]
                  [else (local [(define dequeued    (dequeue visit))
                                (define visit-first (car dequeued))
                                (define visit-rest  (cdr dequeued))]
                          (step (set-add current visit-first)
                                (enqueue-list visit-rest
                                              (reachable nfa
                                                         visit-first
                                                         current))))]))]
    (step (set) (enqueue empty-queue from))))

;;; All possible reachable states from NFA
;;; ENFA -> [Setof State]
(define (all-reachable/nfa nfa)
  (all-reachable nfa (nfa-initial nfa) reachable-states))

;;; Epsilon closure of state (all states that can be reached via
;;; epsilon transitions).
;;; ENFA State -> [Setof State]
(define (epsilon-closure nfa state)
  (all-reachable nfa state reachable-states/epsilon))

;;; Remove epsilon transitions from NFA.
;;; ENFA -> NFA
(define (remove-epsilon nfa)
  (make-nfa (nfa-states nfa)
            (state-transitions nfa (nfa-states nfa) '())
            (nfa-initial nfa)
            (nfa-accepting nfa)))

;;; Equivalent NFA without epsilon transitions.
;;; ENFA -> NFA
(define (no-epsilon nfa)
  (local [(define (new-transitions current state eclosure)
            (local [(define e-transitions (state-transitions current
                                                             eclosure
                                                             '()))
                    (define transitions (map (λ (transition)
                                               (cons (cons state
                                                           (cdar transition))
                                                     (cdr transition)))
                                             e-transitions))]
              (hash+list (nfa-transitions current) transitions)))
          (define (new-accepting current state eclosure)
            (if (not (set-empty? (set-intersect (nfa-accepting current)
                                                eclosure)))
                (set-add (nfa-accepting current) state)
                (nfa-accepting current)))
          (define (step current visit visited)
            (cond [(queue-empty? visit) current]
                  [else (local [(define dequeued (dequeue visit))
                                (define visit-first (car dequeued))
                                (define visit-rest (cdr dequeued))
                                (define new-visited
                                  (set-add visited visit-first))
                                (define eclosure
                                  (epsilon-closure nfa
                                                   visit-first))]
                          (step (make-nfa (nfa-states nfa)
                                          (new-transitions current
                                                           visit-first
                                                           eclosure)
                                          (nfa-initial nfa)
                                          (new-accepting current
                                                         visit-first
                                                         eclosure))
                                (enqueue-list visit-rest
                                              (reachable-states nfa
                                                                visit-first
                                                                new-visited))
                                new-visited))]))
          (define equivalent (step nfa
                                   (enqueue empty-queue
                                            (nfa-initial nfa))
                                   (set)))]
    (remove-epsilon equivalent)))