#lang racket

(require "queue.rkt")

;;; A State is a Symbol
;;; A Input is either a Char or 'ep
;;; A Condition is a [Pairof State Input]
;;; A Transition is a [Pairof Condition State]
;;; A TransitionTable is a [Hashof Condition State]

;;; A NFA is a (make-nfa [Setof State] TransitionTable State [Setof State])
(define-struct nfa (states transitions initial accepting))

;;; An NFA equal to regular expression "AB"
(define test-nfa (let ([s0 (gensym)]
                       [s1 (gensym)]
                       [s2 (gensym)])
                   (make-nfa (set s0 s1 s2)
                             (hash (cons s0 'ep) s1
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

;;; All states reachable from state (excluding ones on blacklist).
;;; NFA State [Setof State] -> [Listof State]
(define (reachable-states nfa from blacklist)
  (filter-map (λ (transition)
                (and (equal? (car (car transition))
                             from)
                     (symbol? (cdr (car transition)))
                     (not (set-member? blacklist (cdr transition)))
                     (cdr transition)))
              (hash->list (nfa-transitions nfa))))

;;; Epsilon closure of state (all states that can be reached via
;;; epsilon transitions).
;;; NFA State -> [Setof State]
(define (epsilon-closure nfa state)
  (local [(define (step current visit)
            (cond [(queue-empty? visit) current]
                  [else (local [(define dequeued    (dequeue visit))
                                (define visit-first (car dequeued))
                                (define visit-rest  (cdr dequeued))]
                          (step (set-add current visit-first)
                                (enqueue-list visit-rest
                                              (reachable-states nfa
                                                                visit-first
                                                                current))))]))]
    (step (set) (enqueue empty-queue state))))

;;; All non-epsilon transitions for a set of states.
;;; NFA [Setof State] -> [Listof Transition]
(define (set-transitions nfa states)
  (filter (λ (transition)
            (and (set-member? states (car (car transition)))
                 (not (symbol? (cdr (car transition))))))
          (hash->list (nfa-transitions nfa))))

;;; Remove epsilon transitions from NFA.
;;; NFA -> NFA
(define (remove-epsilon nfa)
  (local [(define transitions (hash->list (nfa-transitions nfa)))]
    (make-nfa (nfa-states nfa)
              (hash+list (hash)
                         (filter (λ (transition)
                                   (not (symbol? (cdar transition))))
                                 transitions))
              (nfa-initial nfa)
              (nfa-accepting nfa))))

;;; Equivalent NFA without epsilon transitions.
;;; NFA -> NFA
(define (no-epsilon nfa)
  (local [(define (new-transitions current state eclosure)
            (local [(define e-transitions (set-transitions current
                                                           eclosure))
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

(nfa-accepting (no-epsilon test-nfa))
(nfa-transitions (no-epsilon test-nfa))