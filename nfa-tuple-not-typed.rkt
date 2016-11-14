#lang racket

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

;;; Add a list of keys and values to a hash.
;;; [A B]   [Listof [Pairof A B]] -> [Hashof A B]
(define (list->hash l)
  (cond [(empty? l) (hash)]
        [else (hash-set (list->hash (rest l))
                        (car (first l))
                        (cdr (first l)))]))

;;;  Zips together two lists of equal size.
;;; [A B]   [Listof A] [Listof B] -> [Listof [Pairof A B]]
(define (zip a b)
  (map cons a b))

;;; Epsilon closure of state (all states that can be reached via
;;; epsilon transitions).
;;; NFA State -> State
(define (epsilon-closure nfa state)
  (foldl (lambda (transition states)
           (if (and (symbol=? (car (car transition)) state)
                    (symbol? (cdr (car transition))))
               (set-union states (epsilon-closure nfa (cdr transition)))
               states))
         (set state) (hash->list (nfa-transitions nfa))))

;;; List of states and their epsilon closures.
;;; NFA -> [Listof [Pairof State [Setof State]]]
(define (epsilon-closures nfa)
  (set-map (nfa-states nfa)
           (lambda (state)
             (cons state
                   (epsilon-closure nfa
                                    state)))))

;;; All non-epsilon transitions for a set of states.
;;; NFA [Setof State] -> [Listof Transition]
(define (set-transitions nfa states)
  (filter (lambda (transition)
            (and (set-member? states (car (car transition)))
                 (not (symbol? (cdr (car transition))))))
          (hash->list (nfa-transitions nfa))))

;;; All transitions for given epsilon closures.
;;; NFA [Listof [Setof State]] -> [Listof Transition]
(define (epsilon-closure-transitions nfa eclosures)
  (apply append (map (lambda (eclosure)
                       (map (lambda (transition)
                              (cons eclosure (cdr transition)))
                            (set-transitions nfa eclosure)))
                     eclosures)))

;;; Equivalent NFA without epsilon transitions.
;;; NFA -> NFA
(define (no-epsilon nfa)
  (letrec ([eclosure-pairs (epsilon-closures nfa)]
           [eclosures (map cdr eclosure-pairs)])
    (make-nfa (list->set eclosures)
              (list->hash (epsilon-closure-transitions nfa
                                                       eclosures))
              (epsilon-closure nfa (nfa-initial nfa))
              (list->set (map cdr (filter (lambda (pair)
                                            (set-member? (nfa-accepting nfa)
                                                         (car pair)))
                                          eclosure-pairs))))))