#lang racket

;;; A State is either a Symbol or a [Setof Symbol]
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
                             (hash (cons s0 #\A) s1
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
;;; NFA -> [Listof [Pairof State State]]
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

;;; Remove epsilon transitions from list of transitions.
;;; [Hashof State State] [Listof Transition] -> [Listof Transition]
(define (remove-epsilon-transitions transitions)
  (filter (lambda (transition)
            (not (symbol? (cdr (car transition)))))
          transitions))

;;; Updates representations of states in transition.
;;; [Hashof State State] Transition -> Transition
(define (update-transition-states state->state transition)
  (let ([state (car (car transition))]
        [input (cdr (car transition))]
        [to    (cdr transition)])
    (cons (cons (hash-ref state->state state)
                input)
          (hash-ref state->state to))))

;;; Equivalent NFA without epsilon transitions.
;;; NFA -> NFA
(define (no-epsilon nfa)
  (let ([eclosures (epsilon-closures nfa)])
    (make-nfa (list->set (map cdr eclosures))
              (hash)
              (epsilon-closure nfa (nfa-initial nfa))
              (list->set (filter (lambda (pair)
                                   (set-member? (nfa-accepting nfa)
                                                (car pair)))
                                 eclosures)))))