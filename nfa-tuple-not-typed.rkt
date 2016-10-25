#lang racket

;;; A State is either a Symbol or a [Setof Symbol]
;;; A Input is either a Char or 'ep
;;; A Condition is a [Pairof State Input]
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
;;; [A B]   [Listof [Pairof A B]] [Hashof A B] -> [Hashof A B]
(define (add-list-to-hash l h)
  (cond [(empty? l) h]
        [else (hash-set (add-list-to-hash (rest l) h)
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

;;; All non-epsilon transitions from state.
;;; NFA State -> [Setof [Pairof Condition State]]
(define (transitions nfa state)
  (foldl (lambda (transition states)
           (if (and (symbol=? (car (car transition)) state)
                    (not (symbol? (cdr (car transition)))))
               (set-add states transition)
               states))
         (set)
         (hash->list (nfa-transitions nfa))))

;;; All non-epsilon transitions for a set of states.
;;; NFA [Setof State] -> [Setof [Pairof Condition State]]
(define (set-transitions nfa states)
  (apply set-union (set-map states ((curry transitions) nfa))))

#|
(: accepting-s-states (-> nfa (Listof s-state) (Setof s-state)))
(define (accepting-s-states nfa-in eclosures)
  (list->set (filter (lambda ([s : s-state])
                       (set-empty? (set-intersect (nfa-accepting nfa-in) s)))
                     eclosures)))

#|
(: eclosure-transitions (-> nfa (Listof s-state) (HashTable condition s-state)))
(define (eclosure-transitions nfa-in eclosures)
  (add-list-to-hash (apply append (map (lambda ([e : s-state])
                           (map ((curry cons) e)
                                (set->list (set-transitions nfa-in e)))
                         eclosures)))
                    (hash)))
|#

(: no-epsilon (-> nfa nfa-s))
(define (no-epsilon nfa-in)
  (let ([eclosures (map (lambda ([s : Symbol])
                          (epsilon-closure nfa-in s))
                        (set->list (nfa-states nfa-in)))])
    (nfa-s (list->set eclosures)
           (hash)
           (epsilon-closure nfa-in (nfa-initial nfa-in))
           (accepting-s-states nfa-in eclosures))))
|#