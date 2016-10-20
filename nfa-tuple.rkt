#lang typed/racket

(define-type state Symbol)
(define-type char (U 'empty Char))
(define-type condition (Pair Symbol char))

; NFA where each state is represented as a symbol.
(struct nfa ([states      : (Setof state)]
             [transitions : (HashTable condition state)]
             [initial     : state]
             [accepting   : (Setof state)]))

(define-type s-state (Setof state))

; NFA where each state is represented as a set of symbols.
(struct nfa-s ([states      : (Setof s-state)]
               [transitions : (HashTable condition s-state)]
               [initial     : s-state]
               [accepting   : (Setof s-state)]))

(define test-nfa (let ([s0 (gensym)]
                       [s1 (gensym)]
                       [s2 (gensym)])
                   (nfa (set s0 s1 s2)
                        (hash (cons s0 #\A) s1
                              (cons s1 #\B) s2)
                        s0
                        (set s2))))

(: add-list-to-hash (All (A B) (-> (Listof (Pairof A B)) (HashTable A B) (HashTable A B))))
(define (add-list-to-hash l h)
  (cond [(empty? l) h]
        [else (hash-set (add-list-to-hash (rest l) h)
                        (car (first l))
                        (cdr (first l)))]))

(: zip (All (A B) (-> (Listof A) (Listof B) (Listof (Pairof A B)))))
(define (zip a b)
  (map (inst cons A B) a b))

(: epsilon-closure (-> nfa state s-state))
(define (epsilon-closure nfa-in state-in)
  (foldl (lambda ([transition : (Pair condition state)]
                  [states     : (Setof state)])
           (if (and (symbol=? (car (car transition)) state-in)
                    (symbol? (cdr (car transition))))
               (set-union states (epsilon-closure nfa-in (cdr transition)))
               states))
         (set state-in) (hash->list (nfa-transitions nfa-in))))

(: transitions (-> nfa state (Setof (Pairof char state))))
(define (transitions nfa-in state-in)
  (foldl (lambda ([transition : (Pair condition state)]
                  [states     : (Setof (Pairof char state))])
           (if (symbol=? (car (car transition)) state-in)
               (set-add states (cons (cdr (car transition))
                                     (cdr transition)))
               states))
         (ann (set) (Setof (Pairof char state)))
         (hash->list (nfa-transitions nfa-in))))

(: set-transitions (-> nfa (Setof state) (Setof (Pairof char state))))
(define (set-transitions nfa-in states-in)
  (foldl (lambda ([s      : state]
                  [states : (Setof (Pairof char state))])
           (set-union states (transitions nfa-in s)))
         (ann (set) (Setof (Pairof char state)))
         (set->list states-in)))

#|
(: accepting-s-states (-> nfa (Listof s-state) (Setof s-state)))
(define (accepting-s-states nfa-in eclosures)
  (foldl (lambda ([c : s-state]
                  [s : (Setof s-state)])
           (if (set-empty? (set-intersect (nfa-accepting nfa-in) c))
               s
               (set-add s c)))
         (ann (set) (Setof s-state)) eclosures))
|#

(: accepting-s-states (-> nfa (Listof s-state) (Setof s-state)))
(define (accepting-s-states nfa-in eclosures)
  (list->set (filter (lambda ([s : s-state])
                       (set-empty? (set-intersect (nfa-accepting nfa-in) s)))
                     eclosures)))

#|
(: eclosure-transitions (-> nfa (Listof s-state) (HashTable condition s-state)))
(define (eclosure-transitions nfa-in eclosures)
  (add-list-to-hash
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