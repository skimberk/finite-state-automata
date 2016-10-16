#lang typed/racket

(struct nfa ([states      : (Setof Symbol)]
             [transitions : (HashTable (Pair Symbol (U 'empty Char)) Symbol)]
             [initial     : Symbol]
             [accepting   : (Setof Symbol)]))

(define test-nfa (let ([s0 (gensym)]
                       [s1 (gensym)]
                       [s2 (gensym)])
                   (nfa (set s0 s1 s2)
                        (hash (cons s0 #\A) s1
                              (cons s1 #\B) s2)
                        s0
                        (set s2))))

(: first-epsilon-transition (-> (Listof (Pair Symbol (U 'empty Char)))
                                (U 'none (Pair Symbol (U 'empty Char)))))
(define (first-epsilon-transition ts)
  (cond [(empty? ts) 'none]
        [else (if (symbol? (cdr (first ts)))
                  (first ts)
                  (first-epsilon-transition (rest ts)))]))


(: without-epsilon (-> nfa nfa))
(define (without-epsilon nfa-in)
  (let ([first-epsilon
         (first-epsilon-transition (hash-keys (nfa-transitions nfa-in)))])
    (cond [(symbol? first-epsilon) nfa-in]
          [else (let ([states (nfa-states nfa-in)]
                      [transitions (nfa-transitions nfa-in)]
                      [initial (nfa-initial nfa-in)]
                      [accepting (nfa-accepting nfa-in)]
                      [to (hash-ref (nfa-transitions nfa-in)
                                    first-epsilon)])
                  (nfa (set-remove states to)
                       (hash-remove transitions
                                    first-epsilon)
                       initial
                       (if (set-member? accepting to)
                           (set-add (set-remove accepting to)
                                    (car first-epsilon))
                           accepting)))])))

(: epsilon-closure (-> nfa Symbol (Setof Symbol)))
(define (epsilon-closure nfa-in state)
  (foldl (lambda ([transition : (Pair (Pair Symbol (U 'empty Char)) Symbol)]
                  [states     : (Setof Symbol)])
           (if (and (symbol=? (car (car transition)) state)
                    (symbol? (cdr (car transition))))
               (set-union states (epsilon-closure nfa-in (cdr transition)))
               states))
         (set state) (hash->list (nfa-transitions nfa-in))))