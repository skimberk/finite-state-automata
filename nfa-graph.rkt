#lang racket

(provide (struct-out state))
(provide (struct-out transition))
(provide (struct-out nfa))
(provide match-string?)
(provide eval-nfa)

;;; A ListOfTransitions is one of:
;;; - empty
;;; - (cons Transition ListOfTransitions)

;;; A SetOfBoxedStates is a (set (box State) ...)

;;; An Input is either 'empty or Character

;;; A State is a (make-state ListOfTransitions)
(define-struct state (transitions))

;;; A Transition is a (make-transition Input (box State))
(define-struct transition (char to-state))

;;; An NFA is a (make-nfa (box State) (box State))
(define-struct nfa (start final))

;;; Test NFA matching regular expression "AB"
(define test-nfa (shared ([f (box (make-state empty))]
                          [b (box (make-state (list (make-transition #\B
                                                                     f))))]
                          [a (box (make-state (list (make-transition #\A
                                                                     b))))])
                   (make-nfa a f)))

;; Does NFA match string?
;; match-string? : NFA String
(define (match-string? nfa s)
  (set-member? (eval-nfa nfa s) (nfa-final nfa)))

;;; States of NFA after evaluating with string.
;;; eval-nfa : NFA String -> SetOfBoxedStates
(define (eval-nfa nfa s)
  (foldl (lambda (char states)
           (let ([r (set-map states
                             (lambda (state)
                               (resulting-states state
                                                 char)))])
             (if (empty? r) (set) (apply set-union r))))
         (set (nfa-start nfa)) (string->list s)))

;;; Resulting states after passing char to state.
;;; resulting-states : (box State) Character -> SetOfBoxedStates
(define (resulting-states state char)
  (foldl (lambda (t states)
           (if (and (char? (transition-char t))
                    (char=? (transition-char t) char))
               (set-add states (transition-to-state t))
               states)) (set) (state-transitions (unbox state))))

;;; Can final state be reached with epsilon transitions?
;;; epsilon-to-final? : NFA -> Boolean
(define (epsilon-to-final? nfa)
  (or (equal? (nfa-start nfa) (nfa-final nfa))
      (foldl (lambda (t bool)
               (or bool
                   (and (symbol? (transition-char t))
                        (epsilon-to-final? (make-nfa (transition-to-state t)
                                                     (nfa-final nfa))))))
             #false (state-transitions (unbox (nfa-start nfa))))))

;; One recursive step of removing epsilon transitions from NFA.
;; remove-epsilons-step! : (box State) SetOfBoxedStates -> (box State)
(define (remove-epsilons-step! state visited-states)
  (cond [(set-member? visited-states state) state]
        [else (set-box! state (make-state (foldl (lambda (t ts)
                                                   (append (if (symbol? (transition-char t))
                                                               (state-transitions (unbox (remove-epsilons-step! (transition-to-state t)
                                                                                                                (set-add visited-states state))))
                                                               (list t))
                                                           ts)) '() (state-transitions (unbox state)))))
              state]))
        
  