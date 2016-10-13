#lang racket

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
(define test-nfa (shared ([f (box (make-state (list (make-transition 'empty
                                                                     f))))]
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
           (if (or (symbol? (transition-char t))
                   (char=? (transition-char t) char))
               (set-add states (transition-to-state t))
               states)) (set) (state-transitions (unbox state))))