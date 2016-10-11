#lang racket

(provide state)
(provide transition)
(provide eval-nfa)

;;; A State is a (make-state ListOfTransitions)
(define-struct state (transitions))

;;; A Transition is a (make-transition StateIndex Character)
(define-struct transition (to-index char))

;;; A StateIndex is an Integer.

;;; An NFA is one of:
;;; - empty
;;; - (cons State NFA)

;;; A ListOfTransitions is one of:
;;; - empty
;;; - (cons Transition ListOfTransitions)

;;; A SetOfStateIndices is a (set StateIndex ...)

(define concat-nfa (list (make-state (list (make-transition 1 #\A)))
                         (make-state (list (make-transition 2 #\B)))
                         (make-state (list (make-transition 2 empty)))))

;;; States of NFA after evaluating with string.
;;; NFA String -> ListOfStateIndices
(define (eval-nfa nfa input)
  (foldl (lambda (char indices)
           (let ([r (set-map indices
                             (lambda (index)
                               (resulting-states (list-ref nfa index)
                                                 char)))])
             (if (empty? r) (set) (apply set-union r))))
         (set 0) (string->list input)))

;;; Resulting states after passing char to state.
;;; State Character -> SetOfStateIndices
(define (resulting-states state char)
  (foldl (lambda (t indices)
           (if (or (empty? (transition-char t))
                   (char=? (transition-char t) char))
               (set-add indices (transition-to-index t))
               indices)) (set) (state-transitions state)))