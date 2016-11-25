#lang racket

(require "queue.rkt")

;;; A [State T] is a T
;;; A Input is a Char
;;; A [Condition T] is a [Pairof [State T] Input]

;;; A [NTransition T] is a [Pairof [Condition T] [Setof [State T]]]
;;; A [NTransitionTable T] is a [Hashof [Condition T] [Setof [State T]]]

;;; A [DTransition T] is a [Pairof [Condition T] [State T]]
;;; A [DTransitionTable T] is a [Hashof [Condition T] [State T]]

;;; A [NFA T] is a (make-nfa [Setof [State T]]
;;;                          [NTransitionTable T]
;;;                          [State T]
;;;                          [Setof [State T]])

;;; A [DFA T] is a (make-nfa [Setof [State T]]
;;;                          [DTransitionTable T]
;;;                          [State T]
;;;                          [Setof [State T]])

(define-struct nfa (states transitions initial accepting))