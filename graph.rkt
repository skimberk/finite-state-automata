#lang racket

(require "queue.rkt")

;;; A Graph is a [Hashof Symbol [Listof Symbol]]

(define graph0 (hash 'a '(b c)
                     'b '(a)
                     'c '(d)
                     'd '(e)
                     'e '()))

;;; Graph Symbol [Queueof Symbol] [Setof Symbol] -> Boolean
(define (breadth-first-search graph end visit blacklist)
  (cond [(queue-empty? visit) #false]
        [else (local [(define dequeued    (dequeue visit))
                      (define visit-first (car dequeued))
                      (define visit-rest  (cdr dequeued))]
                (or (equal? visit-first end)
                    (breadth-first-search graph
                                          end
                                          (if (set-member? blacklist
                                                           visit-first)
                                              visit-rest
                                              (enqueue-list visit
                                                            (hash-ref graph
                                                                      visit-first)))
                                          (set-add blacklist visit-first))))]))

;;; Is there a path from a to b?
;;; Graph Symbol Symbol -> Boolean
(define (path? graph a b)
  (breadth-first-search graph b (enqueue empty-queue a) (set)))