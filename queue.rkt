#lang racket

(provide empty-queue
         queue-empty?
         enqueue
         enqueue-list
         enqueue-set
         dequeue)

;;; A [Queueof T] is a (make-queue [Listof T] [Listof T])
(define-struct queue [enqueue dequeue])

(define empty-queue (make-queue '() '()))

;;; Is queue empty?
;;; [Queueof T] -> Boolean
(define (queue-empty? queue)
  (and (empty? (queue-enqueue queue))
       (empty? (queue-dequeue queue))))

;;; Adds element to end of queue.
;;; [Queueof T] T -> [Queueof T]
(define (enqueue queue item)
  (make-queue (cons item (queue-enqueue queue))
              (queue-dequeue queue)))

;;; Adds list of elements to end of queue.
;;; [Queueof T] [Listof T] -> [Queueof T]
(define (enqueue-list queue items)
  (foldl (λ (item curr-queue)
           (enqueue curr-queue item))
         queue items))

;;; Adds set of elements to end of queue.
;;; [Queueof T] [Setof T] -> [Queueof T]
(define (enqueue-set queue items)
  (enqueue-list queue (set->list items)))

;;; Removes first element from queue.
;;; [Queueof T] -> [Pairof T [Queueof T]]
(define (dequeue queue)
  (cond [(queue-empty? queue)
         (error "Cannot remove first item from empty queue.")]
        [(empty? (queue-dequeue queue))
         (local [(define reversed-enqueue (reverse (queue-enqueue queue)))]
           (cons (first reversed-enqueue)
                 (make-queue '() (rest reversed-enqueue))))]
        [else (cons (first (queue-dequeue queue))
                    (make-queue (queue-enqueue queue)
                                (rest (queue-dequeue queue))))]))
