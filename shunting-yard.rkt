#lang racket

;;; SYState is a (make-sy-state List List List)
(define-struct sy-state (input output stack))

;;; Associativity is either 'left or 'right
;;; Operator is either "+", "-", "*", or "/"

;;; OperatorProperties is a (make-operator Number Associativity Number)
(define-struct op-props (preced assoc))

;;; Hash mapping Operators to OperatorProperties
(define operators (hash "+" (make-op-props 1 'left)
                        "-" (make-op-props 1 'left)
                        "*" (make-op-props 2 'left)
                        "/" (make-op-props 2 'left)))

;;; Is given string an Operator?
;;; String -> Boolean
(define (operator? input)
  (hash-has-key? operators input))

;;; Does first operator have lower precendence than second?
;;; Operator Operator -> Boolean
(define (lesser? op-one op-two)
  (let ([op-one-props (hash-ref operators op-one)]
        [op-two-props (hash-ref operators op-two)])
    (or (and (symbol=? (op-props-assoc op-one-props) 'left)
             (<= (op-props-preced op-one-props)
                 (op-props-preced op-two-props)))
        (and (symbol=? (op-props-assoc op-one-props) 'right)
             (< (op-props-preced op-one-props)
                (op-props-preced op-two-props))))))

;;; Steps through the Shunting Yard algorithm for given operator.
;;; SYState -> SYState
(define (sy-operator-step operator state)
  (let ([input  (sy-state-input state)]
        [output (sy-state-output state)]
        [stack  (sy-state-stack state)])
    (cond [(and (not (empty? (sy-state-stack state)))
                (lesser? operator
                         (first stack)))
           (sy-operator-step operator
                             (make-sy-state input
                                            (append output
                                                    (list (first stack)))
                                            (rest stack)))]
          [else (make-sy-state input
                               output
                               (cons operator stack))])))
                               

;;; Steps through the Shunting Yard algorithm after right parenthese.
;;; SYState -> SYState
(define (sy-right-paren-step operator state)
  (let ([input  (sy-state-input state)]
        [output (sy-state-output state)]
        [stack  (sy-state-stack state)])
    (cond [(string=? (first stack) "(") (make-sy-state input
                                                       output
                                                       (rest stack))]
          [else (sy-right-paren-step operator (make-sy-state input
                                                             (append output
                                                                     (list (first stack)))
                                                             (rest stack)))])))

;;; Steps through the Shunting Yard algorithm for given token.
;;; SYState -> SYState
(define (sy-token-step token state)
  (let ([input  (sy-state-input state)]
        [output (sy-state-output state)]
        [stack  (sy-state-stack state)])
    (cond [(operator? token)
           (sy-operator-step token state)]
          [(string=? token "(")
           (make-sy-state input
                          output
                          (cons "(" stack))]
          [(string=? token ")")
           (sy-right-paren-step state)]
          [else
           (make-sy-state input
                          (append output (list token))
                          stack)])))

;;; Steps through the Shunting Yard algorithm.
;;; SYState -> SYState
(define (sy-step state)
  (let ([input  (sy-state-input state)]
        [output (sy-state-output state)]
        [stack  (sy-state-stack state)])
    (cond [(empty? input)
           (make-sy-state empty
                          (append output stack)
                          empty)]
          [else
           (sy-step (sy-token-step (first input)
                                   (make-sy-state (rest input)
                                                  output
                                                  stack)))])))

(sy-state-output (sy-step (make-sy-state (list "1" "+" "2" "*" "4") empty empty)))