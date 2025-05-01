#lang racket

;; Define success and failure constructors for Either/Result
(struct success (value) #:transparent)
(struct failure (message) #:transparent)

;; Helper functions similar to from-just for Maybe
(define (from-success default result)
  (if (success? result)
      (success-value result)
      default))

(define (from-failure default result)
  (if (failure? result)
      (failure-message result)
      default))

;; Safe division that returns Either instead of Maybe
(define (safe-div x y)  ; num num -> Either
  (if (= y 0)
      (failure "Division by zero")
      (success (/ x y))))

;; Helper function to check if an item is in a list
(define in-list?   ; item list -> bool 
  (λ (x lst)
    (not (false? (member x lst)))))

;; Valid ID check - start with letter, followed by letters, digits, hyphens, or underscores
(define (valid-id? id)
  (and (symbol? id)
       (let ([str (symbol->string id)])
         (and (not (empty? str))
              (char-alphabetic? (string-ref str 0))
              (for/and ([c (string->list str)])
                (or (char-alphabetic? c)
                    (char-numeric? c)
                    (char=? c #\-)
                    (char=? c #\_)))))))

;; Convert an expression result and state into an Either with state
(define (with-state result state)
  (if (success? result)
      (success (list (success-value result) state))
      result))

;; Lookup a variable in the state
(define (lookup-var var state)
  (let ([pair (assoc var state)])
    (if pair
        (let ([value (second pair)])
          (if (eq? value 'undefined)
              (failure (format "Variable '~a' is undefined" var))
              (success value)))
        (failure (format "Variable '~a' not found" var)))))

;; Add or update a variable in the state
(define (update-state var value state)
  (let ([pair (assoc var state)])
    (if pair
        (map (lambda (p) (if (equal? (first p) var) 
                            (list var value) 
                            p)) 
             state)
        (cons (list var value) state))))

;; Evaluator function that takes an expression and state, returns result and new state
(define (eval expr state)
  (cond
    ;; Number literal
    [(equal? (first expr) 'num) 
     (success (list (second expr) state))]
    
    ;; Variable reference
    [(equal? (first expr) 'id)
     (let ([var (second expr)])
       (if (valid-id? var)
           (let ([result (lookup-var var state)])
             (if (success? result)
                 (success (list (success-value result) state))
                 result))
           (failure (format "Invalid identifier: ~a" var))))]
    
    ;; Define a variable
    [(equal? (first expr) 'define)
     (let ([var (second expr)])
       (if (not (valid-id? var))
           (failure (format "Invalid identifier: ~a" var))
           (let ([pair (assoc var state)])
             (if pair
                 (failure (format "Variable '~a' already defined" var))
                 (if (= (length expr) 2)
                     ;; Just declare with undefined value
                     (success (list 'undefined (update-state var 'undefined state)))
                     ;; Define with initial value
                     (let ([result (eval (third expr) state)])
                       (if (success? result)
                           (let ([value (first (success-value result))]
                                 [new-state (second (success-value result))])
                             (success (list value (update-state var value new-state))))
                           result)))))))]
    
    ;; Assign to an existing variable
    [(equal? (first expr) 'assign)
     (let ([var (second expr)])
       (if (not (valid-id? var))
           (failure (format "Invalid identifier: ~a" var))
           (let ([pair (assoc var state)])
             (if (not pair)
                 (failure (format "Variable '~a' not defined" var))
                 (let ([result (eval (third expr) state)])
                   (if (success? result)
                       (let ([value (first (success-value result))]
                             [new-state (second (success-value result))])
                         (success (list value (update-state var value new-state))))
                       result))))))]
    
    ;; Remove a variable
    [(equal? (first expr) 'remove)
     (let ([var (second expr)])
       (if (not (valid-id? var))
           (failure (format "Invalid identifier: ~a" var))
           (let ([pair (assoc var state)])
             (if (not pair)
                 (begin
                   (displayln (format "Error: remove ~a: variable not defined, ignoring" var))
                   (success (list 'ok state)))
                 (success (list 'ok (filter (lambda (p) (not (equal? (first p) var))) state)))))))]
    
    ;; Arithmetic operations
    [(in-list? (first expr) '(div add sub mult))
     (let ([result-x (eval (second expr) state)])
       (if (success? result-x)
           (let ([x (first (success-value result-x))]
                 [mid-state (second (success-value result-x))])
             (let ([result-y (eval (third expr) mid-state)])
               (if (success? result-y)
                   (let ([y (first (success-value result-y))]
                         [final-state (second (success-value result-y))])
                     (case (first expr)
                       [(div) (let ([div-result (safe-div x y)])
                                (if (success? div-result)
                                    (success (list (success-value div-result) final-state))
                                    div-result))]
                       [(add) (success (list (+ x y) final-state))]
                       [(sub) (success (list (- x y) final-state))]
                       [(mult) (success (list (* x y) final-state))]))
                   result-y)))
           result-x))]
    
    ;; Unknown operation
    [else (failure (format "Unknown operation: ~a" (first expr)))]))

;; REPL loop
(define (repl)
  (let loop ([state '()])
    (display "expr> ")
    (flush-output)
    (let ([input (read)])
      (if (eq? input 'quit)
          (displayln "Goodbye!")
          (begin
            ;; Check if the input is already quoted
            (let* ([expr (if (and (pair? input) (eq? (car input) 'quote))
                            (cadr input)  ; Extract the quoted expression
                            input)]       ; Use as is if not quoted
                   [result (eval expr state)])
              (if (success? result)
                  (let ([value (first (success-value result))]
                        [new-state (second (success-value result))])
                    (printf "Result: ~a\n" value)
                    (printf "State: ~a\n" new-state)
                    (loop new-state))
                  (begin
                    (printf "Error: ~a\n" (failure-message result))
                    (printf "State: ~a\n" state)
                    (loop state)))
              (newline)))))))

;; Example expressions to try in the REPL:
#|
;; Basic arithmetic operations
'(num 5)                                               ; Simple number literal
'(add (num 5) (mult (num 2) (num 3)))                  ; 5 + (2 * 3) = 11
'(sub (num 20) (div (add (mult (num 4) (num 5)) (num 10)) (num 6)))  ; 20 - ((4 * 5) + 10) / 6 = 15
'(div (num 5) (sub (num 5) (num 5)))                   ; Division by zero error

;; Variable operations
'(define a)                                            ; Define a as undefined
'(define b (num 10))                                   ; Define b as 10
'(id b)                                                ; Get value of b (10)
'(define c (add (id b) (num 5)))                       ; Define c as b + 5 (15)
'(id a)                                                ; Error: a is undefined
'(id z)                                                ; Error: z not found
'(assign a (num 7))                                    ; Assign 7 to a
'(id a)                                                ; Get value of a (7)
'(assign b (add (id b) (num 1)))                       ; Increment b by 1
'(id b)                                                ; Get value of b (11)
'(remove c)                                            ; Remove variable c
'(id c)                                                ; Error: c not found
'(remove z)                                            ; Error: z not defined
'(define complex-expr (div (add (id a) (id b)) (num 2)))  ; Define complex expression
'(id complex-expr)                                     ; Get value (9)

;; Type quit to exit the REPL
|#

;; Start the REPL
(repl)