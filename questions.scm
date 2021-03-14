(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
    (list (map car pairs) (map cadr pairs))
    )


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (sub_enum idx current s)
      (if (null? s) current
          (sub_enum (+ idx 1) (append current (list (cons idx (list (car s))))) (cdr s)
              )
          )
      )
  (sub_enum 0 nil s)
  )
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (define (merge_helper comp current lst1 lst2)
      (cond ((null? lst1) (append current lst2))
            ((null? lst2) (append current lst1))
            ((comp (car lst1) (car lst2))
                 (merge_helper comp (append current (list (car lst1))) (cdr lst1) lst2)
                 )
             (else
                 (merge_helper comp (append current (list (car lst2))) lst1 (cdr lst2))
             ))
            )
  (merge_helper comp '() list1 list2)
  )
  ; END PROBLEM 16


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Problem 17

(define (nondecreaselist s)
    ; BEGIN PROBLEM 17
    (define (last_element l)
      (cond ((null? (cdr l)) (car l))
            (else (last_element (cdr l)))))

    (define (helper archived current lst)
        (cond ((null? lst) (append archived (list current)))
              ((null? current) (helper archived (list (car lst)) (cdr lst)))
              ((<= (last_element current) (car lst)) (helper archived (append current (list (car lst))) (cdr lst)))
              (else (helper (append archived (list current)) nil lst)
                  )
            )
        )
    (helper nil nil s)
    )
    ; END PROBLEM 17

;; Problem EC
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM EC
         expr
         ; END PROBLEM EC
         )
        ((quoted? expr)
         ; BEGIN PROBLEM EC
         expr
         ; END PROBLEM EC
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM EC
           (cons form (cons params (map let-to-lambda body)))
           ; END PROBLEM EC
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM EC
          (append (list (cons 'lambda (cons (car (zip values)) (map let-to-lambda body)))) (map let-to-lambda (cadr (zip values))))
           ; END PROBLEM EC
           ))
        (else
         ; BEGIN PROBLEM EC
         (cons (car expr) (map let-to-lambda (cdr expr)))
         ; END PROBLEM EC
         )))


;; A list representing the let-to-lambda procedure
(define let-to-lambda-code
  '(define (let-to-lambda expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (map let-to-lambda body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
          (append (list (cons 'lambda (cons (car (zip values)) (map let-to-lambda body)))) (map let-to-lambda (cadr (zip values))))
           ))
        (else
         (cons (car expr) (map let-to-lambda (cdr expr)))
         )))
)

;; A let-to-lambda procedure that does not use 'let'!
(define let-to-lambda-without-let
  (let-to-lambda let-to-lambda-code))
