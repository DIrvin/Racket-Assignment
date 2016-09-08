#lang Racket

;Derek Irvin
; Professor Gordon
; Homework Assignment 2
; November 10, 2014

;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
; A: Fracktorial
;Factorial function for even numbers
(define (fracktorial x)
  ; if X is less than 2 it can only be 1 or 0. which is only 1 fracktorial wise
  (if (< x 2) 1
      ; we know that even numbers have no remainder. so if there is no
      ; remainder the value of x must be even. So we multiply the value and 
      ; jump to the next even value
          (if (= 0 (modulo x 2)) (* x (fracktorial (- x 2)))
              ; else the value must be odd, we get the next even value and add it.  
              (* (- x 1) (fracktorial(- x 2))))))

;//////////////////////////////////////////////////////////////////////////////////////////////////////

;B Reverse List Halves

; reverses two halves of a list
(define (reverseListHalves L)
; Base Condition to make sure that the List is not empty
  (cond ((null? L)  '())
        ; from here we apply the same way to check if it was even as we did in part A where we know that 
        ; an even number wil not have a remainder from modulus
        ; If the value is even. we will 
                  ((= 0 (modulo (length L) 2)) (appendTo (cdr (splitList (reverseList L) (/ (length L) 2)))(car (splitList (reverseList L) (/ (length L) 2)))))))
                   ; If modulus is equal to One we know that the list is odd sized
                   ; So from here we need to find the middle value. and the idea is to reverse each half of the list. and then append to to each side. 
                   ; to do this we would probably have to find the middle getting the length and using the modulus. then two helper functions for the left and right side
; Determine where to split the list Using the reversed List. and Half the Length. 
(define (splitList L mPosition)
  (define (iterator n L i)
    ; if the list is empty or we are at the middle position
    (if (or (null? L) (= i mPosition))
      (cons (reverse n) L)
      (iterator (cons (car L) n) (cdr L) (+ i 1))
    )
  )
  (iterator '() L 0)
) 

;Reverse the List Variable
(define (reverseList L)
  (if (null? L) '()
     (append (reverseList (cdr L)) (list (car L)))
  )
)

; helper returns the length of the list
;counts all the items in the list
(define (length L)
   (cond ((null? L)
          0)
         (else
          (+ 1 (length (cdr L))))))

;appends x to the end of L
(define (appendTo L x)
  (if (null? L) (cons x L)
                (cons (car L) (appendTo (cdr L) x))))
;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
; C: Sum Picker
;Sum Picker takes two Lists. The first List being the choosers and the second is the selectors. We find the position based on list 1 and we add that value from list two
(define (sumPicker LOne LTwo)
  ; Base Case: We Check List One to make sure there is actually a value in there. Since we cant choose a value to add from List Two if we dont' know what to Choose
  (if (null? LOne) 0
      ; if it is not equal we will add the value of L two with reference to List One. Being we search list two for the position index using a helper
      ; we then take that value and recusively call with the remainder of List One. If we Shorten LTwo it will screw up the position index. Learned this the fun way. 
      (+ (getIndex LTwo (car LOne)) (sumPicker (cdr LOne) LTwo))))

;Helper function for sumPicker to get the Index Item. Returns the item in the list at the index point we need from List One
(define (getIndex LOne i)
  (if (= 1 i) (car LOne)
      (getIndex (cdr LOne) (- i 1))))


;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
; D: Count Evens
;Counts all evens in a list. 
(define (countEvens L)
  (if (null? L) 0
      (if (list? (car L)) (+ (countEvens(car L)) (countEvens(cdr L)))
          ; If even value add one. 
          (if (= 0 (modulo (car L) 2)) (+ 1 (countEvens (cdr L)))
              ; else. continue on with the rest of the list. 
                 (+ 0 (countEvens (cdr L)))))))
      

;//////////////////////////////////////////////////////////////////////////////////////////////
; E Apply Until Too Big

;takes a function and recursively applies it to x until x is greater than 100
(define (applyUntilTooBig f x)
  ; Base Case To make sure that X is not Null. 
   (cond ((null? x)  '())
         ; If X is over 100 then we have reached our limit and we are too big
         ((> x 100) x)
         ; recursively call the function and 'pumped' value of x
         ((< x 100) (applyUntilTooBig f (f x)))))

;Helper Function for Apply Until Too Big returns x time 2
(define (double x) (* x 2))

;///////////////////////////////////////////////////////////////////////////////////////////////
; F: Make Exploder

; Define the Function to be returned (exploder L)
; Followed by calling the Function you named with your list. 
;Define the Two Variables A and B To Apply To The List
(define (makeExploder A B)
  ; Define the List
  (define (exploder L)
    ; Base Case to check if the list is empty. 
    (if (null? L) '()
        ; If the list is not empty however we will Add A to each element and then Multiply by B and recall the exploder with the rest of the lsit to apply again
        (cons (* (+ (car L) A) B) 
              (exploder (cdr L)))))
  exploder
)
