#lang racket

(provide (all-defined-out))

;======================================= foldr =======================================

; my-foldr - foldr function
; parameters: lst - input list, fn - function, init - initial value for empty list
(define (my-foldr lst fn init)
  (if (null? lst)
      init
      (fn (car lst) (my-foldr (cdr lst) fn init))))

;======================================= foldl =======================================

; my-foldr - foldl function
; parameters: lst - input list, fn - function, init - initial value for empty list
(define (my-foldl lst fn init)
  (if (null? lst)
      init
      (my-foldl (cdr lst) fn (fn init (car lst)))))

;===================================== foldtree ======================================

; my-foldtree - foldtree function
; parameters: lst - input list, fn - function, init - initial value for empty list
(define (my-foldtree tree fn init)
  (if (null? tree)
      init
      (apply fn (car tree) (map (lambda (x) (my-foldtree x fn init)) (cdr tree)))))

;======================================== map ========================================

; my-map - map function
; parameters: lst - input list, fn - function
(define (my-map lst fn)
  (my-foldr lst (lambda (e res) (cons (fn e) res)) null))

;====================================== reduce =======================================

; my-reduce - reduce function
; parameters: lst - input list, fn - function
(define (my-reduce lst fn)
  (if (null? lst)
      #f
      (my-foldr (cdr lst)  (lambda (e res) (fn e res)) (car lst))))

;====================================== filter =======================================

; my-filter - filter function
; parameters: lst - input list, fn - function
(define (my-filter lst fn)
  (my-foldr lst
            (lambda (e res) (if (fn e)
                                (cons e res)
                                res))
            null))

;==================================== quick sort =====================================

; my-quicksort - quicksort function
; parameters: lst - input list, cmp - compare function (the function must not return true on equality!)
(define (my-quicksort lst cmp)
  (cond
    ((null? lst)       null)
    ((null? (cdr lst)) lst)
    (#t (let [(pivot (car lst))]
          (append
           (my-quicksort (my-filter lst (lambda (x) (cmp x pivot))) cmp)
           (my-filter lst (lambda (x) (equal? pivot x)))
           (my-quicksort (my-filter lst (lambda (x) (not (or (cmp x pivot) (equal? x pivot))))) cmp))))))
     
;==================================== merge sort =====================================

(define (my-split lst)
  (my-split-aux lst lst))

(define (my-split-aux slow fast)
  (if (or (null? fast) (null? (cdr fast)))
      (cons null slow)
      (let
          [(res (my-split-aux (cdr slow) (cddr fast)))]
        (cons (cons (car slow) (car res)) (cdr res)))))

(define (my-merge l1 l2 cmp)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    ((cmp (car l1) (car l2)) (cons (car l1) (my-merge (cdr l1) l2 cmp)))
    (#t                      (cons (car l2) (my-merge l1 (cdr l2) cmp)))))

; my-mergesort - mergesort function
; parameters: lst - input list, cmp - compare function
(define (my-mergesort lst cmp)
  (cond
    ((null? lst) null)
    ((null? (cdr lst)) lst)
    (#t (let
            [(split (my-split lst))]
          (my-merge (my-mergesort (car split) cmp)
                    (my-mergesort (cdr split) cmp)
                    cmp)))))
