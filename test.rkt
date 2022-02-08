#lang racket

(require "main.rkt")

;======================================= foldr =======================================

; foldr-test-sum - my-foldr test 1
(define (foldr-test-sum lst)
  (my-foldr lst + 0))

; foldr-test-min - my-foldr test 2
(define (foldr-test-min lst)
  (my-foldr lst min (car lst)))

; foldr-test-count - my-foldr test 3
(define (foldr-test-count lst)
  (my-foldr lst (lambda (e res) (+ 1 res)) 0))

; foldr-test-all - run all my-foldr tests
; parameters: lst (OPTIONAL) - input list
; return: list containing test results
(define (foldr-test-all [lst '(2 -1 8 6 -2 3 3)])
  (list (foldr-test-sum lst) (foldr-test-min lst) (foldr-test-count lst)))

;======================================= foldl =======================================

; foldl-test-sum - my-foldl test 1
(define (foldl-test-sum lst)
  (my-foldl lst + 0))

; foldl-test-min - my-foldl test 2
(define (foldl-test-min lst)
  (my-foldl lst min (car lst)))

; foldl-test-count - my-foldl test 3
(define (foldl-test-count lst)
  (my-foldl lst (lambda (res e) (+ 1 res)) 0))

; foldl-test-all - run all my-foldl tests
; parameters: lst (OPTIONAL) - input list
; return: list containing test results
(define (foldl-test-all [lst '(2 -1 8 6 -2 3 3)])
  (list (foldl-test-sum lst) (foldl-test-min lst) (foldl-test-count lst)))

;===================================== foldtree ======================================

; foldtree-test-binary-inorder - my-foldtree test 1
(define (foldtree-test-binary-inorder [tree '(10 (5 (2 (0 () ()) (4 () ())) (7 () ())) (15 (12 () ()) (20 () ())))])
  (my-foldtree tree (lambda (root left right) (append left (list root) right)) null))

; foldtree-test-ternary-preorder - my-foldtree test 2
(define (foldtree-test-ternary-preorder [tree '(5 (2 (-3 () () ()) () ()) (7 () () ()) (4 (1 () () ()) () ()))])
  (my-foldtree tree (lambda (root left middle right) (append (list root) left middle right)) null))

; foldtree-test-all - run all my-foldtree tests
; return: list containing test results
(define (foldtree-test-all)
  (list (foldtree-test-binary-inorder) (foldtree-test-ternary-preorder)))

;======================================== map ========================================

; map-test-power - my-map test 1
(define (map-test-power lst)
  (my-map lst (lambda (x) (* x x))))

; map-test-list - my-map test 2
(define (map-test-list lst)
  (my-map lst list))

; map-test-zero - my-map test 3
(define (map-test-zero lst)
  (my-map lst (lambda (x) (- x x))))

; map-test-all - run all my-map tests
; parameters: lst (OPTIONAL) - input list
; return: list containing test results
(define (map-test-all [lst '(2 -1 8 6 -2 3 3)])
  (list (map-test-power lst) (map-test-list lst) (map-test-zero lst)))

;====================================== reduce =======================================

; reduce-test-sum - my-reduce test 1
(define (reduce-test-sum lst)
  (my-reduce lst +))

; reduce-test-min - my-reduce test 2
(define (reduce-test-product lst)
  (my-reduce lst *))

; reduce-test-all - run all my-reduce tests
; parameters: lst (OPTIONAL) - input list
; return: list containing test results
(define (reduce-test-all [lst '(2 -1 8 6 -2 3 3)])
  (list (reduce-test-sum lst) (reduce-test-product lst)))

;====================================== filter =======================================

; filter-test-odd - my-filter test 1
(define (filter-test-odd lst)
  (my-filter lst (lambda (x) (equal? (modulo x 2) 0))))

; filter-test-negative - my-filter test 2
(define (filter-test-negative lst)
  (my-filter lst (lambda (x) (>= x 0))))

; filter-test-all - run all my-filter tests
; parameters: lst (OPTIONAL) - input list
; return: list containing test results
(define (filter-test-all [lst '(2 -1 8 6 -2 3 3)])
  (list (filter-test-odd lst) (filter-test-negative lst)))

;==================================== quick sort =====================================

; quicksort-test- - run quicksort test
; parameters: lst (OPTIONAL) - input list
; return: sorted list
(define (quicksort-test [lst '(2 -1 8 6 -2 3 3)])
  (my-quicksort lst <))

;==================================== merge sort =====================================

; mergesort-test- - run mergesort test
; parameters: lst (OPTIONAL) - input list
; return: sorted list
(define (mergesort-test [lst '(2 -1 8 6 -2 3 3)])
  (my-mergesort lst <))
