;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following line is REQUIRED (do not remove)
(require "puzlib.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;********************************************************************
;; Karan A.Patel (20609643)
;; CS 135 Fall 2015
;; Assignment 10, Problem 2 (puzzle)
;;********************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS

;; A Puzzle is a (list (listof String) (listof String))

;; A Grid is a (listof (listof Char))

(define-struct wpos (row col horiz? len))
;; A WPos (Word Position) is a (make-wpos Nat Nat Bool Nat)
;; requires: len > 1

(define-struct state (grid positions words))
;; A State is a (make-state Grid (listof WPos) (listof Str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:

(define puzz01 (read-puzzle "puzzle01.txt"))
(define grid-abc '((#\A #\B #\C) (#\X #\Y #\Z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED HELPER:

;; (flip wp) transposes wp by reversing row/col and negating horiz?
;; flip: WPos -> WPos
;; Example:
(check-expect (flip (make-wpos 3 4 true 5))
              (make-wpos 4 3 false 5))

(define (flip wp)
  (make-wpos (wpos-col wp) (wpos-row wp) (not (wpos-horiz? wp)) (wpos-len wp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; REQUIRED FUNCTIONS:


;; (transpose g)
;; transpose: Grid -> Grid
;; Examples:
(check-expect (transpose grid-abc) '((#\A #\X) (#\B #\Y) (#\C #\Z)))

(define (transpose g)
  (build-list (length (first g))
              (lambda (x)
                (foldr (lambda (a b)
                         (cons (list-ref a x) b))
                       empty
                       g))))

;; Tests:


;; (find-wpos loc row)
;; find-wpos: (listof Char) Nat -> (listof WPos)
;; Examples:
#|(check-expect (find-wpos (string->list "####") 0)
              (list (make-wpos 0 0 true 4)))|#



(define (find-wpos loc row)
  (local
    [(define (count# list n)
       (cond
         [(empty? list) n]
         [(equal? (first list) #\#) (count# (rest list) (+ n 1))]
         [else (count# (rest list) n)]))
     
     (define (remove-empty list)
       (cond
         [(empty? list) empty]
         [(empty? (first list)) (remove-empty (rest list))]
         [else (cons (first list) (remove-empty (rest list)))]))
     
     (define (pos# list n a templist row)
       (cond
         [(empty? list) empty]
         [(equal? (first list) #\#)
          (pos# (rest list) (+ n 1) (+ a 1) (append templist (cons (first list) empty)) row)]
         [else (remove-empty (cons (cond
                                     [(>= (length templist) 2) (make-wpos row (- n a) true (length templist))]
                                     [else empty])
                                   (pos# (rest list) (+ n 1) 0 empty row)))]))]
    
    (cond
      [(>= (count# loc 0) 2)
       (pos# (append (list #\.) loc (list #\.)) -1 0 empty row)]
      [else (list )])))

;; Tests:
(check-expect (find-wpos (string->list "###") 5)
              (list (make-wpos 5 0 true 3))) 
(check-expect (find-wpos (string->list "..####..") 5)
              (list (make-wpos 5 2 true 4)))
;; the order does not matter: here is an example
;; that uses lists-equiv?
(check-expect (lists-equiv?
               (find-wpos (string->list "..####...###..") 5)
               (list (make-wpos 5 2 true 4)
                     (make-wpos 5 9 true 3)))
              true)
(check-expect (find-wpos (string->list "#.#..#.#") 5)
              empty)


;; (initial-state puzzle)
;; initial-state: Puzzle -> State
;;Examples:
(check-expect (initial-state puzz01)
              (make-state (list (list #\# #\# #\#))
                          (list (make-wpos 0 0 true 3))
                          (list "CAT")))



(define (initial-state puzzle)
  (local
    [(define (grid-char grid-list)
       (cond
         [(empty? grid-list) empty]
         [else (cons (string->list (first grid-list))
                     (grid-char (rest grid-list)))]))
     
     (define (list-wpos grid-list n)
       (cond
         [(empty? grid-list) empty]
         [else (append (find-wpos (first grid-list) n) (list-wpos (rest grid-list) (+ n 1)))]))]
    
    (make-state (grid-char (first puzzle))
                (append (list-wpos (grid-char (first puzzle)) 0)
                        (foldr (lambda (x y)
                                 (cons (flip x) y))
                               empty
                               (list-wpos (transpose (grid-char (first puzzle))) 0)))
                (second puzzle))))

;; Tests:


;; (extract-wpos g wp)
;; extract-wpos: Grid WPos -> (listof Char)
;; Examples: 
(check-expect (extract-wpos grid-abc (make-wpos 0 0 true 2)) '(#\A #\B))
(check-expect (extract-wpos grid-abc (make-wpos 0 0 false 2)) '(#\A #\X))
(check-expect (extract-wpos grid-abc (make-wpos 0 2 false 2)) '(#\C #\Z))

(define (extract-wpos g wp)
  (local
    [(define (row-grid grid-list col)
       (cond
         [(empty? grid-list) empty]
         [(= col 0) (listof# grid-list (wpos-len wp))]
         [else (row-grid (rest grid-list) (- col 1))]))
     
     (define (listof# list len)
       (cond
         [(= len 0) empty]
         [else (cons (first list) (listof# (rest list) (- len 1)))]))]
    
    (cond
      [(wpos-horiz? wp) (row-grid (list-ref g (wpos-row wp)) (wpos-col wp))]
      [else (row-grid (list-ref (transpose g) (wpos-row (flip wp))) (wpos-col (flip wp)))])))

;; Tests:


;; (replace-wpos g wp loc)
;; replace-wpos: Grid WPos (listof Char) -> Grid
;; requires: len in WPos is equal to length of (listof Char)
;; Examples:
(check-expect (replace-wpos grid-abc (make-wpos 0 0 true 2) '(#\J #\K))
              '((#\J #\K #\C) (#\X #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 0 false 2) '(#\J #\K))
              '((#\J #\B #\C) (#\K #\Y #\Z)))
(check-expect (replace-wpos grid-abc (make-wpos 0 2 false 2) '(#\J #\K))
              '((#\A #\B #\J) (#\X #\Y #\K)))

(define (replace-wpos g wp loc)
  (local
    [(define (word list loc len)
       (cond
         [(empty? list) loc]
         [(= len 0) (append loc list)]
         [else (word (rest list) loc (- len 1))]))
     (define (modify list loc col wp)
       (cond
         [(empty? list) empty]
         [(= col 0) (append (word list loc (wpos-len wp)) empty)]
         [else (cons (first list) (modify (rest list) loc (- col 1) wp))]))
     (define (final-list g wp loc row)
       (cond
         [(empty? g) empty]
         [(= row 0) (cons (modify (first g) loc (wpos-col wp) wp) (rest g))]
         [else (cons (first g) (final-list (rest g) wp loc (- row 1)))]))]
    (cond    
      [(wpos-horiz? wp) (final-list g wp loc (wpos-row wp))]
      [else (transpose (final-list (transpose g) (flip wp) loc (wpos-row (flip wp))))])))


;; Tests:


;; (fit? word cells)
;; fit? (listof Char) (listof Char) -> Bool
;; Examples:
(check-expect (fit? (string->list "STARWARS") (string->list "S##RW##S")) true)
(check-expect (fit? (string->list "STARWARS") (string->list "S##RT##K")) false)

(define (fit? word cells)
  (local
    [(define (check words cells)
       (cond
         [(empty? words) true]
         [(or (equal? (first cells) #\#)
              (equal? (first words) (first cells))) (check (rest words) (rest cells))]
         [else false]))]
    (cond
      [(= (length word) (length cells))
       (check word cells)]
      [else false])))

;; Tests:


;; (neighbours s)
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state (list (list #\# #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      (list "CAT")))
              (list (make-state '((#\C #\A #\T)) empty empty)))


(define (neighbours s)
  (local
    [(define (lo-ex-wpos my-grid lowpos)
       (cond
         [(empty? lowpos) empty]
         [else (cons (extract-wpos my-grid (first lowpos))
                     (lo-ex-wpos my-grid (rest lowpos)))]))
     
     (define (counter my-grid wpos)
       (foldr
        (lambda (x y)
          (cond
            [(not (char=? x #\#)) (+ y 1)]
            [else y]))
        0
        (extract-wpos my-grid wpos)))
     
     (define (best-wpos grid lo-ex-wp acc)
       (cond [(empty? lo-ex-wp) acc]
             [(> (counter grid (first lo-ex-wp)) (counter grid acc))
              (best-wpos grid (rest lo-ex-wp) (first lo-ex-wp))]
             [else (best-wpos grid (rest lo-ex-wp) acc)]))
     (define (change-grid my-grid wp lostr)
       (cond
         [(empty? lostr) empty]
         [(fit? (string->list (first lostr)) (extract-wpos my-grid wp) )
          (cons (first lostr) (change-grid my-grid wp (rest lostr)))]
         [else (change-grid my-grid wp (rest lostr))]))
     
     (define (final state wpos lostr)
       (cond
         [(empty? lostr) empty]
         [else (cons (make-state (replace-wpos (state-grid state)
                                               wpos
                                               (string->list (first lostr)))
                                 (remove wpos (state-positions state))
                                 (remove (first lostr) (state-words state)))
                     (final state wpos (rest lostr)))]))
     (define best-wp (best-wpos 
                      (state-grid s)
                      (rest (state-positions s))
                      (first (state-positions s))))]
    
    (final s best-wp
           (change-grid (state-grid s)
                        best-wp
                        (state-words s)))))

;; Tests:
#|(check-expect (neighbours (make-state '((#\C #\# #\#))
                                      (list (make-wpos 0 0 true 3))
                                      '("CAT" "DOG" "CAR")))
              (list (make-state '((#\C #\A #\T)) empty '("DOG" "CAR"))
                    (make-state '((#\C #\A #\R)) empty '("CAT" "DOG"))))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDED FUNCTIONS:

;; (solved? s) determines if s is a solved criss-cross problem
;;   by checking if all of the word positions have been filled
;; solved?: State -> Bool
(define (solved? s)
  (empty? (state-positions s)))


;; (criss-cross puzzle) produces a list of strings corresponding
;;   to the solution of the the criss-cross puzzle,
;;   or false if no solution is possible
;; criss-cross: Puzzle -> (anyof false (listof Str))

(define (criss-cross puzzle)
  (local [(define result (solve (initial-state puzzle)
                                neighbours
                                solved?))]
    (cond [(false? result) false]
          [else (map list->string (state-grid result))])))

(check-expect (criss-cross puzz01) '("CAT"))

;; note that [solve] (used in criss-cross above) is provided in puzlib.rkt

;; when you are all done, you can use disp to
;; view your solutions:

 (disp (criss-cross (read-puzzle "puzzle10.txt")))

;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

