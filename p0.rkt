#lang racket

;; Project 0 Tic-tac-toe with Racket
;; 
;; Please immediately read README.md

(provide board?
         next-player
          valid-move?
          make-move
          winner?
          calculate-next-move)

;; 
;; Useful utility functions
;;

;; Returns the number of elements in l for which the predicate f
;; evaluates to #t. For example:
;;
;;    (count (lambda (x) (> x 0)) '(-5 0 1 -3 3 4)) => 3
;;    (count (lambda (x) (= x 0)) '(-5 0 1 -3 3 4)) => 1
;; 
;; This function is useful for later parts, especially winner?, where
;; you can use it to count if the length of the row is equal to the
;; number of elements in the row containing 'X (or 'O).
(define (count f l)
  (cond [(empty? l) 0]
        [(f (car l)) (add1 (count f (cdr l)))]
        [else (count f (cdr l))]))

;; 
;; Your solution begins here
;;

;; Check whether a list is a valid board
(define (perfect-square-root lst)
  (let ((len (length lst)))
  (= (* (floor (sqrt len)) (floor (sqrt len))) len)))

(define (foo l)
  (match l
    ;; pattern `(,hd . ,tl) matches a cons cell -- the reason is that
    ;; the dot separates the car (left) from the cdr (right)
    ;; the cdr of every list is a list
    [`(,hd . ,tl)
     (displayln "the first element is")
     (displayln hd)]
    [(? cons?)
     (define hd (car l))
     (define tl (cdr l))
     (displayln "the first element is")
     (displayln hd)]
    [(cons hd tl)
     (displayln "the first element is")
     (displayln hd)]))

(define (board-empty? lst)
  (cond
    [(null? lst) #t]
    [(or (equal? (car lst) 'X) (equal? (car lst) 'O)) #f]
    [else (board-empty? (cdr lst))]))
               
      
;;- It contains only the symbols 'X 'O 'E
(define (are-all-XOE b)
  (if (empty? b)
      ;; are all elements of the empty list 'X, 'O, or 'E
      #t
      (cond [(equal? (first b) 'X) (are-all-XOE (rest b))]
            [(equal? (first b) 'O) (are-all-XOE (rest b))]
            [(equal? (first b) 'E) (are-all-XOE (rest b))]
            [else #f])))

(define (are-all-XOE-again2 b)
  (equal? (length b)
          (count (λ (x) (or (equal? x 'X) (equal? x 'O) (equal? x 'E))) b)))

              
  
(define (board? lst)
  (and (integer? (sqrt (length lst))) ;; square
       ;; is every cell of the board 'X, 'O, or 'E
       (are-all-XOE-again2 lst)
       (let ([d (- (count (λ (x) (equal? x 'X)) lst)
                   (count (λ (x) (equal? x 'O)) lst))])
         (and (>= d 0) (<= d 1)))))

;; From the board, calculate who is making a move this turn
;; Get count of X and O, if X and O are the same, Xs turn, else O's turn

(define (count-X-O lst)
  (let ((count-x (count (lambda (x) (equal? x 'X)) lst)) ; Count the X's
        (count-o (count (lambda (x) (equal? x 'O)) lst))) ; Count the O's
    (list count-x count-o))) ;; Return a list of counts
 
(define (next-player board)
  (let* (( counts (count-X-O board))
  (count-x (car counts))
  (count-o (car(cdr counts))))
(if (and (number? count-x) (number? count-o))
    (if (= count-x count-o) 'X 'O)
  (error "next-player: Invalid values" counts))))
  
  

;; If player ('X or 'O) want to make a move, check whether it's this
;; player's turn and the position on the board is empty ('E)
;; 1.You should check that rown and column have to be less than or equal
;; than the length of the board
;; 2. Check if board is empty
;;there should be an and

;;helper function:
(define (is-empty? b r c)
  (equal? (list-ref b (+ c(* r (sqrt (length b))))) 'E))

(define (valid-move? b r c p)
  (let ((size (exact-floor (sqrt (length b))))) ;; Board Size NXN
    (and
   ;; have to check first that r / c are within the bounds of the board
     (<= 0 r (- size 1))
     (<= 0 c (- size 1))
   ;; Check if its players tunr
   (equal? p (next-player b))
   ;; Check if the position is empty
   (is-empty? b r c))))

  
;;(define (valid-move? board row col player)
  ;;'todo)

;; To make a move, replace the position at (row,col) to the player's
;; (either 'X or 'O).
;; 
;; Hint: the board is a one-dimensional list. Figure out a formula for
;; (row,col) in terms of the board and use list-set.
(define (make-move board row col player)
  (list-set board (+ (* row (sqrt (length board))) col) player))

;; Determine whether or not there is a winner
;; 
;; Hint: write a function to grab all rows, all columns, and the
;; diagonals. Then check if any of them (consider using `ormap`) has
;; the property that the count of cells with 'X (or 'O) is equal to
;; its length.
(define (andmap f lst)
  (cond
    [(empty? lst) #t] ;; vacuous truth, "if every element of the empty list ..."
    [(f (first lst)) (andmap f (rest lst))]
    [else #f])) ;; failed the test for f

(define (X-wins-row row)
  (andmap (lambda (e) (equal? e 'X)) row))

(define (player-wins-row row player) ;; player is {'X, 'O, 'E}
  (andmap (lambda (e) (equal? e player)) row))

;; (ormap f lst) -- returns #t iff every element of lst satisfies f
(define (ormap f lst)
  (cond
    [(empty? lst) #f] ;; vacuous truth, "if every element of the empty list ..."
    [(f (first lst)) #t]
    [else (ormap f (rest lst))])) ;; failed the test for f

;; assume that rows is a list of lists. Each row consists of either {'X, 'O, or 'E}
;; return #t iff some list in rows has the property that every element is 'X
(define (player-wins-any rows player)
  (ormap (lambda (row) ;; each row
           (andmap (lambda (e) (equal? e player)) row))
         rows))

(define size 3)

(define (get-kth-column board k)
  (map (lambda (row) (list-ref board (+ (* row size) k))) '(0 1 2)))
  

(define (get-diag b)
  (map
   (λ (i) (list-ref b (+ i (* i (sqrt (length b))))))
   (range (sqrt (length b))))) ;;need to get the right angle aside frm this

(define (get-other-diag b)
  (map
   (λ (i) (list-ref b (+ (- (sqrt (length b)) 1) (* i (sqrt (length b))))))
   (range (sqrt (length b))))) ;;need to get the right angle aside frm this

(define (get-column b i)
  (define s (sqrt (length b)))
  (define l (range 0 s))
  ;; to get the ith column, if x is the row the formula is:
  ;; x * s + i
  (map (λ (x) (list-ref b (+ (* x s) i))) l))

(define (get-columns b)
  (define s (sqrt (length b)))
  (map (λ (i) (get-column b i)) (range s)))

(define (all-rows b)
  (define s (sqrt (length b))) ;; find the size of the board
  (define (h b)
    (if (empty? b)
        '()
           ;; grab the first s cells from b
        (cons (take b s)
           ;; drop the frist s cells from b and recur
              (h (drop b s)))))
  (h b))

  


;;To code (winner? b)
;; 1. Collect a list of rows, columns, and the two diagonals
;; -> Build a list of lists
;; 2. Observe: if any of these lists has all 'x, then 'X wins
(define (winner? b)
  (define rows (all-rows b)) ;; Get rows
  (define cols (get-columns b));; Get cols
  (define diags (list (get-diag b) (get-other-diag b))) ;;Get diagonals
  (define rows-cols-diags (append rows cols diags)) ;combine all above ^^

  (define (is-winner-for? player)
    (ormap (λ (l) (andmap (λ (x) (equal? x player)) l)) rows-cols-diags))

  (cond
  [(is-winner-for? 'X) 'X]
  [(is-winner-for? 'O) 'O]
  [else #f]))
  

;; cond to check a few things
;; 1. 'X wins one row/col/diag
;; 2. 'O wins one row/col/diag
;; 3. neither



;; The board is the list containing E O X
;; Player will always be 'O
;; returns a pair of x and y
(define (calculate-next-move board player)
  'todo)

