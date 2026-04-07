#lang racket

; Board data structure:
;((0 0 0 0)
; (0 0 0 0)
; (0 0 2 0)
; (0 0 0 2))
; The board is a list of rows. A row is a list of numbers.
; 0 represents an empty tile and positive numbers represent usable tiles

; TileAmount must be (* x y)
; CurrentTile starts at 0
; x is the width (number of columns) of the board.
; y is the height (number of rows) of the board.
; Board starts as an empty list.
(define (GenerateMatrix tile_amount current_tile x y board)
  (cond [(>= current_tile tile_amount) board] ; base case returns the completed board when all tiles have been created.
        [else (GenerateMatrix
               tile_amount
               (+ current_tile 1)
               x
               y
               (cons (GenerateRow 0 x '()) board)
               )]))

; Builds a row of 0 for the matrix.
; row must start as nil.
(define (GenerateRow current_tile x row) 
  (cond
    [(>= current_tile x) row]
    [else (GenerateRow (+ current_tile 1) x (cons 0 row))]))
; [else (cons 0 (GenerateRow (+ current_tile 1) x row))]))

; ------------------------------------------------- Matrix movement logic
; --------- MoveRightRow --------- Old Idea
; ; Calculate a new row based on the current row and moving right. 
; ; Receives a row of the matrix and returns that row after the desired move.
; ; Start values (MoveRightRow x 0 '(the_given_row_to_calculate x #f)) <--> (MoveRightRow x 0 '(the_given_row_to_calculate the length_of_the_row should_eliminate_number))
; (define (MoveRightRow expected_size left_tile row)
;     (cond
;         [(= (caar row) 0) (MoveRightRow (- expected_size 1) left_tile (cons (cdar row) cdr row))])
;     )
; (define (GetRestOfRow row) (cons (cdar row) cdr row))



; Calculate a new row based on the current row and moving left.
; Receives a row of the matrix and returns that row after a move to the left.
; Traverses the row to build a list of succesful matches to add together (this is a list of booleans),
; then builds the new row using the original row and and the succesful matches list.
(define (MoveLeftRow row)
  (MoveLeftRowAux row (FindSuccesfulMatches row '() 0) #f 0)
  )
(define (MoveRightRow row)
  (reverse (MoveLeftRowAux (reverse row) (FindSuccesfulMatches (reverse row) '() 0) #f 0))
  )
; Applies MoveLeftRow to each row in a matrix.
; Returns a matrix with the result.
(define (MoveLeftMatrix matrix)
  (cond
    [(empty? matrix) '()]
    [else (cons (MoveLeftRow (car matrix)) (MoveLeftMatrix (cdr matrix)))]))
(define (MoveRightMatrix matrix)
  (cond
    [(empty? matrix) '()]
    [else (cons (MoveRightRow (car matrix)) (MoveRightMatrix (cdr matrix)))]))
(define (MoveUpMatrix matrix)
  (Transpose (MoveLeftMatrix (Transpose matrix))))
(define (MoveDownMatrix matrix)
  (Transpose (MoveRightMatrix (Transpose matrix))))



; Starting values: the_given_row_to_calculate, FindSuccesfulMatches, #f
; Builds the resulting list with leading zeros.
(define (MoveLeftRowAux row succesful_matches is_adding_pair add_zero_count)
  ; (display "row: ") (display row) (display "| succesful_matches: ") (display succesful_matches) (display "| is_adding_pair: ") (display is_adding_pair)  (display "| add_zero_count: ") (display add_zero_count)
  ; (displayln "")
  (cond
    [(empty? row) (ZerosList '() add_zero_count)]   ; End Recursion by adding the leading zeros.
    ; Skip zeros.
    [(equal? (car row) 0) (MoveLeftRowAux (cdr row) succesful_matches is_adding_pair (+ add_zero_count 1))]
    [else (cond
            [is_adding_pair (cons (* (car row) 2) (MoveLeftRowAux (cdr row) (cdr succesful_matches) #f add_zero_count))]
            [else (cond
                    [(not (car succesful_matches)) (cons (car row) (MoveLeftRowAux (cdr row) (cdr succesful_matches) #f add_zero_count))]
                    [else (MoveLeftRowAux (cdr row) succesful_matches #t (+ add_zero_count 1))]
                    )]
            )]
    )
  )
; Starting values: the_given_row_to_calculate, nil, 0
(define (FindSuccesfulMatches row succesful_matches last_number)
  (cond
    ; Stop condition.
    [(empty? row) (cond
                    ; Exit the function normally.
                    [(equal? last_number 0) (reverse succesful_matches)]
                    ; Exit the function when the last number couldn't be paired.
                    [else (reverse (cons #f succesful_matches))])]    
    ; If the current number is 0, skip it.
    [(equal? (car row) 0) (FindSuccesfulMatches (cdr row) succesful_matches last_number)]
    [else (cond
            ; If there was no number to match enter recursion with the current number as a possible pair.
            [(equal? last_number 0) (FindSuccesfulMatches (cdr row) succesful_matches (car row))]
            [else (cond
                    ; If a pair is found, add #t to the matches and enter recursion with 0 as last_number.
                    [(equal? (car row) last_number) (FindSuccesfulMatches (cdr row) (cons #t succesful_matches) 0)]
                    ; If not, add #f and set the current number as a possible pair. 
                    [else (FindSuccesfulMatches (cdr row) (cons #f succesful_matches) (car row))])])]))

; Builds a list of zeros of the given length.
; Starting values: nil, length
(define (ZerosList list length)
  (cond
    [(<= length 0) list]
    [else (ZerosList (cons 0 list) (- length 1))]))

; Matrix transpose
(define (Transpose matrix)
  ; (display "Transpose: matrix = ") (display matrix)
  ; (displayln "")
  (cond
    [(empty? (car matrix)) '()]
    [else (cons (BuildColumn matrix) (Transpose (EliminateColumn matrix)))]))
; Start values: matrix
(define (BuildColumn matrix)
  ; (display "BuildColumn: matrix = ") (display matrix)
  ; (displayln "")
  (cond
    [(empty? matrix) '()]
    [else (cons (caar matrix) (BuildColumn (cdr matrix)))]))
; Start values: matrix
(define (EliminateColumn matrix)
  ; (display "EliminateColumn: matrix = ") (display matrix)
  ; (displayln "")
  (cond
    [(empty? matrix) '()]
    [else (cons (cdar matrix) (EliminateColumn (cdr matrix)))]))



; ------------------------------------------------- Insert random values and win lose logic
; Returns the given matrix with a random number (2 or 4) in place of a random zero.
; Returns a matrix
; If there is no space to add a number, this function returns the matrix as is
(define (InsertRandomNumber matrix)
  (cond
    [(> (CountZeros matrix) 0) 
     (ReplaceZeroInMatrix matrix (CountZerosMatrix matrix) (RandomTileGenerator2048) (random (CountZeros matrix)) 0)]
    [else (display "No free space in (InsertRandomNumber matrix).") matrix])
  )
; Returns 2 or 4 at random with a 8:1 ratio (a bit more 4s than in the game).
(define (RandomTileGenerator2048)
  (cond
    [(> (random) (/ 1 9)) 2]
    [else 4]))
; Counts the amount of zeros in the matrix.
(define (CountZeros matrix)
  (cond
    ; Stop condition.
    [(empty? matrix) 0]
    ; Advance to the next row if the current one is empty
    [(empty? (car matrix)) (CountZeros (cdr matrix))]
    ; Is the current tile a zero?
    [(zero? (caar matrix)) (+ 1 (CountZeros (cons (cdar matrix) (cdr matrix))))]
    [else (CountZeros (cons (cdar matrix) (cdr matrix)))]
    ))
; Sums all the integers of a list.
(define (SumList list)
  (cond
    [(empty? list) 0]
    [else (+ (car list) (SumList (cdr list)))]))

; Counts the number of zeros in a list of numbers.
(define (CountZerosRow row)
  (cond
    ; Stop condition.
    [(empty? row) 0]
    ; Is the current tile a zero?
    [(zero? (car row)) (+ 1 (CountZerosRow (cdr row)))]
    [else (CountZerosRow (cdr row))]
    ))
; Counts the amount of zeros in each row of the matrix.
; Returns a list of numbers
(define (CountZerosMatrix matrix)
  (cond
    ; Stop condition.
    [(empty? matrix) '()]
    [else (cons (CountZerosRow (car matrix)) (CountZerosMatrix (cdr matrix)))]
    ))

; Changes the n-th zero in the matrix with number. This functions needs the zero to exist.
; returns the matrix.
; --> Main functionality for generating random numbers here! <--
(define (ReplaceZeroInMatrix matrix zeros_list number n zero_count)
  ; (display "matrix: ") (display matrix) (display "| zeros_list: ") (display zeros_list) (display "| number: ") (display number) (display "| n: ") (display n) (display "| zero_count: ") (display zero_count)
  ; (displayln "")
  (cond
    [(empty? matrix) '()]   ; This condition shouldn't be reached.
    [(> (+ (car zeros_list) zero_count) n) (cons (ReplaceZeroInRow (car matrix) number (- n zero_count) 0) (cdr matrix))]
    [else (cons (car matrix) (ReplaceZeroInMatrix (cdr matrix) (cdr zeros_list) number n (+ zero_count (car zeros_list))))]
    )
  )
; Replaces the n-th zero in the row with number.
; Returns the row.
(define (ReplaceZeroInRow row number n zero_count)
  ; (display "row: ") (display row) (display "| number: ") (display number) (display "| n: ") (display n) (display "| zero_count: ") (display zero_count)
  ; (displayln "")
  (cond
    ; This condition shouldn't be reached.
    [(empty? row) -1]
    ; Is the current tile a zero?
    [(zero? (car row)) (cond
                         [(>= zero_count n) (cons number (cdr row))]
                         [else (cons (car row) (ReplaceZeroInRow (cdr row) number n (+ zero_count 1)))]
                         )]
    [else (cons (car row) (ReplaceZeroInRow (cdr row) number n zero_count))]
    ))

; (define (InsertRandomNumber matrix number n zero_count)
;     (cond
;         ; Stop condition.
;         [(empty? matrix) '()]
;         ; Advance to the next row if the current one is empty
;         [(empty? (car matrix)) (list (InsertRandomNumber (cdr matrix) number n zero_count))]
;         ; Is the current tile a zero?
;         [(zero? (caar matrix)) (InsertRandomNumber (cons (cdar matrix) (cdr matrix)) number n (+ zero_count 1))]
;         [else (InsertRandomNumber (cons (cdar matrix) (cdr matrix)))]
;         ))


; Add a random 2 or 4 on a random tile  !!!!!!!!



















(provide GenerateMatrix InsertRandomNumber MoveUpMatrix MoveDownMatrix MoveLeftMatrix MoveRightMatrix CountZeros )