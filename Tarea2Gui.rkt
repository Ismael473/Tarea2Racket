#lang racket/gui
(require "CE2048.rkt")

; --- Starting state variables ---
(define rows 4)    ; N (Rows)
(define columns 4) ; M (Columns)
(define winCondition 2048) ; Sets the initial Win condition to reach 2048

(define GameMatrix (GenerateMatrix (* rows columns) 0 rows columns '() )); Calls the GenerateMatrix feature and sends
(define score 0)

(define (tileColor value)
  (if (= value 0)
      "white"
      (make-object color%
        255
        (inexact->exact(max 50 (- 255 (* 20 (log value 2)))))
        (inexact->exact(max 50 (- 255 (* 20 (log value 2)))))
        )
      )
  )

(define frame 
  (new frame% 
       [label "Configurador de Cuadrícula 2048"] 
       [width 500] 
       [height 500]))

; Horizontal Pane for the controls
(define control-panel ;This is the space where the sliders, text field and buttons are gonna be displayed.
  (new horizontal-panel% 
       [parent frame] 
       [alignment '(center center)] 
       [stretchable-height #f]))

; --- Selection Controls ---
(define row-slider ;This slider controls how many rows are gonna be on the game board
  (new slider% 
       [label "Filas"] 
       [min-value 4] ;This just sets the minimum of the slider
       [max-value 10] ;This just sets the maximum of the slider
       [parent control-panel]
       [init-value rows] 
       [callback (lambda (s e) 
                   (set! rows (send s get-value)); This is the function that gets and sets the values for rows without this the game doesnt recieve the amount of rows
                   )]
       ))
  

(define col-slider ;This slider controls how many columns are gonna be on the game board
  (new slider% 
       [label "Columnas"] 
       [min-value 4] ;This just sets the minimum of the slider
       [max-value 10] ;This just sets the maximum of the slider
       [parent control-panel] 
       [init-value columns]
       [callback (lambda (s e) 
                   (set! columns (send s get-value)); This is the function that gets and sets the values for columns without this the game doenst receive the amount of columns
                   )]))


(define win-condition;This is just the text field where the Score is setted up
  (new text-field%
       [label "Puntaje objetivo"]
       [parent control-panel]
       [init-value "2048"]
       ))



(define establish-button; This button is important because it sets rows, columns and win condition and starts the game every time it gets pressed
  (new button%
       [label "Establecer e Iniciar"]
       [parent control-panel]
       [callback (lambda (s e)
                   (let ([val (string->number (send win-condition get-value))]);This part set the win condition
                     (if val
                         (set! winCondition val)
                         (set! winCondition 2048)))
                   (set! GameMatrix (GenerateMatrix rows 0 columns rows '()));This part sets and resets the game board
                   (set! GameMatrix (InsertRandomNumber GameMatrix));This part starts the game placing the first number
                   (set! score 0)
                   (send score-disp set-label
                         (string-append "Puntaje: "(number->string score)))
                   (send canvas refresh))]));This sends all the prior data to the canvas so it can be displayed


(define score-panel ;This is the space where the sliders, text field and buttons are gonna be displayed.
  (new horizontal-panel% 
       [parent frame] 
       [alignment '(center center)] 
       [stretchable-height #f]
       ))

(define score-disp
  (new message%
       [parent score-panel]
       [label "Puntaje: 0"]
       [min-width 70]))

; --- Drawing space  ---
(define grid-canvas% ;Defines the type of canvas
  (class canvas%     ;Defines the canvas class 
    (inherit get-dc get-width get-height) ;This inherits predefined constructs like drawing context and the dimensions of the already created window
    (define/override (on-paint);this define/override on paint let us change specific things about the standard settings of the canvas
;From ln 82 through 84 are just normal variables that need to be initialized for the canvas
      (define dc (get-dc)) 
      (define w (get-width))
      (define h (get-height))
;Line 86 and 87 gets us the partition of the size each tile is going to have
      (define cell-w(/ w columns))
      (define cell-h(/ h rows))

;This function is going to draw all the tiles 
      (for([row GameMatrix][r (in-naturals)])
        (for([value row] [c (in-naturals)])
          (let ([x-pos (* c cell-w)] ;This part is gonna set up which position is gonna have each column of tiles
                [y-pos (* r cell-h)]) ;This part is gonna set up which position is gonna have each row of tiles
            
            (send dc set-brush (tileColor value) 'solid); This set the tile colors based if they are empty or not
            (send dc set-pen "black" 1 'solid) ;This makes the lines between the tiles
            (send dc draw-rectangle x-pos y-pos cell-w cell-h) ;This draws tile
            
            

            (when (not (= value 0))
              (send dc set-text-foreground "black") ;This makes the number readable
              (send dc draw-text (number->string value) (+ x-pos 15) (+ y-pos 15)))))) ; this draws the number within the tile

      
      )
    (define/override (on-char event);This define/override on paint let us change specific things about the standard settings of the canvas events
      (define key (send event get-key-code)) ;This is gonna be hearing each pressed key
      (define old-matrix GameMatrix) ;This is gonna be used to compare the matrix before and after a movement is made

      ;From line 112 through line 117 is just what happens if any of the arrows is being pressed, and if there is like a different key pressed is gonna ignore it
      (cond 
        [(equal? key 'up)
         (set! score (CalcScoreUpMtx GameMatrix score))
         (set! GameMatrix (MoveUpMatrix GameMatrix))]
        [(equal? key 'down)
         (set! score (CalcScoreDownMtx GameMatrix score))
         (set! GameMatrix (MoveDownMatrix GameMatrix))]
        [(equal? key 'right)
         (set! score (CalcScoreRightMtx GameMatrix score))
         (set! GameMatrix (MoveRightMatrix GameMatrix))]
        [(equal? key 'left)
         (set! score (CalcScoreLeftMtx GameMatrix score))

         (set! GameMatrix (MoveLeftMatrix GameMatrix))]
        [else (void)]) ;This is what ignores if there was a different key being pressed
      
      (when (not (equal? old-matrix GameMatrix)) ;This compares if the matrix is the same it does nothing, and if it changes adds a a random number
        (set! GameMatrix (InsertRandomNumber GameMatrix))
        (send this refresh)
        (send score-disp set-label
              (string-append "Puntaje: "(number->string score)))
        (cond ;This compares on each move if the win condition is being met or if there are no more posible movements 
          [(WinConditionMet? GameMatrix winCondition)
           (message-box "Ganaste" "llegaste al puntaje deseado" frame '(ok))]

          [(GameOverConditionMet? GameMatrix)
           (message-box "Perdiste" "Ya no hay más movimientos posibles" frame  '(ok stop))])
        )

      )
    

    (super-new)));This is neccessary for the correct deployment of the canvas 
;From line 138 through 140 are just necessary things for the canvas 
(define canvas (new grid-canvas% 
                    [parent frame]
                    [style '(border no-autoclear)]));This helps avoid the flicker effect

(send frame show #t);This makes the window visible
(send canvas focus); This makes the canvas focus on the events which in this case are the key pressings