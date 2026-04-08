#lang racket/gui
(require "CE2048.rkt")

; --- Starting state variables ---
(define rows 4)    ; N (Rows)
(define columns 4) ; M (Columns)

(define GameMatrix (GenerateMatrix (* rows columns) 0 rows columns '() ))

(define frame 
  (new frame% 
       [label "Configurador de Cuadrícula 2048"] 
       [width 500] 
       [height 500]))

; Horizontal Pane for the controls
(define control-panel 
  (new horizontal-panel% 
       [parent frame] 
       [alignment '(center center)] 
       [stretchable-height #f]))

; --- Selection Controls ---
(define row-slider
  (new slider% 
       [label "Filas"] 
       [min-value 4] 
       [max-value 10]
       [parent control-panel]
       [init-value rows]
       [callback (lambda (s e) 
                   (set! rows (send s get-value))
                   )]
       ))
  

(define col-slider
  (new slider% 
       [label "Columnas"] 
       [min-value 4] 
       [max-value 10]
       [parent control-panel] 
       [init-value columns]
       [callback (lambda (s e) 
                   (set! columns (send s get-value))
                   )]))


(define win-condition
  (new text-field%
       [label "Puntaje objetivo"]
       [parent control-panel]
       ;[callback (lambda(s e)
                   ))

(define establish-button
  (new button%
       [label "Establecer e Iniciar"]
       [parent control-panel]
       [callback (lambda (s e)
                   (set! GameMatrix (GenerateMatrix (* rows columns) 0 rows columns '()))
                   (set! GameMatrix (InsertRandomNumber GameMatrix))
                   (send canvas refresh))]))







; --- Drawing space  ---
(define grid-canvas% ;defines the type of canvas
  (class canvas%     ;defines the canvas class 
    (inherit get-dc get-width get-height) ;this inherits predefined constructs like drawing context and the dimensions of the already created window
    (define/override (on-paint)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-height))

      (define cell-w(quotient w columns))
      (define cell-h(quotient h rows))
      

      (for([row GameMatrix][r (in-naturals)])
        (for([value row] [c (in-naturals)])
          (let ([x-pos (* c cell-w)]
                [y-pos (* r cell-h)])

           (send dc set-brush (if (= value 0) "bisque" "orange") 'solid)
           (send dc set-pen "black" 1 'solid)
           (send dc draw-rectangle x-pos y-pos cell-w cell-h)
            
            

          (when (not (= value 0))
            (send dc set-text-foreground "black")
            (send dc draw-text (number->string value) (* x-pos 10) (+ y-pos 10))))))
      )
      

    (super-new)))

(define canvas (new grid-canvas% [parent frame]))

(send frame show #t)