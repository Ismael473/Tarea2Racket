#lang racket/gui

; --- Starting state variables ---
(define rows 4)    ; N (Rows)
(define columns 4) ; M (Columns)

(define frame (new frame% [label "Configurador de Cuadrícula NxM"] [width 500] [height 500]))

; Horizontal Pane for the controls
(define control-panel (new horizontal-panel% [parent frame] [alignment '(center center)] [stretchable-height #f]))

; --- Selection Controls ---
(define row-slider
  (new slider% [label "Filas (N)"] [min-value 4] [max-value 10]
       [parent control-panel] [init-value rows]
       [callback (lambda (s e) 
                   (set! rows (send s get-value))
                   (send canvas refresh))]))

(define col-slider
  (new slider% [label "Columnas (M)"] [min-value 4] [max-value 10]
       [parent control-panel] [init-value columns]
       [callback (lambda (s e) 
                   (set! columns (send s get-value))
                   (send canvas refresh))]))

; --- Drawing space  ---
(define grid-canvas%
  (class canvas%
    (inherit get-dc get-width get-height)
    (define/override (on-paint)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-height))
      
      ;Dynamic cell resezing
      (define cell-w (/ w columns))
      (define cell-h (/ h rows))
      
      (send dc set-pen "black" 1 'solid)
      (send dc set-brush "white" 'solid)
      
      ;Row movement
      (for ([r (in-range rows)])
        (for ([c (in-range columns)])
          (send dc draw-rectangle 
                (* c cell-w) ; X position based on columns
                (* r cell-h) ; Y position based on row
                cell-w 
                cell-h))))
    (super-new)))

(define canvas (new grid-canvas% [parent frame]))

(send frame show #t)