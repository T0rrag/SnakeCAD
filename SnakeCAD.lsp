;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuous Snake Game for AutoCAD 2024 in AutoLISP
;;
;; In this version the snake moves continuously (every 100 ms) and
;; polls for keyboard input. Use:
;;    W, A, S, D - to change direction
;;    Q         - to quit
;;
;; Load this file via APPLOAD and then run the command SnakeCAD.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)  ; ensure Visual LISP COM functions are available

;;; Helper Functions

(defun butlast (lst)
  "Return all but the last element of LST."
  (if lst
    (reverse (cdr (reverse lst)))
    nil
  )
)

;;; Pseudo-Random Number Generator (Linear Congruential Generator)

(defun my-random (max)
  "Return a pseudo-random integer from 0 to max-1."
  (setq *seed* (rem (+ (* 1103515245 *seed*) 12345) 2147483648))
  (rem *seed* max)
)

(defun random-int (min max)
  "Return a pseudo-random integer between MIN and MAX (inclusive)."
  (+ min (my-random (1+ (- max min)))))

;;; Drawing Functions

(defun clear-game-drawing ()
  "Delete all game entities drawn on the SNAKE layer."
  (foreach e *game-ents*
    (if (and e (entget e))
      (entdel e)
    )
  )
  (setq *game-ents* nil)
)

(defun draw-cell (x y color)
  "Draw a filled square (cell) at grid coordinate (X,Y) with the given COLOR."
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 "SNAKE")   ; layer name "SNAKE"
      (cons 62 color)    ; color code (3=green, 1=red, 7=white)
      (cons 100 "AcDbPolyline")
      (cons 90 4)        ; number of vertices
      (cons 70 1)        ; closed polyline flag
      (cons 10 (list x y))
      (cons 10 (list (+ x 1) y))
      (cons 10 (list (+ x 1) (+ y 1)))
      (cons 10 (list x (+ y 1)))
    )
  )
  (setq *game-ents* (cons (entlast) *game-ents*))
)

(defun draw-border ()
  "Draw the border of the game area."
  (entmake
    (list
      (cons 0 "LWPOLYLINE")
      (cons 100 "AcDbEntity")
      (cons 8 "SNAKE")
      (cons 62 7)  ; white/default color
      (cons 100 "AcDbPolyline")
      (cons 90 4)
      (cons 70 1)
      (cons 10 (list 0 0))
      (cons 10 (list *grid-width* 0))
      (cons 10 (list *grid-width* *grid-height*))
      (cons 10 (list 0 *grid-height*))
    )
  )
  (setq *game-ents* (cons (entlast) *game-ents*))
)

(defun draw-game ()
  "Erase the old frame and redraw the game state."
  (clear-game-drawing)
  (draw-border)
  ;; Draw each snake segment (green = color 3)
  (foreach seg *snake*
    (draw-cell (car seg) (cadr seg) 3)
  )
  ;; Draw the food (red = color 1)
  (draw-cell (car *food*) (cadr *food*) 1)
  (princ (strcat "\nScore: " (itoa *score*)))
  (princ)
)

;;; Game Mechanics Functions

(defun update-snake ()
  "Calculate the new head position and add it to the snake."
  (setq head (car *snake*))
  (setq newHead (list (+ (car head) (car *dir*)) 
                      (+ (cadr head) (cadr *dir*))))
  (setq *snake* (cons newHead *snake*))
)

(defun check-collision (pt)
  "Return T if point PT is outside the grid or collides with the snake’s body."
  (or
    (< (car pt) 0)
    (>= (car pt) *grid-width*)
    (< (cadr pt) 0)
    (>= (cadr pt) *grid-height*)
    (member pt (cdr *snake*))  ; collides with itself
  )
)

(defun update-dir (input)
  "Update the snake’s direction based on INPUT.
Prevents a 180-degree reversal."
  (setq input (strcase input))
  (cond
    ((equal input "W") (if (not (equal *dir* '(0 -1))) (setq *dir* '(0 1))))
    ((equal input "S") (if (not (equal *dir* '(0 1)))  (setq *dir* '(0 -1))))
    ((equal input "A") (if (not (equal *dir* '(1 0)))  (setq *dir* '(-1 0))))
    ((equal input "D") (if (not (equal *dir* '(-1 0))) (setq *dir* '(1 0))))
  )
)

(defun place-food ()
  "Place food at a random grid cell not occupied by the snake."
  (setq valid nil)
  (while (not valid)
    (setq fx (random-int 0 (- *grid-width* 1)))
    (setq fy (random-int 0 (- *grid-height* 1)))
    (setq candidate (list fx fy))
    (if (member candidate *snake*)
      (setq valid nil)
      (setq valid T)
    )
  )
  (setq *food* candidate)
)

(defun exit-game ()
  "Clean up game drawings and print an exit message."
  (clear-game-drawing)
  (princ "\nExiting Snake Game.")
)

;;; Continuous Game Loop

(defun game-loop ()
  "Run the continuous game loop."
  (setq *game-running* T)
  (while *game-running*
    (update-snake)
    (if (check-collision (car *snake*))
      (progn
        (draw-game)
        (princ (strcat "\nGame Over! Final Score: " (itoa *score*)))
        (setq *game-running* nil)
      )
      (progn
        (if (equal (car *snake*) *food*)
          (progn
            (setq *score* (+ *score* 1))
            (place-food))
          (setq *snake* (butlast *snake*))
        )
        (draw-game)
        ;; Poll for keyboard input (non-blocking)
        (setq input (grread T 0.0))
        (if input
          (progn
            (setq key (strcase (car input)))
            (cond
              ((equal key "W") (if (not (equal *dir* '(0 -1))) (setq *dir* '(0 1))))
              ((equal key "S") (if (not (equal *dir* '(0 1))) (setq *dir* '(0 -1))))
              ((equal key "A") (if (not (equal *dir* '(1 0))) (setq *dir* '(-1 0))))
              ((equal key "D") (if (not (equal *dir* '(-1 0))) (setq *dir* '(1 0))))
              ((equal key "Q") (progn 
                                 (setq *game-running* nil)
                                 (princ "\nGame Quit.")))
            )
          )
        )
        (vlax-sleep 100)  ; pause 100 milliseconds before next move
      )
    )
  )
  (exit-game)
  (princ)
)

;;; Initialization and Command Definition

(defun init-game ()
  "Initialize all game variables."
  (setq *grid-width*  20)    ; grid width (cells)
  (setq *grid-height* 20)    ; grid height (cells)
  (setq *snake* (list (list 10 10) (list 9 10) (list 8 10)))  ; starting snake
  (setq *dir* '(1 0))        ; initial direction: right
  (setq *seed* (fix (* (getvar "DATE") 1000)))  ; seed for random numbers
  (setq *game-ents* nil)
  (setq *score* 0)
  (place-food)
)

(defun c:SnakeCAD ()
  "Start the continuous Snake Game."
  (init-game)
  (game-loop)
  (princ)
)
