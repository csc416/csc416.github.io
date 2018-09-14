; Author: Dor Rondel
; Course: CSc-416

; Problem: Three Quarters / Far Corners

; Diameter of a quarter in inches as per wikipedia
(setf quarter-diameter 0.955)

; Deducted values
; Redundant value, just for convenience
(setf rectangle-side quarter-diameter)
(setf rectangle-height (* 3 rectangle-side))

; Rectangle's diagonal essentially is found by using the Pythagorean Theorem
(setf diagonal 
    (sqrt 
        (+
            (expt rectangle-side 2)
            (expt rectangle-height 2))))
