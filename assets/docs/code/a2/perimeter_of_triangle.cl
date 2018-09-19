; Author: Dor Rondel
; Course: CSc 416

; Problem: Perimeter of Triangle

(defun distance (x1 y1 x2 y2) 
    (setf final-distance (sqrt 
                            (+
                                (- x2 x1)
                                (- y2 y1))))
    final-distance
)

(defun perimeter (x1 y1 x2 y2 x3 y3) 
    (setf side-a (distance x1 y1 x2 y2))
    (setf side-b (distance x2 y2 x3 y3))
    (setf side-c (distance x1 y1 x3 y3))
    (princ (+ side-a side-b side-c))
)