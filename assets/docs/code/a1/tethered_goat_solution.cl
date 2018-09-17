; Author: Dor Rondel
; Course: CSc 416

; Problem: Tethered Goat

; Supplied Information
(setf side-a 100)
(setf side-b 70)
(setf radius 107)

; Deducted values
(setf barn-area (* side-a side-b))
(setf rope-area (* pi (expt radius 2))) ; pi*r^2

; Final area the goat can physically graze
(setf goat-area (- rope-area barn-area))