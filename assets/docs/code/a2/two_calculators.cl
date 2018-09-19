; Author: Dor Rondel
; Course: CSc 416

; Problem: LR/RL Infix Calculator

(defun calculator-LR ()
    (princ "Expression? ")
    (setf input (list (read) (read) (read) (read) (read)))
    (setf operator-a (second input))
    (setf operator-b (nth 3 input))
    (setf frst (first input))
    (setf scnd (third input))
    (setf thrd (nth 4 input))
    (setf result (apply operator-b 
                    (list 
                        (apply operator-a (list frst scnd)) 
                        thrd)))
    (prin1 result)
    (terpri)
    (calculator-LR)
)

(defun calculator-RL ()
    (princ "Expression? ")
    (setf input (list (read) (read) (read) (read) (read)))
    (setf operator-a (nth 3 input))
    (setf operator-b (second input))
    (setf frst (nth 4 input))
    (setf scnd (third input))
    (setf thrd (first input))
    (setf result (funcall operator-b 
                        (funcall operator-a frst scnd) 
                        thrd))
    (prin1 result)
    (terpri)
    (calculator-RL)
)