; Author: Dor Rondel
; Course: CSc 416

; Problem: Recursive List Processing

(defun singleton-p (lst)
  (and 
    (not (null lst)) 
    (null (cdr lst)))
)


(defun rac (lst)
  (cond
    ((null lst) nil)
    ((singleton-p lst) (car lst))
    (t (rac (cdr lst))))
)


(defun rdc (lst)
  (cond
    ((or (null lst) (singleton-p lst)) nil)
    (t (cons (car lst) (rdc (cdr lst)))))
)


(defun snoc (item lst)
  (cond
    ((null lst) (list item))
    (t (cons (car lst) (snoc item (cdr lst)))))
)


(defun palindrome-p (lst)
  (cond
    ((or (null lst) (singleton-p lst)) t)
    (t (and (eq (car lst) (rac lst)) 
                   (palindrome-p (cdr (rdc lst))))))
)


(defun select (idx lst)
  (cond
    ((null lst) nil)
    ((= idx 0) (car lst))
    (t (select (- idx 1) (cdr lst))))
)


(defun pick (lst)
  (select (random (length lst)) lst)
)


(defun sum (lst)
  (cond
    ((null lst) 0)
    (t (+ (car lst) (sum (cdr lst)))))
)


(defun product (lst)
  (cond
    ((null lst) 1)
    (t (* (car lst) (product (cdr lst)))))
)


(defun iota (n)
    (cond
        ((= n 0) nil)
        (t (snoc n (iota (- n 1)))))
)


(defun duplicate (n lo)
    (cond
        ((= n 0) nil)
        (t (snoc lo (duplicate (- n 1) lo))))
)


(defun factorial (n)
  (product (iota n))
)



(defun power (n e)
  (product (duplicate n e))
)


(defun filter-in (expr lst)
    (cond
        ((null lst) ())
        ((funcall expr (car lst)) (cons (car lst) (filter-in expr (cdr lst))))
        (t (filter-in expr (cdr lst))))
)


(defun filter-out (expr lst)
    (cond
        ((null lst) ())
        ((funcall expr (car lst)) (filter-out expr (cdr lst)))
        (t (cons (car lst) (filter-out expr (cdr lst)))))
)


(defun take-from (obj lst)
  (cond
    ((null lst) nil)
    ((equal (car lst) obj) (take-from obj (cdr lst)))
    (t (cons (car lst) (take-from obj (cdr lst)))))
)


(defun random-permutation (lst)
  (cond
    ((null lst) nil)
    (t
      (setf element (pick lst))
      (setf remainder (take-from element lst))
      (cons element (random-permutation remainder))))
)