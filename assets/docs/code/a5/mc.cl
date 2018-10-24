; Author: Dor Rondel, Prof. Craig Graci
; Course: CSc 416

; Problem: Programming Challenge: Interactive Missionaries and Cannibals Problem Solving Framework

; Code Provided below until noted otherwise
( defun mc ()
    ( establish-world )
    ( init-move-list )
    ( make-moves )
)

( defun make-moves ()
    ( display-world )
    ( cond 
        (( goalp )
            ( write-line "good work!" )
            nil
        )
        (( feast-state-p )
            ( write-line "Yummy yummy yummy, I got Good in mt tummy!!")
            nil
        )
        ( t 
            ( let (( m (read)))
                ( if ( applicable-p m )
                    ( let () ( perform-move m ) ( make-moves ))
                    ( let () ( write-line "move inapplicable" ) nil )
                )
            )
        )
    )
)

( defun perform-move (move)
    ( setf *move-list* ( snoc move *move-list* ))
    ( if ( equal ( current-bank ) *left-bank*)
        ( move-lr move )
        ( move-rl move )
    )
)

( defun move-lr ( ml ) 
    ( if ( null ml ) ( return-from move-lr ))
    ( move-lr-1 ( first ml ))
    ( move-lr ( rest ml ))
)

( defun move-rl ( ml ) 
    ( if ( null ml ) ( return-from move-rl ))
    ( move-rl-1 ( first ml ))
    ( move-rl ( rest ml ))
)


; Code written par moi

(defun snoc (item lst)
  (cond
    ((null lst) (list item))
    (t (cons (car lst) (snoc item (cdr lst)))))
)

(defun establish-world () 
    (setf *left-bank* '(M M M C C C B))
    (setf *right-bank* '())
)

(defun init-move-list ()
    (setf *move-list* '())
)

(defun display-world () 
    (princ "*left-bank*       ")
    (princ *left-bank*)
    (terpri)
    (princ "*right-bank*       ")
    (princ *right-bank*)
    (terpri)
)

(defun goalp () 
    (and 
        (eq (length *right-bank*) 7)
        (eq (count 'm *right-bank*) 3)
        (eq (count 'c *right-bank*) 3)
        (eq (count 'b *right-bank*) 1)
    )
)

(defun feast-state-p ()
  (or
    (and
      (>= (count 'm *left-bank*) 1)
      (> (count 'c *left-bank*) (count 'm *left-bank*))
    )
    (and
      (>= (count 'm *right-bank*) 1)
      (> (count 'c *right-bank*) (count 'm *right-bank*))
    )
  )
)

(defun applicable-p (m)
  (and
    (member 'b m)
    (eq (count 'b m) 1)
    (if (equal (current-bank) *left-bank*)
      (and
        (<= (count 'm m) (count 'm *left-bank*))
        (<= (count 'c m) (count 'c *left-bank*))
      )
      (and
        (<= (count 'm m) (count 'm *right-bank*))
        (<= (count 'c m) (count 'c *right-bank*))
      )
    )
  )
)

(defun current-bank ()
  (if (member 'b *left-bank*)
    *left-bank*
    *right-bank*
  )
)

(defun move-lr-1 (m)
	(setf *right-bank* (cons m *right-bank*))
	(setf *left-bank* (remove m *left-bank* :count 1))
)

(defun move-rl-1 (m)
	(setf *left-bank* (cons m *left-bank*))
	(setf *right-bank* (remove m *right-bank* :count 1))
)

(defun display-solution() 
	(print "(C C B)")
	(print "(C B)")
	(print "(C C B)")
	(print "(C B)")
	(print "(M M B)")
	(print "(C M B)")
	(print "(M M B)")
	(print "(C B)")
	(print "(C C B)")
	(print "(C B)")
	(print "(C C B)")
)