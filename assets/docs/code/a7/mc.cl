; Author: Dor Rondel
; Course: CSc 416
;
; Problem: Programming Challenge: M&C State Space Problem Solver

(load "mc_provided")

(setf *trace-search* t)

(defmethod display ((b bank))
    (format t "MISSIONARIES: ~A  CANNIBALS: ~A  BOAT: ~A" 
        (bank-missionaries b) 
        (bank-cannibals b) 
        (bank-boat b)
    )
)

(defmethod copy-bank ((b bank))
    (make-instance 'bank
        :missionaries (bank-missionaries b)
        :cannibals (bank-cannibals b)
        :boat (bank-boat b)
    )
)

(defmethod equal-bank-p ((b1 bank) (b2 bank))
    (and
        (equal (bank-missionaries b1) (bank-missionaries b2))
        (equal (bank-cannibals b1) (bank-cannibals b2))
        (equal (bank-boat b1) (bank-boat b2))
    )
)

(defmethod bank-empty-p ((b bank))
    (and
        (null (bank-missionaries b))
        (null (bank-cannibals b))
        (null (bank-boat b))
    )
)

(defmethod m-count ((b bank))
    (length (bank-missionaries b))
)

(defmethod c-count ((b bank))
    (length (bank-cannibals b))
)

(defmethod contains-boat-p ((b bank))
    (not (null (bank-boat b)))
)

(defmethod toggle-boat ((s state))
    (cond 
        ((contains-boat-p (state-left-bank s))
            (setf (bank-boat (state-left-bank s)) nil)
            (setf (bank-boat (state-right-bank s)) 'b)
        )
        ((contains-boat-p (state-right-bank s))
            (setf (bank-boat (state-right-bank s)) nil)
            (setf (bank-boat (state-left-bank s)) 'b)
        )
    )
)

(defmethod copy-state ((s state))
    (make-instance 'state 
        :left-bank (copy-bank (state-left-bank s))
        :right-bank (copy-bank (state-right-bank s))
    )
)

(defmethod equal-state-p ((s1 state)(s2 state))
    (and
        (equal-bank-p (state-left-bank s1) (state-left-bank s2))
        (equal-bank-p (state-right-bank s1) (state-right-bank s2))
    )
)


(defmethod goalp ((s state))
    (and
        (equal-bank-p (state-left-bank s) (make-instance 'bank :missionaries '() :cannibals '() :boat nil))
        (equal-bank-p (state-right-bank s) (make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b))
    )
)

(defmethod feast-state-p ((s state) &aux lb rb)
    (setf lb (state-left-bank s))
    (setf rb (state-right-bank s))
    (or
        (and
            (> (length (bank-cannibals lb)) (length (bank-missionaries lb)))
            (> (length (bank-missionaries lb)) 0)		
        )
        (and
            (> (length (bank-cannibals rb)) (length (bank-missionaries rb)))
            (> (length (bank-missionaries rb)) 0)
        )
    )
)

(defmethod rootp ((n node))
    (equal (node-name n) "root")
)

(defmethod display ((op operator))
   (princ (operator-name op))
)

(defmethod exploredp ((n node))
    (member-node-p n *explored*)
)

(defmethod member-node-p ((n node) (l list)) 
    (cond 
        ((null l) nil)
        ((equal-node-p n (car l)) t)
        (t
            (member-node-p n (cdr l))
        )
    )
)

(defmethod equal-node-p ((n1 node) (n2 node))
    (equal-state-p (node-state n1) (node-state n2))
)

(defmethod establish-operators ()
    (setf *move-2m-r*
        (make-instance 'operator
            :name 'move-2m-r
            :precondition "Moving two missionaries right doesn't kill any missionaries"
            :description "Moves two missionaries to the right bank"
        )
    )
    (setf *move-2m-l*
        (make-instance 'operator
            :name 'move-2m-l
            :precondition "Moving two missionaries left doesn't kill any missionaries"
            :description "Moves two missionaries to the left bank"
        )
    )
    (setf *move-2c-r*
        (make-instance 'operator
            :name 'move-2c-r
            :precondition "Moving two cannibals right doesn't kill any missionaries"
            :description "Moves two cannibals to the right bank"
        )
    )
    (setf *move-2c-l*
        (make-instance 'operator
            :name 'move-2c-l
            :precondition "Right bank has (C C B)"
            :description "Move (C C B) from right bank to left bank"
        )
    )
    (setf *move-1mc-r*
        (make-instance 'operator
            :name 'move-1mc-r
            :precondition "Moving single missionary and cannibal right doesn't kill any missionaries"
            :description "Moves single missionary and cannibal to the right bank"
        )
    )
    (setf *move-1mc-l*
        (make-instance 'operator
            :name 'move-1mc-l
            :precondition "Moving single missionary and cannibal left doesn't kill any missionaries"
            :description "Moves single missionary and cannibal to the left bank"
        )
    )
    (setf *move-1m-r*
        (make-instance 'operator
            :name 'move-1m-r
            :precondition "Moving single missionary right doesn't kill any missionaries"
            :description "Moves single missionary to the right bank"
        )
    )
    (setf *move-1m-l*
    (make-instance 'operator
        :name 'move-1m-l
        :precondition "Moving single missionary left doesn't kill any missionaries"
        :description "Moves single missionary to the left bank"
    )
    )
    (setf *move-1c-r*
        (make-instance 'operator
            :name 'move-1c-r
            :precondition "Moving single cannibal right doesn't kill any missionaries"
            :description "Moves single cannibal to the right bank"
        )
    )
    (setf *move-1c-l*
        (make-instance 'operator
            :name 'move-1c-l
            :precondition "Moving single cannibal left doesn't kill any missionaries"
            :description "Moves single cannibal to the left bank"
        )
    )
    (setf *operator-list* 
        (list 
            *move-1m-l*
            *move-2m-l*
            *move-1m-r*
            *move-2m-r*
            *move-1c-l*
            *move-2c-l*
            *move-1c-r*
            *move-2c-r*
            *move-1mc-l*
            *move-1mc-r*
        )
    )
    nil
)

(defmethod display-e-node ((n node))
    (princ "======CURRENT NODE======")
    (terpri)
    (display n)
    nil
)


(defmethod display-explored-nodes ()
    (terpri)
    (princ "======EXPLORED======")
    (terpri)
    (mapcar #'display *explored*)
    (terpri)
    nil
)

(defmethod display-unexplored-nodes ()
    (terpri)
    (princ "======UNEXPLORED======")
    (terpri)
    (mapcar #'display *unexplored*)
    (terpri)
    nil
)

(defmethod applicablep ((op operator) (s state))
    (cond
        ((eq (operator-name op) 'move-2m-r)
            (applicable-move-2m-r s)
        )
        ((eq (operator-name op) 'move-2m-l) 
            (applicable-move-2m-l s)
        )
        ((eq (operator-name op) 'move-2c-r)
            (applicable-move-2c-r s)
        )
        ((eq (operator-name op) 'move-2c-l)
            (applicable-move-2c-l s)
        )
        ((eq (operator-name op) 'move-1mc-r)
            (applicable-move-1mc-r s)
        )
        ((eq (operator-name op) 'move-1mc-l)
            (applicable-move-1mc-l s)
        )
        ((eq (operator-name op) 'move-1m-r)
            (applicable-move-1m-r s)
        )
        ((eq (operator-name op) 'move-1m-l)
            (applicable-move-1m-l s)
        )
        ((eq (operator-name op) 'move-1c-r)
            (applicable-move-1c-r s)
        )
        ((eq (operator-name op) 'move-1c-l)
            (applicable-move-1c-l s)
        )
    )
)

(defmethod applicable-move-2m-r ((s state))
    (and
        (> (m-count (state-left-bank s)) 1) 
        (contains-boat-p (state-left-bank s))
    )
)

(defmethod applicable-move-2m-l ((s state))
    (and
        (> (m-count (state-right-bank s)) 1) 
        (contains-boat-p (state-right-bank s))
    )
)

(defmethod applicable-move-2c-r ((s state))
    (and  
        (> (c-count (state-left-bank s)) 1) 
        (contains-boat-p (state-left-bank s))
    )
)

(defmethod applicable-move-2c-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 1) 
        (contains-boat-p (state-right-bank s))
    )
)

(defmethod applicable-move-1mc-r ((s state))
    (> (c-count (state-left-bank s)) 0) 
    (> (m-count (state-left-bank s)) 0) 
    (contains-boat-p (state-left-bank s))
)

(defmethod applicable-move-1mc-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 0) 
        (> (m-count (state-right-bank s)) 0) 
        (contains-boat-p (state-right-bank s))
    )
)

(defmethod applicable-move-1m-r ((s state))
    (and
        ; have m to move
        (> (m-count (state-left-bank s)) 0) 
        (contains-boat-p (state-left-bank s))
    )
)

(defmethod applicable-move-1m-l ((s state))
    (and
        (> (m-count (state-right-bank s)) 0) 
        (contains-boat-p (state-right-bank s))
    )
)


(defmethod applicable-move-1c-r ((s state))
    (and
        (> (c-count (state-left-bank s)) 0) 
        (contains-boat-p (state-left-bank s))
    )
)

(defmethod applicable-move-1c-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 0) 
        (contains-boat-p (state-right-bank s))
    )
)

(defmethod find-applicable-nodes ((e-node node) &aux node-list)
    (if (applicablep *move-2m-r* (node-state e-node))
        (push (child-of e-node *move-2m-r*) node-list)
    )
    (if (applicablep *move-2m-l* (node-state e-node))
        (push (child-of e-node *move-2m-l*) node-list)
    )
    (if (applicablep *move-2c-r* (node-state e-node))
        (push (child-of e-node *move-2c-r*) node-list)
    )
    (if (applicablep *move-2c-l* (node-state e-node))
        (push (child-of e-node *move-2c-l*) node-list)
    )
    (if (applicablep *move-1m-r* (node-state e-node))
        (push (child-of e-node *move-1m-r*) node-list)
    )
    (if (applicablep *move-1m-l* (node-state e-node))
        (push (child-of e-node *move-1m-l*) node-list)
    )
    (if (applicablep *move-1c-r* (node-state e-node))
        (push (child-of e-node *move-1c-r*) node-list)
    )
    (if (applicablep *move-1c-l* (node-state e-node))
        (push (child-of e-node *move-1c-l*) node-list)
    )
    (if (applicablep *move-1mc-r* (node-state e-node))
        (push (child-of e-node *move-1mc-r*) node-list)
    )
    (if (applicablep *move-1mc-l* (node-state e-node))
        (push (child-of e-node *move-1mc-l*) node-list)
    )
    node-list
)


(defmethod children-of ((e-node node) &aux kids)
 (when (not (feast-state-p (node-state e-node)))
    (setf kids (find-applicable-nodes e-node))
 )
 kids
)

(defmethod apply-operator ((o operator) (c state))
    (setf bank-left (state-left-bank c))
    (setf bank-right (state-right-bank c))
    (cond
        ((eq (operator-name o) 'move-2m-r)
            (setf *m1* (bank-missionaries bank-left)) 
            (pop *m1*) (pop *m1*)
            (setf (bank-missionaries bank-left) *m1*)

            (setf *m2* (bank-missionaries bank-right)) 
            (push 'm *m2*) (push 'm *m2*)
            (setf (bank-missionaries bank-right) *m2*)
            
            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-2m-l)
            (setf *m1* (bank-missionaries bank-right)) 
            (pop *m1*) (pop *m1*)
            (setf (bank-missionaries bank-right) *m1*)

            (setf *m2* (bank-missionaries bank-left)) 
            (push 'm *m2*) (push 'm *m2*)
            (setf (bank-missionaries bank-left) *m2*)
            
            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-2c-r)
            (setf *c1* (bank-cannibals bank-left)) 
            (pop *c1*) (pop *c1*)
            (setf (bank-cannibals bank-left) *c1*)

            (setf *c2* (bank-cannibals bank-right)) 
            (push 'c *c2*) (push 'c *c2*)
            (setf (bank-cannibals bank-right) *c2*)
            
            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-2c-l)
            (setf *c1* (bank-cannibals bank-right)) 
            (pop *c1*) (pop *c1*)
            (setf (bank-cannibals bank-right) *c1*)

            (setf *c2* (bank-cannibals bank-left)) 
            (push 'c *c2*) (push 'c *c2*)
            (setf (bank-cannibals bank-left) *c2*)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-1mc-r)
            (setf *m1* (bank-missionaries bank-left)) 
            (pop *m1*) 
            (setf (bank-missionaries bank-left) *m1*)
            (setf *m2* (bank-missionaries bank-right)) 
            (push 'm *m2*) 
            (setf (bank-missionaries bank-right) *m2*)

            (setf *c1* (bank-cannibals bank-left)) 
            (pop *c1*) 
            (setf (bank-cannibals bank-left) *c1*)
            (setf *c2* (bank-cannibals bank-right)) 
            (push 'c *c2*) 
            (setf (bank-cannibals bank-right) *c2*)

            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-1mc-l)
            (setf *m1* (bank-missionaries bank-right)) 
            (pop *m1*) 
            (setf (bank-missionaries bank-right) *m1*)
            (setf *m2* (bank-missionaries bank-left)) 
            (push 'm *m2*) 
            (setf (bank-missionaries bank-left) *m2*)

            (setf *c1* (bank-cannibals bank-right)) 
            (pop *c1*) 
            (setf (bank-cannibals bank-right) *c1*)
            (setf *c2* (bank-cannibals bank-left)) 
            (push 'c *c2*)
            (setf (bank-cannibals bank-left) *c2*)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-1m-r)
            (setf *m1* (bank-missionaries bank-left)) 
            (pop *m1*) 
            (setf (bank-missionaries bank-left) *m1*)

            (setf *m2* (bank-missionaries bank-right)) 
            (push 'm *m2*) 
            (setf (bank-missionaries bank-right) *m2*)

            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-1m-l)
            (setf *m1* (bank-missionaries bank-right)) 
            (pop *m1*) 
            (setf (bank-missionaries bank-right) *m1*)

            (setf *m2* (bank-missionaries bank-left)) 
            (push 'm *m2*) 
            (setf (bank-missionaries bank-left) *m2*)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-1c-r)
            (setf *c1* (bank-cannibals bank-left)) 
            (pop *c1*) 
            (setf (bank-cannibals bank-left) *c1*)

            (setf *c2* (bank-cannibals bank-right)) 
            (push 'c *c2*) 
            (setf (bank-cannibals bank-right) *c2*)

            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-1c-l)
            (setf *c1* (bank-cannibals bank-right)) 
            (pop *c1*) 
            (setf (bank-cannibals bank-right) *c1*)

            (setf *c2* (bank-cannibals bank-left)) 
            (push 'c *c2*)
            (setf (bank-cannibals bank-left) *c2*)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
    )
    (setf (state-left-bank c) bank-left)
    (setf (state-right-bank c) bank-right)
    nil
)

(defmethod display-solution ((n node))
    (cond
        ((rootp n)
            (terpri)
        )
        (t
            (princ (operator-description (node-operator n)))
            (terpri)
            (display-solution (node-parent n))   
        ) 
    )
    nil
)