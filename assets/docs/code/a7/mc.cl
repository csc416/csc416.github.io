; Author: Dor Rondel
; Course: CSc 416
;
; Problem: Programming Challenge: M&C State Space Problem Solver

(load "mc_provided")

(setf *trace-search* t)

; Outputs formatted string state of bank
(defmethod display ((b bank))
    (format t "MISSIONARIES: ~A  CANNIBALS: ~A  BOAT: ~A" 
        (bank-missionaries b) 
        (bank-cannibals b) 
        (bank-boat b)
    )
)

; Returns copy of instance of a bank
(defmethod copy-bank ((b bank))
    (make-instance 'bank
        :missionaries (bank-missionaries b)
        :cannibals (bank-cannibals b)
        :boat (bank-boat b)
    )
)

; T if banks equal NIL otherwise
(defmethod equal-bank-p ((b1 bank) (b2 bank))
    (and
        (equal (bank-missionaries b1) (bank-missionaries b2))
        (equal (bank-cannibals b1) (bank-cannibals b2))
        (equal (bank-boat b1) (bank-boat b2))
    )
)

; T if bank empty NIL otherwise
(defmethod bank-empty-p ((b bank))
    (and
        (null (bank-missionaries b))
        (null (bank-cannibals b))
        (null (bank-boat b))
    )
)

; Returns amount of missionaries in given bank
(defmethod m-count ((b bank))
    (length (bank-missionaries b))
)

; Returns amount of cannibals in given bank
(defmethod c-count ((b bank))
    (length (bank-cannibals b))
)

; T if bank contains boat NIL otherwise
(defmethod contains-boat-p ((b bank))
    (not (null (bank-boat b)))
)

; Returns copy of state parameter
(defmethod copy-state ((s state))
    (make-instance 'state 
        :left-bank (copy-bank (state-left-bank s))
        :right-bank (copy-bank (state-right-bank s))
    )
)

; T if states identical NIL otherwise
(defmethod equal-state-p ((s1 state) (s2 state))
    (and
        (equal-bank-p (state-left-bank s1) (state-left-bank s2))
        (equal-bank-p (state-right-bank s1) (state-right-bank s2))
    )
)

; T if goal state reached NIL otherwise
(defmethod goalp ((s state))
    (and
        (equal-bank-p (state-left-bank s) (make-instance 'bank :missionaries '() :cannibals '() :boat nil))
        (equal-bank-p (state-right-bank s) (make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b))
    )
)

; T if feast state reached NIL otherwise
(defmethod feast-state-p ((s state))
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

; T if root node reached NIL otherwise
(defmethod rootp ((n node))
    (equal (node-name n) "root")
)

; T if node previously traversed NIL otherwise
(defmethod exploredp ((n node))
    (member-node-p n *explored*)
)

; T if node member of list parameter NIL otherwise
(defmethod member-node-p ((n node) (l list)) 
    (cond 
        ((null l) nil)
        ((equal-node-p n (car l)) t)
        (t
            (member-node-p n (cdr l))
        )
    )
)

; T if nodes identical NIL otherwise
(defmethod equal-node-p ((n1 node) (n2 node))
    (equal-state-p (node-state n1) (node-state n2))
)

; Outputs name of operator applied
(defmethod display ((op operator))
   (princ (operator-name op))
)

; binds global variables to the 10 different state space operator instances
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

; Output current node
(defmethod display-e-node ((n node))
    (princ "======CURRENT NODE======")
    (terpri)
    (display n)
    nil
)

; Output traversed nodes
(defmethod display-explored-nodes ()
    (terpri)
    (princ "======EXPLORED======")
    (terpri)
    (mapcar #'display *explored*)
    (terpri)
    nil
)

; Output non-traversed nodes
(defmethod display-unexplored-nodes ()
    (terpri)
    (princ "======UNEXPLORED======")
    (terpri)
    (mapcar #'display *unexplored*)
    (terpri)
    nil
)

; T if state space operator permitted in given state NIL otherwise
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

; T if can move 2 missionaries right else NIL
(defmethod applicable-move-2m-r ((s state))
    (and
        (> (m-count (state-left-bank s)) 1) 
        (contains-boat-p (state-left-bank s))
    )
)

; T if can move 2 missionaries left else NIL
(defmethod applicable-move-2m-l ((s state))
    (and
        (> (m-count (state-right-bank s)) 1) 
        (contains-boat-p (state-right-bank s))
    )
)

; T if can move 2 cannibals right else NIL
(defmethod applicable-move-2c-r ((s state))
    (and  
        (> (c-count (state-left-bank s)) 1) 
        (contains-boat-p (state-left-bank s))
    )
)

; T if can move 2 cannibals left else NIL
(defmethod applicable-move-2c-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 1) 
        (contains-boat-p (state-right-bank s))
    )
)

; T if you can move one missionary and one cannibal right NIL otherwise
(defmethod applicable-move-1mc-r ((s state))
    (> (c-count (state-left-bank s)) 0) 
    (> (m-count (state-left-bank s)) 0) 
    (contains-boat-p (state-left-bank s))
)

; T if you can move one missionary and one cannibal left NIL otherwise
(defmethod applicable-move-1mc-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 0) 
        (> (m-count (state-right-bank s)) 0) 
        (contains-boat-p (state-right-bank s))
    )
)

; T if you can move one missionary right NIL otherwise
(defmethod applicable-move-1m-r ((s state))
    (and
        ; have m to move
        (> (m-count (state-left-bank s)) 0) 
        (contains-boat-p (state-left-bank s))
    )
)

; T if you can move one missionary left NIL otherwise
(defmethod applicable-move-1m-l ((s state))
    (and
        (> (m-count (state-right-bank s)) 0) 
        (contains-boat-p (state-right-bank s))
    )
)

; T if you can move one cannibal right NIL otherwise
(defmethod applicable-move-1c-r ((s state))
    (and
        (> (c-count (state-left-bank s)) 0) 
        (contains-boat-p (state-left-bank s))
    )
)

; T if you can move one cannibal left NIL otherwise
(defmethod applicable-move-1c-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 0) 
        (contains-boat-p (state-right-bank s))
    )
)

; Returns list of children nodes for a given node based off applicable operators
; to that node's state
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

; Finds children nodes for valid node states
(defmethod children-of ((e-node node) &aux kids)
    (when (not (feast-state-p (node-state e-node)))
        (setf kids (find-applicable-nodes e-node))
    )
    kids
)

; Actually applies operator to application's internal state
(defmethod apply-operator ((o operator) (c state))
    (setf bank-left (state-left-bank c))
    (setf bank-right (state-right-bank c))
    (cond
        ((eq (operator-name o) 'move-2m-r)
            (setf m-group-one (bank-missionaries bank-left)) 
            (pop m-group-one) (pop m-group-one)
            (setf (bank-missionaries bank-left) m-group-one)

            (setf m-group-two (bank-missionaries bank-right)) 
            (push 'm m-group-two) (push 'm m-group-two)
            (setf (bank-missionaries bank-right) m-group-two)
            
            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-2m-l)
            (setf m-group-one (bank-missionaries bank-right)) 
            (pop m-group-one) (pop m-group-one)
            (setf (bank-missionaries bank-right) m-group-one)

            (setf m-group-two (bank-missionaries bank-left)) 
            (push 'm m-group-two) (push 'm m-group-two)
            (setf (bank-missionaries bank-left) m-group-two)
            
            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-2c-r)
            (setf c-group-one (bank-cannibals bank-left)) 
            (pop c-group-one) (pop c-group-one)
            (setf (bank-cannibals bank-left) c-group-one)

            (setf c-group-two (bank-cannibals bank-right)) 
            (push 'c c-group-two) (push 'c c-group-two)
            (setf (bank-cannibals bank-right) c-group-two)
            
            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-2c-l)
            (setf c-group-one (bank-cannibals bank-right)) 
            (pop c-group-one) (pop c-group-one)
            (setf (bank-cannibals bank-right) c-group-one)

            (setf c-group-two (bank-cannibals bank-left)) 
            (push 'c c-group-two) (push 'c c-group-two)
            (setf (bank-cannibals bank-left) c-group-two)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-1mc-r)
            (setf m-group-one (bank-missionaries bank-left)) 
            (pop m-group-one) 
            (setf (bank-missionaries bank-left) m-group-one)
            (setf m-group-two (bank-missionaries bank-right)) 
            (push 'm m-group-two) 
            (setf (bank-missionaries bank-right) m-group-two)

            (setf c-group-one (bank-cannibals bank-left)) 
            (pop c-group-one) 
            (setf (bank-cannibals bank-left) c-group-one)
            (setf c-group-two (bank-cannibals bank-right)) 
            (push 'c c-group-two) 
            (setf (bank-cannibals bank-right) c-group-two)

            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-1mc-l)
            (setf m-group-one (bank-missionaries bank-right)) 
            (pop m-group-one) 
            (setf (bank-missionaries bank-right) m-group-one)
            (setf m-group-two (bank-missionaries bank-left)) 
            (push 'm m-group-two) 
            (setf (bank-missionaries bank-left) m-group-two)

            (setf c-group-one (bank-cannibals bank-right)) 
            (pop c-group-one) 
            (setf (bank-cannibals bank-right) c-group-one)
            (setf c-group-two (bank-cannibals bank-left)) 
            (push 'c c-group-two)
            (setf (bank-cannibals bank-left) c-group-two)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-1m-r)
            (setf m-group-one (bank-missionaries bank-left)) 
            (pop m-group-one) 
            (setf (bank-missionaries bank-left) m-group-one)

            (setf m-group-two (bank-missionaries bank-right)) 
            (push 'm m-group-two) 
            (setf (bank-missionaries bank-right) m-group-two)

            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-1m-l)
            (setf m-group-one (bank-missionaries bank-right)) 
            (pop m-group-one) 
            (setf (bank-missionaries bank-right) m-group-one)

            (setf m-group-two (bank-missionaries bank-left)) 
            (push 'm m-group-two) 
            (setf (bank-missionaries bank-left) m-group-two)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
        ((eq (operator-name o) 'move-1c-r)
            (setf c-group-one (bank-cannibals bank-left)) 
            (pop c-group-one) 
            (setf (bank-cannibals bank-left) c-group-one)

            (setf c-group-two (bank-cannibals bank-right)) 
            (push 'c c-group-two) 
            (setf (bank-cannibals bank-right) c-group-two)

            (setf (bank-boat bank-left) nil)
            (setf (bank-boat bank-right) 'b)
        )
        ((eq (operator-name o) 'move-1c-l)
            (setf c-group-one (bank-cannibals bank-right)) 
            (pop c-group-one) 
            (setf (bank-cannibals bank-right) c-group-one)

            (setf c-group-two (bank-cannibals bank-left)) 
            (push 'c c-group-two)
            (setf (bank-cannibals bank-left) c-group-two)

            (setf (bank-boat bank-right) nil)
            (setf (bank-boat bank-left) 'b)
        )
    )
    (setf (state-left-bank c) bank-left)
    (setf (state-right-bank c) bank-right)
    nil
)

; Outputs solution to problem
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