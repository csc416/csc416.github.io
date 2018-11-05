; Author: Dor Rondel
; Course: CSc 416
;
; Problem: Programming Challenge: M&C State Space Problem Solver

(load "mc_provided")

; Class methods for bank
(defmethod remove-1m ((b bank))
    (pop (bank-missionaries b))
    nil
)

(defmethod remove-2m ((b bank)) 
    (pop (bank-missionaries b))
    (pop (bank-missionaries b))
    nil
)

(defmethod add-1m ((b bank))
    ;(setf (b bank-missionaries) (append (bank-missionaries b) '(m)))
    (push 'm (bank-missionaries b))
    nil
)

(defmethod add-2m ((b bank)) 
    (push 'm (bank-missionaries b))
    (push 'm (bank-missionaries b))
    nil
)

(defmethod remove-1c ((b bank))
    (pop (bank-cannibals b))
    nil
)

(defmethod remove-2c ((b bank))
    (pop (bank-cannibals b))
    (pop (bank-cannibals b))
    nil
)

(defmethod add-1c ((b bank))
    ;(setf (b bank-cannibals) (append (bank-cannibals b) '(m)))
    (push 'm (bank-cannibals b))
    nil
)

(defmethod add-2c ((b bank))
    (push 'm (bank-cannibals b))
    (push 'm (bank-cannibals b))
    nil
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

(defmethod contains-boat ((b bank))
    (not (null (bank-boat b)))
)

(defmethod equal-bank-p ((b1 bank) (b2 bank))
    (and
        (equal (bank-missionaries b1) (bank-missionaries b2))
        (equal (bank-cannibals b1) (bank-cannibals b2))
        (equal (bank-boat b1) (bank-boat b2))
    )
)

(defmethod display ((b bank))
    (format t "MISSIONARIES: ~A  CANNIBALS: ~A  BOAT: ~A" (bank-missionaries b) (bank-cannibals b) (bank-boat b))
)

; Class methods for state
(defmethod goalp ((s state))
    (and
        (equal-bank-p (state-left-bank s) (make-instance 'bank :missionaries '() :cannibals '() :boat nil))
        (equal-bank-p (state-right-bank s) (make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b))
    )
) 

(defmethod copy-state ((s state))
    (make-instance 'state 
        :left-bank (state-left-bank s)
        :right-bank (state-right-bank s)
    )
)

(defmethod equal-state-p ((s1 state) (s2 state))
    (and
        (equal-bank-p (state-left-bank s1) (state-left-bank s2))
        (equal-bank-p (state-right-bank s1) (state-right-bank s2))
    )
)  

(defmethod rootp ((n node))
    (equal (node-name n) "root")
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

; Class methods for operators

(defmethod display ((op operator))
    (format t "Operator name: ~A" (operator-name op))
    (terpri)
    (format t "Operator name: ~A" (operator-name op))
    (terpri)
    (format t "Operator name: ~A" (operator-name op))
    (terpri)
)

(defmethod applicable-p ((op operator) (s state))
    (cond
        ((eq (operator-name op) 'move-1m-l)
            (applicable-move-1m-l s)
        )
        ((eq (operator-name op) 'move-2m-l)
            (applicable-move-2m-l s)
        )
        ((eq (operator-name op) 'move-1m-r)
            (applicable-move-1m-r s)
        )
        ((eq (operator-name op) 'move-2m-r)
            (applicable-move-2m-r s)
        )
        ((eq (operator-name op) 'move-1c-l)
            (applicable-move-1c-l s)
        )
        ((eq (operator-name op) 'move-2c-l)
            (applicable-move-2c-l s)
        )
        ((eq (operator-name op) 'move-1c-r)
            (applicable-move-1c-r s)
        )
        ((eq (operator-name op) 'move-2c-r)
            (applicable-move-2c-r s)
        )
        ((eq (operator-name op) 'move-1mc-r)
            (applicable-move-1mc-r s)
        )
        ((eq (operator-name op) 'move-1mc-l)
            (applicable-move-1mc-l s)
        )
    )
)

(defmethod applicable-move-1m-l ((s state))
    (and
        ; have m to move
        (> (m-count (state-right-bank s)) 0) 
        ; left ok after
        (>= (+ (m-count (state-left-bank s)) 1) (c-count (state-left-bank s)))
        ; right ok after
        (>= (- (m-count (state-right-bank s)) 1) (c-count (state-right-bank s)))
    )
)

(defmethod applicable-move-1m-r ((s state))
    (and
        ; have m to move
        (> (m-count (state-left-bank s)) 0) 
        (>= (- (m-count (state-left-bank s)) 1) (c-count (state-left-bank s)))
        (>= (+ (m-count (state-right-bank s)) 1) (c-count (state-right-bank s)))
    )
)

(defmethod applicable-move-2m-l ((s state))
    (and
        ; have m to move
        (> (m-count (state-right-bank s)) 1) 
        (>= (+ (m-count (state-left-bank s)) 2) (c-count (state-left-bank s)))
        (>= (- (m-count (state-right-bank s)) 2) (c-count (state-right-bank s)))
    )
)

(defmethod applicable-move-2m-r ((s state))
    (and
        ; have m to move
        (> (m-count (state-left-bank s)) 1) 
        (>= (- (m-count (state-left-bank s)) 2) (c-count (state-left-bank s)))
        (>= (+ (m-count (state-right-bank s)) 2) (c-count (state-right-bank s)))
    )
)

(defmethod applicable-move-1c-l ((s state))
    (and
        ; have c to move
        (> (c-count (state-right-bank s)) 0) 
        ; left ok after
        (<= (+ (c-count (state-left-bank s)) 1) (m-count (state-left-bank s)))
        ; right ok after
        (<= (- (c-count (state-right-bank s)) 1) (m-count (state-right-bank s)))
    )
)

(defmethod applicable-move-1c-r ((s state))
    (and
        (> (c-count (state-left-bank s)) 0) 
        (<= (- (c-count (state-left-bank s)) 1) (m-count (state-left-bank s)))
        (<= (+ (c-count (state-right-bank s)) 1) (m-count (state-right-bank s)))
    )
)

(defmethod applicable-move-2c-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 1) 
        (<= (+ (c-count (state-left-bank s)) 2) (m-count (state-left-bank s)))
        (<= (- (c-count (state-right-bank s)) 2) (m-count (state-right-bank s)))
    )
)

(defmethod applicable-move-2c-r ((s state))
    (and
        (> (c-count (state-left-bank s)) 1) 
        (<= (- (c-count (state-left-bank s)) 2) (m-count (state-left-bank s)))
        (<= (+ (c-count (state-right-bank s)) 2) (m-count (state-right-bank s)))
    )
)

(defmethod applicable-move-1mc-l ((s state))
    (and
        (> (c-count (state-right-bank s)) 0) 
        (> (m-count (state-right-bank s)) 0) 
        (<= 
            (- (c-count (state-right-bank s)) 1) 
            (- (m-count (state-right-bank s)) 1)
        )
        (<= 
            (+ (c-count (state-left-bank s)) 1) 
            (+ (m-count (state-left-bank s)) 1)
        )
    )
)

(defmethod applicable-move-1mc-r ((s state))
    (and
        (> (c-count (state-left-bank s)) 0) 
        (> (m-count (state-left-bank s)) 0) 
        (<= 
            (- (c-count (state-left-bank s)) 1) 
            (- (m-count (state-left-bank s)) 1)
        )
        (<= 
            (+ (c-count (state-right-bank s)) 1) 
            (+ (m-count (state-right-bank s)) 1)
        )
    )
)

(defmethod establish-operators ()
    (setf *move-1m-l* 
        (make-instance 'operator
            :name 'move-1m-l
            :precondition "Moving single missionary left doesn't kill any missionaries"
            :description "Moves single missionary to the left bank"
        )
    )
    (setf *move-2m-l* 
        (make-instance 'operator
            :name 'move-2m-l
            :precondition "Moving two missionaries left doesn't kill any missionaries"
            :description "Moves two missionaries to the left bank"
        )
    )
    (setf *move-1m-r* 
        (make-instance 'operator
            :name 'move-1m-r
            :precondition "Moving single missionary right doesn't kill any missionaries"
            :description "Moves single missionary to the right bank"
        )
    )
    (setf *move-2m-r* 
        (make-instance 'operator
            :name 'move-2m-r
            :precondition "Moving two missionaries right doesn't kill any missionaries"
            :description "Moves two missionaries to the right bank"
        )
    )
    (setf *move-1c-l* 
        (make-instance 'operator
            :name 'move-1c-l
            :precondition "Moving single cannibal left doesn't kill any missionaries"
            :description "Moves single cannibal to the left bank"
        )
    )
    (setf *move-2c-l* 
        (make-instance 'operator
            :name 'move-2c-l
            :precondition "Moving two cannibals left doesn't kill any missionaries"
            :description "Moves two cannibals to the left bank"
        )
    )
    (setf *move-1c-r* 
        (make-instance 'operator
            :name 'move-1c-l
            :precondition "Moving single cannibal right doesn't kill any missionaries"
            :description "Moves single cannibal to the right bank"
        )
    )
    (setf *move-2c-r* 
        (make-instance 'operator
            :name 'move-2c-l
            :precondition "Moving two cannibals right doesn't kill any missionaries"
            :description "Moves two cannibals to the right bank"
        )
    )
    (setf *move-1mc-l* 
        (make-instance 'operator
            :name 'move-1mc-l
            :precondition "Moving single missionary and cannibal left doesn't kill any missionaries"
            :description "Moves single missionary and cannibal to the left bank"
        )
    )
    (setf *move-1mc-r* 
        (make-instance 'operator
            :name 'move-1mc-r
            :precondition "Moving single missionary and cannibal right doesn't kill any missionaries"
            :description "Moves single missionary and cannibal to the right bank"
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

(defmethod children-of (( e-node node ) &aux kids)
    (if (applicable-p *move-1m-l* (node-state e-node))
        (push (child-of e-node *move-1m-l*) kids)
    )
    (if (applicable-p *move-1m-r* (node-state e-node))
        (push (child-of e-node *move-1m-r*) kids)
    )
    (if (applicable-p *move-2m-l* (node-state e-node))
        (push (child-of e-node *move-2m-l*) kids)
    )
    (if (applicable-p *move-2m-r* (node-state e-node))
        (push (child-of e-node *move-2m-r*) kids)
    )
    (if (applicable-p *move-1c-l* (node-state e-node))
        (push (child-of e-node *move-1c-l*) kids)
    )
    (if (applicable-p *move-1c-r* (node-state e-node))
        (push (child-of e-node *move-1c-r*) kids)
    )
    (if (applicable-p *move-2c-l* (node-state e-node))
        (push (child-of e-node *move-2c-l*) kids)
    )
    (if (applicable-p *move-2c-r* (node-state e-node))
        (push (child-of e-node *move-2c-r*) kids)
    )
    (if (applicable-p *move-1mc-l* (node-state e-node))
        (push (child-of e-node *move-1mc-l*) kids)
    )
    (if (applicable-p *move-1mc-r* (node-state e-node))
        (push (child-of e-node *move-1mc-r*) kids)
    )
    kids
)

; first few lines written by Prof. Graci
(defmethod child-of ( ( n node ) ( o operator ) &aux c)
    ; prof. graci's code
    (setf new-node (make-instance 'node))
    (setf ( node-name new-node ) ( next *ng* ))
    (setf ( node-parent new-node ) n)
    (setf ( node-operator new-node ) o)
    (setf c ( copy-state ( node-state n )))
    ; my code 
    (cond
        ((eq (operator-name o) 'move-1m-l)
            (remove-1m (state-right-bank c))
            (add-1m (state-left-bank c))
        )
        ((eq (operator-name o) 'move-2m-l)
            (remove-2m (state-right-bank c))
            (add-2m (state-left-bank c))
        )
        ((eq (operator-name o) 'move-1m-r)
            (remove-1m (state-left-bank c))
            (add-1m (state-right-bank c))
        )
        ((eq (operator-name o) 'move-2m-r)
            (remove-2m (state-left-bank c))
            (add-2m (state-right-bank c))
        )
        ((eq (operator-name o) 'move-1c-l)
            (remove-1c (state-right-bank c))
            (add-1c (state-left-bank c))
        )
        ((eq (operator-name o) 'move-2c-l)
            (remove-2c (state-right-bank c))
            (add-2c (state-left-bank c))
        )
        ((eq (operator-name o) 'move-1c-r)
            (remove-1c (state-left-bank c))
            (add-1c (state-right-bank c))
        )
        ((eq (operator-name o) 'move-2c-r)
            (remove-2c (state-left-bank c))
            (add-2c (state-right-bank c))
        )
        ((eq (operator-name o) 'move-1mc-r)
            (remove-1m (state-left-bank c))
            (remove-1c (state-left-bank c))
            (add-1m (state-right-bank c))
            (add-1c (state-right-bank c))
        )
        ((eq (operator-name o) 'move-1mc-l)
            (remove-1m (state-right-bank c))
            (remove-1c (state-right-bank c))
            (add-1m (state-left-bank c))
            (add-1c (state-left-bank c))
        )
    )
    (setf (node-state new-node) c)
    new-node
)

(defmethod display-solution ((n node))
    (cond
        ((rootp n) (terpri))
        (t
            (display-solution (node-parent n))
            (princ (operator-description (node-operator)))
            (terpri)
        )
    )
    nil
)

(defmethod display-explored-nodes ()
    (prin1 'EXPLORED) (terpri)(terpri)
    (mapcar #'display-node *explored*)
    (terpri)
    nil
)

(defmethod display-unexplored-nodes ()
    (prin1 'UNEXPLORED) (terpri)(terpri)
    (mapcar #'display-node *unexplored*)
    (terpri)
    nil
)

(defmethod display-node ((n node))
    (display n)
)

(defmethod display-e-node ((n node))
    (format t "current node: ~a" (node-name n))
)