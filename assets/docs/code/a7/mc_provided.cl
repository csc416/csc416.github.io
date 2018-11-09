; Author: Prof. Craig Graci
; Course: CSc 416
;
; Problem: Programming Challenge: M&C State Space Problem Solver
;
; Program Description
;
; Missionaries and Cannibals problem solver.
;
; This program is a state space problem solver for the classic 
;   "Missionaries and Cannibals" problem. An explicit state
;   space tree is grown with breadth first search
;   for a solution.
;
; Banks are represented as a 3-slot class consisting of
;   missionaries, cannibals, and boat.
;
; States are represented as a 2-slot class consisting of
;   left-bank and right-bank.
;
; Operators are represented as a 3-slot class consisting of a name,
;   a precondition, and a description.

;---------------------
; Modelling a bank

( defclass bank ()
    (
        ( missionaries :accessor bank-missionaries :initarg :missionaries )
        ( cannibals :accessor bank-cannibals :initarg :cannibals )
        ( boat :accessor bank-boat :initarg :boat )
    )
)

;---------------------
; Modelling a state

( defclass state ()
    (
        ( left-bank :accessor state-left-bank :initarg :left-bank )
        ( right-bank :accessor state-right-bank :initarg :right-bank )
    )
)

( defmethod display ( ( s state ) ) 
    ( display ( state-left-bank s ) )
    ( display ( state-right-bank s ) )
    (terpri)
    nil
)

;---------------------
; Modelling a node

( defclass node ()
    (
        ( name :accessor node-name :initarg :name)
        ( state :accessor node-state :initarg :state)
        ( parent :accessor node-parent :initarg :parent)
        ( operator :accessor node-operator :initarg :operator)
    )
)

( defmethod display ( ( n node) )
    ( princ ( node-name n ))
    ( if ( not ( rootp n ))
        ( let () 
            ( princ " " ) ( princ ( node-name ( node-parent n ) ) ) ( princ " ")
            ( display ( node-operator n ) )
        )
    )
    ( terpri )
    ( display ( node-state n ) )
    nil
)

;-----------------------
; Modelling an operator

( defclass operator () 
    (
        ( name :accessor operator-name :initarg :name)
        ( precondition :accessor operator-precondition :initarg :precondition)
        ( description :accessor operator-description :initarg :description)
    )
)


;; ;----------------------------
; Modelling a name-generator

( defclass name-generator () 
    ( ( prefix :accessor name-generator-prefix :initarg :prefix :initform "name" )
      ( nr :accessor name-generator-nr :initform 0 )
    )
)

( defmethod next ( ( ng name-generator ) )
    ( setf ( name-generator-nr ng ) ( + 1 ( name-generator-nr ng ) ) )
    ( concatenate  'string 
        ( name-generator-prefix ng )
        ( write-to-string ( name-generator-nr ng ))
    )
)

;----------------------------
; Main method

( defmethod mc () 
    ( establish-operators )
    ( setup )
    ( solve )
)

;----------------------------
; The setup

( defmethod setup ( &aux root lb rb istate) 
  ;; establish root node
  ( setf lb ( make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b ) )
  ( setf rb ( make-instance 'bank :missionaries '() :cannibals '() :boat nil ) )
  ( setf istate ( make-instance 'state :left-bank lb :right-bank rb ) )
  ( setf root ( make-instance 'node  :state istate :name "root") )
  ;; initialize list of unexplored nodes
  ( setf *unexplored* ( list root ) )
  ;; initialize list of explored nodes
  ( setf *explored* () )
  ;; get ready to create good names
  ( setf *ng* ( make-instance 'name-generator :prefix "N" ) )
)

;----------------------------
; breadth first search

( defmethod solve ( &aux kids e-node ) 
    ( if *trace-search* 
        ( let ()
            ( terpri ) ( write-line ">> Solve")
            ( display-explored-nodes )
            ( display-unexplored-nodes )
        )
    )
    ( cond
        ( ( null *unexplored* ) 
            ( write-line "There is no solution." )
            ( return-from solve NIL )
        )
    )
    ( setf e-node ( pop *unexplored* ) )
    ( if *trace-search* 
        ( let ()
            ( display-e-node e-node )
        )
    )
    ( cond 
        ( ( goalp ( node-state e-node ) ) 
            ( display-solution e-node )
        )
        ( ( exploredp e-node )
            ( solve )
        )
        ( t
            ( push e-node *explored* )
            ( setf kids ( children-of e-node ) )
            ( setf *unexplored* ( append *unexplored* kids ) )
            ( solve )
        )
    )
    NIL
)

( defmethod child-of ( ( n node ) ( o operator ) &aux c )
    ( setf new-node (make-instance 'node) )
    ( setf ( node-name new-node ) ( next *ng* ) )
    ( setf ( node-parent new-node ) n)
    ( setf ( node-operator new-node ) o)
    ( setf c ( copy-state ( node-state n ) ) )
    ( apply-operator o c )
    ( setf (node-state new-node) c )
    new-node
)