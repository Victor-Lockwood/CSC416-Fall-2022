; Preliminary code by Professor Graci
;---------------------------------------------------------------------------------
; GENERAL INFORMATION
;
; FILE: mc_ssps.l
; DATE: Fall 2020
; LINE: State Space Solver for the Missionaries and Cannibals Problem
;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
; PROGRAM DESCRIPTION
;
; This program is a state space problem solver for a classic missionaries and
; cannibals problem. An explicit state space tree is grown in concert with breadth
; first search for a solution.
;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
; REPRESENTATIONAL NOTES
;
; Banks are represented as a 3-slot class consisting of
; missionaries, cannibals, and boat.
;
; States are represented as a 2-slot class consisting of
; left-bank and right-bank.
;
; Operators are represented as a 3-slot class consisting of
; a name, a precondition, and a description.
;
; Nodes are represented as a 4-slot class consisting of
; a name, a state, a parent node, and a move (state space operator)
;---------------------------------------------------------------------------------

;---------------------------------------------------------------------------------
; MODELING A BANK

( defclass bank ()
    (
        ( missionaries :accessor bank-missionaries :initarg :missionaries )
        ( cannibals :accessor bank-cannibals :initarg :cannibals )
        ( boat :accessor bank-boat :initarg :boat )
    )
)

( defmethod display ( ( b bank ) ) 
    ( format t "MISSIONARIES: ~A    CANNIBALS: ~A   BOAT=~A~%" ( bank-missionaries b ) ( bank-cannibals b ) ( bank-boat b ) )
)

;------------------------------------------------------------------
; MODELLING A STATE

( defclass state ()
    (
        ( left-bank :accessor state-left-bank :initarg :left-bank )
        ( right-bank :accessor state-right-bank :initarg :right-bank )
    )
)

( defmethod display ( ( s state ) )
    ( display ( state-left-bank s ) )
    ( display ( state-right-bank s ) )
    nil
)

;-----------------------------------
; Checks if state is a goal state 

( defmethod goalp ( ( s state ) )
    ( and 
        ( leftbank-ok-p ( state-left-bank s ) )
        ( rightbank-ok-p ( state-right-bank s ) )
    )
)

( defmethod leftbank-ok-p ( ( lb bank ) )
    ( and 
        ( null ( bank-missionaries lb ) )
        ( null ( bank-cannibals lb ) )
        ( null ( bank-boat lb ) )
    )
)

( defmethod rightbank-ok-p ( ( rb bank ) )
    ( and 
        ( equal '( M M M ) ( bank-missionaries rb ) )
        ( equal '( C C C ) ( bank-cannibals rb ) )
        ( equal 'B ( bank-boat rb ) )
    )
)

;-----------------------------------
; Checks if state is a feast state 

( defmethod feast-state-p ( ( s state ) ) 
    ( or
        ( leftbank-feast-p ( state-left-bank s ) )
        ( rightbank-feast-p ( state-right-bank s ) )
    )
)

( defmethod leftbank-feast-p ( ( lb bank ) )
    ( and 
        ( member 'M ( bank-missionaries lb ) )
        ( > ( count 'C ( bank-cannibals lb ) ) ( count 'M ( bank-missionaries lb ) ) )
    )
)

( defmethod rightbank-feast-p ( ( rb bank ) )
    ( and 
        ( member 'M ( bank-missionaries rb ) )
        ( > ( count 'C ( bank-cannibals rb ) ) ( count 'M ( bank-missionaries rb ) ) )
    )
)

;------------------------------------------------------------------
; MODELLING A NODE

( defclass node ()
    (
        ( name :accessor node-name :initarg :name )
        ( state :accessor node-state :initarg :state )
        ( parent :accessor node-parent :initarg :parent )
        ( operator :accessor node-operator :initarg :operator )
    )
)

( defmethod display ( ( n node ) )
    ( format t "~A " ( node-name n ) )
    ( if ( not ( rootp n ) ) 
        ( let ()
            ( format t "~A " ( node-name ( node-parent n ) ) )
            ( display ( node-operator n ) )
        )
    )
    ( terpri )
    ( display ( node-state n ) )
    nil
)

( defmethod display-the-e-node ( ( e-node node ) ) 
    ( format t ">>> E-NODE~%" )
    ( display e-node )
    ( terpri )
    nil
)

;-----------------------------------
; Checks if node is a root node 

( defmethod rootp ( ( n node ) )
    ( eq ( node-name n ) 'root )
)

;---------------------------------------------------------------------------------
; MODELING A STATE SPACE OPERATOR

( defclass operator ()
    (
        ( name :accessor operator-name :initarg :name )
        ( precondition :accessor operator-precondition :initarg :precondition )
        ( description :accessor operator-description :initarg :description )
    )
)

;---------------------------------------------------------------------------------
; METHOD TO ESTABLISH THE 8 STATE SPACE OPERATORS AND ENCAPSULATE THEM IN A LIST
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; The following global variables are bound when establish-operatores is run:
; *move-1c*, *move-2c*, *move-1m*, *move-2m*, *move-1m-1c*

( defmethod establish-operators ()
    ; LEFT BANK TO RIGHT BANK
    ( setf *move-c-lr* 
        ( make-instance 'operator
            :name 'move-c-lr 
            :precondition "At least one cannibal and the boat are available to move on the left bank"
            :description "Move one cannibal and the boat to the right bank"
        )
    )
    ( setf *move-cc-lr* 
        ( make-instance 'operator
            :name 'move-cc-lr
            :precondition "At least two cannibals and the boat are available to move on the left bank"
            :description "Move two cannibals and the boat to the right bank"
        )
    )
    ( setf *move-m-lr* 
        ( make-instance 'operator
            :name 'move-m-lr
            :precondition "At least one missionary and the boat are available to move on the left bank"
            :description "Move one missionary and the boat to the right bank"
        )
    )
    ( setf *move-mm-lr* 
        ( make-instance 'operator
            :name 'move-mm-lr
            :precondition "At least two missionaries and the boat are available to move on the left bank"
            :description "Move two missionaries and the boat to the right bank"
        )
    )
    ( setf *move-cm-lr* 
        ( make-instance 'operator
            :name 'move-cm-lr
            :precondition "At least one missionary, one cannibal and the boat are available to move on the left bank"
            :description "Move one missionary, one cannibal and the boat to the right bank"
        )
    )

    ; RIGHT BANK TO LEFT BANK
    ( setf *move-c-rl* 
        ( make-instance 'operator
            :name 'move-c-rl 
            :precondition "At least one cannibal and the boat are available to move on the right bank"
            :description "Move one cannibal and the boat to the left bank"
        )
    )
    ( setf *move-cc-rl* 
        ( make-instance 'operator
            :name 'move-cc-lr
            :precondition "At least two cannibals and the boat are available to move on the right bank"
            :description "Move two cannibals and the boat to the left bank"
        )
    )
    ( setf *move-m-rl* 
        ( make-instance 'operator
            :name 'move-m-rl
            :precondition "At least one missionary and the boat are available to move on the right bank"
            :description "Move one missionary and the boat to the left bank"
        )
    )
    ( setf *move-mm-rl* 
        ( make-instance 'operator
            :name 'move-mm-rl
            :precondition "At least two missionaries and the boat are available to move on the right bank"
            :description "Move two missionaries and the boat to the left bank"
        )
    )
    ( setf *move-cm-rl* 
        ( make-instance 'operator
            :name 'move-cm-rl
            :precondition "At least one missionary, one cannibal and the boat are available to move on the right bank"
            :description "Move one missionary, one cannibal and the boat to the left bank"
        )
    )
    nil
)

;---------------------------------------------------------------------------------
; THE MAIN PROGRAM - argument values of e u x eu ex ux eux will cause tracing

( defmethod mc ( ( trace-instruction symbol ) ) 
    ( setf *trace-instruction* trace-instruction )
    ( establish-operators )
    ( setup )
    ( solve )
)

( defmethod display-the-unexplored-list ()
    ( format t "~%>>> UNEXPLORED LIST~%" )
    ( display-unexplored-states )
    nil
)

( defmethod display-unexplored-states ()
    ( mapcar #'display *unexplored* )
    nil
)

( defmethod display-the-explored-list ()
    ( format t "~%>>> EXPLORED LIST~%" )
    ( display-explored-states )
    ( terpri )
    nil
)

( defmethod display-explored-states ()
    ( mapcar #'display *explored* )
    nil
)

;---------------------------------------------------------------------------------
; SOLVE PERFORMS BREADTH FIRST SEARCH

( defmethod solve ( &aux kids e-node )
    ( if ( member *trace-instruction* '( u eu ux eux ) ) ( display-the-unexplored-list ) )
    ( if ( member *trace-instruction* '( x ex ux eux ) ) ( display-the-explored-list ) )
    ( cond
        ( ( null *unexplored* )
            ( format t ">>> THERE IS NO SOLUTION. ~%" )
            ( return-from solve NIL )
        )
    )
    ( setf e-node ( pop *unexplored* ) )
    ( if ( member *trace-instruction* '( e ex eu eux ) ) ( display-the-e-node e-node ) )
    ( cond
        ( ( goalp ( node-state e-node ) ) 
            ( format t "~%>>> GOT A SOLUTION!" )
            ( display-solution e-node )
            ( display-solution e-node )
        )
        ( ( feast-state-p ( node-state e-node ) )
            ( solve )
        )
        ( ( exploredp ( node-state e-node ) )
            ( solve )
        )
        ( t
            ( push e-node *explored* )
            ( setf kids ( children-of e-node ) )
            ( setf *unexplored* ( append *unexplored* kids ) )
            ( solve )
        )
    )
    nil
)

( defmethod exploredp ( ( s state ) )
    ;### best to use member with two key word args -- :key and :test
)

;------------------------------------------------------------------
; THE SETUP

( defmethod setup ( &aux root lb rb istate )
    ;; establish root node
    ( setf lb ( make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b ) )
    ( setf rb ( make-instance 'bank :missionaries '() :cannibals '() :boat nil ) )
    ( setf istate ( make-instance 'state :left-bank lb :right-bank rb ) )
    ( setf root ( make-instance 'node :state istate :name 'root ) )
    ;; initialize list of unexplored nodes
    ( setf *unexplored* ( list root ) )
    ;; initialize list of explored nodes
    ( setf *explored* () )
    ; get ready to create good names
    ( setf *ng* ( make-instance 'name-generator :prefix "N" ) )
)

;------------------------------------------------------------------
; GENERATING CHILDREN

( defmethod children-of ( ( e-node node ) &aux kids ) 
    ;###
)

( defmethod child-of ( ( n node ) ( o operator ) &aux c )
    ( setf new-node ( make-instance 'node ) )
    ( setf ( node-name new-node ) ( next *ng* ) )
    ( setf ( node-parent new-node ) n )
    ( setf ( node-operator new-node ) o )
    ( setf c ( copy-state (node-state n ) ) )
    ( apply-operator o c )
    ( setf ( node-state new-node ) c )
    new-node
)

;------------------------------------------------------------------
; MODELLING A NAME-GENERATOR

( defclass name-generator ()
    ( ( prefix :accessor name-generator-prefix :initarg :prefix :initform "name" )
      ( nr :accessor name-generator-nr :initform 0 )
    )
)

( defmethod next ( ( ng name-generator ) ) 
    ( setf ( name-generator-nr ng ) ( + 1 ( name-generator-nr ng ) ) )
    ( concatenate 'string
        ( name-generator-prefix ng )
        ( write-to-string ( name-generator-nr ng ) )
    )
)