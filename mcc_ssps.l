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


;-----------------------------------
; Move methods

( defmethod move-c ( ( from-bank bank ) ( to-bank bank) )
    ( pop ( bank-cannibals from-bank ) ) 
    ( push 'C ( bank-cannibals to-bank ) ) 

    ( setf ( bank-boat from-bank ) nil )
    ( setf ( bank-boat to-bank ) 'B )

    nil
)

( defmethod move-cc ( ( from-bank bank ) ( to-bank bank) )
    ( pop ( bank-cannibals from-bank ) ) 
    ( push 'C ( bank-cannibals to-bank ) ) 

    ( pop ( bank-cannibals from-bank ) ) 
    ( push 'C ( bank-cannibals to-bank ) ) 

    ( setf ( bank-boat from-bank ) nil )
    ( setf ( bank-boat to-bank ) 'B )

    nil
)

( defmethod move-m ( ( from-bank bank ) ( to-bank bank) )
    ( pop ( bank-missionaries from-bank ) ) 
    ( push 'M ( bank-missionaries to-bank ) ) 

    ( setf ( bank-boat from-bank ) nil )
    ( setf ( bank-boat to-bank ) 'B )

    nil
)

( defmethod move-mm ( ( from-bank bank ) ( to-bank bank) )
    ( pop ( bank-missionaries from-bank ) ) 
    ( push 'M ( bank-missionaries to-bank ) ) 

    ( pop ( bank-missionaries from-bank ) ) 
    ( push 'M ( bank-missionaries to-bank ) ) 

    ( setf ( bank-boat from-bank ) nil )
    ( setf ( bank-boat to-bank ) 'B )

    nil
)

( defmethod move-cm ( ( from-bank bank ) ( to-bank bank) )
    ( pop ( bank-cannibals from-bank ) ) 
    ( push 'C ( bank-cannibals to-bank ) ) 

    ( pop ( bank-missionaries from-bank ) ) 
    ( push 'M ( bank-missionaries to-bank ) ) 

    ( setf ( bank-boat from-bank ) nil )
    ( setf ( bank-boat to-bank ) 'B )

    nil
)

;-----------------------------------
; Checks if banks are equal 

( defmethod equal-bank-p ( ( b1 bank ) ( b2 bank ) )
    ( and 
        ( equal ( bank-missionaries b1 ) ( bank-missionaries b2 ) )
        ( equal ( bank-cannibals b1 ) ( bank-cannibals b2 ) )
        ( equal ( bank-boat b1 ) ( bank-boat b2 ) )
    )
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

( defmethod copy-state ( ( s state ) ) 
    ( setf new-left-bank ( copy-bank ( state-left-bank s ) ) )
    ( setf new-right-bank ( copy-bank ( state-right-bank s ) ) )
    ( make-instance 'state :left-bank new-left-bank :right-bank new-right-bank )
)

( defmethod copy-bank ( ( b bank ) &aux new-boat )
    ( setf new-boat nil )
    ( if ( eq ( bank-boat b ) 'B ) ( setf new-boat nil ) )
    ( make-instance 'bank :missionaries ( copy-list ( bank-missionaries b ) ) :cannibals ( copy-list ( bank-cannibals b ) ) :boat new-boat  )
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

;-----------------------------------
; Checks if states are equal 

( defmethod equal-state-p ( ( s1 state ) ( s2 state ) )
    ( and 
        ( equal-bank-p ( state-right-bank s1 ) ( state-right-bank s2 ) )
        ( equal-bank-p ( state-left-bank s1 ) ( state-left-bank s2 ) )
    )
)

;-----------------------------------
; Checks applicability of an operator 

; LEFT BANK TO RIGHT BANK
( defmethod applicable-move-c-lr ( ( s state ) )
    ( setf lb ( state-left-bank s ) )

    ( and
        ( member 'C ( bank-cannibals lb ) )
        ( equal 'B ( bank-boat lb ) )
    )
)

( defmethod applicable-move-cc-lr ( ( s state) )
    ( setf lb ( state-left-bank s ) )

    ( and
        ( >= ( count 'C ( bank-cannibals lb ) ) 2 )
        ( equal 'B ( bank-boat lb ) )
    )
)

( defmethod applicable-move-m-lr ( ( s state) )
    ( setf lb ( state-left-bank s ) )

    ( and
        ( member 'M ( bank-missionaries lb ) )
        ( equal 'B ( bank-boat lb ) )
    )
)

( defmethod applicable-move-mm-lr ( ( s state) )
    ( setf lb ( state-left-bank s ) )

    ( and
        ( >= ( count 'M ( bank-missionaries lb ) ) 2 )
        ( equal 'B ( bank-boat lb ) )
    )
)

( defmethod applicable-move-cm-lr ( ( s state) )
    ( setf lb ( state-left-bank s ) )

    ( and
        ( member 'M ( bank-missionaries lb ) )
        ( member 'M ( bank-missionaries lb ) )
        ( equal 'B ( bank-boat lb ) )
    )
)

; RIGHT BANK TO LEFT BANK
( defmethod applicable-move-c-rl ( ( s state ) )
    ( setf rb ( state-right-bank s ) )

    ( and
        ( member 'C ( bank-cannibals rb ) )
        ( equal 'B ( bank-boat rb ) )
    )
)

( defmethod applicable-move-cc-rl ( ( s state) )
    ( setf rb ( state-right-bank s ) )

    ( and
        ( >= ( count 'C ( bank-cannibals rb ) ) 2 )
        ( equal 'B ( bank-boat rb ) )
    )
)

( defmethod applicable-move-m-rl ( ( s state) )
    ( setf rb ( state-right-bank s ) )

    ( and
        ( member 'M ( bank-missionaries rb ) )
        ( equal 'B ( bank-boat rb ) )
    )
)

( defmethod applicable-move-mm-rl ( ( s state) )
    ( setf rb ( state-right-bank s ) )

    ( and
        ( >= ( count 'M ( bank-missionaries rb ) ) 2 )
        ( equal 'B ( bank-boat rb ) )
    )
)

( defmethod applicable-move-cm-rl ( ( s state) )
    ( setf rb ( state-right-bank s ) )

    ( and
        ( member 'M ( bank-missionaries rb ) )
        ( member 'M ( bank-missionaries rb ) )
        ( equal 'B ( bank-boat rb ) )
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

;-----------------------------------
; Checks if node has been explored

( defmethod exploredp ( ( s state ) )
    ( setf explored-states ( mapcar #'node-state *explored* ) )
    ( member s explored-states :test #'equal-state-p )
)

( defmethod equal-node-p ( ( n1 node ) ( n2 node ) ) 
    ( equal-state-p ( node-state n1 ) ( node-state n2 ) )
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

( defmethod display ( ( o operator ) )
    ( format t "~A " ( operator-name o ) )
)

;-----------------------------------
; Checks if operator is applicable 

( defmethod applicablep ( ( op operator ) ( n node ) )
    ( cond 
        ; LEFT BANK TO RIGHT BANK
        ( ( eq ( operator-name op ) 'move-c-lr )
            ( applicable-move-c-lr ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-cc-lr )
            ( applicable-move-cc-lr ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-m-lr )
            ( applicable-move-m-lr ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-mm-lr )
            ( applicable-move-mm-lr ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-cm-lr )
            ( applicable-move-cm-lr ( node-state n ) )
        )

        ; RIGHT BANK TO LEFT BANK
        ( ( eq ( operator-name op ) 'move-c-rl )
            ( applicable-move-c-rl ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-cc-rl )
            ( applicable-move-cc-rl ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-m-rl )
            ( applicable-move-m-rl ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-mm-rl )
            ( applicable-move-mm-rl ( node-state n ) )
        )
        ( ( eq ( operator-name op ) 'move-cm-rl )
            ( applicable-move-cm-rl ( node-state n ) )
        )
    )
)

;---------------------------------------------------------------------------------
; METHOD TO ESTABLISH THE 8 STATE SPACE OPERATORS AND ENCAPSULATE THEM IN A LIST
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; The following global variables are bound when establish-operatores is run:
; *move-c-lr*, *move-cc-lr*, *move-m-lr*, *move-mm-lr*, *move-cm-lr*,
; *move-c-rl*, *move-cc-rl*, *move-m-rl*, *move-mm-rl*, *move-cm-rl*

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
            :name 'move-cc-rl
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
    ; LEFT BANK TO RIGHT BANK
    ( if ( applicablep *move-c-lr* e-node ) 
        ( push ( child-of e-node *move-c-lr* ) kids )
    )

    ( if ( applicablep *move-cc-lr* e-node ) 
        ( push ( child-of e-node *move-cc-lr* ) kids )
    )

    ( if ( applicablep *move-m-lr* e-node ) 
        ( push ( child-of e-node *move-m-lr* ) kids )
    )

    ( if ( applicablep *move-mm-lr* e-node ) 
        ( push ( child-of e-node *move-mm-lr* ) kids )
    )

    ( if ( applicablep *move-cm-lr* e-node ) 
        ( push ( child-of e-node *move-cm-lr* ) kids )
    )

    ; RIGHT BANK TO LEFT BANK
    ( if ( applicablep *move-c-rl* e-node ) 
        ( push ( child-of e-node *move-c-rl* ) kids )
    )

    ( if ( applicablep *move-cc-rl* e-node ) 
        ( push ( child-of e-node *move-cc-rl* ) kids )
    )
    
    ( if ( applicablep *move-m-rl* e-node ) 
        ( push ( child-of e-node *move-m-rl* ) kids )
    )

    ( if ( applicablep *move-mm-rl* e-node ) 
        ( push ( child-of e-node *move-mm-rl* ) kids )
    )

    ( if ( applicablep *move-cm-rl* e-node ) 
        ( push ( child-of e-node *move-cm-rl* ) kids )
    )
    kids
)

( defmethod child-of ( ( n node ) ( o operator ) &aux c )
    ( setf new-node ( make-instance 'node ) )
    ( setf ( node-name new-node ) ( next *ng* ) )
    ( setf ( node-parent new-node ) n )
    ( setf ( node-operator new-node ) o )
    ( setf c ( copy-state ( node-state n ) ) )
    ( apply-operator o c )
    ( setf ( node-state new-node ) c )
    new-node
)

( defmethod apply-operator ( ( op operator ) ( s state ) )
    ( setf lb ( state-left-bank s ) )
    ( setf rb ( state-right-bank s ) )
    ( cond 
        ; LEFT BANK TO RIGHT BANK
        ( ( eq ( operator-name op ) 'move-c-lr )
            ( move-c lb rb )
        )
        ( ( eq ( operator-name op ) 'move-cc-lr )
            ( move-cc lb rb )
        )
        ( ( eq ( operator-name op ) 'move-m-lr )
            ( move-m lb rb )
        )
        ( ( eq ( operator-name op ) 'move-mm-lr )
            ( move-mm lb rb )
        )
        ( ( eq ( operator-name op ) 'move-cm-lr )
            ( move-cm lb rb )
        )

        ; RIGHT BANK TO LEFT BANK
        ( ( eq ( operator-name op ) 'move-c-rl )
            ( move-c rb lb )
        )
        ( ( eq ( operator-name op ) 'move-cc-rl )
            ( move-cc rb lb )
        )
        ( ( eq ( operator-name op ) 'move-m-rl )
            ( move-m rb lb )
        )
        ( ( eq ( operator-name op ) 'move-mm-rl )
            ( move-mm rb lb )
        )
        ( ( eq ( operator-name op ) 'move-cm-rl )
            ( move-cm rb lb )
        )
    )
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

;------------------------------------------------------------------
; DISPLAY THE SOLUTION

( defmethod display-solution ( ( n node ) ) 
    ( cond
        ( ( rootp n )
            ( terpri )
        )
        ( t
            ( display-solution ( node-parent n ) )
            ( format t "Poop~%")
        )
    )
)