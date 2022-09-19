; File: mc_interactive.l
; Code refined from Professor Graci's base code

( defun mc ()
    ( establish-world )
    ( init-move-list )
    ( make-moves )
)

( defun establish-world ()
    ( setf *left-bank* '( M M M C C C B ) )
    ( setf *right-bank* '( ) )
)

( defun init-move-list ()
    ( setf *move-list* () )
)


( defun make-moves ()
    ( display-world )
    ( cond
        ( ( goalp )
            ( write-line "Good work!" )
            nil
        )
        ( ( feast-state-p )
            ( write-line "Yummy yummy yummy, I got Good in my tummy!!" )
            nil
        )
        ( t
            ( let ( m )
                ( format t ">>> " ) ( setf m ( read ) )
                ( if ( applicable-p m )
                    ( let () ( perform-move m ) ( make-moves ) )
                    ( let () ( write-line "Move inapplicable" ) nil )
                )
            )
        )
    )
)

( defun display-world ()
    ( format t "*left-bank*   " ) ( write *left-bank* )
    ( terpri )
    ( format t "*right-bank*   " ) ( write *right-bank* )
    ( terpri )
)

( defun feast-state-p ()
    ( cond
        ( ( > ( count 'C *right-bank* ) ( count 'M *right-bank* ) )
            ( if ( not ( member 'M *right-bank* ) ) 
                nil
                t
            )
        )
        ( ( > ( count 'C *left-bank* ) ( count 'M *left-bank* ) )
            ( if ( not ( member 'M *left-bank* ) ) 
                nil
                t
            )
        )
        ( t
            nil
        )
    )
)

( defun goalp ()
    ( cond
        ( ( equal '( M M M C C C B ) *right-bank* )
            t
        )
        ( t
            nil
        )
    )
)

( defun applicable-p ( m )
    ( setf working-bank ( current-bank ) )
    ( cond 
        ( ( < ( length m ) 2)
            nil
        )
        ( ( > ( length m ) 3)
            nil
        )
        ( ( not ( member 'B m ) ) 
            nil
        )
        ( ( not ( check-list m working-bank ) )
            nil
        )
        ( t
            t
        )
    )
)

( defun check-list ( l  working-bank )

    ( cond 
        ( ( null l)
            t
        )
        ( ( not ( member ( first l ) working-bank ) )
            nil
        )
        ( t
            ( setf working-bank ( remove ( first l ) working-bank :count 1) )
            ( check-list ( rest l ) working-bank )
        )
    )
)

( defun perform-move ( move )
    ( setf *move-list* ( snoc move *move-list* ) )
    ( if ( equal ( current-bank ) *left-bank* )
        ( move-lr move )
        ( move-rl move )
    )
)

( defun current-bank ()
    ( cond 
        ( ( member 'B *right-bank* )
            *right-bank*
        )
        ( ( member 'B *left-bank* )
            *left-bank*
        )
    
    )
)

( defun move-lr ( ml )
    ( if ( null ml ) ( return-from move-lr ) )
    ( move-lr-1 ( first ml ) )
    ( move-lr ( rest ml ) )
)

( defun move-rl ( ml )
    ( if ( null ml ) ( return-from move-rl ) )
    ( move-rl-1 ( first ml ) )
    ( move-rl ( rest ml ) )
)

( defun move-lr-1 ( m ) 
    ( setf *right-bank* ( snoc m *right-bank* ) )
    ( setf *left-bank* ( remove m *left-bank* :count 1) )
)

( defun move-rl-1 ( m ) 
    ( setf *left-bank* ( snoc m *left-bank* ) )
    ( setf *right-bank* ( remove m *right-bank* :count 1 ) )
)

( defun snoc ( o l )
    ( cond
        ( ( null l )
            ( list o )
        )
        ( t
            ( cons ( car l ) ( snoc o ( cdr l ) ) )
        )
    )
)   

( defun display-solution ( )
    ( if ( null *move-list* ) ( return-from display-solution ) )
    ( display-solution-rest *move-list* )
)

( defun display-solution-rest ( l )
    ( if ( null l ) ( return-from display-solution-rest ) )
    ( write ( first l ) )
    ( terpri )
    ( display-solution-rest ( rest l ))
)