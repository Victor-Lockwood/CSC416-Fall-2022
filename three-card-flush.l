; File: three-card-flush.l
( load "lp.l" )

; By analogy with the make-notes function written in class, 
; write a make-deck function which returns a standard deck 
; of 52 cards, where each card is represented as a cons cell 
; with a rank drawn from 
; {2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king, ace} and a suit 
; is drawn from {club, diamond, heart, spade}. When I say 
; “by analogy”, I mean that this program must use mapcan and call 
; a function that uses mapcar.

( defun make-deck ()
    ( mapcan #'make-cards '( club diamond heart spade ) )
)

( defun make-cards ( suits &aux ranks )
    ( setf ranks '( 2 3 4 5 6 7 8 9 10 jack queen king ace ) )
    ( setf duplicates ( duplicate ( length ranks ) suits ) )
    ( mapcar #'cons ranks duplicates )
)

( defun demo--make-deck ()
    ( format t ">>> Testing: make-deck~%" )
    ( setf deck ( make-deck ) )
    ( format t "--- deck = ~A~%" deck )
    ( format t "--- number of cards in deck = ~A~%" ( length deck ) )
    nil
)


( defun establish-shuffled-deck ()
    ( setf *deck* ( shuffle ( make-deck ) ) )
    nil
)

( defun shuffle ( deck )
    ( cond 
        ( ( null deck ) () )
        ( t 
            ( setf card ( pick deck) )
            ( setf shuffled-deck ( cons card ( shuffle ( remove card deck :count 1 ) ) ) )
            shuffled-deck
        )
    )
)

( defun demo--establish-shuffled-deck ()
    ( format t ">>> Testing: establish-shuffled-deck~%" )
    ( establish-shuffled-deck )
    ( format t "--- *deck* ...~A~%" *deck* )
    ( format t "--- number of cards in *deck* = ~A~%" ( length *deck* ) )
    nil
)