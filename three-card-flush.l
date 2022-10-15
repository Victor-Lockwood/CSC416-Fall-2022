; File: three-card-flush.l

( load "lp.l" )

; ------------------------------
; PART 1
; ------------------------------

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

; ------------------------------
; PART 2
; ------------------------------

( defun deal-hands ()
    ( establish-shuffled-deck )
    ( setf *hand1* () )
    ( setf *hand2* () )
    ( deal-card-to-hand1 )
    ( deal-card-to-hand2 )
    ( deal-card-to-hand1 )
    ( deal-card-to-hand2 )
    ( deal-card-to-hand1 )
    ( deal-card-to-hand2 )
    nil
)

( defun deal-card-to-hand1 ()
    ( cond 
        ( ( null *deck* ) nil)
        ( t
            ( setf first-card ( car *deck* ) )
            ( setf *hand1* ( snoc first-card *hand1* ) )

            ( setf new-deck ( remove first-card *deck* :count 1 ) )
            ( setf *deck* new-deck ) 
            ; I do this because the delete function scares me

            nil
        )
    )
)

( defun deal-card-to-hand2 ()
    ( cond 
        ( ( null *deck* ) nil)
        ( t
            ( setf last-card ( rac *deck* ) )
            ( setf *hand2* ( snoc last-card *hand2* ) )
            ( setf new-deck ( rdc *deck* ) )
            ( setf *deck* new-deck )
            nil
        )
    )
)

( defun demo--deal-hands ()
    ( format t ">>> Testing: deal-hands~%" )

    ( deal-hands )
    ( format t "--- *hand1* = ~A~%" *hand1*)
    ( format t "--- *hand2* = ~A~%" *hand2*)
    ( format t "--- number of cards in *deck* = ~A~%" ( length *deck* ) )
)


( defun randomly-discard-cards ()
    ( randomly-discard-card-from-hand1 )
    ( randomly-discard-card-from-hand2 )
    nil
)

( defun randomly-discard-card-from-hand1 () 
    ( setf position ( random ( length *hand1* ) ) )
    ( setf ( nth position *hand1* ) () )
    nil
)

( defun randomly-discard-card-from-hand2 () 
    ( setf position ( random ( length *hand2* ) ) )
    ( setf ( nth position *hand2* ) () )
    nil
)

( defun demo--randomly-discard-cards () 
    ( format t ">>> Testing: randomly-discard-cards~%" )

    ( deal-hands )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )

    ( randomly-discard-cards )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    nil
)


; ------------------------------
; PART 3
; ------------------------------

( defun replace-cards ()
    ( replace-card-in-hand1 )
    ( replace-card-in-hand2 )
    nil
)

( defun replace-card-in-hand1 ()
    ( setf first-card ( car *deck* ) )
    ( setf position-of-void ( position nil *hand1* ) )
    ( setf ( nth position-of-void *hand1* ) first-card )

    ( setf new-deck ( remove first-card *deck* :count 1 ) )
    ( setf *deck* new-deck ) 
    nil
)

( defun replace-card-in-hand2 ()
    ( setf first-card ( car *deck* ) )
    ( setf position-of-void ( position nil *hand2* ) )
    ( setf ( nth position-of-void *hand2* ) first-card )

    ( setf new-deck ( remove first-card *deck* :count 1 ) )
    ( setf *deck* new-deck ) 
    nil
)

( defun demo--replace-cards ()
    ( format t ">>> Testing: replace-cards ~%" )

    ( deal-hands )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )

    ( randomly-discard-cards )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )

    ( replace-cards )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    nil
)

( defun players-each-take-a-turn () 
    ( randomly-discard-cards )
    ( replace-cards )
)

( defun demo--players-each-take-a-turn ()
    ( format t ">>> Testing: players-each-take-a-turn~%" )

    ( deal-hands )
    ( format t "--- The hands ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )

    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )

    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )

    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )

    ( players-each-take-a-turn )
    ( format t "--- Each player takes a turn ...~%" )
    ( format t "--- *hand1* = ~A~%" *hand1* )
    ( format t "--- *hand2* = ~A~%" *hand2* )
    
    nil
)