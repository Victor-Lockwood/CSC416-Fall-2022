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

; ------------------------------
; PART 4
; ------------------------------

; Modified some Racket code I wrote in CSC344 in order to define helper functions
; Applicable file "classifier_ur.rkt" can be found in my github repo here:
; https://github.com/Victor-Lockwood/CSC344-Fall-2022

; Code from prior assignments in this class were also used.

( defun flush-p ( hand )
    ( setf check-list ( mapcar #'cdr hand ) )
    ( uniform-p check-list )
)

( defun uniform-p ( l )
    ( cond 
        ( ( null l ) t )

        ( ( = ( length l ) 1 ) t )

        ( ( equal ( car l ) ( cadr l ) ) 
            ( uniform-p ( cdr l ) )
        )

        ( t nil )
    )
)

( defun demo--flush-p ( &aux hand )
    ( format t ">>> Testing: flush-p~%" )
    ( setf hand '( ( 2 . club ) ( ace . club ) ( 10 . club ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )

    ( setf hand '( ( jack . diamond ) ( 9 . diamond ) ( 5 . diamond ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )

    ( setf hand '( ( jack . heart ) ( 10 . heart ) ( 9 . heart ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )

    ( setf hand '( ( 2 . spade) ( 3 . spade ) ( ace . spade ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )

    ( setf hand '( ( 10 . spade) ( 5 . diamond ) ( ace . spade ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )

    ( setf hand '( ( 8 . club) ( 9 . diamond ) ( 10 . heart ) ) )
    ( format t "~A " hand )
    ( if ( flush-p hand )
        ( format t "is a flush~%" )
        ( format t "is not a flush~%" )
    )
)

( defun high-card ( hand )
    ; Sort by rank first 
    ( setf sorted-list-pass1 ( sort-cards-rank hand ) )

    ; Grab the first card of the rank-sorted list
    ( setf high-card-candidate ( car sorted-list-pass1 ) )

    ; Get only the cards with that rank
    ; Trying to use filter-in and coming up with a predicate was cumbersome, so I referred to this:
    ; https://stackoverflow.com/questions/16223993/associative-list-in-lisp-get-all-entries-of-a-key-from
    ( setf filtered-list ( remove-if-not ( lambda ( x ) ( eq ( car x ) ( car high-card-candidate ) ) ) sorted-list-pass1 ) )
    
    ; Sort the filtered list by suit
    ( setf sorted-list-pass2 ( sort-cards-suit filtered-list ) )

    ; Return the car of the final sorted list 
    ( setf high-card-candidate ( car filtered-list ) )
    high-card-candidate
)

( defun demo--high-card ()
    ( format t ">>> Testing: high-card~%" )
    ( setf hand '( ( 10 . heart ) ( 5 . club ) ( queen . spade ) ( 7 . heart ) ) )
    ( format t "~A is the high card of~% ~A~%" ( high-card hand ) hand )

    ( setf hand '( ( 2 . diamond ) ( 2 . club ) ( 10 . heart ) ( 4 . diamond ) ( ace . club ) ) )
    ( format t "~A is the high card of~% ~A~%" ( high-card hand ) hand )

    ( setf hand '( ( ace . diamond ) ( ace . club ) ( 5 . spade ) ) )
    ( format t "~A is the high card of~% ~A~%" ( high-card hand ) hand )

    nil
)

( defun straight-p ( hand )
    ( setf sorted-hand ( sort-cards-rank hand ) )
    ( setf last-rank ( get-number-value-rank ( car ( car sorted-hand ) ) ) )
    ( setf rest-of-list ( cdr sorted-hand ) )
    ( setf straight-p ( straight-p-helper last-rank rest-of-list ) )
    straight-p
)

( defun straight-p-helper ( last-rank card-list )
    ( cond 
        ( ( equal ( length card-list ) 0 ) t )

        ; Check if last-rank is one more than the rank of the first element of the card list
        ( ( = last-rank ( + ( get-number-value-rank ( car ( car card-list ) ) ) 1 ) ) 
            ( straight-p-helper ( get-number-value-rank ( car ( car card-list ) ) ) ( cdr card-list ) )
        )

        ( t nil )
    )
)

( defun demo--straight-p ()
    ( format t ">>> Testing: straight-p~%" )

    ( setf hand '( ( 5 . spade) ( 3 . diamond ) ( 4 . spade ) ( 6 . club )) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )

    ( setf hand '( ( 5 . spade) ( 7 . diamond ) ( 4 . spade ) ( 8 . club ) ) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )

    ( setf hand '( ( king . heart ) ( queen . diamond ) ( ace . spade ) ( 10 . club )
    ( jack . diamond ) ) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )

    ( setf hand '( ( ace . club ) ( 2 . diamond ) ( 3 . spade ) ) )
    ( format t "~A " hand )
    ( if ( straight-p hand )
        ( format t "is a straight~%" )
        ( format t "is not a straight~%" )
    )

    nil
)

( defun analyze-hand ( hand )
    ( setf is-flush ( flush-p hand ) )
    ( setf is-straight ( straight-p hand ) )
    ( setf high-card ( high-card hand ) )
    ( setf rank-high-card ( car high-card ) )
    ( setf suit-high-card ( cdr high-card ) )

    ( cond 
        ( ( NOT is-flush ) 'bust )
        ( ( NOT is-straight ) ( list rank-high-card 'high suit-high-card 'flush ) )
        ( t ( list rank-high-card 'high 'straight suit-high-card 'flush ) )
    )
)

( defun demo--analyze-hand ()
    ( format t ">>> Testing: analyze-hand~%" )

    ( setf hand '( ( 5 . spade) ( 3 . diamond ) ( 4 . spade ) ) )
    ( format t "~A is a ~A~%" hand ( analyze-hand hand ) )

    ( setf hand '( ( 5 . club) ( 9 . club ) ( 4 . club ) ) )
    ( format t "~A is a ~A~%" hand ( analyze-hand hand ) )

    ( setf hand '( ( queen . heart ) ( ace . heart ) ( king . heart ) ) )
    ( format t "~A is a ~A~%" hand ( analyze-hand hand ) )
    nil
)

( defun sort-cards-suit ( card-list ) 
    ( sort card-list #'high-suit-p )
)

( defun sort-cards-rank ( card-list ) 
    ( sort card-list #'high-rank-p )
)

( defun high-suit-p ( card1 card2 )
   ( setf suit1 ( cdr card1 ) )
   ( setf suit2 ( cdr card2 ) )
   ( setf number-val1 ( get-number-value-suit suit1 ) )
   ( setf number-val2 ( get-number-value-suit suit2 ) )
   
   ( > number-val1 number-val2 )
)

( defun high-rank-p ( card1 card2 )
   ( setf rank1 ( car card1 ) )
   ( setf rank2 ( car card2 ) )
   ( setf number-val1 ( get-number-value-rank rank1 ) )
   ( setf number-val2 ( get-number-value-rank rank2 ) )
   
   ( > number-val1 number-val2 )
)

( defun get-number-value-suit ( input ) 
   ( cond
      ( ( eq input 'CLUB )
        1
      )
      ( ( eq input 'DIAMOND )
        2
      )
      ( ( eq input 'HEART )
        3
      )
      ( ( eq input 'SPADE )
        4
      )
   )
)

( defun get-number-value-rank ( input ) 
   ( cond
      ( ( numberp input )
        input
      )
      ( ( eq input 'JACK )
        11
      )
      ( ( eq input 'QUEEN )
        12
      )
      ( ( eq input 'KING )
        13
      )
      ( ( eq input 'ACE )
        14
      )
   )
)