;----------------------------------------------------
; File: wildcardga.l
;----------------------------------------------------

;--------------------------------------------
; TASK 1 - WORD BANK AND SENTENCE CREATOR
;--------------------
; From a set word bank, generate a sentence of a length dictated by *word-limit*.
; The word bank is derived from 5 sub-banks, which will help us
; create fitness metrics later.
;--------------------------------------------


( load "lp.l" )
( setf *word-limit* 10 )


; Establish the word lists

( setf *articles* '( THE A AN ) )
( setf *nouns* '( ROBOT COMPUTER HUMAN CAT PLANT PAPER BOOK ) )
( setf *adjectives* '( TALL SMALL ANGRY HAPPY MEAN EXCITED NERVOUS SHY RUDE SPITEFUL ) )
( setf *conjunctions* '( AND BUT ) )
( setf *verbs* '( IS HAS THROWS HITS HUGS HATES LOVES LIFTS DESTROYS WATERS FEEDS PATS ) )
( setf *word-bank* ( append *articles* *nouns* *adjectives* *conjunctions* *verbs* ) )


; Sentence constructors

( defun word ()
    ( pick *word-bank* )
)

( defun create-sentence ()
    ( setf sentence () )

    ( dotimes ( i *word-limit* ) 
        ( setf sentence ( snoc ( word ) sentence ) )
    )

    sentence
)


;--------------------------------------------
; TASK 2 - SENTENCE MUTATION
;--------------------
; Take a sentence and change a word to something else from the word bank.
; As of now, everything is still very close to the original GA mimicked prior.
; However, I modified some methods and functions to my liking, in part to simplify the code
; and for it to make more sense to me.
;--------------------------------------------

( defmethod mutation ( ( sentence list ) &aux position old-word new-word )
    ( setf position ( random ( length sentence ) ) )
    ( setf old-word ( nth position sentence ) )
    ( setf new-word ( get-new-word old-word ) )

    ( setf mutated-sentence ( copy-list sentence ) )
    ( setf ( nth position mutated-sentence ) new-word )

    mutated-sentence
)

( defun get-new-word ( old-word )
    ( setf new-word ( word ) )
    ( if ( eq old-word new-word ) ( get-new-word old-word ) )
    new-word
)