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
; Take a sentence and change a word to one that is of a different type.
; For example, it's not particularly interesting to only swap an 'AND' for a 'BUT' 
; when functionally they're pretty equivalent.
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

    ( setf old-word-type ( get-word-type old-word ) )
    ( setf new-word-type ( get-word-type new-word) )
    ( if ( eq old-word-type new-word-type ) ( get-new-word old-word ) )

    new-word
)

( defun get-word-type ( word-to-check )
    ( setf type nil )

    ( cond
        ( ( member word-to-check *articles* )
            ( setf type 'ARTICLE )
        )
        ( ( member word-to-check *nouns* )
            ( setf type 'NOUN )
        )
        ( ( member word-to-check *adjectives* )
            ( setf type 'ADJECTIVE )
        )
        ( ( member word-to-check *conjunctions* )
            ( setf type 'CONJ )
        )
        ( ( member word-to-check *verbs* )
            ( setf type 'VERB )
        )
    )
    type
)


;--------------------------------------------
; TASK 3 - SENTENCE CROSSOVER
;--------------------
; Take two sentences and create a new one by crossing them over with each other.
; I found an easier way of going about grabbing sections of lists from here:
; https://stackoverflow.com/questions/1719551/getting-the-first-n-elements-of-a-list-in-common-lisp
; I think it makes more sense to use a built-in method.
;--------------------------------------------

( defmethod crossover ( ( list1 list ) ( list2 list ) )
    ( setf position ( + 1 ( random ( length list1 ) ) ) )

    ( format t "Position: ~A~%" position )
    ( if ( = position ( length list2 ) ) ( setf position ( - position 1 ) ) )
    ( format t "Position Change: ~A~%" position )

    ( append 
        ( subseq list1 0 position ) 
        ( subseq list2 position ( length list2 ) ) 
    )
)