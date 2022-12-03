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
;
; My fitness metric(s) will end up giving points for fitting a grammar that will be defined
; later on - to give those a fighting chance, my crossovers will be in word-phrases
; in lengths 3, 6, and 7.
;--------------------------------------------

( defmethod crossover ( ( father1 list ) ( father2 list ) )
    ( setf position ( pick '( 3 6 7 ) ) )

    ( append 
        ( subseq father1 0 position ) 
        ( subseq father2 position ( length father2 ) ) 
    )
)


;--------------------------------------------
; TASK 4 - DEMOS FOR MUTATION AND CROSSOVER
;--------------------------------------------

( defmethod mutation-demo ( &aux start-sentence mutated-sentence ) 
    ( setf start-sentence ( create-sentence ) )

    ( dotimes ( i 10 )
        ( format t "--- ITERATION ~A --- ~%~%" i )
        ( format t "Start Sentence   = ~A~%" start-sentence )

        ( setf mutated-sentence ( mutation start-sentence ) )
        ( format t "Mutated Sentence = ~A~%~%" mutated-sentence )
    )
)

( defmethod crossover-demo ( &aux father1 father2 child )
    ( setf father1 ( create-sentence ) )
    ( setf father2 ( create-sentence ) )

    ( dotimes ( i 10 )
        ( format t "--- ITERATION ~A --- ~%~%" i )
        ( format t "Father 1 = ~A~%" father1)
        ( format t "Father 2 = ~A~%" father2)

        ( setf child ( crossover father1 father2 ) )
        ( format t "Child    = ~A~%~%" child )
    )
)

;--------------------------------------------
; TASK 5 - THE FITNESS METRIC
;--------------------
; There is only one fitness metric, but it's a rather complex one.  It checks for two main
; parts: first that the structure is correct, and the second that if an article is placed
; in the correct position that is used an appopriate article.
;
; The grammar structure is as follows:
; S             ->      NOUNPHRASE VERBPHRASE CONJUNCTION VERBPHRASE 
; NOUNPHRASE    ->      ARTICLE ADJECTIVE NOUN 
; VERBPHRASE    ->      VERB ARTICLE NOUN 
;
; Where VERB, ADJECTIVE, NOUN, CONJUNCTION, ARTICLE and ADJECTIVE are members
; of their previously defined banks (which is why I didn't define them again here).        
;--------------------------------------------

( defmethod fitness-grammar ( ( sentence list ) &aux word-types )
    ( setf word-types ( mapcar #'get-word-type sentence ) )
    ( setf fitness 0 )

    ( setf found-article1 T )
    ( setf found-article2 T )
    ( setf found-article3 T )
    
    ; < ARTICLE ADJECTIVE NOUN >
    ( if ( eq ( nth 0 word-types ) 'ARTICLE ) 
        ( setf fitness ( + fitness 1 ) ) 
        ( setf found-article1 nil )
    ) 
    ( if ( eq ( nth 1 word-types ) 'ADJECTIVE ) 
        ( setf fitness ( + fitness 1 ) ) 
    ) 
    ( if ( eq ( nth 2 word-types ) 'NOUN ) 
        ( setf fitness ( + fitness 1 ) ) 
    ) 

    ; < VERB ARTICLE NOUN >
    ( if ( eq ( nth 3 word-types ) 'VERB ) 
        ( setf fitness ( + fitness 1 ) ) 
    ) 
    ( if ( eq ( nth 4 word-types ) 'ARTICLE ) 
        ( setf fitness ( + fitness 1 ) ) 
        ( setf found-article2 nil )
    ) 
    ( if ( eq ( nth 5 word-types ) 'NOUN ) 
        ( setf fitness ( + fitness 1 ) ) 
    ) 

    ; < CONJ >
    ( if ( eq ( nth 6 word-types ) 'CONJ ) 
        ( setf fitness ( + fitness 1 ) ) 
    ) 

    ; < VERB ARTICLE NOUN >
    ( if ( eq ( nth 7 word-types ) 'VERB ) 
        ( setf fitness ( + fitness 1 ) ) 
    ) 
    ( if ( eq ( nth 8 word-types ) 'ARTICLE ) 
        ( setf fitness ( + fitness 1 ) ) 
        ( setf found-article3 nil )
    ) 
    ( if ( eq ( nth 9 word-types ) 'NOUN ) 
        ( setf fitness ( + fitness 1 ) ) 
    ) 

    ( if found-article1 
        ( setf fitness ( + ( article-check ( nth 0 sentence ) ( nth 1 sentence ) ) fitness ) )
    ) 

    ( if found-article2
        ( setf fitness ( + ( article-check ( nth 4 sentence ) ( nth 5 sentence ) ) fitness ) )
    ) 

    ( if found-article3
        ( setf fitness ( + ( article-check ( nth 8 sentence ) ( nth 9 sentence ) ) fitness ) )
    ) 

    fitness
)

( defun article-check ( article check-word )
    ( if ( eq article 'the ) ( return-from article-check 1 ) )

    ( setf first-letter ( char ( string check-word ) 0 ) )
    ( setf is-vowel ( vowel-p first-letter ) )

    ( if ( and is-vowel ( eq article 'an ) )
        ( return-from article-check 1 )
    )
    ( if ( and ( not is-vowel ) ( eq article 'a ) )
        ( return-from article-check 1 )
    )

    0
)

; From: https://stackoverflow.com/questions/15759828/lisp-delete-vowels-from-a-string
; Returns the character if found, which ultimately resolves to t
( defun vowel-p ( char )
    (find char "aeiou" :test #'char-equal)
)

( defmethod fitness-demo ( &aux test-sentence fitness ) 
    ( setf test-sentence ( create-sentence ) )

    ( format t "Sentence = ~A~%" test-sentence )
    ( format t "Directly applying grammar fitness...~%" )
    ( format t "fitness-grammar = ~A~%" ( fitness-grammar test-sentence ) )

    ( format t "Indirectly applying grammar fitness...~%" )
    ( setf fitness-metric #'fitness-grammar )
    ( format t "fitness-grammar = ~A~%" ( funcall fitness-metric test-sentence ) )
)