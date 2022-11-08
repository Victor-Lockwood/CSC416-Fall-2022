;----------------------------------------------------
; File: rbg.l
;----------------------------------------------------

( load "lp.l" )
( setf *limit* 25 )

;--------------------
; Select a random letter (R, B or G)

( defun rbg ()  
    ( setf selectedletter ( pick '( R B G ) ) )
    selectedletter
)

( defun rbg-string ()
    ( generate-rbg-string *limit* )
)

( defun generate-rbg-string ( n ) 
    ( cond 
        ( ( = n 0 )
           ( )
        )
        ( t
            ( setf letter ( rbg ) ) 
            ( setf generate-rbg-string ( snoc letter ( generate-rbg-string ( - n 1 ) ) ) )
            generate-rbg-string
        )
    )
)


;--------------------
; Mutate a string

( defmethod mutation ( ( rbg-str list ) &aux position symbol )
    ( setf position ( random ( length rbg-str ) ) )
    ( setf symbol ( others '( r b g ) ( nth position rbg-str ) ) )
    ( change rbg-str ( pick symbol ) position )
)

( defmethod others ( ( rbg-str list ) element )
    ( setf other-list () )
    ( dotimes ( i ( length rbg-str ) )
        ( setf candidate ( nth i rbg-str ) )
        ;( format t "candidate = ~A~%" candidate )
        ( cond 
            ( ( not ( eq candidate element ) )
                ;( format t "candidate ~A passed conditions~%" candidate )
                ( setf other-list ( snoc candidate other-list ) )
                ;( format t "other-list = ~A~%" other-list )
            )
        )
    )

    other-list
)

( defmethod change ( ( rbg-str list ) el position ) 
    ( setf mutated-list ( copy-list rbg-str ) )
    ( setf ( nth position mutated-list ) el )
    mutated-list
)

;--------------------
; Crossover two strings

( defmethod crossover ( ( m list ) ( f list ) &aux pos)
    ( setf pos ( + 1 ( random ( length m ) ) ) )
    ( append ( first-n m pos ) ( rest-n f pos ) )
)

( defmethod first-n ( ( input-list list ) position )
    ( setf return-list () )
    
    ( cond
        ( ( = position 1 )
            ( setf return-list ( snoc ( car input-list ) return-list ) )
            return-list
        )
        (
            ( setf candidate ( nth (- position 1 ) input-list ) )
            ( setf return-list ( snoc candidate ( first-n input-list ( - position 1 )  ) ) )
            return-list
        )
    )
)

( defmethod rest-n ( ( input-list list ) position )
    ( setf return-list () )
    
    ( cond
        ( ( = position ( length input-list ) )
            return-list
        )
        (
            ( setf candidate ( nth position input-list ) )
            ( setf return-list ( cons candidate ( rest-n input-list ( + position 1 ) ) ) )
            return-list
        )
    )
)