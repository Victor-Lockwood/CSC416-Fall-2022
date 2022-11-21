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


;--------------------
; Crossover and Mutation Demos

( defmethod mutation-demo (&aux s m)
    ( setf s ( rbg-string ) )
    ( dotimes ( i 10 )
        ( format t "s = ~A~%" s )
        ( setf m ( mutation s ) )
        ( format t "m = ~A~%~%" m )
    )
)

( defmethod crossover-demo (&aux m f x)
    ( setf m ( rbg-string ) )
    ( setf f ( rbg-string ) )
    ( dotimes ( i 10 )
        ( format t "m = ~A~%" m )
        ( setf x ( crossover m f ) )
        ( format t "x = ~A~%" x )
        ( format t "f = ~A~%~%" f )
    )
)


;--------------------
; Fitness metrics

( defmethod fitness-r ( ( rbg-string list ) )
    ( setf num-r ( count 'R rbg-string ) )
    num-r
)

( defmethod fitness-b ( ( rbg-string list ) )
    ( setf num-b ( count 'B rbg-string ) )
    num-b
)

( defmethod fitness-g ( ( rbg-string list ) )
    ( setf num-g ( count 'G rbg-string ) )
    num-g
)

( defmethod fitness-demo (&aux x fitness)
    ( setf x (rbg-string) )
    ( format t "x = ~A~%" x )
    ( format t "Directly applying the fitness metrics ...~%" )
    ( format t "fitness-r = ~A~%" ( fitness-r x ) )
    ( format t "fitness-b = ~A~%" ( fitness-b x ) )
    ( format t "fitness-g = ~A~%" ( fitness-g x ) )
    ( format t "Indirectly applying the fitness metrics ...~%" )
    ( setf fitness #'fitness-r )
    ( format t "fitness-r = ~A~%" ( funcall fitness x ) )
    ( setf fitness #'fitness-g )
    ( format t "fitness-g = ~A~%" ( funcall fitness x ) )
    ( setf fitness #'fitness-b )
    ( format t "fitness-b = ~A~%" ( funcall fitness x ) )
)

;--------------------
; Individual class

( defclass individual ()
    (
        ( rbg-string :accessor individual-rbg-string :initarg :rbg-string )
        ( fitness :accessor individual-fitness :initarg :fitness )
        ( number :accessor individual-number :initarg :number )
    )
)

( defmethod random-individual (&aux rbg)
    ( setf rbg ( rbg-string ) )
    ( make-instance 'individual
        :rbg-string rbg
        :fitness ( funcall *fitness* rbg )
        :number 0
    )
)

( defmethod new-individual ( ( nr number ) ( notes list ) )
    ( make-instance 'individual
        :rbg-string notes
        :fitness ( funcall *fitness* notes )
        :number nr
    )
)

( defmethod display ( ( i individual ) )
    ( display-nnl i ) ( terpri )
)

( defmethod display-nnl ( ( i individual ) )
    ( prin1 ( individual-number i ) )
    ( princ ( filler ( individual-number i ) ) )
    ( prin1 ( individual-rbg-string i ) )
    ( princ " " )
    ( prin1 ( individual-fitness i ) )
    ( princ ( filler ( individual-fitness i ) ) )
)

( defmethod filler ( ( n number ) )
    ( cond
        ( ( < n 10 ) " " )
        ( ( < n 100 ) " " )
        ( ( < n 1000 ) " " )
        ( ( < n 10000 ) " " )
        ( ( < n 100000 ) " " )
    )
)

( defmethod fitness-b ( ( i individual ) )
    ( fitness-b ( individual-rbg-string i ) )
)

( defmethod fitness-r ( ( i individual ) )
    ( fitness-r ( individual-rbg-string i ) )
)

( defmethod fitness-g ( ( i individual ) )
    ( fitness-g ( individual-rbg-string i ) )
)

( defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
    ( setf *fitness* #'fitness-r )
    ( setf i0 ( random-individual ) )
    ( display i0 )
    ( setf one ( rbg-string ) )
    ( setf i1 ( new-individual 1 one ) )
    ( display i1 )
    ( setf two ( rbg-string ) )
    ( setf i2 ( new-individual 2 two ) )
    ( display i2 )
    ( setf three ( rbg-string ) )
    ( setf i3 ( new-individual 3 three ) )
    ( display i3 )
    ( format t "Fitness of i0 = ~A~%" ( funcall *fitness* i0 ) )
    ( format t "Fitness of i1 = ~A~%" ( funcall *fitness* i1 ) )
    ( format t "Fitness of i2 = ~A~%" ( funcall *fitness* i2 ) )
    ( format t "Fitness of i3 = ~A~%" ( funcall *fitness* i3 ) )
    nil
)

;--------------------
; Population class

( defconstant *population-size* 100 )
( defconstant *selection-size* 8 )
( setf *fitness* #'fitness-b)

( defclass population ()
    (
        ( individuals :accessor population-individuals :initarg :individuals )
        ( generation :accessor population-generation :initform 0 )
    )
)

( defmethod i-size ( ( p population ) )
    ( length ( population-individuals p ) )
)

( defmethod display ( ( p population ) )
    ( terpri ) ( terpri )
    ( princ "Generation " )
    ( prin1 ( population-generation p ) )
    ( princ " population ..." )
    ( terpri ) ( terpri )

    ( dolist ( i ( population-individuals p ) )
        ( display i )
    )

    ( terpri )
)

( defmethod initial-population ( &aux individuals )
    ( setf individuals () )
    ( dotimes ( i *population-size* ) 
        ( push ( new-individual ( + i 1 ) ( rbg-string ) ) individuals )
    )
    ( make-instance 'population :individuals ( reverse individuals ) )
)

( defmethod average ( ( p population ) &aux ( sum 0 ) ) 
    ( setf sum 0 )
    ( dolist ( i ( population-individuals p ) )
        ( display i )
        ( setf sum ( + sum ( individual-fitness i ) ) )
    )
    ( float ( / sum ( i-size p ) ) )
)

( setf *select-demo* nil )

( defmethod select-individual ( ( p population ) &aux i candidates rn )
    ( setf candidates ( select-individuals p ) )
    ( setf mfi ( most-fit-individual candidates ) )
    ( if *select-demo* ( select-demo-helper candidates mfi ) )
    mfi
)

( defmethod select-individuals ( ( p population ) &aux individuals candidates rn ) 
    ( setf individuals ( population-individuals p ) )
    ( setf candidates () )
    ( dotimes ( i *selection-size* )
        ( setf rn ( random *population-size* ) )
        ( push ( nth rn individuals ) candidates )
    )
    candidates
)

( defmethod most-fit-individual ( ( l list ) &aux max-value max-individual )
    ; ( setf max-individual ( car list ) )
    ; ( setf max-value ( ))
)

( defmethod select-demo-helper ( ( l list ) ( i individual ) )
    ( princ "The sample of individuals..." )
    ( mapcar #'display l )
    ( terpri )
    ( princ "The most fit of the sample ..." ) ( terpri )
    ( display i )
    ( terpri )
    nil 
)

( defmethod population-demo ( &aux p )
    ( setf p ( initial-population ) )
    ( display p )
    ( format t "Average fitness = ~A~%~%" ( average p ) )
    ( setf *select-demo* t )

    ( format t "Sampling...~%~%" )
    ( select-individual p ) ( terpri )

    ( format t "Sampling...~%~%" )
    ( select-individual p ) ( terpri )

    ( format t "Sampling...~%~%" )
    ( select-individual p ) ( terpri )
)