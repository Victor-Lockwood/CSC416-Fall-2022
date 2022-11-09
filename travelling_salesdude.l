( load "lp.l" )

( setf ( symbol-plist 'a ) '(b 900 c 600 d 700 e 600) )
( setf ( symbol-plist 'b ) '(a 900 c 800 d 1100 e 1700 ) )
( setf ( symbol-plist 'c ) '(a 600 b 800 d 700 e 1500 ) )
( setf ( symbol-plist 'd ) '(a 700 b 1100 c 700 e 600 ) )
( setf ( symbol-plist 'e ) '(d 600 c 1500 b 1700 a 600 ) )

( setf *cities* '( a b c d e ) )

; S is start city
( defun solve ( s &aux champion challenger )
    ( setf champion ( cons s ( snoc s ( remove s *cities* ) ) ) )
    ( format t "Initial champion: ~A ~A ~%" champion ( distance champion ) )
    ( dotimes ( i 25 )
        ( setf challenger ( modify-tour champion ) )
        ( format t "Challenger: ~A ~A ~%" challenger ( distance challenger ) )
        ( if ( < ( distance challenger ) ( distance champion ) )
            ( setf champion challenger )
        )
        ( format t "Current champion: ~A ~A ~%" champion ( distance champion ) )
    )
)

( defun distance ( tour ) 
    ( setf leg1 ( get ( nth 0 tour ) ( nth 1 tour ) ) )
    ( setf leg2 ( get ( nth 1 tour ) ( nth 2 tour ) ) )
    ( setf leg3 ( get ( nth 2 tour ) ( nth 3 tour ) ) )
    ( setf leg4 ( get ( nth 3 tour ) ( nth 4 tour ) ) )
    ( setf leg5 ( get ( nth 4 tour ) ( nth 5 tour ) ) )

    ( setf distance ( + leg1 leg2 leg3 leg4 leg5 ) )
    distance
)


( defun modify-tour ( tour &aux city1 city2 )
    ( setf point1 ( + 1 ( random ( - ( length tour ) 2 ) ) ) )
    ( setf point2 ( + 1 ( random ( - ( length tour ) 2 ) ) ) )
    ( if ( = point1 point2 ) ( modify-tour tour ) )
    ( setf tour ( copy-tree tour ) )
    ( setf city1 ( nth point1 tour ) )
    ( setf city2 ( nth point2 tour ) )
    ( setf ( nth point1 tour ) city2 )
    ( setf ( nth point2 tour ) city1 )
    tour
)