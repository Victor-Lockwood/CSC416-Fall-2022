( defun replace-lcr ( location element list )
    ( cond
        ( ( > ( length list ) 3)
            nil
        )
        ( ( equal location 'left )
            ( setf ( car list ) element )
            list
        )
        ( ( equal location 'center )
            ( setf ( car ( cdr list ) ) element )
            list
        )
        ( ( equal location 'right )
            ( setf ( car ( cddr list ) ) element )
            list
        )
        ( t 
            nil
        )
    )
)

( defun uniform-p ( l )
    ( cond 
        ( ( null l ) 
            t 
        )
        ( ( = ( length l ) 1 )
            t
        )
        ( ( equal ( car l ) ( cadr l ) ) 
            ( uniform-p ( cdr l ) )
        )
        ( t
            nil
        )
    )
)

( defun flush-p ( l )
    ( setf check-list ( mapcar #'cdr l ) )
    ( uniform-p check-list )
)