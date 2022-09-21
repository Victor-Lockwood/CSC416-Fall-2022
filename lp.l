; File: lp.lp
; Code from Professor Graci's lecture notes

; Returns T if the list contains exactly one element, NIL otherwise
( defun singleton-p ( l )
    ( cond
        ( ( null l ) nil )
        ( ( null ( cdr l ) ) t )
        ( t nil )
    )
)

; Returns the last element of a nonempty list
( defun rac ( l )
    ( cond
        ( ( singleton-p l )
            ( car l )
        )
        ( t
            ( rac ( cdr l ) )
        )
    )
)

; Returns the list with the last element removed
( defun rdc ( l )
    ( cond
        ( ( singleton-p l )
            ()
        )
        ( t
            ( cons ( car l ) ( rdc ( cdr l ) ) )
        )
    )
)

; Places the given object at the end of a given list
( defun snoc ( o l )
    ( cond
        ( ( null l )
            ( list o )
        )
        ( t
            ( cons ( car l ) ( snoc o ( cdr l ) ) )
        )
    )
)

; Returns T if the list reads the same forwards and backwards at 
; the top level of conception, NIL otherwise
( defun palindrome-p ( l )
    ( cond
        ( ( null l )
            t
        )
        ( ( singleton-p l )
            t
        )
        ( ( equal ( car l ) ( rac l ) )
            ( palindrome-p ( cdr ( rdc l ) ) )
        )
        ( t
            nil
        )
    )
)

; Returns the sum of a list of numbers
( defun sum ( l )
    ( cond
        ( ( null l )
            0
        )
        ( t
            ( + ( car l ) ( sum ( cdr l ) ) )
        )
    )
)

; Returns the list of the integers from 1 to n
( defun iota ( n )
    ( cond
        ( ( = n 0 )
            ()
        )
        ( t
            ( snoc n ( iota ( - n 1 ) ) )
        )
    )
)