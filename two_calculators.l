; File: two_calculators.l
; ------
; Problem 1: Define a function to simulate a calculator that is consistent
; with the following specifications:
;   - The name of the function will be calculator-LR.
;   - The function takes no parameters and returns no values.
;   - The function repeatedly reads and evaluates a 
;     parenthesis-free infix expression, from left
;     to right, of the form operand operator operand operator 
;     operand, and prints the result.
;   - The calculator will evaluate the leftmost operator prior to 
;     evaluating the rightmost operator, which is what is meant by 
;     “from left to right”. (Thus, for example, 1 + 2 * 3 would
;     be interpreted as ( ( 1 + 2 ) * 3 ).)
;   - The function makes good use of the apply function.
;   - The calculator need only handle the four basic arithmetic 
;     operators and numeric operands
; ------

; Code modified from Professor Graci's lecture Code

( defun calculator-LR (&aux input operator1 operator2 operands result1 result2)
    ( format t "Expression? " )
    ( setf input ( list (read) (read) (read) (read) (read) ) )

    ( setf operator1 ( second input ) )
    ( setf operator2 ( nth 3 input ) )

    ( setf operands ( list ( first input ) ( third input ) ) )
    ( setf result1 ( apply operator1 operands ) )

    ( setf operands ( list result1 ( nth 4 input ) ) )
    ( setf result2 ( apply operator2 operands ) )

    ( format t "Result: ~A~%" result2)
)


; Problem 2: Define a function to simulate a calculator that is 
; consistent with the following specifications:
;   - The name of the function will be calculator-RL.
;   - The function takes no parameters and returns no values.
;   - The function repeatedly reads and evaluates a 
;     parenthesis-free infix expression, from right
;     to left, of the form operand operator operand 
;     operator operand, and prints the result.
;   - The calculator will evaluate the rightmost operator prior 
;     to evaluating the leftmost operator, which is what is 
;     meant by “from right to left”. (Thus, for example, 1 + 2 * 3 
;     would be interpreted as ( 1 + ( 2 * 3 )).)
;   - The function makes good use of the funcall function.
;   - The calculator need only handle the four basic 
;     arithmetic operators and numeric operands.
; ------

; Code modified from Professor Graci's lecture Code

( defun calculator-RL (&aux input operator1 operator2 operand1 operand2 operand3 result1 result2)
    ( format t "Expression? " )
    ( setf input ( list (read) (read) (read) (read) (read) ) )

    ( setf operator1 ( nth 3 input ) )
    ( setf operator2 ( second input ) )

    ( setf operand1 ( third input ) )
    ( setf operand2 ( nth 4 input ) )
    ( setf operand3 ( first input ) )

    ( setf result1 ( funcall operator1 operand1 operand2 ) )
    ( setf result2 ( funcall operator2 result1 operand3 ) )

    ( format t "Result: ~A~%" result2)
)