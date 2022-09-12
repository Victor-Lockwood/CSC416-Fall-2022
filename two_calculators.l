; File: two_calculators.l
; ------
; Problem: Define a function to simulate a calculator that is consistent
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