
; File: distance_between_two_points.l 
; ------
; Problem:
; Define a function to compute the distance between
; two points which is consistent with the following specifications:
;   - The name of the function will be distance.
;   - It will receive four parameters, called x1 and y1 (which collectively constitute point 1)
;     and x2 and y2 (which collectively constitute point 2) – in that order, from left to right.
;   - The function returns the distance between (x1,y1) and (x2,y2)
;
; Referenced this for the distance formula: https://www.calculator.net/distance-calculator.html
; -----

( defun distance (x1 y1 x2 y2 &aux result)
  ( setf result 
    ( sqrt 
        ( + 
            ( expt ( - x2 x1 ) 2 ) 
            ( expt ( - y2 y1 ) 2 ) 
        ) 
    ) 
  )

  result
)