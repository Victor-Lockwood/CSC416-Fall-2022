; File: perimeter_of_triangle.l 
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
;
; Problem 2:
; Define a function, which makes good use of the
; distance function, to compute the perimeter of a 
; triangle and which is consistent with the
; following specifications:
;   - The name of the function will be perimeter.
;   - It will receive six parameters called x1 and y1 (point 1), 
;     and x2 and y2 (point 2) and x3 and y3 (point 3) – 
;     in that order, from left to right.
;   - The function returns the perimeter of the triangle 
;     defined by the three points.
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

( defun perimeter (x1 y1 x2 y2 x3 y3 &aux distance1 distance2 distance3 result)
    ( setf distance1 ( distance x1 y1 x2 y2 ) )
    ( setf distance2 ( distance x1 y1 x3 y3 ) )
    ( setf distance3 ( distance x2 y2 x3 y3 ) )
    ( setf result ( + distance1 distance2 distance3 ) )
    result
)