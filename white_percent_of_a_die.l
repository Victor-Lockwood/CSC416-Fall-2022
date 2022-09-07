; ------------------------------------
; File: white_percent_of_a_die.l
; ------------------------------------
; Problem:
; Suppose a standard die measures 3.25cm on the edge of a face. Further,
; suppose that each dot on the die has a diameter of length one-fifth
; the edge of a face. What percent of the surface area of the die is
; white?
; ------------------------------------
; Solution

( setf die-face-edge-length 3.25 )
( setf die-face-area ( expt die-face-edge-length 2 ) )
( setf total-number-of-faces 6 )
( setf total-face-area ( * total-number-of-faces die-face-area ))

( setf dot-diameter ( / die-face-edge-length 5 ) )
( setf dot-radius ( / dot-diameter 2 ) )
( setf dot-area ( * pi ( expt dot-radius 2) ) )
( setf number-of-dots ( + 1 2 3 4 5 6) )
( setf total-dot-area ( * dot-area number-of-dots ) )

( setf total-white-area ( - total-face-area total-dot-area ) )

