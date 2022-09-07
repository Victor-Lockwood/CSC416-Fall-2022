; ------------------------------------
; File: tethered_goat.l
; ------------------------------------
; Problem:
; A goat is tethered to one corner of a barn that is isolated in a 
; huge grassy field. In whatever units, the barn measures 62 x 44, and 
; the rope is of length 88. What is the area of land on which the 
; goat gets to graze?
; ------------------------------------
; Solution

( setf barn-length 62 )
( setf barn-width 44 )
( setf rope-length 88 )
; rope length is like a radius here

( setf longside-leftover ( - 88 66 ) )
( setf shortside-leftover ( - 88 44 ) )

( setf rope-circle ( * pi ( expt rope-length 2) ) )
( setf quarter-rope-circle ( / rope-circle 4 ))
( setf traversable-rope-circle ( * 3 quarter-rope-circle ) )

( setf shortside-ropecircle ( * pi ( expt longside-leftover 2 ) ) )
( setf traversable-shortside ( / shortside-ropecircle 4 ) )

( setf longside-ropecircle ( * pi ( expt shortside-leftover 4 ) ) )
( setf traversable-longside ( / longside-ropecircle 4 ) )

( setf traversable-by-goat 
    ( + traversable-rope-circle 
        traversable-shortside 
        traversable-longside 
    ) 
)