; -----
; File: quarto.l
; -----
; Piece properties:
; SIZE (BIG or SMALL)
; COLOR (RED or BLUE)
; STYLE (HOLLOW or SOLID)
; SHAPE (CIRCLE or SQUARE
; -----

; Big red pieces
( setf ( symbol-plist 'BRHS ) '( size big color red style hollow shape square ) )
( setf ( symbol-plist 'BRHC ) '( size big color red style hollow shape circle ) )
( setf ( symbol-plist 'BRSS ) '( size big color red style solid shape square ) )
( setf ( symbol-plist 'BRSC ) '( size big color red style solid shape circle ) )

; Big blue pieces
( setf ( symbol-plist 'BBHS ) '( size big color blue style hollow shape square ) )
( setf ( symbol-plist 'BBHC ) '( size big color blue style hollow shape circle ) )
( setf ( symbol-plist 'BBSS ) '( size big color blue style solid shape square ) )
( setf ( symbol-plist 'BBSC ) '( size big color blue style solid shape circle ) )

; Small red pieces
( setf ( symbol-plist 'SRHS ) '( size small color red style hollow shape square ) )
( setf ( symbol-plist 'SRHC ) '( size small color red style hollow shape circle ) )
( setf ( symbol-plist 'SRSS ) '( size small color red style solid shape square ) )
( setf ( symbol-plist 'SRSC ) '( size small color red style solid shape circle ) )

; Small blue pieces
( setf ( symbol-plist 'SBHS ) '( size small color blue style hollow shape square ) )
( setf ( symbol-plist 'SBHC ) '( size small color blue style hollow shape circle ) )
( setf ( symbol-plist 'SBSS ) '( size small color blue style solid shape square ) )
( setf ( symbol-plist 'SBSC ) '( size small color blue style solid shape circle ) )

( setf *pieces* '( BRHS BRHC BRSS BRSC BBHS BBHC BBSS BBSC SRHS SRHC SRSS SRSC SBHS SBHC SBSS SBSC ) )

; Pick function from Professor Graci's lecture code
( defun pick (l) 
    ( nth ( random ( length l ) ) l ) 
)


( defun display-random-piece ( &aux selectedpiece result result2 )
    ( setf selectedpiece ( pick *pieces* ))

    ( setf result (list selectedpiece ) )
    ( setf result2 ( append result ( symbol-plist selectedpiece ) ) )
    ( format t "~A~%" result2)
)