; File: Cell.l
; Note: cell-resident is now a reference to a ship object
(defclass cell()
	(
		(resident :accessor cell-resident :initform nil)
		(explored :accessor cell-explored :initform nil)
		(cellRow :accessor cell-row :initarg :cellRow :initform 0)
		(cellNum :accessor cell-num :initarg :cellNum :initform 0)
	)
)

(defmethod newCell(rowNumber cellNumber)
	(make-instance 'cell
		:cellRow rowNumber
		:cellNum cellNumber
	)
)

(defmethod setCellResident((c cell) resident)
	(setf (cell-resident c) resident)
)

(defmethod fireAtCell((c cell))
	(setf (cell-explored c) t)
)

(defmethod isCellHit((c cell) &aux explored resident)
	(setf explored (cell-explored c))
	(setf resident (cell-resident c))
	(and
		explored
		(not (equal resident nil))
	)
)


; Display Methods -------------------------------------------------------------
; This method display the player's board, includes the ship if present.
; Based on feedback, it will now also display marks.
(defmethod display((c cell) &aux resident explored hit)
	(setf resident (cell-resident c))
	(setf explored (cell-explored c))
	(setf hit (isCellHit c))
	(cond
		; Cell has been explored but no hit
		((and explored (not hit))
			(format t "| o ")
		)
		; Cell has been explored and there is a ship
		((and explored hit)
			(format t "| x ")
		)
		; Cell is not explored but there is a ship
		((and (not explored) (not (equal resident nil)))
			(display resident)
		)
		; Cell is both unexplored and have no ship
		(t
			(format t "|   ")
		)
	)
)

; This method display the marks that keep track of fired spots
; ' ' = not yet explored.
; 'x' = explored and hit.
; 'o' = explored but no hit.
(defmethod displayMarks((c cell) &aux explored hit)
	(setf explored (cell-explored c))
	(setf hit (isCellHit c))

	(cond
		((not explored)
			(format t "|   ")
		)
		((not hit)
			(format t "| o ")
		)
		(t
			(format t "| x ")
		)
	)
)
; -------------------------------------------------------------------------------

; This method is for development purposes
(defmethod getInfo((c cell))
	(format t "Cell row: ~A~%" (cell-row c))
	(format t "Cell number: ~A~%" (cell-num c))
	(format t "Cell resident: ~A~%" (cell-resident c))
	(format t "Cell explored: ~A~%" (cell-explored c))
)