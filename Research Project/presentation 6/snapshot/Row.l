; File: Row.l
(defclass row()
	(
		(number :accessor row-number :initarg :number :initform 0)
		(cells :accessor row-cells :initarg :cells :initform nil)
	)
)


; Constructor --------------------------------------------------------------
(defmethod newRow(width number &aux cells)
	(setf cells (list))
	(dotimes (cellNum width)
		(setf cells (cons (newCell number cellNum) cells))
	)
	(setf cells (reverse cells))
	(make-instance 'row
		:number number
		:cells cells
	)
)
; ---------------------------------------------------------------------------


; Getter ----------------------------------------------------------------------
(defmethod getCellFromRow(x (r row))
	(nth x (row-cells r))
)
; -----------------------------------------------------------------------------


; Fire methods ----------------------------------------------------------------
(defmethod fireAtRow(x (r row))
	(fireAtCell (nth x (row-cells r)))
)

(defmethod checkHitRow(x (r row) &aux cells)
	(setf cells (row-cells r))
	(isCellHit (nth x cells))
)
; -----------------------------------------------------------------------------


; Display methods ------------------------------------------------------
; Display the top line, called in Board.l
(defun displayTop(boardWidth)
	(format t "   ")
	(dotimes (n boardWidth)
		(format t "  ~A " (cellToLetter n))
	)
	(format t " ~%")
)

(setf *letters* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun cellToLetter(cellNumber)
	(nth cellNumber *letters*)
)

(defun letterToCell(cellLetter)
	(position cellLetter *letters*)
)

; Print a new formated line
(defun newLine(cellLength)
	(format t "   ")
	(dotimes (n cellLength)
		(format t "+---")
	)
	(format t "+ ~%")
)

; The main display method
(defmethod display((r row) &aux cells rowLength rowNumber)
	(setf cells (row-cells r))
	(setf rowLength (length cells))

	; +---+---+---+
	(newLine rowLength)

	; Numbers on the left
	(setf rowNumber (row-number r))
	(if (< 9 rowNumber)
		(format t " ~A" rowNumber)
		(format t " ~A " rowNumber)
	)

	; |   |   |
	(loop for cell in cells do
		(display cell)
	)
	(format t "| ~%")
)

; This method display the firing marks
(defmethod displayMarks((r row) &aux cells rowLength rowNumber)
	(setf cells (row-cells r))
	(setf rowLength (length cells))

	; +---+---+---+
	(newLine rowLength)

	; Numbers on the left
	(setf rowNumber (row-number r))
	(if (< 9 rowNumber)
		(format t " ~A" rowNumber)
		(format t " ~A " rowNumber)
	)

	; |   |   |
	(loop for cell in cells do
		(displayMarks cell)
	)
	(format t "| ~%")
)

; The new and improved display methods,
; This method will display both the cell and the marks on the same line.
; The left board will display the marks of player's fire record.
; The right board will display the player's ship and enemy's fire record.
(defun displayBothTop(rowLength)
	; Left board
	(format t "   ")
	(dotimes (n rowLength)
		(format t "  ~A " (cellToLetter n))
	)
	(format t " ")

	(middleBuffer)

	; Right board
	(format t "   ")
	(dotimes (n rowLength)
		(format t "  ~A " (cellToLetter n))
	)
	(format t " ~%")
)

(defun displayBothNewLine(rowLength)
	; Left board
	(format t "   ")
	(dotimes (n rowLength)
		(format t "+---")
	)
	(format t "+")

	(middleBuffer)

	; Right board
	(format t "   ")
	(dotimes (n rowLength)
		(format t "+---")
	)
	(format t "+~%")
)

(defmethod displayBoth((this row) (other row) &aux thisCells otherCells rowLength rowNumber)
	; Both rows should be of equal length.
	(setf thisCells (row-cells this))
	(setf otherCells (row-cells other))
	(setf rowLength (length thisCells))
	(setf rowNumber (row-number this))

	; Display a new line
	(displayBothNewLine rowLength)

	; Display the left board (marks)
	(if (< 9 rowNumber)
		(format t " ~A" rowNumber)
		(format t " ~A " rowNumber)
	)
	(loop for cell in otherCells do 
		(displayMarks cell)
	)
	(format t "|")

	(middleBuffer)

	; Display the right board
	(if (< 9 rowNumber)
		(format t " ~A" rowNumber)
		(format t " ~A " rowNumber)
	)
	(loop for cell in thisCells do
		(display cell)
	)
	(format t "|~%")
)

(defun middleBuffer()
	; Middle buffer space (4x space)
	(format t "    ")
)
; ---------------------------------------------------------------------