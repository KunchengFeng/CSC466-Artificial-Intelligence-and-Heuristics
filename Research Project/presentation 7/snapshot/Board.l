; File: Board.l
(defclass board()
	(
		(rows :accessor board-rows :initarg :rows)
		(width :accessor board-width :initarg :width)
		(recent :accessor board-recent :initform nil)
	)
)


; Constructor -----------------------------------------------------------
; The board can support up to 26 width, due to user experience reasons.
(defmethod newBoard(width height &aux rows)
	(setf rows (list))
	(dotimes (rowNum height)
		(setf rows (cons (newRow width rowNum) rows))
	)
	(setf rows (reverse rows))
	(make-instance 'board
		:rows rows
		:width width
	)
)
; ------------------------------------------------------------------------


; Get a specified cell ---------------------------------------------------
(defmethod getCell(x y (b board))
	(getCellFromRow x (nth y (board-rows b)))
)
; ------------------------------------------------------------------------


; Fire methods  ------------------------------------------------------------
(defmethod fireAtBoard(x y (b board) &aux rows)
	(setf rows (board-rows b))
	(fireAtRow x (nth y rows))
	(setf (board-recent b) (list x y))
)
; ---------------------------------------------------------------------------


; checkers ------------------------------------------------------------------
; (X Y)
;     Y   Y
; X |0 0|0 1|
; X |1 0|1 1|
(defmethod checkBorder(x y (b board))
	(and 
		(>= x 0)
		(< x (length (board-rows b)))
		(>= y 0)
		(< y (board-width b))
	)
)

(defmethod checkHit(x y (b board) &aux rows)
	(setf rows (board-rows b))
	(checkHitRow x (nth y rows))
)
; --------------------------------------------------------------------------


; Display the board --------------------------------------------------------
(defmethod display((b board) &aux width rows)
	(setf width (board-width b))
	(setf rows (board-rows b))
	(displayTop width)
	(loop for row in rows do
		(display row)
	)
	(newLine width)
)

(defmethod displayMarks((b board) &aux width rows)
	(setf width (board-width b))
	(setf rows (board-rows b))
	(displayTop width)
	(loop for row in rows do
		(displayMarks row)
	)
	(newLine width)
)

(defmethod displayBoth((this board) (other board) &aux width thisRows otherRows)
	(setf width (board-width this))
	(setf thisRows (board-rows this))
	(setf otherRows (board-rows other))

	(displayBothTop width)
	(dotimes (n (length thisRows))
		(displayBoth (nth n thisRows) (nth n otherRows))
	)
	(displayBothNewLine width)
)
; ---------------------------------------------------------------------------