; File Ship.l
(defclass ship()
	(
		(type :accessor ship-type :initarg :type)
		(cells :accessor ship-cells :initform nil)
	)
)

(setf *shipTypes* '(carrier battleship cruiser submarine destroyer))
(setf (symbol-plist '*shipRep*) '(carrier 5 battleship 4 cruiser 3 submarine 2 destroyer 1))
(setf (symbol-plist '*shipSize*) '(carrier 5 battleship 4 cruiser 3 submarine 3 destroyer 2))


; Constructor class ----------------------------------------------------------------------------
(defmethod newShip(type)
	(cond
		((not (equal (member type *shipTypes*) nil))
			(make-instance 'ship :type type)
		)
		(t
			(format t "Ship type not supported.~%")
			nil
		)
	)
)
; ----------------------------------------------------------------------------------------------


; Display the ship, only a number representation is needed --------------------------------------
(defmethod display((s ship) &aux rep)
	(setf rep (get '*shipRep* (ship-type s)))
	(format t "| ~A " rep)
)
; -----------------------------------------------------------------------------------------------


; For development purposes
(defmethod getInfo((s ship))
	(format t "Ship type: ~A ~%" (ship-type s))
	(format t "Ship cells: ~A ~%" (ship-cells s))
)


; Checking for valid ship placement -------------------------------------------------------------
; The ship position should be checked valid before placement
(defmethod checkValid(x1 y1 x2 y2 (s ship) (b board) &aux shipType result)
	(setf shipType (ship-type s))
	(setf result t)
	(cond
		((not (checkType shipType))
			(format t "Error: Incorrect ship type.~%")
			(setf result nil)
		)
		((not (checkSize shipType x1 y1 x2 y2))
			(format t "Error: Incorrect size.~%")
			(setf result nil)
		)
		; This method is in Board.l, it returns true if the position is in the board.
		((not (checkBorder x1 y1 b))
			(format t "Error: Position 1 out of bound.~%")
			(setf result nil)
		)
		((not (checkBorder x2 y2 b))
			(format t "Error: Position 2 out of bound.~%")
			(setf result nil)
		)
		((not (checkDiagonal x1 y1 x2 y2))
			(format t "Error: Ship needs to be either vertical or horizontal.~%")
			(setf result nil)
		)
		((not (checkResidents x1 y1 x2 y2 b))
			(format t "Error: Cells already occupied.~%")
			(setf result nil)
		)
	)
	result
)

; This one is for non-players
(defmethod checkValidNoText(x1 y1 x2 y2 (s ship) (b board) &aux shipType result)
(setf shipType (ship-type s))
	(setf result t)
	(cond
		((not (checkType shipType))
			(setf result nil)
		)
		((not (checkSize shipType x1 y1 x2 y2))
			(setf result nil)
		)
		((not (checkBorder x1 y1 b))
			(setf result nil)
		)
		((not (checkBorder x2 y2 b))
			(setf result nil)
		)
		((not (checkDiagonal x1 y1 x2 y2))
			(setf result nil)
		)
		((not (checkResidents x1 y1 x2 y2 b))
			(setf result nil)
		)
	)
	result
)


(defmethod checkType(shipType)
	(not (equal (member shipType *shipTypes*) nil))
)

; Note: (0 1 2 3 4) would count as size 5
(defmethod checkSize(type x1 y1 x2 y2 &aux shipSize horSize verSize)
	(setf shipSize (get '*shipSize* type))
	(setf horSize (+ (abs (- x1 x2)) 1))
	(setf verSize (+ (abs (- y1 y2)) 1))
	(or
		(= shipSize horSize)
		(= shipSize verSize)
	)
)

; A ship cannot be placed diagonally
(defmethod checkDiagonal(x1 y1 x2 y2)
	(or
		(and
			(= x1 x2)
			(not (= y1 y2))
		)
		(and
			(not (= x1 x2))
			(= y1 y2)
		)
	)
)

; All the cells that the ship is going to occupy have to be empty
; The methods uses here are defined down below.
(defmethod checkResidents(x1 y1 x2 y2 (b board) &aux cells result)
	(if (= x1 x2)
		(setf cells (sameXCells x1 y1 y2 b))
		(setf cells (sameYCells x1 x2 y1 b))
	)
	(setf result t)
	(loop for cell in cells do
		(setf result (and result (checkCell cell)))
	)
	result
)

; Check if the cell is empty
(defmethod checkCell((c cell))
	(equal (cell-resident c) nil)
)
; ----------------------------------------------------------------------------------------------------


; Placing ships --------------------------------------------------------------------------------------

; Note, ship is placed with this function
; Ship position should be checked before this function is called!
(defmethod placeShip(x1 y1 x2 y2 (s ship) (b board) &aux cells)
	(if (= x1 x2)
		(setf cells (sameXCells x1 y1 y2 b))
		(setf cells (sameYCells x1 x2 y1 b))
	)
	(setResidents cells s)
	(setShipCells s cells)
)


; Mark the resident at the given cells
(defmethod setResidents(cells (s ship))
	(loop for cell in cells do
		(setCellResident cell s)
	)
)

; Associate this ship with its cells
(defmethod setShipCells((s ship) cells)
	(setf (ship-cells s) cells)
)

; Return all the cells between two Ys.
(defmethod sameXCells(x y1 y2 (b board))
	(cond
		((= y1 y2)
			(list (getCell x y1 b))
		)
		((< y1 y2)	; Up to down
			(cons (getCell x y1 b) (sameXCells x (+ y1 1) y2 b))
		)
		(t 			; Down to up
			(cons (getCell x y2 b) (sameXCells x y1 (+ y2 1) b))
		)
	)
)

; Return all the cells between two Xs.
(defmethod sameYCells(x1 x2 y (b board))
	(cond
		((= x1 x2)
			(list (getCell x1 y b))
		)
		((< x1 x2)	; Left to right
			(cons (getCell x1 y b) (sameYCells (+ x1 1) x2 y b))
		)
		(t 			; Right to left
			(cons (getCell x2 y b) (sameYCells x1 (+ x2 1) y b))
		)
	)
)
; ---------------------------------------------------------------------------------


; Check if a ship has been sunk, returns true if it is -----------------------------
(defmethod isShipSunk((s ship) &aux cells result hit)
	(setf cells (ship-cells s))
	(setf result t)

	; Loop through each cell and see if it had been fired at.
	(loop for cell in cells do
		(setf hit (cell-explored cell))
		(setf result (and result hit))
	)

	result
)
; ----------------------------------------------------------------------------------


; Randomly place a ship, this method is popular with a few random players ----------
(defmethod randomlyPlaceShip((s ship) (b board) &aux pos)
    (setf pos (getRandomPosition s b))
    (placeShip (first pos) (second pos) (third pos) (fourth pos) s b)
)

(defmethod getRandomPosition((s ship) (b board) &aux size maxX maxY x1 y1 x2 y2)
    ; Note! Ship positioning is inclusive, the -1 is needed here!
    (setf size (- (get '*shipSize* (ship-type s)) 1))

    ; Ship is either placed (left to right) or (small Y to big Y).
    (setf maxX (- (board-width b) size))
    (setf maxY (- (length (board-rows b)) size))

    (loop
        (setf x1 (random maxX))
        (setf y1 (random maxY))
    
        ; Roll a number of either 0 or 1;
        ; 0 will result in horizontal placement,
        ; 1 will result in vertical placement.
        (cond
            ((= (random 2) 0)
                (setf x2 (+ x1 size))
                (setf y2 y1)
            )
            (t
                (setf x2 x1)
                (setf y2 (+ y1 size))
            )
        )

        (when
            (equal (checkValidNoText x1 y1 x2 y2 s b) t)
            (return (list x1 y1 x2 y2))
        )
    )
)
; ----------------------------------------------------------------------------------