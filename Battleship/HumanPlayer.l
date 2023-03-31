; File HumanPlayer.l

(defclass humanPlayer()
    (
        (name :accessor player-name :initform 'HumanPlayer)
        (thisBoard :accessor player-board :initarg :thisBoard)
        (otherBoard :accessor player-otherBoard :initarg :otherBoard)
        (ships :accessor player-ships :initarg :ships)
    )
)


; Constructor -------------------------------------------------------------------
(defmethod newHumanPlayer((this board) (other board) (ships list))
    (make-instance 'humanPlayer
        :thisBoard this
        :otherBoard other
        :ships ships
    )
)
; -------------------------------------------------------------------------------


; Ask the player to place ships on the board --------------------------------------
; Note since this is a human, texted feedback will be given
(defmethod playerPlaceShips((player humanPlayer) &aux board ships)
    (setf board (player-board player))
    (setf ships (player-ships player))

    ; Loop through each ship and have them placed.
    (loop for ship in ships do
        (humanPlaceShip ship board)
    )

    (format t "All ships have been placed.~%~%~%")
)

; Note that this method repeats until success.
(defmethod humanPlaceShip((s ship) (b board) &aux shipType x1 y1 x2 y2)
    (display b)
    (setf shipType (ship-type s))
    (format t "Now placing: ~A, size: ~A~%" shipType (get '*shipSize* shipType))
    (format t "Enter position 1: ")
	(setf x1 (read))
	(setf x1 (letterToCell x1))
	(setf y1 (read))
	(format t "Enter position 2: ")
	(setf x2 (read))
	(setf x2 (letterToCell x2))
	(setf y2 (read))

    (if (checkValid x1 y1 x2 y2 s b)
        (placeShip x1 y1 x2 y2 s b)
        (humanPlaceShip s b)
    )
)
; ------------------------------------------------------------------------------------


; Allow the player to take a shot at inputed position --------------------------------
(defmethod playerOpenFire((p humanPlayer) &aux myBoard enemyBoard pos cell ship)
    (setf myBoard (player-board p))
    (setf enemyBoard (player-otherBoard p))

    (format t "~%~%~%Your markers and your board: ~%")
    (displayBoth myBoard enemyBoard)

    ; Give a little feedback on where the enemy recently fired.
    (setf pos (board-recent myBoard))
    (if (not (equal pos nil))
        (format t "Enemy fired at ~A, ~A~%" (cellToLetter (first pos)) (second pos))
    )

    (setf pos (getPlayerInput enemyBoard))
    (fireAtBoard (first pos) (second pos) enemyBoard)

    ; Give a little feedback on the result of recent firing.
    (setf cell (getCell (first pos) (second pos) enemyBoard))
    (setf ship (cell-resident cell))
    (if (isCellHit cell)
        ; If a cell is hit, it have a residence.
        (if (isShipSunk ship)
            (format t "You sunk a ~A.~%" (ship-type ship))
            (format t "It's a hit!~%")
        )
        (format t "It missed.~%")
    )
)

(defmethod getPlayerInput((b board) &aux x y)
    (format t "Enter target location: ")
    (setf x (read))
    (setf x (letterToCell x))
    (setf y (read))
    
    (cond
        ((checkBorder x y b)
            (list x y)
        )
        (t
            (format t "Position out of bound.~%")
            (getPlayerInput b)
        )
    )
)
; -----------------------------------------------------------------------------------


; End condition checker --------------------------------------------------------------
(defmethod isPlayerDefeated((p humanPlayer) &aux ships result sunk)
    (setf ships (player-ships p))
    (setf result t)

    (loop for ship in ships do
        (setf sunk (isShipSunk ship))
        (setf result (and result sunk))
    )

    result
)
; ---------------------------------------------------------------------------------------