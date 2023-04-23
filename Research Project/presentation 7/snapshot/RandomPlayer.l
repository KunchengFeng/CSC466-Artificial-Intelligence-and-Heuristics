; File: RandomPlayer.l

(defclass randomPlayer()
    (
        (name :accessor player-name :initform 'RandomPlayer)
        (thisBoard :accessor player-board :initarg :thisBoard)
        (otherBoard :accessor player-otherBoard :initarg :otherBoard)
        (ships :accessor player-ships :initarg :ships)
    )
)


; Constructor -----------------------------------------------------------------
(defmethod newRandomPlayer((this board) (other board) (ships list))
    (make-instance 'randomPlayer 
        :thisBoard this
        :otherBoard other
        :ships ships
    )
)
; -----------------------------------------------------------------------------


; Ask the random player to place ships on the board ---------------------------
; Note, the board should be at least 5 x 5 in order for all ships to be placed.
; Loop through the ships this player have, placing them one at a time.
(defmethod playerPlaceShips((player randomPlayer) &aux board ships)
    (setf ships (player-ships player))
    (setf board (player-board player))

    ; Loop through the ships that have to be placed
    (loop for ship in ships do
        (randomlyPlaceShip ship board)
    )
)
; randomlyPlaceShip method can be found in "Ship.l"
; -----------------------------------------------------------------------------


; Player takes a shot at a random location ------------------------------------
(defmethod playerOpenFire((p randomPlayer) &aux enemyBoard x y)
    (setf enemyBoard (player-otherBoard p))
    (setf x (random (board-width enemyBoard)))
    (setf y (random (length (board-rows enemyBoard))))
    (fireAtBoard x y enemyBoard)
)
; -----------------------------------------------------------------------------


; Returns true if all ships of this player has been sunk ----------------------
(defmethod isPlayerDefeated((p randomPlayer) &aux ships result sunk)
    (setf ships (player-ships p))
    (setf result t)

    (loop for ship in ships do
        (setf sunk (isShipSunk ship))
        (setf result (and result sunk))
    )

    result
)
; ----------------------------------------------------------------------------------