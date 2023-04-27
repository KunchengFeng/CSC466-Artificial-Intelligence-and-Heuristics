; File: RandomPlayer.l

(defclass randomPlayer()
    (
        (name :accessor player-name :initform 'RandomPlayer)
        (thisBoard :accessor player-board :initarg :thisBoard)
        (otherBoard :accessor player-otherBoard :initarg :otherBoard)
        (ships :accessor player-ships :initarg :ships)
        (unexplored :accessor player-unexplored :initarg :unexplored)
        (explored :accessor player-explored :initform nil)
    )
)


; Constructor -----------------------------------------------------------------
(defmethod newRandomPlayer((this board) (other board) (ships list) &aux locations)
    (setf locations (generateAllLocations (board-width this) (length (board-rows this))))
    (make-instance 'randomPlayer 
        :thisBoard this
        :otherBoard other
        :ships ships
        :unexplored locations
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
(defmethod playerOpenFire((p randomPlayer) &aux enemyBoard allL nextL)
    ; Pick a location
    (setf enemyBoard (player-otherBoard p))
    (setf allL (player-unexplored p))
    (setf nextL (nth (random (length allL)) allL))
    
    ; Fire at it
    (fireAtLocation nextL enemyBoard)

    ; Move to explored list
    (setf (player-explored p) (cons nextL (player-explored p)))
    ; Update unexplored list
    (setf (player-unexplored p) (remove nextL (player-unexplored p)))
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