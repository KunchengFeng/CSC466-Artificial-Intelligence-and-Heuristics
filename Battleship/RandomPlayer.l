; File: RandomPlayer.l

(defclass randomPlayer()
    (
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