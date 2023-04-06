; File RandomPlayerPlusPlus.l
; This player improves from RandomPlayerPlus, 
; it will infer a ship's positioning after consecutive hits, 
; it will also clear the adjacent locations from the preferred list after the ship has been sunk.
(defclass randomPlayerPlusPlus()
    (
        (name :accessor player-name :initform 'RandomPlayerPlusPlus)
        (thisBoard :accessor player-board :initarg :thisBoard)
        (otherBoard :accessor player-otherBoard :initarg :otherBoard)
        (ships :accessor player-ships :initarg :ships)
        (unexplored :accessor player-unexplored :initarg :unexplored)
        (explored :accessor player-explored :initform nil)
        (preferred :accessor player-preferred :initform nil)
    )
)


; Constructor -----------------------------------------------------------------------------------------------------
(defmethod newRandomPlayerPlusPlus((this board) (other board) (ships list) &aux width height unexplored)
    (setf width (board-width this))
    (setf height (length (board-rows this)))
    (setf unexplored (generateAllLocations width height))
    (make-instance 'randomPlayerPlusPlus
        :thisBoard this
        :otherBoard other
        :ships ships
        :unexplored unexplored
    )
)
; --------------------------------------------------------------------------------------------------------------------


; Placing ships ---------------------------------------------------------------------------------------------
; This player still place ships randomly
(defmethod playerPlaceShips((player randomPlayerPlusPlus) &aux board ships)
    (setf ships (player-ships player))
    (setf board (player-board player))

    ; Loop through the ships that have to be placed
    (loop for ship in ships do
        (randomlyPlaceShip ship board)
    )
)
; randomlyPlaceShip method can be found in "Ship.l"
; ----------------------------------------------------------------------------------------------------------------


; Player open fire -----------------------------------------------------------------------------------------------
; The difference is in the list modification after the shot has been fired.
(defmethod playerOpenFire((p randomPlayerPlusPlus) &aux location hit)
    (setf location (getNextLocation p))
    (setf hit (fireAtLocation location (player-otherBoard p)))
    (modifyLists location hit p)
)

; Same as randomPlayerPlus
(defmethod getNextLocation((p randomPlayerPlusPlus) &aux unexplored preferred location)
    ; Gather the player's information
    (setf unexplored (player-unexplored p))
    (setf preferred (player-preferred p))

    (setf location nil)
    (if (equal preferred nil)
        (setf location (randomFromList unexplored))
        (setf location (randomFromList preferred))
    )

    location
)

; Main difference here ↓
; This player will infer the ship placement after consecutive hits;
; Player will also remove adjacent cells from preferred list after sinking the ship
(defmethod modifyLists((l location) hit (p randomPlayerPlusPlus) &aux unexplored explored preferred board adjacent)
    ; Get the player's lists in place
    (setf unexplored (player-unexplored p))
    (setf explored (player-explored p))
    (setf preferred (player-preferred p))

    ; Remove the current location from the (unexplored / preferred) list, and add it to explored
    (setf unexplored (remove l unexplored))
    (setf preferred (remove l preferred))
    (setf explored (cons l explored))

    ; Note:
    ;   only from the explored list elements will be checked for consecutive hits.
    ;   only the unexplored list elements will be moved to preferred list.
    ;   the ? mark represent the location that will be moved to the preferred list if not nil.
    (setf board (player-otherBoard p))
    (cond
        ; | x | x | ? |
        ((and (isLocationHit (getLeftAdjacent l explored) board) hit)
            (setf adjacent (getRightAdjacent l unexplored))
        )
        ; | ? | x | x |
        ((and (isLocationHit (getRightAdjacent l explored) board) hit)
            (setf adjacent (getLeftAdjacent l unexplored))
        )
        ; x
        ; x
        ; ?
        ((and (isLocationHit (getAboveAdjacent l explored) board) hit)
            (setf adjacent (getBelowAdjacent l unexplored))
        )
        ; ?
        ; x
        ; x
        ((and (isLocationHit (getBelowAdjacent l explored) board) hit)
            (setf adjacent (getAboveAdjacent l unexplored))
        )
        ; |   | ? |   |
        ; | ? | x | ? |
        ; |   | ? |   |
        (hit
            (setf adjacent nil)
            (setf preferred (append (getAdjacents l unexplored) preferred))
        )
    )
    (if (not (equal adjacent nil))
        (setf preferred (cons adjacent preferred))
    )

    ; Write back the modified values
    (setf (player-unexplored p) unexplored)
    (setf (player-explored p) explored)
    (setf (player-preferred p) preferred)

    nil
)
; ---------------------------------------------------------------------------------------------------------------


; End state ---------------------------------------------------------------------
(defmethod isPlayerDefeated((p randomPlayerPlusPlus) &aux ships result sunk)
    (setf ships (player-ships p))
    (setf result t)

    (loop for ship in ships do
        (setf sunk (isShipSunk ship))
        (setf result (and result sunk))
    )

    result
)
; -------------------------------------------------------------------------------