; File RandomPlayerPlus.l
; This player still plays at random, but remembers where it had fired, and infers a nearby ship after a hit.
(defclass randomPlayerPlus()
    (
        (name :accessor player-name :initform 'RandomPlayerPlus)
        (thisBoard :accessor player-board :initarg :thisBoard)
        (otherBoard :accessor player-otherBoard :initarg :otherBoard)
        (ships :accessor player-ships :initarg :ships)
        (unexplored :accessor player-unexplored :initarg :unexplored)
        (explored :accessor player-explored :initform nil)
        (preferred :accessor player-preferred :initform nil)
    )
)


; Constructor -----------------------------------------------------------------------------------------------
(defmethod newRandomPlayerPlus((this board) (other board) (ships list) &aux width height unexplored)
    (setf width (board-width this))
    (setf height (length (board-rows this)))
    (setf unexplored (generateAllLocations width height))
    (make-instance 'randomPlayerPlus
        :thisBoard this
        :otherBoard other
        :ships ships
        :unexplored unexplored
    )
)
; ------------------------------------------------------------------------------------------------------------


; Placing ships ---------------------------------------------------------------------------------------------
; This player still place ships randomly
(defmethod playerPlaceShips((player randomPlayerPlus) &aux board ships)
    (setf ships (player-ships player))
    (setf board (player-board player))

    ; Loop through the ships that have to be placed
    (loop for ship in ships do
        (randomlyPlaceShip ship board)
    )
)
; randomlyPlaceShip method can be found in "Ship.l"
; ----------------------------------------------------------------------------------------------------------------


; "Open fire" or "Take turn" method ------------------------------------------------------------------------------
; Player will randomly fire at an unexplored location, if the result is a hit, 
; then the adjacent unexplored locations will be add to the preferred list to explore.
(defmethod playerOpenFire((p randomPlayerPlus) &aux location hit)
   (setf location (getNextLocation p))
   (setf hit (fireAtLocation location p))
   (modifyLists location hit p)
)

; Returns the location that the player should fire at next.
(defmethod getNextLocation((p randomPlayerPlus) &aux unexplored preferred location)
    ; Gather the player's information
    (setf unexplored (player-unexplored p))
    (setf preferred (player-preferred p))

    ; Get a random location either from the preferred list or the unexplored list.
    ; On theory the game should end before or when both of them become empty.
    (setf location nil)
    (if (equal preferred nil)
        (setf location (randomFromList unexplored))
        (setf location (randomFromList preferred))
    )

    location
)

; Returns T if it hits a target, else return nil.
; Note that the lists are modified before return.
(defmethod fireAtLocation((l location) (p randomPlayerPlus) &aux x y board)
    ; Fire at the given location
    (setf x (location-x l))
    (setf y (location-y l))
    (setf board (player-otherBoard p))
    (fireAtBoard x y board)

    ; Check the result
    (isCellHit (getCell x y board))
)

; Modifies the three lists that this player keeps based on the previous fire results.
; Returns nothing
(defmethod modifyLists((l location) hit (p randomPlayerPlus) &aux unexplored explored preferred)
    ; Get the basic list in place
    (setf unexplored (player-unexplored p))
    (setf explored (player-explored p))
    (setf preferred (player-preferred p))

    ; Remove the location from the unexplored / preferred and add to explored
    (setf unexplored (remove l unexplored))
    (setf preferred (remove l preferred))
    (setf explored (cons l explored))

    ; If the location hit something, move its adjacent locations from unexplored to preferred.
    ; That means it will ignore the locations that are already in preferred or explored list.
    (if hit
        (setf preferred (append (getAdjacents l unexplored) preferred))
    )

    ; Write back the modified values
    (setf (player-unexplored p) unexplored)
    (setf (player-explored p) explored)
    (setf (player-preferred p) preferred)

    nil
)
; -----------------------------------------------------------------------------------------------------------------


; End state ---------------------------------------------------------------------
(defmethod isPlayerDefeated((p randomPlayerPlus) &aux ships result sunk)
    (setf ships (player-ships p))
    (setf result t)

    (loop for ship in ships do
        (setf sunk (isShipSunk ship))
        (setf result (and result sunk))
    )

    result
)
; -------------------------------------------------------------------------------