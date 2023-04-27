; File: TierListPlayerPlusPlus.l

; This is an improvement from "Tier list player plus",
; but the only improvement is that it will avoid placing ships next to each other.

; Note, many code comments are removed, because they are exactly the same as its predecessor.

(defclass tierListPlayerPlusPlus()
    (
        (name :accessor player-name :initform 'TierListPlayerPlusPlus)
        (thisBoard :accessor player-board :initarg :thisBoard)
        (otherBoard :accessor player-otherBoard :initarg :otherBoard)
        (ships :accessor player-ships :initarg :ships)
        (t0 :accessor player-t0 :initform '())
        (t1 :accessor player-t1 :initform '())
        (t2 :accessor player-t2 :initarg :t2)
        (t3 :accessor player-t3 :initform '())
        (t4 :accessor player-t4 :initform '())
        (all :accessor player-all :initarg :all)
    )
)


; Constructor
(defmethod newTierListPlayerPlusPlus((this board) (other board) (ships list) &aux x y locations)
    (setf x (board-width this))
    (setf y (length (board-rows other)))
    (setf locations (generateAllLocations x y))
    (make-instance 'tierListPlayerPlusPlus
        :thisBoard this
        :otherBoard other
        :ships ships
        :t2 locations
        :all locations
    )
)


; Placing ships
; This player will avoid placing ships next to each other
(defmethod playerPlaceShips((player tierListPlayerPlusPlus) &aux board ships)
    (setf ships (player-ships player))
    (setf board (player-board player))

    ; Loop through the ships that have to be placed
    (loop for ship in ships do
        (noneAdjacentPlaceShip ship board)
    )
)
; noneAdjacentPlaceShip method can be found in "Ship.l"


; Open fire
(defmethod playerOpenFire((p tierListPlayerPlusPlus) &aux location hit)
    (setf location (getNextLocation p))
    (setf hit (fireAtLocation location (player-otherBoard p)))
    (modifyLists location hit p)
)

(defmethod getNextLocation((p tierListPlayerPlusPlus) &aux t1 t2 t3 t4 location)
    (setf t1 (player-t1 p))
    (setf t2 (player-t2 p))
    (setf t3 (player-t3 p))
    (setf t4 (player-t4 p))

    (setf location nil)
    (cond
        ; Pick from t4 if not empty
        ((not (equal t4 nil))
            (setf location (randomFromList t4))
        )
        ; Pick from t3 if not empty, given that t4 is empty
        ((not (equal t3 nil))
            (setf location (randomFromList t3))
        )
        ; Pick from t2 if not empty, given that t3 is empty
        ((not (equal t2 nil))
            (setf location (randomFromList t2))
        )
        ; Pick from t1 if no other options, the game should end when t1 runs out
        (t
            (setf location (randomFromList t1))
        )
    )

    location
)


; Check Results
(defmethod modifyLists((l location) hit (p tierListPlayerPlusPlus))
    (moveToTier l p 0)
    (if hit 
        (doWhenHit l p) 
        (moveNeighborsToTier l p 1)
    )
)

(defmethod doWhenHit((thisL location) (p tierListPlayerPlusPlus) &aux opposite board ship)
    ; Move all unexplored (not t0) neighbors to preferred (t3) list.
    (moveNeighborsToTier thisL p 3)

    ; Get next inferred ship location, if qualified, move to critical (t4) list.
    (setf opposite (getOpposite thisL p))
    (if (isQualifiedOpposite opposite p)
        (moveToTier opposite p 4)
    )

    ; But ... if this hit sinks a ship
    (setf board (player-otherBoard p))
    (if (isLocationSunk thisL board)
        (progn
            (setf ship (getLocationShip thisL board))
            ; For all of the locations around this ship
            (loop for otherL in (getLocationsAroundTheShip ship p) do
                ; If not next to an known unsunk ship, and is unexplored, move to avoid (t1) list.
                (if (and (not (isNextToUnsunkShip otherL p)) (isUnexplored otherL p))
                    (moveToTier otherL p 1)
                )
            )
        )
    )
)


; Get *ONE* adjacent location from all tiers of this player.
(defmethod getAdjacentFromTiers(direction (l location) (p tierListPlayerPlusPlus) &aux fullList)
    (setf fullList (player-all p))

    (cond
        ((equal direction 'left)
            (getLeftAdjacent l fullList)
        )
        ((equal direction 'right)
            (getRightAdjacent l fullList)
        )
        ((equal direction 'above)
            (getAboveAdjacent l fullList)
        )
        (t
            (getBelowAdjacent l fullList)
        )
    )
)

; Get an location instance that is opposite to an consecutive hit.
(defmethod getOpposite((l location) (p tierListPlayerPlusPlus) &aux fullList opposite)
    (setf fullList (player-all p))
    (cond
        ; Left is also hit
        ((isConsecutiveHit (getLeftAdjacent l fullList) p)
            (getRightAdjacent l fullList)
        )
        ; Right is also hit
        ((isConsecutiveHit (getRightAdjacent l fullList) p)
            (getLeftAdjacent l fullList)
        )
        ; Above is also hit
        ((isConsecutiveHit (getAboveAdjacent l fullList) p)
            (getBelowAdjacent l fullList)
        )
        ; Below is also hit
        ((isConsecutiveHit (getBelowAdjacent l fullList) p)
            (getAboveAdjacent l fullList)
        )
        (t
            nil
        )
    )
)

; Get *EVERY* location that surrounds this ship
(defmethod getLocationsAroundTheShip((s ship) (p tierListPlayerPlusPlus) &aux board cells all-ls ship-ls adjs result)
    (setf board (player-otherBoard p))
    (setf cells (ship-cells s))
    (setf all-ls (player-all p))
    (setf ship-ls (list))
    (setf result (list))

    ; First translate the cell coordinates into this player's location instances.
    (loop for cell in cells do
        (setf ship-ls (cons (getFromList (cell-num cell) (cell-row cell) all-ls) ship-ls))
    )

    ; For each of the location of the ship
    (loop for l in ship-ls do
        ; Get all the locations that surrounds the current one
        (setf adjs (getAdjacents l all-ls))
        ; And for each of the surrounding location
        (loop for adj in adjs do
            ; Add to the result if it is not this ship's location
            (if (not (member adj ship-ls))
                (setf result (cons adj result))
            )
        )
    )

    result
)


(defmethod moveNeighborsToTier((l location) (p tierListPlayerPlusPlus) tier &aux adjacents)
    (setf adjacents (getAdjacents l (player-all p)))
    (loop for adjacent in adjacents do
        (if (isUnexplored adjacent p)
            (moveToTier adjacent p tier)
        )
    )
)

; Remove this location from *EVERY* tiers of this player.
(defmethod removeFromTiers((l location) (p tierListPlayerPlusPlus))
    (setf (player-t0 p) (remove l (player-t0 p)))
    (setf (player-t1 p) (remove l (player-t1 p)))
    (setf (player-t2 p) (remove l (player-t2 p)))
    (setf (player-t3 p) (remove l (player-t3 p)))
    (setf (player-t4 p) (remove l (player-t4 p)))
)

; Add this location to the desired tier of this player.
(defmethod addToTier((l location) (p tierListPlayerPlusPlus) target)
    (cond
        ((= target 0)
            (setf (player-t0 p) (cons l (player-t0 p)))
        )
        ((= target 1)
            (setf (player-t1 p) (cons l (player-t1 p)))
        )
        ((= target 2)
            (setf (player-t2 p) (cons l (player-t2 p)))
        )
        ((= target 3)
            (setf (player-t3 p) (cons l (player-t3 p)))
        )
        (t
            (setf (player-t4 p) (cons l (player-t4 p)))
        )
    )
)

; Remove this location from all tiers, then write into the desired one.
(defmethod moveToTier((l location) (p tierListPlayerPlusPlus) target)
    (removeFromTiers l p)
    (addToTier l p target)
)


; Checkers
(defmethod isExplored((l location) (p tierListPlayerPlusPlus))
    (member l (player-t0 p))
)

(defmethod isUnexplored((l location) (p tierListPlayerPlusPlus))
    (not (member l (player-t0 p)))
)

(defmethod isConsecutiveHit(location (p tierListPlayerPlusPlus) &aux board)
    (setf board (player-otherBoard p))

    ; This location need to:
    (and
        ; Be not null
        (not (equal location nil))
        ; Have achieved an hit
        (isLocationHit location board)
        ; Have an unsunk ship
        (not (isLocationSunk location board))
    )

    ; Can't just ask if a neighbor have an unsunk ship, that would be cheating.
)

(defmethod isQualifiedOpposite(location (p tierListPlayerPlusPlus))
    ; This location need to:
    (and
        ; Be not null
        (not (equal location nil))
        ; Be unexplored
        (isUnexplored location p)
    )
)

; Return T/F
(defmethod isNextToUnsunkShip((l location) (p tierListPlayerPlusPlus) &aux board adjacents result)
    ; An location is considered next to an unsunk ship if:
    (setf board (player-otherBoard p))
    (setf result nil)

    ; For each of its adjacent locations
    (loop for adjacent in (getAdjacents l (player-all p)) do
        ; If one of them:
        ; Has been explored! (Else it would be cheating)
        ; And it had result in a hit
        ; But the ship has not sunk
        (if (and
                (isExplored adjacent p)
                (isLocationHit adjacent board)
                (not (isLocationSunk adjacent board))
            )
            (setf result t)
        )
    )

    result
)


; End state 
(defmethod isPlayerDefeated((p tierListPlayerPlusPlus) &aux ships result sunk)
    (setf ships (player-ships p))
    (setf result t)

    (loop for ship in ships do
        (setf sunk (isShipSunk ship))
        (setf result (and result sunk))
    )

    result
)