; File: TierListPlayer.l
; This playing machine is built on top of RandomPlayerPlusPlus machine.

; Locations of a board will be divided to 5 tiers,
; 0 tier = explored tier, won't explore again.
; 1 tier = tier to avoid, won't explore unless out of options.
; 2 tier = nomal tier, will randomly pick from here under normal circumstances.
; 3 tier = preferred tier, will randomly pick from here before picking from 2nd tier.
; 4 tier = critical tier, will always be picked first.

; This player also addresses the short comings of RandomPlayer++ not recognizing unsunk parallel ships.

(defclass tierListPlayer()
    (
        (name :accessor player-name :initform 'TierListPlayer)
        (thisBoard :accessor player-board :initarg :thisBoard)
        (otherBoard :accessor player-otherBoard :initarg :otherBoard)
        (ships :accessor player-ships :initarg :ships)
        (t0 :accessor player-t0 :initform '())
        (t1 :accessor player-t1 :initform '())
        (t2 :accessor player-t2 :initarg :locations)
        (t3 :accessor player-t3 :initform '())
        (t4 :accessor player-t4 :initform '())
    )
)


; Constructor --------------------------------------------------------------------------------
(defmethod newTierListPlayer((this board) (other board) (ships list) &aux x y locations)
    (setf x (board-width this))
    (setf y (length (board-rows other)))
    (setf locations (generateAllLocations x y))
    (make-instance 'tierListPlayer
        :thisBoard this
        :otherBoard other
        :ships ships
        :locations locations
    )
)
; ---------------------------------------------------------------------------------------------


; Placing ships ---------------------------------------------------------------------------------------------
; This player still place ships randomly
(defmethod playerPlaceShips((player tierListPlayer) &aux board ships)
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
(defmethod playerOpenFire((p tierListPlayer) &aux location hit)
    (setf location (getNextLocation p))
    (setf hit (fireAtLocation location (player-otherBoard p)))
    (modifyLists location hit p)
)

(defmethod getNextLocation((p tierListPlayer) &aux t1 t2 t3 t4 location)
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

(defmethod modifyLists((l location) hit (p tierListPlayer))
    (moveToTier l p 0)
    (if hit 
        (doWhenHit l p) 
        (doWhenNotHit l p)
    )
)

; If shot landed a hit
(defmethod doWhenHit((l location) (p tierListPlayer) &aux adjacent opposite)
    ; Move all unexplored (not t0) neighbors to preferred (t3) list.
    (moveNeighborsToTier l p 3)

    ; If a neighbor counts as a consecutive hit, then move the opposite neighbor (?) into critical tier (t4).
    ; | X | X | ? |
    (if (isConsecutiveHit (getAdjacentFromTiers 'left l p) p)
        (setf opposite (getAdjacentFromTiers 'right l p))
    )
    ; | ? | X | X |
    (if (isConsecutiveHit (getAdjacentFromTiers 'right l p) p)
        (setf opposite (getAdjacentFromTiers 'left l p))
    )
    ; | ? |
    ; | X |
    ; | X |
    (if (isConsecutiveHit (getAdjacentFromTiers 'below l p) p)
        (setf opposite (getAdjacentFromTiers 'above l p))
    )
    ; | X |
    ; | X |
    ; | ? |
    (if (isConsecutiveHit (getAdjacentFromTiers 'above l p) p)
        (setf opposite (getAdjacentFromTiers 'below l p))
    )

    ; Move the opposite (and qualified) neighbor into critical tier (t4)
    (if (isQualifiedOpposite opposite p)
        (moveToTier opposite p 4)
    )
)

; If shot did not hit, move all unexplored (not t0) neighbors to avoid list (t1).
(defmethod doWhenNotHit((l location) (p tierListPlayer))
    (moveNeighborsToTier l p 1)
)

; Move all neighbors to a tier, except the explored ones.
(defmethod moveNeighborsToTier((l location) (p tierListPlayer) tier &aux adjacents)
    (setf adjacents (getAdjacentsFromTiers l p))
    (loop for adjacent in adjacents do
        (if (isUnexplored adjacent p)
            (moveToTier adjacent p tier)
        )
    )
)

; Return T/F
(defmethod isUnexplored((l location) (p tierListPlayer))
    (not (member l (player-t0 p)))
)

; Return T/F
(defmethod isConsecutiveHit(location (p tierListPlayer) &aux board)
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

; Return T/F
(defmethod isQualifiedOpposite(location (p tierListPlayer))
    ; This location need to:
    (and
        ; Be not null
        (not (equal location nil))
        ; Be unexplored
        (isUnexplored location p)
    )
)

; Get *all* locations that this player keep track of
(defmethod getFullTierList((p tierListPlayer) &aux t0 t1 t2 t3 t4)
    (setf t0 (player-t0 p)) ; Explored list
    (setf t1 (player-t1 p)) ; Avoid list
    (setf t2 (player-t2 p)) ; Normal list
    (setf t3 (player-t3 p)) ; Preferred list
    (setf t4 (player-t4 p)) ; Critical list
    (append t0 t1 t2 t3 t4)
)

; Get *ALL* adjacent locations from all tiers of this player.
(defmethod getAdjacentsFromTiers((l location) (p tierListPlayer) &aux fullList)
    (setf fullList (getFullTierList p))
    (getAdjacents l fullList)
)

; Get *ONE* adjacent location from all tiers of this player.
(defmethod getAdjacentFromTiers(direction (l location) (p tierListPlayer) &aux fullList)
    (setf fullList (getFullTierList p))

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

; Remove this location from *ALL* tiers of this player.
(defmethod removeFromTiers((l location) (p tierListPlayer))
    (setf (player-t0 p) (remove l (player-t0 p)))
    (setf (player-t1 p) (remove l (player-t1 p)))
    (setf (player-t2 p) (remove l (player-t2 p)))
    (setf (player-t3 p) (remove l (player-t3 p)))
    (setf (player-t4 p) (remove l (player-t4 p)))
)

; Add this location to the desired tier of this player.
(defmethod addToTier((l location) (p tierListPlayer) target)
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
(defmethod moveToTier((l location) (p tierListPlayer) target)
    (removeFromTiers l p)
    (addToTier l p target)
)
; ----------------------------------------------------------------------------------------------------------------


; End state ---------------------------------------------------------------------
(defmethod isPlayerDefeated((p tierListPlayer) &aux ships result sunk)
    (setf ships (player-ships p))
    (setf result t)

    (loop for ship in ships do
        (setf sunk (isShipSunk ship))
        (setf result (and result sunk))
    )

    result
)
; -------------------------------------------------------------------------------