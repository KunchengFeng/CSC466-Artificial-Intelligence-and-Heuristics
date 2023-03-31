; File: Location.l
; This file is created in accompany to the needs of RandomPlayerPlus.l
; Main reason been:
; (member '(a b) '((a b) (c d))) --> NIL
; (remove '(a b) '((a b) (c d))) --> ((A B) (C D))

(defclass location()
    (
        (x :accessor location-x :initarg :x)
        (y :accessor location-y :initarg :y)
    )
)

(defmethod newLocation(x y)
    (make-instance 'location :x x :y y)
)

(defmethod generateAllLocations(width height &aux locations)
    (setf locations (list))
    (dotimes (x width)
        (dotimes (y height)
            (setf locations (cons (newLocation x y) locations))
        )
    )
    (reverse locations)
)

(defmethod randomFromList((ls list))
    (nth (random (length ls)) ls)
)

; Return a list of adjacent "locations" of the parameter,
; If it's not in this list (unexplored), it should already be in others (preferred or explored).
(defmethod getAdjacents((l location) (ls list) &aux x y adjacent result)
    (setf x (location-x l))
    (setf y (location-y l))
    (setf result (list))

    ; Left
    (setf adjacent (getFromList (- x 1) y ls))
    (if (not (equal adjacent nil))
        (setf result (cons adjacent result))
    )

    ; Right
    (setf adjacent (getFromList (+ x 1) y ls))
    (if (not (equal adjacent nil))
        (setf result (cons adjacent result))
    )

    ; Up
    (setf adjacent (getFromList x (- y 1) ls))
    (if (not (equal adjacent nil))
        (setf result (cons adjacent result))
    )

    ; Down
    (setf adjacent (getFromList x (+ y 1) ls))
    (if (not (equal adjacent nil))
        (setf result (cons adjacent result))
    )

    result
)

; Recursively search through a list of "location" objects.
; Returns that object if its X and Y matches.
(defmethod getFromList(x y (ls list) &aux location)
    (setf location (car ls))
    (cond
        ; End of the list
        ((equal location nil)
            nil
        )
        ; Location's X and Y matches
        ((and (= x (location-x location)) (= y (location-y location)))
            location
        )
        ; No match, continue the search
        (t
            (getFromList x y (cdr ls))
        )
    )
)