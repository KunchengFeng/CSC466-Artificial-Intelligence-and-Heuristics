(defclass gameResult()
    (
        (turns :accessor game-turns :initarg :turns)
        (winner :accessor game-winner :initarg :winner)
        (hits1 :accessor game-hits1 :initarg :hits1)
        (hits2 :accessor game-hits2 :initarg :hits2)
    )
)

(defmethod newGameResult(turns winner hits1 hits2)
    (make-instance 'gameResult 
        :turns turns 
        :winner winner 
        :hits1 hits1 
        :hits2 hits2
    )
)