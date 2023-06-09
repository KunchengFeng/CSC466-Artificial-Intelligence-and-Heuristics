(defclass gameStatistics()
    (
        (gamesPlayed :accessor s-gamesPlayed :initform 0)
        (averageTurns :accessor s-averageTurns :initform 0)
        (p1Wins :accessor s-p1Wins :initform 0)
        (averageP1Hits :accessor s-p1Hits :initform 0)
        (p2Wins :accessor s-p2Wins :initform 0)
        (averageP2Hits :accessor s-p2Hits :initform 0)
        (draws :accessor s-draws :initform 0)
    )
)

(defmethod newGameStatistics()
    (make-instance 'gameStatistics)
)

(defmethod combineResult((stats gameStatistics) (result gameResult) &aux winner)
    (setf (s-gamesPlayed stats) (+ (s-gamesPlayed stats) 1))
    (setf (s-averageTurns stats) (combineValues (s-averageTurns stats) (game-turns result)))
    (setf winner (game-winner result))
    (cond
        ((equal winner 'player1)
            (setf (s-p1Wins stats) (+ (s-p1Wins stats) 1))
        )
        ((equal winner 'player2)
            (setf (s-p2Wins stats) (+ (s-p2Wins stats) 1))
        )
        (t
            (setf (s-draws stats) (+ (s-draws stats) 1))
        )
    )
    (setf (s-p1Hits stats) (combineValues (s-p1Hits stats) (game-hits1 result)))
    (setf (s-p2Hits stats) (combineValues (s-p2Hits stats) (game-hits2 result)))
)

; For some reason my version of Common Lisp don't have the 'average function
(defun combineValues(value1 value2)
    (/ (+ value1 value2) 2.0)
)

(defmethod displayStatistics((s gameStatistics))
    (format t "Game statistics:~%")
    (format t "Number of games played: ~A~%" (s-gamesPlayed s))
    (format t "Average number of turns: ~A~%" (s-averageTurns s))
    (format t "Player 1 victories: ~A~%" (s-p1Wins s))
    (format t "Player 2 victories: ~A~%" (s-p2Wins s))
    (format t "Draws: ~A~%" (s-draws s))
    (format t "Average hits achieved by player 1: ~A~%" (s-p1Hits s))
    (format t "Average hits achieved by player 2: ~A~%" (s-p2Hits s))
    (format t "~%~%")
)