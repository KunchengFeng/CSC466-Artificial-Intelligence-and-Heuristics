; File: Main.l
(load "Cell.l")
(load "Row.l")
(load "Board.l")
(load "Ship.l")
(load "Location.l")
(load "HumanPlayer.l")
(load "RandomPlayer.l")
(load "RandomPlayerPlus.l")


; Main method to start everything -------------------------------------------------
(defun play(&aux human ai board1 board2 ships1 ships2)
	; Some info to help the player get started.
	(format t "Welcome to the Battleship game.~%")
	(format t "Each player have 5 ships on the board,~%")
	(format t "to win, you have to sink all of the other player's ship before it sinks all of yours.~%")
	(format t "At each turn, you will be shown a marker map on the left, and your board on the right.~%")
	(format t "When you are asked to enter a position, enter an letter for X, followed by space, and then a number for Y.~%")
	(format t "For example: B 7~%~%")
	(format t "Enter anything to start ...")
	(read)

	(setf board1 (newBoard 10 10))
	(setf board2 (newBoard 10 10))
	(setf ships1 (reverse (generateShips)))
	(setf ships2 (reverse (generateShips)))

	(setf human (newHumanPlayer board1 board2 ships1))
	(setf ai (newRandomPlayerPlus board2 board1 ships2))

	(playerPlaceShips human)
	(playerPlaceShips ai)

	(takeTurn human ai)
)

(defun generateShips(&aux ships)
	(setf ships (list))
	(loop for ship in *shipTypes* do
		(setf ships (cons (newShip ship) ships))
	)
	ships
)

; At this point, player 1 is assumed to be the human player
(defun takeTurn(player1 player2 &aux p1Win p2Win)
	(playerOpenFire player1)
	(playerOpenFire player2)

	(setf p1Win (isPlayerDefeated player2))
	(setf p2Win (isPlayerDefeated player1))

	(cond
		((and p1Win p2Win)
			(format t "It's a draw.~&")
		)
		(p1Win
			(format t "You won!~%")
		)
		(p2Win
			(format t "You lost!~%")
		)
		(t
			(takeTurn player1 player2)
		)
	)
)