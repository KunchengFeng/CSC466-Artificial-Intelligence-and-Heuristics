; File: Main.l
(load "Cell.l")
(load "Row.l")
(load "Board.l")
(load "Ship.l")
(load "Location.l")
(load "HumanPlayer.l")
(load "RandomPlayer.l")
(load "RandomPlayerPlus.l")
(load "RandomPlayerPlusPlus.l")


; Main method to start everything -------------------------------------------------
(defun play(&aux human ai num board1 board2 ships1 ships2 winner)
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
	(format t "Choose your Opponent: ~%")
	(format t "Enter 1 for 'Random Player'~%")
	(format t "Enter 2 for 'Random Player Plus'~%")
	(format t "Enter 3 for 'Random Player Plus Plus'~%")
	(setf num (read))
	(cond
		((equal num 1)
			(setf ai (newRandomPlayer board2 board1 ships2))
		)
		((equal num 2)
			(setf ai (newRandomPlayerPlus board2 board1 ships2))
		)
		((equal num 3)
			(setf ai (newRandomPlayerPlusPlus board2 board1 ships2))
		)
		(t
			(bye)
		)
	)

	(playerPlaceShips human)
	(playerPlaceShips ai)

	(setf winner (takeTurn human ai))
	(cond
		((equal winner human)
			(format t "You won!~%")
		)
		((equal winner ai)
			(format t "~A won!~%" (player-name ai))
		)
		(t
			(format t "It's a draw!~%")
		)
	)
)

(defun generateShips(&aux ships)
	(setf ships (list))
	(loop for ship in *shipTypes* do
		(setf ships (cons (newShip ship) ships))
	)
	ships
)

; Return the victor player's instance.
(defun takeTurn(player1 player2 &aux p1Win p2Win)
	(playerOpenFire player1)
	(playerOpenFire player2)

	(setf p1Win (isPlayerDefeated player2))
	(setf p2Win (isPlayerDefeated player1))

	(cond
		((and p1Win p2Win)
			nil
		)
		(p1Win
			player1
		)
		(p2Win
			player2
		)
		(t
			(takeTurn player1 player2)
		)
	)
)

; A quick game intended for two AIs, only the end result shall be displayed if desired
; Returns the winner's instance or nil
(defun quickGame(p1Num p2Num &optional text &aux b1 b2 s1 s2 p1 p2 winner)
	; Basic game objects
	(setf b1 (newBoard 10 10))
	(setf b2 (newBoard 10 10))
	(setf s1 (reverse (generateShips)))
	(setf s2 (reverse (generateShips)))

	; Set up player 1
	(cond
		((equal p1Num 1)
			(setf p1 (newRandomPlayer b1 b2 s1))
		)
		((equal p1Num 2)
			(setf p1 (newRandomPlayerPlus b1 b2 s1))
		)
		((equal p1Num 3)
			(setf p1 (newRandomPlayerPlusPlus b1 b2 s1))
		)
		(t
			(format t "Player 1's number do not corrolate to any AI player.~%")
			(bye)
		)
	)

	; Set up player 2
	(cond
		((equal p2Num 1)
			(setf p2 (newRandomPlayer b2 b1 s2))
		)
		((equal p2Num 2)
			(setf p2 (newRandomPlayerPlus b2 b1 s2))
		)
		((equal p2Num 3)
			(setf p2 (newRandomPlayerPlusPlus b2 b1 s2))
		)
		(t
			(format t "Player 2's number do not corrolate to any AI player.~%")
			(bye)
		)
	)

	; Have players place ships
	(playerPlaceShips p1)
	(playerPlaceShips p2)

	; Player the game until the end
	(setf winner (takeTurn p1 p2))

	; Display text if needed
	(cond
		((equal text t)
			; Announce the winner
			(if (equal winner nil)
				(format t "It's a draw!")
			)
			(if (equal winner p1)
				(format t "Player 1 (~A) won!~%" (player-name p1))
			)
			(if (equal winner p2)
				(format t "Player 2 (~A) won!~%" (player-name p2))
			)

			; Display the final state
			(format t "~%Player 1's board:~%")
			(display b1)
			(format t "~%Player 2's board:~%")
			(display b2)
		)
	)

	winner
)