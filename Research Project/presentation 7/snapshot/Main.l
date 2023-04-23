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
(load "TierListPlayer.l")
(load "TierListPlayerPlus.l")

(setf *AIs* '(randomPlayer randomPlayerPlus randomPlayerPlusPlus tierListPlayer tierListPlayerPlus))

; Human vs AI ----------------------------------------------------------------------------------------
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

	(setf human (createPlayer 0 board1 board2 ships1))
	(format t "Available AIs: ~%")
	(dotimes (n (length *AIs*))
		(format t "~A - ~A~%" (+ n 1) (nth n *AIs*))
	)
	(format t "Choose your opponent: ")
	(setf num (read))
	(setf ai (createPlayer num board2 board1 ships2))

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

; Return a player instance.
(defun createPlayer(num (this board) (other board) (ships list))
	(cond
		; 0 = human player
		((equal num 0)
			(newHumanPlayer this other ships)
		)
		; 1 = random player
		((equal num 1)
			(newRandomPlayer this other ships)
		)
		; 2 = random player +
		((equal num 2)
			(newRandomPlayerPlus this other ships)
		)
		; 3 = random player ++
		((equal num 3)
			(newRandomPlayerPlusPlus this other ships)
		)
		; 4 = tier list player
		((equal num 4)
			(newTierListPlayer this other ships)
		)
		; 5 = tier list player +
		((equal num 5)
			(newTierListPlayerPlus this other ships)
		)
		(t
			(format t "No player correspond to the number entered: ~A~%" num)
			(bye)
		)
	)
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


; AI vs AI ------------------------------------------------------------------------------------
(defun getStatistics(&aux n1 n2 iterations player1 player2 winner b1 b2 s1 s2 v1 v2 draw)
	; Get basic informations
	(format t "Available AIs: ~%")
	(dotimes (n (length *AIs*))
		(format t "~A - ~A~%" (+ n 1) (nth n *AIs*))
	)
	(format t "Enter a corresponding number to choose AI 1: ")
	(setf n1 (read))
	(format t "Enter a corresponding number to choose AI 2: ")
	(setf n2 (read))
	(format t "Enter the number of iterations: ")
	(setf iterations (read))

	; Initialize
	(setf v1 0)
	(setf v2 0)
	(setf draw 0)

	(dotimes (n iterations)
		; Set up necessary instances
		(setf b1 (newBoard 10 10))
		(setf b2 (newBoard 10 10))
		(setf s1 (generateShips))
		(setf s2 (generateShips))
		(setf player1 (createPlayer n1 b1 b2 s1))
		(setf player2 (createPlayer n2 b2 b1 s2))

		; Play the game
		(playerPlaceShips player1)
		(playerPlaceShips player2)
		(setf winner (takeTurn player1 player2))

		; Keep track
		(cond
			((equal winner player1)
				(setf v1 (+ v1 1))
			)
			((equal winner player2)
				(setf v2 (+ v2 1))
			)
			(t
				(setf draw (+ draw 1))
			)
		)
	)

	; Report the result
	(format t "~%~A games played: ~%" iterations)
	(format t "Player 1 (~A) victories: ~A~%" (player-name player1) v1)
	(format t "Player 2 (~A) victories: ~A~%" (player-name player2) v2)
	(format t "Draws: ~A~%" draw)

	nil
)