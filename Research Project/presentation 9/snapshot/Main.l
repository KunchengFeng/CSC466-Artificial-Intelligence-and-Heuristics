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
(load "TierListPlayerPlusPlus.l")
(load "GameResult.l")
(load "GameStatistics.l")

(setf *AIs* '(randomPlayer randomPlayerPlus randomPlayerPlusPlus tierListPlayer tierListPlayerPlus tierListPlayerPlusPlus))

; Human vs AI ----------------------------------------------------------------------------------------
(defun play(&aux num winner)
	; Some info to help the player get started.
	(format t "Welcome to the Battleship game.~%")
	(format t "Each player have 5 ships on the board,~%")
	(format t "to win, you have to sink all of the other player's ship before it sinks all of yours.~%")
	(format t "At each turn, you will be shown a marker map on the left, and your board on the right.~%")
	(format t "When you are asked to enter a position, enter an letter for X, followed by space, and then a number for Y.~%")
	(format t "For example: B 7~%~%")
	(format t "Enter anything to start ...")
	(read)

	(format t "Available AIs: ~%")
	(dotimes (n (length *AIs*))
		(format t "~A - ~A~%" (+ n 1) (nth n *AIs*))
	)
	(format t "Choose your opponent: ")
	(setf num (read))
	
	(setf winner (game-winner (quickGame 0 num)))
	(if (equal winner 'player1)
		(format t "Congraduations, you won!~%")
		(format t "Game over!~%")
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
		; 6 = tier list player ++
		((equal num 6)
			(newTierListPlayerPlusPlus this other ships)
		)
		(t
			(format t "No player correspond to the number entered: ~A~%" num)
			(bye)
		)
	)
)


; AI vs AI ------------------------------------------------------------------------------------
(defun getStatistics(&aux p1Type p2Type iterations gameStatistics)
	; Get basic informations
	(format t "Available AIs: ~%")
	(dotimes (n (length *AIs*))
		(format t "~A - ~A~%" (+ n 1) (nth n *AIs*))
	)
	(format t "Enter a corresponding number to choose AI 1: ")
	(setf p1Type (read))
	(format t "Enter a corresponding number to choose AI 2: ")
	(setf p2Type (read))
	(format t "Enter the number of iterations: ")
	(setf iterations (read))
	
	; Entering hte loop
	(setf gameStatistics (newGameStatistics))
	(format t "Simulating games ...~%~%")
	(dotimes (i iterations)
		(combineResult gameStatistics (quickGame p1Type p2Type))
	)

	(displayStatistics gameStatistics)
)
; ---------------------------------------------------------------------------------------------


; Play games ---------------------------------------------------------------------------------
(defun quickGame(player1Type player2Type &aux player1 player2 ships1 ships2 board1 board2 turns winner player1Hits player2Hits)
	; Initialize the game
	(setf board1 (newBoard 10 10))
	(setf board2 (newBoard 10 10))
	(setf ships1 (reverse (generateShips)))
	(setf ships2 (reverse (generateShips)))
	(setf player1 (createPlayer player1Type board1 board2 ships1))
	(setf player2 (createPlayer player2Type board2 board1 ships2))

	(playerPlaceShips player1)
	(playerPlaceShips player2)
	(setf winner nil)
	(setf turns 0)

	; Main game loop
	(loop while (equal winner nil) do
		(playerOpenFire player1)
		(playerOpenFire player2)
		(setf turns (+ turns 1))
		(setf winner (getWinner player1 player2))
	)

	; Count the hits achieved on the opponent
	(setf player1Hits (countHits ships2))
	(setf player2Hits (countHits ships1))

	(newGameResult turns winner player1Hits player2Hits)
)

; Return the winner in text form, 'player1, 'player2, 'draw, or 'nil
(defun getWinner(player1 player2 &aux p1Win p2Win)
	(if (isPlayerDefeated player1)
		(setf p2Win t)
	)
	(if (isPlayerDefeated player2)
		(setf p1Win t)
	)

	(cond
		((and p1Win p2Win)
			'draw
		)
		(p1Win
			'player1
		)
		(p2Win
			'player2
		)
		(t
			nil
		)
	)
)