(load "minimax.lisp")

(defun play-game ()
  "Does the interfacing for a 2-player Go game."
  (let* ((board-size 4)
         (board (list (make-list board-size :initial-element '-)))
         (move nil)
         (available-moves '((0 0)))
         (captures nil)
         (w-captures 0)
         (b-captures 0)
         (ko '(nil)))
    ;; Populate initial available moves
    (dotimes (i board-size)
      (dotimes (j board-size)
        (nconc available-moves (list (list i j)))))
    (setf available-moves (rest available-moves)) ; Fixes '(0 0) repetition
    ;; Create empty board
    (dotimes (i (- board-size 1))
      (nconc board (list (make-list board-size :initial-element '-))))
    ;; Begin game playing
    (print-board board)
    (princ (format nil "~%Your move: "))
    (loop while (not (equal (setf move (read)) 'end)) do
      ;; Player's move
      (setf move (process-move move))
      (loop while (not (find move available-moves :test #'equal)) do
        (princ (format nil "~%Invalid move!  Try again: "))
        (setf move (process-move (read))))
      (make-move move 'B available-moves ko board)
      
      ;;; 2-Player's move
      (print-board board)
      (princ (format nil "~%Your move: "))
      (setf move (process-move (read)))
      (loop while (not (find move available-moves :test #'equal)) do
        (princ (format nil "~%Invalid move!  Try again: "))
        (setf move (process-move (read))))
      (make-move move 'W available-moves ko board)
      
      ;;; AI's move
      ;(setf move (random-move available-moves))
      ;(setf available-moves (delete move available-moves :test #'equal))
      ;(at move board :set 'W)
      ;(perform-captures move board)
      ;; Display
      (print-board board)
      (princ (format nil "~%Your move: ")))))

(defun neighbors (position)
  "Returns a list containing all neighbors of a position."
  (let* ((row (first position))
         (col (second position))
         (neighbors (list (list (- row 1) col))))
    (nconc neighbors (list (list row (+ col 1))
                           (list (+ row 1) col)
                           (list row (- col 1))))))

(defun print-board (board)
  "Formats board for readable printing"
  (let ((row-labels "ABCDEFGHIJKLMNOPQRS") ; Supports up to 19x19 board
        (row-index 0))
    (terpri)
    (princ " ")
    (dotimes (col (length board))
      (princ (format nil " ~A" (+ col 1))))
    (terpri)
    (dolist (row board)
      (princ (format nil "~A" (char row-labels row-index)))
      (dolist (pos row)
        (princ (format nil " ~A" pos)))
      (princ (format nil " ~A" (char row-labels row-index)))
      (setf row-index (+ row-index 1))
      (terpri))
    (princ " ")
    (dotimes (col (length board))
      (princ (format nil " ~A" (+ col 1))))
    (terpri)))

(defun process-move (symbol)
  "Takes a move like 'E7' (as a symbol) and processes it to find the proper
  indices for the game board."
  (let ((symbol-string (symbol-name symbol)))
    (list (position (char symbol-string 0) "ABCDEFGHIJKLMNOPQRS")
          (- (parse-integer (subseq symbol-string 1)) 1))))

(defun make-move (move color available-moves ko board)
  "Updates board and available-moves with the given move for color.
  Note: ko is a 1-element list containing nil or the ko position.
  Returns number of captures made."
  (let ((captures nil)
        (last nil))
    (if (equal (first available-moves) move) ; Delete doesn't work for 1st elt
        (progn (setf last (first (last available-moves)))
               (delete last available-moves :test #'equal)
               (setf (car available-moves) last))
        (delete move available-moves :test #'equal))
    (at move board :set color)
    (when (first ko) ; Ko has passed, can add to moves again
      (append-modify available-moves (list (copy-list (first ko)))))
    (setf captures (perform-captures move board))
    (if (> (length captures) 1) ; Check for a new ko
        (append-modify available-moves captures)
        (setf (first ko) (first captures)))
    (length captures)))

(defun append-modify (list1 list2)
  "Redefines list1 to list2 appended to list1.  Works inside functions which
  takes lists as parameters to update the actual variable put into the function."
  (setf (cdr list1) (append (cdr list1) list2)))

(defun random-move (available-moves)
  "Returns a valid random move given the current board."
  (nth (random (length available-moves)) available-moves))

(defun at (position board &key set)
  "Returns the token at position on board. Returns X if position off board.
  If set is not nil, update this position with value of set."
  (let ((row (first position))
        (col (second position))
        (board-size (length board)))
    (if (or (< row 0) (< col 0) (>= row board-size) (>= col board-size))
        (return-from at 'X))
    (if set
        (setf (nth col (nth row board)) set)
        (nth col (nth row board)))))

(defun is-free (position board)
  "Determines if the stone at position is free by using a recoloring each stone
  attached to position while looking for a free liberty, then filling it back."
  (let ((result nil)
        (board-color (at position board))
        (marker 'M))
    (if (or (equal board-color '-) (equal board-color 'X))
        (return-from is-free t))
    (labels
      ((flood-fill (position board old-color new-color)
         "Fills up the chain of color, replacing spaces with a marker.
         If an open liberty is found, coloring stops."
         (labels
           ((ff (position board old-color new-color)
              "Inner function for flood-fill allowing us to terminate early."
              (let ((row (first position))
                    (col (second position))
                    (color (at position board)))
                (when (equal color '-)
                  (setf result t)
                  (return-from flood-fill nil))
                (if (equal color new-color)
                    (return-from ff nil))
                (when (equal color old-color)
                  (at position board :set new-color)
                  (ff (list (- row 1) col) board old-color new-color)
                  (ff (list row (+ col 1)) board old-color new-color)
                  (ff (list (+ row 1) col) board old-color new-color)
                  (ff (list row (- col 1)) board old-color new-color)))))
         (ff position board old-color new-color))))
      (flood-fill position board board-color marker)
      (flood-fill position board marker board-color)
      result)))

(defun perform-captures (position board)
  "After a move has been made, checks what captures might happen.  Captures are
  performed and the removed positions are returned."
  (let ((color (at position board))
        (removed nil)
        (captured-list nil))
    (labels ((capture (position board old-color)
               "Removes the chain covering position on board."
               (let ((row (first position))
                    (col (second position))
                    (color (at position board)))
                (when (equal color old-color)
                  (at position board :set '-)
                  (setf captured-list (nconc captured-list (list position)))
                  (capture (list (- row 1) col) board old-color)
                  (capture (list row (+ col 1)) board old-color)
                  (capture (list (+ row 1) col) board old-color)
                  (capture (list row (- col 1)) board old-color)))))
    (dolist (space (neighbors position))
      (unless (or (is-free space board)
                  (equal color (at space board)))
        (setf removed t)
        (capture space board (at space board))))
    (if (and (not removed) (not (is-free position board)))
        (capture position board color))
    captured-list)))

(defun is-eye (position board)
  "Position is an empty space on board. Determines if it is an eye (a space
  surrounded by 1 chain of all the same color).  Returns eye color if true."
  (let ((result 0) ; How many surrounding stones we've found
        (goal 4) ; How many surrounding stones we need
        (origin 'O) ; To remember where we started at
        (marker 'M) ; A token to see where we've been
        (neighbors (neighbors position))
        (color nil)
        (row (first position))
        (col (second position))
        (board-length (length board)))
    (labels
      ((flood-fill (position board old-color new-color)
         "Fills up the chain of color, replacing spaces with a marker.
         If original space is found, coloring stops."
         (let ((row (first position))
               (col (second position))
               (color (at position board)))
           (when (equal color origin)
             (setf result (+ result 1)))
           (if (equal color new-color)
               (return-from flood-fill nil))
           (when (equal color old-color)
             (at position board :set new-color)
             (flood-fill (list (- row 1) col) board old-color new-color)
             (flood-fill (list row (+ col 1)) board old-color new-color)
             (flood-fill (list (+ row 1) col) board old-color new-color)
             (flood-fill (list row (- col 1)) board old-color new-color)))))
      ;; Check for one chain surrounding position
      ;; After a B or W stone found, no need to check other neighbors
      (block control
        (dolist (space neighbors)
          (setf color (at space board))
          (when (or (equal color 'W) (equal color 'B))
            (at position board :set origin)
            (flood-fill space board color marker)
            (at position board :set '-)
            (flood-fill space board marker color)
            (return-from control nil))))
      ;; Reduce goal if stone on edge
      (if (or (= row 0) (= row (- board-length 1)))
          (setf goal (- goal 1)))
      (if (or (= col 0) (= col (- board-length 1)))
          (setf goal (- goal 1)))
      (if (= result goal)
          color
          nil))))

(defun game-over (available-moves board)
  "Evaluates the board to determine if only eyes remain."
  (dolist (space available-moves)
    (unless (is-eye space board)
      (return-from game-over nil)))
  t)