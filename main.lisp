(defun play-game ()
  "Does the interfacing for a 2-player Go game."
  (let* ((board-size 9)
         (board (list (make-list board-size :initial-element '-)))
         (move nil)
         (available-moves '((0 0))))
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
      (setf available-moves (delete move available-moves :test #'equal))
      (at move board :set 'B)
      (setf avilable-moves (append available-moves (check-captures move board)))
      ;; 2-Player's move
      (print-board board)
      (loop while (not (find move available-moves :test #'equal)) do
        (princ (format nil "~%Invalid move!  Try again: "))
        (setf move (process-move (read))))
      (setf available-moves (delete move available-moves :test #'equal))
      (at move board :set 'W)
      (setf available-moves (append available-moves (check-captures move board)))
      ;; AI's move
      ;(setf move (make-move available-moves))
      ;(setf available-moves (delete move available-moves :test #'equal))
      ;(at move board :set 'W)
      ;(check-captures move board)
      ;; Display
      (print-board board)
      (princ (format nil "~%Your move: ")))))

(defun neighbors (position)
  "Returns a list containing a position and all its neighbors."
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

(defun make-move (available-moves)
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

(defun check-captures (position board)
  "After a move has been made, checks what captures might happen.  Captures are
  performed and the removed positions are returned."
  (let ((removed nil)
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
      (unless (is-free space board)
        (setf removed t)
        (capture space board (at space board))))
    (if (and (not removed) (not (is-free position board)))
        (capture position board))
    (print captured-list))))