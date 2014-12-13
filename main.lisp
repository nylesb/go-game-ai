(defun play-game ()
  "Does the interfacing for a 2-player Go game."
  (let* ((board-size 9)
         (board (list (make-list board-size :initial-element '-)))
         (move nil))
    (dotimes (i (- board-size 1))
      (nconc board (list (make-list board-size :initial-element '-))))
    (print-board board)
    (princ (format nil "~%Your move: "))
    (loop while (not (equal (setf move (read)) 'end)) do
      (setf move (process-move move)) ; Player's move
      (setf (nth (second move) (nth (first move) board)) 'B)
      (setf move (make-move board)) ; AI's move
      (setf (nth (second move) (nth (first move) board)) 'W)
      (print-board board)
      (princ (format nil "~%Your move: ")))))

(defun print-board (board)
  "Formats board for readable printing"
  (let ((row-labels "ABCDEFGHIJKLMNOPQRS") ; Supports up to 19x19 board
        (row-index 0))
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
      (princ (format nil " ~A" (+ col 1))))))

(defun process-move (symbol)
  "Takes a move like 'E7' (as a symbol) and processes it to find the proper
  indices for the game board."
  (let ((symbol-string (symbol-name symbol)))
    (list (position (char symbol-string 0) "ABCDEFGHIJKLMNOPQRS")
          (- (parse-integer (subseq symbol-string 1)) 1))))

(defun make-move (board)
  "Returns a valid random move given the current board."
  (let ((board-size (length board))
        (x nil)
        (y nil))
    (loop do
      (setf x (random board-size))
      (setf y (random board-size))
     while (not (equal (nth y (nth x board)) '-)))
    (list x y)))