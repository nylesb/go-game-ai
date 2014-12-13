(defun play-game ()
  "Does the interfacing for a 2-player Go game."
  (let* ((board-size 9)
         (board (list (make-list board-size :initial-element '-))))
    (dotimes (i (- board-size 1))
      (nconc board (list (make-list board-size :initial-element '-))))
    (print-board board)
    (loop do
      (print "Input please: ") 
      (setf input (process-input (read)))
      (setf (nth (second input) (nth (first input) board)) 'B)
      (print-board board)
      while (not (equal input "end")))))

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

(defun process-input (symbol)
  "Takes a move like 'E7' (as a symbol) and processes it to find the proper
  indices for the game board."
  (let ((symbol-string (symbol-name symbol)))
    (list (position (char symbol-string 0) "ABCDEFGHIJKLMNOPQRS")
          (- (parse-integer (subseq symbol-string 1)) 1))))