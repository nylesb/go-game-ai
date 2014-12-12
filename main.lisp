(defun play-game ()
  "Does the interfacing for a 2-player Go game."
  (let* ((board-size 9)
         (board (make-list board-size :initial-element
                           (make-list board-size :initial-element '-))))
    (loop do
      (print "Input please: ") 
      (setf input (read))
      (print board)
      while (not (equal input "end")))))

(defun print-board (board)
  "Formats board for readable printing"
  (let ((row-labels "ABCDEFGHIJKLMNOPQRS")
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