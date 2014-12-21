;;;; Implementation file for minimax algorithm
;;;; Assert: main.lisp has been loaded already

;; A class to create the tree to apply the minimax algorithm to.
;; Each node contains all the information needed about its current state.
(defclass minimax-node ()
  ((board :initarg :board :accessor board)
   (available-moves :initarg :available-moves :accessor available-moves)
   (children :initarg :children :accessor children)
   (value :initarg :value :accessor value)
   (color :initarg :color :accessor color)
   (captures :initarg :captures :accessor captures)
   (ko :initarg :ko :accessor ko)
   (move :initarg :move :accessor move)))

(defun minimax-move (available-moves color board)
  "Given a board state, returns a move for color decided on using a minimax
  algorithm. No optimization methods have been applied, so the search copy-tree
  grows symmetrically (severely increasing run time)."
  (let ((root (make-instance 'minimax-node
                :board (copy-list board)
                :available-moves (copy-list available-moves)
                :color (if (equal color 'W) 'B 'W)
                :children nil
                :ko '(nil)))
        (search-depth 2)
        (choices nil))
    (generate-children root 3)
    (set-values root t)
    (dolist (child (children root)) ; Find best move
      (if (equal (value child) (value root))
          (push (move child) choices)))
    (print (nth (random (length choices)) choices))))

(defun generate-children (node depth)
  "Generates all children of node down to a certain depth."
  (dolist (move (available-moves node))
    ;; Make a new child node, then call recursively if more depth is needed.
    ;; Each node contains a list of all its children
    (let ((child (make-instance 'minimax-node
                    :ko (copy-list (ko node))
                    :children nil
                    :board (copy-tree (board node))
                    :color (if (equal (color node) 'W) 'B 'W)
                    :move move
                    :available-moves (copy-tree (available-moves node)))))
      (make-move move (color child) (available-moves child) (ko child) (board child))
      (if (children node)
          (append-modify (children node) (list child))
          (setf (children node) (list child)))
      (if (> depth 1)
          (generate-children child (- depth 1))))))

(defun board-eval (board color)
  "Evaluates the strength of color given a board.  The specifics of how this
  is done will change based on testing.  Currently the evaluator uses the idea
  of influence stones exert on the board.  Here influence radiates out from a stone,
  giving more influence on spaces directly next to it and less influence farther out."
  (let* ((board-copy (copy-tree board))
         (board-size (length board-copy))
         (positions (list (list 0 0)))
         (range 2)
         (token nil)
         (sum 0))
    (dotimes (i board-size)
      (dotimes (j board-size)
        (nconc positions (list (list i j)))))
    (setf positions (rest positions)) ; Fixes double '(0 0) issue
    (dolist (space positions) ; Initialize board with zeroes
      (if (equal (at space board-copy) '-)
          (at space board-copy :set 0)))
    (labels ((fill (position sign depth)
                "Fills in influence points around position."
                (let ((neighbors (neighbors position)))
                  (if (numberp (at position board-copy))
                      (at position board-copy :set (+ (at position board-copy) (* sign (+ depth 1)))))
                  (dolist (space neighbors)
                    (if (> depth 1)
                        (fill space sign (- depth 1)))
                    (if (numberp (at space board-copy))
                        (at space board-copy :set (+ (at space board-copy) sign)))))))
      (dolist (space positions) ; Begin distributing influence points.
        (cond ((equal (setf token (at space board-copy)) color) (fill space 1 range))
              ((numberp token) nil)
              (t (fill space -1 range))))
      (dolist (space positions)
        (if (numberp (setf token (at space board-copy)))
            (setf sum (+ sum token))))
      sum)))

(defun set-values (root max)
  "Sets the value of node and all descendents of the root using the minimax rules.
  If max is t, then max is used, if max is nil, then a min rule is used."
  (let ((best nil))
    (if (= (length (children root)) 0)
        (return-from set-values (setf (value root) (board-eval (board root) (color root)))))
    (dolist (child (children root))
      (set-values child (not max))) ; Children make opposite choices
    (setf best (value (first (children root))))
    (dolist (child (children root))
      (if max ; Check if node is minimizer or maximizer
          (if (> (value child) best)
              (setf best (value child)))
          (if (< (value child) best)
              (setf best (value child)))))
    (setf (value root) best)))

(defun print-children (root depth function)
  "A tree traversal algorithm.  Can modify it to display characteristics of
  nodes, and the algorithm will display the information in a semi-nice way."
  (let ((data (funcall function board)))
    ;; Use spacing to indicate children
    (princ (format nil "~A~A~%" (make-string (* 4 depth) :initial-element #\Space) data))
    (dolist (child (children root))
      (print-children child (+ depth 1)))))

;;; Below functions were intended to help the AI, but currently are not being used

(defun is-eye (position board)
  "Position is an empty space on board. Determines if it is an eye (a space
  surrounded by a single chain of all the same color).  Returns eye color if true."
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