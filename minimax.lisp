;;; Implementation file for minimax algorithm
;;; Assert: main.lisp has been loaded already

(defclass minimax-node ()
  ((parent :initarg :parent :accessor parent)
   (board :initarg :board :accessor board)
   (available-moves :initarg :available-moves :accessor available-moves)
   (children :initarg :children :accessor children)
   (value :initarg :value :accessor value)
   (color :initarg :color :accessor color)
   (captures :initarg :captures :accessor captures)
   (ko :initarg :ko :accessor ko)))

(defun minimax-move (available-moves color board)
  "Given a board state, returns a move  for color decided on using a minimax
  algorithm."
  (let ((root (make-instance 'minimax-node
                :board (copy-list board)
                :available-moves (copy-list available-moves)
                :color color
                :children nil
                :ko '(nil)))
        (search-depth 4))
    (generate-children root 1)
    (print-board (board root))
    (dolist (child (children root))
      (print (available-moves child))
      (print-board (board child))
      (dolist (child2 (children child))
        (print (available-moves child2))
        (print-board (board child2))))))

(defun generate-children (node depth)
  "Generates all children of node down to a certain depth"
  (dolist (move (available-moves node))
    (let ((child (make-instance 'minimax-node
                    :ko (ko node)
                    :children nil
                    :board (copy-tree (board node))
                    :color (if (equal (color node) 'W) 'B 'W)
                    :available-moves (copy-tree (available-moves node)))))
      (make-move move (color child) (available-moves child) (ko child) (board child))
      (if (children node)
          (append-modify (children node) (list child))
          (setf (children node) (list child)))
      (if (> depth 0)
          (generate-children child (- depth 1))))))

(defun board-evaluator (board color)
  "Evaluates the strength of color given a board.  The specifics of how this
  is done will change based on testing and what tends to make stronger AI."
  1)