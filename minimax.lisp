;;; Implementation file for minimax algorithm
;;; Assert: main.lisp has been loaded already

(defclass minimax-node ()
  ((parent :initarg :parent :accessor parent)
   (board :initarg :board :accessor board)
   (available-moves :initarg :available-moves :accessor available-moves)
   (children :initarg :children :accessor children)
   (value :initarg :value :accessor value)
   (color :initarg :color :accessor color)
   (captures :initarg :captures :accessor captures)))

(defun minimax-move (available-moves color board)
  "Given a board state, returns a move  for color decided on using a minimax
  algorithm."
  (let ((root (make-instance 'minimax-node
                :board (copy-list board)
                :available-moves (copy-list available-moves)
                :color color))
        (search-depth 4))
    (dolist (i search-depth)
      ())))

(defun generate-children (node depth)
  "Generates all children of node down to a certain depth"
  (dolist (move (available-moves node))
    (make-instance 'minimax-node :board (copy-list (board node))
                   :color (if (equal (color node) 'W) 'B 'W)
                   :available-moves (copy-list (available-moves node)))
    (setf (children node) (append (children node) ))))

(defun board-evaluator (board color)
  "Evaluates the strength of color given a board.  The specifics of how this
  is done will change based on testing and what tends to make stronger AI."
  1)