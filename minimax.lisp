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
                :color (if (equal color 'W) 'B 'W)
                :children nil
                :ko '(nil)))
        (search-depth 4))
    (generate-children root 3)
    (set-values root t)
    (print-children root 0)))

(defun generate-children (node depth)
  "Generates all children of node down to a certain depth"
  (dolist (move (available-moves node))
    (let ((child (make-instance 'minimax-node
                    :ko (copy-list (ko node))
                    :children nil
                    :board (copy-tree (board node))
                    :color (if (equal (color node) 'W) 'B 'W)
                    :available-moves (copy-tree (available-moves node)))))
      (make-move move (color child) (available-moves child) (ko child) (board child))
      (if (children node)
          (append-modify (children node) (list child))
          (setf (children node) (list child)))
      (if (> depth 1)
          (generate-children child (- depth 1))))))

(defun board-eval (board)
  "Evaluates the strength of color given a board.  The specifics of how this
  is done will change based on testing and what tends to make stronger AI."
  (random 1000))

(defun set-values (root max)
  "Sets the value of node and all descendents of the root using the minimax rules.
  We assume we start by maxing moves, then alternating moves and so on."
  (let ((best nil))
    (if (= (length (children root)) 0)
        (return-from set-values (setf (value root) (board-eval (board root)))))
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

(defun print-children (root depth)
  "Prints the boards of all the children of node and the available moves."
  ;(print (available-moves root))
  ;(print-board (board root))
  (princ (format nil "~A~A~%" (make-string (* 4 depth) :initial-element #\Space) (value root)))
  (dolist (child (children root))
    (print-children child (+ depth 1))))