;; Uses a limited board with a small available-moveset just to demonstrate that
;; the minimax tree is generated properly.

(load "main.lisp")

(print "Working with the following board:")
(print-board (setf board '((W - - -) (- - - -) (- - - -) (- - - -))))
(print "The board evaluator applied to this board. We should expect 10 from:")
(print-board '((W 3 1 0) (3 2 0 0) (1 0 0 0) (0 0 0 0)))
(print "Here is the result from the evaluator:")
(print (board-eval board 'W))
(print "Working with a smaller set of available-moves (for a more readable tree).")
(print (setf available-moves '((0 1) (0 2) (1 0) (1 1) (1 2))))
(print "The minimax tree. The leftmost node is the root, and each indent
       indicates a level down.  The root's choice starts with a max, then the
       next choice is a min, then max.")
(minimax-move available-moves 'B board :examine-tree t)