(defstruct node 
  (value)
  (left)
  (right)
)

(defun create_node (val)
    (make-node :value val :left nil :right nil)
)

(defun bst-cons (left tree right)
    (setf (node-left tree) left)
    (setf (node-right tree) right)
    tree
)

(defun insert (tree key)
    (if (null tree)
        (create_node key)
        (if (< key (node-value tree))
            (bst-cons (insert (node-left tree) key) tree (node-right tree))
            (bst-cons (node-left tree) tree (insert (node-right tree) key))
        )
    )
)

(setf tree nil)

(setf tree (insert tree 4))
(setf tree (insert tree 9))
(setf tree (insert tree 1))
(setf tree (insert tree 6))
(setf tree (insert tree 3))


(defun in-order-traversal (bst)
  (if (not (null bst))
      (progn
        (in-order-traversal (node-left bst))
        (print (node-value bst))
        (in-order-traversal (node-right bst)))))

(in-order-traversal tree)
