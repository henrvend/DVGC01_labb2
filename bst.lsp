(defstruct node
    elem
    (left nil)
    (right nil)
)



;; insert-funktion för bst, tar emot bst, key och jämförelseoperator
(defun bst-insert (bst key less-fn)
  (if (null bst)
      ;;make-node används för att skapa upp en struct node
      (make-node :elem key)
      ;; funcall används för att använda en medskickad funktion (#'<), en less-than funktion.
      ;; funcall måste användas, det fungerar inte att använda #'< direkt utan måste skickas med 
      (if (funcall less-fn key (node-elem bst))
          (if (null (node-left bst))              
              (setf (node-left bst) (make-node :elem key))
              (bst-insert (node-left bst) key less-fn))
          (if (null (node-right bst))
              (setf (node-right bst) (make-node :elem key))
              (bst-insert (node-right bst) key less-fn))      
        )
    )
)

(defun in-order-traversal (bst)
  (if (not (null bst))
      (progn
        (in-order-traversal (node-left bst))
        (print (node-elem bst))
        (in-order-traversal (node-right bst)))))



;; "#'<" skickas med som less-fn för att kunna 
;; bestämma större eller mindre, det måste finnas
;; då "<"" operatorn inte kan bestämma mot "nil"
;; Vill man få listan i omvänd ordning kan man 
;; istället använda ">"

(setf x (bst-insert nil 2 #'<))

(bst-insert x 4 #'<)
(bst-insert x 3 #'<)
(bst-insert x 8 #'<)
(bst-insert x 1 #'<)

(in-order-traversal x)