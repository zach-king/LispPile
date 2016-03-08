(defun insert(X L)
    (cond ((null L) nil)
        (T (cons (cons X (car L)) (insert X (cdr L))))))

(defun power(set)
    (cond ((null set) '(nil))
        (T (append (insert (car set) (power (cdr set))) (power (cdr set))))))
