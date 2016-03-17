
(defun tentotwo (N)
    (cond 
        ((= N 0) NIL)
        (T (append (tentotwo (truncate (/ N 2))) (list (mod N 2))))))


(defun twototen (L) 
    (cond 
        ((NULL L) 0) 
        (T (+ (* 2 (twototen (butlast L))) (car (last L))))
    )
)

(defun mergesort (NLIST1 NLIST2)
    (merge 'list NLIST1 NLIST2 #'<))

(defun parens (L)
    (cond ((NULL L) NIL)
        (T (cons (car L) (list(parens (cdr L)))))))

(defun pairup (L) 
    (cond ((NULL L) NIL)
        (T (cons (append (list(car L)) (list (car (cdr L)))) (pairup (cdr (cdr L)))))))

(defun inc (NUMLIST)
    (cond ((NULL NUMLIST) NIL)
        (T (cons (+ 1 (car NUMLIST)) (inc (cdr NUMLIST))))))

(defun  solve (L)
    (cond
        ((equal (car L) '=) (car (cdr L)))
        ((equal (car L) '+) (- (car (cdr L)) (solve (cdr (cdr L)))))
        ((equal (car L) '-) (+ (car (cdr L)) (solve (cdr (cdr L)))))
        ((equal (car L) '*) (/ (car (cdr L)) (solve (cdr (cdr L)))))
        ((equal (car L) '/) (* (car (cdr L)) (solve (cdr (cdr L)))))))

(defun gt (X L)
    (cond ((NULL L) NIL)
        ((> (car L) X) (cons (car L) (gt X (cdr L))))
        (T (gt X (cdr L)))))

(defun le (X L)
    (cond ((NULL L) NIL)
        ((<= (car L) X) (cons (car L) (le X (cdr L))))
        (T (le X (cdr L)))))

(defun quicksort (L)
    (cond ((NULL (cdr L)) L)
        (T (append (quicksort (le (car L) (cdr L)))
           (list (car L))
           (quicksort (gt (car L) (cdr L)))) )))

(defun mirror (tree)
    (cond ((NULL tree) NIL)
        (T (list (car tree) (mirror (cdr (cdr tree))) (mirror (car (cdr tree))) ))))

(defun hanoi (n from to spare)
    (cond ((= n 0) nil)
        (T (hanoi (- n 1) from spare to) 
            (list 'move 'ring n 'from from 'to to)
            (hanoi (- n 1) spare to from))))