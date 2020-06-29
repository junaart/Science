;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;Find generating sequence;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Lmax (L)
  (let ((max (car L)))
    (loop for i in (cdr L) do
	 (if (> i max) (setf max i)))
    max))

;;generating sequence N for a>=2
(defun get-sequence (N a)
  (cond
    ((= N 0) nil)
    (t (cons (ceiling (/ N  a))
	     (get-sequence (- N (ceiling (/ N  a))) a)))))

;;generating sequence N for a<2
(defun gen_pred(Smax c)
  (let ((current Smax) result)
    (loop while (> current 0) do
	 (if (/= (setf current (ceiling (/ (- current 1) c))) 0)
	     (push  current result)))
        result))

;;Common function for generating sequence
(defun gen-seq(N c)
  (cond
    ((< c 2)
     (nreverse (gen_pred N c)))
    (t (get-sequence N c))))

(defun cl-algorithm (L a)
  (gen-seq (Lmax L) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Find k-th statistics for lists;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iter-find(L S a k compare)
  (cond
    ((null S) (values compare (car L)))
    ((<= k 0) (values compare (car L)))
    ((null (cdr L)) (values compare (car L)))
    (t
     (multiple-value-bind (less more len-l)
	 (divisor-l L (+ a (car S)))
       (setf compare (+ compare (length L)))
       (if (>= len-l k)
	   (iter-find less (cdr S) a k compare)
	   (iter-find more (cdr S)
		      (+ a (car S)) (- k len-l) compare))))))

(defun divisor-l (L a)
  (let (less more (length_less 0))
    (loop for i in L do
	 (if (>= i a)
	     (push i more) 
	     (progn (push i less)
		    (setf length_less (1+ length_less)))))
    (values less more length_less)))

(defun cs-select (L S k)
  (iter-find L S 0 k 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Realization quickselect algorithm;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quickselect (L k compare)
  (cond
    ((and ( = k 1) (null (cdr L))) (values compare (car L)))
    (t (let (left right (len_left 0) (pivot (nth (random (length L)) L)))
	 (setf compare (+ compare (length L)))
	 (loop for i in L do
	      (if (< i pivot) (progn (push i left) (setf len_left (1+ len_left)))
		  (push i right)))
	 (if (<= k len_left) (quickselect left k compare)
	     (quickselect right (- k len_left) compare))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Testing and experiments;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen_list (a b n)
  (let (result)
    (loop for i from 1 to n do
	 (push (+ a 1 (random (- b a))) result))
    result))

(defun make_list(N)
  (let (result)
    (loop for i from 1 to N do
	 (setf result (cons  i result)))
    (nreverse result)))

;;Function generate permutatios for elements of list
(defun gen_permutation (n m &optional key)
  ;;n - count of elements
  ;;m - number of permutation
  ;;key - if key = nil then create vector, otherwise create list
  (let ((L1 (make_list n)) i j)
    (loop for k from 1 to m do
	 (setf i (random n))
	 (setf j (random n))
	 (if (not key)
	     (rotatef (nth i L1) (nth j L1))))	     
    L1))

(defun test (L S &optional p)
  (let* ((max 0) (min (Lmax L)) (avg 0) kmax kmin)
    (format t "cs-select~%")
    (loop for i from 1 to (length L) do
	 (multiple-value-bind (compare kth)
	     (cs-select L S i)
	   (if p (format t "Count of compares: ~d, ~d-th element: ~d~%" compare i kth))
	   (if (> min compare) (progn (setf min compare) (setf kmin i)))
	   (if (< max compare) (progn (setf max compare) (setf kmax i)))
	   (setf avg (+ avg compare))))
    (format t "Max count of compares ~d for kth = ~d ~% Min count of compares ~d for kth ~d ~% Average cout of compares ~f~%" max kmax min kmin (/ avg (length L)))
    (setf max 0)
    (setf min (Lmax L))
    (setf kmin nil)
    (setf kmax nil)
    (setf avg 0)
    (format t "quickselect~%")
    (loop for i from 1 to (length L) do
	 (multiple-value-bind (compare kth)
	     (quickselect L i 0)
	   (if p (format t "Count of compares: ~d, ~d-th element: ~d~%" compare i kth))
	   (if (> min compare) (progn (setf min compare) (setf kmin i)))
	   (if (< max compare) (progn (setf max compare) (setf kmax i)))
	   (setf avg (+ avg compare))))
    (format t "Max count of compares ~d for kth = ~d ~% Min count of compares ~d for kth ~d ~% Average cout of compares ~f~%" max kmax min kmin (/ avg (length L)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun testing (Nmax lower upper number)
  (let* ((L (gen_list lower upper Nmax)) (S (cl-algorithm L 2))
	 (n25 (ceiling (* 0.25 Nmax ))) (n50 (ceiling (* 0.5 Nmax)))
	 (n75 (ceiling (* 0.75 Nmax))) compare kth)
    (if (= number 1)
	(progn
	  (format t "Testing Q(X,1)~%")
	(multiple-value-bind (compare kth)
	    (time (cs-select L S 1))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))
	(multiple-value-bind (compare kth)
	    (time (quickselect L 1 0))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))))
    (if (= number 2)
	(progn
	  (format t "Testing Q(X,0.25n)~%")
	(multiple-value-bind (compare kth)
	    (time (cs-select L S n25))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))
	(multiple-value-bind (compare kth)
	    (time (quickselect L n25 0))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))))
    (if (= number 3)
	(progn
	  (format t "Testing Q(X,0.5n)~%")
	(multiple-value-bind (compare kth)
	    (time (cs-select L S n50))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))
	(multiple-value-bind (compare kth)
	    (time (quickselect L n50 0))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))))
    (if (= number 4)
	(progn
	  (format t "Testing Q(X,0.75n)~%")
	(multiple-value-bind (compare kth)
	    (time (cs-select L S n75))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))
	(multiple-value-bind (compare kth)
	    (time (quickselect L n75 0))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))))
    (if (= number 5)
	(progn
	  (format t "Testing Q(X,n)~%")
	(multiple-value-bind (compare kth)
	    (time (cs-select L S Nmax))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))
	(multiple-value-bind (compare kth)
	    (time (quickselect L Nmax 0))
	  (format t "Compare: ~d, kth: ~d~%" compare kth))))
    
    ))

(defun testing-compare (Nmax lower upper number)
  (let* ( n L S
	 (n25 (ceiling (* 0.25 Nmax ))) (n50 (ceiling (* 0.5 Nmax)))
	 (n75 (ceiling (* 0.75 Nmax))) compare kth (comp_avg_cs 0) (comp_avg_quick 0))
    (if (= number 1) (setf n 1))
    (if (= number 2) (setf n n25))
    (if (= number 3) (setf n n50))
    (if (= number 4) (setf n n75))
    (if (= number 5) (setf n Nmax))
    (loop for i from 1 to 100 do
	 (setf L (gen_list lower upper Nmax))
	 (setf S (cl-algorithm L 2))
	 (multiple-value-bind (compare kth)
	     (cs-select L S n)
	   (setf comp_avg_cs (+ comp_avg_cs compare)))
	 (multiple-value-bind (compare kth)
	     (quickselect L n 0)
	   (setf comp_avg_quick (+ comp_avg_quick compare))))
    (format t "Average cs-select: ~f, Average quickselect ~f~%" (float (/ comp_avg_cs 100)) (float (/ comp_avg_quick 100)))))
    
	 
    
(testing-compare 10 1 9999999999999999 1)
(testing-compare 10 1 9999999999999999 2)
(testing-compare 10 1 9999999999999999 3)
(testing-compare 10 1 9999999999999999 4)
(testing-compare 10 1 9999999999999999 5)

(testing-compare 100 1 9999999999999999 1)
(testing-compare 100 1 9999999999999999 2)
(testing-compare 100 1 9999999999999999 3)
(testing-compare 100 1 9999999999999999 4)
(testing-compare 100 1 9999999999999999 5)

(testing-compare 1000 1 9999999999999999 1)
(testing-compare 1000 1 9999999999999999 2)
(testing-compare 1000 1 9999999999999999 3)
(testing-compare 1000 1 9999999999999999 4)
(testing-compare 1000 1 9999999999999999 5)

(testing-compare 10000 1 9999999999999999 1)
(testing-compare 10000 1 9999999999999999 2)
(testing-compare 10000 1 9999999999999999 3)
(testing-compare 10000 1 9999999999999999 4)
(testing-compare 10000 1 9999999999999999 5)

(testing-compare 100000 1 9999999999999999 1)
(testing-compare 100000 1 9999999999999999 2)
(testing-compare 100000 1 9999999999999999 3)
(testing-compare 100000 1 9999999999999999 4)
(testing-compare 100000 1 9999999999999999 5)

(testing-compare 1000000 1 9999999999999999 1)
(testing-compare 1000000 1 9999999999999999 2)
(testing-compare 1000000 1 9999999999999999 3)
(testing-compare 1000000 1 9999999999999999 4)
(testing-compare 1000000 1 9999999999999999 5)

