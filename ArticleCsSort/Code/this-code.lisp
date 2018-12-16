;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;Coding;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun vmax (L)
  (let ((max (svref L 0)))
    (loop for i from 1 to (1- (length L)) do
	 (if (< max (svref L i)) (setf max (svref L i))))
    max))

(defun get_sequence (N)
  (cond
    ((= N 0) nil)
    (t (cons (ceiling (/ N  2))
	     (get_sequence (- N (ceiling (/ N  2))))))))

(defun binalgorithm (L)
  (get_sequence (vmax L)))

(defun divisor (L a x y)
  (let ((z y) (n x) (current 0) pr)
    (loop while (< n z) do
	 (if (>= (svref L n) a)
	     (progn
	       (setf pr t)
	       (loop while (and (< n z) (>= (svref L z) a)) do
		    (setf z (1- z)))
	       (setf current (svref L z))
	       (setf (svref L z) (svref L n))
	       (setf (svref L n) current)))
	 (setf n (1+ n)))
    (values z pr)))

(defun iter-sort (L S a x y)
  (cond
    ((>= x y) t)
   ((null S) t)
    (t
     (multiple-value-bind (z pr) (divisor L (+ a (car S)) x y)
       (if pr
	   (progn
	     (iter-sort L (cdr S) (+ a (car S)) z y) 
	     (iter-sort L (cdr S) a x (1- z)))
	   (iter-sort L (cdr S) a x y))))))

(defun begin-sort (L S)
  (iter-sort L S 0 0 (1- (length L))))

(defun cs-sort (L)
  (begin-sort L (binalgorithm L)))

;;;;;;;;;;;;;;;;;;;
;;;;;;Testing;;;;;;
;;;;;;;;;;;;;;;;;;;

(defun gen_vector (a b n)
  (let ((result (make-array n
			    :fill-pointer 0
			    :element-type 'number)))
    (loop for i from 1 to n do
	 (vector-push (+ a 1 (random (- b a))) result))
    (coerce result 'simple-vector)))

(defmacro test_time (method L &optional parameter)
  `(let ()
     (if (null ,parameter)
	 (time (funcall ,method ,L))
	 (time (funcall ,method ,L ,parameter)))
     t))

(defmacro test (a b n)
  `(let* ((L (gen_vector ,a ,b ,n))
	  (LL (copy-seq L))
	  (LLL (copy-seq LL)))
     (format t "Cs-sort algorithm. Amount of numbers: ~D. Capacity of numbers: ~D.~%" ,n ,b)
     (test_time 'cs-sort LL)
     (format t "Built-in stable algorithm. Amount of numbers: ~D. Capacity of numbers: ~D.~%" ,n ,b)
     (test_time 'stable-sort LLL '>)
     (format t "Built-in nonstable algorithm. Amount of numbers: ~D. Capacity of numbers: ~D.~%" ,n ,b)
     (test_time 'sort L '>)
     (makunbound 'L)
     (makunbound 'LL)
     (makunbound 'LLL)
     t))

(defparameter  amount ' (100000 500000 1000000 5000000))

(defparameter capacity '(1000000 10000000 100000000 500000000))

(defmacro all-tests ()
  `(loop for j in capacity do
       (format t " Capacity of numbers: ~D.~%" j)
	(loop for i in amount do
	     (format t " Count of numbers: ~D.~%" i)
	     (loop for k from 1 to 30 do
		  (format t " Iteration № ~D ~%" k)
		  (test  1 j i)))))

;;for save-and-die
(defun run () (all-tests))

;;Running: sbcl --disable-ldb --control-stack-size 800000
;;Loading: (load "this-code.lisp")
;;Make image: (sb-ext:save-lisp-and-die "my-core" :compression 9 :toplevel #'run :executable t)
;;./my-core>>test_result.txt
;;flex parser.l
;;cc lex.yy.c -o parser -lfl
