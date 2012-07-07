;;; Utils

;; Range from start to end - 1
(defun range (&key (start 1) (end 10))
  (loop for i from start upto (1- end) collecting i))

;; Range using a do-loop - it's slower
(defun range-do (start end)
  (do ((x start (1+ x))
       (lst nil (push x lst)))
      ((= end x) (nreverse lst))))

;; Predicate for testing divisibility
(defun divides? (m n)
  (zerop (mod n m)))

(defun my-integer-p (n)
  (zerop (second (multiple-value-list (floor n)))))

;; Square function
(defun square (x)
  (* x x))

;; Predicate for testing squareness
(defun square? (s)
  (= s (square (isqrt s))))

;; Calculates nth root of m
(defun root (n m)
  (expt m (/ 1 n)))

;; Predicate for testing whether a number is the nth power of an integer base
(defun nth-power? (n m)
  (= m (expt (floor (root n m)) n)))

;; Concatenates two numbers
(defun concat-numbers (n &rest numbers)
  (parse-integer (reduce #'(lambda (x y) (concatenate 'string x y))
			 (mapcar #'write-to-string numbers)
			 :initial-value (write-to-string n))))

;; Fast exponentiation
(defun fast-exp (base exp)
  (cond ((zerop exp) 1)
	((evenp exp) (square (fast-exp base (/ exp 2))))
	(t (* base (fast-exp base (1- exp))))))

;; Find the maximum value in a list
(defun max-list (lst &key (key #'identity))
  (if (null lst) nil
      (extreme-list-helper lst (first lst) #'> :key key)))

;; Find the minimum value in a list
(defun min-list (lst &key (key #'identity))
  (if (null lst) nil
      (extreme-list-helper lst (first lst) #'< :key key)))

(defun extreme-list-helper (lst cur-min pred &key key)
  (cond ((null lst) cur-min)
	((funcall pred (funcall key (car lst)) (funcall key cur-min))
	 (extreme-list-helper (rest lst) (first lst) pred :key key))
	(t (extreme-list-helper (rest lst) cur-min pred :key key))))

;; List element equality
(defun set-equal (s1 s2 &key key (test #'eql))
  (and (subsetp s1 s2 :key key :test test)
       (subsetp s2 s1 :key key :test test)))

;; Determine if the string is a palindrome
(defun palindrome? (str)
  (string-equal str (reverse str)))

;; Determines if a number is a palindrome
(defun number-palindrome? (n)
  (palindrome? (write-to-string n)))

;; Determine if the number is prime
(defun prime-p (n)
  (cond ((< n 2) nil)
	((= n 2) t)
	(t (loop for i from 2 to (ceiling (sqrt n)) never (divides? i n)))))

;; Determines if two numbers are relatively prime
(defun relatively-prime? (n m)
  (= 1 (gcd n m)))

;; Calculates Euler's totient function
(defun slow-totient (n &optional (i 1) (acc 0))
  (cond ((= i n) acc)
	((relatively-prime? i n)
	 (totient n (1+ i) (1+ acc)))
	(t (totient n (1+ i) acc))))

(defun fast-totient (n)
  (* n (reduce #'* (mapcar (lambda (x) (- 1 (/ 1 x)))
			   (unique-prime-factors n)))))

(defun totient (n)
  (fast-totient n))

;; Generates a list of primes up to n
(defun generate-primes (n)
  (loop for i from 2 to n when (prime-p i) collect i))

;; Gets the next prime
(defun next-prime (n)
  (if (prime-p (1+ n)) (1+ n)
      (next-prime (1+ n))))

;; Generates a hash of primes up to n
(defun generate-primes-hash (n)
  (let ((primes-hash (make-hash-table)))
    (do ((i 2 (1+ i)))
	((>= i n) primes-hash)
      (when (prime-p i)
	(setf (gethash i primes-hash) t)))))

;; Put the digits of a number in a list, one element per digit
(defun digits (n)
  (map 'list (lambda (char) (parse-integer (string char))) (prin1-to-string n)))

(defun digits-array (n)
  (let ((arr (digits n)))
    (make-array (length arr) :initial-contents arr)))

;; Build a number from a list of digits
(defun digits-to-num (lst)
  (if (null lst) 0
    (+ (* (first lst) (expt 10 (1- (length lst)))) (digits-to-num (rest lst)))))

;; Reverses a number
(defun reverse-num (n)
  (digits-to-num (nreverse (digits n))))

;; Number of digits in a number
(defun num-digits (n)
  (if (zerop n) 1
      (floor (1+ (log n 10)))))

;; Last n digits in a number
(defun last-n-digits (n number)
  (rem number (expt 10 n)))

;; First n digits in a number
(defun first-n-digits (n number)
  (floor (float (/ number (expt 10 n)))))

;; Factorial
(defun factorial (n)
  (if (zerop n) 1
    (reduce #'* (range :end (1+ n)))))

;; Slower, but good to know
(defun factorial-do (n)
  (do ((j n (1- j))
       (f 1 (* j f)))
       ((= j 0) f)))

;; Fibonacci sequence up to and including term n
(defun fibo (n)
  (nreverse 
   (loop repeat n for x = 0 then y and y = 1 then (+ y x) collect y)))

;; Fibonacci sequence up to value less than n
(defun fibo-values (n)
  (nreverse
   (loop for x = 0 then y and y = 1 then (+ y x) until (>= y n) collect y)))

;; Get the factors of a number
(defun factors (n)
  (cond ((< n 1) ())
	((= 1 n) (list n))
	((oddp n) (remove-duplicates (loop for m from 1 to (sqrt n) by 2
				   when (divides? m n)
					   nconc (list m (/ n m)))))
	(t (remove-duplicates (loop for m from 1 to (sqrt n)
				    when (divides? m n)
				    nconc (list m (/ n m)))))))

;; Gets the prime factors of a number
(defun prime-factors (n &optional (start 3))
  (if (evenp n) (cons 2 (prime-factors (/ n 2)))
    (when (> n 1)
      (do ((x start (incf x 2)))
	  ((zerop (mod n x))
	   (cons x (prime-factors (/ n x) x)))))))

;; Gets the unique prime factors
(defun unique-prime-factors (n)
  (remove-duplicates (prime-factors n)))

;; The proper divisors of n, ie, all factors of n that are less than n
(defun proper-divisors (n)
  (remove n (factors n)))

;; Determines if m is a power of n
(defun power-of-p (m n)
  (zerop (- (floor (log m n)) (log m n))))

;; Deterimines if n is pandigital
(defun pandigital? (n)
  (and (subsetp (digits n) 
		(range :end (1+ (length (digits n)))))
       (subsetp (range :end (1+ (length (digits n))))
		(digits n))))

;; Determines if n is pandigital of a certain length
(defun pandigital-length? (n len)
  (and (= (length (digits n)) len)
       (pandigital? n)))

;; nth triange number
(defun triangle-number (n)
  (/ (* n (+ 1 n)) 2))

;; triangle numbers up to limit
(defun triangle-numbers (limit &optional (n 1) (acc nil))
  (if (> (triangle-number n) limit) (nreverse acc)
    (progn
      (push (triangle-number n) acc)
      (triangle-numbers limit (1+ n) acc))))

(defun triangle-numbers-orig (limit &optional (n 1))
  (if (> (triangle-number n) limit) nil
    (append (list (triangle-number n))
	    (triangle-numbers limit (1+ n)))))

;; square numbers up to limit
(defun square-numbers (limit &optional (n 1))
  (if (> (square n) limit) nil
      (append (list (square n))
	      (square-numbers limit (1+ n)))))

;; nth pentagonal number
(defun pentagonal-number (n)
  (/ (* n (- (* 3 n) 1)) 2))

;; pentagonal numbers up to limit
(defun pentagonal-numbers (limit &optional (n 1))
  (if (> (pentagonal-number n) limit) nil
    (append (list (pentagonal-number n)) 
	    (pentagonal-numbers limit (1+ n)))))

;; determines if the number is pentagonal (stack overflow - no tail recursion :( )
(defun pentagonal?-recur (n &optional (p 0))
  (cond ((= n (pentagonal-number p)) t)
	((> (pentagonal-number p) n) nil)
	(t (pentagonal? n (1+ p)))))

;; iterative version with no stack overflow problems
(defun pentagonal?-iter (n &optional (ip 1))
  (do ((ip ip (1+ ip))
       (p (pentagonal-number ip) (pentagonal-number ip)))
      ((>= p n) (= p n))))

;; mathematical version
;; n = (sqrt(24m + 1) + 1) / 6
;; if n is a natural number, then m is the nth pentagonal number
(defun pentagonal? (m)
  (let ((n (/ (+ 1 (sqrt (+ 1 (* 24 m)))) 6)))
    (if (my-integer-p n) (values t (floor n)) (values nil 0))))

;; nth hexagonal number
(defun hexagonal-number (n)
  (* n (- (* 2 n) 1)))

;; hexagonal numbers up to a limit
(defun hexagonal-numbers (limit &optional (n 1))
  (if (> (hexagonal-number n) limit) nil
    (append (list (hexagonal-number n)) 
	    (hexagonal-numbers limit (1+ n)))))

;; nth heptagonal number
(defun heptagonal-number (n)
  (/ (* n (- (* 5 n) 3)) 2))

;; heptagonal numbers up to a limit
(defun heptagonal-numbers (limit &optional (n 1))
  (if (> (heptagonal-number n) limit) nil
    (append (list (heptagonal-number n)) 
	    (heptagonal-numbers limit (1+ n)))))

;; nth octogonal number
(defun octogonal-number (n)
  (* n (- (* 3 n) 2)))

;; octogonal numbers up to a limit
(defun octogonal-numbers (limit &optional (n 1))
  (if (> (octogonal-number n) limit) nil
    (append (list (octogonal-number n)) 
	    (octogonal-numbers limit (1+ n)))))

;; letter value compared to 'A' (case insensitive)
(defun letter-value (c &optional (compared-to #\A))
  (if (alpha-char-p c)
      (1+ (- (char-code (char-upcase c)) (char-code compared-to)))
    0))

;; word value
(defun word-value (w)
  (loop for c across w summing (letter-value c)))

;; removes the first instance of an item in a sequence
(defun remove-first (elt seq)
  (cond ((equal elt (first seq))
	 (rest seq))
	(t (cons (first seq) (remove-first elt (rest seq))))))

;; get the nth lexicographical permutation of a list, p
(defun permute (n p)
  (permute-helper n p))

(defun permute-helper (n p)
  (let ((f (factorial (length p))))
    (cond ((zerop n) p)
	  ((= n f) (reverse p))
	  ((> n f) (permute-helper (mod n (* f (1+ (length p)))) p))
	  (t (multiple-value-bind (d r) (floor n (/ f (length p)))
	       (cons (nth d p) (permute-helper r (remove (nth d p) p :count 1))))))))
  
;; get all permutations of a list
(defun permutations (lst)
  (let ((result nil))
    (dotimes (i (factorial (length lst)) result)
      (setf result (push (permute i lst) result)))))

(defun permutation-p (n p)
  (equalp (sort (digits n) #'<)
	  (sort (digits p) #'<)))

;; powerset
(defun powerset (set)
  (reduce #'(lambda (item ps)
	      (append (mapcar #'(lambda (e) (cons item e))
			      ps)
		      ps))
	  set
	  :from-end t
	  :initial-value '(())))

;; n choose r
(defun ncr (n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

;; http://coding.derkeiler.com/Archive/Lisp/comp.lang.lisp/2003-10/0016.html
(defun string-split (split-string string)
  "Returns a list containing items in 'string' split from
occurrences of 'split-string'."
  (loop with l = (length split-string)
	for n = 0 then (+ pos l)
	for pos = (search split-string string :start2 n)
	if pos collect (subseq string n pos)
	else collect (subseq string n)
	while pos))

;;; Vector operations
;; vector addition
(defun vector+ (&rest vectors)
  (apply #'mapcar #'+ vectors))

;; vector subtraction
(defun vector- (&rest vectors)
  (apply #'mapcar #'- vectors))

;; Dot product
(defun dot-product (v1 v2)
  (reduce #'+ (mapcar #'* v1 v2)))

;; Cross product
(defun cross-product (v1 v2)
  "Cross product of 2 3D vectors"
  (list (- (* (second v1) (third v2)) (* (third v1) (second v2)))
	(- (* (third v1) (first v2)) (* (first v1) (third v2)))
	(- (* (first v1) (second v2)) (* (second v1) (first v2)))))
  

;;; Problem 1 - Sum multiples of 3 or 5 below 1000
;;  Example: (euler1 (1000))

(defun euler1 (&optional (x 1000))
  (euler1-helper 0 x 0))

(defun euler1-helper (n x sum)
  (if (< n x)
      (if (or (divides? 3 n) (divides? 5 n))
	  (euler1-helper (1+ n) x (+ n sum))
	(euler1-helper (1+ n) x sum))
    sum))

(defun euler1-loop (x)
  (loop for x from 1 below x 
	when (or 
	      (zerop (mod x 3))
	      (zerop (mod x 5))) sum x))

;;; Problem 2 - Sum of even-valued terms under 4,000,000 in Fibonacci sequence
;;  Example: (euler2 (2 1) 4000000)

(defun euler2 (&optional (max 4000000))
  (reduce #'+ (remove-if #'oddp (fibo-values max))))

(defun euler2-orig (lst max)
  (apply #'+ (remove-if #'oddp (fib-list lst max))))

(defun fib-list (lst max)
  (if (>= (first lst) max) (rest lst)
    (fib-list (append-fib lst) max)))

(defun append-fib (lst)
  (push (+ (first lst) (second lst)) lst))


;;; Problem 3 - Largest prime factor of a number
;; Example: (euler3 600851475143)

(defun euler3 (n)
  (fermat-factor n))

(defun fermat-factor (n)
  (fermat-factor-helper n (ceiling (sqrt n))))

(defun fermat-factor-helper (n a)
  (let ((bsq (- (* a a) n)))
    (if (square? bsq) (values (- a (sqrt bsq)) (+ a (sqrt bsq)))
      (fermat-factor-helper n (1+ a)))))


;;; Problem 4 - Largest panlindrome made from product of 2 3-digit numbers

(defun product-list (start end j)
  (loop for i from start to end collecting (* i j)))

(defun all-products (start end)
  (loop for i from start to end appending (product-list start i i)))

(defun euler4 ()
  (apply #'max (remove-if-not #'number-palindrome? (all-products 100 999))))
    

;;; Problem 5 - Smallest number divisible by 1-20

(defun euler5-lcm (start end)
  (apply #'lcm (loop for i from start to end collect i)))

(defun euler5 (start end)
  (do ((i end (1+ i)))
      ((divisible-by-range i start end) i)))

(defun divisible-by-range (n start end)
  (do ((i start (1+ i)))
       ((or (> i end) (/= 0 (mod n i)))
	(if (> i end) t nil))))

;;; Problem 6 - Difference between square of sums and sum of squares of a range
;; Pencil/paper:
;; Sum of first n natural numbers = n(n+1)/2
;; Square of sum = n^2(n+1)^2 / 4
;; Sum of squares = n(n+1)(2n+1)/6
;; Example: (euler6 1 100)

(defun euler6 (start end)
  (- (square (apply #'+ (range start end)))
     (apply #'+ (loop for i from start to end collecting (square i)))))

(defun euler6-mathy (n)
  (- (/ (* (square n) (square (+ n 1))) 4)
     (/ (* n (+ n 1) (+ (* 2 n) 1)) 6)))

;; Loop method
(defun square-diff (limit)
  (let (diff)
    (loop for i from 1 to limit
	  sum i into nums
	  sum (* i i) into squares
	  finally (setq diff (- (* nums nums) squares)))
    diff))

;;; Problem 8 - Greatest product of 5 consecutive digits in 1000-digit number
;; Example: (euler 8 5 number)

(defun euler8 (seq-size number)
  (apply #'max (all-seq-products seq-size (if (stringp number) number (write-to-string number)))))

(defun all-seq-products (seq-size number-string)
  (loop for i from 0 to (- (length number-string) seq-size) collecting 
	(seq-product (subseq number-string i (+ 5 i)))))

(defun seq-product (string-seq)
  (apply #'* (loop for x across string-seq collecting (parse-integer (coerce (list x) 'string)))))

;;; Problem 9 - Only Pythagorean triplet where a + b + c = 1000
(defun euler9 (&optional (sum 1000))
  (loop for x from 1 to sum thereis (check-triplets x sum)))

(defun check-triplets (x sum)
  (loop for y from 1 to sum do 
	(if (pythagorean-triplet? x y (- sum (+ x y))) 
	    (return (* x y (- 1000 (+ x y)))))))

(defun pythagorean-triplet? (x y z)
  (let* ((c (max x y z))
	 (ab (remove c (list x y z))))
    (if (< (length ab) 2) nil
      (= (square c) (+ (square (first ab)) (square (second ab)))))))
	

;;; Problem 10 - Sum of primes below 2 million
(defun euler10 (&optional (end 2000000))
  (loop for i from 1 below end when (prime-p i) summing i))

;;; Problem 11
(defparameter *euler11-grid* 
  #2a((08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
      (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
      (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
      (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
      (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
      (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
      (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
      (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
      (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
      (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
      (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
      (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
      (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
      (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
      (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
      (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
      (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
      (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
      (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
      (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))

(defun euler11 (&optional (grid *euler11-grid*))
  (apply #'max (loop for x from 0 to (1- (array-dimension grid 0)) collect
		     (loop for y from 0 to (1- (array-dimension grid 1))
			   maximize (euler11-max-product-position x y grid)))))

(defun euler11-max-product-position (row col &optional (grid *euler11-grid*))
  (max (euler11-product-right row col grid)
       (euler11-product-down row col grid)
       (euler11-product-diag-right row col grid)
       (euler11-product-diag-left row col grid)))

(defun euler11-product-right (row col &optional (grid *euler11-grid*))
  (if (array-in-bounds-p grid row (+ 3 col))
       (* (aref grid row col)
	  (aref grid row (+ 1 col))
	  (aref grid row (+ 2 col))
	  (aref grid row (+ 3 col)))
      0))

(defun euler11-product-down (row col &optional (grid *euler11-grid*))
  (if (array-in-bounds-p grid (+ 3 row) col)
      (* (aref grid row col)
	 (aref grid (+ 1 row) col)
	 (aref grid (+ 2 row) col)
	 (aref grid (+ 3 row) col))
    0))

(defun euler11-product-diag-right (row col &optional (grid *euler11-grid*))
  (if (array-in-bounds-p grid (+ 3 row) (+ 3 col))
      (* (aref grid row col)
	 (aref grid (+ 1 row) (+ 1 col))
	 (aref grid (+ 2 row) (+ 2 col))
	 (aref grid (+ 3 row) (+ 3 col)))
    0))

(defun euler11-product-diag-left (row col &optional (grid *euler11-grid*))
  (if (array-in-bounds-p grid (+ 3 row) (- col 3))
      (* (aref grid row col)
	 (aref grid (+ 1 row) (- col 1))
	 (aref grid (+ 2 row) (- col 2))
	 (aref grid (+ 3 row) (- col 3)))
    0))
	       

;;; Problem 12 - First triangle number with over 500 divisors
(defun euler12 (&optional (num-factors 500) (i 1))
  (loop for i from 1 when (> (length (factors (triangle-number i))) num-factors) return i))

;;; Problem 13 - First 10 digits of sum of 100 50-digit numbers
;; Example: (euler13 "euler13.txt" 10)
(defun euler13 (&optional (file-name "euler13.txt") (first-n 10))
  (parse-integer 
   (format nil "~{~a~}" 
	   (subseq 
	    (digits (with-open-file (in file-name) 
				    (loop for line = (read-line in nil) 
					  while line summing (parse-integer line)))) 0 first-n))))

;;; Problem 14 -
(defun euler14 (&optional (limit 1000000) (fn #'fn14))
  (do ((i 1 (1+ i)) (sl 0) (max 0) (argmax 0))
      ((> i limit) argmax)
    (setf sl (seq-length i fn))
    (if (> sl max)
	(progn (setf max sl) (setf argmax i)))))

(defun seq-length (n fn)
  (do ((i 0 (1+ i)) (r (funcall fn n) (funcall fn r)))
      ((= r 1) (1+ i))))

(defun fn14 (n)
  (if (evenp n) (/ n 2) (+ 1 (* 3 n))))

;; Complete loop method
(defun collatz-sequence (start)
  (loop for n = start then (if (evenp n)
			       (/ n 2)
			     (1+ (* n 3)))
	collect n
	until (= 1 n)))
 
(defun problem-14 ()
  (loop for x from 1000000 downto 1
	and prev-max = 0 then max
	and num = x then (if (> max prev-max) x num)
	maximizing (length (collatz-sequence x)) into max
	finally (return num)))

;;; Problem 15 - Number of routes from top-left to bottom-right in 20x20 grid
;; Need to move right 20 times and down 20 times in any combination
;; Thus, 40 slots to fill with 2 types of object, 20 Rs, 20 Ds:
;;                               40! / (20! * 20!)
(defun euler15 (&optional (dim 20))
  (/ (factorial (* 2 dim)) (square (factorial dim))))

;;; Problem 16 - Sum of digits in 2^1000
(defun euler16 (&optional (base 2) (exp 1000))
  (reduce #'+ (digits (expt base exp))))

(defun euler16-loop (&optional (base 2) (exp 1000))
  (loop for x across (write-to-string (expt base exp))
	summing (parse-integer (coerce (list x) 'string))))

;;; Problem 17 - Sum of letters in "one" to "one thousand"
(defun euler17 (&optional (limit 1000))
  (loop for i from 1 to limit summing (count-letters i)))

(defun euler17-norvig ()
  (+ (* 3 9 99)
     (count-if #'alpha-char-p 
	       (format nil "~{~r ~}" 
		       (loop for i from 1 to 1000 collect i)))))

;; Counts the letters in the written form of n, adding "and" to numbers
;; that take it in British English (eg, three-hundred and forty-two)
(defun count-letters (n)
  (if (and (> n 100) (not (zerop (mod n 100))))
      (+ 3 (length (remove-spaces-and-hyphens (format nil "~r" n))))
    (length (remove-spaces-and-hyphens (format nil "~r" n)))))

(defun remove-spaces-and-hyphens (s)
  (remove #\space (remove #\- s)))

;;; Problem 18
(defparameter *euler18-triangle*
  '((75)
  (95 64)
  (17 47 82)
  (18 35 87 10)
  (20 04 82 47 65)
  (19 01 23 75 03 34)
  (88 02 77 73 07 63 67)
  (99 65 04 28 06 16 70 92)
  (41 41 26 56 83 40 80 70 33)
  (41 48 72 33 47 32 37 16 94 29)
  (53 71 44 65 25 43 91 52 97 51 14)
  (70 11 33 28 77 73 17 78 39 68 17 57)
  (91 71 52 38 17 14 91 43 58 50 27 29 48)
  (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
  (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

(defparameter *euler18-test*
  '((3)
    (7 5)
    (2 4 6)
    (8 5 9 3)))

(defun euler18 (&optional (triangle *euler18-triangle*))
  (max-list (flatten-triangle triangle)))

(defun flatten-triangle (triangle)
  (let ((result nil))
    (dotimes (i (length triangle) result)
      (setf result (flatten-triangle-row i (nth i triangle) result)))))


(defun flatten-triangle-row (n cur-row parent-row)
  (let ((flat-row nil))
    (dotimes (i (length cur-row) flat-row)
      (multiple-value-bind (lp rp) (triangle-parents n i parent-row)
	(setf flat-row 
	      (append flat-row (list (max (+ lp (nth i cur-row)) 
					  (+ rp (nth i cur-row))))))))))
      

(defun triangle-parents (row col parent-row)
  (cond ((<= row 0) (values 0 0))
	((= col 0) (values (first parent-row) 0))
	((= col (length parent-row))
	 (values 0 (nth (1- col) parent-row)))
	(t (values (nth (1- col) parent-row)
		   (nth col parent-row)))))
	 

  

;;; Problem 19 - Number of Sundays starting months in 20th century
(defun euler19 (&optional (start-year 1901) (end-year 2000) (day-of-week 6))
  (loop for year from start-year to end-year summing
	(loop for month from 1 to 12 
	      count (= day-of-week (month-start-day year month)))))

(defun month-start-day (year month)
  (nth 6 (multiple-value-list (decode-universal-time
			       (encode-universal-time 0 0 0 1 month year)))))

;;; Problem 20 - Sum of digits in 100!
(defun euler20 (&optional (n 100))
  (apply #'+ (digits (factorial n))))

;;; Problem 21 - Sum of amicable numbers under 10000
(defun euler21 (&optional (n 10000))
  (loop for i from 1 to n
	for d = (sum-of-proper-divisors i)
	when (and (> i d) (= i (sum-of-proper-divisors d)))
	sum i))

(defun amicable-pair? (n m)
  (and (/= m n)
       (= m (sum-of-proper-divisors n))
       (= n (sum-of-proper-divisors m))))

(defun sum-of-proper-divisors (n)
  (apply #'+ (proper-divisors n)))

;;; Problem 22 - Sum of name scores
(defun euler22 (&optional (filename "euler22.txt"))
  (let ((sum 0)
	(names nil))
    (with-open-file (in filename)
		    (setf names (sort (string-split "," (read-line in)) #'string<)))
    (dotimes (i (length names) sum) 
      (incf sum (score-listed-name (nth i names) (1+ i))))))

(defun score-listed-name (name pos)
  (* (score-name name) pos))

(defun score-name (name)
  (loop for c across name when (alpha-char-p c) summing (1+ (- (char-code (char-upcase c)) (char-code #\A)))))

;;; Problem 23 - Sums of abundant numbers (24 is the smallest such sum)
(defun euler23 ()
  (loop for i from 24 to 28123 when (sum-of-abundant-numbers? i) sum i))

(defun sum-of-abundant-numbers? (n)
  (let ((summands (if (evenp n) (list (/ n 2) (/ n 2))
		    (list (floor (/ n 2)) (ceiling (/ n 2))))))
    (dotimes (i (first summands))
      (if (every #'abundant-number-p summands) (return-from sum-of-abundant-numbers? summands)
	(progn (decf (first summands)) (incf (second summands)))))))
   
(defun sums-of-abundant-numbers ()
  (let ((abundants (abundant-numbers)))
    (loop for i from 0 to (length abundants) collect
	  (loop for j from 0 to i collect (+ (nth i abundants)
					     (nth j abundants))))))
 
(defun abundant-numbers (&optional (limit 28123))
  (loop for i from 2 to limit when (abundant-number-p i) collect i))

(defun abundant-number-p (n)
  (> (apply #'+ (proper-divisors n)) n))

;;; Problem 24 - Millionth lexicographic permutation of [0-9]
(defun euler24 (&optional (n 999999) (p '(0 1 2 3 4 5 6 7 8 9)))
  (permute n p))

;;; Problem 25 - First Fibonacci term with 1000 digits
(defun euler25 (&optional (i 1) (num-digits 1000))
  (if (>= (length (digits (first (fibo i)))) num-digits) i (euler25 (1+ i) num-digits)))

;;; Porblem 26 - inverse with longest repeating decimal
(defun euler26 (&optional (limit 1000))
  (let ((max-len 0)
	(max-d 2)
	(cur-len 0))
    (dotimes (d (- limit 2) max-d)
      (setf cur-len (find-recurrence-length (+ 2 d)))
      (if (> cur-len max-len)
	  (progn (setf max-len cur-len) (setf max-d (+ 2 d)))))))

(defun find-recurrence-length (d)
  (let ((e 1)
	(used-e nil)
	(steps 0))
    (do () ((> e d)) (setf e (* 10 e)))
    (do () ((and (> steps 0) (member e used-e)) steps)
      (push e used-e)
      (setf e (- e (* d (truncate (/ e d)))))
      (if (zerop e) (return-from find-recurrence-length 0))
      (do () ((> e d)) (setf e (* 10 e)))
      (incf steps))))
  

;;; Problem 27 - Primes generated by quadratic expressions
(defun euler27 ()
  (let ((max-a -999)
	(max-b -999)
	(chain-len 0)
	(max-chain 0))
    (do ((a -999 (1+ a))) ((= a 1000))
      (do ((b -999 (1+ b))) ((= b 1000))
	(setf chain-len (quadratic-prime-sequence a b))
	(if (> chain-len max-chain)
	    (progn (setf max-a a) 
		   (setf max-b b) 
		   (setf max-chain chain-len)))))
    (* max-a max-b)))

(defun quadratic-prime-sequence (a b &optional (n 0) (acc 0))
  (if (not (prime-p (+ (* n n) (* a n) b))) acc
      (quadratic-prime-sequence a b (1+ n) (1+ acc))))

;;; Problem 28 - Sum of diagonals in spiral
(defun euler28 (&optional (diag 1001))
  (1+ (loop for i from 2 to diag by 2
	for rotations from 1 by 2
	for even = 2 then (+ even 2)
	for last = 1 then (expt rotations 2)
	summing (sum-level last even))))

(defun sum-level (last even)
  (loop for i from 1 to 4 summing (+ last (* even i))))

;;; The formula for the sum of the 4 corners is given by: 4n^2 - 6n +6
;;  where n is the dimension of the spiral
(defun euler28-formula (&optional (diag 1001))
  (1+ (loop for i from 3 to diag by 2 summing (+ 6 (- (* 4 (expt i 2)) (* 6 i))))))

;;; Problem 29 - Distinct a^b, 2 <= a, b <= 100
(defun euler29 (&optional (min 2) (max 100))
  (length (remove-duplicates (all-bases-powers-list min max))))

(defun all-bases-powers-list (min max)
  (loop for i from min to max nconcing (powers-list i min max)))

(defun powers-list (a start end)
  (loop for i from start to end collecting (expt a i)))

;;; Problem 30 - Sum of 5th powers of digit
(defun euler30 (&optional (power 5))
  (loop for i from 10 to 1000000 when (= i (sum-of-powers i power)) collect i))

(defun sum-of-powers (n pow)
  (apply #'+ (mapcar (lambda (x) (expt x pow)) (digits n))))

;;; Problem 31 - Currency combinations
(defun euler31 (&optional (sum 200) (coins '(200 100 50 20 10 5 2 1)))
  (cond ((zerop sum) 1)
	((< sum 0) 0)
	((and (null coins) (>= sum 1)) 0)
	(t (+ (euler31 sum (rest coins)) (euler31 (- sum (first coins)) coins)))))

;; Dynamic programming version of the solution - ~100x faster!
(defun euler31-dyn (&optional (sum 200) (coins '(1 2 5 10 20 50 100 200)))
  (let ((table (make-array (list (1+ sum) (1+ (length coins))) 
			   :initial-element 0)))
    (dotimes (i (1+ (length coins)))
      (setf (aref table 0 i) 1))
    (loop for i from 1 to sum do
	  (loop for j from 1 to (length coins) do
		(if (>= (- i (nth (1- j) coins)) 0)
		    (setf (aref table i j) (+ (aref table i (1- j))
					      (aref table (- i (nth (1- j) coins)) j)))
		  (setf (aref table i j) (aref table i (1- j))))))
    (aref table sum (length coins))))
    

;;; Problem 32 - Pandigital multiplicand/multiplier/product numbers
(defun euler32 ()
  (let ((product-hash (make-hash-table)))
    (do ((i 1 (1+ i))) ((= i 99))
      (do ((j 100 (1+ j))) ((= j 9999))
	(if (and (null (gethash (* i j) product-hash))
		 (pandigital-series? i j (* i j)))
	    (setf (gethash (* i j) product-hash) (list i j)))))
    ;(loop for k being the hash-keys in product-hash using (hash-value v) do
	;  (format t "~a: ~a ~a~%" k (first v) (second v)))))
    (loop for k being the hash-keys in product-hash sum k)))

(defun pandigital-series? (multiplicand multiplier product)
  (pandigital-length? (digits-to-num (append (digits multiplicand)
					     (digits multiplier)
					     (digits product)))
		      9))
		

;;; Problem 33
(defun euler33 ()
  (expt (reduce #'*
		(mapcar #'(lambda (lst) (/ (first lst) (second lst)))
			(euler33-fractions))) -1))
(defun euler33-fractions ()
  (loop for i from 10 to 99 append 
	(loop for j from (1+ i) to 99 
	      when (and (< i j) 
			(not (zerop (mod i 10))) 
			(not (zerop (mod j 10))) 
			(= (mod i 10) (floor (/ j 10))) 
			(= (/ i j) (/ (floor (/ i 10)) (mod j 10))))
	      collect (list i j))))

;;; Problem 34 - Sum of factorials of digits
(defun euler34 ()
  (loop for i from 10 to (* 6 (factorial 9)) when (= i (sum-of-factorials i)) sum i))

(defun sum-of-factorials (n)
  (apply #'+ (mapcar #'factorial (digits n))))

;;; Problem 35 - Circular primes
(defun euler35 (&optional (limit 1000000))
  (let ((num-hash (make-hash-table)))
    (do ((n 2 (1+ n)))
	((> n limit))
      (if (and (null (gethash n num-hash))
	       (circular-prime? n))
	  (dolist (m (digit-rotations n))
	    (setf (gethash m num-hash) t))))
    (hash-table-count num-hash)))
	  

(defun circular-prime? (n)
  (every #'prime-p (digit-rotations n)))

(defun digit-rotations (n)
  (let ((rotations nil)
	(digits (digits n)))
    (dotimes (i (length digits) rotations)
      (push (digits-to-num digits) rotations)
      (setf digits (rotate-digits digits)))))

(defun rotate-digits (digits)
  (append (rest digits) (list (first digits))))

;; Problem 36 - Palindromes in base 2 and 10
(defun euler36 (&optional (max 1000000))
  (loop for i from 1 to max by 2 when 
	(and (palindrome? (format nil "~b" i))
	     (number-palindrome? i))
	summing i))

;; Problem 37 - Truncatable primes
(defun euler37 ()
  (let ((tprimes nil))
    (do ((i 10 (1+ i)))
	((= 11 (length tprimes)))
      (when (truncatable-left-and-right i) (setf tprimes (cons i tprimes))))
    (apply #'+ tprimes)))
      

(defun truncatable-left-and-right (n)
  (and (> n 10) (truncatable-left n) (truncatable-right n)))

(defun truncatable-left (n)
  (if (and (prime-p n) (> n 10)) 
      (truncatable-left (digits-to-num (rest (digits n))))
    (prime-p n)))

(defun truncatable-right (n)
  (if (and (prime-p n) (> n 10)) 
      (truncatable-right (digits-to-num 
			  (reverse (rest 
				    (reverse (digits n))))))
    (prime-p n)))

;;; Problem 38 - Pandigitals formed by multiplying a number by (1,2,...,n), n > 1
(defun euler38 ()
  (let ((max 918273645))
  (do ((i 10 (1+ i))
       (conc 0 (product-conc i 9))
       (max max (if (and (> conc max)
			 (pandigital? conc))
		    conc 
		  max)))
      ((>= i 10000) max))))
       

(defun product-conc (n len)
  (do ((i 1 (1+ i))
       (digits nil (append digits (digits (* n i)))))
      ((or (and (not (null digits))
		(< (first digits) len))
	   (>= (length digits) len))
       (if (> (length digits) len) 0 (digits-to-num digits)))))

;;; Problem 39 - Pythagorean triples given a perimeter
;;               Note: all right triangles have even perimeters
(defun euler39 (&optional (p 1000))
  (let ((cur-sols 0)
	(max-sols 0)
	(max-p 0))
    (do ((n 2 (+ 2 n))) ((= n p) max-p)
      (setf cur-sols (length (pythag-perimeter n)))
      (if (> cur-sols max-sols)
	  (progn
	    (setf max-sols cur-sols)
	    (setf max-p n))))))

(defun pythag-perimeter (perimeter)
  (let ((solutions nil))
    (dotimes (i perimeter solutions)
      (do ((j (1+ i) (1+ j))) ((= j perimeter))
	(if (and (= perimeter (+ i j (- perimeter (+ i j))))
		 (pythag-triple? i j (- perimeter (+ i j))))
	    (if (not (member (list i j (- perimeter (+ i j))) solutions 
				   :test #'(lambda (x y) 
					     (null 
					      (set-difference x y)))))
		     (push (list i j (- perimeter (+ i j))) solutions)))))))
	      
    

(defun pythag-triple? (a b c)
  (and (not (some #'zerop (list a b c)))
       (= (square c) (+ (square a) (square b)))))

;;; Problem 40 - Product of certain digits in the irrational number:
;;  0.123456789101112131415161718192021...
(defun euler40 ()
  (loop with product = 1 and next = 1
	for num-digits = 0 then (+ num-digits (num-digits i))
	for i from 1 until (>= num-digits 1000000) 
	do (if (>= (+ num-digits (num-digits i)) next)
	       (progn 
		 (setf product (* product 
				  (get-power-digit (digits i) 
						   num-digits next))) 
		 (setf next (* 10 next)))) 
	finally (return product)))

(defun get-power-digit (digits num-digits next)
  (nth (- next num-digits 1) digits))

;; Solution on the forums
(defun euler40-forum (&optional (digits '(1 10 100 1000 10000 100000 1000000)))
  (apply '* (mapcar 'nth-digit digits)))
 
(defun nth-digit (n &optional (d 1))
  "returns n-th digit (from 1) of fractional part"
  (let ((td (* 9/10 d (expt 10 d))))
    (if (<= n td)
        (ith-digit (+ (floor (1- n) d) (expt 10 (1- d))) (mod (1- n) d))
        (nth-digit (- n td) (1+ d)))))
 
(defun ith-digit (n i)
  "returns i-th digit (from 0) of integer n"
  (- (char-int (char (format nil "~A" n) i)) (char-int #\0)))


;;; Problem 41 - Largest pandigital prime (8- and 9-digit pandigitals cannot be
;;               prime because the sums of their digits are multiples of 3
;;               (36 and 45, respectively). In fact, 7 and 4 are the only
;;               possibilities - (1 - 1, 2 - 3, 3 - 6, 4 - 10, 5 - 15, 6 - 21, 7 - 28)
(defun euler41 ()
  (let ((permutations nil))
    (do ((i 7 (1- i))) 
	((= 0 i))
      (setf permutations (sort (mapcar #'digits-to-num (permutations (range 1 i))) #'>))
      (dolist (p permutations)
	(if (prime-p p) (return-from euler41 p))))))

;;; Problem 42 - Triangle words
(defun euler42 (&optional (filename "euler42.txt"))
  (let* ((word-values (mapcar #'word-value (load-words-42 filename)))
	 (triangles (triangle-numbers (max-list word-values))))
    (count-if #'(lambda (v) (member v triangles))
	      word-values)))
					
				       

(defun load-words-42 (filename)
  (with-open-file (in filename)
		  (string-split "," (read-line in))))

;;; Problem 43 - 0-to-9-pandigitals with funny property
(defun euler43 ()
  (loop for perm in (permutations '(0 1 2 3 4 5 6 7 8 9))
	when (has-euler43-property perm) sum (digits-to-num perm)))

(defun has-euler43-property (perm)
  (and (> (first perm) 0) 
       (divides? 2 (digits-to-num (subseq perm 1 4)))
       (divides? 3 (digits-to-num (subseq perm 2 5)))
       (divides? 5 (digits-to-num (subseq perm 3 6)))
       (divides? 7 (digits-to-num (subseq perm 4 7)))
       (divides? 11 (digits-to-num (subseq perm 5 8)))
       (divides? 13 (digits-to-num (subseq perm 6 9)))
       (divides? 17 (digits-to-num (subseq perm 7 10)))))
	
;;; Problem 44 - Sum and difference of pentagonal numbers is pentagonal
;;;              and the difference is minimized
(defun euler44 ()
  (do ((i 1 (1+ i))
       (match ()
	      (find-if #'pentagonal-match
		       (next-pentagonal-pair-list-first i))))
      ((not (null match)) match)))
      

(defun next-pentagonal-pair-list (last-n)
  (let ((n (pentagonal-number (1+ last-n)))
	(p-list ()))
    (dotimes (i last-n (nreverse p-list))
      (push (list (pentagonal-number (1+ i)) n) p-list))))

;;; Problem 45 - Number that is triangle, pentagonal, and hexgonal
;;;              Note: all hexagonal numbers are triangle numbers
(defun euler45 (&optional (start 40755))
  (let ((p-n (1+ (length (pentagonal-numbers start))))
	(h-n (1+ (length (hexagonal-numbers start)))))
    (do ((i (1+ start) (1+ i))) (())
      (if (and (= i (pentagonal-number p-n))
	       (= i (hexagonal-number h-n)))
	  (return-from euler45 i)
	(progn
	  (if (> i (pentagonal-number p-n)) (incf p-n))
	  (if (> i (hexagonal-number h-n)) (incf h-n)))))))
	
(defun euler45-fast (&optional (start 40755))
  (let ((h-n (1+ (length (hexagonal-numbers start)))))
    (do ((h (hexagonal-number h-n) (hexagonal-number h-n))) (())
      (if (my-integer-p (/ (1+ (sqrt (+ 1 (* 24L0 h)))) 6))
	  (return-from euler45-fast h)
	(incf h-n)))))
	

;;; Problem 46 - Smallest odd composite that cannot be written as the sum of a
;;;              prime and twice a square
(defun euler46 (&optional (start 9))
  (find-first-golbach-failure start))

(defun find-first-golbach-failure (g)
  (if (passes-golbach? g)
      (find-first-golbach-failure (next-odd-composite g))
      g))

(defun passes-golbach? (g)
  (dolist (p (generate-primes g) nil) 
    (dolist (s (double-squares-upto g))
      (if (= g (+ p s))
	  (return-from passes-golbach? t)))))
    
(defun next-odd-composite (c)
  (if (prime-p (+ 2 c))
      (next-odd-composite (+ 2 c))
      (+ 2 c)))

(defun double-squares-upto (n)
  (mapcar (lambda (x) (* 2 x)) (squares-upto n)))

(defun squares-upto (n &optional (m 1) (acc nil))
  (if (>= (square m) n) acc
      (squares-upto n (1+ m) (cons (square m) acc))))

  
;;; Problem 48 - Last 10 digits of 1^1 + 2^2 + ... + 1000 ^ 1000
(defun euler48 (&optional (n 1000) (last-digits 10))
  (mod (loop for i from 1 to n summing (expt i i)) (expt 10 last-digits)))

;;; Problem 50 - Prime that can be written as the sum of most consecutive prime
(defun euler50 (&optional (limit 1000000))
  (let ((primes-hash (generate-primes-hash limit))
	(primes-list (generate-primes limit)))
    (apply #'+ (first 
		(sort (mapcar #'(lambda (x) 
				  (max-prime-sequence (subseq primes-list x) 
						      primes-hash (1- limit))) 
			      (range :start 0 :end (length primes-list)))
		      #'(lambda (x y) (> (length x) (length y))))))))

(defun max-prime-sequence (primes-list primes-hash limit)
  (let ((used-primes nil)
	(sum-primes nil)
	(sum 0))
    (dolist (p primes-list sum-primes)
      (incf sum p)
      (if (> sum limit) (return-from max-prime-sequence sum-primes)
	(progn 
	  (push p used-primes)
	  (when (gethash sum primes-hash)
	    (setf sum-primes (append sum-primes used-primes))
	    (setf used-primes nil)))))))
      

;;; Problem 51 - Prime families by replacing digits with the same number
(defun replace-digit (n-arr replacement position)
  (setf (aref n-arr position) replacement)
  n-arr)

(defun replace-digits (n replacement positions)
  (reduce #'(lambda (x y) (replace-digit x replacement (1- y)))
	  positions
	  :initial-value (digits-array n)))

(defun valid-positions (n)
  (remove-if #'(lambda (x) (or (null x)
			       (= (length x) (num-digits n))))
	     (powerset (range :start 1 :end (1+ (num-digits n))))))

(defun family-primes (family positions)
  (mapcar #'(lambda (x) (digits-to-num (coerce (replace-digits family x positions)
							 'list)))
	  (range :start 0 :end 10)))

(defun count-family-primes (family positions)
  (count-if #'prime-p (remove-if #'(lambda (x) (< (num-digits x) (num-digits family)))
				 (family-primes family positions))))

(defun prime-family-p (size family)
  (if (find size
	    (mapcar #'(lambda (x) (count-family-primes family x))
		    (remove-if #'(lambda (x) (or (null x)
						 (= (length x) (num-digits family))))
			       (powerset (range :start 1 :end (1+ (num-digits family)))))))
      family
      nil))

(defun find-prime-family (size &optional family)
  (if (prime-family-p size family) family
      (find-prime-family size (next-prime family))))


;;; Problem 52 - Smallest number x such that 2x, 3x, 4x, 5x, 6x contain the
;;;              same digits
(defun euler52 ()
  (loop for i from 1 when (multiples-same-digits i) return i))

(defun multiples-same-digits (n)
  (and (same-digits (* 2 n) (* 3 n))
       (same-digits (* 2 n) (* 4 n))
       (same-digits (* 2 n) (* 5 n))
       (same-digits (* 2 n) (* 6 n))))

(defun same-digits (m n)
  (and (subsetp (digits m) (digits n))
       (subsetp (digits n) (digits m))))

;;; Problem 53 - nCr > 1000000
(defun euler53 (&optional (limit 1000000))
  (loop for n from 1 to 100 sum (length (more-than-combos n limit))))

(defun more-than-combos (n limit &optional (r 1) (acc nil))
  (if (= r n) acc
    (more-than-combos n limit (1+ r)
		      (if (> (ncr n r) limit) (append (list (ncr n r)) acc)
			acc))))

;;; Problem 54 - How many hands of poker did a player win?
(defun euler54 (&optional (poker-file "euler54.txt"))
  (count t (mapcar (lambda (x) (winner (first x) (second x)))
		   (hands-from-file poker-file))))

(defun parse-suit (suit-str)
  (cond ((string= (string-upcase suit-str) "C") 1)
	((string= (string-upcase suit-str) "D") 2)
	((string= (string-upcase suit-str) "S") 3)
	((string= (string-upcase suit-str) "H") 4)))

(defun card-value-map (card-value)
  (cond ((string= (string-upcase card-value) "A") "14")
	((string= (string-upcase card-value) "K") "13")
	((string= (string-upcase card-value) "Q") "12")
	((string= (string-upcase card-value) "J") "11")
	((string= (string-upcase card-value) "T") "10")
	(t card-value)))

(defun parse-card (card)
  (cons (parse-integer (card-value-map (subseq card 0 1)))
	(cons (parse-suit (subseq card 1 2))
	      ())))

(defun parse-one-hand (hand-string-list)
  (mapcar #'parse-card hand-string-list))

(defun parse-poker-hand (hand-line)
  (cons (parse-one-hand (subseq (string-split " " hand-line) 0 5))
	(cons (parse-one-hand (subseq (string-split " " hand-line) 5))
	())))

(defun hands-from-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
	 while line collect (parse-poker-hand line))))

(defun card-value (card)
  (first card))

(defun card-values (hand)
  (mapcar #'card-value hand))

(defun card-suit (card)
  (second card))

(defun card-suits (hand)
  (mapcar #'card-suit hand))

(defun winner (hand1 hand2)
  (winner-helper (hand-type hand1)
		 (hand-type hand2)))

  
(defun winner-helper (hand1 hand2)
  (if (= (car hand1) (car hand2))
      (winner-helper (cdr hand1) (cdr hand2))
      (> (car hand1) (car hand2))))

(defun hand-type (hand)
  (cond ((straight-flush? hand)
	 (append (list 9)
		 (ordered-hand hand (straight-flush? hand))))
	((four-of-a-kind? hand)
	 (append (list 8)
		 (ordered-hand hand (four-of-a-kind? hand))))
	((full-house? hand) (append (list 7)
				    (ordered-hand hand (full-house? hand))))
	((flush? hand) (append (list 6)
			       (ordered-hand hand (flush? hand))))
	((straight? hand) (append (list 5)
				  (ordered-hand hand (straight? hand))))
	((three-of-a-kind? hand)
	 (append (list 4) (ordered-hand hand (three-of-a-kind? hand))))
	((two-pair? hand) (append (list 3)
				  (ordered-hand hand (two-pair? hand))))
	((one-pair? hand) (append (list 2)
				  (ordered-hand hand (one-pair? hand))))
	(t (append (list 1)
		   (ordered-hand hand nil)))))

(defun ordered-hand (hand high-cards)
  (append
   high-cards
   (sort-card-values (remove-if (lambda (x) (member (car x) high-cards))
				hand))))
  
(defun sort-card-values (hand)
  (let ((sorted-hand (sort (card-values hand) #'>)))
    sorted-hand))
 
(defun one-pair? (hand)
  (cond ((= 1 (length hand)) nil)
	((= 2 (count (card-value (car hand))
		     (card-values hand)))
	 (remove-if (lambda (x) (not (= x (card-value (car hand)))))
		    (card-values hand)))
	(t (one-pair? (cdr hand)))))

(defun two-pair? (hand)
  (if (and (one-pair? hand)
	   (one-pair? 
	    (remove-if (lambda (x) (= (card-value x)
				      (car (one-pair? hand))))
		       hand)))
      (sort (remove-if (lambda (x) (< (count x (card-values hand))
				      2))
		       (card-values hand))
	    #'>)
      nil))
			  

(defun three-of-a-kind? (hand)
  (cond ((= 2 (length hand)) nil)
	((= 3 (count (card-value (car hand))
		    (card-values hand)))
	 (remove-if (lambda (x) (not (= x (card-value (car hand)))))
		    (card-values hand)))
	(t (three-of-a-kind? (cdr hand)))))

(defun straight? (hand)
  (if (and (= 4 (- (max-list (card-values hand))
		   (min-list (card-values hand))))
	   (subsetp (range :start (min-list (card-values hand))
			   :end (1+ (max-list (card-values hand))))
		    (card-values hand)))
      (sort (card-values hand) #'>)
      nil))

(defun flush? (hand)
  (if (every (lambda (x) (eq x (car (card-suits hand))))
	     (card-suits hand))
      (sort (card-values hand) #'>)
      nil))

(defun full-house? (hand)
  (if (and (three-of-a-kind? hand)
	   (one-pair? (remove-if (lambda (x) (= (card-value x)
						(car (three-of-a-kind? hand))))
				 hand)))
      (append (three-of-a-kind? hand)
	      (one-pair? (remove (car (three-of-a-kind? hand))
				 hand
				 :key #'car)))
      nil))

(defun four-of-a-kind? (hand)
  (if (or (= 4 (count (first (card-values hand))
			(card-values hand)))
	    (= 4 (count (second (card-values hand))
			(card-values hand))))
	 (remove-if (lambda (x) (not (= x (card-value (car hand)))))
		    (card-values hand))
	nil))

(defun straight-flush? (hand)
  (if (and (flush? hand) (straight? hand))
      (sort (card-values hand) #'>)
      nil))

;;; Problem 55 - Lychrel numbers
(defun euler55 (&optional (limit 10000))
  (count-if #'lychrel-p (range :end limit)))

(defun lychrel-p (n)
  (let ((cur n))
    (dotimes (i 50 t)
      (setf cur (+ cur (reverse-num cur)))
      (if (number-palindrome? cur) (return-from lychrel-p nil)))))

;;; Problem 56 - Maximum digital sum of a^b, a, b < 100
(defun euler56 (&optional (limit 100))
  (loop for a from 1 to limit maximize
		  (loop for b from 1 to limit
			maximize (apply #'+ (digits (expt a b))))))

;;; Problem 57 - Number of fractions in the expansions of sqrt(2) that have a
;;;              numerator with more digits than the denominator
(defun euler57 (&optional (limit 1000))
  (loop for i from 8 to limit
	for frac = (root2-expansion i) then (root2-expansion i)
	count (> (num-digits (numerator frac))
		 (num-digits (denominator frac)))))

(defun euler57-simple (&optional (limit 1000))
  (loop for i from 1 to limit
	for bot = 2 then (+ top bot)
	and top = 3 then (+ top (* 2 bot))
	count (> (num-digits top) (num-digits bot))))
				

;; Finds the nth expansion of sqrt(2)
(defun root2-expansion (n)
  (cond ((zerop n) 1)
	((= 1 n) (/ 3 2))
	(t (+ 1 (/ 1 (root2-expansion-helper n))))))

(defun root2-expansion-helper (n)
  (if (= 2 n) (+ 2 (/ 1 2))
    (+ 2 (/ 1 (root2-expansion-helper (1- n))))))
  

;;; Problem 58 - Frequency of primes along spiral diagonals
(defun euler58 ()
  (diag-prime-freq< .1))

(defun first-corner (side-len)
  (- (fourth-corner side-len)
     (* 3 (1- side-len))))

(defun second-corner (side-len)
  (- (fourth-corner side-len)
     (* 2 (1- side-len))))

(defun third-corner (side-len)
  (- (fourth-corner side-len) (1- side-len)))

(defun fourth-corner (side-len)
  (square side-len))

(defun next-side-len (cur-side-len)
  (+ 2 cur-side-len))

(defun corners (side-len)
  (list (first-corner side-len)
	(second-corner side-len)
	(third-corner side-len)
	(fourth-corner side-len)))

(defun diag-prime-freq (corners)
  (/ (count-if #'prime-p corners)
     (length corners)))

(defun update-diag-prime-freq (last-corner-len last-prime-count new-corners)
  (/ (+ last-prime-count
	(count-if #'prime-p new-corners))
     (+ last-corner-len (length new-corners))))

(defun diag-prime-freq< (ratio &optional (last-corner-len 1) (last-prime-count 0)
			 (side-len 3) (corners '(1)))
  (cond ((and (> (length corners) 1)
	      (< (update-diag-prime-freq last-corner-len
					 last-prime-count
					 (corners side-len))
		 ratio))
	 side-len)
	(t (diag-prime-freq< ratio
			     (+ last-corner-len (length (corners side-len)))
			     (+ last-prime-count (count-if #'prime-p (corners side-len)))
			     (next-side-len side-len)
			     (append (corners side-len)
				     corners)))))

  
;;; Problem 59 - XOR encryption breaking
;;; See: euler59.py
(defun euler59 ())

(defun try-keys (message decode-f &optional (key '(97 97 97)))
  (if (<= (first key) (char-code #\z))
      (progn
	(format t "~a"
		(print-decoded-message (decode-message key message decode-f)))
	(try-keys message decode-f (next-key key)))))
      

(defun next-key (key)
  (if (= (third key) (char-code #\z))
      (if (= (second key) (char-code #\z))
	  (list (1+ (first key)) (char-code #\a) (char-code #\a))
	  (list (first key) (1+ (second key)) (char-code #\a)))
      (list (first key) (second key) (1+ (third key)))))

(defun print-decoded-message (message)
  (mapcar #'code-char message))

(defun decode-message (key message decode-f)
  (cond ((null message) ())
	((< (length message) (length key))
	 (decode-subseq (subseq key 0 (length message))
			message decode-f))
	(t (append (decode-subseq key (subseq message 0 (length key)) decode-f)
		   (decode-message 
		    key (subseq message (1- (length key))) decode-f)))))

(defun decode-subseq (key cipher decode-f)
  (mapcar decode-f key cipher))


;;; Problem 60- Concatenating primes
(defun euler60 ()
  (check-primes 10000 3))

(defun check-primes (limit p)
  (cond ((> p limit) nil)
	((check-2-primes limit p (next-prime p)))
	(t (check-primes limit (next-prime p)))))

(defun check-2-primes (limit p p2)
  (cond ((> p2 limit) nil)
	((concat-prime-p p p2)
	 (let ((result (check-3-primes limit p p2 (next-prime p2))))
	   (if (null result) (check-2-primes limit p (next-prime p2))
	       result)))
	(t (check-2-primes limit p (next-prime p2)))))

(defun check-3-primes (limit p1 p2 p3)
  (cond ((> p3 limit) nil)
	((concat-3-prime-p p3 p1 p2)
	 (let ((result (check-4-primes limit  p1 p2 p3 (next-prime p3))))
	   (if (null result) (check-3-primes limit p1 p2 (next-prime p3))
	       result)))
	(t (check-3-primes limit p1 p2 (next-prime p3)))))

(defun check-4-primes (limit p1 p2 p3 p4)
  (cond ((> p4 limit) nil)
	((concat-4-prime-p p4 p1 p2 p3)
	 (let ((result (check-5-primes limit p1 p2 p3 p4 (next-prime p4))))
	   (if (null result) (check-4-primes limit p1 p2 p3 (next-prime p4))
	       result)))
	(t (check-4-primes limit p1 p2 p3 (next-prime p4)))))

(defun check-5-primes (limit p1 p2 p3 p4 p5)
  (cond ((> p5 limit) nil)
	((concat-5-prime-p p5 p1 p2 p3 p4) (+ p1 p2 p3 p4 p5))
	(t (check-5-primes limit p1 p2 p3 p4 (next-prime p5)))))
      

(defun concat-prime-p (p1 p2)
  (and (prime-p (concat-numbers p1 p2))
       (prime-p (concat-numbers p2 p1))))

(defun concat-3-prime-p (candidate p1 p2)
  (and (concat-prime-p p1 candidate)
       (concat-prime-p p2 candidate)))

(defun concat-4-prime-p (candidate p1 p2 p3)
  (and (concat-prime-p p1 candidate)
       (concat-prime-p p2 candidate)
       (concat-prime-p p3 candidate)))

(defun concat-5-prime-p (candidate p1 p2 p3 p4)
  (and (concat-prime-p p1 candidate)
       (concat-prime-p p2 candidate)
       (concat-prime-p p3 candidate)
       (concat-prime-p p4 candidate)))



;;; Problem 61 - Cyclic polygonal numbers
(defun euler61 ()
  (reduce #'+ (car (remove-if #'null
			      (mapcar #'(lambda (x)
					  (next-in-cycle x
							 (list (polygon-hash #'triangle-numbers 9999 1000)
							       (polygon-hash #'square-numbers 9999 1000)
							       (polygon-hash #'pentagonal-numbers 9999 1000)
							       (polygon-hash #'hexagonal-numbers 9999 1000)
							       (polygon-hash #'heptagonal-numbers 9999 1000))))
				      (remove-if #'(lambda (x) (< x 1000))
						 (octogonal-numbers 9999)))))))

(defun last-2-digits (n)
  (last-n-digits 2 n))

(defun first-2-digits (n)
  (first-n-digits 2 n))

(defun cyclic-p (x y)
  (= (last-n-digits 2 x)
     (first-n-digits 2 y)))

(defun cyclic-list-p (nums)
  (cond ((or (null nums) (< (length nums) 2)) nil)
	((= 2 (length nums)) (cyclic-p (first nums) (second nums)))
	(t (and (cyclic-p (first nums) (second nums))
		(cyclic-list-p (rest nums))))))

(defun polygon-hash (poly-func limit filter)
  (let ((h (make-hash-table)))
    (dolist (n (remove-if #'(lambda (x) (or (< (length (digits (last-n-digits 2 x))) 2)
					    (< x filter)))
			  (funcall poly-func limit))
	     h) 
      (if (gethash (first-n-digits 2 n) h)
	  (cons n (gethash (first-n-digits 2 n) h))
	  (setf (gethash (first-n-digits 2 n) h)
		(list n))))))

(defun valid-cycle (c)
  (and c (cyclic-list-p (append c (list (first c))))))

(defun next-in-cycle-in-hash (n h)
  (first (gethash (last-2-digits n) h)))

(defun next-in-cycle (n poly-hashes &optional (chain (list n)))
  (if (null poly-hashes) chain
      (dolist (h poly-hashes)
	(if (next-in-cycle-in-hash n h)
	    (let ((c (next-in-cycle (next-in-cycle-in-hash n h)
				    (remove h poly-hashes)
				    (append chain (list (next-in-cycle-in-hash n h))))))
	      (if (valid-cycle c) (return c))
	      nil)))))


;;; Problem 62 - Cube permutations of a cube
(defun euler62 (&optional (pow 3) (p 5) (start 1))
  (first-pow-with-p-permutations pow p start))

(defun first-pow-with-p-permutations (pow p &optional (n 1) (cur-ans 0)
				      (seen (make-hash-table :test #'equal)))
  (let ((val (first (gethash (sort (digits (expt n pow)) #'<) seen)))
	(ps (second (gethash (sort (digits (expt n pow)) #'<) seen))))
    (cond ((null ps)
	   (setf (gethash (sort (digits (expt n pow)) #'<) seen) (list (expt n pow) 1))
	   (first-pow-with-p-permutations pow p (1+ n) cur-ans seen))
	  ((and (= (1- p) ps)
		(or (zerop cur-ans) (< val cur-ans)))
	   (first-pow-with-p-permutations pow p (1+ n) val seen))
	  ((and (> cur-ans 0)
		(= (1- p) (second (gethash (sort (digits cur-ans) #'<) seen)))
		(> (length (digits val)) (length (digits cur-ans))))
	   (gethash (sort (digits (expt cur-ans pow)) #'<) seen)
	   cur-ans)
	  (t (setf (second (gethash (sort (digits (expt n pow)) #'<) seen)) (1+ ps))
	     (first-pow-with-p-permutations pow p (1+ n) cur-ans seen)))))
				  
;;; Problem 63 - n-digit positive integers which are also powers of n
(defun euler63 ()
  (loop for b from 1 to 9 sum
	(loop for e from 1 to 21 count (= e (length (digits (expt b e)))))))

;;; Problem 67 - Uses functions from Problem 18
(defparameter *euler67-triangle*
  '((59)
    (73 41)
    (52 40 09)
    (26 53 06 34)
    (10 51 87 86 81)
    (61 95 66 57 25 68)
    (90 81 80 38 92 67 73)
    (30 28 51 76 81 18 75 44)
    (84 14 95 87 62 81 17 78 58)
    (21 46 71 58 02 79 62 39 31 09)
    (56 34 35 53 78 31 81 18 90 93 15)
    (78 53 04 21 84 93 32 13 97 11 37 51)
    (45 03 81 79 05 18 78 86 13 30 63 99 95)
    (39 87 96 28 03 38 42 17 82 87 58 07 22 57)
    (06 17 51 17 07 93 09 07 75 97 95 78 87 08 53)
    (67 66 59 60 88 99 94 65 55 77 55 34 27 53 78 28)
    (76 40 41 04 87 16 09 42 75 69 23 97 30 60 10 79 87)
    (12 10 44 26 21 36 32 84 98 60 13 12 36 16 63 31 91 35)
    (70 39 06 05 55 27 38 48 28 22 34 35 62 62 15 14 94 89 86)
    (66 56 68 84 96 21 34 34 34 81 62 40 65 54 62 05 98 03 02 60)
    (38 89 46 37 99 54 34 53 36 14 70 26 02 90 45 13 31 61 83 73 47)
    (36 10 63 96 60 49 41 05 37 42 14 58 84 93 96 17 09 43 05 43 06 59)
    (66 57 87 57 61 28 37 51 84 73 79 15 39 95 88 87 43 39 11 86 77 74 18)
    (54 42 05 79 30 49 99 73 46 37 50 02 45 09 54 52 27 95 27 65 19 45 26 45)
    (71 39 17 78 76 29 52 90 18 99 78 19 35 62 71 19 23 65 93 85 49 33 75 09 02)
    (33 24 47 61 60 55 32 88 57 55 91 54 46 57 07 77 98 52 80 99 24 25 46 78 79 05)
    (92 09 13 55 10 67 26 78 76 82 63 49 51 31 24 68 05 57 07 54 69 21 67 43 17 63 12)
    (24 59 06 08 98 74 66 26 61 60 13 03 09 09 24 30 71 08 88 70 72 70 29 90 11 82 41 34)
    (66 82 67 04 36 60 92 77 91 85 62 49 59 61 30 90 29 94 26 41 89 04 53 22 83 41 09 74 90)
    (48 28 26 37 28 52 77 26 51 32 18 98 79 36 62 13 17 08 19 54 89 29 73 68 42 14 08 16 70 37)
    (37 60 69 70 72 71 09 59 13 60 38 13 57 36 09 30 43 89 30 39 15 02 44 73 05 73 26 63 56 86 12)
    (55 55 85 50 62 99 84 77 28 85 03 21 27 22 19 26 82 69 54 04 13 07 85 14 01 15 70 59 89 95 10 19)
    (04 09 31 92 91 38 92 86 98 75 21 05 64 42 62 84 36 20 73 42 21 23 22 51 51 79 25 45 85 53 03 43 22)
    (75 63 02 49 14 12 89 14 60 78 92 16 44 82 38 30 72 11 46 52 90 27 08 65 78 03 85 41 57 79 39 52 33 48)
    (78 27 56 56 39 13 19 43 86 72 58 95 39 07 04 34 21 98 39 15 39 84 89 69 84 46 37 57 59 35 59 50 26 15 93)
    (42 89 36 27 78 91 24 11 17 41 05 94 07 69 51 96 03 96 47 90 90 45 91 20 50 56 10 32 36 49 04 53 85 92 25 65)
    (52 09 61 30 61 97 66 21 96 92 98 90 06 34 96 60 32 69 68 33 75 84 18 31 71 50 84 63 03 03 19 11 28 42 75 45 45)
    (61 31 61 68 96 34 49 39 05 71 76 59 62 67 06 47 96 99 34 21 32 47 52 07 71 60 42 72 94 56 82 83 84 40 94 87 82 46)
    (01 20 60 14 17 38 26 78 66 81 45 95 18 51 98 81 48 16 53 88 37 52 69 95 72 93 22 34 98 20 54 27 73 61 56 63 60 34 63)
    (93 42 94 83 47 61 27 51 79 79 45 01 44 73 31 70 83 42 88 25 53 51 30 15 65 94 80 44 61 84 12 77 02 62 02 65 94 42 14 94)
    (32 73 09 67 68 29 74 98 10 19 85 48 38 31 85 67 53 93 93 77 47 67 39 72 94 53 18 43 77 40 78 32 29 59 24 06 02 83 50 60 66)
    (32 01 44 30 16 51 15 81 98 15 10 62 86 79 50 62 45 60 70 38 31 85 65 61 64 06 69 84 14 22 56 43 09 48 66 69 83 91 60 40 36 61)
    (92 48 22 99 15 95 64 43 01 16 94 02 99 19 17 69 11 58 97 56 89 31 77 45 67 96 12 73 08 20 36 47 81 44 50 64 68 85 40 81 85 52 09)
    (91 35 92 45 32 84 62 15 19 64 21 66 06 01 52 80 62 59 12 25 88 28 91 50 40 16 22 99 92 79 87 51 21 77 74 77 07 42 38 42 74 83 02 05)
    (46 19 77 66 24 18 05 32 02 84 31 99 92 58 96 72 91 36 62 99 55 29 53 42 12 37 26 58 89 50 66 19 82 75 12 48 24 87 91 85 02 07 03 76 86)
    (99 98 84 93 07 17 33 61 92 20 66 60 24 66 40 30 67 05 37 29 24 96 03 27 70 62 13 04 45 47 59 88 43 20 66 15 46 92 30 04 71 66 78 70 53 99)
    (67 60 38 06 88 04 17 72 10 99 71 07 42 25 54 05 26 64 91 50 45 71 06 30 67 48 69 82 08 56 80 67 18 46 66 63 01 20 08 80 47 07 91 16 03 79 87)
    (18 54 78 49 80 48 77 40 68 23 60 88 58 80 33 57 11 69 55 53 64 02 94 49 60 92 16 35 81 21 82 96 25 24 96 18 02 05 49 03 50 77 06 32 84 27 18 38)
    (68 01 50 04 03 21 42 94 53 24 89 05 92 26 52 36 68 11 85 01 04 42 02 45 15 06 50 04 53 73 25 74 81 88 98 21 67 84 79 97 99 20 95 04 40 46 02 58 87)
    (94 10 02 78 88 52 21 03 88 60 06 53 49 71 20 91 12 65 07 49 21 22 11 41 58 99 36 16 09 48 17 24 52 36 23 15 72 16 84 56 02 99 43 76 81 71 29 39 49 17)
    (64 39 59 84 86 16 17 66 03 09 43 06 64 18 63 29 68 06 23 07 87 14 26 35 17 12 98 41 53 64 78 18 98 27 28 84 80 67 75 62 10 11 76 90 54 10 05 54 41 39 66)
    (43 83 18 37 32 31 52 29 95 47 08 76 35 11 04 53 35 43 34 10 52 57 12 36 20 39 40 55 78 44 07 31 38 26 08 15 56 88 86 01 52 62 10 24 32 05 60 65 53 28 57 99)
    (03 50 03 52 07 73 49 92 66 80 01 46 08 67 25 36 73 93 07 42 25 53 13 96 76 83 87 90 54 89 78 22 78 91 73 51 69 09 79 94 83 53 09 40 69 62 10 79 49 47 03 81 30)
    (71 54 73 33 51 76 59 54 79 37 56 45 84 17 62 21 98 69 41 95 65 24 39 37 62 03 24 48 54 64 46 82 71 78 33 67 09 16 96 68 52 74 79 68 32 21 13 78 96 60 09 69 20 36)
    (73 26 21 44 46 38 17 83 65 98 07 23 52 46 61 97 33 13 60 31 70 15 36 77 31 58 56 93 75 68 21 36 69 53 90 75 25 82 39 50 65 94 29 30 11 33 11 13 96 02 56 47 07 49 02)
    (76 46 73 30 10 20 60 70 14 56 34 26 37 39 48 24 55 76 84 91 39 86 95 61 50 14 53 93 64 67 37 31 10 84 42 70 48 20 10 72 60 61 84 79 69 65 99 73 89 25 85 48 92 56 97 16)
    (03 14 80 27 22 30 44 27 67 75 79 32 51 54 81 29 65 14 19 04 13 82 04 91 43 40 12 52 29 99 07 76 60 25 01 07 61 71 37 92 40 47 99 66 57 01 43 44 22 40 53 53 09 69 26 81 07)
    (49 80 56 90 93 87 47 13 75 28 87 23 72 79 32 18 27 20 28 10 37 59 21 18 70 04 79 96 03 31 45 71 81 06 14 18 17 05 31 50 92 79 23 47 09 39 47 91 43 54 69 47 42 95 62 46 32 85)
    (37 18 62 85 87 28 64 05 77 51 47 26 30 65 05 70 65 75 59 80 42 52 25 20 44 10 92 17 71 95 52 14 77 13 24 55 11 65 26 91 01 30 63 15 49 48 41 17 67 47 03 68 20 90 98 32 04 40 68)
    (90 51 58 60 06 55 23 68 05 19 76 94 82 36 96 43 38 90 87 28 33 83 05 17 70 83 96 93 06 04 78 47 80 06 23 84 75 23 87 72 99 14 50 98 92 38 90 64 61 58 76 94 36 66 87 80 51 35 61 38)
    (57 95 64 06 53 36 82 51 40 33 47 14 07 98 78 65 39 58 53 06 50 53 04 69 40 68 36 69 75 78 75 60 03 32 39 24 74 47 26 90 13 40 44 71 90 76 51 24 36 50 25 45 70 80 61 80 61 43 90 64 11)
    (18 29 86 56 68 42 79 10 42 44 30 12 96 18 23 18 52 59 02 99 67 46 60 86 43 38 55 17 44 93 42 21 55 14 47 34 55 16 49 24 23 29 96 51 55 10 46 53 27 92 27 46 63 57 30 65 43 27 21 20 24 83)
    (81 72 93 19 69 52 48 01 13 83 92 69 20 48 69 59 20 62 05 42 28 89 90 99 32 72 84 17 08 87 36 03 60 31 36 36 81 26 97 36 48 54 56 56 27 16 91 08 23 11 87 99 33 47 02 14 44 73 70 99 43 35 33)
    (90 56 61 86 56 12 70 59 63 32 01 15 81 47 71 76 95 32 65 80 54 70 34 51 40 45 33 04 64 55 78 68 88 47 31 47 68 87 03 84 23 44 89 72 35 08 31 76 63 26 90 85 96 67 65 91 19 14 17 86 04 71 32 95)
    (37 13 04 22 64 37 37 28 56 62 86 33 07 37 10 44 52 82 52 06 19 52 57 75 90 26 91 24 06 21 14 67 76 30 46 14 35 89 89 41 03 64 56 97 87 63 22 34 03 79 17 45 11 53 25 56 96 61 23 18 63 31 37 37 47)
    (77 23 26 70 72 76 77 04 28 64 71 69 14 85 96 54 95 48 06 62 99 83 86 77 97 75 71 66 30 19 57 90 33 01 60 61 14 12 90 99 32 77 56 41 18 14 87 49 10 14 90 64 18 50 21 74 14 16 88 05 45 73 82 47 74 44)
    (22 97 41 13 34 31 54 61 56 94 03 24 59 27 98 77 04 09 37 40 12 26 87 09 71 70 07 18 64 57 80 21 12 71 83 94 60 39 73 79 73 19 97 32 64 29 41 07 48 84 85 67 12 74 95 20 24 52 41 67 56 61 29 93 35 72 69)
    (72 23 63 66 01 11 07 30 52 56 95 16 65 26 83 90 50 74 60 18 16 48 43 77 37 11 99 98 30 94 91 26 62 73 45 12 87 73 47 27 01 88 66 99 21 41 95 80 02 53 23 32 61 48 32 43 43 83 14 66 95 91 19 81 80 67 25 88)
    (08 62 32 18 92 14 83 71 37 96 11 83 39 99 05 16 23 27 10 67 02 25 44 11 55 31 46 64 41 56 44 74 26 81 51 31 45 85 87 09 81 95 22 28 76 69 46 48 64 87 67 76 27 89 31 11 74 16 62 03 60 94 42 47 09 34 94 93 72)
    (56 18 90 18 42 17 42 32 14 86 06 53 33 95 99 35 29 15 44 20 49 59 25 54 34 59 84 21 23 54 35 90 78 16 93 13 37 88 54 19 86 67 68 55 66 84 65 42 98 37 87 56 33 28 58 38 28 38 66 27 52 21 81 15 08 22 97 32 85 27)
    (91 53 40 28 13 34 91 25 01 63 50 37 22 49 71 58 32 28 30 18 68 94 23 83 63 62 94 76 80 41 90 22 82 52 29 12 18 56 10 08 35 14 37 57 23 65 67 40 72 39 93 39 70 89 40 34 07 46 94 22 20 05 53 64 56 30 05 56 61 88 27)
    (23 95 11 12 37 69 68 24 66 10 87 70 43 50 75 07 62 41 83 58 95 93 89 79 45 39 02 22 05 22 95 43 62 11 68 29 17 40 26 44 25 71 87 16 70 85 19 25 59 94 90 41 41 80 61 70 55 60 84 33 95 76 42 63 15 09 03 40 38 12 03 32)
    (09 84 56 80 61 55 85 97 16 94 82 94 98 57 84 30 84 48 93 90 71 05 95 90 73 17 30 98 40 64 65 89 07 79 09 19 56 36 42 30 23 69 73 72 07 05 27 61 24 31 43 48 71 84 21 28 26 65 65 59 65 74 77 20 10 81 61 84 95 08 52 23 70)
    (47 81 28 09 98 51 67 64 35 51 59 36 92 82 77 65 80 24 72 53 22 07 27 10 21 28 30 22 48 82 80 48 56 20 14 43 18 25 50 95 90 31 77 08 09 48 44 80 90 22 93 45 82 17 13 96 25 26 08 73 34 99 06 49 24 06 83 51 40 14 15 10 25 01)
    (54 25 10 81 30 64 24 74 75 80 36 75 82 60 22 69 72 91 45 67 03 62 79 54 89 74 44 83 64 96 66 73 44 30 74 50 37 05 09 97 70 01 60 46 37 91 39 75 75 18 58 52 72 78 51 81 86 52 08 97 01 46 43 66 98 62 81 18 70 93 73 08 32 46 34)
    (96 80 82 07 59 71 92 53 19 20 88 66 03 26 26 10 24 27 50 82 94 73 63 08 51 33 22 45 19 13 58 33 90 15 22 50 36 13 55 06 35 47 82 52 33 61 36 27 28 46 98 14 73 20 73 32 16 26 80 53 47 66 76 38 94 45 02 01 22 52 47 96 64 58 52 39)
    (88 46 23 39 74 63 81 64 20 90 33 33 76 55 58 26 10 46 42 26 74 74 12 83 32 43 09 02 73 55 86 54 85 34 28 23 29 79 91 62 47 41 82 87 99 22 48 90 20 05 96 75 95 04 43 28 81 39 81 01 28 42 78 25 39 77 90 57 58 98 17 36 73 22 63 74 51)
    (29 39 74 94 95 78 64 24 38 86 63 87 93 06 70 92 22 16 80 64 29 52 20 27 23 50 14 13 87 15 72 96 81 22 08 49 72 30 70 24 79 31 16 64 59 21 89 34 96 91 48 76 43 53 88 01 57 80 23 81 90 79 58 01 80 87 17 99 86 90 72 63 32 69 14 28 88 69)
    (37 17 71 95 56 93 71 35 43 45 04 98 92 94 84 96 11 30 31 27 31 60 92 03 48 05 98 91 86 94 35 90 90 08 48 19 33 28 68 37 59 26 65 96 50 68 22 07 09 49 34 31 77 49 43 06 75 17 81 87 61 79 52 26 27 72 29 50 07 98 86 01 17 10 46 64 24 18 56)
    (51 30 25 94 88 85 79 91 40 33 63 84 49 67 98 92 15 26 75 19 82 05 18 78 65 93 61 48 91 43 59 41 70 51 22 15 92 81 67 91 46 98 11 11 65 31 66 10 98 65 83 21 05 56 05 98 73 67 46 74 69 34 08 30 05 52 07 98 32 95 30 94 65 50 24 63 28 81 99 57)
    (19 23 61 36 09 89 71 98 65 17 30 29 89 26 79 74 94 11 44 48 97 54 81 55 39 66 69 45 28 47 13 86 15 76 74 70 84 32 36 33 79 20 78 14 41 47 89 28 81 05 99 66 81 86 38 26 06 25 13 60 54 55 23 53 27 05 89 25 23 11 13 54 59 54 56 34 16 24 53 44 06)
    (13 40 57 72 21 15 60 08 04 19 11 98 34 45 09 97 86 71 03 15 56 19 15 44 97 31 90 04 87 87 76 08 12 30 24 62 84 28 12 85 82 53 99 52 13 94 06 65 97 86 09 50 94 68 69 74 30 67 87 94 63 07 78 27 80 36 69 41 06 92 32 78 37 82 30 05 18 87 99 72 19 99)
    (44 20 55 77 69 91 27 31 28 81 80 27 02 07 97 23 95 98 12 25 75 29 47 71 07 47 78 39 41 59 27 76 13 15 66 61 68 35 69 86 16 53 67 63 99 85 41 56 08 28 33 40 94 76 90 85 31 70 24 65 84 65 99 82 19 25 54 37 21 46 33 02 52 99 51 33 26 04 87 02 08 18 96)
    (54 42 61 45 91 06 64 79 80 82 32 16 83 63 42 49 19 78 65 97 40 42 14 61 49 34 04 18 25 98 59 30 82 72 26 88 54 36 21 75 03 88 99 53 46 51 55 78 22 94 34 40 68 87 84 25 30 76 25 08 92 84 42 61 40 38 09 99 40 23 29 39 46 55 10 90 35 84 56 70 63 23 91 39)
    (52 92 03 71 89 07 09 37 68 66 58 20 44 92 51 56 13 71 79 99 26 37 02 06 16 67 36 52 58 16 79 73 56 60 59 27 44 77 94 82 20 50 98 33 09 87 94 37 40 83 64 83 58 85 17 76 53 02 83 52 22 27 39 20 48 92 45 21 09 42 24 23 12 37 52 28 50 78 79 20 86 62 73 20 59)
    (54 96 80 15 91 90 99 70 10 09 58 90 93 50 81 99 54 38 36 10 30 11 35 84 16 45 82 18 11 97 36 43 96 79 97 65 40 48 23 19 17 31 64 52 65 65 37 32 65 76 99 79 34 65 79 27 55 33 03 01 33 27 61 28 66 08 04 70 49 46 48 83 01 45 19 96 13 81 14 21 31 79 93 85 50 05)
    (92 92 48 84 59 98 31 53 23 27 15 22 79 95 24 76 05 79 16 93 97 89 38 89 42 83 02 88 94 95 82 21 01 97 48 39 31 78 09 65 50 56 97 61 01 07 65 27 21 23 14 15 80 97 44 78 49 35 33 45 81 74 34 05 31 57 09 38 94 07 69 54 69 32 65 68 46 68 78 90 24 28 49 51 45 86 35)
    (41 63 89 76 87 31 86 09 46 14 87 82 22 29 47 16 13 10 70 72 82 95 48 64 58 43 13 75 42 69 21 12 67 13 64 85 58 23 98 09 37 76 05 22 31 12 66 50 29 99 86 72 45 25 10 28 19 06 90 43 29 31 67 79 46 25 74 14 97 35 76 37 65 46 23 82 06 22 30 76 93 66 94 17 96 13 20 72)
    (63 40 78 08 52 09 90 41 70 28 36 14 46 44 85 96 24 52 58 15 87 37 05 98 99 39 13 61 76 38 44 99 83 74 90 22 53 80 56 98 30 51 63 39 44 30 91 91 04 22 27 73 17 35 53 18 35 45 54 56 27 78 48 13 69 36 44 38 71 25 30 56 15 22 73 43 32 69 59 25 93 83 45 11 34 94 44 39 92)
    (12 36 56 88 13 96 16 12 55 54 11 47 19 78 17 17 68 81 77 51 42 55 99 85 66 27 81 79 93 42 65 61 69 74 14 01 18 56 12 01 58 37 91 22 42 66 83 25 19 04 96 41 25 45 18 69 96 88 36 93 10 12 98 32 44 83 83 04 72 91 04 27 73 07 34 37 71 60 59 31 01 54 54 44 96 93 83 36 04 45)
    (30 18 22 20 42 96 65 79 17 41 55 69 94 81 29 80 91 31 85 25 47 26 43 49 02 99 34 67 99 76 16 14 15 93 08 32 99 44 61 77 67 50 43 55 87 55 53 72 17 46 62 25 50 99 73 05 93 48 17 31 70 80 59 09 44 59 45 13 74 66 58 94 87 73 16 14 85 38 74 99 64 23 79 28 71 42 20 37 82 31 23)
    (51 96 39 65 46 71 56 13 29 68 53 86 45 33 51 49 12 91 21 21 76 85 02 17 98 15 46 12 60 21 88 30 92 83 44 59 42 50 27 88 46 86 94 73 45 54 23 24 14 10 94 21 20 34 23 51 04 83 99 75 90 63 60 16 22 33 83 70 11 32 10 50 29 30 83 46 11 05 31 17 86 42 49 01 44 63 28 60 07 78 95 40)
    (44 61 89 59 04 49 51 27 69 71 46 76 44 04 09 34 56 39 15 06 94 91 75 90 65 27 56 23 74 06 23 33 36 69 14 39 05 34 35 57 33 22 76 46 56 10 61 65 98 09 16 69 04 62 65 18 99 76 49 18 72 66 73 83 82 40 76 31 89 91 27 88 17 35 41 35 32 51 32 67 52 68 74 85 80 57 07 11 62 66 47 22 67)
    (65 37 19 97 26 17 16 24 24 17 50 37 64 82 24 36 32 11 68 34 69 31 32 89 79 93 96 68 49 90 14 23 04 04 67 99 81 74 70 74 36 96 68 09 64 39 88 35 54 89 96 58 66 27 88 97 32 14 06 35 78 20 71 06 85 66 57 02 58 91 72 05 29 56 73 48 86 52 09 93 22 57 79 42 12 01 31 68 17 59 63 76 07 77)
    (73 81 14 13 17 20 11 09 01 83 08 85 91 70 84 63 62 77 37 07 47 01 59 95 39 69 39 21 99 09 87 02 97 16 92 36 74 71 90 66 33 73 73 75 52 91 11 12 26 53 05 26 26 48 61 50 90 65 01 87 42 47 74 35 22 73 24 26 56 70 52 05 48 41 31 18 83 27 21 39 80 85 26 08 44 02 71 07 63 22 05 52 19 08 20)
    (17 25 21 11 72 93 33 49 64 23 53 82 03 13 91 65 85 02 40 05 42 31 77 42 05 36 06 54 04 58 07 76 87 83 25 57 66 12 74 33 85 37 74 32 20 69 03 97 91 68 82 44 19 14 89 28 85 85 80 53 34 87 58 98 88 78 48 65 98 40 11 57 10 67 70 81 60 79 74 72 97 59 79 47 30 20 54 80 89 91 14 05 33 36 79 39)
    (60 85 59 39 60 07 57 76 77 92 06 35 15 72 23 41 45 52 95 18 64 79 86 53 56 31 69 11 91 31 84 50 44 82 22 81 41 40 30 42 30 91 48 94 74 76 64 58 74 25 96 57 14 19 03 99 28 83 15 75 99 01 89 85 79 50 03 95 32 67 44 08 07 41 62 64 29 20 14 76 26 55 48 71 69 66 19 72 44 25 14 01 48 74 12 98 07)
    (64 66 84 24 18 16 27 48 20 14 47 69 30 86 48 40 23 16 61 21 51 50 26 47 35 33 91 28 78 64 43 68 04 79 51 08 19 60 52 95 06 68 46 86 35 97 27 58 04 65 30 58 99 12 12 75 91 39 50 31 42 64 70 04 46 07 98 73 98 93 37 89 77 91 64 71 64 65 66 21 78 62 81 74 42 20 83 70 73 95 78 45 92 27 34 53 71 15)
    (30 11 85 31 34 71 13 48 05 14 44 03 19 67 23 73 19 57 06 90 94 72 57 69 81 62 59 68 88 57 55 69 49 13 07 87 97 80 89 05 71 05 05 26 38 40 16 62 45 99 18 38 98 24 21 26 62 74 69 04 85 57 77 35 58 67 91 79 79 57 86 28 66 34 72 51 76 78 36 95 63 90 08 78 47 63 45 31 22 70 52 48 79 94 15 77 61 67 68)
    (23 33 44 81 80 92 93 75 94 88 23 61 39 76 22 03 28 94 32 06 49 65 41 34 18 23 08 47 62 60 03 63 33 13 80 52 31 54 73 43 70 26 16 69 57 87 83 31 03 93 70 81 47 95 77 44 29 68 39 51 56 59 63 07 25 70 07 77 43 53 64 03 94 42 95 39 18 01 66 21 16 97 20 50 90 16 70 10 95 69 29 06 25 61 41 26 15 59 63 35)))

(defun euler67 ()
  (euler18 *euler67-triangle*))

;;; Problem 69 - Maxium n/totient(n)
(defun euler69 (&optional (limit 1000000))
  )

(defun find-max-n-over-totient (limit &optional (n 2) (max 2) (max-n 2))
  (cond ((> n limit) (list max-n max))
	((prime-p n) (find-max-n-over-totient limit (1+ n) max max-n))
	(t (let ((tot (totient n)))
	     (if (> (/ n tot) max)
		 (find-max-n-over-totient limit (1+ n) (/ n tot) n)
		 (find-max-n-over-totient limit (1+ n) max max-n))))))
      

;;; Problem 70 - Totient permutations
(defun permuted-totients (start end)
  (let ((result nil))
    (dolist (n (range :start start :end (1+ end)) result)
      (if (and (not (prime-p n))
	       (permutation-p n (totient n)))
	  (push n result)))))

;;; Problem 71 - Fraction just less than 3/7 with d <= 1,000,000
(defun frac-just-left-of (target limit &optional (d 8) (winner 2/7))
  (cond ((> d limit) winner)
	((> (closest-frac< target d)
	    winner)
	 (frac-just-left-of target limit (1+ d) (closest-frac< target d)))
	(t (frac-just-left-of target limit (1+ d) winner))))

(defun closest-frac< (target d)
  (let ((c (closest-frac<= target d)))
    (if (= c target) (- c (/ 1 d))
	c)))
      

(defun closest-frac<= (target d)
  (/ (floor (/ (* d (numerator target))
	       (denominator target)))
     d))

(defun next-denom-frac (cur-num d)
  (if (= (1+ cur-num)
	 (numerator (/ (1+ cur-num) d)))
      (/ (1+ cur-num) d)
      (next-denom-frac (1+ cur-num) d)))

;;; Problem 72 - Number of reduced proper fractions for d <= 1,000,000
(defun add-rel-prime-sets (limit rp-count-hash &optional (cur-set 2))
  (cond ((> cur-set limit) rp-count-hash)
	((null (gethash cur-set rp-count-hash))
	 (add-rel-prime-set cur-set limit rp-count-hash)
	 (add-rel-prime-sets limit rp-count-hash (1+ cur-set)))
	(t (add-rel-prime-sets limit rp-count-hash (1+ cur-set)))))

(defun add-rel-prime-sets (limit)
  (let ((rp-count-hash (make-hash-table)))
    (dolist (n (range :start 2 :end (1+ limit)))
      (if (and (/= 2 n) (prime-p n))
	  (add-prime-rel-prime-set n limit rp-count-hash)
	  (if (null (gethash n rp-count-hash))
	      (add-rel-prime-set n limit rp-count-hash))))
    rp-count-hash))
	  

(defun add-rel-prime-set (d max rp-count-hash)
  (mapcar (lambda (x) (setf (gethash (car x) rp-count-hash)
			    (cdr x)))
	  (rel-prime-set d max)))

(defun prime-rel-prime-set (p limit &optional (mult 2) (acc nil))
  (cond ((> (* p mult) limit) (cons (cons p (1- p)) acc))
	(t (prime-rel-prime-set p limit (1+ mult) (cons (cons (* p mult) (* (1- p) mult)) acc)))))

(defun rel-prime-set (d max &optional (mult 1) (acc nil) (num-rel-prime (num-rel-prime d)))
  (cond ((> (* mult d) max) acc)
	((evenp d)
	 (rel-prime-set d max (* 2 mult)
			(cons (cons (* mult d) (mult-rel-prime-even d num-rel-prime mult)) acc) num-rel-prime))
	(t (rel-prime-set d max (* 3 mult)
			  (cons (cons (* mult d) (mult-rel-prime-odd d num-rel-prime mult)) acc) num-rel-prime))))

(defun num-rel-prime (d &optional (limit d))
  (count-if (lambda (x) (= 1 (gcd x d))) (range :end limit)))

(defun mult-rel-prime-odd (d num-rel-prime mult)
  (cond ((= 1 mult) num-rel-prime)
	(t (mult-rel-prime-odd (* d 3) (* 3 num-rel-prime) (/ mult 3)))))

(defun mult-rel-prime-even (d num-rel-prime mult)
  (cond ((= 1 mult) num-rel-prime)
	(t (mult-rel-prime-even (* d 2) (* 2 num-rel-prime) (/ mult 2)))))

(defun mult-rel-prime (d num-rel-prime mult)
  (cond ((= 1 mult) num-rel-prime)
	((oddp d) (mult-rel-prime (* 2 d) num-rel-prime (/ mult 2)))
	(t (mult-rel-prime (* d 2) (* 2 num-rel-prime) (/ mult 2)))))

;;; TODO: Speed up
;;; Problem 73 - Number of reduced fractions between 1/3 and 1/2 with
;;;              denominator <= 10000
(defun euler73 (&optional (low 1/3) (high 1/2) (d 10000))
  (length (remove-duplicates 
	   (apply #'append 
		  (mapcar #'reduced-fractions-in-range
			  (range :start 5 :end (1+ d)))))))

;;; Returns a list of reduced fractions between low and high, exclusive
(defun reduced-fractions-in-range (n &optional (low 1/3) (high 1/2))
  (remove-if-not #'(lambda (x) (and (> x low) (< x high)))
		 (reduced-fractions n)))

;;; Generates a list of reduced fractions < 1 with the given denominator
(defun reduced-fractions (d)
  (loop for i from 1 below d collecting (/ i d)))


;;; Problem 74 - Factorial chains of length 60

(defparameter *fact-chain-table* (make-hash-table))

(defun num-chains-with-length (len limit &optional (n 1) (num 0))
  (cond ((> n limit) num)
	((= len (chain-len n))
	 (num-chains-with-length len limit (1+ n) (1+ num)))
	(t (num-chains-with-length len limit (1+ n) num))))
			       
(defun chain-len (n &optional (orig n) (seen '()) (len 0))
  (cond ((not (null (gethash n *fact-chain-table*)))
	 (setf (gethash orig *fact-chain-table*)
	       (+ len (gethash n *fact-chain-table*)))
	 ;(format t "~a => ~a~%" orig (gethash orig *fact-chain-table*))
	 (+ len (gethash n *fact-chain-table*)))
	((and (> len 0) (member n seen))
	 (setf (gethash orig *fact-chain-table*) len)
	 ;(format t "~a => ~a~%" orig (gethash orig *fact-chain-table*))
	 len)
	(t (chain-len (fact-digit-sum n) orig (cons n seen) (1+ len)))))

(defun fact-digit-sum (n)
  (reduce #'+ (mapcar (lambda (x) (factorial x))
		      (digits n))))

	  

;;; Problem 76 - Number of ways to write 100 as the sum of at least two numbers
(defun euler76 (&optional (n 100))
  (1- (partition-dyn n 1)))

;;; Calculates the number of partitions of n using addends >= k
;;  Source: http://en.wikipedia.org/wiki/Partition_function_(number_theory)#Intermediate_function
(defun intermediate-partition (k n)
  (cond ((> k n) 0)
	((= k n) 1)
	(t (+ (intermediate-partition k (- n k))
	      (intermediate-partition (1+ k) n)))))

;;; Dynamic programming version of calculating the partitions of k with
;;  addends >= k
(defun partition-dyn (n &optional (k 1))
  (cond ((> k n) 0)
	((= k n) 1)
	(t (let ((table (make-array (list (1+ n) (1+ n)) :initial-element 0)))
	     (dotimes (i (1+ n))
	       (setf (aref table i i) 1))
	     (loop for n from 1 to n do
		   (loop for k from (1- n) downto 1 do
			 (setf (aref table n k) (+ (aref table (- n k) k)
						   (aref table n (1+ k))))))
	     (aref table n k)))))

;;; Generating function version of calculating paritions of n
(defun partition-genfun (n memo)
  (cond ((< n 0) 0)
	((zerop n) 1)
	((> (svref memo (1- n)) 0) (svref memo (1- n)))
	(t 
	 (do ((k 1 (1+ k))
	      (p 0 
		 (+ p
		    (* (expt -1 (1+ k))
		       (+ (partition-genfun (- n (* 1/2 k (- (* 3 k) 1)))
					    memo)
			  (partition-genfun (- n (* 1/2 k (+ (* 3 k) 1)))
					    memo))))))
	     ((> k n) (setf (svref memo (1- n)) p) p)))))

;;; Problem 78 - Smallest number of coins that can be piled in a number
;;               of ways divisible by one million.
(defun euler78 (&optional (divisor 1000000))
  (let ((pt (make-array '(2 2) :initial-element 0)))
    (setf (aref pt 0 0) 1)
    (setf (aref pt 1 1) 1)
    (setf (aref pt 1 0) 2)
    (do ((n 3 (1+ n))
	 (p 2 (aref pt (1- (array-dimension pt 0)) 0)))
	((divides? divisor p) (list (1- n) p))
      (setf pt (next-partition-row pt n divisor)))))

(defun next-partition-row (table n divisor)
  (let ((new-table (copy-partition-table table n)))
    (dotimes (i (array-dimension new-table 0) new-table)
      (setf (aref new-table 
		  (1- (array-dimension new-table 0))
		  (- (array-dimension new-table 0) i 1))
	    (mod (fill-partition-elt new-table 
				     (- (array-dimension new-table 0) i 1))
		 divisor)))
    new-table))

;; p(n, k) = p(n, k+1) + p(n-k, k)
(defun fill-partition-elt (table k)
  (let ((p1 0) (p2 0) (edge (1- (array-dimension table 0))))
    (if (> (+ 2 k) edge) (setf p1 1)
      (setf p1 (aref table (1- (array-dimension table 0)) (+ 1 k))))
    (if (= edge k) (setf p2 0)
      (setf p2 (aref table (- (1- (array-dimension table 0)) (1+ k)) k)))
    (+ p1 p2)))
		     
(defun copy-partition-table (table n)
  (let ((new-table (make-array (list (1+ (floor (/ n 2))) 
				     (1+ (floor (/ n 2))))
			       :initial-element 1))
	(start (- (array-dimension table 0) 
		  (floor (/ n 2)))))
    (setf (aref new-table 
		(1- (array-dimension new-table 0))
		(1- (array-dimension new-table 0)))
	  1)
    (loop for i from start below (array-dimension table 0) do
	  (loop for j from 0 below (array-dimension table 0) do
		(setf (aref new-table (- i start) j) (aref table i j))))
    new-table))
	  
    
;;; Problem 79 - Guess password from attempts
(defun euler79 (&optional (file "euler79.txt"))
  (digits-to-num (reverse (top-sort (build-attempt-graph (attempts-from-file file))))))

(defun attempts-from-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
	 while line collect line)))

(defun top-sort (graph &optional (acc nil))
  (cond ((null (sources graph)) acc)
	(t (top-sort (top-sort-process-node graph
					    (car (sources graph)))
		     (cons (car (sources graph)) acc)))))
					    

(defun top-sort-process-node (graph node)
  (if (null (forward-edges graph node))
      (remove node graph :key #'car)
      (top-sort-process-node (top-sort-process-edge graph
						    (car (forward-edges graph
									node)))
			     node)))
      
(defun top-sort-process-edge (graph edge)
  (remove-reverse-edge graph (first edge) (second edge)))

(defun remove-reverse-edge (graph src dst)
  (if (member dst graph :key #'car)
      (if (member src (assoc dst graph))
	  (append (remove dst graph :key #'car)
		  (list (remove src (assoc dst graph)))))))

(defun forward-edges (graph node)
  (cond ((null graph) ())
	((and (not (= (car (car graph)) node))
	     (member node (car graph)))
	 (cons (list node (car (car graph)))
	       (forward-edges (cdr graph) node)))
	(t (forward-edges (cdr graph) node))))

(defun source-p (graph node)
  (= 1 (length (assoc node graph))))

(defun sources (graph)
  (mapcar #'car (remove-if (lambda (x) (not (source-p graph (car x))))
			   graph)))

(defun build-attempt-graph (attempts &optional (graph nil))
  (cond ((null attempts) graph)
	(t (build-attempt-graph (cdr attempts)
				(add-attempt graph
					     (split-attempt (car attempts)))))))

(defun split-attempt (attempt)
  (cond ((= 1 (length attempt)) ())
	(t (cons (list (parse-integer (subseq attempt 0 1))
		       (parse-integer (subseq attempt 1 2)))
		 (split-attempt (subseq attempt 1))))))
	 

(defun add-attempt (graph attempt)
  (cond ((null attempt) graph)
	(t (add-attempt (add-partial-attempt graph (car attempt))
			(cdr attempt)))))

(defun add-partial-attempt (graph attempt)
  (add-reverse-edge (add-node (add-node graph (car attempt))
			      (car (cdr attempt)))
		    (car attempt)
		    (car (cdr attempt))))
      

(defun add-node (graph node)
  (if (not (member node graph :key #'car))
      (cons (list node) graph)
      graph))
      

(defun add-reverse-edge (graph src dst)
  (if (member dst graph :key #'car)
      (if (not (member src (assoc dst graph)))
	  (append (remove dst graph :key #'car)
		  (list (append (assoc dst graph) (list src))))
	  graph)
      graph))


;;; Problems 81 - 83: Minimum sum path in matrix

;; Priority queue implementation for Dijkstra
(defun heap-left (i)
  (+ 1 (* 2 i)))

(defun heap-right (i)
  (+ 2 (* 2 i)))

(defun heap-parent (i)
  (floor (- i 1) 2))

(defun heap-value (heap i d)
  (dijkstra-distance d (first (aref heap i))))

(defun heap-size (heap)
  (array-total-size heap))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

(defun build-heap (A d &optional (i (1- (floor (array-total-size A) 2))))
  (cond ((< i 0) A)
	(t (heapify A i d)
	   (build-heap A d (1- i)))))

(defun heapify (heap i d &optional call)
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (- (length heap) 1))
	(smallest i))
    (setf smallest (if (and (<= l N)
			    (<= (heap-value heap l d)
				(heap-value heap i d)))
		       l i))
    (if (and (<= r N) (<= (heap-value heap r d)
			  (heap-value heap smallest d)))
	(setf smallest r))
    (cond ((/= smallest i)
	   (rotatef (aref heap i) (aref heap smallest))
	   ;(update-heap-index heap i)
	   ;(update-heap-index heap (heap-parent i))
	   (heapify heap smallest d call))
	  (t heap))))

(defun heap-extract-min (heap d)
  (heapify heap 0 d 1)
  (let ((min (first (aref heap 0))))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 d 2)
    min))

(defun heap-exchange-parent (heap i d)
  (when (and (> i 0) (> (heap-value heap (heap-parent i) d)
		      (heap-value heap i d)))
      (rotatef (aref heap i) (aref heap (heap-parent i)))
      ;(update-heap-index heap i)
      ;(update-heap-index heap (heap-parent i))
      (heap-exchange-parent heap (heap-parent i) d)))

(defun heap-decrease-key (heap i d new)
  (when (<= new (heap-value heap i d))
    ;(setf (aref heap i) new)
    (heap-exchange-parent heap i d)))

(defun heap-insert (heap item d)
  (vector-push-extend item heap)
  (heap-decrease-key heap
		     (1- (length heap))
		     d
		     (heap-value heap (1- (length heap)) d)))
	
(defun track-heap-indices (A &optional (i 0))
  (cond ((= i (length A)) A)
	(t (track-heap-index A i)
	   (track-heap-indices A (1+ i)))))

(defun track-heap-index (A i)
  (setf (aref A i) (cons (aref A i) i)))

(defun update-heap-index (A i)
  (setf (aref A i) (cons (car (aref A i)) i)))

;; Dijkstra's algorithm

(defun do-dijkstra (graph src d Q &optional (qsz (length Q)))
  (if (zerop (length Q))
      d
      (do-dijkstra graph src
		   (dijkstra-update-neighbor-distances
		    graph d (dijkstra-smallest-distance Q d) Q)
		   Q (1- qsz))))

(defun dijkstra-update-neighbor-distances (graph d u Q
					   &optional (neighbors (dijkstra-neighbors graph u)))
  (if (null neighbors) d
      (dijkstra-update-neighbor-distances
       graph
       (dijkstra-update-distance graph d u (first neighbors) Q)
       u Q (rest neighbors))))

(defun dijkstra-update-distance (graph d u v Q)
  (let ((alt (+ (dijkstra-node-value graph v) (dijkstra-distance d u))))
    (cond ((dijkstra-replace-distance? graph d alt v)
	   (dijkstra-add-Q graph Q v d)
	   ;(cons (list v alt)
	   ;(remove v d :key #'car :test #'equal)))
	   (setf (gethash v d) alt)
	   d)
	  (t d))))

(defun dijkstra-replace-distance? (graph d alt v)
  (or (= (dijkstra-max-distance graph)
	 (dijkstra-distance d v))
      (< alt (dijkstra-distance d v))))

(defun dijkstra-neighbors (graph u)
  (mapcar #'car (rest (assoc u graph :key #'car :test #'equal))))

(defun dijkstra-init-distances (graph src &optional (dist-hash (make-hash-table :test #'equal)))
  (let ((inf (dijkstra-max-distance graph)))
    ;(cons (list src (dijkstra-node-value graph src))
    ;(remove src
    ;(mapcar (lambda (x) (list (first x) inf))
    ;(dijkstra-all-verts graph))
    ;:key #'car :test #'equal))))

    (mapcar (lambda (x) (setf (gethash (first x) dist-hash) inf))
	    (dijkstra-all-verts graph))
    (setf (gethash src dist-hash) (dijkstra-node-value graph src))
    dist-hash))

(defun dijkstra-distance (d u)
  ;(second (assoc u d :test #'equal)))
  (gethash u d))

(defun dijkstra-max-distance (graph)
  (reduce #'+ (mapcar #'cadar graph)))

(defun dijkstra-smallest-distance (Q d)
  (heap-extract-min (build-heap Q d) d))

(defun dijkstra-all-verts (graph)
  (mapcar #'car graph))

(defun dijkstra-node-value (graph node)
  (second (assoc node (dijkstra-all-verts graph) :test #'equal)))

(defun dijkstra-init-Q (graph src)
  (make-array 1 :fill-pointer 1 :adjustable t
	      :initial-element (list src (dijkstra-node-value graph src))))

(defun dijkstra-add-Q (graph Q new d)
  (heap-insert Q (list new (dijkstra-node-value graph new)) d)
  Q)

(defun dijkstra-Q (graph src)
  (let ((V (dijkstra-all-verts graph)))
    (build-heap (make-array (length V) :fill-pointer (length V) 
			    :initial-contents V :adjustable t)
		(dijkstra-init-distances graph src))))

;; Matrix utilities
(defun read-matrix-from-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
	 while line collect (mapcar #'parse-integer
				    (string-split "," line)))))

(defun matrix-edge-weight (edge)
  (+ (first edge) (second edge)))

(defun matrix-shortest-path (graph src)
  (do-dijkstra graph src 
	       (dijkstra-init-distances graph src)
	       (dijkstra-init-Q graph src)))

(defun build-matrix-graph (node-adder matrix &optional (graph nil) (row 0))
  (cond ((>= row (length matrix)) graph)
	((matrix-last-row? matrix row)
	 (matrix-add-row-graph node-adder matrix graph row))
	(t (build-matrix-graph node-adder matrix
				 (matrix-add-row-graph node-adder matrix graph row)
				 (1+ row)))))

(defun matrix-add-row-graph (node-adder matrix graph row &optional (col 0))
  (cond ((>= col (length matrix)) graph)
	((matrix-last-col? matrix col)
	 (funcall node-adder matrix row col graph))
	(t (matrix-add-row-graph node-adder matrix
				 (funcall node-adder matrix row col graph)
				 row (1+ col)))))
  
(defun matrix-add-edges (graph src &rest nodes)
  (cons (append (list src) nodes) graph))
  
(defun matrix-elt-id (matrix row col)
  (list (concatenate 'string "r" (write-to-string row) "c" (write-to-string col))
	(nth col (nth row matrix))))
  
(defun matrix-last-row? (matrix row)
  (= (1+ row) (length matrix)))

(defun matrix-last-col? (matrix col)
  (= (1+ col) (length matrix)))

(defun matrix-path-source (matrix)
  (matrix-elt-id matrix 0 0))

(defun matrix-path-dest (matrix)
  (matrix-elt-id matrix (1- (length matrix)) (1- (length matrix))))


;; Problem 81 - Connected nodes are to the right and down in the matrix
(defparameter *euler81-test*
  '((131 673 234 103  18)
    (201  96 342 965 150)
    (630 803 746 422 111)
    (537 699 497 121 956)
    (805 732 524  37 331)))

(defun euler81 (&optional (file "euler81.txt"))
  (let ((matrix (read-matrix-from-file file)))
    ;(second (assoc (first (matrix-path-dest matrix))
    (gethash (first (matrix-path-dest matrix))
	     (matrix-shortest-path (build-matrix-graph #'matrix81-add-node matrix)
				   (first (matrix-path-source matrix))))))
    ;:test #'equal))))

(defun matrix81-add-node (matrix row col graph)
  (let ((item (matrix-elt-id matrix row col)))
    (cond ((and (matrix-last-row? matrix row) (matrix-last-col? matrix col))
	   (cons (list item) graph))
	  ((matrix-last-col? matrix col)
	   (matrix-add-edges graph item (matrix-elt-id matrix (1+ row) col)))
	  ((matrix-last-row? matrix row)
	   (matrix-add-edges graph item (matrix-elt-id matrix row (1+ col))))
	  (t (matrix-add-edges graph item
			       (matrix-elt-id matrix row (1+ col))
			       (matrix-elt-id matrix (1+ row) col))))))

;; Problem 82 - Neighbors are up, down, and right. Shortest path from any in
;;              first column to any in last column
(defparameter *euler82-test* *euler81-test*)

(defun euler82 (&optional (file "euler82.txt"))
  (let ((matrix (read-matrix-from-file file)))
    (second (assoc (first (matrix-path-dest matrix))
		   (matrix-shortest-path (build-matrix-graph #'matrix82-add-node matrix)
					 (first (matrix-path-source matrix)))
		   :test #'equal))))

(defun matrix82-paths (matrix)
  (min-list (mapcar (lambda (x)
		      (matrix82-min-path-from-source matrix (first x)))
		    (matrix82-sources matrix))))
  
(defun matrix82-min-path-from-source (matrix src)
  (min-list (matrix82-dest-lengths
	     matrix
	     (matrix-shortest-path
	      (build-matrix-graph #'matrix82-add-node matrix)
	      src))))

(defun matrix82-dest-lengths (matrix d)
  (mapcar (lambda (x) (dijkstra-distance d (first x)))
	  (matrix82-destinations matrix)))

(defun matrix82-sources (matrix)
  (mapcar (lambda (x) (matrix82-row-source matrix x))
	  (range :start 0 :end (length matrix))))

(defun matrix82-row-source (matrix row)
  (matrix-elt-id matrix row 0))

(defun matrix82-destinations (matrix)
  (mapcar (lambda (x) (matrix82-row-dest matrix x))
	  (range :start 0 :end (length matrix))))

(defun matrix82-row-dest (matrix row)
  (matrix-elt-id matrix row (1- (length matrix))))

(defun matrix82-add-node (matrix row col graph)
  (let ((item (matrix-elt-id matrix row col)))
    (cond ((and (zerop row) (zerop col))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1+ row) col)))
	  ((and (matrix-last-row? matrix row) (matrix-last-col? matrix col))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix (1- row) col)))
	  ((and (matrix-last-row? matrix row) (zerop col))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1- row) col)))
	  ((and (zerop row) (matrix-last-col? matrix col))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix (1+ row) col)))
	  ((zerop row)
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1+ row) col)))
	  ((matrix-last-row? matrix row)
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1- row) col)))
	  ((zerop col)
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1- row) col)
			     (matrix-elt-id matrix (1+ row) col)))
	  ((matrix-last-col? matrix col)
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix (1- row) col)
			     (matrix-elt-id matrix (1+ row) col)))
	  (t (matrix-add-edges graph item
			       (matrix-elt-id matrix row (1+ col))
			       (matrix-elt-id matrix (1- row) col)
			       (matrix-elt-id matrix (1+ row) col))))))
	   

;; Problem 83 - Neighbors are left, right, up, and down
(defparameter *euler83-test* *euler81-test*)

(defun euler83 (&optional (file "euler83.txt"))
  (let ((matrix (read-matrix-from-file file)))
    ;(second (assoc (first (matrix-path-dest matrix))
    (gethash (first (matrix-path-dest matrix))
	     (matrix-shortest-path (build-matrix-graph #'matrix83-add-node matrix)
				   (first (matrix-path-source matrix))))))
					;:test #'equal))))

(defun matrix83-add-node (matrix row col graph)
  (let ((item (matrix-elt-id matrix row col)))
    (cond ((and (matrix-last-row? matrix row) (matrix-last-col? matrix col))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1- col))
			     (matrix-elt-id matrix (1- row) col)))
	  ((and (zerop row) (zerop col))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1+ row) col)))
	  ((and (zerop row) (matrix-last-col? matrix col))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1- col))
			     (matrix-elt-id matrix (1+ row) col)))
	  ((and (zerop col) (matrix-last-row? matrix row))
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1- row) col)))
	  ((zerop row)
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1- col))
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1+ row) col)))
	  ((zerop col)
	   (matrix-add-edges graph item
			     (matrix-elt-id matrix row (1+ col))
			     (matrix-elt-id matrix (1- row) col)
			     (matrix-elt-id matrix (1+ row) col)))
	  ((matrix-last-col? matrix col)
	   (matrix-add-edges graph item
			       (matrix-elt-id matrix row (1- col))
			       (matrix-elt-id matrix (1+ row) col)
			       (matrix-elt-id matrix (1- row) col)))
	  ((matrix-last-row? matrix row)
	   (matrix-add-edges graph item
			      (matrix-elt-id matrix row (1- col))
			      (matrix-elt-id matrix row (1+ col))
			      (matrix-elt-id matrix (1- row) col)))
	  (t (matrix-add-edges graph item
				 (matrix-elt-id matrix row (1- col))
				 (matrix-elt-id matrix row (1+ col))
				 (matrix-elt-id matrix (1+ row) col)
				 (matrix-elt-id matrix (1- row) col))))))

;;; Problem 85 - Number of rectangles within a rectangle
(defun num-rectangles (m n)
  (* (ncr (1+ m) 2) (ncr (1+ n) 2)))

(defun euler85 (&optional (rects-wanted 2000000) (end-length 100))
  (let ((dim-list (closest-rectangle-dimensions rects-wanted end-length)))
    (* (first dim-list) (second dim-list))))

(defun closest-rectangle-dimensions (rects-wanted end-length)
  (min-list (loop for j from 1 to end-length collect
		 (append (list j)
			 (min-list (mapcar #'(lambda (x)
					       (list x (abs (- rects-wanted (num-rectangles j x)))))
					   (loop for i from 1 to j collecting i))
				   :key #'second)))
	    :key #'caddr))

;;; Problem 92 - TODO: make faster
(defun euler92 (&optional (limit 10000000))
  (let ((num-hash (make-hash-table))
	(num-matching 0))
    (setf (gethash 89 num-hash) 89)
    (setf (gethash 1 num-hash) 1)
       
    (dotimes (i limit num-matching)
      (let ((chain (list (1+ i))))
	(do ((n (next-square-chain-link (1+ i))
		(next-square-chain-link n)))
	    ((gethash n num-hash) (push n chain))
	  (push n chain))
	;(format t "~a -> ~a~%" n (gethash n num-hash)))
	;(format t "chain: ~{~a~^, ~}~%" chain)
	(dolist (c (rest chain)) (setf (gethash c num-hash) 
				       (gethash (first chain) num-hash)))
	(if (= (gethash (first chain) num-hash) 89) (incf num-matching))))))


(defun ends-chain? (n &optional (ends '(1 89)))
  (member n ends))
	  
;; Returns the end of the chain when it sees 1 or 89
(defun square-chain (n)
  (cond ((or (= n 1) (= n 89)) n)
	(t (square-chain (next-square-chain-link n)))))

;; Returns the chain when it sees 1 or 89
(defun square-chain-list (n &optional (ends 0))
  (cond ((zerop n) (list 0))
	((and (= 1 ends) (or (= n 1) (= n 89)) (list n)))
	((or (= n 1) (= n 89)) (append (list n) (square-chain-list (next-square-chain-link n) (1+ ends))))
	(t (append (list n) (square-chain-list (next-square-chain-link n) ends)))))

;; Calculates the next number in the chain
(defun next-square-chain-link (n)
  (let ((result 0))
    (dolist (d (digits n) result)
      (incf result (* d d)))))

;;; Problem 97 - Last 10 digits of huge prime
(defun euler97 ()
  (mod (1+ (* 28433 (expt 2 7830457))) (expt 10 10)))

;;; Problem 99 - Which base-exponent pair is the greatest!
(defun euler99 (&optional (file "euler99.txt"))
  (position (find-max-exp-pair (exp-pairs-from-file file)
		     (lambda (x) (* (second x) (log (first x)))))
	    (exp-pairs-from-file file)
	    :test #'equal))

(defun exp-pairs-from-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
	 while line collect (mapcar #'parse-integer
				    (string-split "," line)))))

(defun find-max-exp-pair (exp-pairs value-f &optional (max '(1 1)))
  (cond ((null exp-pairs) max)
	((> (exp-pair-value (car exp-pairs) value-f)
	    (funcall value-f max))
	 (find-max-exp-pair (cdr exp-pairs)
			    value-f
			    (car exp-pairs)))
	(t (find-max-exp-pair (cdr exp-pairs) value-f max))))

(defun exp-pair-value (exp-pair value-f)
  (funcall value-f exp-pair))

;;; Problem 102 - Origin in triangle
;; Algorithm adapted from: http://www.blackpawn.com/texts/pointinpoly/default.html
(defun euler102 (&optional (file "euler102.txt"))
  (count-if #'(lambda (x) (point-in-triangle '(0 0 0) (first x) (second x) (third x)))
	    (mapcar #'points-from-triangle-line (triangle-lines-from-file file))))

(defun points-from-triangle-file (file)
  (mapcar #'points-from-triangle-line (triangle-lines-from-file file)))

(defun points-from-triangle-line (line)
  (list (list (first line) (second line) 0)
	(list (third line) (fourth line) 0)
	(list (fifth line) (sixth line) 0)))

(defun triangle-lines-from-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil)
	 while line collect (mapcar #'parse-integer (string-split "," line)))))

(defun point-in-triangle (point a b c)
  (and (same-side point a b c)
       (same-side point b a c)
       (same-side point c a b)))

(defun same-side (p1 p2 a b)
  (>= (dot-product (cross-product (vector- b a) (vector- p1 a))
		   (cross-product (vector- b a) (vector- p2 a)))
      0))

;;; Problem 145 - Reversible numbers

(defun reverse-number (n)
  (digits-to-num (reverse (digits n))))

(defun reversible-number? (n)
  (and (not (divides? 10 n))
       (every #'oddp (digits (+ n (reverse-number n))))))
  

;;; Problem 206 - Number whose square has form 1_2_3_4_5_6_7_8_9_0
(defun euler206 ()
  (check-square-of 1000000000))

(defun matches-pattern? (n)
  (cond ((zerop n) t)
	((zerop (mod n 10))
	 (matches-pattern? (floor n 100)))
	(t nil)))

(defun square-diff (n)
  (- n 1020304050607080900))

(defun check-square-of (n)
  (cond ((> n 1020304050607080900) -1)
	((matches-pattern? (square-diff (square n)))
	 n)
	(t (check-square-of (+ 10 n)))))