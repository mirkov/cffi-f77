;; Mirko Vukovic
;; Time-stamp: <2011-12-29 20:19:20 subroutine-argument-passing-tests.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:cffi+f77)

;;; This file performs tests of passing variables to f77 subroutines
;;; for all the f77 variable logical, integer, long, real, double,
;;; complex and character.
;;;
;;; For each variable type xyz, we define
;;; - lisp-fortran mapping "xyz_test_" - `xyz-test'
;;; - a run function `run-xyz-test' that exercises the mapping
;;; - a test `xyz'
;;;
;;; Most of the f77 subroutines simply copy the input to the output.
;;; The tests verify that the output matches the input.  A few of the
;;; subroutines perform minor operations: return the (not input) or
;;; complex conjugate of the input

(defcfun ("logical_test_" logical-test) :void
  (logical :pointer))

(defun run-logical-test (&optional (value t))
  ""
  (let ((logical value))
    (let ((logical* (foreign-alloc :boolean)))
      (setf (mem-ref logical* :boolean) logical)
      (logical-test logical*)
      (prog1 (mem-ref logical* :boolean)
	(foreign-free logical*)))))

(define-test logical
  (assert-true (run-logical-test nil))
  (assert-true (not (run-logical-test t))))


(defcfun ("integer_test_" integer-test) :void
  (input :pointer) (output :pointer))

(defun run-integer-test (input)
  ""
  (declare (integer input))
  (let ((input* (foreign-alloc :int))
	(output* (foreign-alloc :int)))
    (setf (mem-ref input* :int) input)
    (integer-test input* output*)
    (prog1 (mem-ref output* :int)
      (foreign-free input*)
      (foreign-free output*))))

(define-test integer
  (assert-number-equal 5 (run-integer-test 5))
  (assert-number-equal -10 (run-integer-test -10)))

(defcfun ("real_test_" real-test) :void
  (input :pointer) (output :pointer))

(defun run-real-test (input)
  ""
  (declare (float input))
  (let ((input* (foreign-alloc :float))
	(output* (foreign-alloc :float)))
    (setf (mem-ref input* :float) input)
    (real-test input* output*)
    (prog1 (mem-ref output* :float)
      (foreign-free input*)
      (foreign-free output*))))

(define-test real
  (assert-number-equal 5.0 (run-real-test 5.0))
  (assert-number-equal -10.0 (run-real-test -10.0)))

(defcfun ("double_test_" double-test) :void
  (input :pointer) (output :pointer))

(defun run-double-test (input)
  ""
  (declare (double-float input))
  (let ((input* (foreign-alloc :double))
	(output* (foreign-alloc :double)))
    (setf (mem-ref input* :double) input)
    (double-test input* output*)
    (prog1 (mem-ref output* :double)
      (foreign-free input*)
      (foreign-free output*))))

(define-test double
  (assert-number-equal 5.0d0 (run-double-test 5.0d0))
  (assert-number-equal -10.0d0 (run-double-test -10.0d0)))


;;; CFFI (and C) does not support the complex type.  Thus we need to
;;; perform the variable passinig manually.  CFFI is still not up to
;;; the C99 standard which handles complex numbers
(defcfun ("complex_test_" complex-test) :void
  (input :pointer) (output :pointer))

(defun run-complex-test (input)
  ""
  (declare (complex input))
  (let ((input* (foreign-alloc :float :count 2))
	(output* (foreign-alloc :float :count 2)))
    (setf (mem-aref input* :float 0) (realpart input)
	  (mem-aref input* :float 1) (imagpart input))
    (complex-test input* output*)
    (prog1
	(complex (mem-aref output* :float 0) (mem-aref output* :float 1))
      (foreign-free input*)
      (foreign-free output*))))

(define-test complex
  (assert-number-equal #C(1.0 -2.0) (run-complex-test (complex 1.0 2.0))))

(defcfun ("char_test_" char-test) :void
  "char_test is a subroutine of two arguments, the input and output string.

String passing to fortran as discussed at this site
http://astro.berkeley.edu/~wright/f2c.html (and others) points out
that fortran subroutines with string arguments also require additional
parameters of type long.  Each additional parameter specifies the
length of its corresponding string parameter.


We pass both string arguments as pointers.  We also pass the string
lengths as long intgers.  Thus even though the subroutine has only two
arguments, we call it with four."
  (input :pointer) (output :pointer) (input-length :long) (output-length :long))

(defun run-char-test (input)
  "Test of passing and retreiving strings to f77

We allocate space of appropriate length, and then transfer string
characters one at a time.  The transfer is done using the unsigned
character type.
"
  (declare (string input))
  (assert (= 65 (char-code #\A)) ()
	  "The character encoding does not seem to follow ascii")
  (let* ((length (length input))
	 (input* (foreign-alloc :char :count length))
	 (output* (foreign-alloc :char :count length)))
    (dotimes (i length)
      (setf (mem-ref input* :uchar i) (char-code (elt input i))))
    (char-test input* output* length length)
    (prog1
	(let ((output (make-array 3 :element-type 'character)))
	  (dotimes (i length)
	    (setf (elt output i)
		  (code-char (mem-aref output* :uchar i))))
	  (coerce output 'string))
      (foreign-free input*)
      (foreign-free output*))))

(define-test char
  (assert-equal "foo" (run-char-test "foo")))

(defcfun ("real_arr_test_" real-arr-test) :void
  (input :pointer) (output :pointer) (n :pointer))

(defun run-real-arr-test (input)
  (let* ((n (length input))
	 (n* (foreign-alloc :int))
	 (input* (foreign-alloc :float :count n))
	 (output* (foreign-alloc :float :count n)))
    (dotimes (i n)
      (setf (mem-aref input* :float i) (elt input i)))
    (setf (mem-ref n* :int) n)
    (real-arr-test input* output* n*)
    (prog1 (let ((output (make-array n :element-type 'float)))
	     (dotimes (i n)
	       (setf (elt output i) (mem-aref output* :float i)))
	     output)
      (foreign-free input*)
      (foreign-free output*)
      (foreign-free n*))))

(define-test real-arr
  (assert-numerical-equal #(2.0 4.0 6.0) (run-real-arr-test #(1.0 2.0 3.0))))

(defcfun ("complex_arr_test_" complex-arr-test) :void
  (input :pointer) (output :pointer) (n :pointer))

(defun run-complex-arr-test (input)
  (let* ((n (length input))
	 (n* (foreign-alloc :int))
	 (input* (foreign-alloc :float :count (* 2 n)))
	 (output* (foreign-alloc :float :count (* 2 n))))
    (dotimes (i n)
      (setf (mem-aref input* :float (* 2 i)) (realpart (elt input i))
	    (mem-aref input* :float (+ 1 (* 2 i))) (imagpart (elt input i))))
    (setf (mem-ref n* :int) n)
    (complex-arr-test input* output* n*)
    (prog1 (let ((output (make-array n :element-type 'complex)))
	     (dotimes (i n)
	       (setf (elt output i)
		     (complex (mem-aref output* :float (* 2 i))
			      (mem-aref output* :float (+ 1 (* 2 i))))))
	     output)
      (foreign-free input*)
      (foreign-free output*)
      (foreign-free n*))))

(define-test complex-arr
  (assert-numerical-equal
   #(#C(2.0 -1.0)
     #C(4.0 -1.0)
     #C(6.0 -1.0))
   (run-complex-arr-test #(#C(1.0 1.0)
			   #C(2.0 1.0)
			   #C(3.0 1.0)))))

