;; Mirko Vukovic
;; Time-stamp: <2011-12-29 20:54:16 simple-example.lisp>
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

;; simple examples

(defcfun ("simple_example_" simple-example) :void
  "Link to subroutine simple_example in simple_example.f77

simple_example(.f77) places the sum of the first two arguments into
the third argument.

It also creates a two-line file simple_example.echo with the first two
arguments in the first line and the third argument (result) in the
second line

In the library, the subroutine is referenced as _simple_example_
(see the output of `>nm library').  We do not need to specify the
leading underscore.  This `mangling' is the gfortran convention.
Other compilers might use different mangling
"
;; Since f77 passes agruments by reference, we pass pointers, and not
;; values themselves
  (xi :pointer) (yi :pointer) (zo :pointer))

(defun run-simple_example ()
  "Example of a call to simple_example which calls the f77 subroutine
  simple_example

See documentation of defcfun simple_example for further details"
  (let ((x 1d0)
	(y 2d0))
    (let (;;Allocate memory
	  (x* (foreign-alloc :double))
	  (y* (foreign-alloc :double))
	  (z* (foreign-alloc :double)))
      ;; copy values from x&y into allocated memory
      (setf (mem-ref x* :double) x)
      (setf (mem-ref y* :double) y)
      ;; call f77
      (simple-example x* y* z*)
      (prog1
	  ;; retreive from the allocated memory
	  (mem-ref z* :double)
	;; de-allocate memory
	(foreign-free x*)
	(foreign-free y*)
	(foreign-free z*)))))


