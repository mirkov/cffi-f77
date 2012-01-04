;; Mirko Vukovic
;; Time-stamp: <2012-01-03 20:48:02 library-ops.lisp>
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

;;; Operations with the foreign library.  These operations are f77
;;; unrelated

(defvar *lib* nil "Current foreign library, nil if none is loaded")

(defun link-lib ()
  "Link to the foreign library"
  (setf *lib*
	(load-foreign-library
	 (asdf:system-relative-pathname "cffi+f77" "library.dll"))))

(defun unlink-lib ()
  "Unlink from the foreign library"
  (if *lib*
      (progn
	(close-foreign-library *lib*)
	(setf *lib* nil))
      (error "Not linked to foreign library")))

(defun relink-lib ()
  "Close and re-open link to library.  Used when the library has been
recreated"
  (when *lib* (unlink-lib))
  (link-lib))

(eval-when (:load-toplevel :execute)
  (relink-lib))