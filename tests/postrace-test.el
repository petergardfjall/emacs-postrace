;;; postrace-test.el -- tests for postrace.el -*- lexical-binding: t -*-
;;
;; Copyright © 2021 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Package-Requires: ((cl-lib "0.5"))
;;
;;
;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file is part of postrace.el
;;
;;; Code:

(require 'cl-lib)
(require 'postrace)

(defmacro postrace-fails (body-form)
  "Evaluate the BODY-FORM expression and return t on error, nil otherwise."
  `(condition-case nil
      (progn
	,body-form
	nil)
     (error t))
  )


(defun postrace-stack-empty-test ()
  "Verify behavior of an empty stack."
  (message "postrace-stack-empty-test ...")

  (let ((empty (postrace-stack--new)))
    (cl-assert (= 0 (postrace-stack--len empty))
	       "unexpected size of empty stack")
    (cl-assert (= -1 (postrace-stack--last-index empty))
	       "unexpected last index of empty stack")
    (cl-assert (not (postrace-fails (postrace-stack--len empty)))
	       "should be possible to get length of empty stack")
    (cl-assert (postrace-fails (postrace-stack--get empty 0))
	       "should not be possible to get element 0 from empty stack")
    (cl-assert (postrace-fails (postrace-stack--pop empty))
	       "should not be possible to pop empty stack")
    (cl-assert (postrace-fails (postrace-stack--remove-at empty 0))
	       "should not be possible to remove from empty stack"))
  )



(defun postrace-stack-push-test ()
  "Verify push behavior."
  (message "postrace-stack-push-test ...")

  (setq st (postrace-stack--new))
  (cl-assert (= 0 (postrace-stack--len st))
	     "unexpected size of empty stack")
  (cl-assert (= -1 (postrace-stack--last-index st))
	     "unexpected last index of empty stack")
  (cl-assert (equal (postrace-stack-elements st) '())
	     "unexpected elements in stack")

  (postrace-stack--push st 0)
  (cl-assert (= 1 (postrace-stack--len st))
	     "unexpected stack size")
  (cl-assert (= 0 (postrace-stack--last-index st))
	     "unexpected last index")
  (cl-assert (equal (postrace-stack-elements st) '(0))
	     "unexpected elements in stack")

  (postrace-stack--push st 1)
  (cl-assert (= 2 (postrace-stack--len st))
	     "unexpected stack size")
  (cl-assert (= 1 (postrace-stack--last-index st))
	     "unexpected last index")
  (cl-assert (equal (postrace-stack-elements st) '(0 1))
	     "unexpected elements in stack")

  (postrace-stack--push st 2)
  (cl-assert (= 3 (postrace-stack--len st))
	     "unexpected stack size")
  (cl-assert (= 2 (postrace-stack--last-index st))
	     "unexpected last index")
  (cl-assert (equal (postrace-stack-elements st) '(0 1 2))
	     "unexpected elements in stack")
  )


(defun postrace-stack-insert-test ()
  "Verify insert behavior."
  (message "postrace-stack-insert-test ...")

  (setq st (postrace-stack--new))
  (cl-assert (= 0 (postrace-stack--len st))
	     "unexpected size of empty stack")
  (cl-assert (equal (postrace-stack-elements st) '())
	     "unexpected elements in stack")


  (cl-assert (postrace-fails (postrace-stack--insert-at st 1 10))
	     "should not be possible to insert at index 1 on empty stack")
  (cl-assert (postrace-fails (postrace-stack--insert-at st -1 10))
	     "should not be possible to insert at index -1 on stack")

  (postrace-stack--insert-at st 0 10)
  (cl-assert (= 1 (postrace-stack--len st))
	     "unexpected stack size")
  (cl-assert (equal (postrace-stack-elements st) '(10))
	     "unexpected elements in stack")

  ;; insert last
  (postrace-stack--insert-at st 1 20)
  (cl-assert (= 2 (postrace-stack--len st))
	     "unexpected stack size")
  (cl-assert (equal (postrace-stack-elements st) '(10 20))
	     "unexpected elements in stack")

  ;; insert first
  (postrace-stack--insert-at st 0 5)
  (cl-assert (= 3 (postrace-stack--len st))
	     "unexpected stack size")
  (cl-assert (equal (postrace-stack-elements st) '(5 10 20))
	     "unexpected elements in stack")

  ;; insert middle
  (postrace-stack--insert-at st 2 15)
  (cl-assert (= 4 (postrace-stack--len st))
	     "unexpected stack size")
  (cl-assert (equal (postrace-stack-elements st) '(5 10 15 20))
	     "unexpected elements in stack")

  )


(defun postrace-stack-get-test ()
  "Verify get behavior."
  (message "postrace-stack-get-test ...")

  (setq stack (postrace-stack--new))
  (postrace-stack--push stack 0)
  (postrace-stack--push stack 1)
  (postrace-stack--push stack 2)

  (cl-assert (= 0 (postrace-stack--get stack 0))
	     "expected element 0 to be 0")
  (cl-assert (= 1 (postrace-stack--get stack 1))
	     "expected element 1 to be 1")
  (cl-assert (= 2 (postrace-stack--get stack 2))
	     "expected element 2 to be 2")


  (cl-assert (postrace-fails (postrace-stack--get stack -1))
	     "should not be possible to get element with index -1")
  (cl-assert (postrace-fails (postrace-stack--get stack 3))
	     "should not be possible to get element with index 3")
  )


(defun postrace-stack-remove-test ()
  "Verify remove behavior."
  (message "postrace-stack-remove-test ...")

  (setq stack (postrace-stack--new))
  (postrace-stack--insert-at stack 0 0)
  (postrace-stack--insert-at stack 1 1)
  (postrace-stack--insert-at stack 2 2)
  (postrace-stack--insert-at stack 3 3)

  (cl-assert (equal (postrace-stack-elements stack) '(0 1 2 3)))

  (cl-assert (postrace-fails (postrace-stack--remove-at stack -1))
	     "should not be possible to remove index -1")
  (cl-assert (postrace-fails (postrace-stack--remove-at stack 4))
	     "should not be possible to remove index beyond last element")

  ;; remove from front
  (cl-assert (= 0 (postrace-stack--remove-at stack 0))
	     "failed to remove from front")
  (cl-assert (equal (postrace-stack-elements stack) '(1 2 3))
	     "unexpected elements")

  ;; remove from middle
  (cl-assert (= 2 (postrace-stack--remove-at stack 1))
	     "failed to remove from middle")
  (cl-assert (equal (postrace-stack-elements stack) '(1 3))
	     "unexpected elements")

  ;; remove from back
  (cl-assert (= 3 (postrace-stack--remove-at stack 1))
	     "failed to remove from back")
  (cl-assert (equal (postrace-stack-elements stack) '(1))
	     "unexpected elements")

  ;; remove from back
  (cl-assert (= 1 (postrace-stack--remove-at stack 0))
	     "failed to remove last element")
  (cl-assert (equal (postrace-stack-elements stack) '())
	     "unexpected elements")
  )

(defun postrace-stack-pop-test ()
  "Verify pop behavior."
  (message "postrace-stack-pop-test ...")

  (setq stack (postrace-stack--new))
  (postrace-stack--insert-at stack 0 0)
  (postrace-stack--insert-at stack 1 1)
  (postrace-stack--insert-at stack 2 2)
  (postrace-stack--insert-at stack 3 3)

  (cl-assert (equal (postrace-stack-elements stack) '(0 1 2 3)))


  (cl-assert (= 3 (postrace-stack--pop stack))
	     "failed to pop")
  (cl-assert (equal (postrace-stack-elements stack) '(0 1 2))
	     "unexpected elements")

  (cl-assert (= 2 (postrace-stack--pop stack))
	     "failed to pop")
  (cl-assert (equal (postrace-stack-elements stack) '(0 1))
	     "unexpected elements")

  (cl-assert (= 1 (postrace-stack--pop stack))
	     "failed to pop")
  (cl-assert (equal (postrace-stack-elements stack) '(0))
	     "unexpected elements")

  (cl-assert (= 0 (postrace-stack--pop stack))
	     "failed to pop")
  (cl-assert (equal (postrace-stack-elements stack) '())
	     "unexpected elements")

  ;; cannot pop empty stack
  (cl-assert (postrace-fails (postrace-stack--pop stack))
	     "must not be able to pop empty stack")
  )

(defun postrace-stack-swap-test ()
  "Verify pop behavior."
  (message "postrace-stack-swap-test ...")

  (setq stack (postrace-stack--new))

  ;; cannot swap elements in empty stack
  (cl-assert (postrace-fails (postrace-stack--swap stack 0 0))
	     "must not be able to swap in empty stack")

  (postrace-stack--insert-at stack 0 0)
  (postrace-stack--insert-at stack 1 1)
  (postrace-stack--insert-at stack 2 2)
  (postrace-stack--insert-at stack 3 3)
  (cl-assert (equal (postrace-stack-elements stack) '(0 1 2 3)))

  (cl-assert (postrace-fails (postrace-stack--swap stack -1 0))
	     "must not be able to swap with index1 out-of-bounds")
  (cl-assert (postrace-fails (postrace-stack--swap stack 1 4))
	     "must not be able to swap with index2 out-of-bounds")


  (postrace-stack--swap stack 0 1)
  (cl-assert (equal (postrace-stack-elements stack) '(1 0 2 3)))
  ;; order of indices does not matter 0,1 vs 1,0 is equivalent
  (postrace-stack--swap stack 1 0)
  (cl-assert (equal (postrace-stack-elements stack) '(0 1 2 3)))

  (postrace-stack--swap stack 0 3)
  (cl-assert (equal (postrace-stack-elements stack) '(3 1 2 0)))
  (postrace-stack--swap stack 1 2)
  (cl-assert (equal (postrace-stack-elements stack) '(3 2 1 0)))

  (postrace-stack--swap stack 0 2)
  (cl-assert (equal (postrace-stack-elements stack) '(1 2 3 0)))
  (postrace-stack--swap stack 3 0)
  (cl-assert (equal (postrace-stack-elements stack) '(0 2 3 1)))
  (postrace-stack--swap stack 1 3)
  (cl-assert (equal (postrace-stack-elements stack) '(0 1 3 2)))
  (postrace-stack--swap stack 3 2)
  (cl-assert (equal (postrace-stack-elements stack) '(0 1 2 3)))

  )


(defun postrace-test-suite ()
  "Run the entire suite of test functions."
  (postrace-stack-empty-test)
  (postrace-stack-push-test)
  (postrace-stack-insert-test)
  (postrace-stack-get-test)
  (postrace-stack-remove-test)
  (postrace-stack-pop-test)
  (postrace-stack-swap-test)
  (postrace-test-end))

(defun postrace-test-end ()
  "Any code to run when a test function ends."
  (print "all tests completed successfully." #'external-debugging-output)
  (kill-emacs))


(provide 'postrace-test)
;;; postrace-test.el ends here
