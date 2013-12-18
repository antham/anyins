;;; anyins.el --- Test for anyins

;; Copyright (C) 2013 Anthony HAMON

;; Author: Anthony HAMON
;; URL: http://github.com/antham/anyins

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'ert)
(require 'anyins)

(ert-deftest anyins-record-position ()
  (setq anyins-buffers-positions '())
  ;; if record already exists return nil, t otherwise
  (should (equal (anyins-record-position '(3 42)) t))
  (should (equal (anyins-record-position '(2 32)) t))
  (should (equal (anyins-record-position '(1 22)) t))
  (should (equal (anyins-record-position '(1 45)) t))
  (should (equal (anyins-record-position '(2 32)) nil))
  (should (equal (anyins-record-position '(2 26)) t))
  (should (equal (anyins-record-position '(3 42)) nil))
  (should (equal (anyins-record-position '(1 45)) nil))
  ;; we get an association table with unique positions keeping first entered
  (should (equal anyins-buffers-positions '((3 42)(2 32)(1 22)(1 45)(2 26))))
  )

(ert-deftest anyins-remove-positions ()
  (setq anyins-buffers-positions '((3 3)(4 4)(5 5)))
  (anyins-remove-positions)
  ;; delete an entry
  (should (equal-including-properties anyins-buffers-positions '()))
  ;; try to delete entry already deleted
  (anyins-remove-positions)
  (should (equal-including-properties anyins-buffers-positions '()))
  )

(ert-deftest anyins-prepare-content-to-insert ()
  ;; text is splitted at new line
  (should (equal-including-properties (anyins-prepare-content-to-insert "hello world\nhello world\nhello world") '("hello world" "hello world" "hello world")))
  (should (equal-including-properties (anyins-prepare-content-to-insert "hello world") '("hello world")))
  (should (equal-including-properties (anyins-prepare-content-to-insert nil) nil))
  )
