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
  (anyins-record-position 3 42 "file.el")
  (anyins-record-position 2 32 "file.el")
  (anyins-record-position 1 22 "file.el")
  (anyins-record-position 1 45 "file.el")
  (anyins-record-position 2 26 "file.el")
  (anyins-record-position 2 67 "feature.el")
  (anyins-record-position 1 1 "feature.el")
  (anyins-record-position 1 1 "feature.el")
  ;; we get an association table with unique positions ordered
  (should (equal (gethash "file.el" anyins-buffers-positions) '((1 45)(2 26)(3 42))))
  (should (equal (gethash "feature.el" anyins-buffers-positions) '((1 1)(2 67))))
  )
