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


;;; Commentary:
;;

(require 'ert)
(require 'anyins)

;;; Code:

(ert-deftest anyins-record-position () "Record position."
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
             (should (equal anyins-buffers-positions '((3 42)
                                                       (2 32)
                                                       (1 22)
                                                       (1 45)
                                                       (2 26)))))

(ert-deftest anyins-remove-positions () "Remove all positions."
             (setq anyins-buffers-positions '((3 3)
                                              (4 4)
                                              (5 5)))
             (anyins-remove-positions)
             ;; delete an entry
             (should (equal-including-properties anyins-buffers-positions '()))
             ;; try to delete entry already deleted
             (anyins-remove-positions)
             (should (equal-including-properties anyins-buffers-positions '())))

(ert-deftest anyins-compute-position-offset () "Compute position using content to insert."
             (let ((rows '("Lorem ipsum dolor sit" "amet, consectetur adipiscing elit."
                           "Vivamus non erat laoreet," "tincidunt neque" "et, tempus" "nulla. Fusce"
                           "iaculis eros."))
                   (positions '((1 2)
                                (3 6)
                                (2 3)
                                (1 1)
                                (2 1)
                                (3 1)
                                (2 2))))
               ;; we get an association list with line at first followed by an association list for buffer line with data to insert and computed
               ;; position where to insert this data
               (should (equal-including-properties (anyins-compute-position-offset rows positions)
                                                   '((1 ((1 "tincidunt neque")
                                                         (17 "Lorem ipsum dolor sit")))
                                                     (2 ((1 "et, tempus")
                                                         (12 "iaculis eros.")
                                                         (26 "Vivamus non erat laoreet,")))
                                                     (3 ((1 "nulla. Fusce")
                                                         (18 "amet, consectetur adipiscing elit.")))))))
             ;; we get much rows than positions
             (let ((rows '("Lorem ipsum dolor sit" "amet, consectetur adipiscing elit."
                           "Vivamus non erat laoreet," "tincidunt neque" "et, tempus" "nulla. Fusce"
                           "iaculis eros."))
                   (positions '((1 2)
                                (3 6)
                                (2 3))))
               (should (equal-including-properties (anyins-compute-position-offset rows positions)
                                                   '((1 ((2 "Lorem ipsum dolor sit")))
                                                     (2 ((3 "Vivamus non erat laoreet,")))
                                                     (3 ((6 "amet, consectetur adipiscing elit.")))))))
             ;; we get much positions than rows
             (let ((rows '("Lorem ipsum dolor sit" "amet, consectetur adipiscing elit."
                           "Vivamus non erat laoreet,"))
                   (positions '((1 2)
                                (3 6)
                                (2 3)
                                (3 5)
                                (10 8)
                                (2 7))))
               (should (equal-including-properties (anyins-compute-position-offset rows positions)
                                                   '((1 ((2 "Lorem ipsum dolor sit")))
                                                     (2 ((3 "Vivamus non erat laoreet,")))
                                                     (3 ((6 "amet, consectetur adipiscing elit."))))))))

(ert-deftest anyins-prepare-content-to-insert () "Prepare content to insert."
  ;; text is splitted at new line
  (should (equal-including-properties (anyins-prepare-content-to-insert
                                       "hello world\nhello world\nhello world")
                                      '("hello world" "hello world" "hello world")))
  (should (equal-including-properties (anyins-prepare-content-to-insert "hello world")
                                      '("hello world")))
  (should (equal-including-properties (anyins-prepare-content-to-insert nil) nil)))

(provide 'anyins-test)

;;; anyins-test.el ends here
