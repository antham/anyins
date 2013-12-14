;;; anyins.el --- Enhance rectangular insert

;; Copyright (C) 2013 Anthony HAMON

;; Author: Anthony HAMON <hamon.anth@gmail.com>
;; URL: http://github.com/antham/anyins
;; Keywords: insert, rectangular

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

;;; Code:

(defvar anyins-buffers-positions (make-hash-table :test 'equal)
  "Positions recorded in buffers")

(defun anyins-record-position (position name)
  "Record cursor line and offset"
  (let* ((buffer-positions (gethash name anyins-buffers-positions)))
    (unless buffer-positions
      (setq buffer-positions ())
      )
    (when (assq (car position) buffer-positions)
      (setq buffer-positions (assq-delete-all (car position) buffer-positions))
      )
    (push (list (car position) (cadr position)) buffer-positions)
    (puthash name (sort buffer-positions (lambda(a b)(< (car a) (car b)))) anyins-buffers-positions)
    )
  )

(defun anyins-has-positions (name)
  "Check if a buffer has recorded positions"
  (> (length (anyins-get-positions name)) 0)
  )

(defun anyins-remove-positions (name)
  "Delete recorded positions for name"
  (remhash name anyins-buffers-positions)
  )

(defun anyins-get-positions (name)
  "Get recorded cursor positions"
  (gethash name anyins-buffers-positions)
  )

(defun anyins-prepare-content-to-insert (content)
  "Transform string to list to be inserted"
  (when content
    (split-string content "\n")
    )
  )

(defun anyins-goto-position (position)
  "Move cursor to line at offset"
  (goto-line (car position))
  (goto-char (+ (line-beginning-position) (cadr position)))
  )

(defun anyins-get-current-position ()
  "Get current cursor position"
  (list (line-number-at-pos (point)) (- (point) (line-beginning-position)))
  )

(defun anyins-record-current-position ()
  "Record current cursor position"
  (anyins-record-position (anyins-get-current-position) (buffer-name))
  )

(defun anyins-goto-or-create-position (position)
  "Create position if it doesn't exist, filling with space to do so"
  (let* ((end-position nil))
    (goto-line (car position))
    (end-of-line)
    (setq end-position (anyins-get-current-position))
    (if (<= (cadr position) (cadr end-position))
        (anyins-goto-position position)
      (progn
        (while (> (cadr position) (cadr end-position))
          (insert " ")
          (setq end-position (anyins-get-current-position))
          )
        )
      )
    )
  )

(defun anyins-insert-at-recorded-positions (rows positions)
  "Insert content at each recorded position"
    (dotimes (i (length positions))
      (anyins-goto-position (nth i positions))
      (let ((data (nth i rows)))
        (when (char-or-string-p data)
          (insert data))
        )
      )
    )

(defun anyins-insert-from-current-position (rows)
  "Insert content from current position"
  (let* ((current-position (anyins-get-current-position))
         (line (car current-position)))
    (while (>= (line-number-at-pos (point-max)) line)
      (anyins-goto-or-create-position (list line (cadr current-position)))
      (let ((data (pop rows)))
        (when (char-or-string-p data)
          (insert data)
          )
        )
      (setq line (+ 1 line))
      )
    )
  )

(defun anyins-insert (content name)
  "Insert content"
  (let* ((rows (anyins-prepare-content-to-insert content))
         (positions (anyins-get-positions name)))
    (if (and positions rows)
        (progn
          (anyins-insert-at-recorded-positions rows positions)
          (anyins-remove-positions name))
      (anyins-insert-from-current-position rows)
      )
    )
  )

;;;###autoload
(defun anyins-record ()
  "Record current cursor position"
  (interactive)
  (anyins-record-current-position)
  )

(provide 'anyins)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; anyins.el ends here
