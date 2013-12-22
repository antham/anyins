;;; anyins.el --- Insert anything from shell command or kill-ring

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

(defface anyins-recorded-positions
  '((((background dark)) :background "green" :foreground "white")
    (((background light)) :background "green" :foreground "white"))
  "Marker for recorded position" :group 'anyins)

(defvar anyins-buffers-positions '()
  "Positions recorded in buffers")

(defvar anyins-buffers-overlays '()
  "Overlays recorded in buffers")

(defun anyins-record-position (position)
  "Record cursor line and offset, return true if position doesn't exist yet"
  (let ((previous-length (length anyins-buffers-positions)))
    (setq anyins-buffers-positions (delete-dups (append anyins-buffers-positions (list position))))
    (progn
      (when (/= previous-length (length anyins-buffers-positions))
        t
        )
      )
    )
  )

(defun anyins-remove-positions ()
  "Delete recorded positions"
  (setq anyins-buffers-positions '())
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
  (when (anyins-record-position (anyins-get-current-position))
    (anyins-create-overlay (anyins-get-current-position) (point))
    )
  )

(defun anyins-create-overlay (position point)
  "Create an overlay at point"
  (let ((overlay (make-overlay point (+ 1 point))))
    (overlay-put overlay 'face 'anyins-recorded-positions)
    (push overlay anyins-buffers-overlays)
    )
  )

(defun anyins-delete-overlays ()
  "Delete overlays"
  (when anyins-buffers-overlays
    (dolist (overlay anyins-buffers-overlays)
      (delete-overlay overlay))
    (setq anyins-buffers-overlays '())
    )
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

(defun anyins-insert (content)
  "Insert content"
  (let* ((rows (anyins-prepare-content-to-insert content)))
    (if (and anyins-buffers-positions rows)
        (progn
          (anyins-insert-at-recorded-positions rows anyins-buffers-positions)
          (anyins-remove-positions))
      (anyins-insert-from-current-position rows)
      )
    )
  )

(defun anyins-clear()
  "Clear everything recorded for this buffer"
  (setq buffer-read-only nil)
  (anyins-delete-overlays)
  (anyins-remove-positions)
  (anyins-mode 0)
  )

;;;###autoload
(define-minor-mode anyins-mode
  "Anyins minor mode"
  :lighter " Anyins"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-g") '(lambda()
                                           (interactive)
                                           (anyins-clear)))
            (define-key map (kbd "RET") '(lambda()
                                           (interactive)
                                           (anyins-record-current-position)))
            (define-key map (kbd "k") '(lambda()
                                         (interactive)
                                         (setq buffer-read-only nil)
                                         (anyins-insert (car kill-ring))
                                         (anyins-clear)))
            (define-key map (kbd "s") '(lambda()
                                         (interactive
                                          (let ((command (read-string "shell command : ")))
                                            (setq buffer-read-only nil)
                                            (anyins-insert (shell-command-to-string command))
                                            (anyins-clear)))))
            map)
  (if anyins-mode
      (progn
        (setq buffer-read-only t)
        (make-variable-buffer-local 'anyins-buffers-overlays)
        (make-variable-buffer-local 'anyins-buffers-positions)
        )
      )
  )

(provide 'anyins)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; anyins.el ends here
