;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^the following text in buffer:$"
      (lambda (text)
        (insert text)
        ))

(Given "^the following text in kill-ring:$"
      (lambda (text)
        (kill-new text)
        ))

(Then "^I should see in buffer$"
      (lambda (expected-content)
        (let ((actual-content (buffer-string))
              (message "Expected '%s' to be '%s', but was not."))
          (cl-assert (s-equals? expected-content actual-content) nil message expected-content actual-content))))

(Then "^I should have an overlay at point$"
      (lambda ()
        (let ((message "Expected overlay at position %s"))
          (cl-assert (overlayp (car (overlays-at (point)))) nil message (point))
          )
        )
      )

(Then "current buffer is \\(read-only\\|writable\\)$"
      (lambda (status)
        (let ((message "Expected %s buffer"))
          (if (string= status "writable")
              (cl-assert (not buffer-read-only) nil message status)
            (cl-assert buffer-read-only nil message status))
          )
        )
      )

(Then "^mode \\(.+\\) is \\(enabled\\|disabled\\)$"
      (lambda (mode status)
        (let ((message "Mode %s has to be %s"))
          (if (string= status "enabled")
              (cl-assert mode nil message mode status)
            (cl-assert mode nil message mode status)
            ))
        )
      )
