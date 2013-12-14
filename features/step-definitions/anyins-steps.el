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
