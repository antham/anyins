(require 'f)

(defvar anyins-support-path
  (f-dirname load-file-name))

(defvar anyins-features-path
  (f-parent anyins-support-path))

(defvar anyins-root-path
  (f-parent anyins-features-path))

(add-to-list 'load-path anyins-root-path)

(require 'anyins)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 (anyins-mode 0)
 (setq buffer-read-only nil)
 (remove-overlays)
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
