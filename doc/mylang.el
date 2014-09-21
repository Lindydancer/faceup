;;; mylang.el --- example major mode with font-lock support.

;;; Code:

;; The .faceup file was generated with the following "old" set of
;; rules. The new rules, below, 1) define "is" to be a keyword and 2)
;; ensure that "this" oesn't match in a context like
;; "and_this_should_not_be_a_keyword".
;;
;; When running `ert' two (expected) errors are triggers. You as a
;; user can inspect both of then and, when satisifed, regenerate the
;; .faceup file.

;; Old rules used when generating the .faceup file:
;;
;;(defvar mylang-font-lock-keywords
;;  '(("\\<this\\>" (0 font-lock-keyword-face))))

(defvar mylang-font-lock-keywords
  '(("\\_<this\\_>" (0 font-lock-keyword-face))))

(defun mylang-mode ()
  "Example major mode with font-lock support."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mylang-mode)
  (setq mode-name "MyLang")
  (set (make-local-variable 'font-lock-defaults)
       '(mylang-font-lock-keywords))
  (run-hooks 'mylang-mode-hook))

;;; mylang.el ends here
