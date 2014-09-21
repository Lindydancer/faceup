;;; mylang-font-lock-test-apps.el --- Faceup example for MyLang

;;; Code:

(require 'faceup)

(defvar mylang-font-lock-test-dir (faceup-this-file-directory))

(defun mylang-font-lock-test-apps (file)
  "Test that FILE is fontifies as the .faceup file describes."
  (faceup-test-font-lock-file 'mylang-mode
                              (concat mylang-font-lock-test-dir file)))
(faceup-defexplainer mylang-font-lock-test-apps)

(ert-deftest mylang-font-lock-file-test ()
  (should (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang"))
  ;; ... Add more test files here ...
  )

;;; mylang-font-lock-test-apps.el ends here
