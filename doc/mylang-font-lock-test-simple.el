;;; mylang-font-lock-test-simple.el --- Example of inlined faceup tests

(defun mylang-font-lock-test (faceup)
  (faceup-test-font-lock-string 'mylang-mode faceup))
(faceup-defexplainer 'mylang-font-lock-test)

(ert-deftest mylang-font-lock-test-simple ()
  "Simple MyLang font-lock tests."
  (should (mylang-font-lock-test "«k:this» is a keyword"))
  (should (mylang-font-lock-test "«k:function» «f:myfunc» («v:var»)"))

;;; mylang-font-lock-test-simple.el ends here
