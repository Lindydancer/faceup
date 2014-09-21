;;; faceup-tests.el --- Tests for the `faceup' package.

;; Copyright (C) 2014 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: languages, faces
;; URL: https://github.com/Lindydancer/char-font-lock

;;; Commentary:

;;; Code:

;; Note: In the diagrams below, the face drawn over the other
;; represent a face earlier in the list, in other words, one that take
;; precedence.

(add-to-list 'load-path (concat
			 (if load-file-name
			     (file-name-directory load-file-name)
			   default-directory)
			 ".."))

(require 'faceup)

(ert-deftest faceup-markup ()
  "Test basic `faceup' features."
  (should (equal (faceup-markup-string "")     ""))
  (should (equal (faceup-markup-string "test") "test"))
  (should (equal (faceup-markup-string "«") "««"))
  (should (equal (faceup-markup-string "«A«B«C«") "««A««B««C««"))
  (should (equal (faceup-markup-string "»") "«»"))
  (should (equal (faceup-markup-string "»A»B»C»") "«»A«»B«»C«»"))
  ;; ----------
  ;; Plain property.
  ;;
  ;;   UU
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face underline) s)
    (should (equal (faceup-markup-string s) "AB«U:CD»EF")))
  ;; ----------
  ;; Plain property, full text
  ;;
  ;; UUUUUU
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 0 6 '(face underline) s)
    (should (equal (faceup-markup-string s) "«U:ABCDEF»")))
  ;; ----------
  ;; Nested properties.
  ;;
  ;;   UU
  ;;  IIII
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 1 2 '(face italic) s)
    (set-text-properties 2 4 '(face (underline italic)) s)
    (set-text-properties 4 5 '(face italic) s)
    (should (equal (faceup-markup-string s) "A«I:B«U:CD»E»F")))
  ;; ----------
  ;; Overlapping, but not nesting, properties.
  ;;
  ;;   UUU
  ;;  III
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 1 2 '(face italic) s)
    (set-text-properties 2 4 '(face (underline italic)) s)
    (set-text-properties 4 5 '(face underline) s)
    (should (equal (faceup-markup-string s) "A«I:B«U:CD»»«U:E»F")))
  ;; ----------
  ;; Overlapping, but not nesting, properties.
  ;;
  ;;  III
  ;;   UUU
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 1 2 '(face italic) s)
    (set-text-properties 2 4 '(face (italic underline)) s)
    (set-text-properties 4 5 '(face underline) s)
    (should (equal (faceup-markup-string s) "A«I:B»«U:«I:CD»E»F")))
  ;; ----------
  ;; More than one face at the same location.
  ;;
  ;; The property to the front takes precedence, it is rendered as the
  ;; innermost parenthesis pair.
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (underline italic)) s)
    (should (equal (faceup-markup-string s) "AB«I:«U:CD»»EF")))
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (italic underline)) s)
    (should (equal (faceup-markup-string s) "AB«U:«I:CD»»EF")))
  ;; ----------
  ;; Equal ranges, full text.
  (let ((s "ABCDEF"))
    (set-text-properties 0 6 '(face (underline italic)) s)
    (should (equal (faceup-markup-string s) "«I:«U:ABCDEF»»")))
  ;; Ditto, with stray markup characters.
  (let ((s "AB«CD»EF"))
    (set-text-properties 0 8 '(face (underline italic)) s)
    (should (equal (faceup-markup-string s) "«I:«U:AB««CD«»EF»»"))))


(ert-deftest faceup-clean ()
  "Test the clean features of `faceup'."
  (should (equal (faceup-clean-string "")     ""))
  (should (equal (faceup-clean-string "test") "test"))
  (should (equal (faceup-clean-string "AB«U:CD»EF")         "ABCDEF"))
  (should (equal (faceup-clean-string "«U:ABCDEF»")         "ABCDEF"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»E»F")     "ABCDEF"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»»«U:E»F") "ABCDEF"))
  (should (equal (faceup-clean-string "AB«I:«U:CD»»EF")     "ABCDEF"))
  (should (equal (faceup-clean-string "«I:«U:ABCDEF»»")     "ABCDEF"))
  ;; Escaped markup characters.
  (should (equal (faceup-clean-string "««") "«"))
  (should (equal (faceup-clean-string "«»") "»"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF")))


(ert-deftest faceup-render ()
  "Test the render features of `faceup'."
  (should (equal (faceup-render-string "")     ""))
  (should (equal (faceup-render-string "««") "«"))
  (should (equal (faceup-render-string "«»") "»"))
  (should (equal (faceup-render-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF")))

(defvar faceup-test-dummy)

(ert-deftest faceup-directory ()
  "Test `faceup-this-file-directory'."
  (setq qqq default-directory)
  (let* ((dir (concat (file-name-directory
                       (symbol-file 'faceup-this-file-directory))
                      "test/"))
         (file (concat dir "faceup-test-this-file-directory.el")))
    ;; Test normal load.
    (makunbound 'faceup-test-this-file-directory)
    (load-file file)
    (should (equal faceup-test-this-file-directory dir))
    ;; Test `eval-buffer'.
    (makunbound 'faceup-test-this-file-directory)
    (save-excursion
      (find-file file)
      (eval-buffer))
    (should (equal faceup-test-this-file-directory dir))
    ;; Test `eval-defun'.
    (makunbound 'faceup-test-this-file-directory)
    (save-excursion
      (find-file file)
      (eval-defun nil))
    (should (equal faceup-test-this-file-directory dir))))

(provide 'faceup-tests)

;;; faceup-tests.el ends here
