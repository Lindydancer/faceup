;;; faceup-test-basics.el --- Tests for the `faceup' package.

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

(ert-deftest faceup-functions ()
  "Test primitive functions."
  (should (equal (faceup-reverse-list-and-split-property-lists '()) '()))
  (should (equal (faceup-reverse-list-and-split-property-lists '(a)) '(a)))
  (should (equal (faceup-reverse-list-and-split-property-lists '(a b)) '(b a)))

  (should (equal (faceup-reverse-list-and-split-property-lists '((:foo t)))
                 '((:foo t))))
  (should (equal (faceup-reverse-list-and-split-property-lists
                  '((:foo t) (:bar nil)))
                 '((:bar nil) (:foo t))))
  (should (equal (faceup-reverse-list-and-split-property-lists
                  '((:foo t :bar nil)))
                 '((:bar nil) (:foo t))))
  (should (equal (faceup-reverse-list-and-split-property-lists
                  '(alpha (:foo t :bar nil) gamma))
                 '(gamma (:bar nil) (:foo t) alpha)))
  )


(ert-deftest faceup-markup ()
  "Test basic `faceup' features."
  ;; ----------
  ;; Basics
  (should (equal (faceup-markup-string "")     ""))
  (should (equal (faceup-markup-string "test") "test"))
  ;; ----------
  ;; Escaping
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
  ;; Anonymous face.
  ;;
  ;;   AA
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (:underline t)) s)
    (should (equal (faceup-markup-string s) "AB«:(:underline t):CD»EF")))
  ;; ----------
  ;; Anonymous face -- plist with two keys.
  ;;
  ;;   AA
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face (:foo t :bar nil)) s)
    (should (equal (faceup-markup-string s)
                   "AB«:(:bar nil):«:(:foo t):CD»»EF")))
  ;; Ditto, with plist in list.
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face ((:foo t :bar nil))) s)
    (should (equal (faceup-markup-string s)
                   "AB«:(:bar nil):«:(:foo t):CD»»EF")))
  ;; ----------
  ;; Anonymous face -- Two plists.
  ;;
  ;;   AA
  ;; ABCDEF
  (let ((s "ABCDEF"))
    (set-text-properties 2 4 '(face ((:foo t) (:bar nil))) s)
    (should (equal (faceup-markup-string s)
                   "AB«:(:bar nil):«:(:foo t):CD»»EF")))
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
    (should (equal (faceup-markup-string s) "«I:«U:AB««CD«»EF»»")))

  ;; ----------
  ;; Multiple properties
  (let ((faceup-properties '(alpha beta gamma)))
    ;; One property.
    (let ((s "ABCDEF"))
      (set-text-properties 2 4 '(alpha (a l p h a)) s)
      (should (equal (faceup-markup-string s) "AB«(alpha):(a l p h a):CD»EF")))

    ;; Two properties, inner enclosed.
    (let ((s "ABCDEFGHIJ"))
      (set-text-properties 2 8 '(alpha (a l p h a)) s)
      (font-lock-append-text-property 4 6 'beta '(b e t a) s)
      (should (equal (faceup-markup-string s)
                     "AB«(alpha):(a l p h a):CD«(beta):(b e t a):EF»GH»IJ")))

    ;; Two properties, same end
    (let ((s "ABCDEFGH"))
      (set-text-properties 2 6 '(alpha (a)) s)
      (add-text-properties 4 6 '(beta (b)) s)
      (should
       (equal
        (faceup-markup-string s)
        "AB«(alpha):(a):CD«(beta):(b):EF»»GH")))

    ;; Two properties, overlap.
    (let ((s "ABCDEFGHIJ"))
      (set-text-properties 2 6 '(alpha (a)) s)
      (add-text-properties 4 8 '(beta (b)) s)
      (should
       (equal
        (faceup-markup-string s)
        "AB«(alpha):(a):CD«(beta):(b):EF»»«(beta):(b):GH»IJ")))
    ))


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
  (should (equal (faceup-clean-string "«(foo)I:ABC»DEF")    "ABCDEF"))
  (should (equal (faceup-clean-string "«:(:foo t):ABC»DEF") "ABCDEF"))
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

(provide 'faceup-test-basics)

;;; faceup-test-basics.el ends here
