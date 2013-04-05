;; faceup-tests.el -- Selftest for the `faceup' package.

;;; Commentary:

;;; Code:

;; Note: In the diagrams below, the face drawn over the other
;; represent a face earier in the list, in other words, one that take
;; presedence.

(ert-deftest faceup-markup ()
  "Selftest of basic `faceup' features."
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
  "Selftest of the clean features of `faceup'."
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
  (should (equal (faceup-clean-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF"))
  )

;; Note "Equal" doesn't seem to 
(ert-deftest faceup-render ()
  "Selftest of the render features of `faceup'."
  (should (equal (faceup-render-string "")     ""))
  (should (equal (faceup-render-string "««") "«"))
  (should (equal (faceup-render-string "«»") "»"))
  (should (equal (faceup-render-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF"))
  )

(provide 'faceup-tests)

;; faceup-tests.el ends here.
