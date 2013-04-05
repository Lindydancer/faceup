;; faceup.el -- Markup language and regression test system for text with faces.

;; Copyright (C) 2013 Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.0
;; Created: 2013-01-21
;; Keywords: faces languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The `faceup' markup language is a plain text representation of text
;; with "face" information.
;;
;; This module provides functions for:
;;
;; * Converting text with face properties into the faceup
;;   plain-text representation
;;
;; * Rendering the faceup representation as text with real face
;;   properties
;;
;; * regression testing of text with "face" properties, both generic
;;   and `font-lock' specific.

;; Usage:
;;
;; Place this file in the load path and add the following to an
;; appropriate init file, like ~/.emacs:
;;
;; (autoload 'faceup-write-file    "faceup" nil t)
;; (autoload 'faceup-view-buffer   "faceup" nil t)
;; (autoload 'faceup-clean-buffer  "faceup" nil t)
;; (autoload 'faceup-render-buffer "faceup" nil t)

;; Regression test support:
;;
;; This module provides support for regression tests of font-lock
;; packages.
;;
;; Test rules are typically defined using a regression testing
;; framework like `ert' which is part of the Emacs distribution.
;;
;; The following functions are available:
;;
;; (faceup-test-font-lock-string mode faceup)
;;
;;   Verifies that `faceup' (when stripped of the `faceup' markup) is
;;   fontifies as described by `faceup' in major mode `mode'. This is
;;   typically used form small hand-crafted examples to ensure that
;;   they are fontified correctly.
;;
;; (faceup-test-font-lock-file mode file)
;;
;;   Verify that `file' is fontified as the facit file `file.faceup'
;;   for major mode `mode'. This is typically used for large,
;;   complete, source files to ensure that changes in font-lock rules
;;   do not affect the fontification.

;; The `faceup' markup language:
;;
;; The `faceup' markup language is designed to be human-readable and
;; minimalistic.
;;
;; The two special characters `«' and `»' are used to indicate the
;; start and end of a section containing a specific face. The start
;; marker contains information about the face. A number of faces have
;; a shorthand notation, e.g. `«U:abc»' means that the text "abc" is
;; underlined. Other faces are expressed with their full name, e.g.
;; `«:my-face:abc»'.
;;
;; If more than one face is present at the same location, faceup
;; represents this as nested marks, where the foremost face is the
;; inner one, in the faceup representation.
;;
;; For example, if the text "abcDEFghi" is in "warning" face and the
;; "DEF" substring is underlined, this is represented as
;; `«W:abc«U:DEF»ghi»'.
;;
;; In case the use of faces do not nest, the ranges will be split when
;; represented in `faceup'. Concretely, consider the string,
;; "abcdefghi", where "abcdef" are bold and "defghi" underlined. In
;; case, the latter is foremost, it will be represented in `faceup' as
;; `«B:abc«U:def»»«U:ghi»'
;;
;; Any occurrence of the start or end markers in the original text will
;; be "escaped" using the start marker in the `faceup' representation.
;; In other words, the sequences `««' and `«»' represent a start or
;; end marker, respectively.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar faceup-markup-start-char 171)
(defvar faceup-markup-end-char   187)

(defvar faceup-face-short-alist
  '(;; Generic faces (uppercase letters)
    (bold                                . "B")
    (bold-italic                         . "Q")
    (default                             . "D")
    (error                               . "E")
    (highlight                           . "H")
    (italic                              . "I")
    (underline                           . "U")
    (warning                             . "W")
    ;; font-lock-specific faces (lowercase letters)
    (font-lock-builtin-face              . "b")
    (font-lock-comment-delimiter-face    . "m")
    (font-lock-comment-face              . "x")
    (font-lock-constant-face             . "c")
    (font-lock-doc-face                  . "d")
    (font-lock-function-name-face        . "f")
    (font-lock-keyword-face              . "k")
    (font-lock-negation-char-face        . "n")
    (font-lock-preprocessor-face         . "p")
    (font-lock-regexp-grouping-backslash . "h")
    (font-lock-regexp-grouping-construct . "o")
    (font-lock-string-face               . "s")
    (font-lock-type-face                 . "t")
    (font-lock-variable-name-face        . "v")
    (font-lock-warning-face              . "w"))
  "Alist from faces to one-character representation.")


;; Plain: <W....>
;; Nested: <W...<W...>>

;; Overlapping:   xxxxxxxxxx
;;                    yyyyyyyyyyyy
;;                <X..<Y..>><Y...>


(defun faceup-markup-string (s)
  "Return the faceup version of the string `s'."
  (with-temp-buffer
    (insert s)
    (faceup-markup-buffer)))


(defun faceup-view-buffer ()
  "Display the faceup representation of the selected buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*FaceUp*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (faceup-markup-to-buffer buffer)
    (display-buffer buffer)))


(defun faceup-write-file (&optional file-name)
  "Save the faceup representation of the current buffer to a file.

Unless a name is given, the file will be named xxx.faceup, where
xxx is the file name associated with the buffer."
  (interactive
   (let ((suggested-name (and (buffer-file-name)
                              (concat (buffer-file-name)
                                      ".faceup"))))
     (list (read-file-name "Write faceup file: "
                           default-directory
                           suggested-name
                           nil
                           (file-name-nondirectory suggested-name)))))
  (unless file-name
    (setq file-name (concat (buffer-file-name) ".faceup")))
  (let ((buffer (current-buffer)))
    (with-temp-buffer
      (faceup-markup-to-buffer (current-buffer) buffer)
      ;; Note: Must set `require-final-newline' inside
      ;; `with-temp-buffer', otherwise the value will be overridden by
      ;; the buffers local value.
      (let ((require-final-newline nil))
        (write-file file-name t)))))


(defun faceup-markup-buffer ()
  "Return a string with the content of the buffer using faceup markup."
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (faceup-markup-to-buffer (current-buffer) buf)
      (buffer-substring-no-properties (point-min) (point-max)))))


;; Idea:
;;
;; Typically, only one face is used. However, when two faces are used,
;; the one of top is typically shorter. Hence, the faceup variant
;; should treat the inner group of nested ranges the upper (i.e. the
;; one towards the front.) For example:
;;
;;     <f:aaaaaaa<U:xxxx>aaaaaa>

(defun faceup-copy-and-quote (start end to-buffer)
  (let ((not-markup (concat "^"
                            (make-string 1 faceup-markup-start-char)
                            (make-string 1 faceup-markup-end-char))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((old (point)))
          (skip-chars-forward not-markup end)
          (let ((s (buffer-substring-no-properties old (point))))
            (with-current-buffer to-buffer
              (insert s))))
        ;; Quote stray markup characters.
        (unless (= (point) end)
          (let ((next-char (following-char)))
            (with-current-buffer to-buffer
              (insert faceup-markup-start-char)
              (insert next-char)))
          (forward-char))))))

(defun faceup-markup-to-buffer (to-buffer &optional buffer)
  "Convert content of `buffer' to faceup form in `to-buffer'."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    ;; Font-lock often only fontifies the visible sections. This
    ;; ensures that the entire buffer is fontified before converting
    ;; it.
    (if font-lock-mode
        (font-lock-fontify-region (point-min) (point-max)))
    (let ((last-pos (point-min))
          (pos nil)
          (state '()))                  ; List of faces.
      (while
          (progn
            (setq pos (faceup-next-face-property-change pos))
            pos)
        ;; Insert content.
        (faceup-copy-and-quote last-pos pos to-buffer)
        (setq last-pos pos)
        (let ((faces (get-text-property pos 'face)))
          (unless (listp faces)
            (setq faces (list faces)))
          (let ((next-state faces))
            ;; Let equal ranges live on.
            (setq faces (reverse faces))
            (setq state (reverse state))
            (while (and state
                        faces
                        (equal (car state) (car faces)))
              (pop faces)
              (pop state))
            ;; End faces that should not be included in the next state.
            (while state
              (with-current-buffer to-buffer
                (insert (make-string 1 faceup-markup-end-char)))
              (pop state))
            ;; Start new ranges.
            (while faces
              (with-current-buffer to-buffer
                (insert (make-string 1 faceup-markup-start-char))
                (let ((short (assq (car faces) faceup-face-short-alist)))
                  (if short
                      (insert (cdr short) ":")
                    (insert ":" (symbol-name (car faces)) ":"))))
              (pop faces))
            ;; Insert content.
            (setq state next-state))))
      ;; Insert whatever is left after the last face change.
      (faceup-copy-and-quote last-pos (point-max) to-buffer))))



;; Some basic facts:
;;
;; (get-text-property (point-max) ...) always return nil. To check the
;; last character in the buffer, use (- (point-max) 1).
;;
;; If a text has more than one face, the first one in the list
;; takes precedence, when being viewed in Emacs.
;;
;;   (let ((s "ABCDEF"))
;;      (set-text-properties 1 4
;;        '(face (font-lock-warning-face font-lock-variable-name-face)) s)
;;      (insert s))
;;
;;   => ABCDEF
;;
;; Where DEF is drawn in "warning" face.


(defun faceup-next-face-property-change (pos)
  "Next position after `pos' where the `face' property change.

If `pos' is `nil', also include `point-min' in the search.
If last character contains a face property, return `point-max'."
  (if (equal pos (point-max))
      ;; Last search returned `point-max'. There is no more to search
      ;; for.
      nil
    (if (and (null pos)
             (get-text-property (point-min) 'face))
        ;; `pos' is `nil' and the character at `point-min' contains a
        ;; face property, return `point-min'.
        (point-min)
      (unless pos
        ;; Start from the beginning.
        (setq pos (point-min)))
      ;; Do a normal search. Compensate for that
      ;; `next-single-property-change' does not include the end of the
      ;; buffer, even when a face reach it.
      (let ((res (next-single-property-change pos 'face)))
        (if (and (not res)              ; No more found.
                 (not (equal pos (point-max))) ; Not already at the end.
                 (not (equal (point-min) (point-max))) ; Not an empty buffer.
                 (get-text-property (- (point-max) 1) 'face))
            ;; If a face property goes all the way to the end of the
            ;; buffer, return `point-max'.
            (point-max)
          res)))))


;; ----------------------------------------------------------------------
;; Renderer
;;

;; Functions to convert from the faceup textual representation to text
;; with real face properties.

(defun faceup-render-string (faceup)
  "Return string with face properties as described by `faceup'."
  (with-temp-buffer
    (insert faceup)
    (faceup-render-buffer-to-string (current-buffer))))


(defun faceup-render-buffer (&optional buffer)
  "Convert the faceup markup to a buffer with real face properties."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((text (faceup-render-buffer-to-string buffer)))
    (let ((dest-buffer (get-buffer-create "*FaceUp rendering*")))
      (with-current-buffer dest-buffer
        (delete-region (point-min) (point-max))
        (insert text))
      (display-buffer dest-buffer))))


(defun faceup-render-buffer-to-string (buffer)
  "Convert a buffer containing faceup markup to a string with faces."
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((res "")
          (last-point (point))
          (state '())
          (not-markup (concat
                       "^"
                       (make-string 1 faceup-markup-start-char)
                       (make-string 1 faceup-markup-end-char))))
      (while (progn
               (skip-chars-forward not-markup)
               (if (not (equal last-point (point)))
                   (let ((text (buffer-substring-no-properties
                                last-point (point))))
                     (if state
                         ;; Create one of:
                         ;;    (face name-of-face) or
                         ;;    (face (name-of-face name-of-face ...))
                         (let ((state0 (if (cdr state)
                                           state
                                         (car state))))
                           (set-text-properties 0 (length text)
                                                (list 'face state0)
                                                text)))
                     (setq res (concat res text))
                     (setq last-point (point))))
                (not (eobp)))
        (if (equal (following-char) faceup-markup-start-char)
            ;; Start marker.
            (progn
              (forward-char)
              (if (or (equal (following-char) faceup-markup-start-char)
                      (equal (following-char) faceup-markup-end-char))
                  ;; Escaped markup character.
                  (progn
                    (setq last-point (point))
                    (forward-char))
                ;; Markup sequence.
                (if (equal (following-char) ?:)
                    ;; :face-name:
                    (let ((p (point)))
                      (forward-char)
                      (skip-chars-forward "^:")
                      (unless (eobp)
                        (forward-char))
                      (push (intern (buffer-substring-no-properties
                                     p (point)))
                            state))
                  ;; X:
                  (push (car (rassoc (buffer-substring-no-properties
                                      (point) (+ (point) 1))
                                     faceup-face-short-alist))
                        state)
                  (forward-char 2))
                (setq last-point (point))))
          ;; End marker.
          (pop state)
          (forward-char)
          (setq last-point (point))))
      res)))

;; ----------------------------------------------------------------------

(defun faceup-clean-buffer ()
  "Remove faceup markup from buffer."
  (interactive)
  (goto-char (point-min))
  (let ((not-markup (concat
                     "^"
                     (make-string 1 faceup-markup-start-char)
                     (make-string 1 faceup-markup-end-char))))
    (while (progn (skip-chars-forward not-markup)
                  (not (eobp)))
      (if (equal (following-char) faceup-markup-end-char)
          ;; End markers are always on their own.
          (delete-char 1)
        ;; Start marker.
        (delete-char 1)
        (if (or (equal (following-char) faceup-markup-start-char)
                (equal (following-char) faceup-markup-end-char))
            ;; Escaped markup character, delete the escape and skip
            ;; the original character.
            (forward-char)
          ;; Markup sequence.
          (if (equal (following-char) ?:)
              ;; :face-name:
              (let ((p (point)))
                (forward-char)
                (skip-chars-forward "^:")
                (unless (eobp)
                  (forward-char))
                (delete-region p (point)))
            ;; X:
            (delete-char 1)             ; The one-letter form.
            (delete-char 1)))))))       ; The colon.


(defun faceup-clean-string (s)
  (with-temp-buffer
    (insert s)
    (faceup-clean-buffer)
    (buffer-substring (point-min) (point-max))))


;; ----------------------------------------------------------------------
;; Regression test support
;;

(defvar faceup-test-explain nil
  "When non-nil, tester functions returns a text description on failure.

Of course, this only work for test functions aware of this
variable, like `faceup-test-equal' and functions based on this
function.

This is intended to be used to simplify `ert' explain functions,
which could be defined as:

    (defun my-test (args...) ...)
    (defun my-test-explain (args...)
      (let ((faceup-test-explain t))
        (the-test args...)))
    (put 'my-test 'ert-explainer 'my-test-explain)

Alternative, you can use the macro `faceup-defexplainer' as follows:

    (defun my-test (args...) ...)
    (faceup-defexplainer my-test)

Test functions, like `faceup-test-font-lock-buffer', built on top
of `faceup-test-equal', and other functions that adhere to this
variable, can easily define their own explainer functions.")

(defmacro faceup-defexplainer (function)
  "Defines an `ert' explainer function for `function'.

`function' must return an explanation when the test fails and
`faceup-test-explain' is set."
  (let ((name (intern (concat (symbol-name function) "-explainer"))))
    `(progn
       (defun ,name (&rest args)
         (let ((faceup-test-explain t))
           (apply (quote ,function) args)))
       (put (quote ,function) 'ert-explainer (quote ,name)))))


;; ------------------------------
;; Multi-line string support.
;;

(defun faceup-test-equal (lhs rhs)
  "Compares two (multi-line) strings for equality.

This is intended to be used in the `ert' regression test rules.

When `faceup-test-explain' is non-nil, instead of returning `nil'
on inequality, a list is returned with a explanation what
differs. Currently, this function reports 1) if the number of
lines in the strings differ. 2) the lines and the line numbers on
which the string differed.

For example:
    (let ((a \"ABC\\nDEF\\nGHI\")
          (b \"ABC\\nXXX\\nGHI\\nZZZ\")
          (faceup-test-explain t))
      (message \"%s\" (faceup-test-equal a b)))

    ==> (4 3 number-of-lines-differ (on-line 2 (DEF) (XXX)))

When used in an `ert' rule, the output is as below:

    (ert-deftest faceup-test-equal-example ()
      (let ((a \"ABC\\nDEF\\nGHI\")
            (b \"ABC\\nXXX\\nGHI\\nZZZ\"))
        (should (faceup-test-equal a b))))

    F faceup-test-equal-example
        (ert-test-failed
         ((should
           (faceup-test-equal a b))
          :form
          (faceup-test-equal \"ABC\\nDEF\\nGHI\" \"ABC\\nXXX\\nGHI\\nZZZ\")
          :value nil :explanation
          (4 3 number-of-lines-differ
             (on-line 2
                      (\"DEF\")
                      (\"XXX\")))))"
  (if (equal lhs rhs)
      t
    (if faceup-test-explain
        (let ((lhs-lines (split-string lhs "\n"))
              (rhs-lines (split-string rhs "\n"))
              (explanation '())
              (line 1))
          (unless (= (length lhs-lines) (length rhs-lines))
            (setq explanation (list 'number-of-lines-differ
                                    (length lhs-lines) (length rhs-lines))))
          (while lhs-lines
            (let ((one (pop lhs-lines))
                  (two (pop rhs-lines)))
              (unless (equal one two)
                (setq explanation
                      (cons (list 'on-line line (list one) (list two))
                            explanation)))
              (setq line (+ line 1))))
          (nreverse explanation))
      nil)))

(faceup-defexplainer faceup-test-equal)


;; ------------------------------
;; Font-lock regression test support.
;;

(defun faceup-test-font-lock-buffer (mode faceup &optional buffer)
  "Verify that `buffer' is fontified as `faceup' for major mode `mode'.

If `buffer' is not specified the current buffer is used.

Note that the major mode of the buffer is set to `mode' and that
the buffer is fontified."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (funcall mode)
    (font-lock-fontify-region (point-min) (point-max))
    (let ((result (faceup-markup-buffer)))
      (faceup-test-equal faceup result))))

(faceup-defexplainer faceup-test-font-lock-buffer)


(defun faceup-test-font-lock-string (mode faceup)
  "True if `faceup' is re-fontified as the faceup markup for major mode `mode'.

The string `faceup' is stripped from markup, inserted into a
buffer, the requested major mode activated, the buffer is
fontified, the result is again converted to the faceup form, and
compared with the original string."
  (with-temp-buffer
    (insert faceup)
    (faceup-clean-buffer)
    (faceup-test-font-lock-buffer mode faceup)))

(faceup-defexplainer faceup-test-font-lock-string)


(defun faceup-test-font-lock-file (mode file)
  "Verify that `file' is fontified as `file.faceup' for major mode `mode'."
  (let ((faceup (with-temp-buffer
                  (insert-file-contents (concat file ".faceup"))
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert-file-contents file)
      (faceup-test-font-lock-buffer mode faceup))))

(faceup-defexplainer faceup-test-font-lock-file)


;; ----------------------------------------------------------------------
;; Convenience functions
;;


(defun faceup-list-latin1 ()
  (interactive)
  (with-output-to-temp-buffer "*Latin1*"
    (let ((i 0))
      (while (< i 256)
        (princ (format "%c" i))
        (if (equal (% i 16) 0)
            (princ "\n"))
        (setq i (+ i 1))))))




(provide 'faceup)

;; faceup.el ends here.
