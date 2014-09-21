;;; faceup.el --- Regression test system for font-lock

;; Copyright (C) 2013,2014 Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.2
;; Created: 2013-01-21
;; Keywords: faces languages
;; URL: https://github.com/Lindydancer/faceup

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

;; Emacs is capable of highlighting buffers based on language-specific
;; `font-lock' rules. This package, `faceup', makes it possible to
;; perform regression test for packages that provide font-lock rules.
;;
;; The underlying idea is to convert text with highlights ("faces")
;; into a plain text representation using the Faceup markup language.
;; By comparing the current highlight with a highlight performed with
;; earlier versions of a package, it's possible to find problems that
;; otherwise would have been hard to spot.
;;
;; The `faceup' package is designed to be used in conjunction with
;; Ert, the standard Emacs regression test system.
;;
;; The Faceup markup language is a generic markup language, regression
;; testing is merely one way to use it.

;; Installation:
;;
;; This package is designed to be installed with the Emacs built-in
;; package manager.

;; Regression test examples:
;;
;; This section describes the two typical ways regression testing with
;; this package is performed.
;;
;;
;; Full source file highlighting:
;;
;; The most straight-forward way to perform regression testing is to
;; collect a number of representative source files. From each source
;; file, say `alpha.mylang', you can use `M-x faceup-write-file RET'
;; to generate a Faceup file named `alpha.mylang.faceup', this file
;; use the Faceup markup language to represent the text with
;; highlights and is used as a reference in future tests.
;;
;; An Ert test case can be defined as follows:
;;
;;    (require 'faceup)
;;
;;    (defvar mylang-font-lock-test-dir (faceup-this-file-directory))
;;
;;    (defun mylang-font-lock-test-apps (file)
;;      "Test that the mylang FILE is fontifies as the .faceup file describes."
;;      (faceup-test-font-lock-file 'mylang-mode
;;                                  (concat mylang-font-lock-test-dir file)))
;;    (faceup-defexplainer mylang-font-lock-test-apps)
;;
;;    (ert-deftest mylang-font-lock-file-test ()
;;      (should (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang"))
;;      ;; ... Add more test files here ...
;;      )
;;
;; To execute the tests simply run something like `M-x ert RET t RET'.
;;
;;
;; Source snippets:
;;
;; To test smaller snippets of code, you can use the
;; `faceup-test-font-lock-string'. It takes a major mode and a string
;; written using the Faceup markup language. The functions strips away
;; the Faceup markup, inserts the plain text into a temporary buffer,
;; highlights it, converts the result back into the Faceup markup
;; language, and finally compares the result with the original Faceup
;; string.
;;
;; For example:
;;
;;    (defun mylang-font-lock-test (faceup)
;;      (faceup-test-font-lock-string 'mylang-mode faceup))
;;    (faceup-defexplainer mylang-font-lock-test)
;;
;;    (ert-deftest mylang-font-lock-test-simple ()
;;      "Simple MyLang font-lock tests."
;;      (should (mylang-font-lock-test "«k:this» is a keyword"))
;;      (should (mylang-font-lock-test "«k:function» «f:myfunc» («v:var»)")))
;;

;; Executing the tests:
;;
;; Once the tests have been defined, you can use `M-x ert RET t RET'
;; to execute them. Hopefully, you will be given the "all clear".
;; However, if there is a problem, you will be presented with
;; something like:
;;
;;     F mylang-font-lock-file-test
;;         (ert-test-failed
;;          ((should
;;            (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang"))
;;           :form
;;           (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang")
;;           :value nil :explanation
;;           ((on-line 2
;;     		("but_«k:this»_is_not_a_keyword")
;;     		("but_this_is_not_a_keyword")))))
;;
;; You should read this that on line 2, the old font-lock rules
;; highlighted `this' inside `but_this_is_not_a_keyword' (which is
;; clearly wrong), whereas the new doesn't. Of course, if this is the
;; desired result (for example, the result of a recent change) you can
;; simply regenerate the .faceup file and store it as the reference
;; file for the future.

;; The Faceup markup language:
;;
;; The Faceup markup language is designed to be human-readable and
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
;; For example, if the text `abcDEFghi' is in *warning* face and the
;; `DEF' substring is *underlined*, this is represented as
;; `«W:abc«U:DEF»ghi»'.
;;
;; In case the use of faces do not nest, the ranges will be split when
;; represented in Faceup. Concretely, consider the string `abcdefghi'
;; where `abcdef' are bold and `defghi' underlined. In case it will be
;; represented in Faceup as `«B:abc«U:def»»«U:ghi»'
;;
;; Any occurrence of the start or end markers in the original text will
;; be "escaped" using the start marker in the `faceup' representation.
;; In other words, the sequences `««' and `«»' represent a start and
;; end marker, respectively.
;;
;; Limitations:
;;
;; Faceup only supports the `face' attribute. (Some font-lock packages
;; set other attributes like `help-echo'.)
;;
;; Faceup supports only named faces. It does not support face
;; properties where colors or attributes are used directly.

;; Reference section:
;;
;; Faceup commands and functions:
;;
;; `M-x faceup-write-file RET' - generate a Faceup file based on the
;; current buffer.
;;
;; `M-x faceup-view-file RET' - view the current buffer converted to
;; Faceup.
;;
;; `faceup-markup-{string,buffer}' - convert text with face properties
;; to the Faceup markup language.
;;
;; `faceup-render-{string,buffer,buffer-to-string}' - convert a text
;; with Faceup markup to a text with face properties.
;;
;; `faceup-clean-{buffer,string}' - remove Faceup markup.
;;
;; 
;; Regression test support:
;;
;; The following functions can be used as Ert test functions, or can
;; be used to implement new Ert test functions.
;;
;; `faceup-test-equal' - Test function, work like Ert:s `equal', but
;; more ergonomically when reporting multi-line string errors.
;; Concretely, it breaks down multi-line strings into lines and
;; reports which line number the error occurred on and the content of
;; that line.
;;
;; `faceup-test-font-lock-buffer' - Test that a buffer is highlighted
;; according to a reference Faceup text, for a specific major mode.
;;
;; `faceup-test-font-lock-string' - Test that a text with Faceup
;; markup is refontified to match the original Faceup markup.
;;
;; `faceup-test-font-lock-file' - Test that a file is highlighted
;; according to a reference .faceup file.
;;
;; `faceup-defexplainer' - Macro, define an explainer function and set
;; the `ert-explainer' property on the original function, for
;; functions based on the above test functions.
;;
;; `faceup-this-file-directory' - Macro, the directory of the current
;; file.

;; Real-world examples:
;;
;; The following are examples of real-world package that use faceup to
;; test their font-lock keywords.
;;
;; * [cmake-font-lock](https://github.com/Lindydancer/cmake-font-lock)
;;   an advanced set of font-lock keywords for the CMake language
;;
;; * [objc-font-lock](https://github.com/Lindydancer/objc-font-lock)
;;   highlight Objective-C function calls.
;;

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar faceup-markup-start-char 171)   ;; «
(defvar faceup-markup-end-char   187)   ;; »

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


;; Plain: «W....»
;; Nested: «W...«W...»»

;; Overlapping:   xxxxxxxxxx
;;                    yyyyyyyyyyyy
;;                «X..«Y..»»«Y...»


(defun faceup-markup-string (s)
  "Return the faceup version of the string S."
  (with-temp-buffer
    (insert s)
    (faceup-markup-buffer)))


;;;###autoload
(defun faceup-view-buffer ()
  "Display the faceup representation of the selected buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*FaceUp*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max)))
    (faceup-markup-to-buffer buffer)
    (display-buffer buffer)))


;;;###autoload
(defun faceup-write-file (&optional file-name)
  "Save the faceup representation of the current buffer to the file FILE-NAME.

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
;;     «f:aaaaaaa«U:xxxx»aaaaaa»

(defun faceup-copy-and-quote (start end to-buffer)
  "Quote and insert the text between START and END into TO-BUFFER"
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
  "Convert content of BUFFER to faceup form and insert in TO-BUFFER."
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
  "Next position after POS where the `face' property change.

If POS is nil, also include `point-min' in the search.
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
  "Return string with face properties from FACEUP written with Faceup markup."
  (with-temp-buffer
    (insert faceup)
    (faceup-render-buffer-to-string (current-buffer))))


;;;###autoload
(defun faceup-render-buffer (&optional buffer)
  "Convert BUFFER containing Faceup markup to a new buffer and display it."
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
  "Convert BUFFER containing faceup markup to a string with faces."
  (with-current-buffer buffer
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

;;;###autoload
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
  "Defines an Ert explainer function for FUNCTION.

FUNCTION must return an explanation when the test fails and
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
  "Compares two (multi-line) strings, LHS and RHS, for equality.

This is intended to be used in Ert regression test rules.

When `faceup-test-explain' is non-nil, instead of returning nil
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
  "Verify that BUFFER is fontified as FACEUP for major mode MODE.

If BUFFER is not specified the current buffer is used.

Note that the major mode of the buffer is set to MODE and that
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
  "True if FACEUP is re-fontified as the faceup markup for major mode MODE.

The string FACEUP is stripped from markup, inserted into a
buffer, the requested major mode activated, the buffer is
fontified, the result is again converted to the faceup form, and
compared with the original string."
  (with-temp-buffer
    (insert faceup)
    (faceup-clean-buffer)
    (faceup-test-font-lock-buffer mode faceup)))

(faceup-defexplainer faceup-test-font-lock-string)


(defun faceup-test-font-lock-file (mode file)
  "Verify that FILE is fontified as FILE.faceup for major mode MODE."
  (let ((faceup (with-temp-buffer
                  (insert-file-contents (concat file ".faceup"))
                  (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer
      (insert-file-contents file)
      (faceup-test-font-lock-buffer mode faceup))))

(faceup-defexplainer faceup-test-font-lock-file)


;; ------------------------------
;; Get current file directory. Test cases can use this to locate test
;; files.
;;

(defun faceup-this-file-directory ()
  "The directory of the file where the call to this function is located in.
Intended to be called when a file is loaded."
  (expand-file-name
   (if load-file-name
       ;; File is being loaded.
       (file-name-directory load-file-name)
     ;; File is being evaluated using, for example, `eval-buffer'.
     default-directory)))


;; ----------------------------------------------------------------------
;; The end
;;

(provide 'faceup)

;;; faceup.el ends here
