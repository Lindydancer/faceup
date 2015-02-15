;;; faceup-test-files.el --- Self test of `faceup' using.

;; Copyright (C) 2015 Anders Lindgren

;; Author: Anders Lindgren
;; Keywords: faces languages
;; Created: 2015-02-14

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Self test of `faceup' with a major mode that sets both the
;; `syntax-table' and the `echo-help' property.
;;
;; This file can also be seen as a blueprint of test cases for real
;; major modes.

;;; Code:

(require 'faceup)
(require 'faceup-test-mode)

(defvar faceup-test-files-dir (faceup-this-file-directory)
  "The directory of this file.")

(defun faceup-test-file (file)
  "Test that FILE is fontified as the .faceup file describes.

FILE is interpreted as relative to this source directory."
  (let ((faceup-properties '(face syntax-table help-echo)))
    (faceup-test-font-lock-file 'faceup-test-mode
                                (concat
                                 faceup-test-files-dir
                                 file))))
(faceup-defexplainer faceup-test-file)

(ert-deftest faceup-files ()
  (should (faceup-test-file "files/test1.txt")))

(provide 'faceup-test-files)

;; faceup-test-files.el ends here.
