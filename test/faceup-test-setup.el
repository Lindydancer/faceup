;;; faceup-test-setup.el --- Setup and execute all tests.

;;; Commentary:

;; This package sets up a suitable enviroment for testing
;; faceup, and executes the tests.
;;
;; Usage:
;;
;;   emacs -q -l faceup-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.
;;
;; Note that different Emacs versions highlight Objective-C slightly
;; differently. The corresponding .faceup file was generated using
;; Emacs 24.3.

;;; Code:

(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)

(defvar faceup-test-setup-directory
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".." "../../faceup"))
  (add-to-list 'load-path (concat faceup-test-setup-directory dir)))

(require 'faceup)
(require 'faceup-test-basics)
(require 'faceup-test-files)

(if noninteractive
    (ert-run-tests-batch-and-exit)
  (ert t))

;;; faceup-test-setup.el ends here
