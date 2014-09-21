# faceup - Regression test system for font-lock

*Author:* Anders Lindgren<br>
*Version:* 0.0.2<br>
*URL:* [https://github.com/Lindydancer/faceup](https://github.com/Lindydancer/faceup)<br>

Emacs is capable of highlighting buffers based on language-specific
`font-lock` rules. This package, `faceup`, makes it possible to
perform regression test for packages that provide font-lock rules.

The underlying idea is to convert text with highlights ("faces")
into a plain text representation using the Faceup markup language.
By comparing the current highlight with a highlight performed with
earlier versions of a package, it's possible to find problems that
otherwise would have been hard to spot.

The `faceup` package is designed to be used in conjunction with
Ert, the standard Emacs regression test system.

The Faceup markup language is a generic markup language, regression
testing is merely one way to use it.

## Installation

This package is designed to be installed with the Emacs built-in
package manager.

## Regression test examples

This section describes the two typical ways regression testing with
this package is performed.

### Full source file highlighting

The most straight-forward way to perform regression testing is to
collect a number of representative source files. From each source
file, say `alpha.mylang`, you can use <kbd>M-x faceup-write-file RET</kbd>
to generate a Faceup file named `alpha.mylang.faceup`, this file
use the Faceup markup language to represent the text with
highlights and is used as a reference in future tests.

An Ert test case can be defined as follows:

       (require 'faceup)

       (defvar mylang-font-lock-test-dir (faceup-this-file-directory))

       (defun mylang-font-lock-test-apps (file)
         "Test that the mylang FILE is fontifies as the .faceup file describes."
         (faceup-test-font-lock-file 'mylang-mode
                                     (concat mylang-font-lock-test-dir file)))
       (faceup-defexplainer mylang-font-lock-test-apps)

       (ert-deftest mylang-font-lock-file-test ()
         (should (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang"))
         ;; ... Add more test files here ...
         )

To execute the tests simply run something like <kbd>M-x ert RET t RET</kbd>.

### Source snippets

To test smaller snippets of code, you can use the
`faceup-test-font-lock-string`. It takes a major mode and a string
written using the Faceup markup language. The functions strips away
the Faceup markup, inserts the plain text into a temporary buffer,
highlights it, converts the result back into the Faceup markup
language, and finally compares the result with the original Faceup
string.

For example:

       (defun mylang-font-lock-test (faceup)
         (faceup-test-font-lock-string 'mylang-mode faceup))
       (faceup-defexplainer mylang-font-lock-test)

       (ert-deftest mylang-font-lock-test-simple ()
         "Simple MyLang font-lock tests."
         (should (mylang-font-lock-test "«k:this» is a keyword"))
         (should (mylang-font-lock-test "«k:function» «f:myfunc» («v:var»)")))


## Executing the tests

Once the tests have been defined, you can use <kbd>M-x ert RET t RET</kbd>
to execute them. Hopefully, you will be given the "all clear".
However, if there is a problem, you will be presented with
something like:

    F mylang-font-lock-file-test
        (ert-test-failed
         ((should
           (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang"))
          :form
          (mylang-font-lock-test-apps "apps/FirstApp/alpha.mylang")
          :value nil :explanation
          ((on-line 2
    		("but_«k:this»_is_not_a_keyword")
    		("but_this_is_not_a_keyword")))))

You should read this that on line 2, the old font-lock rules
highlighted `this` inside `but_this_is_not_a_keyword` (which is
clearly wrong), whereas the new doesn't. Of course, if this is the
desired result (for example, the result of a recent change) you can
simply regenerate the .faceup file and store it as the reference
file for the future.

## The Faceup markup language

The Faceup markup language is designed to be human-readable and
minimalistic.

The two special characters `«` and `»` are used to indicate the
start and end of a section containing a specific face. The start
marker contains information about the face. A number of faces have
a shorthand notation, e.g. `«U:abc»` means that the text "abc" is
underlined. Other faces are expressed with their full name, e.g.
`«:my-face:abc»`.

If more than one face is present at the same location, faceup
represents this as nested marks, where the foremost face is the
inner one, in the faceup representation.

For example, if the text `abcDEFghi` is in *warning* face and the
`DEF` substring is *underlined*, this is represented as
`«W:abc«U:DEF»ghi»`.

In case the use of faces do not nest, the ranges will be split when
represented in Faceup. Concretely, consider the string `abcdefghi`
where `abcdef` are bold and `defghi` underlined. In case it will be
represented in Faceup as `«B:abc«U:def»»«U:ghi»`

Any occurrence of the start or end markers in the original text will
be "escaped" using the start marker in the `faceup` representation.
In other words, the sequences `««` and `«»` represent a start and
end marker, respectively.

### Limitations

Faceup only supports the `face` attribute. (Some font-lock packages
set other attributes like `help-echo`.)

Faceup supports only named faces. It does not support face
properties where colors or attributes are used directly.

## Reference section

### Faceup commands and functions

<kbd>M-x faceup-write-file RET</kbd> - generate a Faceup file based on the
current buffer.

<kbd>M-x faceup-view-file RET</kbd> - view the current buffer converted to
Faceup.

`faceup-markup-{string,buffer}` - convert text with face properties
to the Faceup markup language.

`faceup-render-{string,buffer,buffer-to-string}` - convert a text
with Faceup markup to a text with face properties.

`faceup-clean-{buffer,string}` - remove Faceup markup.

### Regression test support

The following functions can be used as Ert test functions, or can
be used to implement new Ert test functions.

`faceup-test-equal` - Test function, work like Ert:s `equal`, but
more ergonomically when reporting multi-line string errors.
Concretely, it breaks down multi-line strings into lines and
reports which line number the error occurred on and the content of
that line.

`faceup-test-font-lock-buffer` - Test that a buffer is highlighted
according to a reference Faceup text, for a specific major mode.

`faceup-test-font-lock-string` - Test that a text with Faceup
markup is refontified to match the original Faceup markup.

`faceup-test-font-lock-file` - Test that a file is highlighted
according to a reference .faceup file.

`faceup-defexplainer` - Macro, define an explainer function and set
the `ert-explainer` property on the original function, for
functions based on the above test functions.

`faceup-this-file-directory` - Macro, the directory of the current
file.

## Real-world examples

The following are examples of real-world package that use faceup to
test their font-lock keywords.

* [cmake-font-lock](https://github.com/Lindydancer/cmake-font-lock)
  an advanced set of font-lock keywords for the CMake language
* [objc-font-lock](https://github.com/Lindydancer/objc-font-lock)
  highlight Objective-C function calls.



---
Converted from `faceup.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
