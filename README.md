# faceup - Regression test system for font-lock

*Author:* Anders Lindgren<br>
*Version:* 0.0.3<br>
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

The two special characters `«` and `»` marks the start and end of a
range of a face.

### Compact format for special faces

The compact format `«<LETTER>:text»` is used for a number of common
faces. For example, `«U:abc»` means that the text `abc` is
underlined.

See `faceup-face-short-alist` for the known faces and the
corresponding letter.

### Full format

The format `«:<NAME OF FACE>:text»` is used use to encode other
faces.

For example `«:my-special-face:abc»` meanst that `abc` has the face
`my-special-face`.

### Anonymous faces

An "anonymous face" is when the `face` property contains a property
list (plist) on the form `(:key value)`. This is represented using
a variant of the full format: `«:(:key value):text»`.

For example, `«:(:background "red"):abc»` represent the text `abc`
with a red background.

### Multiple properties

In case a text contains more than one face property, they are
represented using nested sections.

For example:

* `«B:abc«U:def»»` represent the text `abcdef` that is both *bold*
  and *underlined*.
* `«W:abc«U:def»ghi»` represent the text `abcdefghi` where the
  entire text is in *warning* face and `def` is *underlined*.

In case two faces partially overlap, the ranges will be split when
represented in Faceup. For example:

* `«B:abc«U:def»»«U:ghi»` represent the text `abcdefghi` where
  `abcdef` is bold and `defghi` is underlined.

### Escaping start and end markers

Any occurrence of the start or end markers in the original text
will be escaped using the start marker in the Faceup
representation. In other words, the sequences `««` and `«»`
represent a start and end marker, respectively.

### Other properties

In addition to representing the `face` property (or, more
correctly, the value of `faceup-default-property`) other properties
can be encoded. The variable `faceup-properties` contains a list of
properties to track. If a property behaves like the `face`
property, it is encoded as described above, with the addition of
the property name placed in parentheses, for example:
`«(my-face)U:abd»`.

The variable `faceup-face-like-properties` contains a list of
properties considered face-like.

Properties that are not considered face-like are always encoded
using the full format and the don't nest. For example:
`«(my-fibonacci-property):(1 1 2 3 5 8):abd»`.

Examples of properties that could be tracked are:

* `font-lock-face` -- an alias to `face` when `font-lock-mode` is
  enabled.
* `syntax-table` -- used by a custom `syntax-propertize` to
  override the default syntax table.
* `help-echo` -- provides tooltip text displayed when the mouse is
  held over a text.

## Reference section

### Faceup commands and functions

<kbd>M-x faceup-write-file RET</kbd> - generate a Faceup file based on the
current buffer.

<kbd>M-x faceup-view-file RET</kbd> - view the current buffer converted to
Faceup.

`faceup-markup-{string,buffer}` - convert text with properties to
the Faceup markup language.

`faceup-render-view-buffer` - convert buffer with Faceup markup to
a buffer with real text properties and display it.

`faceup-render-string` - return string with real text properties
from a string with Faceup markup.

`faceup-render-to-{buffer,string}` - convert buffer with Faceup
markup to a buffer/string with real text properties.

`faceup-clean-{buffer,string}` - remove Faceup markup from buffer
or string.

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
