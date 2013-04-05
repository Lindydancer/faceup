# faceup - Markup language and regression test system for text with faces

*Author:* Anders Lindgren<br>
*Version:* 0.0.0<br>

The `faceup` markup language is a plain text representation of text
with "face" information.

## This module provides functions for

* Converting text with face properties into the faceup
  plain-text representation
* Rendering the faceup representation as text with real face
  properties
* regression testing of text with "face" properties, both generic
  and `font-lock` specific.

## Usage

Place this file in the load path and add the following to an
appropriate init file, like ~/.emacs:

    (autoload 'faceup-write-file    "faceup" nil t)
    (autoload 'faceup-view-buffer   "faceup" nil t)
    (autoload 'faceup-clean-buffer  "faceup" nil t)
    (autoload 'faceup-render-buffer "faceup" nil t)

## Regression test support

This module provides support for regression tests of font-lock
packages.

Test rules are typically defined using a regression testing
framework like `ert` which is part of the Emacs distribution.

### The following functions are available

    (faceup-test-font-lock-string mode faceup)

  Verifies that `faceup` (when stripped of the `faceup` markup) is
  fontifies as described by `faceup` in major mode `mode`. This is
  typically used form small hand-crafted examples to ensure that
  they are fontified correctly.

    (faceup-test-font-lock-file mode file)

  Verify that `file` is fontified as the facit file `file.faceup`
  for major mode `mode`. This is typically used for large,
  complete, source files to ensure that changes in font-lock rules
  do not affect the fontification.

## The `faceup` markup language

The `faceup` markup language is designed to be human-readable and
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

For example, if the text "abcDEFghi" is in "warning" face and the
"DEF" substring is underlined, this is represented as
`«W:abc«U:DEF»ghi»`.

In case the use of faces do not nest, the ranges will be split when
represented in `faceup`. Concretely, consider the string,
"abcdefghi", where "abcdef" are bold and "defghi" underlined. In
case, the latter is foremost, it will be represented in `faceup` as
`«B:abc«U:def»»«U:ghi»`

Any occurrence of the start or end markers in the original text will
be "escaped" using the start marker in the `faceup` representation.
In other words, the sequences `««` and `«»` represent a start or
end marker, respectively.


---
Converted from `faceup.el` by *el2markup*.