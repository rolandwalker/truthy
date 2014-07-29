[![Build Status](https://secure.travis-ci.org/rolandwalker/truthy.png?branch=master)](http://travis-ci.org/rolandwalker/truthy)

# Overview

Test the content of a value in Emacs Lisp.

## Quickstart

```elisp
(require 'truthy)
 
(truthy "")                   ; nil
(truthy '[])                  ; nil
(truthy 0)                    ; nil
(truthy (lambda ()))          ; nil
(truthy (make-sparse-keymap)) ; nil
(truthy 1)                    ; 1
(truthy '(a b c))             ; '(a b c)
(truthy '(nil nil nil))       ; nil
(truthy '([] "" 0))           ; nil
 
(truthy-s '([] "" 0))         ; '([] "" 0)         ; shallow test
 
(truthy-l '(nil nil nil))     ; '(nil nil nil)     ; lengthwise test
```

## Explanation

This library has no user-level interface; it is only useful
for programming in Emacs Lisp.  Three functions are provided:

	truthy
	truthy-s
	truthy-l

Truthy provides an alternative measure of the "truthiness" of a
value.  Whereas Lisp considers any non-nil value to be "true" when
evaluating a Boolean condition, `truthy` considers a value to be
"truthy" if it has *content*.  If the value is a string or buffer,
it must have non-zero length.  If a number, it must be non-zero.
If a hash, it must have keys.  If a window, it must be live.  See
the docstring to `truthy` for more details.

`truthy` always returns its argument on success.

`truthy-s` is the shallow version of `truthy`.  It does not recurse
into sequences, but returns success if any element of a sequence is
non-nil.

`truthy-l` is the "lengthwise" version of `truthy`.  It does not
recurse into sequences, but returns success if the argument has
length, considering only the variable portion of a data type.

To use truthy, place the `truthy.el` library somewhere Emacs can find
it, and add the following to your `~/.emacs` file:

```elisp
(require 'truthy)
```

## Compatibility and Requirements

	GNU Emacs version 24.4-devel     : yes, at the time of writing
	GNU Emacs version 24.3           : yes
	GNU Emacs version 23.3           : yes
	GNU Emacs version 22.3 and lower : no

Uses if present: [list-utils.el](http://github.com/rolandwalker/list-utils)
