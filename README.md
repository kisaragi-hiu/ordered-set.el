# set.el

Insertion-order once-only collections.

## Functions

…

- `set-new`
- `set-from-list`
- `(seq-into SEQUENCE 'set)`
- `(seq-length SET)`

- `set-add (set value)`
- `set-delete (set value)`
- `set-has (set value)`
- `set-clear (set)`

## Examples

Do stuff:

``` emacs-lisp
(do-stuff)
```

## Why?

In Emacs Lisp:

- If you want fast (constant) lookup time, you use a hash table
- If you want to preserve order, you use a list

But sometimes you want a collection that has both constant lookup time and the ability to preserve element order. This library implements that.

This is more or less a copy of ECMAScript's Set.

## Installation

## Changelog

## Acknowledgements

- ht.el - for the project structure
- seq.el - this library implements the seq.el interface
- ECMAScript - this library is strongly inspired by JavaScript Sets.
