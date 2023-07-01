## 0.0.3

- The `seq-concatenate` and `seq-into` implementations actually work on Emacs 25 ~ 27 now.

  Previously I was relying on this change in Emacs 28:

  > \*\*\* 'form' in '(eql form)' specializers in 'cl-defmethod' is now evaluated.
  > This corresponds to the behavior of defmethod in Common Lisp Object System.
  > For compatibility, '(eql SYMBOL)' does not evaluate SYMBOL, for now.

## 0.0.2

- Change order so that `(seq-elt set 0)` gives the earliest inserted element
- Fix the `seq-concatenate` implementation keeping the last instance of an element rather than the first
- Require cl-lib to avoid a `cl-defstruct` not defined error that sometimes pops up

## 0.0.1

- Initial release.
