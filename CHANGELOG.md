# Changelog

## 0.2.0

- Rename to ordered-set.el because set.el is a bit too ambitious, even though it surprisingly isn't taken.

## 0.1.0

- Setting elements with an index via eg. `(setf (seq-elt set 0) "a")` now signals an error of type `set-invalid-operation`. Previously it signaled `cl-no-applicable-method` on Emacs 26+, and ended up actually breaking the struct in Emacs 25 (because Emacs 25 structs are vectors).
- Add `set-push` as an alias of `set-add`.
- The underlying fields of sets are now not named like they are private.
- `set-to-list` has been removed in favor of using `set-lst` to access the underlying list direcly.
- `set-new` has been renamed to `set-create`, similar to `popup-create` and `ht-create`.

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
