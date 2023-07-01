## 0.0.2

- Change order so that `(seq-elt set 0)` gives the earliest inserted element
- Fix the `seq-concatenate` implementation keeping the last instance of an element rather than the first
- Require cl-lib to avoid a `cl-defstruct` not defined error that sometimes pops up

## 0.0.1

- Initial release.
