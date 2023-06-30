# set.el

Insertion-order once-only collections.

## Why?

In Emacs Lisp:

- If you want fast (constant) lookup time, you use a hash table
- If you want to preserve order, you use a list

But sometimes you want a collection that has both constant lookup time and the ability to preserve element order. This library implements that.

## Specifications

- Sets are implemented as structs called `set` with two slots: a hash table, and a list.
- The hash table is used for testing membership. The list is used to preserve order.
- The hash table and the list are maintained while adding or deleting elements.
- Membership equality uses `equal`. Allowing the hash table to accept other equality functions might be doable but would open up too much complexity.

## Functions

### Constructor

- `set-new (&optional seq)`

  Create a new set. If `seq` is provided, create one with the elements of `seq`.

### Conversion

- `set-to-list (set)`: Convert `set` to a list.
- `(seq-into set type)`: Convert `set` to any other type supported by `seq-into`.
- `(seq-into sequence 'set)`: Convert any sequence to a set.

### Main methods

- `set-add (set value)`: Add `value` to `set`. Return `set`.
- `set-delete (set value)`: Delete `value` from `set`. Return t if `value` was in `set`, nil otherwise.
- `set-has (set value)`: Return whether `value` is in `set`.
- `set-clear (set)`: Clear `set` so that it is empty. Implemented for parity with JavaScript's Set only.

### Set methods à la [tc39/proposal-set-methods](https://github.com/tc39/proposal-set-methods)

These functions accept any sequence, like `seq` functions. The difference between, say, `set-intersection` and `seq-intersection` is that `set-intersection` returns a set whereas `seq-intersection` returns a list.

- `set-intersection (seq1 seq2)`: (A B) ∩ (B C) → (B)
- `set-union (seq1 seq2)`: (A B) ∪ (B C) → (A B C)
- `set-difference (seq1 seq2)`: (A B) ∖ (B C D) → (A)

  Elements that are only in `seq2` are not included. That sort of “difference” is provided by `set-symmetric-difference`.

- `set-symmetric-difference (seq1 seq2)`: (A B) ⊖ (B C D) → (A C D)
- `set-subset-p (sub super)`: every element of `sub` is in `super`
- `set-superset-p (super sub)`: every element of `sub` is in `super`
- `set-disjoint-p (set1 set2)`: `seq1` and `seq2` do not intersect

### Implementation of the seq.el interface

seq.el methods just work.

- Iteration maps over the underlying list.
- Membership tests use the underlying hash table.
- When uniqueness `testfn` arguments are not given or are nil, the underlying list is used directly as the members' uniqueness had already been guaranteed when they were added.

Of particular note:

- `(setf (seq-elt set n) value)` is not implemented. There is no right answer to what modifying a set by index is supposed to mean, even if the set is order-aware.
- `seq-into-sequence` just does nothing, as is also the case for builtin seq types.

Here are the function signatures:

- `(seq-into set type)`: Convert `set` to any other type supported by `seq-into`.
- `(seq-into sequence 'set)`: Convert any sequence to a set.
- `(seqp set)`: Return t for sets.
- `(seq-into-sequence set)`: do nothing and return `set` like the generic implementation.
- `(seq-elt set n)`: Return `n`th element of `set`. Last added elements come first.
- `(seq-length set)`: Return the number of elements in `set`.
- `(seq-do function set)`: Apply `function` to each element of `set`, presumably for side effects. Return `set`.
- `(seq-copy set)`: Return a shallow copy of `set`.
- `(seq-subseq set start &optional end)`: Return a new set containing elements of `set` from `start` to `end`.
- `(seq-map function set)`: Run `function` on each element of `set` and collect them in a list. Possibly faster implementation utilizing mapcar.
- `(seq-sort pred set)`: Sort `set` using `pred` as the comparison function. The original set is not modified. Returns a new set.
- `(seq-reverse set)`: Return a new set which is a reversed version of `set`.
- `(seq-concatenate 'set &rest sequences)`: Concatenate `sequences` into a single set.
- `(seq-uniq set &optional testfn)`: Return a list of unique elements in `set`.

  This is very fast if `testfn` is nil, as sets already avoid adding duplicates.

- `(seq-contains-p set elt &optional testfn)`: Return non-nil if `set` contains an element equal to `elt`.

  This is very fast if `testfn` is nil, as the membership test utilizes the underlying hash table.

## Examples

Do stuff:

``` emacs-lisp
(do-stuff)
```

## Installation

## Changelog

[./CHANGELOG.md](./CHANGELOG.md)

## Acknowledgements

- ht.el - for the project structure
- seq.el - this library implements the seq.el interface
- ECMAScript - this library is strongly inspired by JavaScript Sets.
