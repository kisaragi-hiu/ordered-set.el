;;; ordered-set.el --- Insertion-order sets  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kisaragi Hiu

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Version: 0.2.0
;; Keywords: extensions sequences collection set
;; Package-Requires: ((emacs "25.3") (seq "2.23"))
;; URL: https://github.com/kisaragi-hiu/ordered-set.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Sets are collections that does not permit duplicate objects, subject to some
;; possibly arbitrary definition of "duplicate". This library currently always
;; uses `equal'.
;;
;; Using lists as sets has performance issues, as list lookup is O(n). Hash
;; tables provide O(1) lookup, but they don't preserve insertion order.
;;
;; This library attempts to provide the best of both worlds, at the cost of
;; memory, by storing both a hash table and a list.

;;; Code:

(require 'seq)
(require 'cl-lib)

;;; Constructor

(cl-defstruct (ordered-set (:copier nil)
                           (:constructor ordered-set--new))
  "A once-only collection type that preserves insertion order."
  (ht nil :documentation
      "The underlying hash table of the set.

Mutating the hash table without making a copy will break the
set object.")
  (lst nil :documentation "The underlying list of the set.

Mutating the list without making a copy will break the
set object."))

(defun ordered-set-create (&optional seq)
  "Create and return a new set.

If SEQ is non-nil, create a set that has the same elements as
SEQ, except that duplicate elements won't be included twice.

The order is kept, so (seq-elt SEQ 0) equals
\(seq-elt (set-create SEQ) 0)."
  (let ((new (ordered-set--new :ht (make-hash-table :test #'equal)
                               :lst nil)))
    (when seq
      (seq-doseq (value seq)
        (ordered-set-add new value)))
    new))

;;; Main methods
(defun ordered-set-add (set value)
  "Add VALUE to SET.
The first added element comes first."
  ;; Like `-distinct', each Set value is represented as a key.
  (unless (ordered-set-has set value)
    (puthash value t (ordered-set-ht set))
    (setf (ordered-set-lst set)
          (nconc (ordered-set-lst set) (list value))))
  set)

(defalias 'ordered-set-push 'ordered-set-add)

(defun ordered-set-delete (set value)
  "Remove VALUE from SET such that (set-has SET VALUE) will return nil.
Return nil if VALUE wasn't in SET in the first place."
  (when (ordered-set-has set value)
    (remhash value (ordered-set-ht set))
    ;; This assumes the hash table's equality is `equal'.
    (setf (ordered-set-lst set)
          (remove value (ordered-set-lst set)))
    ;; Return true to indicate removal was successful
    t))

;; Using define-inline made this go from ~1.5x slower than gethash to ~1.1x
(define-inline ordered-set-has (set value)
  "Test if VALUE is in SET."
  (inline-quote (gethash ,value (ordered-set-ht ,set))))

(defun ordered-set-clear (set)
  "Remove all elements from SET.
Return nil."
  (setf (ordered-set-ht set)
        (make-hash-table :test #'equal))
  (setf (ordered-set-lst set)
        nil))

;;; tc39/proposal-set-methods

;; Some of these are wrappers over seq's functions, the only difference being
;; that they convert the result back into a set.

;; These functions accept any sequence, but they return sets if applicable.

(defun ordered-set-intersection (seq1 seq2)
  "Return a new set that is the intersection of SEQ1 and SEQ2.
\(A B ∩ (B C) -> (B)"
  (ordered-set-create (seq-intersection seq1 seq2)))
(defun ordered-set-union (seq1 seq2)
  "Return a new set that is the union of SEQ1 and SEQ2.
\(A B ∪ (B C) -> (A B C)"
  (ordered-set-create (seq-union seq1 seq2)))
(defun ordered-set-difference (seq1 seq2)
  "Return a new set that is the \"difference\" of SEQ1 and SEQ2.

This is like taking SEQ1 then removing each of SEQ2 from it.

\(A B ∖ (B C D) -> (A)

Elements that are only in SEQ2 are not included. For that kind of
difference, see `set-symmetric-difference'."
  (ordered-set-create (seq-difference seq1 seq2)))
(defun ordered-set-symmetric-difference (seq1 seq2)
  "Return a new set that is the symmetric difference of SEQ1 and SEQ2.

The new set contains elements that are not shared between SEQ1 and SEQ2.

\(A B) ⊖ (B C D) -> (A C D)

See also `set-difference'."
  (ordered-set-difference (seq-union seq1 seq2)
                          (seq-intersection seq1 seq2)))
(defun ordered-set-subset-p (sub super)
  "Return t if SUB is a subset of SUPER.
This means every element of SUB is present in SUPER."
  ;; This promises to return a list, so we can test "is it nil"
  ;; instead of "is its length = 0"
  (not (seq-difference sub super)))
(defun ordered-set-superset-p (super sub)
  "Return t if SUPER is a superset of SUB.
This means every element of SUB is present in SUPER."
  (ordered-set-subset-p sub super))
(defun ordered-set-disjoint-p (seq1 seq2)
  "Return t if SEQ1 is disjoint from SEQ2.
This means they do not intersect."
  ;; This promises to return a list, so we can test "is it nil"
  ;; instead of "is its length = 0"
  (not (seq-intersection seq1 seq2)))

;;; Conversion

(cl-defmethod seq-into (sequence (_type (eql ordered-set)))
  "Convert SEQUENCE into a set."
  (ordered-set-create sequence))

(cl-defmethod seq-into ((set ordered-set) type)
  "Convert SET into a sequence of TYPE."
  (pcase type
    (`list (ordered-set-lst set))
    (_ (seq-into (ordered-set-lst set) type))))

;;; seq.el integration
(cl-defmethod seqp ((_set ordered-set))
  "Sets are sequences."
  t)

;; I don't understand what the point of `seq-into-sequence' is.
(cl-defmethod seq-into-sequence ((set ordered-set))
  "Return SET, thereby treating sets as sequences."
  set)

(cl-defmethod seq-elt ((set ordered-set) n)
  "Return Nth element of SET."
  ;; We may have to do some questionable stuff with N, like reversing it.
  (elt (ordered-set-lst set) n))

(cl-defmethod seq-length ((set ordered-set))
  "Return length of SET."
  (hash-table-count (ordered-set-ht set)))

(cl-defmethod seq-do (function (set ordered-set))
  "Apply FUNCTION to each element of SET, presumably for side effects.
Return SET."
  (mapc function (ordered-set-lst set)))

(cl-defmethod seq-copy ((set ordered-set))
  "Return a shallow copy of SET."
  (ordered-set--new :ht (copy-hash-table (ordered-set-ht set))
                    :lst (copy-sequence (ordered-set-lst set))))

(cl-defmethod seq-subseq ((set ordered-set) start &optional end)
  "Return a new set containing elements of SET from START to END."
  (ordered-set-create
   (seq-subseq (ordered-set-lst set) start end)))

(cl-defmethod seq-map (function (set ordered-set))
  "Run FUNCTION on each element of SET and collect them in a list.
Possibly faster implementation utilizing mapcar."
  (mapcar function (ordered-set-lst set)))

(cl-defmethod seq-sort (pred (set ordered-set))
  "Sort SET using PRED as the comparison function.
The original set is not modified.
Returns a new set."
  (let ((new (seq-copy set)))
    (setf (ordered-set-lst new)
          ;; Already copied, no need to copy again
          (sort (ordered-set-lst new) pred))
    new))

(cl-defmethod seq-reverse ((set ordered-set))
  "Return a new set which is a reversed version of SET."
  (let ((new (seq-copy set)))
    (setf (ordered-set-lst new)
          ;; Already copied
          (nreverse (ordered-set-lst new)))
    new))

(cl-defmethod seq-concatenate ((_type (eql ordered-set))
                               &rest sequences)
  "Concatenate SEQUENCES into a single set."
  (seq-into (apply #'seq-concatenate 'list sequences)
            'ordered-set))

(cl-defmethod seq-uniq ((set ordered-set) &optional testfn)
  "Return a list of unique elements in SET, which just means every element.
That only applies when TESTFN is nil."
  (if testfn
      (seq-uniq (ordered-set-lst set) testfn)
    (ordered-set-lst set)))

(cl-defmethod seq-contains-p ((set ordered-set) elt &optional testfn)
  "Return non-nil if SET contains an element equal to ELT.
Equality is defined by TESTFN if non-nil or by SET's equality function if nil."
  (if testfn
      (seq-contains-p (ordered-set-lst set) elt testfn)
    (ordered-set-has set elt)))

;; We need to catch this case and raise an error ourselves so that it is still
;; an error in Emacs 25.
;; Emacs 25 structs are vectors, without this (setf (seq-elt set ...) ...) would
;; set the indexed elements in the vector.
(define-error 'ordered-set-invalid-operation "Invalid operation")
(cl-defmethod (setf seq-elt) (_store (_sequence ordered-set) _n)
  (signal 'ordered-set-invalid-operation
          (list (format-message "Cannot set indexed members of an ordered set"))))

(provide 'ordered-set)
;;; ordered-set.el ends here
