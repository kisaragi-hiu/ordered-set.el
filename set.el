;;; set.el --- Insertion-order once-only collections  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Kisaragi Hiu

;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Version: 0.0.2
;; Keywords: extensions sequences collection set
;; Package-Requires: ((emacs "25.3") (seq "2.23"))
;; URL: https://github.com/kisaragi-hiu/set.el

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

;; Insertion-order once-only collections.

;;; Code:

(require 'seq)
(require 'cl-lib)

;;; Constructor

(cl-defstruct (set (:copier nil)
                   (:constructor set--new))
  -ht -lst)

(defun set-new (&optional seq)
  "Create and return a new set.

If SEQ is non-nil, create a set that has the same elements as
SEQ, except that duplicate elements won't be included twice.

The order is kept, so (seq-elt SEQ 0) equals
\(seq-elt (set-new SEQ) 0)."
  (let ((new (set--new :-ht (make-hash-table :test #'equal)
                       :-lst nil)))
    (when seq
      (seq-doseq (value seq)
        (set-add new value)))
    new))

;;; Main methods
(defun set-add (set value)
  "Add VALUE to SET.
The first added element comes first."
  ;; Like `-distinct', each Set value is represented as a key.
  (unless (set-has set value)
    (puthash value t (set--ht set))
    (setf (set--lst set)
          (nconc (set--lst set) (list value))))
  set)

(defun set-delete (set value)
  "Remove VALUE from SET such that (set-has SET VALUE) will return nil.
Return nil if VALUE wasn't in SET in the first place."
  (when (set-has set value)
    (remhash value (set--ht set))
    ;; This assumes the hash table's equality is `equal'.
    (setf (set--lst set)
          (remove value (set--lst set)))
    ;; Return true to indicate removal was successful
    t))

;; Using define-inline made this go from ~1.5x slower than gethash to ~1.1x
(define-inline set-has (set value)
  "Test if VALUE is in SET."
  (inline-quote (gethash ,value (set--ht ,set))))

(defun set-clear (set)
  "Remove all elements from SET.
Return nil."
  (setf (set--ht set)
        (make-hash-table :test #'equal))
  (setf (set--lst set)
        nil))

;;; tc39/proposal-set-methods

;; Some of these are wrappers over seq's functions, the only difference being
;; that they convert the result back into a set.

;; These functions accept any sequence, but they return sets if applicable.

(defun set-intersection (seq1 seq2)
  "Return a new set that is the intersection of SEQ1 and SEQ2.
\(A B) ∩ (B C) -> (B)"
  (set-new (seq-intersection seq1 seq2)))
(defun set-union (seq1 seq2)
  "Return a new set that is the union of SEQ1 and SEQ2.
\(A B) ∪ (B C) -> (A B C)"
  (set-new (seq-union seq1 seq2)))
(defun set-difference (seq1 seq2)
  "Return a new set that is the \"difference\" of SEQ1 and SEQ2.

This is like taking SEQ1 then removing each of SEQ2 from it.

\(A B) ∖ (B C D) -> (A)

Elements that are only in SEQ2 are not included. For that kind of
difference, see `set-symmetric-difference'."
  (set-new (seq-difference seq1 seq2)))
(defun set-symmetric-difference (seq1 seq2)
  "Return a new set that is the symmetric difference of SEQ1 and SEQ2.

The new set contains elements that are not shared between SEQ1 and SEQ2.

\(A B) ⊖ (B C D) -> (A C D)

See also `set-difference'."
  (set-difference (seq-union seq1 seq2)
                  (seq-intersection seq1 seq2)))
(defun set-subset-p (sub super)
  "Return t if SUB is a subset of SUPER.
This means every element of SUB is present in SUPER."
  ;; This promises to return a list, so we can test "is it nil"
  ;; instead of "is its length = 0"
  (not (seq-difference sub super)))
(defun set-superset-p (super sub)
  "Return t if SUPER is a superset of SUB.
This means every element of SUB is present in SUPER."
  (set-subset-p sub super))
(defun set-disjoint-p (seq1 seq2)
  "Return t if SEQ1 is disjoint from SEQ2.
This means they do not intersect."
  ;; This promises to return a list, so we can test "is it nil"
  ;; instead of "is its length = 0"
  (not (seq-intersection seq1 seq2)))

;;; Conversion

(defun set-to-list (set)
  "Convert SET to a list."
  (set--lst set))

(cl-defmethod seq-into (sequence (_type (eql 'set)))
  "Convert SEQUENCE into a set."
  (set-new sequence))

(cl-defmethod seq-into ((set set) type)
  "Convert SET into a sequence of TYPE."
  (pcase type
    (`list (set-to-list set))
    (_ (seq-into (set-to-list set) type))))

;;; seq.el integration
(cl-defmethod seqp ((_set set))
  "Sets are sequences."
  t)

;; I don't understand what the point of `seq-into-sequence' is.
(cl-defmethod seq-into-sequence ((set set))
  "Return SET, thereby treating sets as sequences."
  set)

(cl-defmethod seq-elt ((set set) n)
  "Return Nth element of SET."
  ;; We may have to do some questionable stuff with N, like reversing it.
  (elt (set--lst set) n))

(cl-defmethod seq-length ((set set))
  "Return length of SET."
  (hash-table-count (set--ht set)))

(cl-defmethod seq-do (function (set set))
  "Apply FUNCTION to each element of SET, presumably for side effects.
Return SET."
  (mapc function (set--lst set)))

(cl-defmethod seq-copy ((set set))
  "Return a shallow copy of SET."
  (set--new :-ht (copy-hash-table (set--ht set))
            :-lst (copy-sequence (set--lst set))))

(cl-defmethod seq-subseq ((set set) start &optional end)
  "Return a new set containing elements of SET from START to END."
  (set-new
   (seq-subseq (set--lst set) start end)))

(cl-defmethod seq-map (function (set set))
  "Run FUNCTION on each element of SET and collect them in a list.
Possibly faster implementation utilizing mapcar."
  (mapcar function (set--lst set)))

(cl-defmethod seq-sort (pred (set set))
  "Sort SET using PRED as the comparison function.
The original set is not modified.
Returns a new set."
  (let ((new (seq-copy set)))
    (setf (set--lst new)
          ;; Already copied, no need to copy again
          (sort (set--lst new) pred))
    new))

(cl-defmethod seq-reverse ((set set))
  "Return a new set which is a reversed version of SET."
  (let ((new (seq-copy set)))
    (setf (set--lst new)
          ;; Already copied
          (nreverse (set--lst new)))
    new))

(cl-defmethod seq-concatenate ((_type (eql 'set))
                               &rest sequences)
  "Concatenate SEQUENCES into a single set."
  (seq-into (apply #'seq-concatenate 'list sequences)
            'set))

(cl-defmethod seq-uniq ((set set) &optional testfn)
  "Return a list of unique elements in SET, which just means every element.
That only applies when TESTFN is nil."
  (if testfn
      (seq-uniq (set--lst set) testfn)
    (set--lst set)))

(cl-defmethod seq-contains-p ((set set) elt &optional testfn)
  "Return non-nil if SET contains an element equal to ELT.
Equality is defined by TESTFN if non-nil or by SET's equality function if nil."
  (if testfn
      (seq-contains-p (set--lst set) elt testfn)
    (set-has set elt)))

(provide 'set)
;;; set.el ends here
