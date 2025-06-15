;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'ordered-set)
(require 'ert)

(ert-deftest ordered-set-clear/add/delete/has/length/elt ()
  (let ((set (ordered-set-create)))
    (ordered-set-add set "hello")
    (ordered-set-add set "hello")
    (ordered-set-add set "world")
    (should (equal 2 (seq-length set)))
    (should (equal "hello" (seq-elt set 0)))
    (should (ordered-set-has set "world"))
    (should-not (ordered-set-has set 1234))
    (ordered-set-delete set "world")
    (should (equal 1 (seq-length set)))
    (should-not (ordered-set-has set "world"))
    (ordered-set-clear set)
    (should (equal 0 (seq-length set)))))

(ert-deftest ordered-set-intersection ()
  (should (equal
           (ordered-set-lst (ordered-set-intersection
                             (ordered-set-create '(a b))
                             (ordered-set-create '(b c))))
           '(b))))
(ert-deftest ordered-set-union ()
  (should (equal
           (ordered-set-lst (ordered-set-union
                             (ordered-set-create '(a b))
                             (ordered-set-create '(b c))))
           '(a b c))))
(ert-deftest ordered-set-difference ()
  (should (equal
           (ordered-set-lst (ordered-set-difference
                             (ordered-set-create '(a b))
                             (ordered-set-create '(b c d))))
           '(a))))
(ert-deftest ordered-set-symmetric-difference ()
  (should (equal
           (ordered-set-lst (ordered-set-symmetric-difference '(a b) '(b c d)))
           '(a c d))))
(ert-deftest ordered-set-subset-p ()
  (should (ordered-set-subset-p '() '(a b c)))
  (should (ordered-set-subset-p '(a) '(a b c)))
  (should-not (ordered-set-subset-p '(a b c d) '(a b c))))
(ert-deftest ordered-set-superset-p ()
  (should (ordered-set-superset-p '(a b c) '()))
  (should (ordered-set-superset-p '(a b c) '(b)))
  (should (ordered-set-superset-p '(a b c d) '(a b c)))
  (should-not (ordered-set-superset-p '(a b c d) '(a b c e))))
(ert-deftest ordered-set-disjoint-p ()
  (should (ordered-set-disjoint-p '(a b c) '()))
  (should-not (ordered-set-disjoint-p '(a b c) '(b))))

(ert-deftest ordered-set:seq-concatenate ()
  (should
   (equal
    (seq-into (seq-concatenate 'ordered-set "abc" "def" "cgi" '("s" "t" "ralkj"))
              'list)
    '(97 98 99 100 101 102 103 105 "s" "t" "ralkj"))))
(ert-deftest ordered-set:seq-reverse ()
  (let ((set (ordered-set-create '(1 2 3 4 5 9))))
    ;; Can sort
    (should (equal (ordered-set-lst (seq-reverse set))
                   '(9 5 4 3 2 1)))
    ;; Does not mutate original
    (should (equal (ordered-set-lst set)
                   '(1 2 3 4 5 9)))))
(ert-deftest ordered-set:seq-sort ()
  (let ((set (ordered-set-create '("a" "b" "c" "e" "d"))))
    ;; Can sort
    (should (equal (ordered-set-lst (seq-sort #'string< set))
                   '("a" "b" "c" "d" "e")))
    ;; Does not mutate original
    (should (equal (ordered-set-lst set)
                   '("a" "b" "c" "e" "d")))))
(ert-deftest ordered-set:seq-uniq ()
  (should (equal
           (seq-uniq (ordered-set-create '("a" "a" "b" "c")))
           '("a" "b" "c")))
  (should (equal
           (seq-uniq (ordered-set-create '("a" "a" "b" "c"))
                     (lambda (a b)
                       (and (< 0 (elt a 0) 100)
                            (< 0 (elt b 0) 100))))
           '("a"))))
(ert-deftest ordered-set:seq-elt ()
  (let ((set (ordered-set-create '(1 2 3 4 5))))
    (should (equal 1 (seq-elt set 0)))
    (should-error
     (setf (seq-elt set 0) "a")
     :type 'ordered-set-invalid-operation)))
