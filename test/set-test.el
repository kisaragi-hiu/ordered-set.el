;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'set)
(require 'ert)

(ert-deftest set-clear/add/delete/has/length/elt ()
  (let ((set (set-new)))
    (set-add set "hello")
    (set-add set "hello")
    (set-add set "world")
    (should (equal 2 (seq-length set)))
    (should (equal "world" (seq-elt set 0)))
    (should (set-has set "world"))
    (should-not (set-has set 1234))
    (set-delete set "world")
    (should (equal 1 (seq-length set)))
    (should-not (set-has set "world"))
    (set-clear set)
    (should (equal 0 (seq-length set)))))

(ert-deftest set-intersection ()
  (should (equal
           (set-to-list (set-intersection
                         (set-new '(a b))
                         (set-new '(b c))))
           '(b))))
(ert-deftest set-union ()
  (should (equal
           (set-to-list (set-union
                         (set-new '(a b))
                         (set-new '(b c))))
           '(a b c))))
(ert-deftest set-difference ()
  (should (equal
           (set-to-list (set-difference
                         (set-new '(a b))
                         (set-new '(b c d))))
           '(a))))
(ert-deftest set-symmetric-difference ()
  (should (equal
           (set-to-list (set-symmetric-difference '(a b) '(b c d)))
           '(a c d))))
(ert-deftest set-subset-p ()
  (should (set-subset-p '() '(a b c)))
  (should (set-subset-p '(a) '(a b c)))
  (should-not (set-subset-p '(a b c d) '(a b c))))
(ert-deftest set-superset-p ()
  (should (set-superset-p '(a b c) '()))
  (should (set-superset-p '(a b c) '(b)))
  (should (set-superset-p '(a b c d) '(a b c)))
  (should-not (set-superset-p '(a b c d) '(a b c e))))
(ert-deftest set-disjoint-p ()
  (should (set-disjoint-p '(a b c) '()))
  (should-not (set-disjoint-p '(a b c) '(b))))

(ert-deftest set:seq-concatenate ()
  (should
   (equal
    (seq-into (seq-concatenate 'set "abc" "def" "cgi" '("s" "t" "ralkj"))
              'list)
    '(97 98 100 101 102 99 103 105 "s" "t" "ralkj"))))
(ert-deftest set:seq-reverse ()
  (let ((set (set-new '(1 2 3 4 5 9))))
    ;; Can sort
    (should (equal (set-to-list (seq-reverse set))
                   '(9 5 4 3 2 1)))
    ;; Does not mutate original
    (should (equal (set-to-list set)
                   '(1 2 3 4 5 9)))))
(ert-deftest set:seq-sort ()
  (let ((set (set-new '("a" "b" "c" "e" "d"))))
    ;; Can sort
    (should (equal (set-to-list (seq-sort #'string< set))
                   '("a" "b" "c" "d" "e")))
    ;; Does not mutate original
    (should (equal (set-to-list set)
                   '("a" "b" "c" "e" "d")))))
(ert-deftest set:seq-uniq ()
  (should (equal
           (seq-uniq (set-new '("a" "a" "b" "c")))
           '("a" "b" "c")))
  (should (equal
           (seq-uniq (set-new '("a" "a" "b" "c"))
                     (lambda (a b)
                       (and (< 0 (elt a 0) 100)
                            (< 0 (elt b 0) 100))))
           '("a"))))
(ert-deftest set:seq-elt ()
  (let ((set (set-new '(1 2 3 4 5))))
    (should (equal 1 (seq-elt set 0)))
    (should-error
     (setf (seq-elt set 0)
           "a")
     :type 'cl-no-applicable-method)))
