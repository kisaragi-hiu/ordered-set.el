;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'set)
(require 'ert)

(defvar set-test:lst (seq-uniq (cl-loop repeat 10000 collect (random 10000))))
(defvar set-test:set (set-new set-test:lst))
(defvar set-test:hash (set--ht set-test:set))
(ert-deftest set:performance:set-has:control:list ()
  (dotimes (i 100000)
    (memq i set-test:lst)))
(ert-deftest set:performance:set-has:control:ht ()
  (dotimes (i 100000)
    (gethash i set-test:hash)))
(ert-deftest set:performance:set-has ()
  (dotimes (i 100000)
    (set-has set-test:set i)))
