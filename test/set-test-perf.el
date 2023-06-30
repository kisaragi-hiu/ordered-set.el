;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'set)
(require 'ert)

(defun no-gc (result)
  "Return time without GC from benchmark-run RESULT."
  (- (elt result 0)
     (elt result 2)))

;; Don't warn me about stuff while a unit test is running
(when noninteractive
  (setq warning-minimum-log-level :emergency))

(defvar set-test:lst (seq-uniq (cl-loop repeat 10000 collect (random 10000))))
(defvar set-test:set (set-new set-test:lst))
(defvar set-test:hash (set--ht set-test:set))

(ert-deftest set:set-has:overhead ()
  (let ((l (benchmark-run-compiled 10
             (dotimes (i 10000)
               (memq i set-test:lst))))
        (h (benchmark-run-compiled 10
             (dotimes (i 10000)
               (gethash i set-test:hash))))
        (s (benchmark-run-compiled 10
             (dotimes (i 10000)
               (set-has set-test:set i)))))
    (let ((overhead (/ (no-gc s) (no-gc h))))
      (message "   `set-has' is %.2fx slower than `gethash'" overhead)
      (message "   `set-has' is %.2fx faster than `memq'" (/ (no-gc l) (no-gc s)))
      (message "   `gethash' is %.2fx faster than `memq'" (/ (no-gc l) (no-gc h)))
      ;; set-has should not be more than 2x slower than gethash
      (should (< overhead 2)))))
