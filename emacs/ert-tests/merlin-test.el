;;; merlin-test --- Tests for the Emacs Merlin mode.
;;; Commentary:
;;; Code:

(require 'merlin)
(setq merlin-command (concat default-directory "../../_build/src/ocamlmerlin.native"))

(defun merlin-test-with-temp-ml-file (f)
  "Run F with the current buffer attached to a temporary .ml file.
After F returns, the buffer is killed and the temporary file is
deleted."
  (lexical-let* ((temp-file (make-temp-file "merlin-test-" nil ".ml")))
    (unwind-protect
        (save-excursion
          (find-file-literally temp-file)
          (unwind-protect
            (funcall f)
            (merlin-capture-message 'kill-buffer nil)))
      (delete-file temp-file))))

(defun merlin-test-error-on-current-line ()
  "Return the Merlin error on the current line.
Return nil if there is no error."
  (lexical-let* ((current-line (line-number-at-pos))
                 (err (merlin-find-error-for-line
                       (line-number-at-pos) merlin-pending-errors)))
    (if err (merlin-chomp (cdr (assoc 'message err))))))

(defun merlin-test-goto-line (n)
  "Go to line number N.
Implemented as advised in the help for `goto-line'."
  (goto-char (point-min)) (forward-line (1- n)))

(defun merlin-test-maybe-make-list (x)
  (if (listp x) x (list x)))

(defun merlin-test-check-errors (errors)
  "Test that the current Merlin errors match ERRORS.
ERRORS should be a possibly empty association list mapping line
numbers to error messages."
  (dotimes (line-number-pred (line-number-at-pos (point-max)))
    (lexical-let*
        ((line-number (1+ line-number-pred))
         (err (assoc line-number errors)))
      (merlin-test-goto-line line-number)
      (merlin-capture-message
       'merlin-show-error-on-current-line
       (lambda (msg)
         (should (equal msg (if err (merlin-test-maybe-make-list (cadr err)) nil))))))))

(defun merlin-capture-message (f g)
  "Call F capturing its output, and then call G on the captured output.
The captured output consists in calls to `message', which are
intercepted are redirected to a buffer."
  (lexical-let* ((buffer nil) (orig-message (function message)))
    (flet ((message (fmt &rest args)
                     (setq buffer (append buffer (list (apply 'format (cons fmt args)))))))
      (funcall f))
    (if g (funcall g buffer))))

(defun merlin-test-errors (file-contents errors &optional num-restarts)
  "Test that the Merlin errors for FILE-CONTENTS match ERRORS.
ERRORS is the same format as for `merlin-test-check-errors'.
NUM-RESTARTS is an optional number of times to restart Merlin and
do the check on errors.  temporary file ending in .ml is created
with contents FILE-CONTENTS.  It is automatically removed after
the test."
  (merlin-test-with-temp-ml-file
   (lambda ()
     (merlin-mode t)
     (should (equal merlin-mode t))
     (insert file-contents)
     (dotimes (i (or num-restarts 1))
       (merlin-capture-message 'merlin-to-end nil)
       (merlin-test-check-errors errors)
       (merlin-capture-message 'merlin-restart-process nil)))))

;; idle callbacks are not called in batch mode
;; (ert-deftest idle ()
;;   "Idle callback test."
;;   (run-with-idle-timer 0 nil (lambda () (message "bouh")))
;;   (merlin-capture-message
;;    (lambda ()
;;      (run-with-idle-timer 0.1 nil
;;                           (lambda () (message "coucou")))
;;      (sit-for 0.2))
;;    (lambda (captured) (should (equal captured "coucou")))))

(ert-deftest simple-error ()
  "Simple type error."
  (merlin-test-errors
   "let a = 1. + 2
let b = 42"
   '((1 "This expression has type float but an expression was expected of type int"))))

(ert-deftest simple-error-stress-restart ()
  "Simple type error, restarting Merlin many times and checking
each time."
  (merlin-test-errors
   "let a = 1. + 2"
   '((1 "This expression has type float but an expression was expected of type int"))
   10))

(ert-deftest no-error ()
  "Simple file with no error."
  (merlin-test-errors "let a = 1 + 2" nil))

(ert-deftest empty-no-error ()
  "Line with one empty line."
  (merlin-test-errors "\n" nil))

;;; merlin-test.el ends here

