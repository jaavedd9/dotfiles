;; for straight package
(setq native-comp-deferred-compilation-deny-list '("powerline"))

;; to avoid cl package deprecated warning
;; https://github.com/kiwanami/emacs-epc/issues/35#issuecomment-773420321
(setq byte-compile-warnings '(cl-functions))


;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. straight.el handles package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
(advice-add 'package--ensure-init-file :override 'ignore)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(defmacro fk/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s" (float-time (time-since time)))))



(defun fk/time-since-startup (&optional prefix)
  "Display the time that past since emacs startup. Add PREFIX if given at the
start of message for debug purposes."
  (interactive)
  (let* ((prefix (or prefix ""))
         (time (float-time (time-since before-init-time)))
         (str (format "%s%s seconds" prefix time)))
    (if (or (not (string-empty-p prefix))
            (called-interactively-p 'interactive))
        (message str)
      str)))

(defvar fk/time-last-check nil)
(defvar fk/time-threshold 0)
(setq fk/time-threshold 0.02)

(defun fk/time-since-last-check (&optional prefix)
  "Display the time that past since last check. Add PREFIX if given at the
start of message for debug purposes."
  (interactive)
  (let* ((prefix (or prefix ""))
         (time (float-time (time-since (or fk/time-last-check before-init-time))))
         (str (format "%s%s seconds" prefix time)))
    (setq fk/time-last-check (current-time))
    (if (or (not (string-empty-p prefix))
            (called-interactively-p 'interactive))
        (when (> time fk/time-threshold) (message "%s" str))
      str)))



;; temp fix for emacs 28 version problem
;; from https://github.com/hlissner/doom-emacs/issues/4636

(defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.
This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:
  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"27.1\")
This macro uses `defvaralias' and `make-obsolete-variable' (which see).
See the Info node `(elisp)Variable Aliases' for more details.
If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
where OBSOLETE-NAME may be set, e.g. in an init file, before the
alias is defined), then the define-obsolete-variable-alias
statement should be evaluated before the defcustom, if user
customizations are to be respected.  The simplest way to achieve
this is to place the alias statement before the defcustom (this
is not necessary for aliases that are autoloaded, or in files
dumped with Emacs).  This is so that any user customizations are
applied before the defcustom tries to initialize the
variable (this is due to the way `defvaralias' works).
For the benefit of Customize, if OBSOLETE-NAME has
any of the following properties, they are copied to
CURRENT-NAME, if it does not already have them:
`saved-value', `saved-variable-comment'."
  (declare (doc-string 4)
           (advertised-calling-convention
            (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     (dolist (prop '(saved-value saved-variable-comment))
       (and (get ,obsolete-name prop)
            (null (get ,current-name prop))
            (put ,current-name prop (get ,obsolete-name prop))))
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(defmacro define-obsolete-face-alias (obsolete-face current-face &optional when)
  "Make OBSOLETE-FACE a face alias for CURRENT-FACE and mark it obsolete.
WHEN should be a string indicating when the face was first made
obsolete, for example a date or a release number."
  `(progn (put ,obsolete-face 'face-alias ,current-face)
          (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))

(defmacro define-obsolete-function-alias (obsolete-name current-name &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.
\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")
is equivalent to the following two lines of code:
\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")
WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.
See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4))
  `(progn (defalias ,obsolete-name ,current-name ,docstring)
          (make-obsolete ,obsolete-name ,current-name ,when)))

;; (defadvice! doom--fix-wrong-number-of-args-during-byte-compile (recipe)
;;   :override #'straight--build-compile
;;   (let* ((package (plist-get recipe :package))
;;          (dir (straight--build-dir package))
;;          (program (concat invocation-directory invocation-name))
;;          (args
;;           `("-Q" "-L" ,dir
;;             ,@(apply #'append
;;                      (mapcar (lambda (d)
;;                                (let ((d (straight--build-dir d)))
;;                                  (when (file-exists-p d) (list "-L" d))))
;;                              (straight--get-dependencies package)))
;;             "--batch"
;;             "--eval"
;;             ,(prin1-to-string
;;               '(progn
;;                  (defmacro define-obsolete-face-alias (obsolete-face current-face &optional when)
;;                    `(progn (put ,obsolete-face 'face-alias ,current-face)
;;                            (put ,obsolete-face 'obsolete-face (or (purecopy ,when) t))))
;;                  (defmacro define-obsolete-function-alias (obsolete-name current-name &optional when docstring)
;;                    `(progn (defalias ,obsolete-name ,current-name ,docstring)
;;                            (make-obsolete ,obsolete-name ,current-name ,when)))
;;                  (defmacro define-obsolete-variable-alias (obsolete-name current-name &optional when docstring)
;;                    `(progn (defvaralias ,obsolete-name ,current-name ,docstring)
;;                            (dolist (prop '(saved-value saved-variable-comment))
;;                              (and (get ,obsolete-name prop)
;;                                   (null (get ,current-name prop))
;;                                   (put ,current-name prop (get ,obsolete-name prop))))
;;                            (make-obsolete-variable ,obsolete-name ,current-name ,when)))))
;;             "--eval"
;;             ,(format "(byte-recompile-directory %S 0 'force)" dir))))
;;     (when straight-byte-compilation-buffer
;;       (with-current-buffer (get-buffer-create straight-byte-compilation-buffer)
;;         (insert "\n$ " (replace-regexp-in-string
;;                         "\\(-L [^z-a]*? \\)"
;;                         "\\1\\\\ \n  "
;;                         (string-join `(,program ,@args) " "))
;;                 "\n")))
;;     (apply #'call-process program nil straight-byte-compilation-buffer nil args)))


