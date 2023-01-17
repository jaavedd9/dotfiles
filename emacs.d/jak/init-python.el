; elpy mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;elpy;;;;;;;;;;;;;;;;;;;;;
(setq python-shell-interpreter "python3")
;; to deal with "Searching for program: No such file or directory"
;; solution: https://github.com/jorgenschaefer/pyvenv/issues/74
;; (set-variable 'shell-command-switch "-ic")
;; (setenv "BASH_ENV" "~/.bashrc")
(setq pyvenv-virtualenvwrapper-python "python3")
;;(setq shell-file-name "zsh")
;;(setq shell-command-switch "-ic")
;;(setq shell-command-switch "-i")

;; (use-package elpy
;;   :ensure t
;;   :bind (
;;          ;;("M-." . elpy-goto-definition)
;;          ;; ("M-." . dumb-jump-go)
;;          ;; ("M-," . xref-pop-marker-stack)
;;          (:map elpy-refactor-map
;;                ("f" . elpy-yapf-fix-code)
;;                ;; ("f" . elpy-black-fix-code)
;;                )
;;          )
;;   :init
;;   ;; Since ELPY is not a simple mode, but a collection of smaller modes stitched
;;   ;; together, we have to call with-eval-after-load
;;   (with-eval-after-load 'python (elpy-enable))
;;   ;; requires `pip3 install jedi`
;;   (setq elpy-rpc-backend "jedi")
;;   (add-hook 'python-mode-hook #'flycheck-mode)
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               ;; (setq indent-tabs-mode t)
;;               (setq indent-tabs-mode nil)
;;               (setq tab-width 4)
;;               (setq python-indent-offset 4)
;;               ;;              (setq elpy-rpc-timeout 10)
;; 	          )
;;             )


;;   ;; Make python shell use utf-8 encoding
;;   ;;  (setenv "LC_CTYPE" "UTF-8")
;;   :config
;;   ;; (elpy-enable)
;;   (setq elpy-rpc-python-command "python3")
;;   (setq python-shell-interpreter-args "-i"
;;         py-electric-colon-active t
;;         python-indent-offset 4
;;         py-force-py-shell-name-p t
;;         py-shell-switch-buffers-on-execute-p t
;;         py-smart-indentation t
;;         python-shell-completion-native nil
;;         python-shell-interpreter "python3")
;;   (setq elpy-shell-starting-directory 'current-directory)
;;   (let ((disabled-modules '(elpy-module-flymake)))
;;     (setq elpy-modules (-difference elpy-modules disabled-modules))))

;; (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;elpy end;;;;;;;;;;;;;;;;;;;;;


(defun fk/async-process (command &optional name filter)
  "Start an async process by running the COMMAND string with bash. Return the
process object for it.

NAME is name for the process. Default is \"async-process\".

FILTER is function that runs after the process is finished, its args should be
\"(process output)\". Default is just messages the output."
  (make-process
   :command `("bash" "-c" ,command)
   :name (if name name
           "async-process")
   :filter (if filter filter
             (lambda (process output) (message (s-trim output))))))

;; Examples:
;;
;; (fk/async-process "ls")
;;
;; (fk/async-process "ls" "my ls process"
;;                   (lambda (process output) (message "Output:\n\n%s" output)))
;;
;; (fk/async-process "unknown command")


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))


(require 'all-the-icons)

(use-package python
  :straight (:type built-in)
  :init
  (add-to-list 'all-the-icons-icon-alist
               '("\\.py$" all-the-icons-alltheicon "python" :height 1.1 :face all-the-icons-dblue))
  ;;(global-unset-key (kbd "C-c C-v"));; unset python mode map
  ;;(define-key python-mode-map (kbd "C-c C-v") nil)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (python-indent-guess-indent-offset-verbose nil)
  :bind
  ( :map python-mode-map
    ;;("M-n" . python-nav-forward-block)
    ;;("M-p" . python-nav-backward-block)
    ("C-c r" . python-indent-shift-right)
    ("C-c l" . python-indent-shift-left)
    ;; affecting in insert mode as well
    ;; ("<tab>" . bicycle-cycle)
    ("C-c C-j" . nil)
    ("C-c C-v" . nil))
  :hook
  ;; With pyls:
  ;; pip install python-language-server flake8 pyls-black(optional) pyls-isort(optional)
  ;; pip install 'python-lsp-server[all]' is the maintained version of pyls
  ;; https://github.com/python-lsp/python-lsp-server
  ;; With pyright
  ;; sudo npm install -g pyright && pip install flake8 black(optional)
  ;; NOTE: these hooks runs in reverse order
  ;;(python-mode . fk/python-auto-f-string-mode)
  (python-mode . (lambda ()
                   (setq-local company-prescient-sort-length-enable nil)
                   ;; (lsp-deferred)
                   ;;(flycheck-add-next-checker 'python-pyright 'python-flake8 'python-mypy 'python-pylint 'lsp)
                   ;; (flycheck-add-next-checker 'lsp)
                   ;; (flycheck-add-next-checker 'python-pyright 'python-flake8 'python-pylint 'lsp 'append)
               ))
  ;;(python-mode . (lambda () (fk/add-local-hook 'before-save-hook 'eglot-format-buffer)))
  ;;(python-mode . eglot-ensure)
  ;; importmagic runs ~100mb ipython process per python file, and it does not
  ;; always find imports, 60%-70% maybe. I stop using this, but still want to keep.
  ;;(python-mode . importmagic-mode)
  ;; (python-mode . fk/activate-pyvenv)
  (python-mode . (lambda ()
                   (when (and (buffer-file-name)
                              (string=
                               (car (last (f-split (f-parent (buffer-file-name)))))
                               "tests"))
                     (fk/hide-second-level-blocks))))
  (python-mode . (lambda () (require 'tree-sitter-langs) (tree-sitter-hl-mode)))
  (python-mode . (lambda () (setq-local fill-column 88)))
  :config
  ;;;; Smart f-strings
  ;; https://github.com/ubolonton/emacs-tree-sitter/issues/52
  ;; TODO: Create a mode from this
  (defun fk/python-f-string-ify ()
    ;; Does nothing if major-mode is not python or point is not on a string.
    (when-let* ((python-mode-p (eq major-mode 'python-mode))
                (str (tree-sitter-node-at-point 'string))
                (text (ts-node-text str)))
      (let* ((is-f-string (string-match-p "^[bru]*f+[bru]*\\(\"\\|'\\)" text))
             (end-of-string (ts-node-end-position (tree-sitter-node-at-point 'string)))
             (is-there-format-method (string= ".format"
                                              (buffer-substring-no-properties end-of-string(+ end-of-string 7))))
             (should-f-string (and (s-contains-p "{" text)
                                   (s-contains-p "}" text)
                                   (not is-there-format-method))))
        (if should-f-string
            (unless is-f-string
              (save-excursion
                (goto-char (ts-node-start-position str))
                (insert "f")))
          (when is-f-string
            (save-excursion
              (goto-char (ts-node-start-position str))
              (when (char-equal (char-after) ?f)
                (delete-char 1))))))))

  (define-minor-mode fk/python-auto-f-string-mode  ; TODO: does not work well
    "Toggle fk/python-auto-f-string-mode which adds 'f' at the
beginning of the string that has curly brackets in it."
    :init-value t
    (if fk/python-auto-f-string-mode
        (progn
          (defadvice wrap-region-trigger (after smart-f-string activate) (fk/python-f-string-ify))
          (defadvice delete-char (after smart-f-string activate) (fk/python-f-string-ify))
          (defadvice delete-active-region (after smart-f-string activate) (fk/python-f-string-ify))
          (defadvice kill-region (after smart-f-string activate) (fk/python-f-string-ify)))
      (ad-remove-advice 'wrap-region-trigger 'after 'smart-f-string)
      (ad-update 'wrap-region-trigger)
      (ad-remove-advice 'delete-char 'after 'smart-f-string)
      (ad-update 'delete-char)
      (ad-remove-advice 'delete-active-region 'after 'smart-f-string)
      (ad-update 'delete-active-region)
      (ad-remove-advice 'kill-region 'after 'smart-f-string)
      (ad-update 'kill-region))))



;;(add-hook 'python-mode-hook #'lsp)
;; (require 'bicycle)
;; (defun baba/outline-overview ()
;;   "Show only outline headings."
;;   (outline-show-all)
;;   (outline-hide-body))
;; (defun jak/outline-python()
;;   "Fold only definitions in Python."
;;   (setq outline-regexp
;;         (rx (or
;;              ;; Definitions
;;              (group (group (* space)) bow (or "class" "def") eow)

;;              ;; Decorators
;;              (group (group (* space)) "@"))))
;;   (baba/outline-overview))

;; (add-hook 'after-save-hook 'python-hook-function)
;; (add-hook 'python-mode-hook 'jak/outline-python)

;; depends on isort: apt install isort
(use-package py-isort
  :ensure t
  :commands py-isort-buffer)

;; https://emacs-lsp.github.io/dap-mode/page/configuration/#python
;; pip install "ptvsd>=4.2" # requires

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-python)
  (require 'dap-ui)
  (dap-mode t)
  (dap-ui-mode t)
  ;; enables mouse hover support
  (dap-tooltip-mode t)
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode t)

  )
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))



;; (dap-register-debug-template "Debug Runner"
;;                              (list :type "python"
;;                                    :args "-i"
;;                                    :cwd nil
;;                                    :env '(("DEBUG" . "1"))
;;                                    :target-module (expand-file-name "~/.virtualenvs/ksaflyer_env/bin/python")
;;                                    :request "launch"
;;                                    :name "KSAFlyer"))


;; (dap-debug
;;  (list :type "python"
;;        :args "runserver --noreload"
;;        :cwd "/home/jaavedkhan/cdrive/code/personal_projects/apps/repo_ksaflyer/ksaflyer"
;;        :module nil
;;        :console "integratedTerminal"
;;        :program "/home/jaavedkhan/cdrive/code/personal_projects/apps/repo_ksaflyer/ksaflyer/manage.py"
;;        :request "launch"
;;        :name "Python: Django"
;;        :django t))

;;;;;;;;;;;;;;;;;;;;; lsp-mode python end ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package importmagic
  :ensure t
  ;; pip install importmagic epc
  ;;
  ;; importmagic runs ~100mb ipython process per python file, and it does not
  ;; always find imports, 60%-70% maybe. I stop using this, but still want to keep.
  :config
    (add-hook 'python-mode-hook 'importmagic-mode)
  :commands importmagic-mode)

(use-package blacken
  ;; pip3 install black flake8
  :ensure t
  :commands blacken-mode blacken-buffer
  )


(use-package elpy
  :init
  (elpy-enable)
  :config
  (setq python-shell-interpreter "ipython"
        ;; python-shell-interpreter-args "-i --simple-prompt")
	python-shell-interpreter-args "-i")
  ;; (add-hook 'python-mode-hook 'eldoc-mode)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-shell-echo-output nil)
  (setq python-shell-completion-native-enable nil)
  (setq elpy-rpc-backend "jedi")
  (setq python-indent-offset 4
        python-indent 4)
  )

;; from this reddit: https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
;; python code folding
;; Customize mode-specific Outline folding.
;; (add-hook 'python-mode-hook
;;         (defun baba/outline-python ()
;;             "Fold only definitions in Python."
;;             (setq outline-regexp
;;                 (rx (or
;;                         ;; Definitions
;;                         (group (group (* space)) bow (or "class" "def") eow)

;;                         ;; Decorators
;;                         (group (group (* space)) "@"))))
;;             (baba/outline-overview)))

(provide 'init-python)
;;; init-python.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
