;; (setq exec-path (append exec-path '("~/.nvm/versions/node/v14.17.4/bin")))

;; from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package eglot
  :ensure t
  )
;; disabled lsp-ui for performance reasons
;; Optional: fine-tune lsp-idle-delay. This variable determines how often lsp-mode will refresh the highlights, lenses, links, etc while you type.
(setq lsp-idle-delay 0.500)
;; to disable formatting on save in lsp mode
(setq lsp-enable-on-type-formatting nil)
;; to disable file watching
;;https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
(setq lsp-enable-file-watchers nil)
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  ;; :custom
  ;;(lsp-pyright-auto-import-completions nil)

  ;;(lsp-pyright-typechecking-mode "off")
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (fk/async-process
   "npm outdated -g | grep pyright | wc -l" nil
   (lambda (process output)
     (pcase output
       ("0\n" (message "Pyright is up to date."))
       ("1\n" (message "A pyright update is available.")))))
  ;; for setting python version of pyright lsp
  (setq lsp-pyright-python-executable-cmd "python3")
  (setq lsp-pyright-venv-path "/home/jaavedkhan/.virtualenvs")
  )

(use-package helm-lsp
  :ensure t
  :commands
  helm-lsp-workspace-symbol)

(use-package  lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  )


(use-package lsp-mode
  :ensure t
  :hook (
         ;;         (scala-mode . lsp)
         (css-mode . lsp)
         (web-mode . lsp)
         (js2-mode . lsp)
         (typescript-mode . lsp)
         (python-mode . lsp)
         )
  :commands (lsp lsp-deferred)
  :hook
  ;;(add-hook 'html-mode-hook 'emmet-mode)
  (lsp-mode . undo-tree-mode)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (push 'company-bootstrap company-backends)
  ;; :ensure-system-package
  ;; ((typescript-language-server . "npm install -g typescript-language-server")
  ;;  (javascript-typescript-langserver . "npm install -g javascript-typescript-langserver")
  ;;  (yaml-language-server . "npm install -g yaml-language-server")
  ;;  (tsc . "npm install -g typescript"))
  ;; requires pip install pip3 install 'python-lsp-server[all]'
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  ;; (define-key lsp-signature-previous "\M-p" nil) ;; remove M-p key binding
  ;; (define-key lsp-signature-previous (kbd "C-M-p"))

  :custom
  (lsp-auto-guess-root t)
  (lsp-keymap-prefix "M-m l")
  (lsp-modeline-diagnostics-enable nil)
  (lsp-keep-workspace-alive nil)
  (lsp-auto-execute-action nil)
  (lsp-before-save-edits nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-diagnostic-package :none)
  (lsp-completion-provider :none)
  (lsp-file-watch-threshold 1500)  ; pyright has more than 1000
  (lsp-enable-links nil)
  (lsp-log-io nil) ; if set to true can cause a performance hit
  ;; Maybe set in future:
  ;;(lsp-enable-on-type-formatting nil)

  :custom-face
  (lsp-face-highlight-read ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-write ((t (:underline t :background nil :foreground nil))))
  (lsp-face-highlight-textual ((t (:underline t :background nil :foreground nil))))

  ;; register client
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
  ;;                   :major-modes '(python-mode)
  ;;                   :server-id 'pyls))

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("html-languageserver" "--stdio"))
  ;;                   :major-modes '(web-mode)
  ;;                   :server-id 'html-languageserver))

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("yaml-language-server" "--stdio"))
  ;;                   :major-modes '(yaml-mode)
  ;;                   :server-id 'yaml-language-server))

  )

;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; npm packages were accessible by emacs
;; solved it by adding below configs
;; https://emacs.stackexchange.com/a/34202/26147
;; https://stackoverflow.com/questions/4393628/emacs-shell-command-not-found
;; use node version == 12.18.2
;; nvm install 12.18.2
;; nvm use 12.18.2
;; nvm alias default 12.18.2

;; Web development

;; LSP requirements on the server
;; sudo npm i -g typescript-language-server; sudo npm i -g typescript
;; sudo npm i -g javascript-typescript-langserver


(add-to-list 'auto-mode-alist '("\\.css?\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))

;; for lsp performance improvement
(setq lsp-log-io nil) ;; Don't log everything = speed
(setq company-lsp-cache-candidates t)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (progn
    ;; (setq lsp-ui-sideline-show-diagnostics t)
    ;; (setq lsp-ui-sideline-show-hover t)
    ;;(setq lsp-ui-sideline-show-code-actions t)
    )
  )

(use-package tree-sitter-langs
  :ensure t
  )

(use-package helm-lsp
  :ensure t
  )

(use-package lsp-treemacs
  :ensure t
  :commands (lsp-treemacs-errors-list)
  )

;;(add-hook 'html-​mode-hook 'lsp) ;; lsp in html

;; remove conflict binding with projectile
(global-unset-key (kbd "M-p"))
(global-set-key (kbd "M-p") 'projectile-command-map)
(provide 'init-lsp)
