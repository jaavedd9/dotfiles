(use-package 
  js2-mode 
  :ensure t 
  :config (progn (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) 
                 (add-hook 'js2-mode-hook #'js2-imenu-extras-mode) 
                 (setq js2-basic-offset 2)))

(use-package 
  js2-refactor 
  :ensure t)

(use-package 
  xref-js2 
  :ensure t)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda () 
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))



;; tide mode
(use-package 
   tide 
   :ensure t 
   :config (defun setup-tide-mode () 
             (interactive) 
             (tide-setup) 
             (flycheck-mode +1)
             ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
             (eldoc-mode +1) 
             (tide-hl-identifier-mode +1)
             ;; company is an optional dependency. You have to
             ;; install it separately via package-install
             ;; `M-x package-install [ret] company`
             (company-mode +1))
;;   ;; aligns annotation to the right hand side
   (setq company-tooltip-align-annotations t)

   ;; formats the buffer before saving
   (add-hook 'before-save-hook 'tide-format-before-save)
   (add-hook 'typescript-mode-hook #'setup-tide-mode))


(add-hook 'web-mode-hook (lambda () 
                           (when (string-equal "js" (file-name-extension buffer-file-name)) 
                             (setup-tide-mode))))

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
;;(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(tsx-tide)))

(add-hook 'js2-mode-hook (lambda () 
                           (setup-tide-mode)))



;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
;; set indent level2
(setq typescript-indent-level 2)

;; eglot

;; (add-hook 'typescript-mode-hook 'eglot-ensure)
;; (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))

;; apply eslintConfig from package.json or else where
(defun eslint-current-buffer () 
  "run a command on the current file and revert the buffer" 
  (interactive) 
  (shell-command (format "eslint --fix %s" (shell-quote-argument (buffer-file-name)))) 
  (revert-buffer t t t))

;; (global-set-key (kbd "C-S-e") 'ansible-lint-current-buffer)


;; typescript and tsx javascript
;; took sometime to figure out config with 2 indentation
;; and supports tsx


;; (use-package typescript-mode
;;   :ensure t
;;   :mode "\\.tsx?$")
;; (use-package emmet-mode :ensure t :hook typescript-mode)
;; (use-package tree-sitter :ensure t)
;; (use-package tree-sitter-langs :ensure t)
;; (use-package prettier-js :ensure t :hook (typescript-mode))

;; ;; couldn't make it work with `use-package`, plain elisp instead
;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)
;; (add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)

;; (use-package typescript-mode
;;   :mode (rx ".ts" string-end)
;;   :init
;;   (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
;;   (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))

;; (use-package tree-sitter
;;   :hook (typescript-mode . tree-sitter-hl-mode)
;;   :config
;;   (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))

(use-package js-react-redux-yasnippets
  :ensure t)

(use-package eslintd-fix
  :ensure t
  )

(use-package eslint-fix
  :ensure t
  )

(provide 'init-javascript)
