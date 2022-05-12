(require 'eglot)
(require 'web-mode)


(define-derived-mode genehack-vue-mode web-mode "ghVue"
  "A major mode derived from web-mode, for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))
(add-hook 'genehack-vue-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(genehack-vue-mode "vls"))

;; vue
(require 'lsp-mode)
(setq lsp-vetur-format-default-formatter-css "none")
(setq lsp-vetur-format-default-formatter-html "none")
(setq lsp-vetur-format-default-formatter-js "none")
(setq lsp-vetur-validation-template nil)
;; ;; for completions
;; ;; (use-package company-lsp
;; ;;   :after lsp-mode
;; ;;   :config (push 'company-lsp company-backends))

;; (use-package vue-mode
;;   :mode "\\.vue\\'"
;;   :config
;;   (add-hook 'vue-mode-hook #'lsp))

;;(setq after-change-functions nil)
;;(setq inhibit-modification-hooks t)
(provide 'init-vue)
