

;; configure jsx-tide checker to run after your default jsx checker
;;(flycheck-add-mode 'javascript-eslint 'typescript-tslint 'typescript-tsx-mode 'web-mode)
;;(flycheck-add-next-checker 'typescript-tslint 'javascript-eslint 'append)

;;(flycheck-add-next-checker 'lsp 'typescript-tslint 'append)

;;(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(tsx-tide)))

(use-package 
  typescript-mode 
  :ensure t
  ;; :mode "\\.tsx?$"
  :config
  (setq typescript-indent-level 2) 
  ;; :hook
  ;; (typescript-mode . (lambda () 
  ;;                      (flycheck-add-mode
  ;;                       'typescript-tslint
  ;;                       'typescript-tsx-mode) 
  ;;                      ;; (flycheck-add-next-checker
  ;;                      ;;  'typescript-tide
  ;;                      ;;  'typescript-tslint
  ;;                      ;;  'javascript-eslint
  ;;                      ;;  'lsp)
  ;;                      ))
  )


(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; set indent level2

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

;; (use-package typescript-mode
;;   :mode (rx ".ts" string-end)
;;   :init
;;   (define-derived-mode typescript-tsx-mode typescript-mode "typescript-tsx")
;;   (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) #'typescript-tsx-mode)))

;; (use-package tree-sitter
;;   :hook (typescript-mode . tree-sitter-hl-mode)
;;   :config
;;   (setf (alist-get 'typescript-tsx-mode tree-sitter-major-mode-language-alist) 'tsx))

(provide 'init-typescript)
