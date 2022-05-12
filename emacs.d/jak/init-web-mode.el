;;; package --- Summary:
;;; Commentary:
;; web-mode
(require 'web-mode)

;; default is 4
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
;;; Code:
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-tag-auto-close-style 1)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-css-colorization t)

(setq web-mode-enable-auto-indentation nil)
(setq web-mode-enable-auto-quoting nil)
;; enhancements
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-heredoc-fontification t)
;; for using spaces instead of tabs for indentation

(setq-default indent-tabs-mode nil)


(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; for crafter free marker template engine
;; https://freemarker.apache.org/
(add-to-list 'auto-mode-alist '("\\.ftl?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; disable smartparens mode
;; (progn
;;   (require 'smartparens-config)
;;   (show-paren-mode -1))

;; (defun turnoff-smartparens ()
;;   (turn-off-smartparens-mode))

;; (add-hook 'web-mode-hook 'turnoff-smartparens)

(use-package web-beautify
  :ensure t
  )

;; (use-package prettier
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-prettier-mode)
;;   )


(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/.nvm/versions/node/v14.17.5/bin")))

(setq exec-path (append exec-path '(expand-file-name "~/.nvm/versions/node/v14.17.5/bin")))

;;(add-to-list 'auto-mode-alist '("\\.j2?\\'" . web-mode))
;; ************ setting up for editing react
;; following this: https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea


(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)
(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  )


 ;; let smartparens mode take of this
(require 'smartparens)
;; https://smartparens.readthedocs.io/en/latest/pair-management.html
(sp-pair "%" "%" :wrap "C-%")
;; (sp-pair "<" ">" :wrap "C->")
(sp-pair "{%" " %}")
(sp-pair "{{" " }}")

(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'")
        ("freemarker"    . "\\.ftl\\'")
        ;; ("django"     . "\\.j2\\'")
        )
      )


;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)

;; --- Emmet Mode ---
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any web modes
;; (add-hook 'web-mode-hook 'lsp) ;; Auto-start on any web modes


;; this auto-rename tag is causing the cloberring issue while rename
;; 
;;(add-hook 'web-mode-hook 'auto-rename-tag-mode) ;; Auto-start on any web modes


;; (use-package auto-rename-tag
;;   :ensure t
;;   :hook
;;   (web-mode-hook . auto-rename-tag-mode))

;; to change emmet suggestion based on tag
;; https://fransiska.github.io/emacs/2017/08/21/web-development-in-emacs
(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
  	                (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "javascript")
    	           (yas-activate-extra-mode 'javascript-mode)
      	         (yas-deactivate-extra-mode 'javascript-mode))
               (if (string= web-mode-cur-language "css")
    	           (setq emmet-use-css-transform t)
      	         (setq emmet-use-css-transform nil)))))


;; (use-package company-tern
;;   :ensure t
;;   )
;; (defun my-web-mode-hook ()
;;   "Hook for `web-mode'."
;;   (set (make-local-variable 'company-backends)
;;        '(company-tern company-web-html company-yasnippet company-files)))

;; (add-hook 'web-mode-hook 'my-web-mode-hook)

;; Enable JavaScript completion between <script>...</script> etc.
(advice-add 'company-tern :before
            #'(lambda (&rest _)
                (if (equal major-mode 'web-mode)
                    (let ((web-mode-cur-language
                           (web-mode-language-at-pos)))
                      (if (or (string= web-mode-cur-language "javascript")
                              (string= web-mode-cur-language "jsx"))
                          (unless tern-mode (tern-mode))
                        (if tern-mode (tern-mode -1)))))))

;; (with-eval-after-load 'company
;;   ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
;;   (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
;;     (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
;;         (setq ad-return-value nil)
;;       ad-do-it)))

;; unset existing keybinding
(define-key web-mode-map (kbd "C-c C-i") nil)

;; react native tsx
(define-derived-mode typescript-tsx-mode typescript-mode "TypeScript-tsx")
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))

  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

(add-hook 'web-mode-hook 'lsp) ;; Auto-start on any web modes


(provide 'init-web-mode)
;;; init-web-mode ends here
