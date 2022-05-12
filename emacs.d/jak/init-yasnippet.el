;; ref: https://joaotavora.github.io/yasnippet/snippet-expansion.html

(use-package yasnippet
  :ensure t
  :hook
  (after-init . yas-global-mode)
  :config
  (progn
    (use-package yasnippet-snippets
      :ensure t
      :bind (("C-c C-y" . yas-expand)
             ("C-." . yas-insert-snippet)))
    )
  ;; adding es6 snippets
 ;; https://github.com/CodyReichert/es6-snippets  
  (add-to-list 'load-path "~/.emacs.d/es6-snippets")
  (require 'es6-snippets)
  ;; Bind `SPC' to `yas-expand' when snippet expansion available (it
  ;; will still call `self-insert-command' otherwise).
  ;;(define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)

  )


(use-package emacs-snippets
  :straight (:host github :repo "linuxing3/emacs-snippets")
  :after yasnippet
  )

(provide 'init-yasnippet)
