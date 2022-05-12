(use-package company-restclient
  :ensure t
  )


(use-package restclient
  :ensure t
  :mode (
         ("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :hook
  (restclient-mode . display-line-numbers-mode)
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient))
  )


(provide 'init-rest)
