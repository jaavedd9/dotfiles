
(use-package ispell
  :ensure nil
  ;; native package
  :config
  (progn
  ;; (ispell-local-dictionary "english")
  (ispell-change-dictionary "american"))
  )

(use-package define-word
  :ensure t)

(use-package powerthesaurus 
  :ensure t)

(provide 'init-languages)
