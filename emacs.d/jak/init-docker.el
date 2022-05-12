(use-package 
  docker 
  :ensure t 
  :bind ("C-c D" . docker))


(use-package 
  dockerfile-mode 
  :ensure t 
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package 
  docker-tramp 
  :ensure t)

(provide 'init-docker)
