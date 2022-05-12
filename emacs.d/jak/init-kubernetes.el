(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :ensure t)

(use-package kubernetes-helm
  :ensure t)

(provide 'init-kubernetes)
