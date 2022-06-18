

(use-package helm-dash
  :init
  (global-set-key (kbd "C-c d") 'helm-dash-at-point)
  (global-set-key (kbd "C-x C-y") 'helm-dash-at-point)
  (defun c-doc ()
    (setq helm-dash-docsets '("C")))
  (defun c++-doc ()
    (setq helm-dash-docsets '("C" "C++")))
  ;; add python and ansible modes etc
  (add-hook 'c-mode-hook 'c-doc)
  (add-hook 'c++-mode-hook 'c++-doc)
  ;; python mode docs
  (defun python-doc ()
    (interactive)
    (setq-local dash-docs-docsets '("Python 3" "Django")))
  (add-hook 'python-mode-hook 'python-doc)
  ;; web mode docs
  (defun web-mode-doc ()
    (interactive)
    (setq-local dash-docs-docsets '("HTML" "Bootstrap 4" "Django" "React" "React_Native")))
  (add-hook 'web-mode-hook 'web-mode-doc)
  ;; javascript mode docs
  (defun js-mode-doc ()
    (interactive)
    (setq-local dash-docs-docsets '("JavaScript" "NodeJS")))
  (add-hook 'typescript-mode-hook 'js-mode-doc)
  ;; yaml mode docs
  (defun yaml-mode-doc ()
    (interactive)
    (setq-local dash-docs-docsets '("Ansible" "Docker" "Kubernetes")))
  (add-hook 'yaml-mode-hook 'yaml-mode-doc)
  ;; Docker mode docs
  (defun docker-mode-doc ()
    (interactive)
    (setq-local dash-docs-docsets '("Docker")))
  (add-hook 'dockerfile-mode-hook 'docker-mode-doc)
  ;; Jinja mode docs
  (defun jinja2-mode-doc ()
    (interactive)
    (setq-local dash-docs-docsets '("Jinja" "Ansible")))
  (add-hook 'jinja2-mode-hook 'jinja2-mode-doc)
  :config
  (setq dash-docs-docsets-path (concat dropbox-path "emacs/docsets"))
  (setq dash-docs-enable-debugging nil)
  (setq helm-dash-min-length 1)
  (setq helm-dash-browser-func 'eww)
  (setq helm-dash-common-docsets '("emacs"))
  ;; :custom
  ;;(helm-dash-docsets-path "/mnt/data/Dropbox/emacs/docssets" "Changed defautl docsets directory")
  )

(use-package devdocs
  :ensure t
  :config
  (setq devdocs-data-dir  (concat dropbox-path "emacs/devdocs"))
  (add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.9" "django~4.0"))))
  (add-hook 'yaml-mode-hook
          (lambda () (setq-local devdocs-current-docs '("ansible~2.10"))))
  (add-hook 'typescript-mode-hook
          (lambda () (setq-local devdocs-current-docs '("typescript" "node~16_lts" "javascript"))))
  (add-hook 'genehack-vue-mode
          (lambda () (setq-local devdocs-current-docs '("vue~2" "vuex~3"))))
  (add-hook 'web-mode-hook
          (lambda () (setq-local devdocs-current-docs '(("html" "bootstrap~4" "django~4.0")))))
  )

(use-package devdocs-browser
  :ensure t
  :config
  (setq devdocs-browser-major-mode-docs-alist 
 '((c++-mode "cpp")
   (c-mode "c")
   (go-mode "go")
   (python-mode '("Python" "Django"))
   (typescript-mode '("JavaScript", "node"))
   (emacs-lisp-mode "elisp")
   (cmake-mode "CMake"))
        )
  )

(provide 'init-documentations)
;;; init-documentation ends here
