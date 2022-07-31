;; evil package
(use-package 
  evil 
  :ensure t 
  :bind ((:map evil-window-map
             ("C-j" . evil-window-down)
             ("C-k" . evil-window-up)
             ("C-l" . evil-window-right)
             ("C-h" . evil-window-left)))
  :init ;; tweak evil's configuration before loading it
  (progn 
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-C-u-scroll t) ;; to use C-u, bound to universal argument, to move up and down
    ;; (setq evil-vsplit-window-right t)
    ;; (setq evil-split-window-below t)
    (evil-mode)
    ;; (global-undo-tree-mode)
    ;; (setq evil-set-undo-system 'undo-tree)
    (setq evil-want-fine-undo t) 
    (setq evil-respect-visual-line-mode t) 
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) 
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up) 
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)) 
    ;; evil-indent-plus
    (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
    (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
    (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
    (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
    (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
    (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down)
  :config (progn 
            (setq evil-emacs-state-cursor '("purple" bar)) 
            (setq evil-normal-state-cursor '("firebrick" box)) 
            (setq evil-visual-state-cursor '("cyan" box)) 
            (setq evil-insert-state-cursor '("green" bar)) 
            (setq evil-replace-state-cursor '("yellow" bar)) 
            (setq evil-operator-state-cursor '("white" hollow)) 
            (setq evil-want-C-u-scroll t) 
            (add-hook 'org-capture-mode-hook 'evil-insert-state) 
            (add-hook 'with-editor-mode-hook 'evil-insert-state) 
            (evil-set-command-property 'xref-find-references 
                                       :jump t)
            ;; remap c-k to use company "select previous"
            (eval-after-load "evil-maps" (dolist (map '(
                                                        ;;evil-motion-state-map
                                                        evil-insert-state-map
                                                        ;;evil-emacs-state-map
                                                        )) 
                                           (define-key (eval map) "\C-k" nil))) 
            (evil-define-key 'normal 'global "gr" 'xref-find-references)
            ;; (evil-add-command-properties #'evil-next-line :jump t)
            ;; (evil-add-command-properties #'evil-previous-line :jump t)
            ;; (evil-add-command-properties #'helm-occur :jump t)
            ;; (evil-add-command-properties #'helm-rg :jump t)
            (evil-add-command-properties #'helm-rg 
                                         :jump t) 
            (evil-add-command-properties #'helm-ag 
                                         :jump t) 
            (evil-add-command-properties #'projectile-previous-project-buffer 
                                         :jump t) 
            (evil-add-command-properties #'projectile-next-project-buffer 
                                         :jump t) 
            (evil-add-command-properties #'helm-projectile-switch-project 
                                         :jump t) 
            (evil-add-command-properties #'helm-projectile-find-file 
                                         :jump t) 
            (evil-add-command-properties #'helm-projectile-find-dir 
                                         :jump t)
            (evil-add-command-properties #'evil-window-left
                                         :jump t)
            (evil-add-command-properties #'evil-window-right
                                         :jump t)
            (evil-add-command-properties #'evil-window-up
                                         :jump t)
            (evil-add-command-properties #'evil-window-down
                                         :jump t)
            (evil-add-command-properties #'evil-scroll-down
                                         :jump t)
            (evil-add-command-properties #'evil-scroll-up
                                         :jump t)
            (evil-add-command-properties #'helm-occur
                                         :jump t)
            (evil-add-command-properties #'dired-find-file :jump t)
            (evil-add-command-properties #'dired-jump :jump t)
            ;; (setq evil--jumps-buffer-targets "")
            ;; add numbered jumpts to the jump list
            (defun vj-rename-dired-buffer ()
            (interactive)
            (unless (string-match-p "Dired:" (buffer-name))
                (rename-buffer (concat "Dired:" (buffer-name)))))

            (add-hook 'dired-mode-hook 'vj-rename-dired-buffer)
            (setq evil--jumps-buffer-targets "\\(\\*\\(\\new\\|scratch\\)\\*\\|Dired:.+\\)")

            (defun my-jump-advice (oldfun &rest args) 
              (let ((old-pos (point))) 
                (apply oldfun args) 
                (when (> (abs (- (line-number-at-pos old-pos) 
                                 (line-number-at-pos (point)))) 1) 
                  (evil-set-jump old-pos)))) 
            (advice-add 'evil-next-line 
                        :around #'my-jump-advice) 
            (advice-add 'evil-previous-line 
                        :around #'my-jump-advice)))

(use-package 
  evil-collection 
  :ensure t 
  :after evil 
  :init (evil-collection-init) 
  :config
  ;; (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (setq evil-collection-setup-minibuffer t) 
  (setq evil-collection-want-find-usages-bindings t))

(use-package 
  evil-tutor 
  :ensure t)

(use-package 
  evil-surround 
  :ensure t 
  :config (global-evil-surround-mode 1))

(use-package 
  evil-matchit 
  :ensure t 
  :config (global-evil-matchit-mode 1))

(use-package 
  evil-escape 
  :ensure t 
  :config
  (progn (setq-default evil-escape-key-sequence "jk") ;; default
                 (setq-default evil-escape-delay 0.2)         ;; default
                 (setq evil-escape-excluded-major-modes (list 'magit-status-mode 'magit-refs-mode 'magit-log-mode))
                 (evil-escape-mode)))

(use-package 
  evil-commentary 
  :ensure t 
  :config (evil-commentary-mode))

(use-package 
  evil-exchange 
  :ensure t 
  :config (evil-exchange-install))

(use-package 
  goto-chg 
  :ensure t)

(use-package 
  evil-anzu 
  :ensure t 
  :config (with-eval-after-load 'evil 
            (require 'evil-anzu)))


(use-package 
  evil-goggles 
  :ensure t 
  :config (evil-goggles-mode)
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces) 
  (setq evil-goggles-pulse t) 
  (setq evil-goggles-duration 0.20) ;; default is 0.200
  ;; (set-face-attribute 'evil-goggles-yank-face nil
  ;; :background "Green"
  ;; ;;:weight 'bold
  ;; )
  )

(use-package 
  evil-textobj-line 
  :ensure t)

;; (use-package
;;   evil-indent-textobject 
;;   :ensure t)

(use-package 
  evil-indent-plus 
  :ensure t)


(provide 'init-evil-mode)

;; init-evil-mode ends here
