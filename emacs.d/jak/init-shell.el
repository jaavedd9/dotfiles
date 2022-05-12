(require 'magit)

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun dw/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun dw/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun dw/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun dw/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (dw/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face 
                 `(:foreground "#62aeed"))
     (propertize " λ " 'face 
                 `(:foreground "white"))
     (propertize (dw/get-prompt-path) 'face 
                 `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face 
                    `(:foreground "white"))
        (propertize (concat " " current-branch) 'face 
                    `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face 
                    `(:foreground "white"))
        (propertize package-version 'face 
                    `(:foreground "#e8a206"))))
     (propertize " • " 'face 
                 `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face 
                 `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face 
                     `(:foreground "red2"))
       (propertize "\nλ" 'face 
                   `(:foreground "#aece4a")))
     (propertize " " 'face 
                 `(:foreground "white")))))

;; (unless dw/is-termux
;; (add-hook 'eshell-banner-load-hook
;; (lambda ()
;; (setq eshell-banner-message
;; (concat "\n" (propertize " " 'display (create-image "~/.dotfiles/.emacs.d/images/flux_banner.png" 'png nil :scale 0.2 :align-to "center")) "\n\n")))))

(defun dw/eshell-configure ()
  ;; (require 'evil-collection-eshell)
  ;; (evil-collection-eshell-setup)

  (use-package 
    xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () 
              (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () 
              (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;; (evil-normalize-keymaps)
  (setenv "PAGER" "cat")
  (setq eshell-prompt-function 'dw/eshell-prompt eshell-prompt-regexp "^λ " eshell-history-size
        10000 eshell-buffer-maximum-lines 10000 eshell-hist-ignoredups t eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t eshell-prefer-lisp-functions nil))
(use-package 
  eshell 
  :hook (eshell-first-time-mode . dw/eshell-configure) 
  :init (setq eshell-directory-name "~/.emacs.d/eshell/" eshell-aliases-file (expand-file-name
                                                                              "~/.emacs.d/eshell/alias")))
(use-package 
  eshell-z 
  :ensure t 
  :hook ((eshell-mode . (lambda () 
                          (require 'eshell-z))) 
         (eshell-z-change-dir . (lambda () 
                                  (eshell/pushd (eshell/pwd))))))
;; (use-package 
;;   exec-path-from-shell 
;;   :ensure t 
;;   :init (setq exec-path-from-shell-check-startup-files nil) 
;;   :config (when (memq window-system '(mac ns x)) 
;;             (exec-path-from-shell-initialize)))

;; (dw/leader-key-def
;; "SPC" 'eshell)

(with-eval-after-load 'esh-opt 
  (setq eshell-destroy-buffer-when-process-dies t) 
  (setq eshell-visual-commands '("htop" "zsh" "vim" "top")))

;; Better colors
(use-package 
  eterm-256color 
  :ensure t 
  :hook (term-mode . eterm-256color-mode))


;; fish completions
(use-package 
  fish-completion 
  :ensure t 
  :hook (eshell-mode . fish-completion-mode))


;; highlight
(use-package 
  eshell-syntax-highlighting 
  :ensure t 
  :after esh-mode 
  :config (eshell-syntax-highlighting-global-mode +1))

;; history auto completion
(use-package 
  esh-autosuggest 
  :ensure t 
  :hook (eshell-mode . esh-autosuggest-mode) 
  :config (setq esh-autosuggest-delay 0.1) 
  (set-face-foreground 'company-preview-common "#4b5668") 
  (set-face-background 'company-preview nil))

;; eshell-toggle allows me to toggle an Eshell window below the current buffer for the path (or project path) of the buffer.
(use-package 
  eshell-toggle 
  :ensure t 
  :after eshell 
  :bind ("C-M-'" . eshell-toggle) 
  :custom (eshell-toggle-size-fraction 3) 
  (eshell-toggle-use-projectile-root t) 
  (eshell-toggle-run-command nil)) 
(provide 'init-shell)


