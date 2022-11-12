;;; Package --- Summary
;;; Commentary:
;; Emacs Startup File --- initialization for Emacs
;;; .emacs.d/init.el
;; ===================================
;; MELPA Package Support
;; ===================================
;; Cache 50MB before garbage collection
;; Enables basic packaging support
;; default is 800kb
;;; Code:
;;(setq debug-on-error t)
;;(setq gc-cons-threshold 500000000)

;; evil integration conflicts
;; following suggestion mentioned here
;; https://www.reddit.com/r/emacs/comments/aybp3b/evilcollection_error/
(setq evil-want-keybinding nil) 

;; lsp performance https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 2 * 1000 * 1000) ;; earlyer it was 10^8

(setq ring-bell-function 'ignore)

;; to make startup faster
;; https://github.com/KaratasFurkan/.emacs.d#garbage-collection
(setq gc-cons-threshold most-positive-fixnum)
(setq dropbox-path "/mnt/data/Dropbox/")
;;(setq dropbox-path "/home/jaavedkhan/Dropbox/")
(setq org-roam-directory  (file-truename(concat dropbox-path  "emacs/org_files/org_roam/files/")))

(defconst 1mb 1048576)
(defconst 20mb 20971520)
(defconst 30mb 31457280)
(defconst 50mb 52428800)

(defun fk/defer-garbage-collection () 
  (setq gc-cons-threshold most-positive-fixnum))

(defun fk/restore-garbage-collection () 
  (run-at-time 1 nil (lambda () 
                       (setq gc-cons-threshold 30mb))))

(add-hook 'emacs-startup-hook 'fk/restore-garbage-collection 100)
(add-hook 'minibuffer-setup-hook 'fk/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook 'fk/restore-garbage-collection)

(setq read-process-output-max 1mb) ;; lsp-mode's performance suggest
;; end start up faster


(require 'package)
;; Adds the Melpa archive to the list of available repositories
;; using stable for lsp-mode autocompletion to work
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; for https://github.com/typefo/company-bootstrap
;; (add-to-list 'package-archives
;;              '("elpa" . "https://elpa.typefo.com/packages/") t)
;; install org-plus-contrib
;; https://github.com/syl20bnr/spacemacs/issues/10251#issuecomment-567317974
;; https://github.com/syl20bnr/spacemacs/issues/10251
;; org contributed packages https://orgmode.org/worg/org-contrib/
;; to install org contributed packages
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; To avoid bad request error
;; ref: https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Initializes the package infrastructure
(package-initialize)

;; install straight package
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory)) 
      (bootstrap-version 5)) 
  (unless (file-exists-p bootstrap-file) 
    (with-current-buffer (url-retrieve-synchronously
                          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                          'silent 'inhibit-cookies) 
      (goto-char (point-max)) 
      (eval-print-last-sexp))) 
  (load bootstrap-file nil 'nomessage))

;; If there are no archived package contents, refresh them
(when (not package-archive-contents) 
  (package-refresh-contents))

;; install use-package with straight
(straight-use-package 'use-package)

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages 
  '(better-defaults ;; Set up some better Emacs defaults
    helm            ;; incremental completion and selection narrowing framework
    elpy            ;; Emacs Lisp python env
    ;; py-autopep8                     ;; run autopep8 on save
    ;; blacken                         ;; black formatting on save
    all-the-icons ;; might needed by  packages before it use-package install it
    ;;lsp-ui ;; might needed by  packages before it use-package install it
    doom-themes ;; doom theme
    color-theme-sanityinc-tomorrow company
    ;;    company-box
    company-quickhelp company-jedi company-anaconda company-web smartparens
    ;;    company-tern
    company-statistics
    ;;    company-try-hard
    company-shell org-caldav projectile helm-projectile zenburn-theme oauth2 ace-jump-mode
    ;; ace-window
    ace-jump-helm-line git-gutter helm-dash helm-descbinds web-mode emmet-mode
    ;; javascript
    js2-mode js2-refactor json-mode ac-js2 js-comint rainbow-delimiters lsp-mode vue-mode
    ;;org-trello
    prodigy restart-emacs magit ein writeroom-mode helm-rg helm-fuzzy))


;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package) 
          (unless (package-installed-p package) 
            (package-install package))) myPackages)


(use-package flycheck
  :ensure t
  :custom
  (flycheck-display-errors-delay 0)
  )
;; On the fly syntax checking

;; ace-window
;; thoughtful keys to switch windows
(use-package 
  ace-window 
  :ensure t
  ;; :straight (:host github :repo "KaratasFurkan/ace-window" :branch "feature/posframe")
  :custom (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) 
  (aw-ignore-current t) 
  :custom-face (aw-leading-char-face ((t 
                                       (:height 1000 
                                                :foreground "red")))) 
  :bind* (
          ;;("C-q" . aw-flip-window)  ; last window
          :map windows ("w" . ace-window) 
               ("D" . ace-delete-window) 
               ("s" . ace-swap-window) 
               ("l" . aw-flip-window)) 
  :config
  ;;  (ace-window-posframe-mode)  ; FIXME: Posframe is very slow at the first time
  (advice-add 'other-window 
              :before (lambda 
                        (&rest 
                         _) 
                        (aw--push-window (selected-window)))) 
  (advice-add 'winum-select-window-by-number 
              :before (lambda 
                        (&rest 
                         _) 
                        (aw--push-window (selected-window)))))

;; ===================================
;; Basic Customization
;; ===================================
;; ***********************************
;; Custom configuration set by Emacs

;; Derek Tylor
(delete-selection-mode 1)
;; (setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't)     ;; scroll window under mouse

;; https://github.com/KaratasFurkan/.emacs.d#lsp-pyright
;; NOTE: I use F1 as C-h (paging & help).
(bind-keys* :prefix-map fk/menu-map 
            :prefix "M-m" ("M-m" . which-key-show-full-major-mode) 
            ("M-h" . help-command) 
            :map fk/menu-map 
            :prefix-map buffers         
            :prefix "b" 
            :map fk/menu-map 
            :prefix-map comments        
            :prefix "c" 
            :map fk/menu-map 
            :prefix-map django          
            :prefix "d" 
            :map fk/menu-map 
            :prefix-map errors          
            :prefix "e" 
            :map fk/menu-map 
            :prefix-map files           
            :prefix "f" 
            :map fk/menu-map 
            :prefix-map org             
            :prefix "o" 
            :map fk/menu-map 
            :prefix-map text            
            :prefix "t" 
            :map fk/menu-map 
            :prefix-map version-control 
            :prefix "v" 
            :map fk/menu-map 
            :prefix-map windows         
            :prefix "w")

;; *************************
;; User-Defined init.el ends here


(use-package 
  flycheck 
  :ensure t 
  :init (global-flycheck-mode)
  :config
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-python-mypy-config "~/.config/mypy.ini")
  (setq-default flycheck-disabled-checkers '(python-pylint))
  )

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; custom path to org init.el
;; jak customizations
(add-to-list 'load-path "~/.emacs.d/jak/")
;; for custom *.el packages not available on melpa
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'use-package)

(use-package 
  diff-hl 
  :ensure t)


;; custom
(require 'init-python)
(require 'init-ansible)
(require 'init-org-mode)
(require 'init-org-agenda)
(require 'init-projectile)
(require 'init-ui)
(require 'init-company)
(require 'init-modeline)
(require 'init-helm)
(require 'init-lsp)
(require 'init-javascript)
(require 'init-typescript)
(require 'init-web-mode)
(require 'init-prodigy)
(require 'init-rest)
(require 'init-yasnippet)
;;(require 'init-treemacs)
;;(require 'init-exwm)
(require 'init-documents-viewer)
;; (require 'init-email)
;; mu4j package is updated but debian package is not updated
;; but the evil collection is updated
(require 'init-email2)
(require 'init-dired)
(require 'init-terminal)
(require 'init-django)
(require 'init-vc)
(require 'init-games)
(require 'init-music)
(require 'init-docker)
(require 'init-kubernetes)
(require 'init-shell)
(require 'init-vue)
(require 'init-search)
(require 'init-evil-mode)
(require 'init-documentations)
;;(require 'init-god-mode)
(require 'init-export)
(require 'init-files)
(require 'init-custom-shortcuts)
(require 'init-org-roam)
(require 'init-external-programs)
(require 'init-fonts)
(require 'init-yml)
(require 'init-languages)
(require 'dired+)

;;emmet

(use-package 
  emmet-mode 
  :ensure t 
  :defer t 
  :init (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook 'emmet-mode)        ;; enable Emmet's css abbreviation.
  )

;; wakatime
;; (use-package wakatime-mode
;;   :ensure t
;;   :config
;;   (global-wakatime-mode)
;;   (custom-set-variables
;;    ’(wakatime-api-key "8dec9208-11ba-4b28-8cf7-6ee88db7d499"))
;;   )




;; default brower
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome")

(with-eval-after-load 'eww (custom-set-variables '(eww-search-prefix
                                                   "https://www.google.com/search?hl=en&q=")))

(use-package 
  engine-mode 
  :ensure t 
  :config (engine/set-keymap-prefix (kbd "C-SPC C-s")) 
  (engine-mode t) 
  (defengine google "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
             ;;    :browser 'eww-browse-url)
             ;;    :browser 'google-chrome
             :keybinding "C-m") 
  (defengine github "https://github.com/search?ref=simplesearch&q=%s" 
             :keybinding "C-h")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s" 
             :keybinding "C-s") 
  (defengine pocket "https://getpocket.com/my-list/search?query=%s" 
             :keybinding "C-p") 
  (defengine evernote "https://www.evernote.com/client/web#?query=%s" 
             :keybinding "C-e") 
  (defengine dictionary "https://www.dictionary.com/browse/%s" 
             :keybinding "C-d"))

;; ;; webjump-plus config
;; (require 'webjump-plus)
;; ;; ref: https://www.neilvandyke.org/webjump/webjump-plus.el
;; (defun webjump-to-evernote (name)
;;   (let
;;       ((search  (webjump-read-string (concat name " search"))))
;;     (if (or search)
;;         (concat
;;          "https://www.evernote.com/client/web"
;;          "#?query="
;;          search
;;          )
;;          "https://www.evernote.com/client/web"))
;;   )

;; (setq webjump-sites
;;       (append '(

;;                 ("Google" .
;;                  [simple-query "www.google.com" "https://www.google.com/search?hl=en&q=" ""] )
;;                 ("Evernote" . webjump-to-evernote)
;;                 )
;;               )
;;       )

;; storing back and autosave files in temp directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq auto-save-visited-file-name t)

;; replace "yes" and "no" with "y" and "n"
(fset 'yes-or-no-p 'y-or-n-p)

;; enable modes

;; auto reload externally edited files
(global-auto-revert-mode t)

;; enable git-gutter mode
(global-git-gutter-mode +1)
(custom-set-variables '(git-gutter:handled-backends '(git hg)))

;; tabs are 4 spaces
(setq-default tab-width 4 indent-tabs-mode nil)

(use-package 
  duplicate-thing 
  :ensure t)

;; smartparens
(use-package 
  smartparens 
  :diminish smartparens-mode 
  :config (progn 
            (require 'smartparens-config) 
            (smartparens-global-mode 1) 
            (show-paren-mode t)))


;; If there are no archived package contents, refresh them
(when (not package-archive-contents) 
  (package-refresh-contents))

;; next and previous buffers


;; stackexchange
;; (use-package sx
;;   :ensure t
;;   :config
;;   (bind-keys :prefix "C-c s"
;;              :prefix-map my-sx-map
;;              :prefix-docstring "Global keymap for SX."
;;              ("q" . sx-tab-all-questions)
;;              ("i" . sx-inbox)
;;              ("o" . sx-open-link)
;;              ("u" . sx-tab-unanswered-my-tags)
;;              ("a" . sx-ask)
;;              ("s" . sx-search)))

;; prodigy to manage external services
(use-package 
  prodigy 
  :ensure t)

;;aggresive indent mode
;; (use-package aggressive-indent
;;   :ensure t
;;   )
;; (global-aggressive-indent-mode 1)



;; drag stuff
(use-package 
  drag-stuff 
  :ensure t 
  :config (drag-stuff-global-mode 1))

;; expand region
;; (use-package 
;;   expand-region 
;;   :ensure t 
;;   :config (global-set-key (kbd "C-=") 'er/expand-region) 
;;   (global-set-key (kbd "C--") 'er/contract-region))

;; restart emacs
(use-package 
  restart-emacs 
  :ensure t)

;; rest client
(use-package 
  restclient 
  :ensure t 
  :config
  ( progn 
    (use-package 
      ob-restclient 
      :ensure) 
    (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t)))))

;; org-trello
;; (use-package org-trello
;;   :ensure t
;;   )

(use-package 
  speed-type 
  :ensure t)

;; http://www.dr-qubit.org/undo-tree/undo-tree.txt
;; (use-package undo-tree
;;   :ensure t
;;   :diminish undo-tree-mode
;;   :bind ("C-x C-/" . undo-tree-visualize)
;;   :bind ("C-/" . undo-tree-undo)
;;   :bind ("C-M-/" . undo-tree-redo)
;;   :config
;;   (global-undo-tree-mode t))

(use-package 
  undo-tree 
  :ensure t 
  :after evil 
  :diminish 
  :config
  (evil-set-undo-system 'undo-tree) 
  (global-undo-tree-mode 1)
  ;; (setq undo-tree-history-directory-alist (concat user-emacs-directory "undo-tree"))
  (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo-tree")))
  ;;   https://emacs.stackexchange.com/questions/69779/emacs-get-frozen-due-to-error-reading-undo-tree-history
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-auto-save-history nil)
  ;; https://www.reddit.com/r/emacs/comments/tejte0/undotree_bug_undotree_files_scattering_everywhere/
  )

(defun clean-undo-tree ()
  (interactive)
  ;; (setq buffer-undo-tree nil))
  (undo-tree-discard-history)
  )
;; (global-set-key [(control c) u] 'clean-undo-tree)

(use-package 
  paren 
  :straight (:type built-in) 
  :custom (show-paren-when-point-inside-paren t) 
  :custom-face (show-paren-match ((t 
                                   (:background nil 
                                                :weight bold 
                                                :foreground "white")))) 
  :hook (dashboard-after-initialize . show-paren-mode))


(use-package 
  multiple-cursors 
  :ensure t 
  :bind (("C-|" . mc/edit-lines) 
         ("C->" . mc/mark-next-like-this) 
         ("C-<" . mc/mark-previous-like-this) 
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))


(use-package 
  anzu 
  :ensure t 
  :diminish anzu-mode 
  :bind (("M-%" . anzu-query-replace) 
         ("C-M-%" . anzu-query-replace-regexp)) 
  :config (global-anzu-mode t))

(when (fboundp 'winner-mode) 
  (winner-mode 1))
(global-set-key (kbd "C-c <C-left>") 'winner-undo)
(global-set-key (kbd "C-c <C-right>") 'winner-redo)


(use-package 
  rainbow-delimiters 
  :ensure t 
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package 
  rainbow-mode 
  :ensure t 
  :hook (prog-mode . rainbow-mode) 
  (html-mode . rainbow-mode) 
  (css-mode . rainbow-mode))


;; (use-package 
  ;; format-all 
  ;; :ensure t)


(use-package 
  auto-highlight-symbol 
  :ensure t 
  :config (global-auto-highlight-symbol-mode t))

;; groovy

(use-package 
  groovy-mode 

  :init 
  :mode (("\\.groovy$" . groovy-mode)))


;; ascii doc

(use-package 
  adoc-mode 
  :ensure t 
  :config (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode)))

(use-package 
  highlight-indent-guides 
  :ensure t 
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode) 
  (setq highlight-indent-guides-method 'bitmap))
;; to avoid breaking server with # files
;;https://github.com/facebook/create-react-app/issues/9056#issuecomment-633540572
(setq create-lockfiles nil)
(setq backup-directory-alist `(("." . "~/.saves")))


;; Save emacs sessions https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html#Saving-Empacs-Sessions
;; (setq desktop-change-dir "/mnt/data/Dropbox/emacs/emacs_sessions/")
;; (desktop-save-mode 1)

(use-package 
  typit 
  :ensure t)

(use-package 
  org-cliplink 
  :ensure t 
  :config (global-set-key (kbd "C-x p i") 'org-cliplink))

(use-package 
  dumb-jump 
  :ensure t 
  :bind (("M-g o" . dumb-jump-go-other-window) 
         ("M-g j" . dumb-jump-go) 
         ("M-g b" . dumb-jump-back)
         ;; ("M-." . dumb-jump-go)
         ;; ("M-," . dumb-jump-back)
         ("M-g x" . dumb-jump-go-prefer-external) 
         ("M-g z" . dumb-jump-go-prefer-external-other-window) 
         ("M-g q" . dumb-jump-quick-look)) 
  :config
  ;;(setq dumb-jump-selector 'ivy)
  (setq dumb-jump-selector 'helm)
  ;; xref configs
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate) 
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) 
  (setq dumb-jump-force-searcher 'rg) 
  :init (dumb-jump-mode))

;; https://github.com/jacktasia/dumb-jump

;; to use M-. to jump
;;
;;(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;;(setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
;;(add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)

;; origami for code folding
;; copied origami configs from
;; https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-origami.el
;; (use-package origami
;;   :ensure t
;;   :bind
;;   ("C-c h o" . hydra-origami/body)
;;   ("<f12>" . origami-recursively-toggle-node)
;;   ("M-<f12>" . origami-close-all-nodes)
;;   ;; Shift(S-)
;;   ("S-<f12>" . origami-open-all-nodes)
;;   :config
;;   (defhydra hydra-origami (:color red
;;                                   :hint nil)
;;     "
;; _t_: toggle    _r_: redo    _p_: prev        _c_: close all
;; _u_: undo      _n_: next    _o_: open all    _q_: quit
;; "
;;     ("t" origami-recursively-toggle-node)
;;     ("u" origami-undo)
;;     ("r" origami-redo)
;;     ("p" origami-previous-fold)
;;     ("n" origami-next-fold)
;;     ("o" origami-open-all-nodes)
;;     ("c" origami-close-all-nodes)
;;     ("q" nil "Quit" :color blue))

;;   (global-origami-mode))

;; enalbe hide-show mode globally
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; (global-set-key (kbd "<f12>") 'hs-toggle-hiding)
;; (global-set-key (kbd "S-<f12>") 'hs-show-all)
;; (global-set-key (kbd "M-<f12>") 'hs-hide-all)

(use-package 
  visual-regexp 
  :ensure t)
(use-package 
  visual-regexp-steroids 
  :ensure t 
  :config (define-key global-map (kbd "C-c r") 'vr/replace) 
  (define-key global-map (kbd "C-c q") 'vr/query-replace)
  ;; if you use multiple-cursors, this is for you:
  (define-key global-map (kbd "C-c g") 'vr/mc-mark))
(require 'visual-regexp)

(setq bookmark-default-file (concat dropbox-path "emacs/bookmarks"))


;; https://github.com/KaratasFurkan/.emacs.d

(use-package 
  helpful 
  :ensure t 
  :bind (([remap describe-function] . helpful-callable) 
         ([remap describe-variable] . helpful-variable) 
         ([remap describe-key] . helpful-key) 
         :map emacs-lisp-mode-map ("C-c C-d" . helpful-at-point)))

;; read-only files will be writable but if you attempt to save your modifications, emacs will ask root user’s password if needed.
(use-package 
  su 
  :ensure t 
  :straight (:host github 
                   :repo "PythonNut/su.el") 
  :config (su-mode))

;; pomodoro
(use-package 
  pomidor 
  :bind (("<f7>" . pomidor)) 
  :ensure t 
  :commands pomidor 
  :custom (pomidor-sound-tick nil) 
  (pomidor-sound-tack nil)
  ;; (pomidor-save-session-file (expand-file-name "pomidor-session.json" no-littering-var-directory))
  :custom-face (pomidor-work-face ((t 
                                    (:inherit success 
                                              :width ultra-condensed)))) 
  (pomidor-overwork-face ((t 
                           (:inherit warning 
                                     :width ultra-condensed)))) 
  (pomidor-break-face ((t 
                        (:inherit font-lock-keyword-face 
                                  :width ultra-condensed)))) 
  (pomidor-skip-face ((t 
                       (:inherit font-lock-comment-face 
                                 :width ultra-condensed)))) 
  :hook (kill-emacs . fk/pomidor-save-session) 
  :config (setq pomidor-seconds (* 45 60)) ; 25 minutes for the work period
  (setq pomidor-break-seconds (* 8 60))    ; 5 minutes break time
  (defun fk/pomidor-save-session () 
    "Call `pomidor-save-session' if pomidor is active, without asking yes or no." 
    (interactive) 
    (when (and 
           (featurep 'pomidor) 
           (get-buffer pomidor-buffer-name)) 
      (cl-letf (((symbol-function 'y-or-n-p) 
                 (lambda (_) t))) 
        (pomidor-save-session)))))


(use-package 
  feature-mode 
  :ensure t 
  :config (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))


;; (use-package gitignore-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist
;;                (cons "/.dockerignore\\'" 'gitignore-mode))
;;   (add-to-list 'auto-mode-alist
;;                (cons "/.gitignore\\'" 'gitignore-mode))
;;   (add-to-list 'auto-mode-alist
;;                (cons "/.gitconfig\\'" 'gitignore-mode))
;;   (add-to-list 'auto-mode-alist
;;                (cons "/.gitattributes\\'" 'gitignore-mode)))


(use-package 
  haskell-mode 
  :ensure t 
  :config (add-to-list 'auto-mode-alist '("\.hs$" . haskell-mode)))


;; (use-package 
;;   magit-todos 
;;   :ensure t 
;;   :commands helm-magit-todos 
;;   :custom (magit-todos-ignored-keywords '("DONE")) 
;;   (magit-todos-exclude-globs '("*jquery*.js" "*min.js" "*min.css")) 
;;   (magit-todos-max-items 30) 
;;   (magit-todos-auto-group-items 30) 
;;   :bind* ( :map version-control ("t" . helm-magit-todos)) 
;;   :hook (magit-mode . magit-todos-mode))



(use-package 
  json-mode 
  :ensure t 
  :defer t 
  :config
  (add-hook 'json-mode-hook (lambda () 
                                      (set (make-local-variable 'company-backends) 
                                           '(( company-files)))))
  (define-key json-mode-map (kbd "C-c C-k") nil)
  )

(setq delete-old-versions t)


;; tramp speedup
;; from here https://emacs.stackexchange.com/questions/17543/tramp-mode-is-much-slower-than-using-terminal-to-ssh
(setq projectile-mode-line "Projectile")

(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))
(setq tramp-verbose 1)


;; *** From  David Wilson personal configurations ***
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org#auto-reverting-changed-files

;; (use-package 
;;   super-save 
;;   :ensure t
;;   ;;:defer 1
;;   ;;:diminish super-save-mode
;;   ;; add integration with ace-window
;;   ;; save on find-file
;;   :config (super-save-mode +1) 
;;   (setq super-save-auto-save-when-idle t) 
;;   (setq super-save-remote-files nil) 
;;   (setq super-save-exclude '(".gpg")))

;; (require 'super-save)
;; (add-to-list 'super-save-triggers 'ace-window)
;; (add-to-list 'super-save-hook-triggers 'find-file-hook)
;; ;; copied from https://github.com/bbatsov/super-save/issues/7
;; (add-to-list 'super-save-triggers 'evil-window-next)
;; (add-to-list 'super-save-triggers 'evil-window-prev)
;; (add-to-list 'super-save-triggers 'next-buffer)
;; (add-to-list 'super-save-triggers 'previous-buffer)
;; (add-to-list 'super-save-triggers 'switch-to-buffer)
;; (add-to-list 'super-save-triggers 'other-window)
;; (add-to-list 'super-save-triggers 'windmove-up)
;; (add-to-list 'super-save-triggers 'windmove-down)
;; (add-to-list 'super-save-triggers 'windmove-left)
;; (add-to-list 'super-save-triggers 'windmove-right)
;; (add-to-list 'super-save-triggers '+workspace/switch-left)
;; (add-to-list 'super-save-triggers '+workspace/switch-right)
;; (add-to-list 'super-save-triggers '+workspace/switch-to-final)
;; (add-to-list 'super-save-triggers '+workspace/switch-to)
;; (add-to-list 'super-save-triggers '+ivy/projectile-find-file)
;; (add-to-list 'super-save-triggers 'magit-status)



;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)


;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/") url-history-file (expand-file-name
                                                                                  "url/history"
                                                                                  user-emacs-directory))


;; Keep .emacs.d Clean
;; Use no-littering to automatically set common paths to the new user-emacs-directory
;; (use-package no-littering
;;   :ensure t
;;   )

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)
;; ;; Set the right directory to store the native comp cache
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
;; Silence compiler warnings as they can be pretty disruptive
;; Derek Tylor
(if (boundp 'comp-deferred-compilation) 
    (setq comp-deferred-compilation nil) 
  (setq native-comp-deferred-compilation nil))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; credential management
;;  sudo apt-get install pass
;; wrapper for pass
;; how to configure:
;; https://www.evernote.com/shard/s226/nl/25312899/e24a5e04-69a5-af51-1a6f-4a1285e18bcb?title=passwordstorage,%20pass,%20debian%20password%20manager
(use-package 
  password-store 
  :ensure t 
  :config (use-package 
            password-store-otp 
            :ensure t) 
  (setq password-store-password-length 12))

(use-package 
  auth-source-pass 
  :ensure t 
  :config (auth-source-pass-enable))


(use-package 
  daemons 
  :ensure t 
  :commands daemons)


;;Automatically clean whitespace
;; (use-package ws-butler
;;   :ensure t
;;   :hook ((text-mode . ws-butler-mode)
;;          (prog-mode . ws-butler-mode)))


;; code folding
;; Clean code folding via Outline minor mode.

;; (add-hook 'prog-mode-hook 'outline-minor-mode)
;; (add-hook 'text-mode-hook 'outline-minor-mode)

;; Show all headings but no content in Outline mode.
;; (add-hook 'outline-minor-mode-hook
;;           (defun baba/outline-overview ()
;;             "Show only outline headings."
;;             (outline-show-all)
;;             (outline-hide-body)))

;; Tab to "zoom in" on a function, backtab to "zoom out" to the outline.
;; (global-set-key 'normal (kbd "<tab>") 'outline-show-entry)
;; (global-set-key 'normal (kbd "<backtab>") 'outline-hide-body)

;; Customize mode-specific Outline folding for python to hide for, ifelse, etc.
;; (add-hook 'python-mode-hook
;;           (defun baba/outline-python ()
;;             "Fold only definitions in Python."
;;             (setq outline-regexp
;;                   (rx (or
;;                        ;; Definitions
;;                        (group (group (* space)) bow (or "class" "def") eow)

;;                        ;; Decorators
;;                        (group (group (* space)) "@"))))
;;             (baba/outline-overview)))


;; supress warning
;; ref: https://stackoverflow.com/a/23752552/5305401
;; not a good idea
;; (setq warning-minimum-level :emergency)

(use-package 
  outline 
  :ensure nil                           ; built-in
  :config (setq outline-blank-line t) 
  ;; leave unhidden blank line before heading
  )

(use-package outline-minor-faces
  :ensure t
  :after outline
  :config (add-hook 'outline-minor-mode-hook
		      #'outline-minor-faces-mode))


;; Customize the distracting folding markers.
;; replace ... with +
(set-display-table-slot
   standard-display-table
   'selective-display
   (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
     (vconcat (mapcar (lambda (c) (+ face-offset c)) " +"))))

(use-package 
  bicycle 
  :ensure t 
  :after outline 
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle) 
              ([C-iso-lefttab] . bicycle-cycle-global)))

(use-package 
  prog-mode 
  :config (add-hook 'prog-mode-hook 'outline-minor-mode) 
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (progn
;;     (key-chord-mode 1)
;;     (key-chord-define-global ",ss" 'save-buffer)
;;     (key-chord-define-global ",xm" 'helm-M-x))
;;   )
;; reset to default font, hack - jak,
(fk/adjust-font-size 0)

;; auto update buffer when file changes
(global-auto-revert-mode)
;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
;;; init.el ends here


(use-package editorconfig 
  :ensure t 
  :config
  (eval-after-load 'editorconfig-mode (load-library "editorconfig"))
  (define-key editorconfig-conf-mode-map (kbd "C-c C-j") nil)
  (progn
    (editorconfig-mode 1)
    (add-hook 'editorconfig-after-apply-functions (lambda (props)
                                                    (setq web-mode-block-padding 0)))
    ;; to make previous and next buffer movement compatible
    ))

(defun copy-current-file-name-on-clipboard () 
  "Put the current file name on the clipboard" 
  (interactive) 
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))) 
    (when filename (with-temp-buffer (insert filename) 
                                     (clipboard-kill-region (point-min) 
                                                            (point-max))) 
          (message filename))))


;; remove universal argument keybinding
;; (define-key universal-argument-map (kbd "C-u") nil)
(global-set-key (kbd "C-u") nil)

;; (define-key universal-argument-map "\C-SPC u" 'universal-argument-more)

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; to ignore white spaces
;; (setq ediff-diff-options "-w")

;; ref: https://systemcrafters.net/emacs-from-scratch/the-best-default-settings/
;; Remember and restore the last cursor location of opened files
(save-place-mode 1)
;; Move customization variables to a separate file and load it

;; (setq custom-file (locate-user-emacs-file "custom-vars.el"))
;; (setq custom-file "/home/jaavedkhan/.emacs.d/emacs-custom.el")

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)
;; (customize-save-variable 'custom-file "/home/jaavedkhan/.emacs.d/emacs-custom.el")
;; (load custom-file)
;; (load custom-file 'noerror 'nomessage)
;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(setq dired-dwim-target t)

;; wakatime-cli installation
;; python3 -c "$(wget -q -O - https://raw.githubusercontent.com/wakatime/vim-wakatime/master/scripts/install_cli.py)"
(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode)
  )

(use-package tldr
  :ensure t)

(use-package imenu-list
  :ensure t)

;; Only show the tab bar if there are 2 or more tabs
(setq tab-bar-show 1)

(defun my/switch-to-tab-buffer ()
  (interactive)
  (if (project-current)
      (call-interactively #'project-switch-to-buffer)
    (call-interactively #'switch-to-buffer)))

;; Turn on tab bar mode after startup
(tab-bar-mode 1)

;; disalbe linenumber in the modeline because it is redundant, left side bar
(line-number-mode 0)
;; Save the desktop session
;; (desktop-save-mode 1)

(defun load-file-if-exists (path)
  (if (file-exists-p path)
      (load-file path)))


;; (server-start) to make the current emacs run as server or daemon for emacsclient
(load "server")
(unless (server-running-p) (server-start))

;; to remove helm buffers inactive selection from helm previous and next buffers
(defun my-buffer-predicate (buffer)
  (if (string-match "helm" (buffer-name buffer))
      nil
    t))

(set-frame-parameter nil 'buffer-predicate 'my-buffer-predicate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Do any initialization that's specific to this machine
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file-if-exists "~/.emacs.d/local-init.el")
(put 'dired-find-alternate-file 'disabled nil)
