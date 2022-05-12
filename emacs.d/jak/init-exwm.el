;; wallpaper
;; (use-package wallpaper
;;   :ensure t
;;   :hook ((exwm-randr-screen-change . wallpaper-set-wallpaper)
;;          (after-init . wallpaper-cycle-mode))
;;   :custom ((wallpaper-cycle-single t)
;;            (wallpaper-scaling 'scale)
;;            (wallpaper-cycle-interval 45)
;;            (wallpaper-cycle-directory "/mnt/data/Dropbox/emacs/wallpapers")))

;; following this https://github.com/daviwil/emacs-from-scratch/blob/219c060e1bd695948c7691955a12a5dcaf3a9530/show-notes/Emacs-Desktop-01.org
;; for basic exwm window manager

;; apt packages required

;;https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Desktop-05.org

;; for polybar
;; sudo apt -t buster-backports install polybar
;; sudo apt install fonts-font-awesome fonts-material-design-icons-iconfont
;; download fonts-material... from here: https://ubuntu.pkgs.org/20.10/ubuntu-main-armhf/fonts-material-design-icons-iconfont_5.0.1-2_all.deb.html


(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /home/jaavedkhan/Dropbox/emacs/wallpapers/wp2646303-new-wallpaper-downlode.jpg"))

(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Start the Polybar panel
  (efs/start-panel)

  ;; Open eshell by default
  ;;(shell)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background
  (efs/run-in-background "nm-applet")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))


(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox-esr: %s" exwm-title)))))

;; This function isn't currently used, only serves as an example how to
;; position a window
;; (defun efs/position-window ()
;;   (let* ((pos (frame-position))
;;          (pos-x (car pos))
;;          (pos-y (cdr pos)))

;;     (exwm-floating-move (- pos-x) (- pos-y))))

;; To check current app's or buffer's do M-: "exwm-class-name"
;; workspace configs of David Willson(https://www.youtube.com/watch?v=HGGU5Zvljj8)
;; 0 chats 1 code and org mode 2 browsing 3 slack etc

;; confifurations to assign application to a workspace
(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("org.remmina.Remmina" (exwm-workspace-move-window 0))
    ("Firefox-esr" (exwm-workspace-move-window 2))
    ("Google-chrome" (exwm-workspace-move-window 2))
    ("Tilix" (exwm-workspace-move-window 3))
    ("Terminator" (exwm-workspace-move-window 3))
    ("Slack" (exwm-workspace-move-window 4))
    ;;  ("thunderbird" (exwm-workspace-move-window 5))
    ;;    ("Thunar" (exwm-workspace-move-window 6))
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))))


(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)
  
  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; Rebind CapsLock to Ctrl
  ;; (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")
  (require 'exwm-randr)
  (require 'local-desktop)
  (exwm-enable)
  (exwm-randr-enable)
  ;; Set the wallpaper after changing the resolution
  (efs/set-wallpaper)

  ;; Load the system tray before exwm-init
  ;; not required when using polybar
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  ;; (exwm-systemtray-enable)
  ;; These keys should always pass through to Emacs not to the applications that are opened in
  ;; emacs

  ;; required for the counsel-linux-app of exwm
  (use-package counsel
    :ensure t 
    )
  ;; these keys will be sent to the exwm directly - not to the OS or the application

  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j  ;; Buffer list
          ?\C-g
          ?\M-p
          ;; ?s-f
          ;; ?s-y
          ?\C-\ ))  ;; Ctrl+SpaceS
  ;; Ctrl+Q will enable the next key to be sent directly
  ;; simulation keys from https://github.com/wandersoncferreira/dotfiles#exwm-basics
  ;; these keys will be rebinded by emacs when sending to the application opened in emacs
  (exwm-input-set-simulation-keys
   '(([?\C-p] . [up])
     ([?\C-n] . [down])
     ([?\C-f] . [right])
     ([?\C-b] . [left])
     ([?\C-s] . [\C-f])
     ([?\M-w] . [\C-c])
     ([?\C-y] . [\C-v])
     ([?\C-w] . [\C-x])))
  
  (define-key exwm-mode-map [?\C-.] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  ;;  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-SPC") 'helm-run-external-command)
  (exwm-input-set-key (kbd "M-SPC") 'exwm-workspace-switch-to-buffer)
  (exwm-enable))

(global-set-key (kbd "C-x k") 'kill-this-buffer)


;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
;;(setq exwm-workspace-minibuffer-position 'bottom)

(defun efs/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "RDP")
    (1 "")
    (2 "")
    (3 "Terminal")
    (4 "Slack")
    (5 "Email")
    (6 "File")
    )
  )


(use-package desktop-environment
  :ensure t
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")
  (desktop-environment-screenshot-command "flameshot gui")
  )

(use-package helm-exwm
  :ensure t
  :config
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
  (setq helm-exwm-source (helm-exwm-build-source))
  (setq helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))


;; exwm display workspace num
(easy-menu-define exwm-workspace-menu nil
  "Menu for Exwm Workspace.

Also used in `exwm-mode-line-workspace-map'."
  '("Exwm Workspace"
    ["Add workspace" exwm-workspace-add]
    ["Delete current workspace" exwm-workspace-delete]
    ["Move workspace to" exwm-workspace-move]
    ["Swap workspaces" exwm-workspace-swap]
    ["Move X window to" exwm-workspace-move-window]
    ["Move X window from" exwm-workspace-switch-to-buffer]
    ["Toggle minibuffer" exwm-workspace-toggle-minibuffer]
    ["Switch workspace" exwm-workspace-switch]
    ;; Place this entry at bottom to avoid selecting others by accident.
    ("Switch to" :filter
     (lambda (&rest _args)
       (mapcar (lambda (i)
                 `[,(format "workspace %d" i)
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch ,i))
                   (/= ,i exwm-workspace-current-index)])
               (number-sequence 0 (1- (exwm-workspace--count))))))))

(defvar exwm-mode-line-workspace-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'exwm-workspace-switch)
    (define-key map [mode-line mouse-3] exwm-workspace-menu)
    map)
  "Local keymap for EXWM mode line string.  See `exwm-mode-line-format'.")

(defcustom exwm-mode-line-format
  `("["
    (:propertize (:eval (format "WS-%d" exwm-workspace-current-index))
		         local-map ,exwm-mode-line-workspace-map
		         face bold
		         mouse-face mode-line-highlight
		         help-echo "mouse-1: Switch to / add / delete to EXWM workspaces.
mouse-2: EXWM Workspace menu.
")
    "]")
  "EXWM workspace in the mode line."
  :type 'sexp)


;; FIXME: Don't push the value.  Instead push a symbol.  If done, (1)
;; this will avoid duplicate entries for EXWM workspace (2) The mode
;; line string will change in sync with the value of
;; `exwm-mode-line-format'.
(add-to-list 'mode-line-misc-info exwm-mode-line-format t)


(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)

;; starting, other needed, services
;; ref:
;; https://www.reddit.com/r/emacs/comments/7kvwv4/running_shell_commands_from_init_file/
(async-shell-command "copyq")
;;(async-shell-command "python3 /opt/thefanclub/overgrive/overgrive")
;;(start-process "process-name" "buffer-name" "program")
;;(start-process "overgrive" nil "python3 /opt/thefanclub/overgrive/overgrive")

(provide 'init-exwm)
