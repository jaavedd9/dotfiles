;; Set the screen resolution (update this to be the correct resolution for your screen!)
;; (require 'exwm-randr)
;; (exwm-randr-enable)
;; (start-process-shell-command "xrandr" nil "xrandr --output  --primary --mode 2048x1152 --pos 0x0 --rotate normal")


;; This will need to be updated to the name of a display!  You can find
;; the names of your displays by looking at arandr or the output of xrandr
;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist
;;       '(0 "DisplayPort-0" 1 "DisplayPort-1"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output DisplayPort-0 --output DisplayPort-1 --auto")))

;; (exwm-randr-enable)


;; assignign workspace to the screens
;; reference https://github.com/ch11ng/exwm/issues/325

;; machine specific Home
(setq exwm-randr-workspace-output-plist '(0 "DisplayPort-0" 1 "DisplayPort-0" 2 "DisplayPort-0" 3 "DisplayPort-1" 4 "DisplayPort-1" 5 "DisplayPort-1" 6 "DisplayPort-1")) ;; DisplayPort-1 being the name of my new monitor as shown by `randr' added as extension
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output DisplayPort-1 --left-of DisplayPort-0 --auto"))) ;; eDP-1 being the name of my original (laptop) monitor
;; end machine specific

;; machine specific Office
;; (setq exwm-randr-workspace-output-plist '(0 "DP-1" 1 "DP-1" 2 "DP-1" 3 "eDP-1" 4 "eDP-1" 5 "eDP-1" 6 "eDP-1")) ;; DisplayPort-1 being the name of my new monitor as shown by `randr' added as extension
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              ;;               "xrandr" nil "xrandr --output --primary --mode 3840x2160 DP-1  --left-of eDP-1 --auto"))) ;; eDP-1 being the name of my original (laptop) monitor
;;              "xrandr" nil "xrandr --output DP-1  --left-of eDP-1 --auto"))) ;; eDP-1 being the name of my original (laptop) monitor



;;  (start-process-shell-command "xrandr" nil "xrandr --output  --primary --mode 2048x1152 --pos 0x0 --rotate normal")
;; end machine specific office

;; from documentation
;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(1 "DP-1"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output DP-1 --right-of DP-2 --auto")))
;; (exwm-randr-enable)


(provide 'local-desktop)
