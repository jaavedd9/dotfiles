;; openwith
(use-package openwith
  :ensure t
  )

(require 'openwith)

(setq openwith-associations
      (list
       (list (openwith-make-extension-regexp
              '("mpg" "mpeg" "mp3" "mp4"
                "avi" "wmv" "wav" "mov" "flv"
                "ogm" "ogg" "mkv" "webm"))
             ;; "mpv"
             "vlc"
       ;;       '(file))
       ;; (list (openwith-make-extension-regexp
       ;;        '("xbm" "pbm" "pgm" "ppm" "pnm"
       ;;          "png" "gif" "bmp" "tif" "jpeg" "jpg" "webp"))
       ;;       "feh"
             ;; "nsxiv"
             '(file))
       (list (openwith-make-extension-regexp
              '("xlsx"))
             "libreoffice"
             '(file))
       ))

(openwith-mode 1)

;; (use-package 
;;   openwith 
;;   :ensure t 
;;   :config
;;   (setq openwith-associations (list (list (openwith-make-extension-regexp '("mpg" "mpeg"
;;                                                                                     "mp3" "mp4"
;;                                                                                     "avi" "wmv"
;;                                                                                     "wav" "mov"
;;                                                                                     "flv" "ogm"
;;                                                                                     "ogg" "mkv"))
;;                                                   "mpv" '(file)) 
;;                                             (list (openwith-make-extension-regexp '("xbm" "pbm"
;;                                                                                     "pgm" "ppm"
;;                                                                                     "pnm" "png"
;;                                                                                     "gif" "bmp"
;;                                                                                     "tif" "jpeg")) ;; Removed jpg because Telega was
;;                                                   ;; causing feh to be opened...
;;                                                   "feh" '(file)) 
;;                                             (list (openwith-make-extension-regexp '("pdf"))
;;                                                   "zathura" '(file)))))
(provide 'init-external-programs)
