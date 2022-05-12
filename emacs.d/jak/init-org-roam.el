(setq org-roam-v2-ack t)

;;(setq org-roam-directory  '/home/jaavedkhan/Dropbox/emacs/org_files/org_roam/files)
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory  (file-truename(concat dropbox-path  "emacs/org_files/org_roam/files/")))
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "meeting" entry
      #'org-roam-capture--get-point
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :file-name "Journal/%<%Y-%m-%d>"
      :olp ("Log")
      :head "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ;; Dailies
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-file-exclude-regexp "org-roam/data/")
  (org-roam-setup)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-location
      (concat org-roam-directory "/.database/org-roam.db"))
  )

;; setting up of capture templates not working in :config
(setq org-roam-capture-templates
      '(("d" "default" plain "%?" :if-new
         (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("p" "Permenant Note" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE:${title}\n#+filetags:permanent\n")
         :unnarrowed t)
        ("f" "Fleeting Note" plain "%?"
         :if-new (file+head "fleeting/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+filetags:fleeting\n")
         :unnarrowed t)
        ("9" "Project Note" plain "%?"
         :if-new (file+head "permenant/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+filetags:project\n")
         :unnarrowed t)
        ("i" "Idea" plain "%?"
         :if-new (file+head "permanent/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: %<%Y%m%d-%H%M%S>--${title}\n#+roam_tags:idea\n#+filetags:idea\n")
         :unnarrowed t)
        ("m" "Meeting Note" plain "%?"
         :if-new (
                  file+head "meetings/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+roam_tags:meeting\n#+filetags:meeting\n* Summary\n** Held at <%<%Y-%m-%d %a>>")
         :unnarrowed t)
        ("h" "Person" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+roam_tags:person\n#+filetags:person\n")
         :unnarrowed t)
        ("o" "Organization" plain "%?"
         :if-new (file+head "permanent/${slug}--%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}\n#+roam_tags:organization\n#+filetags:organization\n")
         :unnarrowed t)
        ("w" "Web" plain "%?"
         :if-new (file+head "internet/%<%Y%m%d-%H%M%S>.org"
                            "#+TITLE: ${title}--%<%Y%m%d-%H%M%S>\n#+filetags:web\n")
         :unnarrowed t)
        ))

;; (server-start) to make the current emacs run as server or daemon for emacsclient
(require 'org-protocol)
(require 'org-roam-protocol)

(setq org-roam-capture-ref-templates
      ;; '(("i" "internet" plain #'org-roam-capture--get-point "%?"
      ;;    :file-name "internet/%<%Y%m%d%H%M>-${slug}"
      ;;    :unnarrowed t))
      '(("i" "internet" plain "%?" :if-new
         (file+head "internet/%<%Y%m%d%H%M>-${slug}.org" "#+title: ${title}\n#+roam_key: ${ref}\n#+filetags: :bookmark:\n ${body}")
         :unnarrowed t)
        ("r" "ref" plain "%?" :if-new
         (file+head "${slug}.org" "#+title: ${title}")
         :unnarrowed t)))


;; (use-package org-roam-ui
;;   :straight
;;   (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;;   :after org-roam
;;   ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;   ;;         a hookable mode anymore, you're advised to pick something yourself
;;   ;;         if you don't care about startup time, use
;;   ;;  :hook (after-init . org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

(use-package org-roam-ui
  :ensure t)

(use-package org-roam-timestamps
  :ensure t)

;; org-protocol
(require 'org-protocol)
;; to enable org-protocol for global shortcuts
(require 'server)
(if (not (server-running-p)) (server-start))
;; (setq server-socket-dir "~/.emacs.d/server")
;; (setq server-name "emacs-26")


;; sudo apt install gawk
;; (use-package org-fc
;;   :straight
;;   (org-fc
;;    :type git :repo "https://git.sr.ht/~l3kn/org-fc"
;;    :files (:defaults "awk" "demo.org"))
;;   :custom
;;   (org-fc-directories (list (concat dropbox-path "emacs/org_files")))
;;   :config
;;   (require 'org-fc-hydra)
;;   (global-set-key (kbd "C-c f") 'org-fc-hydra/org-fc-update)
;;   )

;;(global-set-key (kbd "<f4>") 'org-roam-capture)

(setq org-roam-dailies-capture-templates
      '(
        ("d" "default" entry "* %?" :if-new
         (file+head "%<%Y-%m-%d>.org"
                    "#+title: %<%Y-%m-%d>\n#+filetags: %<:%Y:%B:>"))
        ))

(provide 'init-org-roam)
