(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(global-set-key (kbd "C-c m") `mu4e)
(global-set-key (kbd "C-c C-m") `mu4e)


(defcustom jak/boss-emails nil
  ""
  :type '(repeat string)
  )

(defcustom jak/people-i-know-at-kfupm  nil
  ""
  :type '(repeat string)
  )

(defcustom jak/ictc-colleagues-emails nil
  ""
  :type '(repeat string)
  )

(defcustom jak/kfupm-colleagues-emails nil
  ""
  :type '(repeat string)
  )

(defun jak/add-to-boss-emails (username)
  (interactive "sBOSS_USERNAME: ")
  (add-to-list 'jak/boss-emails username)
  (customize-save-variable 'jak/boss-emails jak/boss-emails)
  (jak/set-people-i-know-at-kfupm)
  (jak/set-boss-query)
  )

(defun jak/add-to-ictc-colleagues-emails (username)
  (interactive "sUSERNAME: ")
  (add-to-list 'jak/ictc-colleagues-emails username)
  (customize-save-variable 'jak/ictc-colleagues-emails jak/ictc-colleagues-emails)
  (jak/set-people-i-know-at-kfupm)
  (jak/set-ictc-colleagues-query)
  )

(defun jak/add-to-kfupm-collegues-emails (username)
  (interactive "sUSERNAME: ")
  (add-to-list 'jak/kfupm-colleagues-emails username)
  (customize-save-variable 'jak/kfupm-colleagues-emails jak/kfupm-colleagues-emails)
  (jak/set-people-i-know-at-kfupm)
  (message "%s" jak/people-i-know-at-kfupm)
  (jak/set-kfupm-colleagues-query)
  )

(defun jak/set-people-i-know-at-kfupm ()
  (setq jak/people-i-know-at-kfupm (append jak/boss-emails jak/ictc-colleagues-emails jak/kfupm-colleagues-emails))

  (setq jak/people-i-know-query (concat "(maildir:/kfupm/Inbox) AND " "from:" (mapconcat 'identity jak/people-i-know-at-kfupm " OR from:")))
  (jak/set-people-i-know-at-kfupm-query)
  )

(defun jak/set-people-i-know-at-kfupm-query ()
  (setq jak/people-i-know-query (concat "(maildir:/kfupm/Inbox) AND " "from:" (mapconcat 'identity jak/people-i-know-at-kfupm " OR from:")))
  )

(jak/set-people-i-know-at-kfupm-query)

(defun jak/set-kfupm-colleagues-query ()
  (setq jak/kfupm-colleauges-query (concat "(maildir:/kfupm/Inbox) AND " "from:" (mapconcat 'identity jak/kfupm-colleagues-emails " OR from:")))
  )

(defun jak/set-ictc-colleagues-query ()
  (setq jak/ictc-colleauges-query (concat "(maildir:/kfupm/Inbox) AND " "from:" (mapconcat 'identity jak/ictc-colleagues-emails " OR from:")))
  )

(defun jak/set-boss-query ()
  (setq jak/boss-query (concat "(maildir:/kfupm/Inbox) AND " "from:" (mapconcat 'identity jak/boss-emails " OR from:")))
  )

(jak/set-kfupm-colleagues-query)
(jak/set-ictc-colleagues-query)
(jak/set-boss-query)

(use-package mu4e
  :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; Load org-mode integration
  (require 'org-mu4e)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir (concat dropbox-path "emacs/.mail"))
  ;;(setq mu4e-maildir "/mnt/data/Dropbox/emacs/.mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  ;;(setq mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Jaaved"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/gmail-main" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "Jaaved Khan")
                    (user-mail-address . "jaavedkhan@gmail.com")
                    (mu4e-sent-folder . "/gmail-main/Sent Items")
                    (mu4e-trash-folder . "/gmail-main/Trash")
                    (mu4e-drafts-folder . "/gmail-main/Drafts")
                    (mu4e-refile-folder . "/gmail-main/Archive")
                    (mu4e-sent-messages-behavior . sent)
                    ))
          ,(make-mu4e-context
            :name "KFUPM"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/kfupm" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-mail-address . "jaavedkhan@kfupm.edu.sa")
                    (mu4e-sent-folder . "/kfupm/Sent")
                    (mu4e-trash-folder . "/kfupm/Deleted")
                    (mu4e-refile-folder . "/kfupm/Archive")
                    ))
          ,(make-mu4e-context
            :name "Jaaved Dev"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/gmail-dev" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-mail-address . "jaavedd9@gmail.com")
                    (mu4e-sent-folder . "/gmail-dev/Sent Items")
                    (mu4e-trash-folder . "/gmail-dev/Trash")
                    (mu4e-drafts-folder . "/gmail-dev/Drafts")
                    (mu4e-refile-folder . "/gmail-dev/Archive")
                    (mu4e-sent-messages-behavior . sent)
                    ))
          ))
  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Use mu4e for sending e-mail
  (setq mail-user-agent 'mu4e-user-agent
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type  'ssl)

  ;; Signing messages (use mml-secure-sign-pgpmime)
  (setq mml-secure-openpgp-signers '("53C41E6E41AAFE55335ACA5E446A2ED4D940BF14"))

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '(("/gmail-main/Inbox"       . ?i)
          ("//gmail-main/Lists/*"     . ?l)
          ("//gmail-main/Sent Mail"   . ?s)
          ("/Fastmail/Trash"       . ?t)))

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "All Inboxes"
                :query "(maildir:/gmail-main/Inbox OR maildir:/kfupm/Inbox  OR  maildir:/gmail-dev/Inbox) AND flag:N"
                :key ?i))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Inbox"
                :query "(maildir:/kfupm/Inbox) AND flag:N"
                :key ?k))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM All"
                :query "(maildir:/kfupm/Inbox)"
                :key ?a))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "JAK Inbox"
                :query "(maildir:/gmail-main/Inbox) AND flag:N"
                :key ?j))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Dev Inbox"
                :query "(maildir:/gmail-dev/Inbox) AND flag:N"
                :key ?d))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Open Projects"
                :query "(maildir:/kfupm/Inbox) AND from:ictc-projects@kfupm.edu.sa"
                :key ?o))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Code"
                :query "(maildir:/kfupm/Inbox) AND from:code@kfupm.edu.sa"
                :key ?c))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Edesk"
                :query "(maildir:/kfupm/Inbox) AND from:edesk@kfupm.edu.sa"
                :key ?e))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Yesterday"
                :query "(maildir:/kfupm/Inbox) AND date:2d..1d"
                :key ?y))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Last 7 days"
                :query "(maildir:/kupm/Inbox) AND date:7..now"
                :key ?7))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Last 6 days"
                :query "(maildir:/kupm/Inbox) AND date:6..now"
                :key ?6))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Last 5 days"
                :query "(maildir:/kupm/Inbox) AND date:5..now"
                :key ?5))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Last 4 days"
                :query "(maildir:/kfupm/Inbox) AND date:4..now"
                :key ?4))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Last 3 days"
                :query "(maildir:/kfupm/Inbox) AND date:3d..now"
                :key ?3))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Last 2 days"
                :query "(maildir:/kfupm/Inbox) AND date:2d..now"
                :key ?2))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "KFUPM Last 1 days"
                :query "(maildir:/kfupm/Inbox) AND date:1d..now"
                :key ?1))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Tadreeb"
                :query "(maildir:/kfupm/Tadreeb)"
                :key ?~))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Program Specifications"
                :query "(maildir:/kfupm/ProgramSpecifications)"
                :key ?1))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "People I know at KFUPM"
                :query jak/people-i-know-query
                :key ?*))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "Boss"
                :query jak/boss-query
                :key ?+))
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "ICTC Colleagues"
                :query jak/ictc-colleauges-query  
                :key ?-))
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq dw/mu4e-inbox-query
        ;;        "(maildir:/gmail/Inbox OR maildir:/kfupm/Inbox  OR  maildir:/gmail-dev/Inbox) AND flag:unread")
        "(maildir:/gmail/Inbox OR maildir:/kfupm/Inbox  OR  maildir:/gmail-dev/Inbox)")

  (defun dw/go-to-inbox ()
    (interactive)
    (mu4e-headers-search dw/mu4e-inbox-query))

  ;; (dw/leader-key-def
  ;;  "m"  '(:ignore t :which-key "mail")
  ;;  "mm" 'mu4e
  ;;  "mc" 'mu4e-compose-new
  ;;  "mi" 'dw/go-to-inbox
  ;;  "ms" 'mu4e-update-mail-and-index)

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))


;; (use-package mu4e-alert
;;   :ensure t
;;   :after mu4e
;;   :config
;;   ;; Show unread emails from all inboxes
;;   (setq mu4e-alert-interesting-mail-query dw/mu4e-inbox-query)

;;   ;; Show notifications for mails already notified
;;   (setq mu4e-alert-notify-repeated-mails nil)

;;   (mu4e-alert-enable-notifications))


(use-package mu4e-dashboard
  :straight (:host github :repo "rougier/mu4e-dashboard" :branch "main")
  )

;; (use-package mu4e-thread-folding
;;   :straight (:host github :repo "rougier/mu4e-thread-folding" :branch "master")
;;   ;; :config
;;   ;; (setq  nano-color-background 5)
;;   ;; (require 'mu4e-thread-folding)
;;   ;; (add-to-list 'mu4e-header-info-custom
;;   ;;              '(:empty . (:name "Empty"
;;   ;;                                :shortname ""
;;   ;;                                :function (lambda (msg) "  "))))
;;   ;; (setq mu4e-headers-fields '((:empty         .    2)
;;   ;;                             (:human-date    .   12)
;;   ;;                             (:flags         .    6)
;;   ;;                             (:mailing-list  .   10)
;;   ;;                             (:from          .   22)
;;   ;;                             (:subject       .   nil)))
;;   ;; (define-key mu4e-headers-mode-map (kbd "<tab>")     'mu4e-headers-toggle-at-point)
;;   ;; (define-key mu4e-headers-mode-map (kbd "<left>")    'mu4e-headers-fold-at-point)
;;   ;; (define-key mu4e-headers-mode-map (kbd "<S-left>")  'mu4e-headers-fold-all)
;;   ;; (define-key mu4e-headers-mode-map (kbd "<right>")   'mu4e-headers-unfold-at-point)
;;   ;; (define-key mu4e-headers-mode-map (kbd "<S-right>") 'mu4e-headers-unfold-all)
;;   )

(use-package svg-tag-mode
  :straight (:host github :repo "rougier/svg-tag-mode" :branch "main")
  )


(provide 'init-email2)
