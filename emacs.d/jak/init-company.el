;;; init-company.el -- Configures company-mode autocompletion.
;;; Commentary:
;;; Code:
;; (use-package company-capf
;;   :ensure t
;;   )

(use-package company-bootstrap
  :straight (:host github :repo "ehsud/company-bootstrap")
  :ensure t
  )

;; (use-package company-tern
;;   :straight (:host github :repo "kevinushey/company-tern")
;;   :ensure t
;;   )
;;(require 'company-bootstrap)
(use-package company
  ;;  :diminish company-mode
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.100 ;; How long to wait before popping up
        company-minimum-prefix-length 1 ;; Show the menu after one key press
        company-tooltip-limit 10 ;; Limit on how many options to display
        company-show-numbers t   ;; Show numbers behind options
        company-tooltip-align-annotations t ;; Align annotations to the right
        company-require-match nil           ;; Allow free typing
        company-selection-wrap-around t ;; Wrap around to beginning when you hit bottom of suggestions
        company-dabbrev-ignore-case t ;; Don't ignore case when completing
        company-dabbrev-downcase t ;; Don't automatically downcase completions
        )
  ;; set default `company-backends'
  (setq company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-capf)  ; completion-at-point-functions
          (company-abbrev company-dabbrev)
          ))
  ;; (setq company-minimum-prefix-length 1)
  ;; (setq company-idle-delay 0)
  ;; (setq company-echo-delay 0)
  ;; (setq company-require-match nil)
  ;; ;;   (setq company-selection-wrap-around t)
  ;; (setq company-tooltip-align-annotations t)
  ;; (setq company-tooltip-flip-when-above t)
  ;; (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency


  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(company-capf  company-keywords company-yasnippet) ;;grouped backends will be called at once
                           )))

  (add-hook 'ansible-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-ansible)))


  (add-hook 'web-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(
                             ;;company-web-html company-bootstrap company-etags  company-capf company-ctags  company-yasnippet company-files
                             company-web-html company-tide company-capf company-files company-bootstrap
                             ;;company-web-html company-bootstrap
                             ))))
  ;; (add-hook 'lsp-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list (make-local-variable 'company-backends)
  ;;                          '(
  ;;                            company-web-html company-tide company-capf company-files company-bootstrap

  ;;                            ))))

  ;; (defun baal-setup-lsp-company ()
  ;;   (setq-local company-backends
  ;;               '(company-capf company-dabbrev company-dabbrev-code company-files company-yasnippet)))

  ;; (add-hook 'lsp-completion-mode-hook #'baal-setup-lsp-company)



  (add-hook 'eshell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-shell company-shell-env company-files)))))


  (add-hook 'html-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '(( company-files)))))


  (add-hook 'org-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-capf company-files company-keywords  :with company-yasnippet))))
            )



  (use-package company-prescient
    :ensure t
    :after company
    :config
    (company-prescient-mode))

  (use-package company-statistics
    :ensure t
    :after company
    :init
    (company-statistics-mode))
  (use-package company-web
    :ensure t
    :after company
    )

  (use-package company-try-hard
    :ensure t
    :after company
    :bind
    (("C-," . company-yasnippet)
     ("C-M-," . company-try-hard)
     :map company-active-map
     ;; ("M-<return>" . company-try-hard)
     )
    )

  ;; (use-package company-quickhelp
  ;;   :ensure t
  ;;   :config
  ;;   (company-quickhelp-mode)
  ;;   (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  ;;   )
  )


;;(use-package company-box
;;  :after company
;;  :hook (company-mode . company-box-mode)
;;  :config
;;  (setq company-box-doc-delay 1))


;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; (use-package company-lsp
;;   :ensure t
;;   :after company
;;   :defer t
;;   :commands company-lsp
;;   :preface
;;   (defun push-company-lsp-backends ()
;;     "Push company-lsp to the backends."
;;     (general-pushnew
;;      '(company-lsp
;;        company-files
;;        (company-dabbrev company-dabbrev-code)
;;        company-keywords
;;        company-yasnippet)
;;      company-backends))
;;   (defun company-lsp-init-h ()
;;     "Make sure that `company-capf' is disabled since it is incompatible with
;; `company-lsp' (see lsp-mode#884)."
;;     (if (not (bound-and-true-p company-mode))
;;         (add-hook 'company-mode-hook #'company-lsp-init-h t t)
;;       (setq-local company-backends
;;                   (cons 'company-lsp
;;                         (remq 'company-capf company-backends)))
;;       (remove-hook 'company-mode-hook #'company-lsp-init-h t)))
;;   :hook ((lsp-mode . company-lsp-init-h))
;;   :init
;;   (setq company-lsp-async               t
;;         company-lsp-enable-recompletion t
;;         company-lsp-enable-snippet      t
;;         company-lsp-cache-candidates    'auto))


;; (with-eval-after-load 'company
;;   ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
;;   (defadvice company-in-string-or-comment (around company-in-string-or-comment-hack activate)
;;     (if (memq major-mode '(html-mode web-mode nxml-mode))
;;         (setq ad-return-value nil)
;;       ad-do-it)))


;; (with-eval-after-load 'company-etags '(progn (add-to-list 'company-etags-modes 'web-mode)))
;; (setq company-etags-everywhere '(html-mode web-mode nxml-mode))

(defun jak/company-to-yasnippet ()
  (interactive)
  (company-abort)
  (call-interactively 'company-yasnippet))
(bind-key "<backtab>" 'jak/company-to-yasnippet company-active-map)
(bind-key "<backtab>" 'company-yasnippet)


;; (use-package company-box
;;   :straight (:host github :repo "KaratasFurkan/company-box" :branch "consider-icon-right-margin-for-frame")
;;   :custom
;;   ;; Disable `single-candidate' and `echo-area' frontends
;;   ;;  (company-frontends '(company-box-frontend))
;;   (company-box-show-single-candidate t)
;;   ;;(company-box-frame-behavior 'point)
;;   (company-box-icon-right-margin 0.5)
;;   (company-box-backends-colors '((company-yasnippet . (:annotation default))))
;;   :hook
;;   (company-mode . company-box-mode))


;; for smart company sorts
(use-package prescient
  :ensure t
  :hook (dashboard-after-initialize . prescient-persist-mode))

(use-package company-prescient
  :ensure t
  :after company
  :config (company-prescient-mode))

;; It turns out company-prescient could not be disabled locally, lets go back to
;; company-statistics
;; (use-package company-statistics
;;   :hook (global-company-mode . company-statistics-mode))


;; A company-backend for human language texts based on word frequency dictionaries.
;; https://github.com/johannes-mueller/company-wordfreq.el

(use-package company-wordfreq
  :straight (:host github :repo "johannes-mueller/company-wordfreq.el")
  :commands fk/company-wordfreq-mode
  :custom
  (company-wordfreq-path (concat no-littering-var-directory "wordfreq-dicts"))
  (ispell-local-dictionary "english")
  :config
  (define-minor-mode fk/company-wordfreq-mode
    "Suggest words by frequency."
    :global nil
    (if fk/company-wordfreq-mode
        (progn
          (setq-local company-backends-backup company-backends)
          (setq-local company-transformers-backup company-transformers)
          (setq-local company-backends '(company-wordfreq))
          (setq-local company-transformers nil))
      (setq-local company-backends company-backends-backup)
      (setq-local company-transformers company-transformers-backup)))

  (defun fk/company-wordfreq-toggle-language (&optional language)
    (interactive)
    (setq ispell-local-dictionary (or language
                                      (if (string= ispell-local-dictionary "english")
                                          "turkish"
                                        "english")))
    (message ispell-local-dictionary)))



;; https://oremacs.com/2017/12/27/company-numbers/
;; to bind numbers to select auto completion candidates

(defun ora-company-number ()
  "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
  (interactive)
  (let* ((k (this-command-keys))
         (re (concat "^" company-prefix k)))
    (if (or (cl-find-if (lambda (s) (string-match re s))
                        company-candidates)
            (> (string-to-number k)
               (length company-candidates))
            (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
        (self-insert-command 1)
      (company-complete-number
       (if (equal k "0")
           10
         (string-to-number k))))))

(defun ora--company-good-prefix-p (orig-fn prefix)
  (unless (and (stringp prefix) (string-match-p "\\`[0-9]+\\'" prefix))
    (funcall orig-fn prefix)))
;;(ora-advice-add 'company--good-prefix-p :around #'ora--company-good-prefix-p)

(let ((map company-active-map))
  (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
        (number-sequence 0 9))
  (define-key map " " (lambda ()
                        (interactive)
                        (company-abort)
                        (self-insert-command 1)))
  (define-key map (kbd "<return>") nil))

(provide 'init-company)
;;; init-company.el ends here
