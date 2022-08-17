(load "init-company.el")

(bind-key "<backtab>" 'jak/company-to-yasnippet company-active-map)
(use-package general
  :ensure t
  :config
  (progn
    (global-unset-key (kbd "C-SPC")) ;; default is for marking
    ;; my user space for personal customizations
    (general-create-definer jak/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")
    )

  (jak/leader-keys
    "4"   '(:ignore t :which-key "email management")
    "4a"   '(:ignore t :which-key "email list add")
    "4ai"  '(jak/add-to-ictc-colleagues-emails :which-key "add ictc emails")
    "4ak"  '(jak/add-to-kfupm-collegues-emails :which-key "add kfupm emails")
    ;; Shell
    "a"   '(:ignore t :which-key "Shell")
    "aa"   '(shell-pop :which-key "shell-pop")
    "as"  '(shell :which-key "Shell")
    "ae"  '(eshell :which-key "e Shell")
    "av"  '(vterm :which-key "vterm")
    "ap"  '(projectile-run-shell :which-key "projectile shell")
    "at"  '(tldr :which-key "tldr")
    ;; Browser
    "b"   '(:ignore t :which-key "Browser")
    "bb"  '(eww-search-words :which-key "eww search words")
    "bp"  '(engine/search-pocket :which-key "Evernote")
    "be"  '(engine/search-evernote :which-key "Evernote")
    "bg"  '(engine/search-google :which-key "Google")
    ;; "bd"  '(engine/search-dictionary :which-key "Dictionary")
    "bd"  '(dictionary-search :which-key "Dictionary")
    "bh"  '(engine/search-github :which-key "Github")
    "bs"  '(engine/search-stack-overflow :which-key "stackoverflow")
    ;; completions
    "c"   '(:ignore t :which-key "completions")
    "ca"   '(company-ansible :which-key "ansible company")
    "cc"   '(company-capf :which-key "generic: completion-at-point-functions backend ")
    "cy"   '(jak/company-to-yasnippet :which-key " yasnippet company")
    "cf"   '(company-files :which-key "files company")
    ;; "cs"   '(company-ispell :which-key "spelling company")
    "cs"   '(flyspell-correct-at-point :which-key "correct at point flyspell")
    "ch"   '(company-try-hard :which-key "try hard company")
    "cd"   '(:ignore t :which-key "completions descriptions")
    "cdy"   '(yas-describe-tables :which-key "yasnippet tables")
    ;; Django
    ;; "d"  '(:ignore t :which-key "django")
    ;; "dm"  '(fk/django-search-models :which-key "Search Model")
    ;; "dv"  '(fk/django-search-views  :which-key "Search View")
    ;; "df"   '(fk/django-search-forms :which-key "Search Forms")
    ;; "ds"   '(fk/django-search-serializers :which-key "Search Serializers")
    ;; "dt"  '(fk/django-search-tests :which-key "Search Tests")
    ;; "dS"  '(fk/django-search-settings :which-key "Search Settings")
    ;; "da"  '(fk/django-search-admins :which-key "Search Admin")
    ;; "dp"  '(fk/django-search-permissions :which-key "Search Permissions")
    ;; "dx"  '(fk/django-search-mixins :which-key "Search mixins")
    ;; "du"  '(fk/django-search-urls :which-key "Search URLs")
    ;; "dc"  '(jak/all-classes :which-key "all classes")
    ;; "db"  '(jak/all-methods :which-key "all methods")
    ;; devdocs meaningful exception for documentation 
    ;; "dd"  '(devdocs-browser-open-in :which-key "devdocs browser open in ")
    "dd"  '(devdocs-lookup :which-key "devdocs lookup")
    "ds"  '(devdocs-peruse :which-key "")
    ;; elisp eval and emacs commands
    "e"   '(:ignore t :which-key "evaluate")
    "eb"   '(eval-buffer :which-key "eval buffer")
    "ef"   '(eval-defun :which-key "eval fun")
    "ek"   '(restart-emacs :which-key "restart emacs")
    "ex"   '(eval-expression :which-key "eval expression")
    "er"   '(eval-region :which-key "eval region")
    "ei"   '(:ignore t :which-key "install")
    "eip"   '(package-install :which-key "package install")
    "eid"   '(:ignore t "install documentation")
    "eids"  '(helm-dash-install-docset :which-key "daSh docs install")
    "eidd"  '(devdocs-browser-install-doc :which-key "devdocs install")
    "eido"  '(devdocs-browser-download-offline-data :which-key "devdocs download offline")
    "et"   '(load-theme :which-key "load theme")
    ;; format and find
    "f"   '(:ignore t :which-key "format")
    ;; ffind
    ;; "ff"  '(projectile-find-file :which-key "find file in project")
    "ff"  '(helm-mini :which-key "list open buffers and recent files")
    "fd"  '(projectile-find-dir :which-key "find directory in project")
    ;; format
    "fa"   '(:ignore t :which-key "ansible")
    "fas"  '(ansible-syntax-check-current-buffer :which-key "syntax check")
    "fal"  '(ansible-lint-current-buffer :which-key "lint")
    "fh"   '(web-beautify-html :which-key "format html")
    "fj"   '(:ignore t :which-key "format/lint javascript")
    "fjb"  '(web-beautify-js :which-key "format javascript")
    "fjl"  '(eslint-current-buffer :which-key "lint: eslint")
    "fc"   '(web-beautify-css :which-key "format css")
    "fe"   '(editorconfig-format-buffer :which-key "editorconfig format")
    "fl"   '(elisp-format-buffer :which-key "elisp format buffer")
    "fp"   '(:ignore t :which-key "python")
    "fpb"   '(elpy-black-fix-code :which-key "blacken python buffer")
    "fpy"   '(elpy-yapf-fix-code :which-key "ypaf python buffer")
    "fpi"   '(py-isort-buffer :which-key "isort python import in buffer")
    ;; aGenda
    "g"   '(:ignore t :which-key "aGenda")
    "gg"  '(org-agenda-show-jak-customized :which-key "show jak agenda")
    "ga"  '(org-agenda-show-super-week-view :which-key "show week view")
    "gb"  '(org-timer-start :which-key "org timer start")
    "gs"  '(org-timer-set-timer :which-key "org set timer")
    "gp"  '(org-timer-pause-or-continue :which-key "org timer pause or continue")
    "gk"  '(org-timer-stop :which-key "org timer kill/stop")
    ;; helm
    "h"   '(:ignore t :which-key "helm")
    ;; "ho"  '(helm-occur :which-key "Helm Occur")
    "hf"  '(helm-find :which-key "Helm Find")
    "h/"  '(helm-projectile-rg :which-key "rg, rip grep")
    "hh"  '(helm-projectile-rg :which-key "rg, rip grep")
    "hg"  '(helm-projectile-rg :which-key "rg, rip grep")
    "hi"  '(helm-imenu :which-key "helm imenu")
    "hp"  '(helm-semantic :which-key "helm semantic")
    "hl"  '(helm-semantic-or-imenu :which-key "helm semantic or imenu")
    ;; "hs"  '(helm-projectile-ag :which-key "ag, silver searcher")
    "hs"   '(:ignore t :which-key "helm search/find")
    "hsf"  '(helm-find :which-key "Helm Find: find files recursively in current directory")
    "hss"  '(helm-projectile-ag :which-key "ag, silver searcher")
    "hsg"  '(helm-projectile-rg :which-key "rg, rip grep")
    "hsn"  '(helm-all-roam-files :which-key "search in notes")
    "hsc"  '(helm-rg-all-repos :which-key "search in code")
    "hso"  '(helm-all-org-files :which-key "search in all org files")
    "hsm"  '(helm-all-meetings  :which-key "search all meeting notes")
    "hr"  '(helm-resume :which-key "Helm Resume")
    "hd"  '(helm-dash-at-point :which-key "Dash Docs")
    "hm"  '(helm-descbinds :which-key "describe bindings")
    "hy"  '(helm-yas-complete :which-key "helm yasnippet")
    "hb"  '(:ignore t :which-key "bookmarks")
    "hbb"  '(helm-bookmarks :which-key "helm bookmarks")
    "hbc"  '(bookmark-set :which-key "create bookmark")
    "hbd"  '(bookmark-delete :which-key "delete bookmark")
    ;; lsp
    "l"   '(:ignore t :which-key "lsp")
    "lr"  '(lsp-rename :which-key "rename")
    "ld"  '(lsp-ui-peek-find-definitions :which-key "defintion")
    ;; "lx"  '(lsp-ui--goto-xref :which-key "xref")
    "lf"  '(lsp-find-references :which-key "references")
    "lo"  '(lsp-ui-imenu :which-key "imenu/outline")
    "lw"  '(lsp-restart-workspace :which-key "restart lsp workspace")
    ;; dumb-jump
    ;; "j"   '(:ignore t :which-key "dumb-jump")
    ;; "jd"  '(dumb-jump-go :which-key "definition")
    ;; "jq"  '(dumb-jump-go-quick-look :which-key "quick look")
    ;; "jo"   '(ace-window :which-key "Ace Window")
    ;; magit
    "m"   '(:ignore t :which-key "magit")
    "mm"  '(magit-status :which-key "magit status")
    "mb"  '(magit-blame :which-key "magit blame")
    "mc"  '(magit-checkout :which-key "magit checkout")
    "mg"  '(magit-status :which-key "magit status")
    "ms"   '(:ignore t :which-key "smerge")
    "msu"  '(smerge-keep-upper :which-key "keep upper")
    "msl"  '(smerge-keep-lower :which-key "keep lower")
    "msa"  '(smerge-keep-all :which-key "keep all")
    "mse"  '(smerge-ediff :which-key "ediff")
    "msm"  '(smerge-keep-mine :which-key "keep mine")
    "mso"  '(smerge-keep-other :which-key "keep other")
    "msj"  '(smerge-next :which-key "smerge next")
    "msk"  '(smerge-prev :which-key "smerge prev")
    "me"   '(:ignore t :which-key "ediff")
    ;; evil window
    ;;"w"   '(evil-window-map :which-key "evil window")
    ;; notdeft
    "n"   '(:ignore t :which-key "notdeft")
    "nn"   '(notdeft :which-key "notdeft")
    "nq"   '(notdeft-open-query :which-key "notdeft query")
    ;; Org Mode
    "o"   '(:ignore t :which-key "Org Mode")
    ;; "os"  '(flyspell-correct-wrapper :which-key "spell check")
    "os"  '(ispell-word :which-key "check spelling")
    "og"   '(:ignore t :which-key "langtool grammar check")
    "ogb"  '(langtool-check :which-key "check grammar begin")
    "oge"  '(langtool-check-done :which-key "check grammar done")
    "od"  '(dictionary-search :which-key "dictionary search")
    "ot"  '(org-roam-tag-add :which-key "add roam tag")
    ;; makki = copy
    "om"   '(:ignore t :which-key "copy links to nodes")
    "omi"  '(org-id-copy :which-key "create org id and copy")
    "omf"  '(copy-current-file-name-on-clipboard :which-key "copy current file name")
    "omc"  '(org-store-link :which-key "copy org node link")
    "omp"  '(org-insert-link :which-key "insert node link")
    "on"  '(org-roam-alias-add :which-key "add roam alias")
    "of"  '(org-roam-node-find :which-key "find node")
    "oi"  '(org-roam-node-insert :which-key "insert node")
    "ol"  '(org-roam-buffer-toggle :which-key "buffer toggle or link buffer")
    "oc"  '(org-capture :which-key "org capture")
    "oo"  '(org-toggle-inline-images :which-key "open/toggle image")
    "oa"  '(org-agenda :which-key "org agenda")
    "oj"  '(org-roam-dailies-capture-today :which-key "roam journal today")
    "or"   '(:ignore t :which-key "Org roam")
    "ort"  '(org-roam-dailies-capture-today :which-key "journal today")
    "orr"  '(org-roam-buffer-refresh :which-key "roam link buffer refresh")
    "orl"  '(org-roam-buffer-toggle :which-key "buffer toggle or link buffer")
    "ord"  '(org-roam-dailies-capture-date :which-key "dated journal ")
    "org"   '(:ignore t :which-key "Daily Goto")
    "orgt"  '(org-roam-dailies-goto-today :which-key "goto daily roam journal")
    "orgd"  '(org-roam-dailies-goto-date :which-key "goto dated journal roam")
    ;; projectile
    "p"   '(projectile-command-map :which-key "Projectile")
    ;; editing/culling/deleting
    "k"   '(:ignore t :which-key "editing/culling")
    "kd"   '(flush-lines :which-key "flush lines")
    "kk"   '(keep-lines :which-key "keep lines")
    "ks"   '(delete-trailing-whitespace :which-key "delete trailing white spaces")
    ;; password, question(?)
    "q"   '(:ignore t :which-key "passwords")
    "qq"   '(password-store-copy :which-key "copy password")
    "qc"   '(password-store-copy :which-key "copy password")
    "qi"   '(password-store-insert :which-key "add password")
    "qe"   '(password-store-edit :which-key "edit password")
    "qd"   '(password-store-remove :which-key "delete password")
    "qg"   '(password-store-generate :which-key "generate password")
    "qr"   '(password-store-rename :which-key "rename password key")
    ;; replace
    "r"   '(:ignore t :which-key "replace")
    "rr"   '(anzu-query-replace :which-key "query-replace")
    "rv"   '(:ignore t :which-key "visual regex replace")
    "rvq"  '(vr/query-replace :which-key "query-replace")
    "rvr"  '(vr/replace :which-key "replace")
    "rvm"  '(vr/mc-mark :which-key "mark")
    ;; virtual containers and container managers
    "v"   '(:ignore t :which-key "docker/kuberntes")
    "vd"   '(docker :which-key "docker")
    "vk"   '(kubernetes-overview :which-key "kubernetes")
    ;; Misc for speed and convenience
    ;; Misc for speed and convenience
    ;; "l"   '(locate :which-key "Locate")
    ;; "f"   '(find-dired :which-key "Find: result in dired")
    ;; "f"   '(fzf-directory :which-key "fzf find file")
    "x"   '(helm-M-x :which-key "M-x")
    "s"   '(save-buffer :which-key "Save Buffer")
    ;; tabs
    "t"   '(:ignore t :which-key "tabs or emacs workspaces")
    "tt"  '(tab-switch  :which-key "switch to another tab")
    "ts"  '(tab-switch  :which-key "switch to another tab")
    "tr"  '(tab-rename  :which-key "tab rename")
    "tb"  '(my/switch-to-tab-buffer  :which-key "select buffers in current tab")
    "tc"  '(tab-close  :which-key "tab close")
    "tn"  '(my/new-project-tab  :which-key "projectile project in new tab")
    "tp"  '(my/new-project-tab  :which-key "projectile project in new tab")
    ;; "e"   '(mu4e :which-key "Email Client")
    "y"   '(helm-show-kill-ring :which-key "Kill Ring")
    ;;"g"   '(magit-status :which-key "magit-status")
    ;; Misc, convinient shortucts
    "SPC" '(helm-projectile-find-file :which-key "projectile find file")
    ;; "SPC" '(helm-mini :which-key "opened buffers and recent files")
    ;; "/"   '(helm-occur :which-key "helm occur local buffer")
    "/"   '(helm-swoop :which-key "helm swoop local buffer")
    ;; "j"  '(helm-projectile-rg :which-key "rg, rip grep")
    ;; in normal mode
    ;; "u"   '(universal-argument :which-key "universal argument")
    ","   '(previous-buffer :which-key "previous buffer")
    "."   '(next-buffer :which-key "next buffer")
   ;; shell commands 
    "!"   '(:ignore t :which-key "shell commands")
    "!!"   '(shell-command :which-key "shell command")
    "!e"   '(eshell-command :which-key "eshell command")
    "!s"   '(shell-command :which-key "shell command")
    )
  )

(global-set-key (kbd "C-x b") #'my/switch-to-tab-buffer)
;; global key shortcuts
;; kill this buffer without asking name
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; ace-jump-mode
;;(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "M-n") 'ace-jump-line-mode)

;; webjump
(global-set-key (kbd "s-j") 'webjump)

;; ace-jump-helm-line
(eval-after-load "helm" '(define-key helm-map (kbd "C-q") 'ace-jump-helm-line))

;; new line and indent
(global-set-key (kbd "<S-return>") 
                (kbd "C-e C-m"))
;; (global-set-key [C-tab] 'next-buffer)
;; (global-set-key [C-S-iso-lefttab] 'previous-buffer)
;; (global-set-key (kbd "M-]") 'next-buffer)
;; (global-set-key (kbd "M-[") 'previous-buffer)

(global-set-key (kbd "C-c C-j") 'previous-buffer)
(global-set-key (kbd "C-c C-k") 'next-buffer)

(global-set-key (kbd "<M-up>") 'drag-stuff-up)
(global-set-key (kbd "<M-down>") 'drag-stuff-down)


;; some package is using M-o by default, placed this here to override it
(global-set-key (kbd "M-o") 'ace-window)

(global-set-key (kbd "C--") 'fk/decrease-font-size)
(global-set-key (kbd "C-*") 'fk/increase-font-size)
(global-set-key (kbd "C-0") 'fk/reset-font-size)

;; dired
(global-set-key (kbd "C-x C-,") `dired-sidebar-toggle-sidebar)
;; delete white spaces before saving
;;(add-hook 'before-save-hook 'whitespace-cleanup)

;; copy line
;;(global-set-key (kbd "s-d") "\C-a\C- \C-n\M-w\C-y")
;; (use-package helm-gitignore
;;   :ensure t)

(provide 'init-custom-shortcuts)
