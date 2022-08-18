(require 'org-habit)


(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode +1)
  )


;; to display habits in agenda grid not at bottom
;; https://emacs.stackexchange.com/questions/17125/how-to-see-habit-items-along-the-timegrid-like-normal-items
(setq org-agenda-sorting-strategy
  '((agenda time-up priority-down category-keep)
    (todo   priority-down category-keep)
    (tags   priority-down category-keep)
    (search category-keep)))


;; default is 40
(setq org-habit-graph-column 80)
(setq org-agenda-window-setup 'reorganize-frame)
;; to record and restored windows config after quitting agenda
(setq org-agenda-restore-windows-after-quit 't)

(setq org-agenda-custom-commands
      '(("z" "Super zaen view"
         (
          ;; (agenda "" ((org-agenda-span 'day)
          ;;             (org-super-agenda-groups
          ;;              '((:name "Today"
          ;;                       :time-grid t
          ;;                       :date today
          ;;                       :todo "TODAY"
          ;;                       :scheduled today
          ;;                       :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "Todo List")
                       (org-super-agenda-groups
                        
                        '(
                          ;; (:name "Today"
                          ;;        :time-grid t
                          ;;        :date today
                          ;;        :todo "TODAY"
                          ;;        :order 1
                          ;;        )
                          (:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "### Important ###"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "## Start Today ##"
                                 :scheduled today
                                 :order 2)
                          (:name "##**## Complete Today ##**##"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Assignments"
                                 :tag "Assignment"
                                 :order 10)
                          (:name "Grouped wrt tags"
                                 :auto-tags t
                                 :order 90
                                 )
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "Emacs"
                                 :tag "emacs"
                                 :order 100)
                          (:name "During leisure"
                                 :priority<= "C"
                                 :tag ("trivial" "unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 200)
                          (:order-multi (99 (:name "Shopping in town"
                                                   ;; Boolean AND group matches items that match all subgroups
                                                   :and (:tag "shopping" :tag "@town"))
                                            (:name "elastic stack"
                                                   ;; Multiple args given in list with implicit OR
                                                   :tag ("app_search" "elastic"))
                                            (:name "Personal"
                                                   :habit t
                                                   :tag "personal")
                                            (:name "Space-related (non-moon-or-planet-related)"
                                                   ;; Regexps match case-insensitively on the entire entry
                                                   :and (:regexp ("space" "NASA")
                                                                 ;; Boolean NOT also has implicit OR between selectors
                                                                 :not (:regexp "moon" :tag "planet")))))
                          ;; (:Discard (:tag ("Chore" "Routine" "Daily")
                          ;;                 )
                          ;;           )
                          )))))))
      )



;; to add additional custom command in agenda
;; https://emacs.stackexchange.com/questions/56029/when-creating-an-org-agenda-custom-commands-command-how-can-i-make-the-agenda

;; (add-to-list 'org-agenda-custom-commands
;;              '("p" "All TODOs groups by category" alltodo ""
;;                (
;;                 (org-super-agenda-groups '((:auto-category t)))
;;                 ;; ((org-super-agenda-group-property-name "project")
;;                 (org-super-agenda-groups
;;                  ;;                '((:auto-group t)))
;;                  '((:auto-tags t)))
;;                 ;;  (org-agenda-list))
;;                 )
;;                ))


;; And this is what I actually use.  The `defvar' is stored in my
;; prot-org.el file.  In the video I explain why I use this style.

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))


(defvar prot-org-custom-daily-agenda
  ;; NOTE 2021-12-08: Specifying a match like the following does not
  ;; work.
  ;;
  ;; tags-todo "+PRIORITY=\"A\""
  ;;
  ;; So we match everything and then skip entries with
  ;; `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-skip-function '(org-agenda-skip-if nil '(timestamp)))
                (org-agenda-skip-function
                 `(org-agenda-skip-entry-if
                   'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "Important tasks without a date\n")))
    (agenda "" ((org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "\nToday's agenda\n")))
    (agenda "" ((org-agenda-start-on-weekday 0)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nNext three days\n")))
    ;; (agenda "" ((org-agenda-time-grid nil)
    ;;             ;; (org-agenda-start-on-weekday nil)
    ;;             (org-agenda-start-on-weekday 0)
    ;;             ;; We don't want to replicate the previous section's
    ;;             ;; three days, so we start counting from the day after.
    ;;             (org-agenda-start-day "+4d")
    ;;             (org-agenda-span 14)
    ;;             (org-agenda-show-all-dates nil)
    ;;             (org-deadline-warning-days 0)
    ;;             (org-agenda-block-separator nil)
    ;;             (org-agenda-entry-types '(:deadline))
    ;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
    ;;             (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
    (alltodo "" (
                 (org-agenda-overriding-header "Tasks without date and priority\n")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                 ;; (org-agenda-skip-entry-if 'regexp "TODO\s*\[\#A\]")
                 ;; (org-agenda-skip-function
                 ;;     '(or (air-org-skip-subtree-if-priority ?A)
                 ;;          (air-org-skip-subtree-if-priority ?B)
                 ;;          (air-org-skip-subtree-if-priority ?C)
                 ;;          (org-agenda-skip-if nil '(scheduled deadline))))
                 (org-agenda-skip-function '(air-org-skip-subtree-if-priority ?A))
                 ;; (org-agenda-skip-function '(air-org-skip-subtree-if-priority ?B))
                 ;; (org-agenda-skip-function '(air-org-skip-subtree-if-priority ?C))
                 (org-super-agenda-groups
                '((:name "Grouped wrt tags"
                                 :auto-tags t
                                 :order 90
                                 )
                 ))
                 ))

    )
  "Custom agenda for use in `org-agenda-custom-commands'.")



(setq org-agenda-custom-commands
      `(("j" "Daily agenda and top priority tasks"
         ,prot-org-custom-daily-agenda)
        ("P" "Plain text daily agenda and top priorities"
         ,prot-org-custom-daily-agenda
         ((org-agenda-with-colors nil)
          (org-agenda-prefix-format "%t %s")
          (org-agenda-current-time-string ,(car (last org-agenda-time-grid)))
          (org-agenda-fontify-priorities nil)
          (org-agenda-remove-tags t))
         ;; ("agenda.txt")
         )
        ("z" "Super zaen view"
         (
          ;; (agenda "" ((org-agenda-span 'day)
          ;;             (org-super-agenda-groups
          ;;              '((:name "Today"
          ;;                       :time-grid t
          ;;                       :date today
          ;;                       :todo "TODAY"
          ;;                       :scheduled today
          ;;                       :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "Todo List")
                       (org-super-agenda-groups
                        
                        '(
                          ;; (:name "Today"
                          ;;        :time-grid t
                          ;;        :date today
                          ;;        :todo "TODAY"
                          ;;        :order 1
                          ;;        )
                          (:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "### Important ###"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "## Start Today ##"
                                 :scheduled today
                                 :order 2)
                          (:name "##**## Complete Today ##**##"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Assignments"
                                 :tag "Assignment"
                                 :order 10)
                          (:name "Grouped wrt tags"
                                 :auto-tags t
                                 :order 90
                                 )
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "Emacs"
                                 :tag "emacs"
                                 :order 100)
                          (:name "During leisure"
                                 :priority<= "C"
                                 :tag ("trivial" "unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 200)
                          (:order-multi (99 (:name "Shopping in town"
                                                   ;; Boolean AND group matches items that match all subgroups
                                                   :and (:tag "shopping" :tag "@town"))
                                            (:name "elastic stack"
                                                   ;; Multiple args given in list with implicit OR
                                                   :tag ("app_search" "elastic"))
                                            (:name "Personal"
                                                   :habit t
                                                   :tag "personal")
                                            (:name "Space-related (non-moon-or-planet-related)"
                                                   ;; Regexps match case-insensitively on the entire entry
                                                   :and (:regexp ("space" "NASA")
                                                                 ;; Boolean NOT also has implicit OR between selectors
                                                                 :not (:regexp "moon" :tag "planet")))))
                          ;; (:Discard (:tag ("Chore" "Routine" "Daily")
                          ;;                 )
                          ;;           )
                          ))))))
        ))

(provide 'init-org-agenda)
;;; init-agenda.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
