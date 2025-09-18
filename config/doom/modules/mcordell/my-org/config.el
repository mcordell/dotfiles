(defvar mcordell/work-org-directory "~/org/qcentrix")

(defvar mcordell/one-on-one-list
  '(
    ("Andrew" . "Tuesday 13:00")
    ("Brad" . "Wednesday 9:00")
    ("Chris" . "Tuesday 12:30")
    ("Jamaal" . "Thursday 11:30")
    ("Mark" . "Wednesday 11:00")
    ("Matt" . "Friday 11:30")
    ("Pierce" . "Wednesday 12:30")
    ("Preeti" . "Friday 12:00")
    ("Teo" . "Tuesday 14:30")
    ))

(defvar mcordell/one-on-one-files-dir "~/org/qcentrix/" "Directory whose .org files are searched for 1:1 items.")

(defvar mcordell/work-meeting-file "~/org/qcentrix/meetings.org" "Org file where meeting headings are stored.")

(after! org
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"             ; A task that needs doing & is ready to do
           "PROJ(p)"             ; A project, which usually contains other tasks
           "LOOP(r)"             ; A recurring task
           "QUEST(q)"            ; A question
           "STRT(s)"             ; A task that is in progress
           "WAIT(w@/!)"          ; Something external is holding up this task
           "HOLD(h)"             ; This task is paused/on hold because of me
           "IDEA(i)"             ; An unconfirmed and unapproved task or notion
           "DELG(l@/!)"
           "|"
           "DONE(d)"    ; Task successfully completed
           "KILL(k)")   ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"                     ; A task that needs doing
           "[-](S)"                     ; Task is in progress
           "[?](W)"                     ; Task is being held up or paused
           "|"
           "[X](D)")                    ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]" . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("QUEST" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("DELG" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))
        org-default-priority 67))

(after! org
  (add-hook 'org-open-link-functions '+org/org-pass-link-to-system)
  (set-company-backend! 'org-mode '(company-capf))
  (setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        )
  )

(after! org (setq-default org-capture-templates '(("s" "ruby snippet" entry (file "~/org/notes.org")
                                                   "* Snippet: %a
#+BEGIN_SRC %^{sourcetype}
 %c
#+END_SRC")

                                                  ("t" "Task" entry (file "~/org/todos.org")
                                                   "* TODO %?
 %i
 %a")
                                                  ("n" "note" entry (file "~/org/notes.org")
                                                   "* %? :NOTE:

%U
%a
")                                                 ("i" "idea" entry (file "~/org/inbox.org")
                                                   "* %?

%U
%a
")
                                                  ("r" "review" entry (file+headline
                                                                       "~/org/qcentrix/qcentrix.org"
                                                                       "Reviews")
                                                   "** TODO [[%c][%^{description}]] :%^{repo|reg-api|reg-imp|reg-web}:")

                                                  ("o" "One on One" entry
                                                   (file "~/org/qcentrix/big_board.org")
                                                   "%(mcordell/create-one-on-one-heading-with-prompt)
%?"
                                                   :empty-lines 1
                                                   :unnarrowed t
                                                   :jump-to-captured t
                                                   )
                                                  ("g" "simple o3" entry (file mcordell/work-meeting-file)
                                                   "%(mcordell/create-simple-one-on-one-heading-with-prompt)
%?"
                                                   :empty-lines 1
                                                   :unnarrowed t
                                                   :jump-to-captured t
                                                   )
                                                  ("m" "Meeting" entry (file "~/org/qcentrix/qcentrix.org")
                                                   "* %^{Subject} %^t<%<%Y-%m-%d %H:00>>
%?
")
                                                  ("a" "q-centrix task" entry (file+headline "~/org/qcentrix/big_board.org" "Tasks")
                                                   "* TODO %^{Subject}
%?
")
                                                  ("x" "Q-Centrix Note" entry (file
                                                                               "~/org/qcentrix/qcentrix.org")
                                                   "* %? %t
"))


                          )
  )

(after! org
  (setq org-agenda-custom-commands '(
                                     ("o" "Work tasks"
                                      ((tags-todo "*"
                                                  ((org-agenda-overriding-header "Work tasks")))
                                       )
                                      ((org-agenda-files
                                        (directory-files-recursively "~/org/qcentrix/" "\\.org\\'")))
                                      )
                                     ("w" "multiple"
                                      ((agenda ""
                                               ((org-agenda-start-day "0d")
                                                (org-agenda-span 1)
                                                ;; Keep only TODO/QUEST items in the agenda block
                                                (org-agenda-skip-function
                                                 (lambda ()
                                                   (save-excursion
                                                     (org-back-to-heading t)
                                                     (let ((kw (org-get-todo-state)))
                                                       (unless (member kw '("TODO" "QUEST"))
                                                         (or (outline-next-heading) (point-max)))))))))
                                       ;; High priority list, limited to TODO|QUEST and A/B priority
                                       (tags-todo "+TODO={TODO\\|QUEST}+PRIORITY={A\\|B}"
                                                  ((org-agenda-overriding-header "High Priority:")
                                                   (org-agenda-sorting-strategy '(priority-down)))))
                                      ;; Settings applied to all blocks in this command
                                      ((org-agenda-files
                                        (directory-files-recursively "~/org/qcentrix/" "\\.org\\'"))))                                       )
        )
  )

(use-package! org-mac-link
  :after org
  :init
  (defun as-get-selected-finder-items ()
    (do-applescript (concat "tell application \"Finder\"\n" " set theSelection to the selection\n"
                            " set links to {}\n" " repeat with theItem in theSelection\n"
                            " set theLink to \"file+sys://\" & (POSIX path of (theItem as string)) & \"::split::\" & (get the name of theItem) & \"\n\"\n"
                            " copy theLink to the end of links\n" " end repeat\n"
                            " return links as string\n" "end tell\n")))

  (defun as-get-selected-mailmate-message ()
    (do-applescript (concat "tell application \"MailMate\"\n" " set allMessages to messages\n"
                            " set theMessage to item 1 of allMessages\n"
                            " return (message url of theMessage) & \"::split::\" & (name of theMessage)\n"
                            " end tell\n")))
  (defun org-mac-mailmate-item-get-selected ()
    (interactive)
    (message "Applescript: Getting mailmate message...")
    (org-mac-link-paste-applescript-links (as-get-selected-mailmate-message)))

  (defun org-mac-mailmate-insert-selected ()
    (interactive)
    (insert (org-mac-mailmate-item-get-selected)))
  (defun org-mac-link-applescript-chrome-get-frontmost-url ()
    "AppleScript to get the links to the frontmost window of the Chrome.app."
    (let ((result
           (org-mac-link-do-applescript
            (concat
             "set frontmostApplication to path to frontmost application\n"
             "tell application \"Brave\"\n"
             "	set theUrl to get URL of active tab of first window\n"
             "	set theResult to (get theUrl) & \"::split::\" & (get name of window 1)\n"
             "end tell\n"
             "activate application (frontmostApplication as text)\n"
             "set links to {}\n"
             "copy theResult to the end of links\n"
             "return links as string\n"))))
      (replace-regexp-in-string
       "^\"\\|\"$" "" (car (split-string result "[\r\n]+" t)))))

  )

(use-package! calfw-org
  :after org
  :config
  (defun my/open-calendar ()
    (interactive)
    (cfw:open-org-calendar)))

(after! calfw
  ;; Custom RET handler
  (defun my/cfw-open-entry-at-point ()
    "Show the calendar item details at point, if any."
    (interactive)
    (let ((cp (cfw:get-cur-cell)))
      (when cp
        (let ((contents (cfw:cp-get-contents cp)))
          (if contents
              (cfw:show-details contents)
            (message "No entry under cursor."))))))

  ;; Override RET key in calendar view
  (define-key cfw:calendar-mode-map (kbd "RET") #'my/cfw-open-entry-at-point))
