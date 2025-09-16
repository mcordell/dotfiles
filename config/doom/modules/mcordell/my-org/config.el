(defvar mcordell/work-org-directory "~/org/qcentrix")

(defvar mcordell/one-on-one-list
'(
  ("Matt" . "Friday 11:30")
  ("Chris" . "Tuesday 12:30")
  ("Andrew" . "Tuesday 13:00")
  ("Teo" . "Tuesday 14:30")
  ("Jamaal" . "Thursday 11:30")
  ("Brad" . "Wednesday 9:00")
  ("Pierce" . "Wednesday 12:30")
  ("Preeti" . "Friday 12:00")
  ("Mark" . "Wednesday 11:00")
  ))

(defvar mcordell/one-on-one-files-dir "~/org/qcentrix/" "Directory whose .org files are searched for 1:1 items.")

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
