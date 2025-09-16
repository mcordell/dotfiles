;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Michael Cordell" user-mail-address "mike@mikecordell.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono"
                           :size 18
                           :weight 'normal)
      )

;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-agenda-files '("~/org/" "~/org/qcentrix/" "~/org/qcentrix/people/" "~/org/qcentrix/mro/" "~/org/qcentrix/mro/products/")
      zot_bib "~/org/mylibrary/mylibrary.bib" 
      one-on-one-list
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
        )
      )

(defvar mcordell/one-on-one-files-dir "~/org/qcentrix/" "Directory whose .org files are searched for 1:1 items.")

(defun mcordell/normalize-name-to-tag (name)
  "Convert NAME to a tag-friendly string, e.g. \"Brian\" -> \"brian\"."
  (let* ((lower (downcase name)))
    (replace-regexp-in-string "[^a-z0-9_@#%]+" "_" lower)))

(defun mcordell/agenda-one-on-one ()
  "Prompt for a 1:1 name, then show open TODOs tagged with that person."
  (interactive)
  (let* ((names (mapcar #'car one-on-one-list))
         (choice (completing-read "1:1 with: " names nil t))
         (when-str (or (cdr (assoc choice one-on-one-list)) ""))  ; optional, for header
         (tag (mcordell/normalize-name-to-tag choice))
         ;; Localize agenda files only for this command:
         (org-agenda-files (directory-files-recursively mcordell/one-on-one-files-dir "\\.org\\'"))
         ;; Nice header
         (org-agenda-overriding-header
          (format "Open TODOs for %s  %s" choice
                  (if (string-empty-p when-str) "" (format "(%s)" when-str)))))
    ;; Show only TODO entries with the selected tag:
    ;; org-tags-view: first arg non-nil => TODO-only
    (org-tags-view t (concat "+" tag))))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Org
(defun mcordell/insert-rrp-jira ()
  "Insert a JIRA or GitHub PR link from the clipboard in Org-mode."
  (interactive)
  (let* ((clipboard-content (with-temp-buffer
                              (call-process "pbpaste" nil t)
                              (buffer-string)))
         (jira-url-regex "https://qcentrix.atlassian.net/browse/\\(RRP-[0-9]+\\)")
         (jira-key-regex "\\(RRP-[0-9]+\\)")
         (github-url-regex "https://github.com/q-centrix/\\(.*\\)/pull/[0-9]+")
         (jira-key (if (string-match jira-url-regex clipboard-content)
                       (match-string 1 clipboard-content)
                     (if (string-match jira-key-regex clipboard-content)
                         (match-string 1 clipboard-content)
                       nil)))
         (github-repo (if (string-match github-url-regex clipboard-content)
                          (match-string 1 clipboard-content)
                        nil)))
    (cond
     (jira-key
      (let ((jira-link (format "[[https://qcentrix.atlassian.net/browse/%s][%s]]" jira-key jira-key)))
        (insert jira-link)))
     (github-repo
      (let ((github-link (format "[[%s][%s PR]]" clipboard-content github-repo)))
        (insert github-link)))
     (t
      (message "No JIRA or GitHub link found in clipboard")))))

;; Creates a heading for a one-on-one meeting with the given NAME on the next occurrence of the day and time specified in the one-on-one list.
;; The format for the heading is "* NAME 1-1 <Y-m-d a H:M>", where <Y-m-d a H:M> is the date and time of the next occurrence of the specified day and time.
;; Returns the formatted heading as a string.
(defun mcordell/create-one-on-one-heading (name)
  (let* ((day-time (cdr (assoc name one-on-one-list)))
         (today (current-time))
         (desired-day (parse-time-string day-time))
         (desired-dow (nth 6 desired-day))
         (desired-hour (nth 2 desired-day))
         (desired-min (nth 1 desired-day))
         (current-dow (nth 6 (decode-time today)))
         (days-until (- desired-dow current-dow)))
    (if (< days-until 0)
        (setq days-until (+ days-until 7)))
    (let ((next-meeting-date (time-add today (days-to-time days-until))))
      (format-time-string (concat "* " name " 1-1 <%Y-%m-%d %a %H:%M>")
                          (encode-time 0 desired-min desired-hour
                                       (nth 3 (decode-time next-meeting-date))
                                       (nth 4 (decode-time next-meeting-date))
                                       (nth 5 (decode-time next-meeting-date)))))))

(defun mcordell/org-agenda-todo-with-tag ()
  (interactive)
  (let* ((tags-list '("jamaal" "chris" "andrew" "brian" "teo" "brad" "brad-b")) ; Define your list of tags here
         (tag (completing-read "Choose tag: " tags-list)))
    (org-tags-view nil (concat "TODO=\"TODO\"&" tag))))

(defun mcordell/create-one-on-one-heading-with-prompt ()
  (let* ((name (completing-read "Select a name: " (mapcar 'car one-on-one-list)))
         (heading (mcordell/create-one-on-one-heading name)))
    heading))

(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delete-dups
   (append
    (delq nil (mapcar (lambda (x)
                        (if (and (buffer-file-name x)
                                 (string-match "\\.org$" (buffer-file-name x)))
                            (buffer-file-name x)))
                      (buffer-list)))
    (directory-files-recursively "~/org/qcentrix/people/" "\\.org$" nil)
    ))
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
          ("[?]" . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("DELG" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)
          org-default-priority 67
          org-agenda-custom-commands '(
                                       ("o" "Agenda and Office-related tasks"
                                        ((agenda (org-agenda-span day))
                                         (tags-todo "work")
                                         (tags "office")))
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
          org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  (defun org-pass-link-to-system (link)
    (if (string-match "^[\"a-zA-Z0-9]+:" link)
        (shell-command (concat "open " link))
      nil)
    )

  (add-hook 'org-open-link-functions 'org-pass-link-to-system)
  (set-company-backend! 'org-mode '(company-capf))
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
(load! "lisp/meeting-creator.el")
(use-package! meeting-creator
  :commands (qcentrix-add-meetings)
  :after org           ; if you want to enforce load order
  )
;; Keymaps

(map! :leader
      (:prefix "TAB"
               "TAB" #'evil-switch-to-windows-last-buffer)
      (:prefix "f"
               ("a" (lambda ()
                      (interactive)
                      (org-agenda nil "w")))
               ("t"  #'treemacs)
               ("w"  #'+default/search-project)
               )
      (:prefix "s"
               ("c"  #'evil-ex-nohighlight))
      (:prefix "m"
               (:map org-mode-map
                :nv "j" #'mcordell/insert-rrp-jira
                )
               )
      (:prefix "o"
               ("c" #'cfw:open-org-calendar))
      )

(map! :prefix ","
      (:map emacs-lisp-mode-map
       :nv "f"
       #'elisp-format-buffer)
      :nv "t" #'projectile-toggle-between-implementation-and-test
      (:map org-mode-map
       :nv "o" #'org-open-at-point
       :nv "b" #'helm-bibtex
       :nv "e" #'org-mac-mailmate-insert-selected
       :nv "f" #'org-mac-finder-item-get-selected
       :nv "l" #'org-mac-link-brave-insert-frontmost-url
       :nv "r" #'helm-org-rifle)
      (:nv "," #'evil-switch-to-windows-last-buffer)
      (:prefix "d"
               (:map org-mode-map
                :nv "t" #'org-time-stamp))
      (:prefix ("s" . "subtree")
               (:map org-mode-map
                :nv "h" #'org-promote-subtree
                :nv "l" #'org-demote-subtree
                :nv "r" #'org-refile
                :nv "a" #'org-archive-subtree))
      (:prefix ("m" . "mix")
               (:map elixir-mode-map
                :nv "m" #'alchemist-mix)))
