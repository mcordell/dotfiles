;;; $$DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Michael Cordell" user-mail-address "mike@mikecordell.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "PragmataPro Mono"
                           :size 18
                           :weight 'normal) doom-variable-pitch-font (font-spec :family
                                                                                "Droid Sans Mono for Powerline"
                                                                                :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Org

(setq global-auto-revert-mode t
      org_notes "~/org/org-roam" zot_bib "~/org/mylibrary/mylibrary.bib" org-directory "~/org/"
      org-agenda-files '("~/org/" "~/org/qcentrix/" "~/org/qcentrix/people/")
      org-download-image-dir "~/org/img"
      org-enforce-todo-dependencies t
      bibtex-completion-notes-path org_notes
      bibtex-completion-bibliography zot_bib
      bibtex-completion-pdf-field "file"
      bibtex-completion-notes-template-multiple-files (concat
                                                       "#+TITLE: ${title}\n"
                                                       "#+ROAM_KEY: cite:${=key=}\n"
                                                       "* TODO Notes\n"
                                                       ":PROPERTIES:\n"
                                                       ":Custom_ID: ${=key=}\n"
                                                       ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                                                       ":AUTHOR: ${author-abbrev}\n"
                                                       ":JOURNAL: ${journaltitle}\n"
                                                       ":DATE: ${date}\n"
                                                       ":YEAR: ${year}\n"
                                                       ":DOI: ${doi}\n"
                                                       ":URL: ${url}\n"
                                                       ":END:\n\n")
 )

(defun mcordell/org-id-update-org-roam-files ()
  "Update Org-ID locations for all Org-roam files."
  (interactive)
  (org-id-update-id-locations (org-roam-list-files)))

(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil (mapcar (lambda (x)
                      (if (and (buffer-file-name x)
                               (string-match "\\.org$" (buffer-file-name x)))
                          (buffer-file-name x)))
                    (buffer-list))))


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

                                                  ("o" "one-on-one" entry (file
                                                                           "~/org/qcentrix/qcentrix.org")
                                                   "* %^{Bijal|Sujay|Brad|Brian|Do|Matt|Teo|Eric|Steve} 1-1 %t
%?
")
                                                  ("m" "Meeting" entry (file "~/org/qcentrix/qcentrix.org")
                                                   "* %^{Subject} <%<%Y-%m-%d %H:00>>
Participants: %^{Participants}
%?
")
                                                  ("a" "q-centrix task" entry (file+headline "~/org/qcentrix/big_board.org" "Tasks")
                                                   "* TODO %^{Subject}
%?
")
                                                  ("x" "Q-Centrix Note" entry (file
                                                                               "~/org/qcentrix/qcentrix.org")
                                                   "* %? %t
")))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"             ; A task that needs doing & is ready to do
           "PROJ(p)"             ; A project, which usually contains other tasks
           "LOOP(r)"             ; A recurring task
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
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("DELG" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel))
        org-default-priority 67
          org-agenda-custom-commands '(
                  ("o" "Agenda and Office-related tasks"
                          ((agenda (org-agenda-span day))
                          (tags-todo "work")
                          (tags "office")))
                  ("w" "multiple" (
                          (agenda ""
                          (
                          (org-agenda-start-day "0d")
                          (org-agenda-span 1)
                          ))
                          (tags-todo "+PRIORITY={A}|+PRIORITY={B}"
                                  (
                                   (org-agenda-overriding-header "High Priority:")
                                   (org-agenda-sorting-strategy '(priority-down))))))
                  )
        org-refile-targets '((+org/opened-buffer-files :maxlevel . 9))
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

(use-package! org-roam
  :custom org-roam-directory "~/org/roam" org-roam-prefer-id-links t
  :config
    (setq org-roam-dailies-directory "daily/"
          org-roam-db-location "~/org/org-roam.db"
          org-roam-dailies-capture-templates (let ((head
                                                    (concat "#+title: %<%Y-%m-%d (%A)>\n* Morning Questions\n"
                                                            "** What Am I Grateful for?\n\n** What Would Make Today Great?\n** What am I worried about?\n"
                                                            "* Evening Questions\n"
                                                            "** How am I feeling?\n** What's Something Good That Happened Today?\n** What Did I Do Well?\n** What Could I Have Done Better?")))
                                               `(("m" "journal" plain
                                                  "%?" :if-new
                                                  (file+head+olp "%<%Y-%m-%d>.org" ,head ("Morning Questions"))
                                                  :unnarrowed t
                                                  )))
          org-roam-capture-templates
          '(("d" "default" plain "#+bibliography: ../mylibrary/mylibrary.bib\n#+cite_export: csl nature.csl\n%?\n-------\n#+print_bibliography:\n"
             :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                "#+title: ${title}")
             :unnarrowed t))
          )

    (add-hook 'org-roam-capture-after-find-file-hook
     (lambda ()
       (org-id-get-create)
       (save-buffer)
       (org-roam-db-update))))
(after! org-download
  (setq org-download-method 'directory)
  )

(after! calfw-org
  ;; hotfix: incorrect time range display
  ;; source: https://github.com/zemaye/emacs-calfw/commit/3d17649c545423d919fd3bb9de2efe6dfff210fe
  (defun cfw:org-get-timerange (text)
    "Return a range object (begin end text). If TEXT does not have a range, return nil."
    (let* ((dotime (cfw:org-tp text 'dotime)))
      (and (stringp dotime) (string-match org-ts-regexp dotime)
	   (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
           (start-date (nth 1 (car matches)))
           (end-date (nth 1 (nth 1 matches)))
	       (extra (cfw:org-tp text 'extra)))
	   (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
       ( list( calendar-gregorian-from-absolute
       (time-to-days
       (org-read-date nil t start-date))
       )
       (calendar-gregorian-from-absolute
       (time-to-days
       (org-read-date nil t end-date))) text)))))))

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config (setq orb-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor"
                                         "keywords"))
  (setq orb-templates '(("r" "ref" plain (function org-roam-capture--get-point) ""
                         :file-name "${slug}"
                         :head
                         "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS ${tags}\n- \n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
                         :unnarrowed t))))


(use-package! ox-hugo
  :after org)

(after! oc-csl
  (setq org-cite-csl-styles-dir "~/Zotero/styles" org-cite-csl-locales-dir "~/.local/share/csl/locales")
  )

(after! citar
  (setq org-cite-global-bibliography '("~/org/mylibrary/mylibrary.bib")
        bibtex-completion-bibliography '("~/org/mylibrary/mylibrary.bib")
        citar-bibliography org-cite-global-bibliography
        citar-open-note-function 'orb-citar-edit-note
        citar-notes-paths '("~/org/roam/")
        citar-file-note-org-include '(org-id org-roam-ref)
        citar-at-point-function 'embark-act
        citar-symbols
        `((file . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
                ,(all-the-icons-icon-for-file "foo.pdf" :face 'citar-icon-dim)))
        (note . (,(all-the-icons-icon-for-file "foo.txt") .
                ,(all-the-icons-icon-for-file "foo.txt" :face 'citar-icon-dim)))
        (link .
                (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim))))
        )
   ;; Here we define a face to dim non 'active' icons, but preserve alignment
   (defface citar-icon-dim
   '((((background dark)) :foreground "#282c34")
   (((background light)) :foreground "#fafafa"))
   "Face for obscuring/dimming icons"
   :group 'all-the-icons-faces))

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

;; Keymaps

(map! :leader
      (:prefix "TAB"
               "TAB" #'evil-switch-to-windows-last-buffer)
      (:prefix "f"
       ("a" (lambda ()
             (interactive)
             (org-agenda nil "w")))
       ("t"  #'treemacs))
      (:prefix "s"
       ("c"  #'evil-ex-nohighlight))
      (:prefix "o"
       ("/" #'org-roam-node-find)
       ("c" #'cfw:open-org-calendar)))

(map! :prefix ","
      (:map emacs-lisp-mode-map
       :nv "f"
       #'elisp-format-buffer)
      (:map org-mode-map
       :nv "o" #'org-open-at-point
       :nv "b" #'helm-bibtex
       :nv "e" #'org-mac-mailmate-insert-selected
       :nv "f" #'org-mac-finder-item-get-selected
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
        :nv "m" #'alchemist-mix
        )))


(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam)

(after! projectile
  (require 'f)
  (setq projectile-enable-caching nil projectile-project-search-path (f-directories "~/Code/Work/q-centrix/"))
  )

(use-package! flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))
