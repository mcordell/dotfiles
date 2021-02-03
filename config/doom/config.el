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
(setq doom-font (font-spec :family "Inconsolata for Powerline"
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

(setq org_notes "~/org/org-roam" zot_bib "~/org/mylibrary/mylibrary.bib" org-directory "~/org/"
      org-agenda-files '("~/org/" "~/org/qcentrix/" "~/org/qcentrix/people/")
      org-enforce-todo-dependencies t)

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
")
                                                  ("r" "review" entry (file+headline
                                                                       "~/org/qcentrix.org"
                                                                       "Reviews")
                                                   "** TODO [[%c][%^{description}]] :%^{repo|reg-api|reg-imp|reg-web}:")
                                                  ("o" "one-on-one" entry (file
                                                                           "~/org/qcentrix.org")
                                                   "* 1-1 %^{Bijal|Sujay|Brad|Brian|Do|Matt|Teo|Grace|Eric} %t
%?
")
                                                  ("m" "Meeting" entry (file "~/org/qcentrix.org")
                                                   "* %^{Subject} <%<%Y-%m-%d %H:00>>
Participants: %^{Participants}
%?
")
                                                  ("e" "evening" entry (file+olp+datetree
                                                                        "~/org/journal.org")
                                                   "* Evening :EVENING:
** What did you learn today?
       %?")
                                                  ("q" "Q-Centrix Note" entry (file
                                                                               "~/org/qcentrix.org")
                                                   "* %? %t
"))))

(setq org-roam-db-location "~/org/org-roam.db")
(use-package! org-roam
  :custom org-roam-directory "~/org/roam" )

(setq org-roam-capture-templates '(("d" "default" plain #'org-roam--capture-get-point "%?"
                                    :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                    :head "#+title: ${title}\n#+roam_tags: ${tags}"
                                    :unnarrowed t)))
(use-package! org-roam-server
  :ensure t
  :config (setq org-roam-server-host "127.0.0.1" org-roam-server-port 8080
                org-roam-server-authenticate nil org-roam-server-export-inline-images t
                org-roam-server-serve-files nil org-roam-server-served-file-extensions '("pdf" "mp4"
                                                                                         "ogv")
                org-roam-server-network-poll t org-roam-server-network-arrows nil
                org-roam-server-network-label-truncate t
                org-roam-server-network-label-truncate-length 60
                org-roam-server-network-label-wrap-length 20))
(use-package! org-ref
  :config (setq org-ref-completion-library 'org-ref-ivy-cite org-ref-get-pdf-filename-function
                'org-ref-get-pdf-filename-helm-bibtex org-ref-default-bibliography (list zot_bib)
                org-ref-bibliography-notes (concat org_notes "/bibnotes.org")
                org-ref-note-title-format
                "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
                org-ref-notes-directory org_notes org-ref-notes-function 'orb-edit-notes))
(setq bibtex-completion-notes-path org_notes bibtex-completion-bibliography zot_bib
      bibtex-completion-pdf-field "file" bibtex-completion-notes-template-multiple-files (concat
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
                                                                                          ":END:\n\n"))
(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config (setq orb-preformat-keywords '("=key=" "title" "url" "file" "author-or-editor"
                                         "keywords"))
  (setq orb-templates '(("r" "ref" plain (function org-roam-capture--get-point) ""
                         :file-name "${slug}"
                         :head
                         "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n- tags ::\n- keywords :: ${keywords}\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
                         :unnarrowed t))))

(defun +org/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil (mapcar (lambda (x)
                      (if (and (buffer-file-name x)
                               (string-match "\\.org$" (buffer-file-name x)))
                          (buffer-file-name x)))
                    (buffer-list))))
(setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(use-package! org-download
  :config
  (setq org-download-method 'directory)
  (setq org-download-image-dir "~/org/img")
  (setq-default org-download-heading-lvl "")
)

(after! org-mac-link
        (defun as-get-selected-finder-items ()
        (do-applescript (concat "tell application \"Finder\"\n" " set theSelection to the selection\n"
                                " set links to {}\n" " repeat with theItem in theSelection\n"
                                " set theLink to \"file+sys://\" & (POSIX path of (theItem as string)) & \"::split::\" & (get the name of theItem) & \"\n\"\n"
                                " copy theLink to the end of links\n" " end repeat\n"
                                " return links as string\n" "end tell\n")))
  )




;; Keymaps

(map! :leader
      (:prefix "TAB"
               "TAB" #'evil-switch-to-windows-last-buffer)
      (:prefix ("a" . "application")
               (:prefix ("o" . "org")
                        "f" #'org-roam-find-file
                        "/" #'helm-org-rifle))
      (:prefix "f"
       ("t"  #'treemacs))
      (:prefix "s"
       ("c"  #'evil-ex-nohighlight)))

(map! :prefix ","
      (:map emacs-lisp-mode-map
       :nv "f"
       #'elisp-format-buffer)
      (:map org-mode-map
       :nv "b" #'helm-bibtex
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
        :nv "a" #'org-archive-subtree)))
