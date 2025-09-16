;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Michael Cordell" user-mail-address "mike@mikecordell.com")

(load! "+ui")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/"
      org-agenda-files '("~/org/" "~/org/qcentrix/" "~/org/qcentrix/people/" "~/org/qcentrix/mro/" "~/org/qcentrix/mro/products/")
      zot_bib "~/org/mylibrary/mylibrary.bib" 
      )

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
        org-refile-targets '((+org/opened-buffer-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
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

(load! "+bindings")
