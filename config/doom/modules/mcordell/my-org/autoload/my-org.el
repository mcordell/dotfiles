;;;###autoload
(defun mcordell/normalize-name-to-tag (name)
  "Convert NAME to a tag-friendly string, e.g. \"Brian\" -> \"brian\"."
  (let* ((lower (downcase name)))
    (replace-regexp-in-string "[^a-z0-9_@#%]+" "_" lower)))

;;;###autoload
(defun mcordell/agenda-one-on-one ()
  "Prompt for a 1:1 name, then show open TODOs tagged with that person."
  (interactive)
  (let* ((names (mapcar #'car mcordell/one-on-one-list))
         (choice (completing-read "1:1 with: " names nil t))
         (when-str (or (cdr (assoc choice mcordell/one-on-one-list)) ""))  ; optional, for header
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

;;;###autoload
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

;;;###autoload
(defun mcordell/create-one-on-one-heading (name)
  (let* ((day-time (cdr (assoc name mcordell/one-on-one-list)))
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

;;;###autoload
(defun mcordell/org-agenda-todo-with-tag ()
  (interactive)
  (let* ((tags-list '("jamaal" "chris" "andrew" "brian" "teo" "brad" "brad-b")) ; Define your list of tags here
         (tag (completing-read "Choose tag: " tags-list)))
    (org-tags-view nil (concat "TODO=\"TODO\"&" tag))))

;;;###autoload
(defun mcordell/create-one-on-one-heading-with-prompt ()
  (let* ((name (completing-read "Select a name: " (mapcar 'car mcordell/one-on-one-list)))
         (heading (mcordell/create-one-on-one-heading name)))
    heading))

;;;###autoload
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

;;;###autoload
(defun +org/org-pass-link-to-system  ()
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
