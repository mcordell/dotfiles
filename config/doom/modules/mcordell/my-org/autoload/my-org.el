;;;###autoload
(defun mcordell/normalize-name-to-tag (name)
  "Convert NAME to a tag-friendly string, e.g. \"Brian\" -> \"brian\"."
  (let* ((lower (downcase name)))
    (replace-regexp-in-string "[^a-z0-9_@#%]+" "_" lower)))

;;;###autoload
(defun mcordell/agenda-one-on-one-for-name (name)
  "Show open TODOs tagged with the specified NAME."
  (let* ((when-str (or (cdr (assoc name mcordell/one-on-one-list)) ""))  ; optional, for header
         (tag (mcordell/normalize-name-to-tag name))
         ;; Localize agenda files only for this command:
         (org-agenda-files (directory-files-recursively mcordell/one-on-one-files-dir "\\.org\\'"))
         ;; Nice header
         (org-agenda-overriding-header
          (format "Open TODOs for %s  %s" name
                  (if (string-empty-p when-str) "" (format "(%s)" when-str)))))
    ;; Show only TODO entries with the selected tag:
    ;; org-tags-view: first arg non-nil => TODO-only
    (org-tags-view t (concat "+" tag))))

;;;###autoload
(defun mcordell/agenda-one-on-one ()
  "Prompt for a 1:1 name, then show open TODOs tagged with that person."
  (interactive)
  (let* ((names (mapcar #'car mcordell/one-on-one-list))
         (choice (completing-read "1:1 with: " names nil t)))
    (mcordell/agenda-one-on-one-for-name choice)))

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
(defun mcordell/create-simple-one-on-one-heading-with-prompt ()
  (let* ((name (completing-read "Select a name: " (mapcar #'car mcordell/one-on-one-list))))
    (mcordell/create-simple-one-on-one-heading name)))

;;;###autoload
(defun mcordell/create-simple-one-on-one-heading (name)
    (concat "* " name " 1-1 %^t"))

;;;###autoload
(defun mcordell/round-time-up ()
  "Round current time up to next 30-minute or hour boundary.
Examples: 10:15 -> 10:30, 10:36 -> 11:00"
  (let* ((now (decode-time))
         (current-hour (nth 2 now))
         (current-min (nth 1 now))
         (target-hour current-hour)
         (target-min 0))
    (cond
     ((= current-min 0) 
      (setq target-min 30))
     ((<= current-min 30) 
      (setq target-min 30))
     ((> current-min 30) 
      (setq target-hour (1+ current-hour)
            target-min 0)))
    (format-time-string "%Y-%m-%d %a %H:%M" 
                        (encode-time 0 target-min target-hour
                                     (nth 3 now) (nth 4 now) (nth 5 now)))))

;;;###autoload
(defun mcordell/create-meeting-heading-directly (name)
  "Create a meeting heading directly in work-meeting-file with rounded time."
  (let* ((heading (format "* %s 1-1 <%s>" name (mcordell/round-time-up)))
         (file-buffer (find-file-noselect mcordell/work-meeting-file)))
    (with-current-buffer file-buffer
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert heading "\n")
      (save-buffer))
    ;; Return the position for focusing
    (list mcordell/work-meeting-file (with-current-buffer file-buffer (point)))))

;;;###autoload
(defun mcordell/read-time (prompt)
  "Read a time string like “09:00” (adds “:00” if only an hour is given)."
  (let* ((raw (read-string prompt nil nil
                           (format-time-string "%H:%M")))
         ;; Accept “9”, “9:30”, “09”, etc.
         (parts (split-string raw ":"))
         (hour  (format "%02d" (string-to-number (car parts))))
         (mins  (if (cadr parts) (cadr parts) "00")))
    (concat hour ":" mins)))

;;;###autoload
(defun mcordell/add-meetings-for-day ()
  "Prompt for a date, number of meetings, then loop to create headings.

Each heading has the form:

  * SUBJECT <YYYY-MM-DD HH:MM>

Entries are appended to `mcordell/work-meeting-file`."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Select meeting date: "))
         (count (read-number "Number of meetings to add: " 1)))
    (with-current-buffer (find-file-noselect mcordell/work-meeting-file)
      (goto-char (point-max))
      (dotimes (i count)
        (let* ((time (mcordell/read-time
                      (format "Start time for meeting %d (HH or HH:MM): "
                              (1+ i))))
               (subject (read-string
                         (format "Subject for meeting %d: " (1+ i)))))
          (insert (format "* %s <%s %s>\n" subject date time))))
      (save-buffer))
    (message "Added %d meeting%s to %s"
             count (if (= count 1) "" "s") mcordell/work-meeting-file)))

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
    (directory-files-recursively "~/org/work/people/" "\\.org$" nil)
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
    (directory-files-recursively "~/org/work/people/" "\\.org$" nil)
    ))
  )

;;;###autoload
(defun mcordell/one-on-one-workflow ()
  "Start a one-on-one workflow: select person, find or create meeting, show agenda."
  (interactive)
  (let* ((names (mapcar #'car mcordell/one-on-one-list))
         (chosen-name (completing-read "1:1 with: " names nil t))
         (today-date (format-time-string "%Y-%m-%d"))
         (meeting-heading-pattern (format "^\\* %s 1-1 <%s" chosen-name today-date))
         (org-files (directory-files-recursively mcordell/one-on-one-files-dir "\\.org\\'"))
         (found-meeting nil))
    
    ;; Search for existing meeting today
    (catch 'meeting-found
      (dolist (file org-files)
        (with-current-buffer (find-file-noselect file)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward meeting-heading-pattern nil t)
              (setq found-meeting (list file (point)))
              (throw 'meeting-found t))))))
    
    (if found-meeting
        ;; Found existing meeting - focus on it
        (progn
          (find-file (car found-meeting))
          (goto-char (cadr found-meeting))
          (org-show-subtree)
          (message "Found existing meeting for %s today" chosen-name))
      ;; No existing meeting - create new one directly
      (progn
        (let ((meeting-info (mcordell/create-meeting-heading-directly chosen-name)))
          (find-file (car meeting-info))
          (goto-char (cadr meeting-info))
          (org-show-subtree))
        (message "Created new meeting for %s" chosen-name)))
    
    ;; After meeting is set up, create vertical split with agenda view
    (split-window-right)
    (other-window 1)
    (mcordell/agenda-one-on-one-for-name chosen-name)))
