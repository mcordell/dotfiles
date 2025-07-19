;;; meeting-creator --- Test
;;; Commentary:
;;; -*- lexical-binding: t; -*-

(require 'org)

(defvar qcentrix-meeting-file "~/org/qcentrix/meetings.org"
  "Org file where meeting headings are stored.")

(defun qcentrix--read-time (prompt)
  "Read a time string like “09:00” (adds “:00” if only an hour is given)."
  (let* ((raw (read-string prompt nil nil
                           (format-time-string "%H:%M")))
         ;; Accept “9”, “9:30”, “09”, etc.
         (parts (split-string raw ":"))
         (hour  (format "%02d" (string-to-number (car parts))))
         (mins  (if (cadr parts) (cadr parts) "00")))
    (concat hour ":" mins)))

;;;###autoload
(defun qcentrix-add-meetings ()
  "Prompt for a date, number of meetings, then loop to create headings.

Each heading has the form:

  * SUBJECT <YYYY-MM-DD HH:MM>

Entries are appended to `qcentrix-meeting-file`."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Select meeting date: "))
         (count (read-number "Number of meetings to add: " 1)))
    (with-current-buffer (find-file-noselect qcentrix-meeting-file)
      (goto-char (point-max))
      (dotimes (i count)
        (let* ((time (qcentrix--read-time
                      (format "Start time for meeting %d (HH or HH:MM): "
                              (1+ i))))
               (subject (read-string
                         (format "Subject for meeting %d: " (1+ i)))))
          (insert (format "* %s <%s %s>\n" subject date time))))
      (save-buffer))
    (message "Added %d meeting%s to %s"
             count (if (= count 1) "" "s") qcentrix-meeting-file)))

(provide 'meeting-creator)
;;; meeting-creator.el ends here
