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
