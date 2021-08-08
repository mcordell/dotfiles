(message "LOADED")
;; OVERRIDE ox-hugo front matter function to output tags
(defun org-hugo--get-front-matter (info)
  "Return the Hugo front-matter string.
INFO is a plist used as a communication channel."
  ;; (message "[hugo front-matter DBG] info: %S" (pp info))
  (let* ((fm-format (plist-get info :hugo-front-matter-format))
         (author-list (and (plist-get info :with-author)
                           (let ((author-raw
                                  (org-string-nw-p
                                   (org-export-data (plist-get info :author) info)))) ;`org-export-data' required
                             (when author-raw
                               ;; Multiple authors can be comma or
                               ;; newline separated. The newline
                               ;; separated authors work only for the
                               ;; #+author keyword; example:
                               ;;   #+author: Author1
                               ;;   #+author: Author2
                               ;;
                               ;; If using the subtree properties they
                               ;; need to be comma-separated:
                               ;;   :EXPORT_AUTHOR: Author1, Author2
                               (let ((author-list-1 (org-split-string author-raw "[,\n]")))
                                 ;; Don't allow spaces around author names.
                                 ;; Also remove duplicate authors.
                                 (delete-dups (mapcar #'org-trim author-list-1)))))))
         (creator (and (plist-get info :with-creator)
                       (plist-get info :creator)))
         (locale (and (plist-get info :hugo-with-locale)
                      (org-hugo--get-lang info)))
         (description (org-string-nw-p (plist-get info :description)))
         (aliases-raw (let ((aliases-raw-1 (org-string-nw-p (plist-get info :hugo-aliases))))
                        (when aliases-raw-1
                          (org-split-string aliases-raw-1 " "))))
         (aliases (let (alias-list)
                    (dolist (alias aliases-raw)
                      (unless (string-match-p "/" alias)
                        (let ((section (file-name-as-directory ;Suffix section with "/" if it isn't already
                                        (org-export-data (plist-get info :hugo-section) info))))
                          (setq alias (concat "/" section alias))))
                      (setq alias-list (append alias-list `(,alias))))
                    alias-list))
         (outputs-raw (org-string-nw-p (plist-get info :hugo-outputs)))
         (outputs (when outputs-raw
                    (org-split-string outputs-raw " ")))
         (draft (org-hugo--parse-draft-state info))
         (headless (when (org-hugo--plist-get-true-p info :hugo-headless)
                     (org-hugo--front-matter-value-booleanize (org-hugo--plist-get-true-p info :hugo-headless))))
         (all-t-and-c-str (org-roam--file-keyword-get "FILETAGS"))
         (all-t-and-c (when (stringp all-t-and-c-str)
                        (org-split-string all-t-and-c-str ":")))
         (tags (or
                ;; Look for tags set using HUGO_TAGS keyword, or
                ;; EXPORT_HUGO_TAGS property if available.
                (org-hugo--delim-str-to-list (plist-get info :hugo-tags))
                ;; Else use Org tags (the ones set in headlines
                ;; and/or inherited) if any.
                (let* ((tags-list (cl-remove-if #'org-hugo--category-p all-t-and-c))
                       (tags-list (dolist (fn org-hugo-tag-processing-functions tags-list)
                                    (setq tags-list (funcall fn tags-list info)))))
                  ;; (message "[get fm DBG] tags: tags-list = %S" tags-list)
                  tags-list)))
         (categories (or
                      ;; Look for categories set using HUGO_CATEGORIES
                      ;; keyword, or EXPORT_HUGO_CATEGORIES property
                      ;; if available.
                      (org-hugo--delim-str-to-list (plist-get info :hugo-categories))
                      ;; Else use categories set using Org tags with
                      ;; "@" prefix (the ones set in headlines and/or
                      ;; inherited) if any.
                      (let* ((categories-list (cl-remove-if-not #'org-hugo--category-p all-t-and-c))
                             (categories-list (dolist (fn org-hugo-tag-processing-functions categories-list)
                                                (setq categories-list (funcall fn categories-list info))))
                             (categories-list (mapcar (lambda (str)
                                                        ;; Remove "@" from beg of categories.
                                                        (replace-regexp-in-string "\\`@" "" str))
                                                      categories-list)))
                        ;; (message "dbg: categories: categories-list = %s" categories-list)
                        categories-list)))
         (keywords (org-hugo--delim-str-to-list (plist-get info :keywords)))
         (weight-data (let ((wt-raw-list (org-hugo--parse-property-arguments (plist-get info :hugo-weight)))
                            weight-data-1)
                        (dolist (wt-raw wt-raw-list)
                          (let (key value)
                            ;; (message "weight DBG wt-raw: %S" wt-raw)
                            ;; (message "weight DBG cdr wt-raw: %S" (cdr wt-raw))
                            ;; (message "weight DBG org-hugo--subtree-coord: %S" org-hugo--subtree-coord)
                            (cond
                             ((null (cdr wt-raw)) ;`wt-raw' is not of the type (TAXONOMY . WEIGHT)
                              (setq key 'weight)
                              (setq value (cond
                                           ((and org-hugo--subtree-coord
                                                 (equal (car wt-raw) 'auto)) ;(auto)
                                            (org-hugo--calc-weight))
                                           ((and (equal (car wt-raw) 'auto) ;Auto weight ineffective for file-based exports
                                                 (null org-hugo--subtree-coord))
                                            nil)
                                           (t
                                            (string-to-number (symbol-name (car wt-raw)))))))
                             (t
                              (setq key (if (equal (car wt-raw) 'page) ;`wt-raw' is of the type (page . WEIGHT)
                                            'weight
                                          (intern (format "%s_weight" (symbol-name (car wt-raw))))))
                              (setq value (cond
                                           ((and org-hugo--subtree-coord
                                                 (equal (cdr wt-raw) "auto")) ;(TAXONOMY . "auto") or (page . "auto")
                                            (org-hugo--calc-weight))
                                           ((numberp (cdr wt-raw))
                                            (cdr wt-raw))
                                           ((and (equal (cdr wt-raw) "auto") ;Auto weight ineffective for file-based exports
                                                 (null org-hugo--subtree-coord))
                                            nil)
                                           (t
                                            (user-error "Ox-hugo: Invalid weight %S" (cdr wt-raw)))))))
                            ;; (message "weight DBG key: %S" key)
                            ;; (message "weight DBG value: %S" value)
                            (push (cons key value) weight-data-1)))
                        ;; (message "weight DBG weight-data: %S" weight-data-1)
                        weight-data-1))
         (menu-alist (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu)))
         (menu-alist-override (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu-override)))
         ;; If menu-alist-override is non-nil, update menu-alist with values from that.
         (menu-alist (let ((updated-menu-alist menu-alist))
                       (dolist (override-prop menu-alist-override)
                         (let* ((override-key (car override-prop))
                                (override-val (cdr override-prop))
                                (matching-prop (assoc override-key updated-menu-alist)))
                           (if matching-prop
                               (setcdr matching-prop override-val)
                             (push override-prop updated-menu-alist))))
                       updated-menu-alist))
         (custom-fm-data (org-hugo--parse-property-arguments (plist-get info :hugo-custom-front-matter)))
         (resources (org-hugo--get-resources-alist
                     (org-hugo--parse-property-arguments (plist-get info :hugo-resources))))
         (blackfriday (org-hugo--parse-blackfriday-prop-to-alist (plist-get info :hugo-blackfriday)))
         (data `(;; The order of the elements below will be the order in which the front-matter
                 ;; variables will be ordered.
                 (title . ,(org-hugo--get-sanitized-title info))
                 (audio . ,(plist-get info :hugo-audio))
                 (author . ,author-list)
                 (description . ,description)
                 (date . ,(org-hugo--format-date :date info))
                 (publishDate . ,(org-hugo--format-date :hugo-publishdate info))
                 (expiryDate . ,(org-hugo--format-date :hugo-expirydate info))
                 (aliases . ,aliases)
                 (images . ,(org-hugo--delim-str-to-list (plist-get info :hugo-images)))
                 (isCJKLanguage . ,(org-hugo--plist-get-true-p info :hugo-iscjklanguage))
                 (keywords . ,keywords)
                 (layout . ,(plist-get info :hugo-layout))
                 (lastmod . ,(org-hugo--format-date :hugo-lastmod info))
                 (linkTitle . ,(plist-get info :hugo-linktitle))
                 (markup . ,(plist-get info :hugo-markup))
                 (outputs . ,outputs)
                 (series . ,(org-hugo--delim-str-to-list (plist-get info :hugo-series)))
                 (slug . ,(plist-get info :hugo-slug))
                 (tags . ,tags)
                 (categories . ,categories)
                 (type . ,(plist-get info :hugo-type))
                 (url . ,(plist-get info :hugo-url))
                 (videos . ,(org-hugo--delim-str-to-list (plist-get info :hugo-videos)))
                 (draft . ,draft)
                 (headless . ,headless)
                 (creator . ,creator)
                 (locale . ,locale)
                 (blackfriday . ,blackfriday)
                 (menu . ,menu-alist)
                 (resources . ,resources)))
         (data `,(append data weight-data custom-fm-data))
         ret)
    ;; (message "[get fm DBG] tags: %s" tags)
    ;; (message "dbg: hugo tags: %S" (plist-get info :hugo-tags))
    ;; (message "[get fm info DBG] %S" info)
    ;; (message "[get fm blackfriday DBG] %S" blackfriday)
    ;; (message "[get fm menu DBG] %S" menu-alist)
    ;; (message "[get fm menu override DBG] %S" menu-alist-override)
    ;; (message "[custom fm data DBG] %S" custom-fm-data)
    ;; (message "[fm resources OUT DBG] %S" resources)
    ;; (message "[fm data DBG] %S" data)
    ;; (message "[fm tags DBG] %S" tags)
    ;; (message "[fm categories DBG] %S" categories)
    ;; (message "[fm keywords DBG] %S" keywords)
    (setq data (org-hugo--replace-keys-maybe data info))
    (setq ret (org-hugo--gen-front-matter data fm-format))
    (if (and (string= "toml" fm-format)
             (org-hugo--pandoc-citations-enabled-p info))
        ;; Pandoc parses fields like csl and nocite from YAML
        ;; front-matter.  So create the `org-hugo--fm-yaml'
        ;; front-matter in YAML format just for Pandoc.
        (setq org-hugo--fm-yaml (org-hugo--gen-front-matter data "yaml"))
      (setq org-hugo--fm-yaml ret))
    ret))
