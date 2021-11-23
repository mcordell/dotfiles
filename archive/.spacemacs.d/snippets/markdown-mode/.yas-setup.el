(defun link-ref()
  (elt (data-from-clipboard) 0)
  )

(defun line-start()
  (string-to-number (elt (data-from-clipboard) 1))
  )

(defun line-end()
  (string-to-number (elt (data-from-clipboard) 2))
  )

(defun data-from-clipboard()
  (extract-data (substring-no-properties (current-kill 0))))

(defun extract-data(url)
(save-match-data
  (and
   (string-match "github.com/\\([^/]*/[^/]*/blob/[a-f0-9]*/.*\\)#L\\([0-9]*\\)\\(-L\\)?\\([0-9]*\\)?" url)
   (list (match-string 1 url)
         (match-string 2 url)
         (match-string 4 url)
         )))
)
