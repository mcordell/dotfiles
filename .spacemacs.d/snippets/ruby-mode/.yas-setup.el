(defun camelize-list (list)
  (let (value)  ; make sure list starts empty
    (while list
      (add-to-list 'value (custom-fixes (snake-to-camel (car list))) t)
      (setq list (cdr list)))
    value))

(defun snake-to-camel (s)
  ""
  (setq s (downcase s))
  (if (string-match "_" s)
      (concat (let ((fs (substring s 0 (match-beginning 0))))
                (concat (upcase (substring fs 0 1)) (substring fs 1 (length fs))))
              (snake-to-camel (substring s (match-end 0) (length s))))
    (concat (upcase (substring s 0 1)) (substring s 1 (length s)))))


(defun camel-to-snake (s)
  ""
  (save-match-data
    (let ((case-fold-search nil))
      (camel-to-snake-recursively s))))


(defun camel-to-snake-recursively (s)
  ""
  (let ((ss (downcase-first s)))
    (if (string-match "[A-Z]" ss)
        (concat (substring ss 0 (match-beginning 0)) "_" (camel-to-snake-recursively (substring ss (match-beginning 0) (length ss))))
      ss)))


(defun upcase-first (s)
  ""
  (concat
   (upcase (substring s 0 1))
   (substring s 1 (length s))))


(defun downcase-first (s)
  ""
  (concat
   (downcase (substring s 0 1))
   (substring s 1 (length s))))


(defun name-space (filename)
  (string-join (butlast (all-namespace-parts filename)) "::")
)

(defun class-name (filename)
  (car (last (all-namespace-parts filename)))
  )

(defun spec-name-space (filename)
  (string-join (cdr (butlast (all-namespace-parts filename))) "::")
  )

(defun spec-class-name (filename)
  (replace-regexp-in-string "Spec" "" (class-name filename))
  )

(defun all-namespace-parts (filename)
  (camelize-list
   (split-string
    (remove-lib-directory
     (remove-project-directory
      (file-name-sans-extension filename))) "/"))
)

(defun remove-lib-directory (filepath)
  "docstring"
      (replace-regexp-in-string "lib/" "" filepath)
  )

(defun custom-fixes (piece)
  (replace-regexp-in-string "Icd" "ICD"
    (replace-regexp-in-string "Pci" "PCI"
      (replace-regexp-in-string "Ncdr" "NCDR" piece)))
  )

(defun remove-project-directory (filepath)
  (replace-regexp-in-string "/proj/" "" filepath)
  )
