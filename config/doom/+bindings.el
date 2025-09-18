;; Keymaps
(map! :leader
      (:prefix "TAB"
               "TAB" #'evil-switch-to-windows-last-buffer)
      (:prefix "f"
               ("a" (lambda ()
                      (interactive)
                      (org-agenda nil "w")))
               ("t"  #'treemacs)
               ("w"  #'+default/search-project)
               )
      (:prefix "s"
               ("c"  #'evil-ex-nohighlight))
      (:prefix "m"
               (:map org-mode-map
                :nv "j" #'mcordell/insert-rrp-jira
                )
               )
      (:prefix "o"
               ("g" #'mcordell/one-on-one-workflow)
               ("c" #'cfw:open-org-calendar)
               ("w" (lambda ()
                      (interactive)
                      (org-agenda nil "o")))
               )
      )

(map! :prefix ","
      (:map emacs-lisp-mode-map
       :nv "f"
       #'elisp-format-buffer)
      :nv "t" #'projectile-toggle-between-implementation-and-test
      (:map org-mode-map
       :nv "o" #'org-open-at-point
       :nv "b" #'helm-bibtex
       :nv "e" #'org-mac-mailmate-insert-selected
       :nv "f" #'org-mac-finder-item-get-selected
       :nv "l" #'org-mac-link-brave-insert-frontmost-url
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
                :nv "m" #'alchemist-mix)))
