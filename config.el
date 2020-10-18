(setq user-full-name "Prashant Kumar Nag"
      user-mail-address "prashantnag.workmail@gmail.com")

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq avy-all-windows t)

(setq doom-theme 'doom-vibrant)

(setq fancy-splash-image (concat doom-private-dir "banners/banner.png"))

;;(setq deft-directory "~/Dropbox/org/roam/Notes/")
(setq deft-recursive t)
(setq deft-use-filename-as-title t
      deft-auto-save-interval -1.0)
(setq deft-current-sort-method 'title)

(setq
 org_notes "~/Dropbox/org/"
 zot_bib "~/Dropbox/org/Notes/Mylib.bib"
 org-directory org_notes
 deft-directory org_notes
 org-roam-directory org_notes
 org-default-notes-file (concat org_notes "inbox.org"))

(after! org

  (setq +org-capture-todo-file org-default-notes-file
        +org-capture-notes-file org-default-notes-file
        +org-capture-projects-file org-default-notes-file)

  (setq org-log-done 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil))

(setq org-tag-alist (quote ((:startgrouptag)
                            ("Context")
                            (:grouptags)
                            ("@errand" . ?e)
                            ("@manit" . ?m)
                            ("@home" . ?h)
                            (:endgrouptag)
                            (:startgrouptag)
                            ("Use this")
                            (:grouptags)
                            ("?phone" . ?p)
                            ("?laptop" . ?l)
                            (:endgrouptag)
                            (:startgrouptag)
                            ("Energy")
                            (:grouptags)
                            ("Challange" . ?1)
                            ("Average" . ?2)
                            ("Easy" . ?3)
                            (:endgrouptag)
                            (:startgrouptag)
                            ("Time")
                            (:grouptags)
                            ("15min" . ?<)
                            ("30min" . ?=)
                            ("1hr" . ?>)
                            (:endgrouptag)
                            (:startgrouptag)
                            ("Related")
                            (:grouptags)
                            ("#PhD" . ?P)
                            ("#coding" . ?C)
                            ("#knowledge" . ?K)
                            (:endgrouptag)
                            (:startgrouptag)
                            ("Status")
                            (:grouptags)
                            ("WAITING" . ?w)
                            ("HOLD" . ?H)
                            ("CANCELLED" . ?c)
                            (:endgrouptag)
                            (:startgrouptag . nil)
                            ("Category")
                            (:grouptags . nil)
                            ("Hobby")
                            ("Health")
                            ("House")
                            ("Bike")
                            ("Bills")
                            (:endgrouptag . nil))))

(after! org
  (setq org-ellipsis "⤵" ;;▾
        org-startup-folded t
        org-src-fontify-natively t))
(setq org-highlight-latex-and-related '(latex))

(after! org-roam
  (setq ;;org-roam-graph-viewer "/usr/bin/open"
        ;;org-roam-completion-system 'default
        ;;org-roam-link-title-format "§:%s"
        +org-roam-open-buffer-on-find-file nil
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "private"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t
        org-roam-graph-executable "/usr/bin/neato" ;; instead of 'dot' we can use 'neato' also
)
(setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}

- source :: ${ref}"
           :unnarrowed t)))
(setq org-roam-capture-templates
'(("l" "lit" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "literature/${slug}"
           :head "#+hugo_slug: ${slug}
#+title: ${title}\n"
           :unnarrowed t)
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
)


;; Interactive Org Roam Server Graph
;; (require 'simple-httpd)
;; (setq httpd-root "/var/www")
;; (httpd-start)

(after! org-journal
   (setq
    org-journal-date-prefix "#+TITLE:"
   org-journal-date-format "%A, %d %B %Y"
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-dir (file-truename "~/Dropbox/org/private/")
   org-journal-enable-encryption nil
   org-journal-enable-agenda-integration t))

(use-package! org-noter
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   ;;org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)
   )
  )

;; Actually start using templates
(after! org-capture
  ;; Firefox
  (add-to-list 'org-capture-templates
               '("P" "Protocol" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
                 :prepend t
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n"
                 :prepend t
                 :kill-buffer t))
  ;; Misc
  (add-to-list 'org-capture-templates
         '("a"               ; key
           "Article"         ; name
           entry             ; type
           (file+headline "~/Dropbox/org/Notes/consolidated.org" "Article")  ; target
           "* %^{Title} %(org-set-tags-command)  :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"  ; template
           :prepend t        ; properties
           :empty-lines 1    ; properties
           :created t        ; properties
           ))
)
;;

(use-package! org-protocol-capture-html
  :after org-protocol
  :config
  (add-to-list 'org-capture-templates
               '("w"
                 "Web site"
                 entry
                 (file+headline +org-capture-notes-file "Website")  ; target
                 "* %a :website:\n\n%U %?\n\n%:initial")
               )
  )
