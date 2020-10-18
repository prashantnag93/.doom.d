(setq user-full-name "Prashant Kumar Nag"
      user-mail-address "prashantnag.workmail@gmail.com")

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq avy-all-windows t)

(setq doom-theme 'doom-vibrant)

(setq fancy-splash-image (concat doom-private-dir "banners/banner.png"))

(setq
 org_notes "~/Dropbox/org/"
 zot_bib "~/Dropbox/org/Notes/Mylib.bib"
 org-directory org_notes
 deft-directory org_notes
 org-roam-directory org_notes

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
