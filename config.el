(setq user-full-name "Prashant Kumar Nag"
      user-mail-address "prashantnag.workmail@gmail.com")

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)                          ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words
;; IMO, modern editors have trained a bad habit into us all: a burning
;; need for completion all the time -- as we type, as we breathe, as we
;; pray to the ancient ones -- but how often do you *really* need that
;; information? I say rarely. So opt for manual completion:
(set-language-environment-charset "UTF-8")
(set-default-coding-systems 'utf-8)
(cd "~/")                                         ; Move to the user directory
(global-auto-revert-mode t)

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))
;; Buffer names modifications
(setq uniquify-buffer-name-style 'forward)
(setq which-key-idle-delay 0.5)
;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)
;; It will prompt for new buffer selection
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
;;Following code will show the windows preview
(setq +ivy-buffer-preview t)

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq avy-all-windows t)

(setq evil-goggles-duration 1
      evil-goggles-pulse t)

(beacon-mode 1)
(setq beacon-push-mark 10)
(setq beacon-blink-delay 0.3)
(setq beacon-blink-duration 0.3)
(setq beacon-color "#ae4cc7")

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(setq warning-minimum-level :emergency)

(after! org (setq org-hide-emphasis-markers t))

(setq ispell-dictionary "en-custom")
(setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

(setq doom-theme 'doom-vibrant)
(setq doom-font (font-spec :family "Ubuntu Mono" :size 16 )) ;;:weight 'semi-light
;; doom-variable-pitch-font (font-spec :family "Overpass" :size 18))
;; (setq doom-variable-pitch-font (font-spec :family "Overpass" :size 16))

(setq fancy-splash-image (concat doom-private-dir "banners/banner.png"))

(doom/set-frame-opacity 85)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Whether show the icon for major mode. It should respect `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

;;(setq deft-directory "~/Dropbox/org/roam/Notes/")
(setq deft-recursive t)
(setq deft-use-filename-as-title t
      deft-auto-save-interval -1.0)
(setq deft-current-sort-method 'title)

(setq
 org_notes "~/Dropbox/org/"
 ;; zot_bib "~/Desktop/exports/paperforwriting/PaperDraft/Paper1.bib"
 zot_bib "~/Dropbox/org/Mylib.bib"
 org-directory org_notes
 deft-directory org_notes
 org-roam-directory org_notes
 )

(after! org
  (setq org-default-notes-file "~/Dropbox/org/gtd/inbox.org")
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

(after! org (setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p!)" "NEXT(n!)" "SOMEDAY(s!)" "DELEGATED(e@/!)" "|" "DONE(d@/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "CANCELLED(c@/!)")
        (sequence "UNREAD(u!)" "READING(r!)" "READ(R@/!)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))

(after! org
  (setq org-ellipsis "⤵" ;;▾
        org-startup-folded t
        org-src-fontify-natively t))
(setq org-highlight-latex-and-related '(latex))

(defun pkn/find-or-create-olp (path &optional this-buffer)
  "Return a marker pointing to the entry at outline path OLP.
If anything goes wrong, throw an error, and if you need to do
something based on this error, you can catch it with
`condition-case'.
If THIS-BUFFER is set, the outline path does not contain a file,
only headings."
  (let* ((file (pop path))
         (level 1)
         (lmin 1)
         (lmax 1)
         (start (point-min))
         (end (point-max))
         found flevel)
    (unless (derived-mode-p 'org-mode)
      (error "Buffer %s needs to be in Org mode" buffer))
    (org-with-wide-buffer
     (goto-char start)
     (dolist (heading path)
       (let ((re (format org-complex-heading-regexp-format
                         (regexp-quote heading)))
             (cnt 0))
         (while (re-search-forward re end t)
           (setq level (- (match-end 1) (match-beginning 1)))
           (when (and (>= level lmin) (<= level lmax))
             (setq found (match-beginning 0) flevel level cnt (1+ cnt))))
         (when (> cnt 1)
           (error "Heading not unique on level %d: %s" lmax heading))
         (when (= cnt 0)
           ;; Create heading if it doesn't exist
           (goto-char end)
           (unless (bolp) (newline))
           (org-insert-heading nil nil t)
           (unless (= lmax 1) (org-do-demote))
           (insert heading)
           (setq end (point))
           (goto-char start)
           (while (re-search-forward re end t)
             (setq level (- (match-end 1) (match-beginning 1)))
             (when (and (>= level lmin) (<= level lmax))
               (setq found (match-beginning 0) flevel level cnt (1+ cnt))))))
       (goto-char found)
       (setq lmin (1+ flevel) lmax (+ lmin (if org-odd-levels-only 1 0)))
       (setq start found
             end (save-excursion (org-end-of-subtree t t))))
     (point-marker))))

(defun pkn/olp-current-buffer (&rest outline-path)
  "Find the OUTLINE-PATH of the current buffer."
  (let ((m (pkn/find-or-create-olp (cons (buffer-file-name) outline-path))))
    (set-buffer (marker-buffer m))
    (org-capture-put-target-region-and-position)
    (widen)
    (goto-char m)
    (set-marker m nil)))

(after! org
  (setq org-capture-templates
        `(("i" "Inbox" entry (file+headline "~/Dropbox/org/gtd/inbox.org" "Inbox")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %u"))
          ("e" "Inbox [mail]" entry (file+headline "~/Dropbox/org/gtd/inbox.org" "Email")
           ,(concat "* TODO Process: \"%a\" %?\n"
                    "/Entered on/ %u"))
          ("c" "org-protocol-capture" entry (file "~/Dropbox/org/gtd/inbox.org")
           "* TODO [[%:link][%:description]]\n\n %i"
           :immediate-finish t)
          ("m" "Metacognition")
          ("mq" "Questions" entry (function ,(lambda ()
                                               (pkn/olp-current-buffer "Metacognition" "Questions")))
           ,(concat "* TODO Q: %?\n"
                    "/Entered on/ %u"))
          ("mn" "Notes" entry (function ,(lambda ()
                                           (pkn/olp-current-buffer "Metacognition" "Notes")))
           "* %?\n")))
  )

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
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n\n"
           :immediate-finish t
           :unnarrowed t)
          ("p" "phd" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "phd/%<%Y%m%d%H%M%S>-${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n\n"
           :immediate-finish t
           :unnarrowed t)))

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+roam_key: ${ref}
#+roam_tags: website
#+title: ${title}

- source :: ${ref}"
           :unnarrowed t)))
  (set-company-backend! 'org-mode '(company-capf))
  )
;; Following code sets the org-roam-dailies
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%A, %d %B %Y>\n\n"
         :olp ("General"))

        ("m" "morning Entry" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%A, %d %B %Y>\n\n"
         :olp ("Morning Entry"))

        ("j" "journal Entry" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%A, %d %B %Y>\n\n"
         :olp ("Journal"))))

(use-package! org-ref
  ;; :init
                                        ; code to run before loading org-ref
  :config
  (setq
   org-ref-completion-library 'org-ref-ivy-cite
   org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
   org-ref-default-bibliography (list zot_bib)
   org-ref-bibliography-notes (concat org_notes "/bibnotes.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory (concat org_notes "/literature")
   org-ref-notes-function 'orb-edit-notes
   )
  (defun pkn/org-ref-open-in-scihub ()
    "Open the bibtex entry at point in a browser using the url field or doi field.
Not for real use, just here for demonstration purposes."
    (interactive)
    (let ((doi (org-ref-get-doi-at-point)))
      (when doi
        (if (string-match "^http" doi)
            (browse-url doi)
          (browse-url (format "http://sci-hub.se/%s" doi)))
        (message "No url or doi found"))))
  (add-to-list 'org-ref-helm-user-candidates '("Open in Sci-hub" . org-ref-open-in-scihub))
  )

(after! org-ref
  (setq
   bibtex-completion-notes-path (concat org_notes "/literature")
   bibtex-completion-bibliography zot_bib
   bibtex-completion-pdf-field "file"
   bibtex-completion-notes-template-multiple-files
   (concat
    "#+TITLE: ${title}\n"
    "#+ROAM_KEY: cite:${=key=}\n"
    "* TODO Notes\n"
    ":PROPERTIES:\n"
    ":Custom_ID: ${=key=}\n"
    ":NOTER_DOCUMENT: ${file}\")\n"
    ":AUTHOR: ${author-abbrev}\n"
    ":JOURNAL: ${journaltitle}\n"
    ":DATE: ${date}\n"
    ":YEAR: ${year}\n"
    ":DOI: ${doi}\n"
    ":URL: ${url}\n"
    ":END:\n\n"
    )
   )
  )

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "file" "author-or-editor" "keywords")
        orb-process-file-field t
        orb-file-field-extensions "pdf")
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "literature/${citekey}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n

- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${citekey}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: ${file}\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

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

(use-package! org-roam-server)
(defun org-roam-server-open ()
  "Ensure the server is active, then open the roam graph."
  (interactive)
  (smartparens-global-mode -1)
  (org-roam-server-mode 1)
  (browse-url-firefox (format "http://localhost:%d" org-roam-server-port))
  (smartparens-global-mode 1))

(after! org-roam
  (smartparens-global-mode -1)
  (org-roam-server-mode)
  (smartparens-global-mode 1))

(after! org (setq org-ditaa-jar-path "~/.emacs.d/.local/straight/repos/org-mode/contrib/scripts/ditaa.jar"))

(with-eval-after-load 'org
  ;; ... bunch of other org configurations ...
  ;; Org-transclusion
  (define-key global-map (kbd "<f12>") #'org-transclusion-mode))

;; ... other configurations ...
(use-package! org-transclusion
  :load-path "~/code/github-cloned/org-transclusion")

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2020/bin/x86_64-linux"))
(setq exec-path (append exec-path '("/usr/local/texlive/2020/bin/x86_64-linux")))

;; (setq org-latex-pdf-process
;;       '("latexmk -shell-escape -interaction=nonstopmode -f -pdf -output-directory=%o %f"))
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

;; add some latex class for article
(add-to-list 'org-latex-classes '("Springer"
                                  "\\documentclass[natbib]{svjour3}"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

'(org-preview-latex-process-alist
  (quote
   ((dvipng :programs
            ("lualatex" "dvipng")
            :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
            (1.0 . 1.0)
            :latex-compiler
            ("lualatex -output-format dvi -interaction nonstopmode -output-directory %o %f")
            :image-converter
            ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
    (dvisvgm :programs
             ("latex" "dvisvgm")
             :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
             (1.7 . 1.5)
             :latex-compiler
             ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvisvgm %f -n -b min -c %S -o %O"))
    (imagemagick :programs
                 ("latex" "convert")
                 :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                 (1.0 . 1.0)
                 :latex-compiler
                 ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
                 :image-converter
                 ("convert -density %D -trim -antialias %f -quality 100 %O")))))

(setq org-file-apps
  '((auto-mode . emacs)
    ("\\.mm\\'" . default)
    ("\\.x?html?\\'" . default)
    ("\\.pdf\\'" . emacs) ;; another option: "okular --unique file:%s"
    ("\\.png\\'" . viewnior)
    ("\\.jpg\\'" . viewnior)
    ))

(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

(use-package! company-math
  :after (:any org-mode TeX-mode)
  :config
  (set-company-backend! 'org-mode 'company-math-symbols-latex)
  (set-company-backend! 'TeX-mode 'company-math-symbols-latex)
  (set-company-backend! 'org-mode 'company-latex-commands)
  (set-company-backend! 'TeX-mode 'company-latex-commands)
  (setq company-tooltip-align-annotations t)
  (setq company-math-allow-latex-symbols-in-faces t))

(add-hook 'org-mode-hook 'org-fragtog-mode)

(use-package! auto-activating-snippets
  :hook (LaTeX-mode . auto-activating-snippets-mode)
  :config (require 'latex-auto-activating-snippets))

(use-package! latex-auto-activating-snippets
  :config
  (defun als-tex-fold-maybe ()
    (unless (equal "/" als-transient-snippet-key)
      (+latex-fold-last-macro-a)))
  (add-hook 'aas-post-snippet-expand-hook #'als-tex-fold-maybe))

(use-package! org-pandoc-import :after org)

(use-package! ox-word
  :after (:all org-ref ox)
  :config)
