(setq user-full-name "Prashant Kumar Nag"
      user-mail-address "prashantnag.workmail@gmail.com")

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq avy-all-windows t)

(setq doom-theme 'doom-vibrant)

(setq fancy-splash-image (concat doom-private-dir "banners/banner.png"))
