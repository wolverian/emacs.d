(require 'cask)
(cask-initialize)
(require 'use-package)

;; settings

(tool-bar-mode -1)
(scroll-bar-mode -1)
(load-theme 'minimal-light t)
(set-default-font "Input Sans 13")

;; packages

(use-package nyan-mode
  :config (progn
	    (nyan-mode)
	    (nyan-start-animation)	   
	    (setq nyan-wavy-trail t)))

(use-package aggressive-indent
  :config (global-aggressive-indent-mode))

(use-package helm
  :config (progn
            (use-package helm-config)
	    (use-package helm-eshell)
	    (use-package helm-files)
	    (use-package helm-grep))
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)))

(use-package smartparens
  :config (progn
	    (use-package smartparens-config)
	    (smartparens-global-mode)
	    (show-smartparens-global-mode)))

(use-package magit
  :bind (("C-, s" . magit-status)))

;; non-mode specific keys

(global-set-key (kbd "C-, e") 'eshell)
