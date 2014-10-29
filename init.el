(require 'cask)
(cask-initialize)
(require 'pallet)

(require 'use-package)

;; visual settings

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "iv")
(load-theme 'minimal-light t)
(set-default-font "Input Sans 13")

;; packages

(use-package nyan-mode
  :init (progn
	  (nyan-mode)
	  (nyan-start-animation))
  :config (setq nyan-wavy-trail t))

(use-package aggressive-indent
  :init (global-aggressive-indent-mode))

(use-package helm
  :init (progn
	  (use-package helm-config)
	  (use-package helm-eshell)
	  (use-package helm-grep))
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)))

(use-package smartparens
  :init (use-package smartparens-config)
  :config (progn
	    (smartparens-global-mode)
	    (show-smartparens-global-mode)))

(use-package magit
  :bind (("C-, s" . magit-status)))

(use-package haskell-mode)

;; non-mode specific keys

(global-set-key (kbd "C-, e") 'eshell)
