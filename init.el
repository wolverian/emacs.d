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
