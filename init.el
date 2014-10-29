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

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns))
	  (exec-path-from-shell-initialize)))

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
	    (show-smartparens-global-mode))
  :bind (("C-)" . sp-forward-slurp-sexp)))

(use-package magit
  :bind (("C-, s" . magit-status)))

(use-package haskell-mode
  :config (progn
	    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
	    (bind-keys :map haskell-mode-map
		       ("C-c C-l" . haskell-process-load-file)
		       ("C-c C-z" . haskell-interactive-switch)
		       ("C-c C-t" . haskell-interactive-do-type)
		       ("C-c C-i" . haskell-interactive-do-info))))

;; non-mode specific keys

(global-set-key (kbd "C-, e") 'eshell)

