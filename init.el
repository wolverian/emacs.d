(require 'cask)
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'use-package)

;; visual settings

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "iv")
(load-theme 'minimal-light t)
(set-default-font "Input Sans Narrow-13")

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
	 ("C-x C-f" . helm-find-files)
	 ("M-i" . helm-show-kill-ring))
  :config (bind-keys :map helm-map
		     ("<tab>" . helm-execute-persistent-action)
		     ("C-i" . helm-execute-persistent-action)
		     ("C-z" . helm-select-action)))

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

(use-package factor-mode
  :config (progn
	    (setq fuel-factor-root-dir "/Users/iv/Applications/factor")))

;; non-mode specific keys

(use-package eldoc
  :init (eldoc-mode))

(bind-keys
 ("C-, e" . eshell)
 ("C-, p" . list-packages))

;; fonts

;; mono / variable width font
(make-face 'default-fixed)
(make-face 'default-variable)

(set-face-font 'default-fixed "Input Mono Narrow-13")
(set-face-font 'default-variable "Input Sans Narrow-13")

(add-hook 'tabulated-list-mode-hook (lambda () (buffer-face-set 'default-fixed)))

;; magit key mode doesn't have hooks
(defadvice magit-key-mode (after use-fixed-font activate)
  (buffer-face-set 'default-fixed))

;; helm buffers are ???
(dolist (face (face-list))
  (when (string-prefix-p "helm-" (face-name face))
    (set-face-attribute face nil :family "Input Mono Narrow")))

(add-hook 'dired-mode-hook (lambda () (buffer-face-set 'default-fixed)))

(defun toggle-fixed-font ()
  (interactive)
  (buffer-face-toggle 'default-fixed))

(bind-key "C-, f" 'toggle-fixed-font)
