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
(setq-default indent-tabs-mode nil)
(defalias #'yes-or-no-p #'y-or-n-p)

;; packages

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns))
	  (exec-path-from-shell-initialize)))

(use-package nyan-mode
  :init (progn
	  (nyan-mode)
	  (nyan-start-animation))
  :config (setq nyan-wavy-trail t))

(use-package coffee-mode
  :config (setq coffee-tab-width 2))

(use-package whole-line-or-region
  :commands whole-line-or-region-mode
  :init (whole-line-or-region-mode))

(use-package aggressive-indent
  :init (global-aggressive-indent-mode))

(use-package helm
  :init (progn
	  (use-package helm-config)
	  (use-package helm-eshell)
	  (use-package helm-grep)
	  (use-package helm-projectile))
  :bind (("M-x" . helm-M-x)
	 ("C-x b" . helm-mini)
	 ("C-x C-f" . helm-find-files)
	 ("C-, l" . helm-projectile)
	 ("M-i" . helm-show-kill-ring))
  :config (bind-keys :map helm-map
		     ("<tab>" . helm-execute-persistent-action)
		     ("C-i" . helm-execute-persistent-action)
		     ("C-z" . helm-select-action)))

(use-package smartparens
  :init (use-package smartparens-config)
  :config (progn
	    (smartparens-global-mode)
	    (show-smartparens-global-mode)
            (add-to-list 'sp-ignore-modes-list 'web-mode))
  :bind (("C-)" . sp-forward-slurp-sexp)))

(use-package magit
  :bind (("C-, s" . magit-status)))

(use-package less-css-mode
  :config (setq css-indent-offset 2))

(use-package vc)

(use-package haskell-mode
  :config (progn
	    (setq haskell-process-path-cabal "/usr/local/bin/cabal")
	    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
	    (bind-keys :map haskell-mode-map
		       ("C-c C-l" . haskell-process-load-file)
		       ("C-c C-z" . haskell-interactive-switch)
		       ("C-c C-t" . haskell-interactive-do-type)
		       ("C-c C-i" . haskell-interactive-do-info)
                       ("M-." . haskell-mode-jump-to-def-or-tag)
                       ("M-," . pop-tag-mark))))

(use-package factor-mode
  :config (progn
	    (setq fuel-factor-root-dir "/Users/iv/Applications/factor")
	    (add-to-list 'auto-mode-alist '("\\.factor-rc\\'" . factor-mode))))

(use-package oracle
  :commands go-oracle-mode
  :load-path "/Users/iv/src/go/src/golang.org/x/tools/cmd/oracle/")

(use-package go-mode
  :config (progn
            (setq gofmt-command "goimports")
            (add-hook 'go-mode-hook 'go-oracle-mode)))

(use-package paradox
  :bind (("C-, p" . paradox-list-packages)))

(use-package popwin
  :config (popwin-mode 1))

(use-package projectile
  :config (projectile-global-mode))

(use-package web-mode
  :mode "\\.jst\\'"
  :init (setq web-mode-engines-alist '(("underscore" . "\\.jst\\'")))
  :config (progn
            (add-hook 'web-mode-hook 'emmet-mode)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-extra-auto-pairs
                  '(("underscore" . (("<%=" . "%>")))))))

(use-package eldoc
  :init (eldoc-mode))

;; non-mode specific keys

(bind-keys
 ("s-e" . eshell)
 ("s-i" . helm-imenu)
 ("s-j" . helm-projectile)
 ("s-b" . helm-mini))

;; fonts

;; mono / variable width font
(set-face-font 'default "Input Mono Narrow-14")
(set-face-font 'variable-pitch "Input Sans Narrow-14")

(add-hook 'tabulated-list-mode-hook (lambda () (buffer-face-set 'default-fixed)))

;; magit key mode doesn't have hooks
(defadvice magit-key-mode (after use-fixed-font activate)
  (buffer-face-set 'default-fixed))

;; helm buffers are ???
(dolist (face (face-list))
  (when (string-prefix-p "helm-" (face-name face))
    (set-face-attribute face nil :family "Input Mono Narrow")))

(add-hook 'dired-mode-hook (lambda () (buffer-face-set 'default-fixed)))

(bind-key "C-, f" 'variable-pitch-mode)

(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (haskell-mode helm pallet paradox projectile web-mode less-css-mode package-build shut-up epl git commander f dash s)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
