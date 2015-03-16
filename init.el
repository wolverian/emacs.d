(package-initialize)

(eval-when-compile
  (require 'use-package))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; visual settings

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message "iv")
(load-theme 'minimal-light t)
(setq-default indent-tabs-mode nil)
(defalias #'yes-or-no-p #'y-or-n-p)

;; editing settings

(desktop-save-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; packages

(use-package exec-path-from-shell
  :demand t
  :init (when (memq window-system '(mac ns))
	  (exec-path-from-shell-initialize)))

(use-package nyan-mode
  :config (progn
            (setq nyan-wavy-trail t)
            (nyan-mode)
            (nyan-start-animation)))

(use-package coffee-mode
  :config (setq coffee-tab-width 2))

(use-package whole-line-or-region
  :demand t
  :commands whole-line-or-region-mode
  :config (whole-line-or-region-mode 1))

;; (use-package aggressive-indent
;;   :init (global-aggressive-indent-mode))

(use-package eshell)

(use-package langtool
  :commands langtool-check
  :config (progn
            (setq langtool-language-tool-jar "/usr/local/Cellar/languagetool/2.8/libexec/languagetool.jar")))

(use-package helm-config
  :bind (("M-x" . helm-M-x)
         ("s-b" . helm-mini)
         ("s-i" . helm-imenu)
         ("s-j" . helm-projectile)
         ("s-e" . eshell)
         ("s-d" . helm-dash-at-point)
         ("C-x C-f" . helm-find-files)
         ("M-i" . helm-show-kill-ring)
         ("s-o" . helm-projectile-switch-project))
  :config (progn
            (use-package helm-eshell)
            (use-package helm-grep)
            (use-package helm-projectile)
            (use-package helm-dash)
            (bind-keys :map helm-map
                       ("<tab>" . helm-execute-persistent-action)
                       ("C-i" . helm-execute-persistent-action)
                       ("C-z" . helm-select-action))
            (setq helm-dash-common-docsets '("Lo-Dash"))
            (setq helm-dash-browser-func #'eww)))

(use-package smartparens
  :config (progn
            (use-package smartparens-config)
	    (smartparens-global-mode)
	    (show-smartparens-global-mode)
            (add-to-list 'sp-ignore-modes-list 'web-mode))
  :bind (("C-)" . sp-forward-slurp-sexp)))

(use-package magit
  :load-path "/Users/iv/.emacs.d/site-lisp/magit"
  :bind (("s-s" . magit-status))
  :config (progn
            (setq magit-expand-staged-on-commit t)))

(use-package less-css-mode
  :config (setq css-indent-offset 2))

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

;; (use-package factor-mode
;;   :config (progn
;; 	    (setq fuel-factor-root-dir "/Users/iv/Applications/factor")
;; 	    (add-to-list 'auto-mode-alist '("\\.factor-rc\\'" . factor-mode))))

;; (use-package oracle
;;   :commands go-oracle-mode
;;   :load-path "/Users/iv/src/go/src/golang.org/x/tools/cmd/oracle/")

;; (use-package go-mode
;;   :config (progn
;;             (setq gofmt-command "goimports")
;;             (add-hook 'go-mode-hook 'go-oracle-mode)))

(use-package paradox
  :bind (("s-p" . paradox-list-packages)))

(use-package popwin
  :config (progn
            (popwin-mode 1)
            (setq popwin:popup-window-position 'right)
            (setq popwin:popup-window-width 0.4)))

(use-package projectile
  :config (projectile-global-mode))

(use-package web-mode
  :mode "\\.jst\\'"
  :config (progn
            (setq web-mode-engines-alist '(("underscore" . "\\.jst\\'")))
            (add-hook 'web-mode-hook 'emmet-mode)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-extra-auto-pairs
                  '(("underscore" . (("<%=" . "%>")))))))

(use-package eldoc
  :init (eldoc-mode))

(use-package clojure-quick-repls
  :commands clojure-quick-repls-connect
  :config (setq cider-switch-to-repl-command #'clojure-quick-repls-switch-to-relevant-repl))

;; fonts

;; mono / variable width font
(set-face-font 'default "Input Mono Narrow-14")
(set-face-font 'variable-pitch "Input Sans Narrow-14")

;(add-hook 'tabulated-list-mode-hook (lambda () (buffer-face-set 'default-fixed)))

;; magit key mode doesn't have hooks
(defadvice magit-key-mode (after use-fixed-font activate)
  (buffer-face-set 'default-fixed))

;; helm buffers are ???
;; (dolist (face (face-list))
;;   (when (string-prefix-p "helm-" (face-name face))
;;     (set-face-attribute face nil :family "Input Mono Narrow")))

;(add-hook 'dired-mode-hook (lambda () (buffer-face-set 'default-fixed)))

(bind-key "C-, f" 'variable-pitch-mode)

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (langtool popwin bind-key clojure-mode dash nyan-mode emmet-mode pcmpl-homebrew helm-idris idris-mode helm-projectile clojure-quick-repls smartparens exec-path-from-shell cider coffee-mode haskell-mode helm helm-dash paradox projectile web-mode less-css-mode use-package minimal-theme whole-line-or-region)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
