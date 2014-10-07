(require 'package)

(setq unstable-package-list '( ;; evil-surround
                              company-go
                              dash-at-point
                              direx
                              go-mode
                              go-projectile
                              haskell-mode
                              minimal-theme
                              racket-mode
                              smart-tab
                              hungry-delete
                              smartparens
                              yaml-mode))

(setq stable-package-list '( ;; evil
                            ;; evil-leader
                            expand-region
                            multiple-cursors
                            clojure-mode
                            rainbow-delimiters
                            markdown-mode
                            coffee-mode
                            company
                            flycheck
                            solarized-theme
                            less-css-mode
                            magit
                            git-gutter
                            flycheck-haskell
                            helm
                            helm-projectile
                            projectile
                            ghc
                            exec-path-from-shell
                            go-eldoc
                            haskell-mode
                            web-mode
                            paradox
                            cider
                            popwin))

(add-to-list 'package-archives
       '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
       '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(dolist (package stable-package-list)
  (add-to-list 'package-pinned-packages `(,package . "melpa-stable") t))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package (append unstable-package-list stable-package-list))
  (unless (package-installed-p package)
    (package-install package)))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(load-library "iso-transl")

;; Write backup files to own directory
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

; General UI stuff
;; (global-linum-mode t)
(global-hungry-delete-mode)
;(global-hl-line-mode t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message "")
(setq x-underline-at-descent-line t)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq-default show-trailing-whitespace t)
(setq-default visible-bell 'top-bottom)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq column-number-mode t)

; Add characters considered part of word
(dolist (c (string-to-list ":_-?!#*"))
  (modify-syntax-entry c "w" emacs-lisp-mode-syntax-table))

; Always load externally changed files
(global-auto-revert-mode 1)

; Save modified buffers on focus lost
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

; Well, why not
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

(load-theme 'minimal-light t)
(set-default-font "Input Sans Narrow-13")

(setq paradox-github-token "508149e5159aa7efff0393a0b706c6e382c565b3")

; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'server)
(unless (server-running-p)
  (server-start))
;(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

;; some custom filetype mappings
(add-to-list 'auto-mode-alist '("\\.jst\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(setq js-indent-level 2)
(setq css-indent-offset 2)
;; (add-hook 'js-mode-hook 'js2-minor-mode)

(require 'web-mode)

(require 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(coffee-mode . "Web"))

(require 'smart-tab)
(global-smart-tab-mode 1)

; When executing shell commands from emacs, set PATH correctly.
(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments
  (delete "-i" exec-path-from-shell-arguments))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching 0)

(require 'recentf)
(recentf-mode 1)

(global-set-key (kbd "C-, p") 'paradox-list-packages)
(global-set-key (kbd "C-, e") 'eshell)
(global-set-key (kbd "C-, s") 'magit-status)
(global-set-key (kbd "C-, a") (lambda ()
                                (interactive)
                                (magit-stage-all)
                                (magit-commit)))

(global-set-key (kbd "M-9") 'backward-sexp)
(global-set-key (kbd "M-0") 'forward-sexp)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)

;; (require 'evil-leader)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader ",")
;; (evil-leader/set-key
;;   "." 'eval-buffer
;;   "d" 'dash-at-point
;;   "," 'projectile-find-file
;;   "c" 'comment-or-uncomment-region
;;   "g" 'magit-status
;;   "w" 'save-buffer
;;   "b" 'switch-to-buffer
;;   "k" 'kill-buffer
;;   "n" 'other-window
;;   "e" 'eshell
;;   "P" 'list-packages
;;   "p"  (lambda () (interactive) (other-window -1))
;;   )

;; (setq evil-want-C-u-scroll         t
;;       evil-want-C-w-in-emacs-state t)

;; (define-key evil-insert-state-map [left] 'undefined)
;; (define-key evil-insert-state-map [right] 'undefined)
;; (define-key evil-insert-state-map [up] 'undefined)
;; (define-key evil-insert-state-map [down] 'undefined)

;; (define-key evil-motion-state-map [left] 'undefined)
;; (define-key evil-motion-state-map [right] 'undefined)
;; (define-key evil-motion-state-map [up] 'undefined)
;; (define-key evil-motion-state-map [down] 'undefined)

;; (require 'evil)
;; (evil-mode t)
;; ;(setq-default evil-shift-width 2)

;; (require 'evil-surround)
;; (global-evil-surround-mode 1)

;(require 'rainbow-delimiters nil)
;(global-rainbow-delimiters-mode t)

(require 'icomplete)

(require 'smartparens-config)
(smartparens-global-mode t)
; https://github.com/Fuco1/smartparens/wiki/Example-configuration

(require 'git-gutter)
;; (git-gutter:linum-setup)
(global-git-gutter-mode t)
(add-to-list 'git-gutter:update-hooks 'after-save-hook)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(default-input-method "TeX")
 '(eshell-visual-commands
   (quote
    ("vi" "vim" "tmux" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm")))
 '(eshell-visual-subcommands (quote (("git" "show" "log" "diff"))))
 '(erc-prompt ">")
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)) t)
 '(markdown-command "pandoc -sS5")
 '(paradox-automatically-star t)
 '(racket-program "~/Applications/Racket v6.1/bin/racket")
 '(rcirc-server-alist
   (quote
    (("irc.freenode.net" :nick "iviv" :port 6697 :channels
      ("#racket")
      :encryption tls))))
 '(save-place t nil (saveplace))
 '(tool-bar-mode nil))

(setq-default mode-line-format nil)

; haskell
(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)
(setq haskell-font-lock-symbols nil)
(setq haskell-process-type 'cabal-repl)

(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-switch)

;;(add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;(add-hook 'haskell-mode-hook
;          (lambda ()
;            (define-key evil-normal-state-map (kbd "C-]") 'haskell-mode-jump-to-def)
;            (define-key evil-normal-state-map (kbd "C-t") 'pop-tag-mark)))

(require 'company)
(global-company-mode)

(add-hook 'go-mode (lambda ()
                     (setq-local evil-shift-width 8)))

;; helm

(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(global-set-key (kbd "C-c h x") 'helm-register)

(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)

(require 'helm-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; multiple cursors

(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; expand region

(require 'expand-region)

(global-set-key (kbd "C-=") 'er/expand-region)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((t (:foreground "gray60"))))
 '(erc-notice-face ((t (:foreground "SlateBlue"))))
 '(erc-timestamp-face ((t nil)))
 '(markdown-pre-face ((t (:inherit (font-lock-constant-face default-fixed) :weight normal))))
 '(racket-paren-face ((t (:foreground "gray60")))))

(global-eldoc-mode)

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

;; markdown

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
