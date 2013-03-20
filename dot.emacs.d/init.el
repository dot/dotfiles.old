;; package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; lang / encoding
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

;; load paths
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/howm")

;; keys
; swap cmd <-> opt(meta)
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))
; input mode
(global-set-key "\C-o" 'toggle-input-method)
; to swap C-h and Backspace
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
; tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
; ignore warnings
(setq next-line-add-newlines nil)

;; window settings
; base
(if (boundp 'window-system)
    (setq initial-frame-alist
          (append (list
                   '(foreground-color . "azure3")
                   '(background-color . "black")
                   '(border-color     . "black")
                   '(mouse-color      . "white")
                   '(cursor-color     . "white")
                   '(cursor-type      . box)
                   '(menu-bar-lines . 1)
                   '(width . 120)
                   '(height . 50)
                   )
                  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

; modeline
(line-number-mode 1)
(column-number-mode 1)
(tool-bar-mode 0)
(setq inhibit-startup-message t)
; ignore scrollbar
(set-scroll-bar-mode nil)
; hilight match paren
(setq show-paren-delay 0)
(setq show-paren-style 'single)
(show-paren-mode t)

; color region
(setq transient-mark-mode t)
(setq highlight-nonselected-windows t)
; scroll size
(setq next-screen-context-lines 10)
; alpha background
(set-frame-parameter (selected-frame) 'alpha '(85 85))

; fonts
(create-fontset-from-ascii-font "Ricty-13:weight=normal:slant=normal" nil "ricty")
(set-fontset-font "fontset-ricty"
                  'unicode
                  (font-spec :family "Ricty" :size 13)
                  nil
                  'append)
(add-to-list 'default-frame-alist '(font . "fontset-ricty"))

; kill ring
(setq kill-ring-max 1000)

; beep
(setq visible-bell t)


;;; ========== packages ==========

;; whitespace
;; @see http://www.emacswiki.org/emacs/WhiteSpace
;; @see http://xahlee.org/emacs/whitespace-mode.html
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-newline "gray5")
(set-face-attribute 'whitespace-newline nil
                    :family "Ricty"
                    :height 50)
(set-face-foreground 'whitespace-tab "gray25")
(set-face-background 'whitespace-tab 'nil)
;(set-face-underline  'whitespace-tab "SteelBlue")
(set-face-foreground 'whitespace-trailing "gray25")
(set-face-background 'whitespace-trailing "gray12")
(set-face-bold-p 'whitespace-trailing 'nil)

(setq whitespace-style '(face tabs space-mark tab-mark newline-mark trailing))
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□]) ;; 2byte space
        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])
        ))

;; helm
(require 'helm-config)
(require 'helm-ls-git)
(helm-mode 1)

(defun my-helm ()
  (interactive)
  (helm :sources '(
                   helm-c-source-buffers-list
                   helm-c-source-recentf
                   helm-c-source-ls-git
                   helm-c-source-files-in-current-dir
                   helm-c-source-mac-spotlight
                   helm-c-source-buffer-not-found)
        :buffer "*helm default*"))

(global-set-key (kbd "C-;") 'my-helm)
(global-set-key (kbd "C-M-;") 'helm-resume)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

; diable auto complete
(custom-set-variables '(helm-ff-auto-update-initial-value nil))
; C-h in backspace
(define-key helm-c-read-file-map (kbd "C-h") 'delete-backward-char)
; tab is completion
(define-key helm-c-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-c-read-file-map (kbd "C-w") 'helm-find-files-down-one-level)


;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(push '("^\*helm.+$" :regexp t :height 30) popwin:special-display-config)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; bm
(require 'bm)
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key "\M-2" 'bm-toggle)
(global-set-key [f2]   'bm-next)
(global-set-key [S-f2] 'bm-previous)

;; session
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; howm
; with org-mode
(require 'org)
(add-hook 'org-mode-hook 'howm-mode)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(setq howm-view-title-header "*")

(require 'howm)
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)
