;; package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

;; lang / encoding
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(setq default-coding-systems 'utf-8)
(setq file-name-coding-system 'utf-8)

;; load paths
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/howm")

(setq max-specpdl-size 6000)

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

; no more new window
(setq ns-pop-up-frames nil)
; drag and drop
(define-key global-map [ns-drag-file] 'ns-find-file)
; trash deleted file
(setq delete-by-moving-to-trash t)

; no more backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

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
                   )
                  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

; fonts
(create-fontset-from-ascii-font "Ricty-16:weight=normal:slant=normal" nil "rictyl")
(create-fontset-from-ascii-font "Ricty-13:weight=normal:slant=normal" nil "rictym")
(create-fontset-from-ascii-font "Source Code Pro-14:weight=normal:slant=normal" nil "scpl")
(create-fontset-from-ascii-font "Source Code Pro-13:weight=normal:slant=normal" nil "scpm")
(set-fontset-font "fontset-rictyl"
                  'unicode
                  (font-spec :family "Ricty")
                  nil
                  'append)
(set-fontset-font "fontset-rictym"
                  'unicode
                  (font-spec :family "Ricty")
                  nil
                  'append)
(set-fontset-font "fontset-scpl"
                  'unicode
                  (font-spec :family "Source Code Pro")
                  nil
                  'append)
(set-fontset-font "fontset-scpm"
                  'unicode
                  (font-spec :family "Source Code Pro")
                  nil
                  'append)


;; 画面の解像度によりフレームサイズを変化させる
(when window-system
  (if (>= (x-display-pixel-width) 2000)
      (progn
        (set-frame-width (next-frame) 150)
        (set-frame-height (next-frame) 80)
        (add-to-list 'default-frame-alist '(font . "fontset-rictyl"))
;        (add-to-list 'default-frame-alist '(font . "fontset-scpl"))
      )
      (progn
        (set-frame-width (next-frame) 120)
        (set-frame-height (next-frame) 60)
        (add-to-list 'default-frame-alist '(font . "fontset-rictym"))
;        (add-to-list 'default-frame-alist '(font . "fontset-scpm"))
      )
  )
)
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
(require 'helm-c-moccur)
(helm-mode 1)
(helm-descbinds-mode)
(setq helm-idle-delay 0.01)
(setq helm-input-idle-delay 0.01)
(setq helm-candidate-number-limit 100)

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
(global-set-key (kbd "M-o") 'helm-c-moccur-occur-by-moccur)
(setq helm-c-moccur-higligt-info-line-flag t
      helm-c-moccur-enable-auto-look-flag t
      helm-c-moccur-enable-initial-pattern t)


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
(setq max-lisp-eval-depth 10000)

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
; c.f. https://github.com/emacs-helm/helm/issues/94
(setq session-save-print-spec '(t nil 40000))

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

;; recentf
(require 'recentf)
(setq recentf-max-saved-items 1000)
(setq recentf-exclude '("^/[^/:]+:"))
(setq recentf-auto-cleanup 'never)
(setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
(recentf-mode 1)

;; yasnippet
(require 'yasnippet-bundle)

;; helm interface
(eval-after-load "helm-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (helm-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*helm yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
     (define-key helm-command-map (kbd "y") 'yas/insert-snippet)))

;; snippet-mode for *.yasnippet files
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


;; ruby-end
(require 'ruby-end)

;; dash-at-point
(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-cd" 'dash-at-point)

;; git-gutter
(global-git-gutter-mode t)

;;; ========== modes ==========

;; ruby
(add-hook 'ruby-mode-hook
 '(lambda ()
   (abbrev-mode 1)
   (electric-pair-mode t)
   (electric-indent-mode t)
   (electric-layout-mode t)))
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)
                ("\\.rake$" . ruby-mode)
                ("Capfile" . ruby-mode)
                ("Gemfile" . ruby-mode)
                ("Guardfile" . ruby-mode)
                ("\\.ru$" . ruby-mode)
                ("\\.thor$" . ruby-mode)
                ("Rakefile" . ruby-mode)) auto-mode-alist))

(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; (custom-set-variables
;;  '(ruby-insert-encoding-magic-comment nil))

;; feature-mode
(setq feature-default-language "ja")
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; slim-mode
(require 'slim-mode)
