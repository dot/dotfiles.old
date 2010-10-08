;; diff mode
(autoload 'diff-mode "diff-mode" "Diff major mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))
(add-hook 'diff-mode-hook
          (lambda ()
            (set-face-foreground 'diff-file-header-face "light goldenrod")
            (set-face-foreground 'diff-index-face "thistle")
            (set-face-foreground 'diff-hunk-header-face "plum")
            (set-face-foreground 'diff-removed-face "pink")
            (set-face-background 'diff-removed-face "gray26")
            (set-face-foreground 'diff-added-face "light green")
            (set-face-background 'diff-added-face "gray26")
            (set-face-foreground 'diff-changed-face "DeepSkyBlue1")
            ))
;; moinmoin mode for trac-wiki
(require 'moinmoin-mode)

;; javascript mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-flag nil)
;(define-key js2-mode-map "\C-i" 'indent-and-back-to-indentation)


;; ruby mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(require 'ruby-electric)

;; 自動的に閉じないようにする
(defun ruby-electric-setup-keymap()
  (define-key ruby-mode-map " " 'ruby-electric-space)
;;;   (define-key ruby-mode-map "{" 'ruby-electric-curlies)
;;;   (define-key ruby-mode-map "(" 'ruby-electric-matching-char)
;;;   (define-key ruby-mode-map "[" 'ruby-electric-matching-char)
;;;   (define-key ruby-mode-map "\"" 'ruby-electric-matching-char)
;;;   (define-key ruby-mode-map "\'" 'ruby-electric-matching-char)
;;;   (define-key ruby-mode-map "|" 'ruby-electric-bar)
  )
;; インデントを深くしない
(setq ruby-deep-indent-paren-style nil)

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "CurrentRubyClass")
            (inf-ruby-keys)
;            (unset-key ruby-mode-map " ")
            (ruby-electric-mode t)
            (define-key ruby-mode-map "\C-m" 'ruby-reindent-then-newline-and-indent)
            (define-key ruby-mode-map "\C-j" 'newline)
            (setq ruby-indent-level 2)))
(setq interpreter-mode-alist
      (append '(("^#!.*ruby" . ruby-mode)) interpreter-mode-alist))
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)
                ("\\.rake$" . ruby-mode)
                ("Capfile" . ruby-mode)
                ("\\.ru$" . ruby-mode)
                ("Rakefile" . ruby-mode)) auto-mode-alist))
(modify-coding-system-alist 'file "\\.rb$" 'utf-8)

;;haml-mode
(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;;sass-mode
(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
