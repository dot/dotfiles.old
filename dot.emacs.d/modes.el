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

;; javascript mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;;(setq js2-bounce-indent-flag nil)
;(define-key js2-mode-map "\C-i" 'indent-and-back-to-indentation)

;; fixing indentation
;; cf. http://d.hatena.ne.jp/speg03/20091011/1255244329
; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 2
        indent-tabs-mode nil
        espresso-expr-indent-offset 2
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  ; (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map "\C-\M-\\"
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
  ; (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  ; (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;; ;; esspresso mode for javascript
;; (autoload #'espresso-mode "espresso" "Start espresso-mode" t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

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
                ("Gemfile" . ruby-mode)
                ("\\.ru$" . ruby-mode)
                ("Rakefile" . ruby-mode)) auto-mode-alist))
(modify-coding-system-alist 'file "\\.rb$" 'utf-8)

;;haml-mode
(require 'haml-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
;;sass-mode
(require 'sass-mode nil 't)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; ReView mode
(require 'review-mode)
(add-to-list 'auto-mode-alist '("\\.re$" . review-mode))

;; Objective-C mode
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
;(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;; c/cpp <=> header
(add-hook 'c-mode-common-hook
	  '(lambda ()
	    (define-key c-mode-map "\C-xt" 'ff-find-other-file)
	    (define-key objc-mode-map "\C-xt" 'ff-find-other-file)
	    (define-key c++-mode-map "\C-xt" 'ff-find-other-file)
            (gtags-mode 1)
;;            (gtags-make-complete-list)
))

;; c++ mode
;; BackSpace キーを「賢く」し，インデント幅は4桁，タブはスペースに展開
(add-hook 'c-mode-common-hook
          '(lambda ()
             (progn
               (c-toggle-hungry-state 1)
               (setq c-basic-offset 4 indent-tabs-mode nil))))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;; cuda mode
(require 'cuda-mode)

;; coffee mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom ()
 "coffee-mode-hook"
(set (make-local-variable 'tab-width) 2)
(set (make-local-variable 'coffee-cleanup-whitespace) nil))
(add-hook 'coffee-mode-hook
 '(lambda() (coffee-custom)))

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))