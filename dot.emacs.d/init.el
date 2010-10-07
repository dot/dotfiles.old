;;; ========== Emacsの基本設定 ==========
; 言語を日本語にする
(set-language-environment 'Japanese)
; 極力UTF-8とする
 (prefer-coding-system 'utf-8)

;; プライベートなelispへのパス
(setq load-path
      (cons
       (expand-file-name
        "~/.emacs.d/site-lisp" ) load-path))
;(add-to-list 'load-path "/Users/shuhei/.emacs.d/site-lisp/icicles")

;; モードラインの変更
(display-time)
(line-number-mode 1)
(column-number-mode 1)

(tool-bar-mode 0)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq next-line-add-newlines nil)
;; タブ
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; 行間の設定
;(setq line-spacing 3)  ;; 3pxの行間
(setq line-spacing 0.1) ;; 行の高さの0.2倍

;; バックアップファイルを作らない
(setq make-backup-files nil)

;; readonyでkillしても怒らない
(setq kill-read-only-ok t)


;;; ======= 操作関係 =======

;; CarbonEmacs用の特殊設定
(when (featurep 'carbon-emacs-package)
  (mac-input-method-mode 1))

;;入力モードの変更
(global-set-key "\C-o" 'toggle-input-method)

;; to swap C-h and Backspace
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
;;avoid hiding with M-h
(setq mac-command-key-is-meta t)
(setq mac-pass-command-to-system nil)

;; 対応する括弧に Jump
(global-set-key "!" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert !."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; minibufferでC-wを使えるようにする
;;(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
;; (define-key minibuffer-local-completion-map "\C-w"
;;   #'(lambda (arg)
;;       (interactive "p")
;;       (if (eolp)
;;           (backward-kill-word arg)
;;         (kill-line arg))))
(defun ys:minibuffer-delete-file-name (&optional arg)
  (interactive "p")
  (if (eolp)
      (save-excursion
        (while (and (< 0 arg)
                    (re-search-backward "[/\\][^/\\]+[/\\]?$" nil t))
          (replace-match "/")
          (setq arg (1- arg))))
    (kill-line arg))
)
(define-key minibuffer-local-completion-map "\C-w" 'ys:minibuffer-delete-file-name)

;; M-x の補完候補をミニバッファに表示
(icomplete-mode 1)

;;; ======= 見映え関係 =======
(if (boundp 'window-system)
    (setq initial-frame-alist
          (append (list
                   '(foreground-color . "azure3") ;; 文字が白
                   '(background-color . "black") ;; 背景は黒
                   '(border-color     . "black")
                   '(mouse-color      . "white")
                   '(cursor-color     . "white")
                   '(cursor-type      . box)
                   '(menu-bar-lines . 1)
                   '(width . 120) ;; ウィンドウ幅
                   '(height . 50) ;; ウィンドウの高さ
                   )
                  initial-frame-alist)))
(setq default-frame-alist initial-frame-alist)

;; Quartz 2Dのアンチエイリアス
(setq mac-allow-anti-aliasing t)

;; 選択範囲に色をつける
(setq transient-mark-mode t)
(setq highlight-nonselected-windows t)

;; 対応する括弧をハイライト表示させる
;(show-paren-mode t)
;(setq show-paren-style 'mixed)
;(set-face-background 'show-paren-match-face "gray10")
;(set-face-foreground 'show-paren-match-face "SkyBlue")
;(setq show-paren-style 'expression)

(global-font-lock-mode 1)
;; スクロールする際に重なって表示するサイズ
(setq next-screen-context-lines 10)

;; 透過処理
(set-frame-parameter (selected-frame)  'alpha  '(90 90))
;(set-alpha 'alpha  '(<active> [<inactive>]))
;(set-alpha 90)

;; エスケープシーケンスを処理する
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; フォント
;; fontset-consolas
(create-fontset-from-mac-roman-font
 "-apple-consolas-medium-r-normal--14-*-*-*-m-0-mac-roman" nil "consolas")
(set-fontset-font "fontset-consolas" 'japanese-jisx0208
                  '("ヒラギノ角ゴ pro w3" . "jisx0208.1983"))
(set-fontset-font "fontset-consolas" 'katakana-jisx0201
                  '("ヒラギノ角ゴ pro w3" . "jisx0201.1976"))
;; fontset-bitstream
(create-fontset-from-mac-roman-font
 "-apple-bitstream vera sans mono-medium-r-normal--13-*-*-*-*-*-*-*" nil "bitstream")
(set-fontset-font "fontset-bitstream" 'japanese-jisx0208
                  '("ヒラギノ角ゴ pro w3" . "jisx0208.1983"))
(set-fontset-font "fontset-bitstream" 'katakana-jisx0201
                  '("ヒラギノ角ゴ pro w3" . "jisx0201.1976"))
(set-fontset-font "fontset-bitstream" 'japanese-jisx0208
                  '("IPAゴシック" . "iso10646"))
(set-fontset-font "fontset-bitstream" 'katakana-jisx0201
                  '("IPAゴシック" . "iso10646"))

;; fontset-monaco
;; (create-fontset-from-mac-roman-font
;;  "-apple-monaco-medium-r-normal--12-*-*-*-*-*-iso10646-1" nil "monaco")
;; (set-fontset-font "fontset-monaco" 'japanese-jisx0208
;;                   '("ヒラギノ角ゴ pro w3" . "jisx0208.1983"))
;; (set-fontset-font "fontset-monaco" 'katakana-jisx0201
;;                   '("ヒラギノ角ゴ pro w3" . "jisx0201.1976"))
;; fontset-anonymous
;; (create-fontset-from-mac-roman-font
;;  "-apple-anonymous-medium-r-normal--12-*-*-*-*-*-iso10646-1" nil "anonymous")
;; (set-fontset-font "fontset-anonymous" 'japanese-jisx0208
;;                   '("ヒラギノ角ゴ pro w3" . "jisx0208.1983"))
;; (set-fontset-font "fontset-anonymous" 'katakana-jisx0201
;;                   '("ヒラギノ角ゴ pro w3" . "jisx0201.1976"))
;; fontset-mplus
;; (create-fontset-from-mac-roman-font
;;  "-apple-M+ 1m-medium-r-normal--14-*-*-*-*-*-iso10646-1" nil "mplus")
;; (set-fontset-font "fontset-mplus" 'japanese-jisx0208
;;                   '("M+ 1m medium" . "iso10646"))
;; (set-fontset-font "fontset-mplus" (cons (make-char 'japanese-jisx0208 #x30 #x20)
;;                                         (make-char 'japanese-jisx0208 #x74 #x7f))
;;                   '("ヒラギノ角ゴ pro w3" . "jisx0208.1983"))
;; (set-fontset-font "fontset-mplus" 'katakana-jisx0201
;;                   '("M+ 1m medium" . "iso10646"))
;; fontset-mplusIPA
;; (create-fontset-from-mac-roman-font
;;  "-apple-M+2VM+IPAG circle-medium-r-normal--14-*-*-*-*-*-iso10646-1" nil "mplusIPA")
;; (set-fontset-font "fontset-mplusIPA" 'japanese-jisx0208
;;                   '("M+2VM+IPAG circle" . "iso10646"))
;; (set-fontset-font "fontset-mplus" 'katakana-jisx0201
;;                   '("M+2VM+IPAG circle" . "iso10646"))
;;
;; fontset-meiryo
;; (create-fontset-from-mac-roman-font
;;  "-apple-MeiryoKe_Console-medium-r-normal--16-*-*-*-*-*-iso10646-1" nil "meiryo")
;; (set-fontset-font "fontset-meiryo" 'japanese-jisx0208
;;                   '("MeiryoKe_Console" . "jisx0208.1983"))
;; (set-fontset-font "fontset-meiryo" 'katakana-jisx0201
;;                   '("MeiryoKe_Console" . "jisx0208.1983"))

(setq my-font "-*-*-medium-r-normal--14-*-*-*-*-*-fontset-hiramaru")
(setq fixed-width-use-QuickDraw-for-ascii t)
(setq mac-allow-anti-aliasing t)
(if (= emacs-major-version 22)
    (require 'carbon-font))
(set-default-font my-font)
(add-to-list 'default-frame-alist `(font . ,my-font))
(when (= emacs-major-version 23)
  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
  (setq face-font-rescale-alist
	'(("^-apple-hiragino.*" . 1.2)
	  (".*osaka-bold.*" . 1.2)
	  (".*osaka-medium.*" . 1.2)
	  (".*courier-bold-.*-mac-roman" . 1.0)
	  (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
	  (".*monaco-bold-.*-mac-roman" . 0.9)
	  ("-cdac$" . 1.3))))

(defun my-set-default-fontset (fontset)
  (interactive                   ; borrowed from describe-fontset code
   (if (not (and window-system (fboundp 'fontset-list)))
       (error "No fontsets being used")
     (let ((fontset-list (nconc
                          (fontset-list)
                          (mapcar 'cdr fontset-alias-alist)))
           (completion-ignore-case t))
       (list (completing-read
              "Fontset: "
              fontset-list nil t "fontset-")))))
  (let ((old (assoc 'font default-frame-alist)))
    (if old (setcdr old fontset)
      (add-to-list 'default-frame-alist (cons 'font fontset))))
  (set-frame-font fontset))

;; select font-set
(my-set-default-fontset "fontset-bitstream")

;; 全角空白、タブ、行末の空白などを表示
;;(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
;;(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-b-2 append)
     ("　" 0 my-face-b-1 append)
     ("[ \t]+$" 0 my-face-u-1 append)
;     ("[\r]*\n" 0 my-face-r-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;;;;; ========== 拡張関連 ==========

;;emacs-w3m
;; (require 'w3m-load)
;; (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)

;; (autoload 'w3m-find-file "w3m" "w3m interface function for local file." t)
;; (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
;; (autoload 'w3m-weather "w3m-weather" "Display weather report." t)
;; (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)
;; (autoload 'w3m-namazu "w3m-namazu" "Search files with Namazu." t)

;; (setq w3m-icon-directory "/Applications/Emacs.app/Contents/Resources/etc/w3m")
;; (setq w3m-namazu-tmp-file-name "~/.nmz.html")
;; (setq w3m-namazu-index-file "~/.w3m-namazu.index")
;; (setq w3m-bookmark-file "~/.w3m/bookmark.html")

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; hightlight-completion
;; mini-bufferの補完
(setq hc-ctrl-x-c-is-completion t)
;;(require 'highlight-completion)
;;(highlight-completion-mode 1)
(global-set-key "\C-\\" 'toggle-input-method)

;; bm
(require 'bm)
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key "\M-2" 'bm-toggle)
(global-set-key [f2]   'bm-next)
(global-set-key [S-f2] 'bm-previous)

;; インテリジェント補完コマンド ac-mode
; M-x ac-mode
(autoload 'ac-mode "ac-mode" "Minor mode for advanced completion." t nil)

;; dynamic macro
;(defconst *dmacro-key* "\C-t" "繰返し指定キー")
;(global-set-key *dmacro-key* 'dmacro-exec)
;(autoload 'dmacro-exec "dmacro" nil t)

;; mic-paren
(if window-system
    (progn
      (require 'mic-paren)
      (paren-activate)     ; activating
      (setq paren-sexp-mode t)
;      (setq paren-match-face 'bold)
      (set-face-background 'paren-face-match "gray10")
      (set-face-foreground 'paren-face-match "SkyBlue")
      ))

;; kill-summery
(autoload 'kill-summary "kill-summary" nil t)
(global-set-key "\M-y" 'kill-summary)

;; minibuffer isearch
(require 'minibuf-isearch)

;; iswitchb-mode
(require 'iswitchb)
(iswitchb-default-keybindings)
(iswitchb-mode t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(add-hook 'iswitchb-define-mode-map-hook
          (lambda ()
            (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
            (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
            (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
            (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "選択している buffer を window に表示してみる。"
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))

;; find-recursive
(require 'find-recursive)

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; dired sorter
(load "sorter" nil t)
(setq insert-directory-program "/bin/ls")

;; session
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (setq session-globals-max-string 100000000)
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize))

;; el-screen
(setq elscreen-prefix-key "\C-t")
(load "elscreen" "ElScreen" t)
(setq elscreen-display-tab t)
;(require 'elscreen-howm)
(require 'elscreen-dired)

;; psvn
(require 'psvn)
;; (autoload 'svn-status "dsvn" "Run `svn status'." t)
;; (autoload 'svn-update "dsvn" "Run `svn update'." t)
(setq process-coding-system-alist
      (cons '("svn" . utf-8) process-coding-system-alist))

;; howm
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)

;; 入力回数に応じて補完する
(require 'pabbrev)

;; 2ch
;;(require 'navi2ch)

;; recentf
(require 'recentf)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("^/[^/:]+:"))
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

;; anything
(require 'anything-config)
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-bookmarks
                             anything-c-source-recentf
                             anything-c-source-file-name-history
                             anything-c-source-locate))
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
(global-set-key (kbd "C-;") 'anything)
;; anything-grep
(require 'anything-grep)
;;; マッチした行に色を付ける
;; (defun anything-persistent-highlight-point (start &optional end buf face rec)
;;   (goto-char start)
;;   (when (overlayp anything-c-persistent-highlight-overlay)
;;     (move-overlay anything-c-persistent-highlight-overlay
;;                   start
;;                   (or end (line-end-position))
;;                   buf))
;;   (overlay-put anything-c-persistent-highlight-overlay 'face (or face 'highlight))
;;   (when rec
;;     (recenter)))
;; (add-hook 'anything-cleanup-hook
;;           (lambda ()
;;             (when (overlayp anything-c-persistent-highlight-overlay)
;;               (delete-overlay anything-c-persistent-highlight-overlay))))
;; (setq anything-grep-goto-hook
;;       (lambda ()
;;         (when anything-in-persistent-action
;;           (anything-persistent-highlight-point (point-at-bol) (point-at-eol)))))

;; anything-grep-by-name
(setq anything-grep-alist
    ;; 全バッファのファイル名においてegrepをかける。moccurの代わり。
  '(("buffers" ("egrep -Hin %s $buffers" "/"))
    ;; ~/memo 以下から再帰的にegrepをかける。不要なファイルは除かれる。
    ("howm" ("ack -af | xargs egrep -Hin '%s'" "~/howm"))
    ;; ruby
    ("ruby" ("ack -afG 'rb$' | xargs egrep -Hin %s" "/usr/local/lib/ruby/1.8"))
    ;; gems
    ("gems" ("ack -afG 'rb$' | xargs egrep -Hin %s" "/Users/kondo/.gem/ruby/1.8/gems"))
    )
)

;; ysnippets
(require 'yasnippet)
(setq yas/trigger-key (kbd "TAB"))
;(setq yas/next-field-key (kbd "TAB"))

;;; anything連携
(require 'anything-c-yasnippet)
;;;スペース区切りで絞り込めるようにする デフォルトは nil
(setq anything-c-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'anything-c-yas-complete)

;; yasnippetのsnippetを置いてあるディレクトリ
(setq yas/root-directory 
      (list 
       (expand-file-name "~/.emacs.d/site-lisp/snippets")))
(yas/load-directory (expand-file-name "~/.emacs.d/snippets")) ; default
;(yas/load-directory (expand-file-name "~/git/yasnippets-rails/rails-snippets")) ; rails

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand  hippie-expand-try-functions-list))
;; コメントやリテラルではスニペットを展開しない
(setq yas/buffer-local-condition
      '(or (not (or (string= "font-lock-comment-face"
                             (get-char-property (point) 'face))
                    (string= "font-lock-string-face"
                             (get-char-property (point) 'face))))
           '(require-snippet-condition . force-in-comment)))

(yas/initialize)
;(yas/load-all-directories)

;;; yasnippet展開中はflymakeを無効にする
(defvar flymake-is-active-flag nil)
(defadvice yas/expand-snippet
  (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
  (setq flymake-is-active-flag
        (or flymake-is-active-flag
            (assoc-default 'flymake-mode (buffer-local-variables))))
  (when flymake-is-active-flag
    (flymake-mode-off)))
(add-hook 'yas/after-exit-snippet-hook
          '(lambda ()
             (when flymake-is-active-flag
               (flymake-mode-on)
               (setq flymake-is-active-flag nil))))

;; anything-c-yasnippet を使うモードの登録
;(add-to-list 'yas/extra-mode-hooks 'ruby-mode-hook)

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-." 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\M-t" 'gtags-pop-stack)
         ))
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (gtags-mode 1)
;;             (setq gtags-libpath `((,(expand-file-name "~/.tags/c") . "/usr/include")))))
;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             (gtags-mode 1)
;;             (setq gtags-libpath `((,(expand-file-name "~/.tags/haxe") . "/usr/local/haxe")))))
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (gtags-mode 1)))
;;             (setq gtags-libpath `((,(expand-file-name "~/.tags/ruby") . "/opt/local/lib/ruby/1.8")
;;                                   (,(expand-file-name "~/.tags/rubygems") . "/opt/local/lib/ruby/gems/1.8/gems")))))

(require 'gist)

(require 'go-mode-load)

;;; =========== モード関連 ==========

;; ReView mode
(require 'review-mode)
(setq auto-mode-alist
      (append '(("\\.re$" . review-mode)
)))

;; (setq load-path (cons "/usr/local/otp/lib/tools-<ToolsVer>/emacs"
;;                       load-path))
(setq erlang-root-dir "/opt/local/lib/erlang")
(setq exec-path (cons "t/local/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; Objective-C and Objective-C++
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
(defun objc-header-file-p ()
  (save-excursion
    (search-forward "@end" nil t)))
(add-to-list 'magic-mode-alist
             '(objc-header-file-p . objc-mode))
(when (require 'objc-c-mode nil t)
  (add-to-list 'c-default-style '(objc-mode . "objc")))

;; Easy-to-switch header and impl.
(define-key c-mode-base-map "\C-c\C-n" 'ff-find-other-file)
(setq cc-other-file-alist
      '(("\\.mm?$" (".h"))
        ("\\.h$" (".c" ".cpp" ".m" ".mm"))))

(when (require 'haskell-mode nil t)
  (setq auto-mode-alist
        (cons '("\\.hs$" . haskell-mode) auto-mode-alist))) 

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


(require 'rcodetools)
;(require 'icicles-rcodetools)
(require 'anything-rcodetools)
;; Command to get all RI entries.
(setq rct-get-all-methods-command "PAGER=cat fri -l")
;; See docs
(define-key anything-map "\C-z" 'anything-execute-persistent-action)

;;use align
;; (add-to-list 'align-rules-list
;;              '(ruby-comma-delimiter
;;                (regexp . ",\\(\\s-*\\)[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))
;; (add-to-list 'align-rules-list
;;              '(ruby-hash-literal
;;                (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))
;; (add-to-list 'align-rules-list
;;              '(ruby-assignment-literal
;;                (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))
;; (add-to-list 'align-rules-list          ;TODO add to rcodetools.el
;;              '(ruby-xmpfilter-mark
;;                (regexp . "\\(\\s-*\\)# => [^#\t\n]")
;;                (repeat . nil)
;;                (modes  . '(ruby-mode))))

;; icicles
;(load "~/Library/site-lisp/icicles/icicles-install")

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

;;  flymake for ruby
(require 'flymake)
;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
;(ido-mode t)
;; Rinari
;(add-to-list 'load-path "/Users/shuhei/git/rinari")
;(require 'rinari)
;; rhtml mode
;(add-to-list 'load-path "/Users/shuhei/git/rhtml")
;(require 'rhtml-mode)
;(add-hook 'rhtml-mode-hook
;          (lambda () (rinari-launch)))


;; for rspec mode
;(require 'rspec-mode)

;; css mode
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css$'" . css-mode) auto-mode-alist))
;;タブ幅を4に
(setq cssm-indent-level 2)
;;インデントをc-styleにする
(setq cssm-indent-function #'cssm-c-style-indenter)
;; (setq cssm-indent-function 'cssm-c-style-indenter)
;; (add-hook 'css-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'indent-tabs-mode) t)))
;; (setq auto-mode-alist
;;       (cons '("\\.css$" . css-mode) auto-mode-alist))

;; scheme mode for gauche
(setq scheme-program-name "gosh")

(defun scheme-other-frame ()
  "Run scheme on other frame"
  (interactive)
  (switch-to-buffer-other-frame
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(defun gauche-info ()
  (interactive)
  (switch-to-buffer-other-frame
   (get-buffer-create "*info*"))
  (info
   "/opt/local/share/info/gauche-refj.info.gz"))

(define-key global-map
  "\C-xS" 'scheme-other-frame)

(define-key global-map
  "\C-cS" 'scheme-other-window)

(define-key global-map
  "\C-cI" 'gauche-info)

(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'match 'scheme-indent-function 1)
(put 'match-lambda 'scheme-indent-function 1)
(put 'match-let 'scheme-indent-fucntion 1)
(put 'match-let* 'scheme-indent-fucntion 1)
(put 'match-letrec 'scheme-indent-fucntion 1)
(put 'match-let1 'scheme-indent-function 2)
(put 'match-define 'scheme-indent-fucntion 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)


;; Reference
;; mgrep
;; http://www.bookshelf.jp/soft/meadow_51.html#SEC753
(require 'mgrep)
(setq mgrep-list
      '(
        ; list
;        ("project(rb)" "/Users/kondo/projects" "*.rb" subfind)
        ("ruby" "/usr/local/lib/ruby/1.8" "*.rb" t)
        ("gem" "/Users/kondo/.gem/ruby/1.8/gems" "*.rb" subfind)
        ("allgems" "/Users/kondo/.gem/ruby/1.8/gems" "*.rb" t)
        ))

;; color-grep
(require 'color-grep)
;; grep バッファを kill 時に，開いたバッファを消す
(setq color-grep-sync-kill-buffer t)

;; use ack as grep
(defun ack ()
  (interactive)
  (let ((grep-find-command "ack --nocolor --nogroup "))
    (call-interactively 'grep-find)))

;;;
;;; trac-wiki mode
;;;
;; (require 'trac-wiki)
;; (trac-wiki-define-project "jalert"
;;                           "https://pichu.netlab.jp/trac/" t)
;; (autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)

;; grep-edit
(require 'grep-edit)

;;;;; ========== Mew    ==========
;; (autoload 'mew "mew" nil t)
;; (autoload 'mew-send "mew" nil t)

;; ; Optional setup (Read Mail menu for Emacs 21):
;; (if (boundp 'read-mail-command)
;;     (setq read-mail-command 'mew))

;; ;; Optional setup (e.g. C-xm for sending a message):
;; (autoload 'mew-user-agent-compose "mew" nil t)
;; (if (boundp 'mail-user-agent)
;;     (setq mail-user-agent 'mew-user-agent))
;; (if (fboundp 'define-mail-user-agent)
;;     (define-mail-user-agent
;;       'mew-user-agent
;;       'mew-user-agent-compose
;;       'mew-draft-send-message
;;       'mew-draft-kill
;;       'mew-send-hook))


;; use emacs-server
(server-start)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; Egg
(when (executable-find "git")
  (require 'egg nil t))

;; zlc
;(require 'zlc)
;(setq zlc-select-completion-immediately t)
