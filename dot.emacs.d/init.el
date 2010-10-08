;;; ========== Emacsの基本設定 ==========
; 言語を日本語にする
(set-language-environment 'Japanese)
; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;;; load path settings
(add-to-list 'load-path "~/.emacs.d/site-lisp/howm")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; auto-install
(require 'auto-install)
(add-to-list 'load-path auto-install-directory)
;; emacs wiki
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; fontセットを読み込む
(load "~/.emacs.d/fontset.el")
;; key設定 を読み込む
(load "~/.emacs.d/keys.el")
;; モード設定 を読み込む
(load "~/.emacs.d/modes.el")


;;;; 見た目
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
(setq line-spacing 0.1) ;; 行の高さの0.1倍

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

;; 選択範囲に色をつける
(setq transient-mark-mode t)
(setq highlight-nonselected-windows t)

;; スクロールする際に重なって表示するサイズ
(setq next-screen-context-lines 10)

;; 透過処理
(set-frame-parameter (selected-frame)  'alpha  '(90 90))

;; minibuffer isearch
(require 'minibuf-isearch)

;; howm
(require 'howm)
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)

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
;; M-x の補完候補をミニバッファに表示
(icomplete-mode 1)

