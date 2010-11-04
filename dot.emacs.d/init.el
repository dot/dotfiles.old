;;; ========== Emacsの基本設定 ==========
; 言語を日本語にする
(set-language-environment 'Japanese)
; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;;; load path settings
(add-to-list 'load-path "~/.emacs.d/site-lisp/howm")
(add-to-list 'load-path "~/.emacs.d/site-lisp/modes")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; auto-install
(require 'auto-install)
(add-to-list 'load-path auto-install-directory)
;; emacs wiki
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; key設定 を読み込む
(load "~/.emacs.d/keys.el")
;; モード設定 を読み込む
(load "~/.emacs.d/modes.el")
;; 見た目周り
(load "~/.emacs.d/look_and_feel.el")

;; バックアップファイルを作らない
(setq make-backup-files nil)

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
                             anything-c-source-emacs-commands
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
;(global-set-key "\M-y" 'kill-summary)
(setq kill-ring-max 20)
;; anything
(setq anything-kill-ring-threshold 5)
(global-set-key "\M-y" 'anything-show-kill-ring)


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

;; minibufferでC-wを使えるようにする
;;(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
;; (define-key minibuffer-local-completion-map "\C-w"
;;   #'(lambda (arg)
;;       (interactive "p")
;;       (if (eolp)
;;           (backward-kill-word arg)
;;         (kill-line arg))))
;; (defun ys:minibuffer-delete-file-name (&optional arg)
;;   (interactive "p")
;;   (if (eolp)
;;       (save-excursion
;;         (while (and (< 0 arg)
;;                     (re-search-backward "[/\\][^/\\]+[/\\]?$" nil t))
;;           (replace-match "/")
;;           (setq arg (1- arg))))
;;     (kill-line arg))
;; )
;; (define-key minibuffer-local-completion-map "\C-w" 'ys:minibuffer-delete-file-name)

(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

;; bm
(require 'bm)
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key "\M-2" 'bm-toggle)
(global-set-key [f2]   'bm-next)
(global-set-key [S-f2] 'bm-previous)


;; rcodetools
(require 'rcodetools)
(setq rct-find-tag-if-available nil)
(defun make-ruby-scratch-buffer ()
  (with-current-buffer (get-buffer-create "*ruby scratch*")
    (ruby-mode)
    (current-buffer)))
(defun ruby-scratch ()
  (interactive)
  (pop-to-buffer (make-ruby-scratch-buffer)))
(defun ruby-mode-hook-rcodetools ()
  (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-d" 'xmp)
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)

(require 'anything-rcodetools)
(setq rct-get-all-methods-command "PAGER=cat ri -l")
;; See docs
(define-key anything-map [(control ?;)] 'anything-execute-persistent-action)


;; anything-c-moccur
;;; color-moccur.elの設定
(require 'color-moccur)
;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))

;;; anything-c-moccurの設定
(require 'anything-c-moccur)
;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;; キーバインドの割当(好みに合わせて設定してください)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))


;; ruby tools
(load "~/.emacs.d/ruby.el")
