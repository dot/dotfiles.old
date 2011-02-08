;;; ========== Emacsの基本設定 ==========
; 言語を日本語にする
(set-language-environment 'Japanese)
; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;;; load path settings
(add-to-list 'load-path "~/.emacs.d/site-lisp/howm")
(add-to-list 'load-path "~/.emacs.d/site-lisp/modes")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; environment path
(setenv "PATH" (concat "/Users/kondo/bin:/Users/kondo/.gem/ruby/1.8/bin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:/usr/local/mongodb/bin:/opt/local/bin:/opt/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin:"
                       (getenv "PATH")))

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
;; load plugins
(load "~/.emacs.d/plugins/subdirs.el")
;; load private
(load "~/.emacs.d/private.el")

;; emacsclientのバッファを閉じるときに確認しない
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; 古いバッファを自動的に閉じる
;; (require 'tempbuf)
;; (add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; (add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)


(setq visible-bell t)
;; バックアップファイルを作らない
(setq make-backup-files nil)
;; ウィンドウを新しく開かない
(setq ns-pop-up-frames nil)
;; D&Dした際にファイルを開く
(define-key global-map [ns-drag-file] 'ns-find-file)
;; 削除ファイルをゴミ箱に入れる
(setq delete-by-moving-to-trash t)
;; M-x の補完候補をミニバッファに表示
(icomplete-mode 1)
;; 同じファイル名のバッファを区別しやすくする
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;; minibuffer isearch
(require 'minibuf-isearch)

;; ファイル名の保管で大文字小文字を区別しない
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; howm
(require 'howm)
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t)

;; recentf
(require 'recentf)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 500)
(setq recentf-exclude '("^/[^/:]+:"))
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

;; anything
(require 'anything-startup)
(require 'anything-gtags)
; anything-my-default() を設定する
(define-key global-map (kbd "C-;") 'anything-my-default)
(defun anything-my-default ()
  "Anything command for you. It is automatically generated by `anything-migrate-sources'."
  (interactive)
  (anything-other-buffer
    '(
      ((name . "Buffers")
       (candidates . anything-c-buffer-list)
       (type . buffer)
       (candidate-transformer
        anything-c-skip-current-buffer
        anything-c-highlight-buffers
        anything-c-skip-boring-buffers)
       (persistent-action . anything-c-buffers+-persistent-action)
       (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer"))
      ((name . "Bookmarks")
       (init lambda nil (require (quote bookmark))) (candidates . bookmark-all-names)
       (type . bookmark))
      ((name . "Recentf")
       (init lambda nil
             (require (quote recentf))
             (or recentf-mode (recentf-mode 1))
             (when (and (numberp recentf-max-saved-items) (<= recentf-max-saved-items 20))
               (setq recentf-max-saved-items 500))) (candidates . recentf-list)
               (match anything-c-match-on-file-name anything-c-match-on-directory-name)
               (type . file))
      ((name . "File Name History")
       (candidates . file-name-history)
       (match anything-c-match-on-file-name anything-c-match-on-directory-name) (type . file)) 
      ((name . "Files from Current Directory")
       (candidates lambda nil
                   (with-current-buffer anything-current-buffer
                     (directory-files (anything-c-current-directory) t)))
       (candidate-transformer anything-c-highlight-files) (type . file))
      ((name . "mdfind")
       (candidates lambda nil (start-process "mdfind-process" nil "mdfind" anything-pattern))
       (type . file) (requires-pattern . 3) (delayed))
      ((name . "Locate") (candidates . anything-c-locate-init) (type . file) (requires-pattern . 3) (delayed)) 
      ((name . "Emacs Commands")
       (candidates lambda nil
                   (let (commands) (mapatoms (lambda (a)
                                               (if (commandp a) (push (symbol-name a) commands))))
                        (sort commands (quote string-lessp)))) (type . command) (requires-pattern . 2)))
    "*anything-my-default*"))
;; (setq anything-sources (list anything-c-source-buffers+
;;                              anything-c-source-bookmarks
;;                              anything-c-source-recentf
;;                              anything-c-source-file-name-history
;;                              anything-c-source-files-in-current-dir+
;;                              anything-c-source-mac-spotlight
;;                              anything-c-source-locate
;;                              anything-c-source-emacs-commands))
;; (global-set-key (kbd "C-;") 'anything-filelist+)
;; ;;(global-set-key (kbd "C-;") 'anything)
(define-key anything-map (kbd "C-M-n") 'anything-next-source)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)

;; Anything-grep
;; anything-grep-by-name
(setq anything-grep-alist
  '(("buffers" ("egrep -Hin %s $buffers" "/"))
    ("howm" ("ack -af | xargs egrep -Hin '%s'" "~/howm"))
    ("ruby" ("ack -afG 'rb$' | xargs egrep -Hin %s" "/usr/local/lib/ruby/1.8"))
    ("gems" ("ack -afG 'rb$' | xargs egrep -Hin %s" "/Users/kondo/.gem/ruby/1.8/gems"))
    )
)

;;; anything-c-moccurの設定
(require 'anything-c-moccur)
(setq anything-c-moccur-anything-idle-delay 0.2
      anything-c-moccur-higligt-info-line-flag t
      anything-c-moccur-enable-auto-look-flag t
      anything-c-moccur-enable-initial-pattern t)
;;; anything-c-moccur key bind
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;; ruby tools
(load "~/.emacs.d/ruby.el")

;; kill-summery
(autoload 'kill-summary "kill-summary" nil t)
;(global-set-key "\M-y" 'kill-summary)
(setq kill-ring-max 100)
;; anything
(setq anything-kill-ring-threshold 5)
(global-set-key "\M-y" 'anything-show-kill-ring)

;; minibuffer isearch
(require 'minibuf-isearch)

;; minibufferでC-wを使えるようにする
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

;; bm
(require 'bm)
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key "\M-2" 'bm-toggle)
(global-set-key [f2]   'bm-next)
(global-set-key [S-f2] 'bm-previous)

;;; color-moccur.elの設定
(require 'color-moccur)
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t)
  (setq moccur-use-migemo t))

;; grep command
;(grep-apply-setting 'grep-command "ack -afG '(rb|el)$' | xargs egrep -Hin ")
(grep-apply-setting 'grep-command "ack -af | xargs egrep -Hin ")

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; ;; zlc
;; (require 'zlc)
;; (setq zlc-select-completion-immediately t)
;; (let ((map minibuffer-local-map))
;;   ;;; like menu select
;;   (define-key map (kbd "<down>")  'zlc-select-next-vertical)
;;   (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
;;   (define-key map (kbd "<right>") 'zlc-select-next)
;;   (define-key map (kbd "<left>")  'zlc-select-previous)

;;   ;;; reset selection
;;   (define-key map (kbd "C-c") 'zlc-reset)
;;   )

;; simple note
(require 'simplenote)
(simplenote-setup)

;;
(require 'org-html5presentation)

;; global
(require 'gtags) 
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key "\M-t" 'gtags-find-tag)
         (local-set-key "\M-r" 'gtags-find-rtag)
         (local-set-key "\M-s" 'gtags-find-symbol)
         (local-set-key "\C-t" 'gtags-pop-stack)
         ))
;; mode hook
(add-hook 'java-mode-hook (lambda () (gtags-mode 1)))
(add-hook 'c-mode-hook (lambda () (gtags-mode 1)))
(add-hook 'c++-mode-hook (lambda () (gtags-mode 1)))

;; save place
(load "saveplace")
(setq-default save-place t)
