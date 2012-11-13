;; lang
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; mail address
(setq user-mail-address "shuhei.kondo@gmail.com")

;; バックアップファイルを作らない
(setq make-backup-files nil)
;; ウィンドウを新しく開かない
(setq ns-pop-up-frames nil)
;; D&Dした際にファイルを開く
(define-key global-map [ns-drag-file] 'ns-find-file)
;; 削除ファイルをゴミ箱に入れる
(setq delete-by-moving-to-trash t)

;; inhibit bell ring
(setq ring-bell-function 'ignore)

;; scroll settings
(setq scroll-step 1)

;; inhibit truncate
(setq-default truncate-lines nil)
(setq-default truncate-partial-width-windows t)

;; indent settings
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq-default tab-width 4)
(c-set-offset 'case-label '+)

;; enable narrowing
(put 'narrow-to-region 'disabled nil)

;; hide startup message
(setq inhibit-startup-message t)

;; set time local
(setq system-time-locale "C")


;; environment path
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))
