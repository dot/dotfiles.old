;; モードラインの変更
(display-time)
(line-number-mode 1)
(column-number-mode 1)

(tool-bar-mode 0)
(setq visible-bell nil)
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



;; fontセットを読み込む
(load "~/.emacs.d/fontset.el")
