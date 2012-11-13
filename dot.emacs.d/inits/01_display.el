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

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; 対応する括弧をハイライト
(show-paren-mode 1)

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
                   '(width . 60) ;; ウィンドウ幅
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
(set-frame-parameter (selected-frame) 'alpha '(85 85))

;; fontセットを読み込む
(load "~/.emacs.d/fontset.el")

;; タブ文字、全角空白、文末の空白の色付け
;; @see http://www.emacswiki.org/emacs/WhiteSpace
;; @see http://xahlee.org/emacs/whitespace-mode.html
(require 'whitespace)
(global-whitespace-mode 1)
;; 常に whitespace-mode だと動作が遅くなる場合がある
;(global-set-key (kbd "C-x w") 'global-whitespace-mode)

(set-face-foreground 'whitespace-newline "gray5")
(set-face-foreground 'whitespace-tab "gray25")
(set-face-background 'whitespace-tab 'nil)
;(set-face-underline  'whitespace-tab "SteelBlue")
(set-face-foreground 'whitespace-trailing "gray25")
(set-face-background 'whitespace-trailing "gray12")
(set-face-bold-p 'whitespace-trailing 'nil)

(setq whitespace-style '(face tabs space-mark tab-mark newline-mark trailing))
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□]) ;; 全角スペース
        (newline-mark ?\n [?\u21B5 ?\n] [?$ ?\n])
;        (newline-mark ?\n [9166 ?\n] [?$ ?\n])
        (tab-mark 9 [8614 9] [92 9])
        ))
;; ;; EOB を表示
;(setq-default indicate-empty-lines t)
;(setq-default indicate-buffer-boundaries 'left)

;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-tomorrow-night-bright)))