;; Menloを使う
(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
                  'unicode
                  (font-spec :family "Hiragino Kaku Gothic ProN" :size 16)
                  nil
                  'append)
;;(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))

;; Ricty {{{2 (http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html)
(set-face-attribute 'default nil
                   :family "Ricty"
                   :height 160)

(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))

(set-fontset-font
 (frame-parameter nil 'font)
 'katakana-jisx0201
 '("Hiragino Maru Gothic Pro" . "iso10646-1"))

;; (let* ((asciifont "Source Code Pro")
;;               (jpfont "Hiragino Maru Gothic ProN")
;;               (fontspec (font-spec :family asciifont :size 9))
;;               (jp-fontspec (font-spec :family jpfont :size 9)))
;;     (set-face-attribute 'default nil :family asciifont)
;;     (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
;;     (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
;;     (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
;;     (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
;;     (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
;;     )
;; ;;; フォントサイズの比を設定
;; (dolist (elt '((".*Hiragino.*" . 2.0)))
;;     (add-to-list 'face-font-rescale-alist elt))
