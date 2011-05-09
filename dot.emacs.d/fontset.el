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
