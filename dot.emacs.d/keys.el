;; keybind
;; CmdとOpt(meta)を入れ替える
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; 入力モードの変更
(global-set-key "\C-o" 'toggle-input-method)

;; to swap C-h and Backspace
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)

;; avoid hiding with M-h
(setq mac-command-key-is-meta t)
(setq mac-pass-command-to-system nil)
(setq mac-pass-control-to-system nil)
(setq mac-pass-option-to-system nil)

;; バッファ切り替えを簡単に
(global-set-key "\M-N" 'next-buffer)
(global-set-key "\M-P" 'previous-buffer)

(defun my-kill-or-delete-line ()
  "ポイントが空行ならキルリングに追加しない"
  (interactive)
  (if (and (bolp) (eolp)) ;お気に入り
      (my-delete-line)
    (kill-line)))

;; ポイント位置が空行なら C-k してもキルリングに追加しない
;; see http://d.hatena.ne.jp/kitokitoki/20100904/p2
;; kill-line から置換。もっと縮められそう。
(defun my-delete-line (&optional arg)
  (interactive "P")
  (delete-region (point)
                 (progn
                   (if arg
                       (forward-visible-line (prefix-numeric-value arg))
                     (if (eobp)
                         (signal 'end-of-buffer nil))
                     (let ((end
                            (save-excursion
                              (end-of-visible-line) (point))))
                       (if (or (save-excursion
                                 (unless show-trailing-whitespace
                                   (skip-chars-forward " \t" end))
                                 (= (point) end))
                               (and kill-whole-line (bolp)))
                           (forward-visible-line 1)
                         (goto-char end))))
                   (point))))

(global-set-key (kbd "C-k") 'my-kill-or-delete-line)