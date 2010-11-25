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