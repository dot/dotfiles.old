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




