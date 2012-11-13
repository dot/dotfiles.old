;; to swap Cmd Opt(meta)
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; input mode
(global-set-key "\C-o" 'toggle-input-method)

;; to swap C-h and Backspace
(keyboard-translate ?\C-h ?\C-?)
(global-set-key "\C-h" nil)
;(global-set-key "\C-?" 'help-command)

;; avoid hiding with M-h
(setq mac-command-key-is-meta t)
(setq mac-pass-command-to-system nil)
(setq mac-pass-control-to-system nil)
(setq mac-pass-option-to-system nil)

;; バッファ切り替えを簡単に
(global-set-key "\M-N" 'next-buffer)
(global-set-key "\M-P" 'previous-buffer)
