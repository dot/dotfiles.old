(require 'rspec-mode)

;; (eval-after-load 'ruby-mode
;;   '(add-hook 'ruby-mode-hook
;;              (lambda nil
;;                (local-set-key (kbd "C-c ,v") 'rspec-verify)
;;                (local-set-key (kbd "C-c ,a") 'rspec-verify-all)
;;                (local-set-key (kbd "C-c ,t") 'rspec-toggle-spec-and-target))))
;; (eval-after-load 'ruby-mode 
;;   '(add-hook 'rails-minor-mode-hook
;;              (lambda nil
;;                (local-set-key (kbd "C-c ,v") 'rspec-verify)
;;                (local-set-key (kbd "C-c ,a") 'rspec-verify-all)
;;                (local-set-key (kbd "C-c ,t") 'rspec-toggle-spec-and-target))))


;; Interactively Do Things (highly recommended, but not strictly required)
;; (require 'ido)
;; (ido-mode t)
;; Rinari
(require 'rinari)

;;; rhtml mode
(require 'rhtml-mode)


;; indent style
;; http://willnet.in/13
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))
