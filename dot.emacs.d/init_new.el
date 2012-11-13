;; -*- emacs-lisp -*-

;; load paths
(add-to-list 'load-path "~/.emacs.d/site-lisp/howm")
(add-to-list 'load-path "~/.emacs.d/site-lisp/modes")
(add-to-list 'load-path "~/.emacs.d/site-lisp/apel")
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; filenome prefix:
;;   - 00 : fundamental settings
;;   - 10 : pre load settings
;;   - 20 : custom functions
;;   - 30 : additional functions
;;   - 40 : minor mode settings
;;   - 50 : major mode settings
;;   - 90 : post load settings
(require 'init-loader)

;; エラーファイルをわかりやすくする
;; http://d.hatena.ne.jp/kitokitoki/20101205/p1
(defun init-loader-re-load (re dir &optional sort)
  (let ((load-path (cons dir load-path)))
    (dolist (el (init-loader--re-load-files re dir sort))
      (condition-case e
          (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))
            (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
        (error
         ;; (init-loader-error-log (error-message-string e)) ；削除
         (init-loader-error-log (format "%s. %s" (locate-library el) (error-message-string e))) ;追加
         )))))

(init-loader-load "~/.emacs.d/inits")
