
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; 默认的主模式

(add-to-list 'load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/plugins/powerline")


;;------------------cedet-------------------
(require 'cedet)
(setq default-major-mode 'text-mode)
(icomplete-mode t)
(show-paren-mode t)
(setq-default kill-whole-line t)
(defalias 'yes-or-no-p 'y-or-n-p)
(auto-image-file-mode)
(add-hook 'java-mode-hook 'hs-minor-mode) 
(add-hook 'perl-mode-hook 'hs-minor-mode) 
(add-hook 'php-mode-hook 'hs-minor-mode) 
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(global-set-key [f1] 'cua-mode)
(global-set-key [f2] 'undo) 
(global-set-key [f3] 'package-install)
(global-set-key [f5] 'replace-string)
(global-set-key [f6] 'replace-regexp)


;;------------powerline------------------
(require 'powerline)
(powerline-default-theme)
;;------------powerline------------------

;;------------pack manager---------------
;;(add-to-list 'package-archives
;;             '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("melpa" . "http://stable.melpa.org/packages/") t)
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
;;------------pack manager---------------




(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (## yasnippet-snippets cedit auto-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
