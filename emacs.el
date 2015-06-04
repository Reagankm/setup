;;Load package.el package manager
;;and then initialize
(require 'package)
(package-initialize) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(echo-keystrokes 0.01)
 '(indent-tabs-mode nil)
 )

;;Changes the appearance so it's pretty!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray13" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(cursor ((t (:background "MediumPurple2"))))
 '(font-lock-comment-face ((t (:foreground "green3"))))
 '(font-lock-constant-face ((t (:foreground "cyan3"))))
 '(font-lock-function-name-face ((t (:foreground "SeaGreen1"))))
 '(font-lock-keyword-face ((t (:foreground "deep pink"))))
 '(font-lock-string-face ((t (:foreground "SlateBlue1"))))
 '(font-lock-type-face ((t (:foreground "cornflower blue"))))
 '(font-lock-variable-name-face ((t (:foreground "DeepSkyBlue1"))))
 )

;;Set up Recent Files manager
(require 'recentf)
(recentf-mode 1) ;;In orig file this was recentf-mode t and it was in custom-set-variables
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;Set up custom keys
(global-set-key [(control z)] #'undo)
(global-set-key [(meta g)] #'goto-line)
(global-set-key [(control x) (control b)] #'ibuffer)

;;Customizations from Dan-----------------------------------------
;;; iswitchb isn't fucking obsolete
(eval-when-compile
  (put 'iswitchb-mode 'byte-obsolete-info nil))
(require 'iswitchb)
(show-paren-mode 1)
(let ((initfunc (intern "iswitchb-default-keybindings")))
(if (fboundp initfunc)
(funcall initfunc)
(iswitchb-mode 1)))
(defun qtmstr-setup-iswitchb-map ()
(define-key iswitchb-mode-map [(left)] #'iswitchb-prev-match)
(define-key iswitchb-mode-map [(right)] #'iswitchb-next-match))
(setq iswitchb-regexp t)
(add-hook 'iswitchb-define-mode-map-hook #'qtmstr-setup-iswitchb-map)

(require 'prog-mode)

(defun qtmstr-prog-mode-init ()
(nlinum-mode 1))

(add-hook 'prog-mode-hook #'qtmstr-prog-mode-init)
;;End of customizations from Dan---------------------------------

(tool-bar-mode 0) ;;Remove GUI toolbar


;;---------Old solution for Javadoc comment indentation.----------
;;Try to bind M-j autoindent comments on return to return key
;;(electric-indent-mode +1)

(defun my-newline-and-indent-mode-hook ()
  (local-set-key (kbd "RET") (key-binding (kbd "M-j")))
  (local-set-key (kbd "<C-return>") #'electric-indent-just-newline)
)

;;(add-hook 'prog-mode-hook #'my-newline-and-indent-mode-hook)(defun my-newline-and-indent-mode-hook ()
;;  (local-set-key (kbd "<C-return>") #'electric-indent-just-newline)
;;)
;;----------End old solution-------------------------

;; New attempted solution for Javadoc comment indentation
;; Make it so hitting Return in a Javadoc comment will automatically
;; add a star and indent the next line appropriately
;;(define-key 'java-mode-map (kbd "RET") (key-binding (kbd "M-j")))
(add-hook 'java-mode-hook #'my-newline-and-indent-mode-hook)


;; Overwrite highlighted text when typing
(delete-selection-mode 1)

;; setup files ending in “.g4” to open in antlr-mode
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

