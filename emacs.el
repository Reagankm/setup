;;Load package.el package manager
;;and then initialize
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("060a78f652a7bdbd2cb1895c236117566947889c56d0e1d1947c38403ab638b0" default)))
 '(package-selected-packages
	 (quote
		(slime-theme molokai-theme web-mode undo-tree recentf-ext nlinum init-open-recentf goto-last-change))))

;;Changes the appearance so it's pretty!
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 '(default ((t (:inherit nil :stipple nil :background "gray13" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 143 :width normal :foundry "unknown" :family "Monaco"))))
 '(cursor ((t (:background "MediumPurple2"))))
 '(font-lock-comment-face ((t (:foreground "green3"))))
 '(font-lock-constant-face ((t (:foreground "cyan3"))))
 '(font-lock-function-name-face ((t (:foreground "SeaGreen1"))))
 '(font-lock-keyword-face ((t (:foreground "deep pink"))))
 '(font-lock-string-face ((t (:foreground "SlateBlue1"))))
 '(font-lock-type-face ((t (:foreground "cornflower blue"))))
'(font-lock-variable-name-face ((t (:foreground "DeepSkyBlue1")))))

;; Set up Recent Files manager
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; Update the recent file list every 20 minutes. (Otherwise it only updates on
;; exiting Emacs, and if Emacs closes unexpectedly [like I killed the terminal
;; it was running in], it doesn't save the list at all.)
(run-at-time nil (* 20 60) 'recentf-save-list)
;; Open Emacs to the recent file list
(init-open-recentf)
;; Wrap lines in the recent file list so all text is visible on the screen.
(defun visible-hook ()
  (visual-line-mode 1)
  (toggle-word-wrap)
  )
(add-hook 'recentf-dialog-mode-hook 'visible-hook)
;; End of Recent Files manager set up

;; Set up custom keys
(global-set-key [(control z)] #'undo)
(global-set-key [(meta g)] #'goto-line)
(global-set-key [(control x) (control b)] #'ibuffer)

;; Swap ctrl and cmd keys
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'meta)

;; Customizations from Dan-----------------------------------------
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

;; Added global-linum-mode below so I don't think I need this anymore:
;; (defun qtmstr-prog-mode-init ()
;; (nlinum-mode 1))

;; (add-hook 'prog-mode-hook #'qtmstr-prog-mode-init)
;;End of customizations from Dan---------------------------------

(tool-bar-mode 0) ;;Remove GUI toolbar

;;Require line numbers and column numbers
(global-linum-mode 1)
(setq column-number-mode t)

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

;; setup files ending in .php, .html, .js, .css, .ejs to open in web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

;;change web indentation to 2 spaces
(defun my-web-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
)

(add-hook 'web-mode-hook 'my-web-hook)

;(add-hook 'web-mode-hook (lambda() (
;                                    (setq web-mode-markup-indent-offset 2))))

;  (setq web-mode-css-indent-offset 2)
 ; (setq web-mode-code-indent-offset 2)
  ;(setq web-mode-enable-current-element-highlight t))))


;;Set up javascript
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode)) ;;open json as js
(setq js-indent-level 2) ;;set indentation level to 2 spaces
;(add-hook 'js-mode-hook 'js2-minor-mode) ;;connect js2-mode
;(add-hook 'js2-mode-hook 'ac-js2-mode) ;;enable autocomplete
;(setq js2-highlight-level 3) ;;set js2 syntax highlighting level
;; (0: none, 1: basic, 2: some Ecma built-in props, 3: many Ecma built-in fns)

;;set all tab widths
(setq tab-width 2)

;;Set up yasnippet and autocomplete
;; yasnippet should be load first so they can work together
;(require 'yasnippet)
;(yas-global-mode 1)
;; auto complete mod should be loaded after yasnippet so that they can work together
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")

;; Dan's function to open scratch buffer
(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))
(global-set-key [(control c) ?b] #'switch-to-scratch-buffer)

;; Enable undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Highlight selected text
(transient-mark-mode t)

;; Set up goto-last-change
(require 'goto-last-change)
 (global-set-key "\C-x\C-\\" 'goto-last-change)

;; Turn off annoying ding noise
(setq ring-bell-function 'ignore)

;; Turn off arrow keys
;; (May God have mercy on my soul)
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

;; Delete trailing whitespace when saving a file
 (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Save backups in a separate directory
;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    backup-directory-alist
;;     '(("." . "~/.saves"))    ; don't litter my fs tree
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups

;; the Google way
(add-to-list 'backup-directory-alist '("." . "~/.saves") :append)
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      )

;Have Latex command run pdflatex
(setq latex-run-command "pdflatex")

;; Prevent emacs from opening files with a split screen
(setq inhibit-startup-screen t)

;; Revert buffers with C-M-r
(global-set-key [134217746] (quote revert-buffer))

;; This was in my other .emacs setup and I don't remember why, so keeping it
;; around in case I want it later
;; Return reverse search to C-r
;;(global-set-key [18] (quote isearch-backward))

;; Something else from an old setup that I may want again
;; Enable ediff-trees
;;(require 'ediff-trees)

;; Enable subword mode in dart, which makes M-f, M-b work in camelCase, and abbrev-mode, which lets you set custom abbreviations
(add-hook 'dart-mode-hook 'my-dart-mode-hook)
(defun my-dart-mode-hook ()
  (subword-mode 1)
  (abbrev-mode 1))

;; Make C-n add new lines if cursor is at the end of the buffer
(setq next-line-add-newlines t)
