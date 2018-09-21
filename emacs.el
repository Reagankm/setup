;;Load package.el package manager
;;and then initialize
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; Add logcat-adb
(add-to-list 'load-path "~/.emacs.d/logcat-mode/")
(package-initialize)

;; Fix OS X issues where Emacs can't find the right path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; load logcat-adb
(load "logcat")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("060a78f652a7bdbd2cb1895c236117566947889c56d0e1d1947c38403ab638b0" default)))
 '(logcat-default-filters (quote ((! (=~ tag "^AndroidRuntime$")))))
 '(package-selected-packages
   (quote
    (async column-enforce-mode dash eslint-fix exec-path-from-shell flymd ggtags go-mode goto-last-change init-open-recentf latex-preview-pane magit molokai-theme recentf-ext rjsx-mode slime-theme typescript-mode undo-tree web-mode)))
 '(preview-orientation (quote below))
 '(safe-local-variable-values (quote ((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")))))

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
 '(font-lock-string-face ((t (:foreground "light sky blue"))))
 '(font-lock-type-face ((t (:foreground "cornflower blue"))))
 '(font-lock-variable-name-face ((t (:foreground "DeepSkyBlue1"))))
 '(highlight ((t (:background "purple3"))))
 '(region ((t (:background "LightSkyBlue1")))))

;; Set up Recent Files manager---------------------------------------
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
;; End of Recent Files manager set up-------------------------------

;; Set up custom keys
(global-set-key [(control z)] #'undo)
(global-set-key [(meta g)] #'goto-line)
(global-set-key [(control x) (control b)] #'ibuffer)

;; Swap ctrl and cmd keys
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'meta)

;;Require line numbers and column numbers
(global-linum-mode 1)
(setq column-number-mode t)

;; Overwrite highlighted text when typing
(delete-selection-mode 1)

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

;; Dan's function to open scratch buffer
(defun switch-to-scratch-buffer ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))
(global-set-key [(control c) ?b] #'switch-to-scratch-buffer)
;;End of customizations from Dan---------------------------------

(tool-bar-mode 0) ;;Remove GUI toolbar

;;Require line numbers and column numbers
(global-linum-mode 1)
(setq column-number-mode t)

;;---------Old solution for Javadoc comment indentation.----------
;;Try to bind M-j autoindent comments on return to return key
;;(electric-indent-mode +1)

(defun my-newline-and-indent-mode-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
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
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

;; setup files ending in .ts or .tsx to open in typescript-mode
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

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

;;Set up javascript
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode)) ;;open json as js
(setq js-indent-level 2) ;;set indentation level to 2 spaces

;;set all tab widths
(setq tab-width 2)

;; Enable undo tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Highlight selected text
(transient-mark-mode t)

;; Set up goto-last-change
(require 'goto-last-change)
 (global-set-key "\C-x\C-l" 'goto-last-change)

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

;; C-x arrow keys to maneuver between open buffers.
;; (I'm not logically inconsistent. You're logically inconsistent.)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)

;; Set forward/backward paragraph to M-n, M-p
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)

;; Delete trailing whitespace when saving a file
 (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO : Figure out how to make this work.  Grump.
;;
;;"Auto-saving .emacs: Opening output file: No such file or directory,
;;/Users/reagan.middlebrook/.emacs.d/backupsUsers/reagan.middlebrook/#!Users!reagan.middlebrook!.emacs#"

;;"Auto-saving notes.txt: Opening output file: No such file or directory,
;;/Users/reagan.middlebrook/.emacs.d/backupsUsers/reagan.middlebrook/Development/Notes/#!Users!reagan.middlebrook!Development!Notes!notes.txt#"

;; Save backups in a separate directory
;; (defvar --backup-directory (concat user-emacs-directory "backups/"))
;; (if (not (file-exists-p --backup-directory))
;;     (make-directory --backup-directory t))
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
;; (setq auto-save-file-name-transforms `(("." ,--backup-directory t)))
;; (setq make-backup-files t               ; backup of a file the first time it is saved.
;;       backup-by-copying t               ; don't clobber symlinks
;;       version-control t                 ; version numbers for backup files
;;       delete-old-versions t             ; delete excess backup files silently
;;       delete-by-moving-to-trash t
;;       kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
;;       kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
;;       auto-save-default t               ; auto-save every buffer that visits a file
;;       auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
;;       auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
;;       )

;; (setq
;;    backup-by-copying t      ; don't clobber symlinks
;;    backup-directory-alist
;;     '(("." . "~/.saves"))    ; don't litter my fs tree
;;    delete-old-versions t
;;    kept-new-versions 6
;;    kept-old-versions 2
;;    version-control t)       ; use versioned backups

;; ;; the Google way
;; (add-to-list 'backup-directory-alist '("." . "~/.saves") :append)
;; (setq backup-by-copying t
;;       delete-old-versions t
;;       kept-new-versions 6
;;       kept-old-versions 2
;;       version-control t
;;       )

;Have Latex command run pdflatex
(setq latex-run-command "pdflatex")

;; Prevent emacs from opening files with a split screen
(setq inhibit-startup-screen t)

;; Revert buffers with C-M-r
(global-set-key [134217746] (quote revert-buffer))

;; Enable subword mode in dart and Java, which makes M-f, M-b work in camelCase, and abbrev-mode, which lets you set custom abbreviations
(add-hook 'dart-mode-hook 'my-subword-mode-hook)
(add-hook 'java-mode-hook 'my-subword-mode-hook)
(defun my-subword-mode-hook ()
  (subword-mode 1)
  (abbrev-mode 1))

;; Make C-n add new lines if cursor is at the end of the buffer
(setq next-line-add-newlines t)

;; Setting up Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Show path of current file
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
  )

(global-set-key "\C-cz" 'show-file-name)

;; Set up GNU Global for ggtags
(add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))

;; Set up exec-path-from-shell, which fixes an OS X problem where Emacs launched
;; from the GUI gets given a default PATH instead of the one it would have if
;; launched from the shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Echo command characters instantly
(setq echo-keystrokes .1)

;; Make sure it never uses tabs
(setq-default indent-tabs-mode nil)

;; Make flymd live updating Markdown open in Firefox cause Chrome is jank
(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)

;; Force 4 space indentation for XML files
(setq nxml-child-indent 4 nxml-attribute-indent 4)

;; Set up yafolding to fold jsons
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))

(add-hook 'prog-mode-hook
          (lambda () (yafolding-mode)))

(require 'yafolding)
(define-key yafolding-mode-map (kbd "<C-S-return>") nil)
(define-key yafolding-mode-map (kbd "<C-M-return>") nil)
(define-key yafolding-mode-map (kbd "<C-return>") nil)
(define-key yafolding-mode-map (kbd "C-c <C-M-return>") 'yafolding-toggle-all)
(define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-hide-parent-element)
(define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)

;; Use python3 instead of python2
(setq python-shell-interpreter "python3")

;; Use rjsx-mode to edit JS files
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
;; Auto-format js and jsx files on save
(eval-after-load 'rjsx-mode
      '(add-hook 'rjsx-mode-hook (lambda () (add-hook 'after-save-hook 'eslint-fix nil t))))

;; Highlight current line
(global-hl-line-mode 1)
