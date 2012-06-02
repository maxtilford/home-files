
(add-to-list 'default-frame-alist '(font . "*-dina-medium-r-*-*-16-*-*-*-*-*-*-*"))

;; automatically ensure a final newline
(setq require-final-newline t)


;;to set foreground color to white
(set-foreground-color "white")

;;to set background color to black
(set-background-color "black")

(set-cursor-color "red")

(menu-bar-mode -1)
(setq line-number-mode t)
(setq column-number-mode t)

(setq-default c-basic-offset 2)
(defun my-indent-setup ()
      (c-set-offset 'arglist-intro '+))
    (add-hook 'java-mode-hook 'my-indent-setup)

(show-paren-mode t)

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-to-list 'load-path "~/.emacs.d")
(autoload 'haml-mode "haml-mode" "haml files" t)
(add-to-list 'auto-mode-alist '("\.haml$" . haml-mode))

;; (add-to-list 'load-path "~/.emacs.d/sass-mode.el")
(autoload 'sass-mode "sass-mode" "sass files" t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))


(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-hook 'sass-mode-hook 'rainbow-turn-on)
(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(add-to-list 'load-path "~/.emacs.d/mustache-mode.el")
(require 'mustache-mode)

;; and load it

(autoload 'yaml-mode "yaml-mode" "yaml" t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(setq haskell-program-name "/usr/bin/ghci")
(require 'inf-haskell)
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb org-bibtex org-gnus org-id org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
; global STYLE property values for completion
(setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)



(iswitchb-mode 1)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-id-method (quote uuidgen))


(setq message-mode-hook
      (quote (orgstruct++-mode
              (lambda nil (setq fill-column 72) (flyspell-mode 1))
              turn-on-auto-fill
              bbdb-define-all-aliases)))



(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(clean-buffer-list-delay-general 2)
 '(column-number-mode t)
 '(desktop-base-file-name "emacs.desktop")
 '(desktop-base-lock-name "emacs.desktop.lock")
 '(desktop-load-locked-desktop (quote ask))
 '(desktop-missing-file-warning t)
 '(desktop-save t)
 '(feature-indent-level 2)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message "
;; Halloo
")
 '(tab-width 2)
 '(js-indent-level 2)
 '(midnight-mode t nil (midnight))
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren nil)
 '(scheme-program-name "csi")
 '(sgml-xml-mode t)
 '(show-paren-mode t)
 '(standard-indent 2)
 '(tab-always-indent t)
 '(tab-stop-list (quote (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40)))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))


(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

(tool-bar-mode -1)

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))




(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; android-mode
;;(add-to-list 'load-path "~/opt/android-mode")
;;(require 'android-mode)
;;(defcustom android-mode-sdk-dir "~/opt/android" "android-mode-sdk")

(setq javascript-indent-level 2)

(require 'dtrt-indent)

(add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
(require 'coffee-mode)

(put 'downcase-region 'disabled nil)
