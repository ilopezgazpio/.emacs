;; User Details
(setq user-full-name "Inigo Lopez-Gazpio")
(setq user-mail-address "inigo.lopez@ehu.eus")

;; Environment
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:" (getenv "PATH")))
;; Uncomment the line below and adjust it according to your Go installation path if needed
;; (add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))

(require 'cl)

;; Initial screen settings
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode)

;; GUI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Text selection and clipboard settings
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Display settings
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Indentation
(setq tab-width 2
      indent-tabs-mode nil)

;; Backup files
(setq make-backup-files nil)

;; Simplified yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-c m") 'magit-status)

;; Echo keystrokes quickly
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

(show-paren-mode t)

;; Ensure truncate-lines is enabled by default
(setq-default truncate-lines t)
(setq column-number-mode t)

;; Enable global auto-revert mode
(global-auto-revert-mode t)

;; Org mode
(require 'org)

;; Buffer cleanup functions
(defun untabify-buffer ()
  "Convert all tabs in buffer to spaces."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Clean up whitespace in the current buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Clean up a selected region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

;; Bind cleanup functions to keys
(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; Flyspell settings
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; Enable flyspell mode by default
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defun set-spell-en ()
  (interactive)
  (ispell-change-dictionary "en_US")
  (flyspell-buffer))

(defun set-spell-es ()
  (interactive)
  (ispell-change-dictionary "es")
  (flyspell-buffer))

(defun set-spell-eu ()
  (interactive)
  (ispell-change-dictionary "eu")
  (flyspell-buffer))


(global-set-key (kbd "C-c s en") 'set-spell-en)
(global-set-key (kbd "C-c s es") 'set-spell-es)
(global-set-key (kbd "C-c s eu") 'set-spell-es)

