(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-list '(dockerfile-mode ir-black-theme magit markdown-mode plantuml-mode terraform-mode yaml-mode))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'ir-black t)

(setq scroll-conservatively most-positive-fixnum)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(global-set-key [?\M-p] 'scroll-down-line)
(global-set-key [?\M-n] 'scroll-up-line)

(setq-default display-line-numbers t)
(setq-default erlang-indent-level 2)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default allout-layout t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq whitespace-style '(tabs trailing lines tab-mark))
;; (setq-default show-trailing-whitespace t)
;; (set-face-attribute 'trailing-whitespace nil :background "red1" :weight 'bold)
(menu-bar-mode -1)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(recentf-mode 1)
(show-paren-mode 1)

; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html
(setq-default auto-save-default nil)
(setq-default auto-save-visited-mode t)
(setq-default auto-save-visited-file-name nil)

(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

(defun disable-magit-highlight-in-buffer ()
  (face-remap-add-relative 'magit-item-highlight '()))
(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)

(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (shell-command "./.git/hooks/prepare-commit-msg")))

(put 'downcase-region 'disabled nil)

(defun my-cut-function (text &optional rest)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))
    (let ((proc (start-process "tmux" "*Messages*" "/opt/homebrew/bin/tmux" "set-buffer" text))))))

(setq large-file-warning-threshold 100000000)
(setq interprogram-cut-function 'my-cut-function)
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 200)

(setq default-directory (concat (getenv "HOME") "/"))

(defun my--is-file-large ()
  "If buffer too large and my cause performance issue."
  (< large-file-warning-threshold (buffer-size)))

(define-derived-mode my-large-file-mode fundamental-mode "LargeFile"
  "Fixes performance issues in Emacs for large files."
  ;; (setq buffer-read-only t)
  (setq bidi-display-reordering nil)
  (jit-lock-mode nil)
  (buffer-disable-undo)
  (set (make-variable-buffer-local 'global-hl-line-mode) nil)
  (set (make-variable-buffer-local 'line-number-mode) nil)
  (set (make-variable-buffer-local 'column-number-mode) nil) )

(add-to-list 'magic-mode-alist (cons #'my--is-file-large #'my-large-file-mode))

(display-line-numbers-mode)
(setq plantuml-executable-path "/opt/homebrew/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-safe-themes nil)
 '(display-line-numbers t)
 '(global-hl-line-mode t)
 '(global-so-long-mode t)
 '(markdown-command "/opt/homebrew/bin/markdown")
 '(size-indication-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#151515" :underline nil)))))
