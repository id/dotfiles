(add-to-list 'load-path "~/.emacs.d")

(setq whitespace-style '(tabs trailing lines tab-mark))
(setq whitespace-line-column 80)
(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil :background "red1" :weight 'bold)
(global-whitespace-mode 1)

(defun highlight-tabs ()
  "Highlight tab characters."
  (font-lock-add-keywords
   nil '(("\\(\t+\\)" 1 '(:background "#333333") t))))
(add-hook 'erlang-mode-hook 'highlight-tabs)

(column-number-mode 1)
(global-linum-mode 1)
(setq linum-format "%d ")
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil :underline nil)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(global-set-key [?\M-p] 'scroll-down-line)
(global-set-key [?\M-n] 'scroll-up-line)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(recentf-mode 1)
(show-paren-mode 1)

(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

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
          '(lambda ()
             (shell-command "./.git/hooks/prepare-commit-msg")))

(load-theme 'ir-black t)

(put 'downcase-region 'disabled nil)

(defun set-erlang-indent (level)
  (interactive "nSet erlang indent level to: ")
  (setq erlang-indent-level level))

(defun switch-erlang-indent ()
  (interactive)
  (if (equal erlang-indent-level 2)
      (setq erlang-indent-level 4)
    (setq erlang-indent-level 2)))


(defun my-cut-function (text &optional rest)
  (let ((process-connection-type nil))
    (start-process "emacs_share_buffer.sh" "*Messages*" "emacs_share_buffer.sh" text)))

(setq interprogram-cut-function 'my-cut-function)
