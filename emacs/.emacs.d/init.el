(setq whitespace-style '(tabs trailing lines tab-mark))
(setq whitespace-line-column 80)
(setq-default show-trailing-whitespace t)
(set-face-attribute 'trailing-whitespace nil :background "red1" :weight 'bold)
(menu-bar-mode -1)

(column-number-mode 1)
(global-linum-mode 1)
(setq linum-format "%d ")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(load-theme 'ir-black t)

(setq scroll-conservatively most-positive-fixnum)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(global-set-key [?\M-p] 'scroll-down-line)
(global-set-key [?\M-n] 'scroll-up-line)

(setq-default erlang-indent-level 2)
(setq-default js-indent-level 2)
(setq-default allout-layout t)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(recentf-mode 1)
(show-paren-mode 1)

(setq auto-save-visited-mode t)

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
          '(lambda ()
             (shell-command "./.git/hooks/prepare-commit-msg")))

(put 'downcase-region 'disabled nil)

(defun my-cut-function (text &optional rest)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))
    (let ((proc (start-process "tmux" "*Messages*" "/usr/local/bin/tmux" "set-buffer" text))))))

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
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq tide-node-executable "/usr/local/bin/node")
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
)

(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (lambda ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))

(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-hook 'web-mode-hook  'my-web-mode-hook)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("25da85b0d62fd69b825e931e27079ceeb9fd041d14676337ea1ce1919ce4ab17" "9e39a8334e0e476157bfdb8e42e1cea43fad02c9ec7c0dbd5498cf02b9adeaf1" default))
 '(markdown-command "/usr/local/bin/markdown")
 '(package-selected-packages
   '(atom-one-dark-theme web-mode tide yaml-mode typescript-mode scala-mode go-mode elixir-mode yafolding json-mode erlang docker-compose-mode dockerfile-mode org-alert org markdown-mode terraform-mode magit ir-black-theme))
 '(typescript-indent-level 2))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
