(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-list '(dockerfile-mode catppuccin-theme magit markdown-mode plantuml-mode terraform-mode yaml-mode))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'catppuccin t)
(setq catppuccin-flavor 'mocha)
(catppuccin-set-color 'base "#000000")
(catppuccin-reload)

(use-package erlang
  :load-path ("/opt/otp-25/lib/tools-3.5.3/emacs")
  :mode (("\\.erl?$" . erlang-mode)
	 ("rebar\\.config$" . erlang-mode)
	 ("relx\\.config$" . erlang-mode)
	 ("sys\\.config\\.src$" . erlang-mode)
	 ("sys\\.config$" . erlang-mode)
	 ("\\.config\\.src?$" . erlang-mode)
	 ("\\.config\\.script?$" . erlang-mode)
	 ("\\.hrl?$" . erlang-mode)
	 ("\\.app?$" . erlang-mode)
	 ("\\.app.src?$" . erlang-mode)
	 ("\\Emakefile" . erlang-mode)))

(add-hook 'erlang-mode-hook 'eglot-ensure)

(setq load-path (cons  "/Users/id/code/copilot.el" load-path))
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-hook 'git-commit-mode-hook 'copilot-mode)
(add-hook 'org-mode-hook 'copilot-mode)
(add-hook 'yaml-mode-hook 'copilot-mode)
(add-hook 'markdown-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(setq scroll-conservatively most-positive-fixnum)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(global-set-key [?\M-p] 'scroll-down-line)
(global-set-key [?\M-n] 'scroll-up-line)

(display-line-numbers-mode)
(setq-default erlang-indent-level 4)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default allout-layout t)

(setq whitespace-style '(tabs trailing lines tab-mark))
(menu-bar-mode -1)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(recentf-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 1)

(setq inhibit-splash-screen t)
(setq initial-scratch-message "")

; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html
(setq-default auto-save-default nil)
(setq-default auto-save-visited-mode t)
(setq-default auto-save-visited-file-name nil)

(setq make-backup-files nil)

(setq require-final-newline t)
(setq vc-follow-symlinks t)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; (defun disable-magit-highlight-in-buffer ()
;;   (face-remap-add-relative 'magit-item-highlight '()))
;; (add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)

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
    (let ((proc (start-process "tmux" "*Messages*" "tmux" "set-buffer" text))))))

(setq interprogram-cut-function 'my-cut-function)

(setq default-directory (concat (getenv "HOME") "/"))

(setq plantuml-executable-path "/opt/homebrew/bin/plantuml")
(setq plantuml-default-exec-mode 'executable)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-locale-environment "en_US.UTF-8")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "611ef0918b8b413badb8055089b5499c1d4ac20f1861efba8f3bfcb36ad0a448" "15604b083d03519b0c2ed7b32da6d7b2dc2f6630bef62608def60cdcf9216184" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "38f04d6cff372da39a8d0451a4903681e52a5a9702f37021cf1d3d246f0b37c6" "f5e666fba0ded6ae9be004314ecf5f7feb605cdb84711b5c5ffd81acfb831183"))
 '(display-line-numbers t)
 '(global-hl-line-mode t)
 '(global-so-long-mode t)
 '(markdown-command "/opt/homebrew/bin/pandoc")
 '(package-selected-packages
   '(jinja2-mode go-mode yaml-mode typescript-mode terraform-mode s rust-mode powershell plantuml-mode modus-themes markdown-mode magit ir-black-theme fill-column-indicator elixir-mode eglot editorconfig dockerfile-mode cmake-mode catppuccin-theme))
 '(safe-local-variable-values
   '((standard-indent . 2)
     (encoding . utf-8)
     (c-indent-level . 2)
     (allout-layout . t)))
 '(size-indication-mode t)
 '(standard-indent 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#151515" :underline nil)))))
