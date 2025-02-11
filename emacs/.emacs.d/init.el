(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-list '(dockerfile-mode magit markdown-mode plantuml-mode terraform-mode yaml-mode hcl-mode editorconfig jsonrpc f s dash lsp-mode ivy))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-icon (display-graphic-p)))

(use-package ivy
  :defer 0.1
  :diminish
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config (ivy-mode))

(defun add-erlang-emacs-to-load-path ()
  (let* ((erlang-lib-dir "/opt/homebrew/opt/erlang/lib/erlang/lib/")
         (tools-dir (car (file-expand-wildcards (concat erlang-lib-dir "tools-*/emacs")))))
    (when tools-dir
      (setq load-path (cons tools-dir load-path)))))

(add-erlang-emacs-to-load-path)
(setq erlang-root-dir "/opt/homebrew/opt/erlang")
(setq exec-path (cons "/opt/homebrew/opt/erlang/bin" exec-path))
(add-to-list 'auto-mode-alist '("rebar\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("relx\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("sys\\.config\\.src$" . erlang-mode))
(add-to-list 'auto-mode-alist '("sys\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hocon$" . hcl-mode))
(require 'erlang-start)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(use-package lsp-mode
  :config
  ;; Enable LSP automatically for Erlang files
  (add-hook 'erlang-mode-hook #'lsp)
  (add-hook 'elixir-mode-hook #'lsp)
  (add-hook 'sh-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'javascript-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)

  ;; ELP, added as priority 0 (> -1) so takes priority over the built-in one
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
                    :major-modes '(erlang-mode)
                    :priority 0
                    :server-id 'erlang-language-platform))
)
(setq lsp-warn-no-matched-clients nil)
(setq lsp-ui-sideline-enable nil)
(setq lsp-lens-enable nil)

(setq load-path (cons  "~/code/copilot.el" load-path))
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-hook 'git-commit-mode-hook 'copilot-mode)
(add-hook 'org-mode-hook 'copilot-mode)
(add-hook 'yaml-mode-hook 'copilot-mode)
(add-hook 'markdown-mode-hook 'copilot-mode)
(setq copilot-indent-offset-warning-disable t)
(setq copilot-max-char-warning-disable t)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(global-set-key [?\M-p] 'scroll-down-line)
(global-set-key [?\M-n] 'scroll-up-line)

(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
(recentf-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)

(setq-default standard-indent 2)
(setq-default erlang-indent-level 4)
(setq-default js-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default c-indent-level 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default allout-layout t)
; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html
(setq-default auto-save-default nil)
(setq-default auto-save-visited-mode t)
(setq-default auto-save-visited-file-name nil)
(setq scroll-conservatively most-positive-fixnum)
(setq size-indication-mode t)
(setq column-number-mode t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq whitespace-style '(tabs trailing lines tab-mark))
(setq make-backup-files nil)
(setq require-final-newline t)
(setq vc-follow-symlinks t)
(setq default-directory (concat (getenv "HOME") "/"))
(setq show-paren-delay 0)
(setq column-number-mode t)
(setq create-lockfiles nil)
(setq display-line-numbers t)
(setq global-hl-line-mode t)
(setq global-so-long-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-locale-environment "en_US.UTF-8")
(setq default-buffer-file-coding-system 'utf-8)
(setq encoding 'utf-8)

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
(setq magit-show-long-lines-warning nil)

(put 'downcase-region 'disabled nil)

(defun my-cut-function (text &optional rest)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))
    (let ((proc (start-process "tmux" "*Messages*" "tmux" "set-buffer" text))))))

(setq interprogram-cut-function 'my-cut-function)

(setq plantuml-executable-path "/opt/homebrew/bin/plantuml")
(setq plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2023.12/libexec/plantuml.jar")
(setq plantuml-default-exec-mode 'executable)
(setq markdown-command "/opt/homebrew/bin/pandoc")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '())
 '(package-selected-packages
   '()))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
