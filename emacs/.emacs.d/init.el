(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq package-list '(dockerfile-mode magit markdown-mode plantuml-mode terraform-mode yaml-mode hcl-mode lsp-mode counsel smex flx ivy doom-themes doom-modeline))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(use-package doom-themes
  :config
  (setq doom-ir-black-brighter-comments t)
  (load-theme 'doom-material-dark t))

(use-package doom-modeline
  :ensure t
  :custom
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-total-line-number t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-time nil)
  (setq doom-modeline-env-version nil)
  (doom-modeline-icon (display-graphic-p))
  :init (doom-modeline-mode 1)
  )

(use-package counsel
  :ensure t
  :config
  (use-package smex
    :ensure t)
  (use-package flx
    :ensure t)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  ;; intentional space before end of string
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

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
(hl-line-mode 1)
(global-hl-line-mode 1)

;; indentation
(setq-default
 indent-tabs-mode nil
 tab-width 2
 tab-stop-list (quote (2 4))
 standard-indent 2
 erlang-indent-level 4
 c-indent-level 2
 sh-basic-offset 2
 js-indent-level 2
 typescript-indent-level 2
 )
;; auto save
(setq-default
 auto-save-default nil
 auto-save-visited-mode t
 auto-save-visited-file-name nil
 )
(setq-default allout-layout t)
(setq-default display-line-numbers t)
(setq-default whitespace-style '(tabs trailing lines tab-mark))
(setq scroll-conservatively most-positive-fixnum)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(setq require-final-newline t)
(setq vc-follow-symlinks t)
(setq default-directory (concat (getenv "HOME") "/"))
(setq show-paren-delay 0)
(setq size-indication-mode t)
(setq column-number-mode t)
(setq global-hl-line-mode t)
(setq global-so-long-mode t)
(setq create-lockfiles nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-locale-environment "en_US.UTF-8")
(setq default-buffer-file-coding-system 'utf-8)
(setq-default encoding 'utf-8)

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
   '("da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "a6920ee8b55c441ada9a19a44e9048be3bfb1338d06fc41bce3819ac22e4b5a1" "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1" "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "6e18353d35efc18952c57d3c7ef966cad563dc65a2bba0660b951d990e23fc07" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "e978b5106d203ba61eda3242317feff219f257f6300bd9b952726faf4c5dee7b" "c1d5759fcb18b20fd95357dcd63ff90780283b14023422765d531330a3d3cec2" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"))
 '(package-selected-packages
   '(devdocs flx yaml-mode vue-mode typescript-mode terraform-mode rust-mode plantuml-mode lsp-mode ivy-rich go-mode exec-path-from-shell elixir-mode editorconfig doom-themes doom-modeline dockerfile-mode counsel copilot-chat clojure-mode catppuccin-theme async)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
