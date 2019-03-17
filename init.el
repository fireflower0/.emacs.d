;; パッケージ
(require 'package)

;; http://stackoverflow.com/a/26110978
(setq package-check-signature nil)

(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-disabled-checkers (quote (javascript-jshint javascript-jscs)))
 '(package-selected-packages
   (quote
    (nodejs-repl add-node-modules-path scss-mode flycheck ensime scala-mode markdown-mode web-mode git-gutter magit neotree powerline paredit rainbow-delimiters tabbar elscreen rjsx-mode ac-slime slime mozc))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 一般設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 起動時のメッセージを省略する
(setq inhibit-startup-message t)

;; ツールバー非表示
(tool-bar-mode 0)

;; メニューバー非表示
(menu-bar-mode 0)

;; スクロールバーは非表示
(scroll-bar-mode 0)

;; カーソルの点滅オフ
(blink-cursor-mode 0)

;; カーソルを強調表示する
;; (global-hl-line-mode 1)

;; 対応する括弧を強調表示する
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; アラートのベルを消す
(setq ring-bell-function 'ignore)

;; Yes/Noの入力を簡略化する
(fset 'yes-or-no-p 'y-or-n-p)

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; 文字コードに関する設定 
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; フォントに関する設定
(set-frame-font "Ricty Diminished-12")
(add-to-list 'default-frame-alist '(font . "Ricty Diminished-12"))

;; テーマ
(load-theme 'tango-dark t)

;; 行番号を表示する
(global-linum-mode 1)

;; タイトルにフルパス表示
(setq frame-title-format "%f")

;;current directory 表示
(let ((ls (member 'mode-line-buffer-identification
                  mode-line-format)))
  (setcdr ls
          (cons '(:eval (concat " ("
                                (abbreviate-file-name default-directory)
                                ")"))
                (cdr ls))))

;; TABの表示幅。初期値は8
(setq-default tab-width 4)

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; 反対側のウィンドウにいけるように
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;; Buffer操作 (icomplete-mode)
(icomplete-mode 1)

;; 日本語に関する設定
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(set-fontset-font t 'unicode "Ricty Diminished" nil 'prepend)

;; 括弧に色をつける
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)

;; 括弧の色を強調する設定
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)

;;; Mode Line
(require 'powerline)
(powerline-default-theme)

;;; neotree（サイドバー）
(require 'neotree)
(global-set-key "\C-o" 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 検索性の向上
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 大文字・小文字を区別しない
(setq case-fold-search t)

;; ファイル名検索
(define-key global-map [(super i)] 'find-name-dired)

;; ファイル内検索（いらないメッセージは消去）
(define-key global-map [(super f)] 'rgrep)

;; rgrepのheader messageを消去
(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defadvice grep (after delete-grep-header activate) (delete-grep-header))
(defadvice rgrep (after delete-grep-header activate) (delete-grep-header))

;; rgrep時などに，新規にwindowを立ち上げる
(setq special-display-buffer-names '("*Help*" "*compilation*" "*interpretation*" "*grep*" ))

;; "grepバッファに切り替える"
(defun my-switch-grep-buffer()
  (interactive)
    (if (get-buffer "*grep*")
            (pop-to-buffer "*grep*")
      (message "No grep buffer")))
(global-set-key (kbd "s-e") 'my-switch-grep-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-gutter-fringe
(global-git-gutter-mode 1)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; ファイル編集時に，bufferを再読込
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto Complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode) ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode) ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t) ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)    ;; 曖昧マッチ

;;; FlyCheck
(require 'flycheck)

(eval-after-load 'flycheck
  '(custom-set-variables
    ;; 他の文法チェッカーが入ってると優先されるので除外
    '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (expand-file-name "~/.roswell/helper.el"))

;; SLIME
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-startup-animation nil)

;; カーソル付近にある単語の情報を表示
(slime-autodoc-mode)

;; 日本語利用のための設定（Lisp 環境側の対応も必要）
(setq slime-net-coding-system 'utf-8-unix)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Qlot
(defun slime-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH="
				  (mapconcat 'identity exec-path ":")))))

;; Lispの括弧の対応に沿った編集モード
(require 'paredit)

(autoload 'enable-paredit-mode
  "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; ParEdit And SLIME REPL
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsp$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))

(defun web-mode-hook ()
  ;; indent
  (setq web-mode-html-offset   2)
  (setq web-mode-style-padding 2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2)
  (local-set-key (kbd "C-m") 'newline-and-indent)
  ;;; auto tag closing
  ;; 0=no auto-closing
  ;; 1=auto-close with </
  ;; 2=auto-close with > and </
  (setq web-mode-tag-auto-close-style 2))

(add-hook 'web-mode-hook 'web-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nodejs REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'nodejs-repl)
(defun nvm-which ()
  (let* ((shell (concat (getenv "SHELL") " -l -c 'nvm which'"))
         (output (shell-command-to-string shell)))
    (cadr (split-string output "[\n]+" t))))
(setq nodejs-repl-command #'nvm-which)

(add-hook 'web-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RJSX Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;インデントはタブではなくスペース
            (setq js-indent-level 2) ;;スペースは２つ、デフォルトは4
            (setq js2-strict-missing-semi-warning nil))) ;;行末のセミコロンの警告はオフ

(require 'scss-mode)
(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markdown Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Do not change font in code block
(set-face-attribute 'markdown-code-face nil :inherit 'default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scala Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'scala-mode)
(require 'ensime)
