;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 表示に関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 起動時のメッセージを省略する
(setq inhibit-startup-message t)

;; アラートのベルを消す
(setq ring-bell-function 'ignore)

;; ツールバー非表示
(tool-bar-mode 0)

;; メニューバー非表示
(menu-bar-mode 0)

;; スクロールバーは非表示
(scroll-bar-mode 0)

;; カーソルの点滅オフ
(blink-cursor-mode 0)

;; カーソルを強調表示する
(global-hl-line-mode 1)

;; 対応する括弧を強調表示する
(show-paren-mode 1)
(setq show-paren-style 'mixed)

;; 行番号を表示する
(global-linum-mode 1)
(setq linum-format "%5d")

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; alpha
(if window-system 
    (progn
      (set-frame-parameter nil 'alpha 95)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 操作に関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yes/Noの入力を簡略化する
(fset 'yes-or-no-p 'y-or-n-p)

;; 改行まで含めてカット出来るようにする
(setq kill-whole-line 1)

;; スクロールは1行ごとに
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; スクロールの加速をやめる
(setq mouse-wheel-progressive-speed nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 文字コードに関する設定 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-coding-systems 'utf-8) ; utf-8 にする
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フォントに関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-coding-system 'utf-8)
(set-frame-font "Ricty Diminished-12")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テキスト編集に関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode t) ; リージョンを削除可能に設定
(cua-mode t) ; 矩形選択可能にする
(setq cua-enable-cua-keys nil) ; 矩形選択の特殊なキーバインドを無効にする

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バックアップに関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージ管理に関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; パッケージの保存先の設定とリポジトリの追加

(setq package-user-dir "~/.emacs.d/elisp/elpa/")

(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode rjsx-mode web-mode php-mode flycheck-pos-tip flycheck exec-path-from-shell quelpa-use-package diminish use-package rainbow-delimiters powerline paredit lispxmp atom-one-dark-theme ac-slime))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; use-packageの自動インストール

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package bind-key
  :ensure t)

(use-package diminish
  :ensure t)

;;; quelpa-use-packageのインストール

(use-package quelpa-use-package
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; システムPATHの引き継ぎ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日本語に関する設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(set-fontset-font t 'unicode "Ricty Diminished" nil 'prepend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テーマ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'atom-one-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; モードライン
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'powerline)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git関連設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; git-gutter-fringe
(global-git-gutter-mode 1)

;; magit
(global-set-key (kbd "C-c C-g") 'magit-diff-working-tree)

;; ファイル編集時に，bufferを再読込
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EShell関連設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eshell + shell-pop
(setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell))))
(global-set-key (kbd "C-c o") 'shell-pop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; プログラミング言語用設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 共通設定
;; ============================================================

(require 'cl-lib)

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

;; RarEdit And SLIME REPL
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'text-mode) ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode) ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t) ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)    ;; 曖昧マッチ

;;; Flycheckによる文法チェック

;; 文法チェックを実行する
(add-hook 'after-init-hook #'global-flycheck-mode)

;; 機能を追加する
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

;;; タブ文字の表示幅

;; TABの表示幅。初期値は8
(setq-default tab-width 4)

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; Emacs Lisp
;; ============================================================

;; 式の後ろに値を注釈する
(require 'lispxmp)

;; emacs-lisp-modeでC-c C-dを押すと注釈
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;; Common Lisp
;; ============================================================

(load (expand-file-name "~/.roswell/helper.el"))

;; SLIME
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner))

;; 日本語利用のための設定（Lisp 環境側の対応も必要）
(setq slime-net-coding-system 'utf-8-unix)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Clojure
;; ================================================================

;;; Clojure Mode
(require 'clojure-mode)

;;; Cider
(require 'cider)

;; clojure-mode で cider を有効に
(add-hook 'clojure-mode-hook 'cider-mode)

;; eldoc を有効にする
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;; ac-cider
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

;; C 系共通
;; ================================================================

(defun my-all-cc-mode-init ()
  ;; C 系(cc-mode を継承した)モード共通の設定を記述

  ;; 空白などを一度に削除
  ;; (c-toggle-hungry-state 1)

  ;; 改行時などで自動インデント
  (c-toggle-electric-state 1)
  ;; 
  ;; ";", "}" などを入力したときに自動改行
  ;; 自動インデントも一緒に ON になる
  ;; (c-toggle-auto-newline 1)

  )
(add-hook 'c-mode-common-hook 'my-all-cc-mode-init)

;; C, C++
;; ================================================================

(autoload 'vs-set-c-style "vs-set-c-style"
  "Set the current buffer's c-style to Visual Studio like style. ")

(defun my-c-c++-mode-init ()
  ;; C, C++ 用の設定を記述
  (setq c-basic-offset 4)
  
  ;; Visual Studio 風の設定
  ;; (vs-set-c-style)
  )
(add-hook 'c-mode-hook   'my-c-c++-mode-init)
(add-hook 'c++-mode-hook 'my-c-c++-mode-init)

;; .h でも C++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; PHP
;; ================================================================

(require 'php-mode)

;; Web Mode
;; ================================================================

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
  
  ;; auto tag closing
                                        ;0=no auto-closing
                                        ;1=auto-close with </
                                        ;2=auto-close with > and </
  (setq web-mode-tag-auto-close-style 2))

(add-hook 'web-mode-hook 'web-mode-hook)

;; RJSX Mode
;; ================================================================

(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;インデントはタブではなくスペース
            (setq js-indent-level 2) ;;スペースは２つ、デフォルトは4
            (setq js2-strict-missing-semi-warning nil))) ;;行末のセミコロンの警告はオフ

;; Markdown
;; ================================================================

(require 'markdown-mode)
