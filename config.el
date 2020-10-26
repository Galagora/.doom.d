;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment macro
;; - Similar to Clojure's. Lets you wrap any elisp code without eval'ing it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro comment (&rest _)
  `nil)

;; CONFIGURATION (setting variables)
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")
;;;; UI
(setq doom-theme 'doom-vibrant)
;;;;;; MODELINE
(custom-set-faces! ;; Make modeline orange after editing
  '(doom-modeline-buffer-modified :foreground "orange"))
;; (display-time-mode 1)                  ; Enable time in the mode-line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some errors are TypeScript-only. Let's mark them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! hl-todo
  (setq hl-todo-keyword-faces
        `(
        ;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold)
          ("TS" warning bold)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 tab-always-indent          t
 create-lockfiles           nil
 uniquify-buffer-name-style 'post-forward-angle-brackets
 +ivy-buffer-preview        t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq version-control t     ;; Use version numbers for backups.
;;       kept-new-versions 10  ;; Number of newest versions to keep.
;;       kept-old-versions 0   ;; Number of oldest versions to keep.
;;       delete-old-versions t ;; Don't ask to delete excess backup versions.
;;       backup-by-copying t   ;; Copy all files, don't rename them.
;;       make-backup-files t
;;       )

;; (setq vc-make-backup-files t)

;; ;; Default and per-save backups go here:
;; (setq backup-directory-alist '(("" . "~/.cache/emacs/per-save")))

;; (defun force-backup-of-buffer ()
;;   ;; Make a special "per session" backup at the first save of each
;;   ;; emacs session.
;;   (when (not buffer-backed-up)
;;     ;; Override the default parameters for per-session backups.
;;     (let ((backup-directory-alist '(("" . "~/.cache/emacs/per-session")))
;;           (kept-new-versions 3))
;;       (backup-buffer)))
;;   ;; Make a "per save" backup on each save.  The first save results in
;;   ;; both a per-session and a per-save backup, to keep the numbering
;;   ;; of per-save backups consistent.
;;   (let ((buffer-backed-up nil))
;;     (backup-buffer)))

;; (add-hook 'before-save-hook  'force-backup-of-buffer)


(setq-default display-line-numbers-type 'relative)
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))
(after! org
  (setq org-directory "~/notes/"
        org-startup-indented t
        )
  )

(global-display-line-numbers-mode t)
;; (visual-line-mode nil)

(after! magit
  (setq magit-section-disable-line-numbers nil))
(add-hook! 'text-mode-hook :local #'turn-on-visual-line-mode
  (lambda () (setq display-line-numbers 'visual)))

(setq which-key-idle-delay 0.5)
(add-hook! kill-emacs #'doom/quicksave-session)


(setq-default
 delete-by-moving-to-trash t
 tab-width 2
 window-combination-resize t           ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                   ; Stretch cursor to the glyph width

;;;; Saving, caches and undoing
(setq undo-limit 80000000              ; Raise undo-limit to 80Mb
      evil-want-fine-undo t            ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t              ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t ; When there are lots of glyphs, keep them in memory
      ;; truncate-string-ellipsis "…"
      )    ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)              ; Replace selection when inserting text
(global-subword-mode -1)                ; Treat camel case as words for evil motions
;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)
(setq scroll-margin 10)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clipboard behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq save-interprogram-paste-before-kill t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hydra Paste
;; - Create a hydra similar to paste-transient-state to allow me to cycle the
;;   kill ring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhydra hydra-paste
  (:color red
    :hint nil)
  "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
  ("C-j" evil-paste-pop)
  ("C-k" evil-paste-pop-next)
  ("p"   evil-paste-after)
  ("P"   evil-paste-before))

(map!
  :after evil
  :nv [remap evil-paste-after] #'hydra-paste/evil-paste-after
  :nv [remap evil-paste-before] #'hydra-paste/evil-paste-before)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ivy-use-selectable-prompt t)
;; Spacemacs
;; (setq ivy-re-builders-alist
;;  '((counsel-rg . ivy--regex-plus)
;;  (swiper . ivy--regex-plus)
;;  (swiper-isearch . ivy--regex-plus)
;;  (t . ivy--regex-ignore-order)))

;; Ask which buffer to open after splitting window
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/projectile-find-file))
(setq +ivy-buffer-preview t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil M-x customize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spacemacs/custom-newline (pos)
  "Make `RET' in a Custom-mode search box trigger that field's action, rather
than enter an actual newline, which is useless and unexpected in a search box.
If not in such a search box, fall back on `Custom-newline'."
  (interactive "d")
  (let ((w (widget-at)))
    (if (and w
             (eq 'editable-field (widget-type w))
             (string-prefix-p "Search" (widget-get w :help-echo)))
        (funcall (widget-get w :action) w)
        (Custom-newline pos))))

(defun spacemacs-defaults/init-cus-edit ()
  ;; Arguably a Vim user's first expectation for such a buffer would be a kind
  ;; of normal mode; besides, `evilified' conflicts with text insertion for
  ;; search.
  (evil-set-initial-state 'Custom-mode 'normal)
  ;; Notes on how this effects the default `custom-mode-map':
  ;; - `TAB' works as `widget-forward' without modification
  ;; - `<S-tab>' works as `widget-backward' without modification
  ;; - `n' as `widget-forward' is redundant with `TAB' and collides with the
  ;; - `evil-ex-search-next' mapping which is useful here. Omitting
  ;;   intensionally.
  ;; - `p' doesn't make any sense without `n' and is redundant with `<S-tab>'.
  ;;   Omitting intensionally.
  ;; - `q' as `Custom-buffer-done' conflicts with the Evil record macro
  ;;   binding, which is, however, of questionable value in a Custom buffer;
  ;;   and there is precedent in many other Spacemacs contexts to bind it to
  ;;   quit actions rather than default evil one; choosing to restore.
  ;; - `SPC' as `scroll-up-command' conflicts with the all-important Spacemacs
  ;;   menu. Omitting intensionally. Evil `C-u' works instead.
  ;; - `S-SPC' as `scroll-down-command' makes no sense without `SPC' as
  ;;   `scroll-up-command'. Evil `C-u' works instead.
  ;; - `C-x' as a prefix command still works.
  ;; - `C-c' as a prefix command still works.
  ;; - `u' as `Custom-goto-parent' conflicts with Evil undo. However it is
  ;;   questionable whether this will work properly in a Custom buffer;
  ;;   choosing to restore this binding.
  (evil-define-key 'normal cus-edit-mode-map (kbd "q") 'Custom-buffer-done)
  (evil-define-key 'normal cus-edit-mode-map (kbd "u") 'Custom-goto-parent)
  ;; `RET' does not work well in the search field. Fix:
  (evil-define-key 'insert cus-edit-mode-map (kbd "RET") 'spacemacs/custom-newline)
  (evil-define-key 'normal cus-edit-mode-map (kbd "RET") 'spacemacs/custom-newline))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq lsp-ui-doc-enable nil)  ; Disable docs on hover
(setq lsp-ui-peek-enable t)

(after! flycheck ;; Real-time syntacx checking
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  ;; (require 'flyspell-lazy) (flyspell-lazy-mode 1) ; Performance boost
  (setq flycheck-navigation-minimum-level 'error)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (after! lsp-python-ms ;; Use Microsoft's python langserver
;;   (setq lsp-python-ms-executable (executable-find "python-language-server"))
;;   (set-lsp-priority! 'mspyls 1))

;; (eval-after-load 'lsp-clients
;;   '(progn
;;      (lsp-dependency 'pyright (:system ,(executable-find "poetry")))
;;      ))

(after! lsp-pyright
  (setq
   ;; lsp-pyright-langserver-command-args "run pyright-langserver --stdio"
   lsp-pyright-python-executable-cmd "poetry run python"
   lsp-pyright-venv-path ".venv/"
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-haskell
 :config
 ;; (setq lsp-haskell-server-path "haskell-language-server-wrapper")
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 (setq lsp-log-io nil)
 )

 (setq haskell-process-type 'stack-ghci)
 (setq haskell-interactive-popup-errors nil)
(setq haskell-hoogle-command "stack hoogle --")

;; (use-package! lsp-mode
;;   :config
;;   (when (featurep! :config default +bindings)
;;     (dolist (leader-key (list doom-leader-key doom-leader-alt-key))
;;       (let ((lsp-keymap-prefix (concat leader-key " c l")))
;;         (lsp-enable-which-key-integration)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cycling flycheck
;; Optional: ensure flycheck cycles, both when going backward and forward.
;; Tries to handle arguments correctly.
;; Since flycheck-previous-error is written in terms of flycheck-next-error,
;; advising the latter is enough.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flycheck-next-error-loop-advice (orig-fun &optional n reset)
  ; (message "flycheck-next-error called with args %S %S" n reset)
  (condition-case err
      (apply orig-fun (list n reset))
    ((user-error)
     (let ((error-count (length flycheck-current-errors)))
       (if (and
            (> error-count 0)                   ; There are errors so we can cycle.
            (equal (error-message-string err) "No more Flycheck errors"))
           ;; We need to cycle.
           (let* ((req-n (if (numberp n) n 1)) ; Requested displacement.
                  ; An universal argument is taken as reset, so shouldn't fail.
                  (curr-pos (if (> req-n 0) (- error-count 1) 0)) ; 0-indexed.
                  (next-pos (mod (+ curr-pos req-n) error-count))) ; next-pos must be 1-indexed
             ; (message "error-count %S; req-n %S; curr-pos %S; next-pos %S" error-count req-n curr-pos next-pos)
             ; orig-fun is flycheck-next-error (but without advise)
             ; Argument to flycheck-next-error must be 1-based.
             (apply orig-fun (list (+ 1 next-pos) 'reset)))
         (signal (car err) (cdr err)))))))

(advice-add 'flycheck-next-error :around #'flycheck-next-error-loop-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS
;; Useful keybinding macros
;; `define-key'
;; `global-set-key'
;; `map!'
;; `undefine-key!'
;; `define-key!'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :m "]e" #'flycheck-next-error
 :m "[e" #'flycheck-previous-error
 :m "U" #'evil-avy-goto-char-timer
 ;; :nv "j" #'evilem-motion-next-line
 ;; :nv "k" #'evilem-motion-previous-line
 :leader
 (:desc "Find file in project" "SPC" #'+ivy/projectile-find-file)
 (:desc "Eval expression" ":" #'pp-eval-expression)
 (:desc "M-x" ";" #'counsel-M-x)

 (:desc "window" "W" evil-window-map)
 ;; Toggles
 ;; (:prefix "t")
 (:prefix "p"
  (:desc "Save project files" "S" #'projectile-save-project-buffers
   :desc "Search project" "s" #'+default/search-project
   :desc "Edit project .dir-locals" "E" #'projectile-edit-dir-locals
   :desc "Reload .envrc" "e" #'envrc-reload))

 (:prefix ("r" . "rings") :desc "Yank from kill ring" "y" #'counsel-yank-pop)

 (:prefix "f"
  :desc "Find file in my-conf" "N"
  #'(lambda () (interactive) (doom-project-browse "/home/ao/my-conf/")))
 ;; (:leader :desc "Lisp" "k" evil-lisp-state-map)
 )

(advice-remove 'evil-open-below #'+evil--insert-newline-below-and-respect-comments-a)
(advice-remove 'evil-open-above #'+evil--insert-newline-above-and-respect-comments-a)

;; (evil-exchange-cx-install)
;; (map! :v
;;       "s")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Lisp State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-comment ()
  "Wrap sexp in (comment ...) and indent it"
  (interactive)
  (let ((sexp (save-excursion
                (sexp-at-point))))
    (if (or (eq sexp 'comment)
            (eq (car sexp) 'comment))
      (unwrap-comment)
      (sp-wrap-with-pair "(")
      (insert "comment\n")
      (indent-for-tab-command)
      (evil-first-non-blank))))

(defun lisp-state-end-of-sexp (&optional arg)
  "Go to the end of current s-exp"
  (interactive "P")
  (forward-char)
  (sp-end-of-sexp))

(use-package! evil-lisp-state
  :init
  (setq evil-lisp-state-global t)
  :config
  ;; Default to evil bindings for unbound keys
  (set-keymap-parent
   evil-lisp-state-map
   (make-composed-keymap evil-normal-state-map evil-motion-state-map))
  (map! :map evil-lisp-state-map
        ";" (evil-lisp-state-enter-command wrap-comment)
        "<SPC>" doom-leader-map
        "u" #'undo-fu-only-undo
        "<ESC>" #'lisp-state-toggle-lisp-state
        "^" #'lisp-state-beginning-of-sexp
        "$" #'lisp-state-end-of-sexp
        ;; "i" #'evil-insert
        ;; "K" #'+lookup/documentation
        ;; "i" (call-interactively evil-insert-state )
        )
  (map! :leader :desc "Enter Lisp state" "k" #'lisp-state-toggle-lisp-state))
;;(define-key evil-insert-state-map [escape]
;;            (lambda () (interactive) (message "hello")))
(defun esc-to-evlisp/evil () (interactive)
 (if (bound-and-true-p modified-vinsert-mode)
     (evil-lisp-state)
   (evil-force-normal-state)))
(define-key evil-insert-state-map [escape] #'esc-to-evlisp/evil)
;; (map! :i "<ESC>" #'esc-to-evlisp/evil)
;; ;; (defvar modified-vinsert-mode-map (make-sparse-keymap)
;; ;;   "High precedence keymap.")

(define-minor-mode modified-vinsert-mode
  "Minor mode where escape goes back to `evil-lisp-state'"
  )

;; ;; (modified-vinsert-mode)

;; ;; (let ((state 'insert))
;; ;;   (evil-make-intercept-map
;; ;;    (evil-get-auxiliary-keymap modified-vinsert-mode-map state t t)
;; ;;    state))

;; ;; (evil-define-key 'insert modified-vinsert-mode-map
;; ;;   ;; (kbd "SPC v") 'find-file
;; ;;   (kbd "<ESC>") #'evil-lisp-state
;; ;;   )

;; (around)
;; (add-hook 'evil-insert-state-entry-hook (lambda () (print (if (eq evil-state 'insert) "Insert!"))))
(add-hook 'evil-lisp-state-entry-hook #'modified-vinsert-mode)
(add-hook 'evil-normal-state-entry-hook    (lambda () (modified-vinsert-mode -1)))
(add-hook 'evil-visual-state-entry-hook   (lambda () (modified-vinsert-mode -1)))
(add-hook 'evil-replace-state-entry-hook  (lambda () (modified-vinsert-mode -1)))
(add-hook 'evil-operator-state-entry-hook (lambda () (modified-vinsert-mode -1)))
(add-hook 'evil-treemacs-state-entry-hook (lambda () (modified-vinsert-mode -1)))
(add-hook 'evil-emacs-state-entry-hook    (lambda () (modified-vinsert-mode -1)))
(add-hook 'evil-replace-state-entry-hook  (lambda () (modified-vinsert-mode -1)))
(add-hook 'evil-motion-state-entry-hook   (lambda () (modified-vinsert-mode -1)))
;; (add-hook 'evil-lisp-state-exit-hook (lambda () (modified-vinsert-mode -1)))
;; (add-hook 'text-mode-hook (lambda () (foo-mode -1)))
(defun j/evil-state-fg (state)
  (let ((sym (intern (concat "doom-modeline-evil-" state "-state"))))
    (face-foreground sym nil t)))

(add-hook! 'doom-load-theme-hook
    (defun j/theme-evil-cursors ()
      (setq
       evil-insert-state-cursor   (list 'bar (j/evil-state-fg "insert"))
       ;; evil-normal-state-cursor   (list 'box (j/evil-state-fg "normal"))
       ;; evil-visual-state-cursor   (list 'box (j/evil-state-fg "visual"))
       ;; evil-operator-state-cursor (list 'box (j/evil-state-fg "operator"))
       evil-lisp-state-cursor     (list 'box (j/evil-state-fg "emacs"))
       ;; evil-vterm-state-cursor    (list 'box (face-foreground 'error nil t))
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (after! evil-collection
;;   (setq
;;    evil-collection-setup-minibuffer t
;;    ))

(map! :g "C-<escape>" (cmd! (progn (evil-normal-state)
                                   (evil-snipe-local-mode +1)
                                   (evil-snipe-override-local-mode +1)))
      :i "C-<escape>" #'evil-force-normal-state
      )
;; (setq evil-want-minibuffer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspace names in which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun g/which-key-workspace-names (_ names &rest __)
  (dotimes (i 9)
    (which-key-add-key-based-replacements
      (format "SPC TAB %d" i)
      (format "Switch to %dth workspace" i)))
  (which-key-add-key-based-replacements "SPC TAB 0"
    "Switch to last workspace")
  (dotimes (i (length names))
    (which-key-add-key-based-replacements
      (format "SPC TAB %d" (+ 1 i))
      (format "Switch to %s" (nth i names)))))
(g/which-key-workspace-names (gensym) (+workspace-list-names))
(add-variable-watcher 'persp-names-cache #'g/which-key-workspace-names)

;; The following code conflicts with the preceeding
;; (setq which-key-allow-multiple-replacements t)
;; (after! which-key
;;   (pushnew!
;;    which-key-replacement-alist
;;    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
;;    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
;;    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window numbers (+ which-key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(winum-mode)
;; https://xkcd.com/1319/
(defmacro g/winum-map ()
  `(map!
    :leader
    ,(let ((value '()))
       (dotimes (i 9 value)
         (setq i (+ i 1))
         (setq value (cons (list :desc (format "select-window-%d" i)
                                 (number-to-string i)
                                 (intern
                                  (format "winum-select-window-%d" i))
                                 )
                           value)))
       (seq-map
        #'(lambda (cell)
            (append (seq-take cell (- (length cell) 1))
                    (list (read (format "#'%s" (symbol-name (car (last cell))))))
                    ))
        (seq-reverse value))
       )
    )
  )

(g/winum-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! circe
  (set-irc-server! "chat.freenode.net"
    `(:tls t
      :port 6697
      :nick "galagora"
      :sasl-username "galagora"
      :sasl-password "4XC5hWayA9"
      )))

;; (smartparens-global-mode -1)
;; (turn-off-smartparens-mode)


(map!
 :v "s" 'evil-surround-region
 :v "S" 'evil-Surround-region
 :ov "x" 'evil-snipe-x
 :ov "X" 'evil-snipe-X
 :ov "z" 'evil-snipe-s
 :ov "Z" 'evil-snipe-S
 )
(setq evil-snipe-repeat-keys nil)

(setq evil-ex-substitute-global t)
   ;; do :thing
;; (after! js2-mode
;;   (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `easy-customize'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(evil-collection-custom-setup)

(defun sp-End-of-sexp () (interactive)
  (progn (call-interactively #'sp-end-of-sexp) (forward-char)))
(map!
 :i "C-]" #'sp-End-of-sexp
 ;; :i "C-[" #'backward-char
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 cider-shadow-cljs-command "shadow-cljs"
 ;; cider-shadow-cljs-global-options
 ;; cider-shadow-cljs-parameters "cljs-repl"
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! ivy-avy
  :after ivy)

;; (map! :g )

;; (defun call-aw-help (&rest) (call-interactively #'aw-show-dispatch-help))
;; (advice-add #'ace-window :before #'call-aw-help)
;; (advice-remove #'ace-window #'call-aw-help)

(map!
 :leader
 (:desc "ace-window" "w" #'ace-window)
 )

;; (use-package!
;;     :init
;;   (defvar aw-dispatch-alist
;;     '((?x aw-delete-window "Delete Window")
;;       (?m aw-swap-window "Swap Windows")
;;       (?M aw-move-window "Move Window")
;;       (?y aw-copy-window "Copy Window")
;;       (?j aw-switch-buffer-in-window "Select Buffer")
;;       (?n aw-flip-window)
;;       (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
;;       (?e aw-execute-command-other-window "Execute Command Other Window")
;;       (?F aw-split-window-fair "Split Fair Window")
;;       (?v aw-split-window-vert "Split Vert Window")
;;       (?b aw-split-window-horz "Split Horz Window")
;;       (?D delete-other-windows "Delete Other Windows")
;;       (?T aw-transpose-frame "Transpose Frame")
;;       ;; ?i ?r ?t are used by hyperbole.el
;;       (?? aw-show-dispatch-help)))
;; )
;; (add-to-list 'aw-dispatch-alist )

;; (defadvice! projectile-add-and-switch (&rest _)
;;   :after '(projectile-add-known-project)
;;   (counsel-projectile-switch-project))

;; (advice-remove #'projectile-add-known-project #'projectile-add-and-switch)

;; (map! :localleader)
;; (map! :after json-mode
;;         :map json-mode-map
;;         :localleader
;;         :desc "Copy path" "p" #'json-mode-show-path
;;         "t" #'json-toggle-boolean
;;         "d" #'json-mode-kill-path
;;         "x" #'json-nullify-sexp
;;         "+" #'json-increment-number-at-point
;;         "-" #'json-decrement-number-at-point
;;         "f" #'json-mode-beautify)

(defun g/goto-nix-conf ()
  (interactive)
  (find-file (expand-file-name "configuration.nix" "~/my-conf/"))
  )

(map!
 :leader :prefix "o"
 "f" nil
 (:prefix ("f" . "Open files")
  ("n" #'g/goto-nix-conf
   "d" #'doom/goto-private-config-file)
  )
 "F" #'make-frame
 )

(setq standard-indent 2)
;; (define-minor-mode g/evil-snipe-local-mode
;;   "evil-snipe minor mode."
;;   :lighter " snipe"
;;   :group 'evil-snipe)
;; (defun g/turn-on-evil-snipe-mode ()
;;   "Enable evil-snipe-mode in the current buffer."
;;   (unless (apply #'derived-mode-p evil-snipe-disabled-modes)
;;     (evil-snipe-local-mode +1)))

;; ;;;###autoload
;; (defun g/turn-on-evil-snipe-override-mode ()
;;   "Enable evil-snipe-mode in the current buffer."
;;   (unless (apply #'derived-mode-p evil-snipe-disabled-modes)
;;     (evil-snipe-override-local-mode +1)))

;; (define-minor-mode g/evil-snipe-override-local-mode
;;   "evil-snipe minor mode that overrides evil-mode f/F/t/T/;/, bindings."
;;   :group 'evil-snipe)

;; (define-globalized-minor-mode g/evil-snipe-mode
;;   evil-snipe-local-mode turn-on-evil-snipe-mode)

;; (define-globalized-minor-mode g/evil-snipe-override-mode
;;   evil-snipe-override-local-mode turn-on-evil-snipe-override-mode)

;; (turn-off-evil-snipe-mode)
;; (turn-off-evil-snipe-override-mode)
;; ;; (g/turn-on-evil-snipe-mode)
;; ;; (g/turn-on-evil-snipe-override-mode)
