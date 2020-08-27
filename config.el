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

;; CONFIGURATION (setting variables)
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")
;;;; UI
(setq doom-theme 'doom-vibrant)
;;;;;; MODELINE
(custom-set-faces! ;; Make modeline orange after editing
  '(doom-modeline-buffer-modified :foreground "orange"))
(display-time-mode 1)                  ; Enable time in the mode-line

(setq-default display-line-numbers-type 'relative)

(after! org
  (setq org-directory "~/org/"
        org-startup-indented t
        )
  )

(add-hook 'org-mode-hook
          (lambda ()
           (setq-local display-line-numbers 'visual)))
(setq which-key-idle-delay 0.3)

(setq-default
 delete-by-moving-to-trash t
 tab-width 2
 uniquify-buffer-name-style 'forward   ; Uniquify buffer names
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
(global-subword-mode 1)                ; Treat camel case as words for evil motions
;; enable word-wrap (almost) everywhere
(+global-word-wrap-mode +1)
(setq scroll-margin 10)


;; (+workspace/switch-to)
;; (emacsql-sqlite-ensure-binary)
;;; Ivy
(setq ivy-use-selectable-prompt t)
;; Spacemacs
(setq ivy-re-builders-alist
 '((counsel-rg . ivy--regex-plus)
 (swiper . ivy--regex-plus)
 (swiper-isearch . ivy--regex-plus)
 (t . ivy--regex-ignore-order)))

;; PROGRAMMING
;;;; LSP
;; Use Microsoft's python langserver
(after! lsp-python-ms
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  (set-lsp-priority! 'mspyls 1))

(setq lsp-ui-doc-enable nil)  ; Disable docs on hover
(setq lsp-ui-peek-enable t)
(after! flycheck ;; Real-time syntacx checking
  (setq flycheck-check-syntax-automatically '(save idle-change ;; new-line
                                                   mode-enabled))
  ;; (require 'flyspell-lazy) (flyspell-lazy-mode 1) ; Performance boost
  )
;; (after! nix-mode)
;; FUNCTIONS
;; Ask which buffer to open after splitting window
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/projectile-find-file))
(setq +ivy-buffer-preview t)

;; KEYBINDINGS
;; `define-key'
;; `global-set-key'
;; `map!'
;; `undefine-key!'
;; `define-key!'

(map!
 :leader (:prefix "p" :desc "Save project files"
          "S" #'projectile-save-project-buffers)
 (:prefix "p" :desc "Search project" "s"
  #'+default/search-project)
 )
(map!
 :leader
 (:prefix ("r" . "rings")
  :desc "Yank from kill ring" "y"
  #'counsel-yank-pop))
(map!
 :leader
 (:prefix "f"
  :desc "Find file in my-conf" "N"
  #'(lambda () (interactive) (doom-project-browse "/home/ao/my-conf/"))))
(map! :leader (:desc "M-x" "SPC" #'counsel-M-x))

;; Navigation
(defun which-key-workspace-names (_ names &rest __)
  (if (> 0 (length names))
      (dotimes (i 9)
        (which-key-add-key-based-replacements
          (format "SPC TAB %d" i)
          (format "Switch to %dth workspace" i)))
    (which-key-add-key-based-replacements "SPC TAB 0"
      "Switch to last workspace")
    (dotimes (i (length names))
      (which-key-add-key-based-replacements
        (format "SPC TAB %d" (+ 1 i))
        (format "Switch to %s" (nth i names))))
    )
  )
;;(which-key-workspace-names (gensym) (+workspace-list-names))
;;(add-variable-watcher 'persp-names-cache #'which-key-workspace-names)
;; The following code conflicts with the preceeding
;; (setq which-key-allow-multiple-replacements t)
;; (after! which-key
;;   (pushnew!
;;    which-key-replacement-alist
;;    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
;;    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
;;    ))

(winum-mode)
;; https://xkcd.com/1319/
(defmacro winum-map- ()
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

(winum-map-)
