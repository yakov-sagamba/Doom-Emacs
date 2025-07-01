;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Org-agenda
;;(after! org
;;  (setq org-agenda-span 30))
(after! org
  (setq org-agenda-files '("/Users/yakov/Documents/03-research/emacs_org/TODO_list.org")))

;; Word count
(setq doom-modeline-enable-word-count t)

;; Org beautifying
(setq org-hide-emphasis-markers t)  ;; cache les marques pour les italiques, gras, etc
(use-package org-bullets            ;; change les * pour des points de différentes formes pour les titres
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Keybinding for heading like Obsidian
;; Insert fixed-level org headings with Command + [1-6], ignoring current heading level
(defun my/org-force-insert-heading-at-level (level)
  "Insert a new heading at a fixed LEVEL, ignoring current org context."
  (interactive "nLevel: ")
  (end-of-line)
  (newline)
  (insert (make-string level ?*) " ")
  (org-align-all-tags))

(after! org
  (map! :map org-mode-map
        "s-1" (lambda () (interactive) (my/org-force-insert-heading-at-level 1))
        "s-2" (lambda () (interactive) (my/org-force-insert-heading-at-level 2))
        "s-3" (lambda () (interactive) (my/org-force-insert-heading-at-level 3))
        "s-4" (lambda () (interactive) (my/org-force-insert-heading-at-level 4))
        "s-5" (lambda () (interactive) (my/org-force-insert-heading-at-level 5))
        "s-6" (lambda () (interactive) (my/org-force-insert-heading-at-level 6))))

;; Italic and bold
(defun my/org-toggle-markup (markup)
  "Toggle ORG emphasis MARKUP (*, /, etc.) around region or word."
  (let* ((markup-len (length markup))
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (beg (copy-marker (car bounds)))
         (end (copy-marker (cdr bounds) t))) ;; marker moves with text
    ;; Check for existing markup around the region
    (let ((before (buffer-substring-no-properties
                   (max (point-min) (- beg markup-len)) beg))
          (after (buffer-substring-no-properties
                  end (min (point-max) (+ end markup-len)))))
      (if (and (string= before markup) (string= after markup))
          ;; If already marked up, remove it
          (progn
            (delete-region (- beg markup-len) beg)
            (delete-region end (+ end markup-len)))
        ;; Else, add markup
        (save-excursion
          (goto-char end)
          (insert markup)
          (goto-char beg)
          (insert markup))))))

(defun my/org-toggle-bold ()
  "Toggle *bold* formatting on region or word."
  (interactive)
  (my/org-toggle-markup "*"))

(defun my/org-toggle-italic ()
  "Toggle /italic/ formatting on region or word."
  (interactive)
  (my/org-toggle-markup "/"))

;; Keybindings in Org mode: ⌘B and ⌘I
(map! :map org-mode-map
      :i "s-b" #'my/org-toggle-bold
      :i "s-i" #'my/org-toggle-italic
      :n "s-b" #'my/org-toggle-bold
      :n "s-i" #'my/org-toggle-italic)

;; Pixel scrolling
(pixel-scroll-precision-mode t)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-nord)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
