;;
;; AlbOrgMode/alb-org-mode-start.el
;;
;;     Copyright (C) 2010-2014 Andrew Lincoln Burrow
;;
;;     This library is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This library is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this library; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;;   - Site start code for basic albcorp Org-Mode setup
;;

;;
;;
;; AMMENDED LOAD PATH
;; ---------------------------------------------------------------------
;;

(setq load-path
      (cons (directory-file-name (file-name-directory load-file-name))
            load-path))

;;
;;
;; REQUIRED FEATURES
;; ---------------------------------------------------------------------
;;

;;
;;
;; IMMEDIATE CONFIGURATION
;; ---------------------------------------------------------------------
;;

;;
;; Auto Modes

(setq auto-mode-alist
      (cons '("\\.org$" . org-mode) auto-mode-alist))

;;
;; Batch Functions

(defun alb-org-publish ()
  "Batch publish Org-Mode files and select agendas

This is designed to be called from the shell as follows.

    emacs \
        --batch \
        --load=${HOME}/.emacs \
        --funcall=alb-org-publish \
        --funcall=kill-emacs
"
  (require 'org-publish)
  (org-publish "default")
  (org-store-agenda-views))

;;
;;
;; DEFERRED CONFIGURATION
;; ---------------------------------------------------------------------
;;

(eval-after-load 'org
 '(progn
    ;; Require org-capture templates
    (require 'org-capture)

    ;; Require functions to customise behavior
    (require 'alb-org-behavior)

    ;; Set up hooks for clock persistence
    (org-clock-persistence-insinuate)))

(add-hook
 'org-mode-hook
 '(lambda ()
    ;; Fix the formatting on write
    (alb-add-org-whitespace-cleanup)

    ;; Remove existing OrgMode key-bindings
    (define-key org-mode-map "\C-c\C-x" (make-sparse-keymap))
    (define-key org-mode-map "\C-i" nil)
    (define-key org-mode-map [(tab)] nil)
    (define-key org-mode-map [(control tab)] nil)
    (define-key org-mode-map [(meta tab)] nil)
    (define-key org-mode-map "\M-\t" nil)
    (define-key org-mode-map "\M-\C-i" nil)
    (define-key org-mode-map [(shift tab)] nil)
    (define-key org-mode-map [backtab] nil)
    (define-key org-mode-map [(shift return)] nil)
    (define-key org-mode-map [(meta shift return)] nil)
    (define-key org-mode-map [(meta return)] nil)
    (define-key org-mode-map [(meta left)] nil)
    (define-key org-mode-map [(meta right)] nil)
    (define-key org-mode-map [(meta up)] nil)
    (define-key org-mode-map [(meta down)] nil)
    (define-key org-mode-map [(meta shift left)] nil)
    (define-key org-mode-map [(meta shift right)] nil)
    (define-key org-mode-map [(meta shift up)] nil)
    (define-key org-mode-map [(meta shift down)] nil)
    (define-key org-mode-map [(shift up)] nil)
    (define-key org-mode-map [(shift down)] nil)
    (define-key org-mode-map [(shift left)] nil)
    (define-key org-mode-map [(shift right)] nil)
    (define-key org-mode-map [(control shift right)] nil)
    (define-key org-mode-map [(control shift left)] nil)
    (define-key org-mode-map org-babel-key-prefix nil)
    (define-key org-mode-map "\C-c\C-a" nil)
    (define-key org-mode-map "\C-c\C-r" nil)
    (define-key narrow-map "s" nil)
    (define-key org-mode-map "\C-c\C-f" nil)
    (define-key org-mode-map "\C-c\C-b" nil)
    (define-key org-mode-map "\C-c$" nil)
    (define-key org-mode-map "\C-c\C-j" nil)
    (define-key org-mode-map "\C-c\C-t" nil)
    (define-key org-mode-map "\C-c\C-q" nil)
    (define-key org-mode-map "\C-c\C-s" nil)
    (define-key org-mode-map "\C-c\C-d" nil)
    (define-key org-mode-map "\C-c;" nil)
    (define-key org-mode-map "\C-c\C-w" nil)
    (define-key org-mode-map "\C-c/" nil)
    (define-key org-mode-map "\C-c\\" nil)
    (define-key org-mode-map "\C-c\C-m" nil)
    (define-key org-mode-map "\M-\C-m" nil)
    (define-key org-mode-map [(control return)] nil)
    (define-key org-mode-map [(shift control return)] nil)
    (define-key org-mode-map "\C-c\C-l" nil)
    (define-key org-mode-map "\C-c\C-o" nil)
    (define-key org-mode-map "\C-c%" nil)
    (define-key org-mode-map "\C-c&" nil)
    (define-key org-mode-map "\C-c\C-z" nil)
    (define-key org-mode-map "\C-c." nil)
    (define-key org-mode-map "\C-c!" nil)
    (define-key org-mode-map "\C-c," nil)
    (define-key org-mode-map "\C-c\C-y" nil)
    (define-key org-mode-map "\C-c>" nil)
    (define-key org-mode-map "\C-c<" nil)
    (define-key org-mode-map [(control ?,)] nil)
    (define-key org-mode-map [(control ?\')] nil)
    (define-key org-mode-map "\C-c[" nil)
    (define-key org-mode-map "\C-c]" nil)
    (define-key org-mode-map "\C-c-" nil)
    (define-key org-mode-map "\C-c*" nil)
    (define-key org-mode-map "\C-c^" nil)
    (define-key org-mode-map "\C-c\C-c" nil)
    (define-key org-mode-map "\C-c\C-k" nil)
    (define-key org-mode-map "\C-c#" nil)
    (define-key org-mode-map "\C-m" nil)
    (define-key org-mode-map "\C-j" nil)
    (define-key org-mode-map "\C-c?" nil)
    (define-key org-mode-map "\C-c " nil)
    (define-key org-mode-map "\C-c+" nil)
    (define-key org-mode-map "\C-c=" nil)
    (define-key org-mode-map "\C-c'" nil)
    (define-key org-mode-map "\C-c`" nil)
    (define-key org-mode-map "\C-c|" nil)
    (define-key org-mode-map [(control ?#)] nil)
    (define-key org-mode-map "\C-c~" nil)
    (define-key org-mode-map "\C-c\C-a" nil)
    (define-key org-mode-map "\C-c}" nil)
    (define-key org-mode-map "\C-c{" nil)
    (define-key org-mode-map "\C-c\C-e" nil)
    (define-key org-mode-map "\C-c:" nil)
    (define-key org-mode-map [?\C-c (control ?*)] nil)
    (define-key org-mode-map "\C-a" nil)
    (define-key org-mode-map "\C-e" nil)
    (define-key org-mode-map "\C-c@" nil)
    (define-key org-mode-map [home] nil)
    (define-key org-mode-map [end] nil)
    (define-key org-mode-map "\M-a" nil)
    (define-key org-mode-map "\M-e" nil)

    ;; Global
    (define-key org-mode-map (kbd "C-c s") 'org-save-all-org-buffers)
    (define-key org-mode-map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)

    ;; Outline visibility
    (define-key org-mode-map (kbd "TAB") 'org-cycle)
    (define-key org-mode-map (kbd "C-c >") 'org-narrow-to-subtree)
    (define-key org-mode-map (kbd "C-c <") 'widen)
    (define-key org-mode-map (kbd "C-c C") 'org-columns)

    ;; Outline navigation
    (define-key org-mode-map (kbd "C-M-u") 'alb-org-up-structure)
    (define-key org-mode-map (kbd "ESC C-u") 'alb-org-up-structure)
    (define-key org-mode-map (kbd "C-M-d") 'alb-org-down-structure)
    (define-key org-mode-map (kbd "ESC C-d") 'alb-org-down-structure)
    (define-key org-mode-map (kbd "C-M-b") 'alb-org-backward-structure)
    (define-key org-mode-map (kbd "ESC C-b") 'alb-org-backward-structure)
    (define-key org-mode-map (kbd "C-M-f") 'alb-org-forward-structure)
    (define-key org-mode-map (kbd "ESC C-f") 'alb-org-forward-structure)
    (define-key org-mode-map (kbd "C-M-p") 'alb-org-prev-structure)
    (define-key org-mode-map (kbd "ESC C-p") 'alb-org-prev-structure)
    (define-key org-mode-map (kbd "C-M-n") 'alb-org-next-structure)
    (define-key org-mode-map (kbd "ESC C-n") 'alb-org-next-structure)
    (define-key org-mode-map (kbd "C-c u") 'alb-org-up-heading)
    (define-key org-mode-map (kbd "C-c d") 'alb-org-down-heading)
    (define-key org-mode-map (kbd "C-c b") 'org-backward-heading-same-level)
    (define-key org-mode-map (kbd "C-c f") 'org-forward-heading-same-level)
    (define-key org-mode-map (kbd "C-c p") 'outline-previous-visible-heading)
    (define-key org-mode-map (kbd "C-c n") 'outline-next-visible-heading)
    (define-key org-mode-map (kbd "C-a") 'org-beginning-of-line)
    (define-key org-mode-map (kbd "C-e") 'org-end-of-line)
    (define-key org-mode-map (kbd "C-c o") 'org-open-at-point)

    ;; Outline structure editing
    (define-key org-mode-map (kbd "C-c .") 'outline-mark-subtree)
    (define-key org-mode-map (kbd "C-c a") 'org-archive-subtree-default)
    (define-key org-mode-map (kbd "C-c i") 'alb-org-insert-heading-before)
    (define-key org-mode-map (kbd "C-c j") 'alb-org-insert-heading-after)
    (define-key org-mode-map (kbd "C-c c") 'org-capture)
    (define-key org-mode-map (kbd "C-c w") 'org-refile)

    ;; Clock commands
    (define-key org-mode-map (kbd "C-c I") 'org-clock-in)
    (define-key org-mode-map (kbd "C-c O") 'org-clock-out)
    (define-key org-mode-map (kbd "C-c X") 'org-clock-cancel)
    (define-key org-mode-map (kbd "C-c J") 'org-clock-goto)

    ;; Meta-data editing
    (define-key org-mode-map (kbd "C-c t") 'org-todo)
    (define-key org-mode-map (kbd "C-c :") 'org-set-tags-command)
    (define-key org-mode-map (kbd "C-c S") 'org-schedule)
    (define-key org-mode-map (kbd "C-c E") 'org-set-effort)
    (define-key org-mode-map (kbd "C-c #") 'org-update-statistics-cookies)

    ;; Agenda views
    (define-key org-mode-map (kbd "C-c v") 'org-agenda)

    ;; Exporting and publishing
    (define-key org-mode-map (kbd "C-c e") 'org-export-dispatch)

    ;; Content editing
    (define-key org-mode-map (kbd "C-c L") 'org-insert-link)
    (define-key org-mode-map (kbd "C-c !") 'org-time-stamp-inactive)
    (define-key org-mode-map (kbd "S-<up>") 'org-timestamp-up)
    (define-key org-mode-map (kbd "S-<down>") 'org-timestamp-down)
    (define-key org-mode-map (kbd "C-<return>") 'alb-org-newline-before)
    (define-key org-mode-map (kbd "C-S-<return>") 'alb-org-newline-after)
    (define-key org-mode-map (kbd "M-<return>") 'alb-org-insert-item)
    (define-key org-mode-map (kbd "ESC <return>") 'alb-org-insert-item)
    (define-key org-mode-map (kbd "M-S-<return>") 'alb-org-insert-checkbox)
    (define-key org-mode-map (kbd "ESC S-<return>") 'alb-org-insert-checkbox)
    (define-key org-mode-map (kbd "M-<up>") 'org-move-item-up)
    (define-key org-mode-map (kbd "ESC <up>") 'org-move-item-up)
    (define-key org-mode-map (kbd "M-<down>") 'org-move-item-down)
    (define-key org-mode-map (kbd "ESC <down>") 'org-move-item-down)
    (define-key org-mode-map (kbd "M-<left>") 'org-outdent-item-tree)
    (define-key org-mode-map (kbd "ESC <left>") 'org-outdent-item-tree)
    (define-key org-mode-map (kbd "M-<right>")'org-indent-item-tree)
    (define-key org-mode-map (kbd "ESC <right>")'org-indent-item-tree)
    (define-key org-mode-map (kbd "C-c -") 'org-cycle-list-bullet)
    (define-key org-mode-map (kbd "C-c x") 'alb-org-toggle-checkbox)
    (define-key org-mode-map (kbd "C-c '") 'org-edit-special)

    ;; Reset default speed keys
    (setq org-speed-commands-default
          '(("Global")
            ("?" . org-speed-command-help)
            ("s" . org-save-all-org-buffers)
            ("Outline visibility")
            ("1" . (progn (org-shifttab 1) (outline-back-to-heading)))
            ("2" . (progn (org-shifttab 2) (outline-back-to-heading)))
            ("3" . (progn (org-shifttab 3) (outline-back-to-heading)))
            ("4" . (progn (org-shifttab 4) (outline-back-to-heading)))
            ("5" . (progn (org-shifttab 5) (outline-back-to-heading)))
            ("0" . show-all)
            (">" . org-narrow-to-subtree)
            ("<" . alb-org-widen)
            ("C" . org-columns)
            ("Outline navigation")
            ("u" . (org-speed-move-safe 'alb-org-up-heading))
            ("d" . (org-speed-move-safe 'alb-org-down-heading))
            ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
            ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
            ("p" . (org-speed-move-safe  'outline-previous-visible-heading))
            ("n" . (org-speed-move-safe 'outline-next-visible-heading))
            ("Outline structure editing")
            ("." . outline-mark-subtree)
            ("B" . org-move-subtree-up)
            ("F" . org-move-subtree-down)
            ("L" . (progn (org-promote) (outline-back-to-heading)))
            ("R" . (progn (org-demote) (outline-back-to-heading)))
            ("i" . alb-org-insert-heading-before)
            ("j" . alb-org-insert-heading-after)
            ("c" . org-capture)
            ("w" . org-refile)
            ("^" . (org-sort-entries nil ?f 'alb-org-sort-string))
            ("Clock commands")
            ("I" . org-clock-in)
            ("O" . org-clock-out)
            ("X" . org-clock-cancel)
            ("J" org-clock-goto t)
            ("Meta-data editing")
            ("t" . org-todo)
            (":" . org-set-tags-command)
            ("S" . org-schedule)
            ("E" . org-set-effort)
            ("#" . alb-org-update-headline-statistics)
            ("l" . org-store-link)
            ("Agenda views")
            ("v" . org-agenda)
            ("Exporting and publishing")
            ("e" . org-export-dispatch)))

    ;; Fix the clock values on entries
    (org-clock-sum)))

;; Local Variables:
;; mode: emacs-lisp
;; End:
