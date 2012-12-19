;;;
;;; AlbOrgMode/alb-org-mode-start.el
;;;
;;;     Copyright (C) 2010-2012 Andrew Lincoln Burrow
;;;
;;;     This library is free software; you can redistribute it and/or
;;;     modify it under the terms of the GNU General Public License as
;;;     published by the Free Software Foundation; either version 2 of
;;;     the License, or (at your option) any later version.
;;;
;;;     This library is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;;
;;;     You should have received a copy of the GNU General Public
;;;     License along with this library; if not, write to the Free
;;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;;     MA 02111-1307, USA.
;;;
;;;   - Site start code for basic albcorp Org-Mode setup.
;;;



;;; *** AMMENDED LOAD PATH ****************************************************


(setq load-path
      (cons (directory-file-name (file-name-directory load-file-name))
            load-path))



;;; *** REQUIRED FEATURES *****************************************************



;;; *** IMMEDIATE CONFIGURATION ***********************************************

;;;
;;; Auto Modes
;;;

(setq auto-mode-alist
      (cons '("\\.org$" . org-mode) auto-mode-alist))


;;; *** DEFERRED CONFIGURATION ************************************************

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
    (define-key org-mode-map "\C-cs" 'org-save-all-org-buffers)
    (define-key org-mode-map "\C-c\C-c" 'org-ctrl-c-ctrl-c)

    ;; Outline visibility
    (define-key org-mode-map "\t" 'org-cycle)
    (define-key org-mode-map "\C-c>" 'org-narrow-to-subtree)
    (define-key org-mode-map "\C-c<" 'widen)
    (define-key org-mode-map "\C-cC" 'org-columns)

    ;; Outline navigation
    (define-key org-mode-map "\C-\M-u" 'alb-org-up-structure)
    (define-key org-mode-map "\C-\M-d" 'alb-org-down-structure)
    (define-key org-mode-map "\C-\M-b" 'alb-org-backward-structure)
    (define-key org-mode-map "\C-\M-f" 'alb-org-forward-structure)
    (define-key org-mode-map "\C-\M-p" 'alb-org-prev-structure)
    (define-key org-mode-map "\C-\M-n" 'alb-org-next-structure)
    (define-key org-mode-map "\C-cu" 'alb-org-up-heading)
    (define-key org-mode-map "\C-cd" 'alb-org-down-heading)
    (define-key org-mode-map "\C-cb" 'org-backward-same-level)
    (define-key org-mode-map "\C-cf" 'org-forward-same-level)
    (define-key org-mode-map "\C-cp" 'outline-previous-visible-heading)
    (define-key org-mode-map "\C-cn" 'outline-next-visible-heading)
    (define-key org-mode-map "\C-a" 'org-beginning-of-line)
    (define-key org-mode-map "\C-e" 'org-end-of-line)
    (define-key org-mode-map "\C-co" 'org-open-at-point)

    ;; Outline structure editing
    (define-key org-mode-map "\C-c." 'outline-mark-subtree)
    (define-key org-mode-map "\C-ca" 'org-archive-subtree-default)
    (define-key org-mode-map "\C-ci" 'alb-org-insert-heading-before)
    (define-key org-mode-map "\C-cj" 'alb-org-insert-heading-after)
    (define-key org-mode-map "\C-cc" 'org-capture)
    (define-key org-mode-map "\C-cw" 'org-refile)

    ;; Clock commands
    (define-key org-mode-map "\C-cI" 'org-clock-in)
    (define-key org-mode-map "\C-cO" 'org-clock-out)
    (define-key org-mode-map "\C-cX" 'org-clock-cancel)
    (define-key org-mode-map "\C-cJ" 'org-clock-goto)

    ;; Meta-data editing
    (define-key org-mode-map "\C-ct" 'org-todo)
    (define-key org-mode-map "\C-c:" 'org-set-tags-command)
    (define-key org-mode-map "\C-cD" 'org-deadline)
    (define-key org-mode-map "\C-cS" 'org-schedule)
    (define-key org-mode-map "\C-cE" 'org-set-effort)
    (define-key org-mode-map "\C-c#" 'org-update-statistics-cookies)

    ;; Agenda views
    (define-key org-mode-map "\C-cv" 'org-agenda)
    (define-key org-mode-map "\C-c/" 'org-sparse-tree)

    ;; Content editing
    (define-key org-mode-map "\C-cL" 'org-insert-link)
    (define-key org-mode-map "\C-c@" 'alb-org-insert-gmail-link-text)
    (define-key org-mode-map "\C-c!" 'org-time-stamp-inactive)
    (define-key org-mode-map [(shift up)] 'org-timestamp-up)
    (define-key org-mode-map [(shift down)] 'org-timestamp-down)
    (define-key org-mode-map [(control return)] 'alb-org-newline-before)
    (define-key org-mode-map [(control shift return)] 'alb-org-newline-after)
    (define-key org-mode-map [(meta return)] 'alb-org-insert-item)
    (define-key org-mode-map [(meta shift return)]
      'alb-org-insert-checkbox)
    (define-key org-mode-map [(meta up)] 'org-move-item-up)
    (define-key org-mode-map [(meta down)] 'org-move-item-down)
    (define-key org-mode-map [(meta left)] 'org-outdent-item-tree)
    (define-key org-mode-map [(meta right)]'org-indent-item-tree)
    (define-key org-mode-map "\C-c-" 'org-cycle-list-bullet)
    (define-key org-mode-map "\C-cx" 'alb-org-toggle-checkbox)
    (define-key org-mode-map "\C-c'" 'org-edit-special)

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
            ("b" . (org-speed-move-safe 'org-backward-same-level))
            ("f" . (org-speed-move-safe 'org-forward-same-level))
            ("p" . (org-speed-move-safe 'outline-previous-visible-heading))
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
            ("+" . org-priority-up)
            ("-" . org-priority-down)
            (":" . org-set-tags-command)
            ("D" . org-deadline)
            ("S" . org-schedule)
            ("E" . org-set-effort)
            ("#" . alb-org-update-headline-statistics)
            ("l" . org-store-link)
            ("Agenda views")
            ("v" . org-agenda)
            ("/" . org-sparse-tree)
            ("Content editing")
            ("@" . alb-org-insert-gmail-link-item)))

    ;; Fix the clock values on entries
    (org-clock-sum)))




;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
