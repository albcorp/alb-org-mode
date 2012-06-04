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



;;; *** REQUIRED FEATURES *****************************************************



;;; *** IMMEDIATE CONFIGURATION ***********************************************

;;
;; Regular expressions

(defconst alb-re-org-date
  (concat "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
          " \\(?:Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)"
          "\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?")
  "Regexp matching an Org-Mode timestamp. It consumes 0
  subexpression.")

(defconst alb-re-org-heading
  (concat "^"
          "\\(\\*+\\)? *"
          "\\(TODO\\|NEXT\\|WAIT\\|DONE\\|STOP\\|DUTY\\)? *"
          "\\(\\[#[A-C]\\]\\)? *"
          "\\(.*?\\) *"
          "\\(:[a-z:@_]+:\\)? *"
          "$")
  "Regexp matching an Org-Mode heading.  The match excludes the
newline terminating the headline. It consumes 5 subexpressions
as follows.

1. (optional) Asterisks
2. (optional) TODO keyword
3. (optional) Priority
4. (mandatory) Heading text including (optional) statistics and
   trailing whitespace
5. (optional) Tags")

(defconst alb-re-org-metadata
  (concat "\\(?:\n[ \t]+"
          "\\(?:"
          "\\(?:DEADLINE\\|SCHEDULED\\):[ \t]+<" alb-re-org-date ">"
          "\\(?:[ \t]+\\(?:DEADLINE\\|SCHEDULED\\):[ \t]+<"
          alb-re-org-date ">\\)?"
          "\\|"
          ":\\(?:LOGBOOK\\|PROPERTIES\\):[ \t]*\\(?:\n.*\\)*?\n[ \t]+:END:"
          "\\)[ \t]*"
          "\\)*")
  "Regexp matching the possibly-empty properties metadata for a
heading. The match includes the newline terminating the headline,
but excludes the newline terminating the metadata. It consumes 0
subexpressions.")

(defconst alb-re-org-project-filename
  "^\\(.*\\)/\\(prj\\|jrn\\)-\\([0-9a-z-]*\\)\\.org$"
  "Regexp matching a project filename for use in formatting
column views. The first subexpression matches the directory part
of the filename. The third subexpression matches the project
ID.")

;;
;; Faces for keywords

(defface alb-org-priority-a
  '((t :foreground "white" :weight bold))
  "Face used for DUTY keyword."
  :group 'org-faces)

(defface alb-org-priority-b
  '((t :foreground "grey80" :weight bold))
  "Face used for DUTY keyword."
  :group 'org-faces)

(defface alb-org-priority-c
  '((t :foreground "grey60" :weight bold))
  "Face used for DUTY keyword."
  :group 'org-faces)

(defface alb-org-keyword-duty
  '((t :foreground "CornflowerBlue" :weight bold))
  "Face used for DUTY keyword."
  :group 'org-faces)

(defface alb-org-keyword-todo
  '((t :foreground "grey" :weight bold))
  "Face used for TODO keyword."
  :group 'org-faces)

(defface alb-org-keyword-next
  '((t :foreground "PaleGreen" :weight bold))
  "Face used for NEXT keyword."
  :group 'org-faces)

(defface alb-org-keyword-wait
  '((t :foreground "LightGoldenrod" :weight bold))
  "Face used for WAIT keyword."
  :group 'org-faces)

(defface alb-org-keyword-done
  '((t :foreground "IndianRed" :weight bold))
  "Face used for DONE keyword."
  :group 'org-faces)

(defface alb-org-keyword-stop
  '((t :foreground "grey30" :weight bold))
  "Face used for STOP keyword."
  :group 'org-faces)

;;
;; Agenda Sorting

(defun alb-org-agenda-cmp (a b)
  "Compare the agenda entries A and B using lexicographic order.

This function customises Org-Mode."
  (let ((a-body (if (string-match alb-re-org-heading a)
                    (match-string 4 a)
                  a))
        (b-body (if (string-match alb-re-org-heading b)
                    (match-string 4 b)
                  b)))
    (cond ((string< a-body b-body) -1)
          ((string< b-body a-body) 1))))

;;
;; Outline Visibility

(defun alb-org-widen ()
  "Widen to the while buffer and centre the headline"
  (interactive)
  (widen)
  (recenter))

;;
;; Outline navigation

(defun alb-org-home ()
  "Move to the start of the headline

Pushes current point onto the mark ring. Places the point on the
first character of the "
  (interactive)
  (push-mark)
  (if (org-before-first-heading-p)
      (outline-next-heading)
    (outline-back-to-heading)))

(defun alb-org-end ()
  "Move to the start of the content beneath the headline

Places the point on the first non-whitespace character after the
metadata. If there is content, point is placed at the first
non-whitespace character, and indents the line. Otherwise, it
ensures there are three blank lines, indents the second blank
line, and places the cursor at the end of the second blank line."
  (interactive)
  (if (org-before-first-heading-p)
      (outline-next-heading)
    (outline-back-to-heading))
  (org-show-entry)
  (let* ((succ-pos (save-excursion      ; Start of successor headline
                     (outline-next-heading)
                     (point)))
         (eofp-pos (save-excursion      ; End of headline properties
                     (looking-at alb-re-org-heading)
                     (goto-char (match-end 0))
                     (looking-at alb-re-org-metadata)
                     (match-end 0)))
         (indt-pos (save-excursion      ; Start of indented content
                     (goto-char eofp-pos)
                     (if (looking-at "\\(?:\n[ \t]*\\)*\\([^[:space:]]\\)")
                         (match-beginning 1)
                       succ-pos))))
    (if (< indt-pos succ-pos)
        (progn (goto-char indt-pos)
               (indent-according-to-mode))
      (progn (goto-char eofp-pos)
             (forward-line)
             (delete-region (point) succ-pos)
             (newline 3)
             (forward-line -2)
             (indent-according-to-mode)))))

;;
;; Outline structure editing

(defun alb-org-insert-heading-after-current ()
  "Insert new heading of the same level and type as the current heading

Inserts a new heading after the current heading. Looks up the
`org-capture' template associated with the key =t=. If the
current heading has a todo state and is at the same level as the
template, inserts the expansion of this template. Otherwise,
inserts a blank heading of the same level."
  (interactive)
  (if (org-before-first-heading-p)
      (outline-next-heading)
    (outline-back-to-heading))
  (looking-at alb-re-org-heading)
  (let* ((curr-star (match-string-no-properties 1))
         (curr-todo (match-string-no-properties 2))
         (tmpl (nth 4 (assoc "t" org-capture-templates)))
         (tmpl-star (and tmpl
                         (string-match "^\\(\\*+\\) " tmpl)
                         (match-string-no-properties 1 tmpl))))
    (if (and curr-star curr-todo (string= curr-star tmpl-star))
        (progn (insert (org-capture-fill-template tmpl))
               (outline-backward-same-level 1)
               (org-move-subtree-down)
               (alb-org-end))
      (progn (org-insert-heading)
             (org-move-subtree-down)
             (end-of-line 1)))))

;;
;; Meta-data editing

(defun alb-org-update-headline-statistics ()
  "Update the statistics cookie on the headline

The behavior depends on the presence of the statistics cookie in
the headline.  If missing, inserts the statistics cookie and then
updates it. If present and equal to =[0/0]= before and after
update, removes it. Repairs the positions of the tags."
  (interactive)
  (save-excursion
    (if (org-before-first-heading-p)
        (outline-next-heading)
      (outline-back-to-heading))
    (let* ((old-title (progn
                        (looking-at alb-re-org-heading)
                        (org-trim (match-string-no-properties 4))))
           (old-stats (progn
                        (if (string-match "\\[[0-9]*/[0-9]*\\]" old-title)
                            (match-string 0 old-title)))))
      (if (not old-stats)
          (save-excursion
            (search-forward old-title)
            (replace-match (concat old-title " [/]"))))
      (org-update-statistics-cookies nil)
      (if (string-equal old-stats "[0/0]")
          (let* ((new-title (progn
                              (looking-at alb-re-org-heading)
                              (org-trim (match-string-no-properties 4))))
                 (new-stats (progn
                              (string-match "\\[[0-9]*/[0-9]*\\]" new-title)
                              (match-string 0 new-title))))
            (if (string-equal old-stats new-stats)
                (save-excursion
                  (search-forward "[0/0]")
                  (replace-match "")))))
      (org-set-tags nil t))))

;;
;; Content editing

(defun alb-org-insert-gmail-link-text ()
  "Insert text describing a gmail message

Inserts the expansion of the `org-capture' template associated
with the key =e= at point."
  (interactive)
  (insert (org-trim
           (org-capture-fill-template
            (nth 4 (assoc "e" org-capture-templates))))))

(defun alb-org-insert-gmail-link-item ()
  "Insert link describing a gmail message into current headline

Calls `alb-org-insert-gmail-link-item'."
  (interactive)
  (save-excursion
    (alb-org-end)
    (cond ((org-at-item-p)
           (org-insert-item))
          ((looking-at "[^[:space:]]")
           (newline-and-indent)
           (newline-and-indent)
           (forward-line -2)
           (indent-according-to-mode)
           (insert "- "))
          (t
           (insert "- ")))
    (alb-org-insert-gmail-link-text)))

(defun alb-org-insert-item ()
  "Insert a list item."
  (interactive)
  (org-insert-item nil))

(defun alb-org-insert-checkbox ()
  "Insert a check boxed list item."
  (interactive)
  (org-insert-item t))

(defun alb-org-toggle-checkbox (&optional toggle-presence)
  "Toggle a check boxed list item."
  (interactive "P")
  (org-toggle-checkbox toggle-presence))

;;
;; Column View Customisation

(defun alb-org-columns-modify-value-for-display-function (column-title value)
  "Modify values for display in column view

The mappings are designed to make deadlines, effort estimates,
and elapsed time easier to follow in column view.

- when COLUMN-TITLE is =Task= remove tags from =ITEM= value; and
- when COLUMN-TITLE is =Project= remove tags from =ITEM= value."
  (cond ((string= column-title "Project")
         (if (string-match alb-re-org-heading value)
             (concat (match-string 1 value) " " (match-string 4 value))))
        ((string= column-title "Tag")
         (if (string-match alb-re-org-project-filename value)
             (match-string 3 value)))
        ((string= column-title "Media")
         (concat (if (string-match-p ":on_email:" value) "@")
                 (if (string-match-p ":on_paper:" value) "#")
                 (if (string-match-p ":on_docs:" value) "$")))
        ((string= column-title "Task")
         (if (string-match alb-re-org-heading value)
             (concat (match-string 1 value) " " (match-string 4 value))))
        ((string= column-title "X")
         (cond ((string= value "TODO") "t")
               ((string= value "NEXT") "n")
               ((string= value "WAIT") "w")
               ((string= value "DONE") "D")
               ((string= value "STOP") "S")
               ((string= value "DUTY") "U")))))


;;; *** DEFERRED CONFIGURATION ************************************************

(add-hook
 'org-load-hook
 'org-clock-persistence-insinuate)

(add-hook
 'org-mode-hook
 '(lambda ()
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
    (define-key org-mode-map "\C-cS" 'org-save-all-org-buffers)
    (define-key org-mode-map "\C-c\C-c" 'org-ctrl-c-ctrl-c)

    ;; Outline visibility
    (define-key org-mode-map "\t" 'org-cycle)
    (define-key org-mode-map "\C-c>" 'org-narrow-to-subtree)
    (define-key org-mode-map "\C-c<" 'widen)
    (define-key org-mode-map "\C-cC" 'org-columns)

    ;; Outline navigation
    (define-key org-mode-map [home] 'alb-org-home)
    (define-key org-mode-map [end] 'alb-org-end)
    (define-key org-mode-map "\C-cp" 'outline-previous-visible-heading)
    (define-key org-mode-map "\C-cn" 'outline-next-visible-heading)
    (define-key org-mode-map "\C-a" 'org-beginning-of-line)
    (define-key org-mode-map "\C-e" 'org-end-of-line)
    (define-key org-mode-map "\C-c." 'outline-mark-subtree)
    (define-key org-mode-map "\C-co" 'org-open-at-point)

    ;; Outline structure editing
    (define-key org-mode-map "\C-ci"
      'alb-org-insert-heading-after-current)
    (define-key org-mode-map "\C-cj" 'org-capture)
    (define-key org-mode-map "\C-cw" 'org-refile)

    ;; Clock commands
    (define-key org-mode-map "\C-cI" 'org-clock-in)
    (define-key org-mode-map "\C-cO" 'org-clock-out)
    (define-key org-mode-map "\C-cX" 'org-clock-cancel)
    (define-key org-mode-map "\C-cJ" 'org-clock-goto)

    ;; Meta-data editing
    (define-key org-mode-map "\C-ct" 'org-todo)
    (define-key org-mode-map "\C-c:" 'org-set-tags-command)
    (define-key org-mode-map "\C-cd" 'org-deadline)
    (define-key org-mode-map "\C-cs" 'org-schedule)
    (define-key org-mode-map "\C-ce" 'org-set-effort)
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
    (define-key org-mode-map [(meta return)] 'alb-org-insert-item)
    (define-key org-mode-map [(meta shift return)]
      'alb-org-insert-checkbox)
    (define-key org-mode-map [(meta up)] 'org-move-item-up)
    (define-key org-mode-map [(meta down)] 'org-move-item-down)
    (define-key org-mode-map [(meta left)] 'org-outdent-item-tree)
    (define-key org-mode-map [(meta right)]'org-indent-item-tree)
    (define-key org-mode-map "\C-c-" 'org-cycle-list-bullet)
    (define-key org-mode-map "\C-cx" 'alb-org-toggle-checkbox)

    ;; Reset default speed keys
    (setq org-speed-commands-default
          '(("Global")
            ("?" . org-speed-command-help)
            ("S" . org-save-all-org-buffers)
            ("Outline navigation")
            ("u" . (org-speed-move-safe 'outline-up-heading))
            ("p" . (org-speed-move-safe
                    'outline-previous-visible-heading))
            ("n" . (org-speed-move-safe 'outline-next-visible-heading))
            ("b" . (org-speed-move-safe 'org-backward-same-level))
            ("f" . (org-speed-move-safe 'org-forward-same-level))
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
            ("Outline structure editing")
            ("." . outline-mark-subtree)
            ("B" . org-move-subtree-up)
            ("F" . org-move-subtree-down)
            ("L" . (progn (org-promote) (outline-back-to-heading)))
            ("R" . (progn (org-demote) (outline-back-to-heading)))
            ("i" . alb-org-insert-heading-after-current)
            ("j" . org-capture)
            ("w" . org-refile)
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
            ("d" . org-deadline)
            ("s" . org-schedule)
            ("e" . org-set-effort)
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
