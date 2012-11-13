;;;
;;; AlbOrgMode/alb-org-behavior.el
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
;;;   - Functions to configure Org-Mode.
;;;



;;; *** PROVIDED FEATURE ******************************************************


(provide 'alb-org-behavior)



;;; *** REQUIRED FEATURES *****************************************************


(require 'org)



;;; *** CONSTANT DECLARATIONS *************************************************


(defconst alb-re-org-date
  (concat "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
          " \\(?:Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)"
          "\\(?: [0-9]\\{2\\}:[0-9]\\{2\\}\\)?"
          "\\(?: \\(\+\\|\+\+\\|\.\+\\)[0-9]+[hdwmy]\\)?"
          "\\(?: -[0-9]+[hdwmy]\\)?"
          )
  "Regexp matching an Org-Mode timestamp including repeaters.  It
consumes 0 subexpression.")


(defconst alb-re-org-todo
  "\\(TODO\\|NEXT\\|WAIT\\|DUTY\\|HOLD\\|DONE\\|STOP\\)"
  "Regexp matching an Org-Mode todo state. It consumes 1
subexpression.")


(defconst alb-re-org-priority
  "\\(\\[#[A-C]\\]\\)"
  "Regexp matching an Org-Mode priority. It consumes 1
subexpression.")


(defconst alb-re-org-heading
  (concat "^"
          "\\(\\*+\\)? *"
          alb-re-org-todo "? *"
          alb-re-org-priority "? *"
          "\\(.*?\\) *"
          "\\(:[a-z:@_]+:\\)? *"
          "$")
  "Regexp matching an Org-Mode heading with anchoring to the
start and end of line.  It consumes 5 subexpressions as follows.

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



;;; *** FACE DECLARATIONS *****************************************************


(defface alb-org-priority-a
  '((t :foreground "white" :weight bold))
  "Face used for priority A keyword."
  :group 'org-faces)


(defface alb-org-priority-b
  '((t :foreground "grey80" :weight bold))
  "Face used for priority B keyword."
  :group 'org-faces)


(defface alb-org-priority-c
  '((t :foreground "grey60" :weight bold))
  "Face used for priority C keyword."
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


(defface alb-org-keyword-duty
  '((t :foreground "CornflowerBlue" :weight bold))
  "Face used for DUTY keyword."
  :group 'org-faces)


(defface alb-org-keyword-hold
  '((t :foreground "LightGoldenrod" :weight bold))
  "Face used for HOLD keyword."
  :group 'org-faces)


(defface alb-org-keyword-done
  '((t :foreground "IndianRed" :weight bold))
  "Face used for DONE keyword."
  :group 'org-faces)


(defface alb-org-keyword-stop
  '((t :foreground "grey30" :weight bold))
  "Face used for STOP keyword."
  :group 'org-faces)



;;; *** FUNCTION DEFINITIONS **************************************************


(defun alb-org-whitespace-cleanup ()
  "Clean buffer of Org-Mode specific whitespace issues

Clean buffer content.  Return nil, so that the function can be
safely added to the `write-contents-functions` hook."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\n\\{2,\\}" nil t)
      (replace-match "\n" nil nil))
    (goto-char (point-min))
    (while (re-search-forward (concat "\\([^[:space:]]\\)\n\\("
                                      org-outline-regexp "\\)")
                              nil t)
      (replace-match "\\1\n\n\\2" nil nil))
    (goto-char (point-min))
    (while (re-search-forward org-outline-regexp-bol nil t)
      (org-set-tags nil t)
      (end-of-line 1))
    nil))


(defun alb-add-org-whitespace-cleanup ()
  "Add buffer local hook to clean buffer of Org-Mode specific whitespace issues

Add `alb-org-whitespace-cleanup` to the buffer local
`write-contents-functions`."
  (add-hook 'write-contents-functions 'alb-org-whitespace-cleanup))


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


(defun alb-org-sort-key ()
  "Generate a key for sorting the entry at point

Ensures that TODO items are sorted by an augmented reverse
chronological order.  Namely, the todo state, and various
properties are given a chronological interpretation.  All =DUTY=
and =HOLD= items occur before all =TODO= items, which occur
before all =NEXT= items, which occur before all =WAIT= items,
which occur before all =DONE= and =STOP= items.  =DUTY= items are
sorted by effort, =TODO= and =NEXT= items are sorted by deadline,
and =WAIT=, =DONE=, and =STOP= items are sorted by last
timestamp.  This function customises Org-Mode."
  (let* ((head (org-get-heading t t))
         (properties (org-entry-properties))
         (todo (cdr (assoc "TODO" properties)))
         (deadline (cdr (assoc "DEADLINE" properties)))
         (timestamp (cdr (assoc "TIMESTAMP_IA" properties))))
    (cond
     ;; XXX the sort order is not worked out yet
     ((not todo)
      (concat "0#" head))
     ((or (string= todo "DUTY") (string= todo "HOLD"))
      (concat "1#" head)))
     ((string= todo "TODO")
      (concat "2#" deadline "#" head))
     ((string= todo "NEXT")
      (concat "3#" deadline "#" head))
     ((string= todo "WAIT")
      (concat "4#" timestamp "#" head))
     ((or (string= todo "DONE") (string= todo "STOP"))
      (concat "5#" timestamp "#" head))))


(defun alb-org-widen ()
  "Widen to the whole buffer and centre the headline"
  (interactive)
  (widen)
  (recenter))


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


(defun alb-org-insert-heading-before ()
  "Move to the containing heading and insert a matching heading"
  (interactive)
  (let ((stars (progn (if (org-before-first-heading-p)
                          (outline-next-heading)
                        (outline-back-to-heading))
                      (looking-at outline-regexp)
                      (match-string-no-properties 0))))
    (newline 2)
    (forward-line -2)
    (insert stars)))


(defun alb-org-insert-heading-after ()
  "Move beyond the containing heading and insert a matching heading"
  (interactive)
  (let ((stars (progn (if (org-before-first-heading-p)
                          (outline-next-heading)
                        (outline-back-to-heading))
                      (looking-at outline-regexp)
                      (match-string-no-properties 0))))
    (org-end-of-subtree)
    (newline 2)
    (insert stars)))


(defun alb-org-newline-before ()
  "Insert a newline before the current line"
  (interactive)
  (save-excursion (beginning-of-line)
                  (newline)))


(defun alb-org-newline-after ()
  "Insert a newline after the current line"
  (interactive)
  (save-excursion (beginning-of-line 2)
                  (newline)))


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

Calls `alb-org-insert-gmail-link-text'."
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


(defun alb-org-columns-modify-value-for-display-function (column-title value)
  "Modify values for display in column view

The mappings are designed to make deadlines, effort estimates,
and elapsed time easier to follow in column view.

- when COLUMN-TITLE is =Task= remove tags from =ITEM= value; and
- when COLUMN-TITLE is =Project= remove tags from =ITEM= value."
  (cond ((string= column-title "Project")
         (if (string-match alb-re-org-heading value)
             (concat (match-string 1 value) " " (match-string 4 value))))
        ((string= column-title "Label")
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
               ((string= value "DUTY") "u")
               ((string= value "HOLD") "h")
               ((string= value "DONE") "D")
               ((string= value "STOP") "S")
               ))))





;;; Local Variables:
;;; mode: emacs-lisp
;;; End:
