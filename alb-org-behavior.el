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
but excludes the newline terminating the metadata.  It consumes 0
subexpressions.")


(defconst alb-re-org-project-filename
  "^\\(.*\\)/prj-\\([0-9a-z-]*\\)\\.org$"
  "Regexp matching a project filename for use in formatting
column views. The first subexpression matches the directory part
of the filename. The second subexpression matches the project
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


(defun alb-org-entry-structure-visit (past-pos)
  "Recursively collect the entry structure upto PAST-POS

Collect the preamble and list at point and advance point to the
end of the list.  Recur on the space remaining before PAST-POS.
See `alb-org-entry-structure`."
  (let* ((text-pos (if (looking-at "[[:space:]]*")
                       (match-end 0)))
         (item-pos (if (re-search-forward (org-item-beginning-re) past-pos t)
                       (match-beginning 0))))
    (cond
     ((<= past-pos text-pos)
      nil)
     ((not item-pos)
      (list (cons text-pos nil)))
     (t
      (let ((struct (progn (goto-char item-pos)
                           (org-list-struct))))
        (cons (cons text-pos struct)
              (progn (goto-char (org-list-get-bottom-point struct))
                     (alb-org-entry-structure-visit past-pos))))))))


(defun alb-org-entry-structure (text-pos past-pos)
  "Parse the entry structure between TEXT-POS and PAST-POS

Let TEXT-POS be the position of the first non-whitespace
character in a non-empty entry, and PAST-POS be a position after
the entry.  Define the *entry* as the text between the end of a
heading's meta-data, and the start of the next heading or the end
of buffer.  Define the *preamble* as the possibly empty text
before each top-level list in the entry.  Parse the entry into a
sequence of preamble-list pairs represented by an alist that maps
the position of the first non-whitespace character to the alist
returned by `org-list-struct`.  Note that an empty preamble is
identified by the fact that the position of the preamble is the
position of the top of the list.  This function customises
Org-Mode."
  (save-excursion (goto-char text-pos)
                  (beginning-of-line)
                  (alb-org-entry-structure-visit past-pos)))


(defun alb-org-visible-pos (pos)
  "Return POS only if it is visible

This function customises Org-Mode."
  (save-excursion (goto-char pos)
                  (and (not (outline-invisible-p))
                       (point))))


(defun alb-org-visible-preamble-pos (text-pos star-pos)
  "Return TEXT-POS only if its distinct from STAR-POS and visible

This function customises Org-Mode."
  (if (< text-pos star-pos)
      (save-excursion (goto-char text-pos)
                      (and (not (outline-invisible-p))
                           (point)))))


(defun alb-org-head-pos (curr-pos)
  "Return position of first non-whitespace character in the heading

Let CURR-POS be a position in the heading or its entry.  Return
the position of the start the enclosing heading.  This function
customises Org-Mode."
  (save-excursion (goto-char curr-pos)
                  (outline-back-to-heading t)
                  (point)))


(defun alb-org-text-pos (head-pos)
  "Return position of first non-whitespace character in the entry

Let HEAD-POS be the position of a heading.  Return the position
of the first non-whitespace character after the heading and
associated metadata.  This function customises Org-Mode."
  (save-excursion (goto-char head-pos)
                  (looking-at alb-re-org-heading)
                  (goto-char (match-end 0))
                  (looking-at alb-re-org-metadata)
                  (goto-char (match-end 0))
                  (looking-at "[[:space:]]*")
                  (match-end 0)))


(defun alb-org-past-pos (head-pos)
  "Return position of first non-whitespace character after the entry

Let HEAD-POS be the position of a heading.  Return the position
of the first non-whitespace character after the entry of the
heading.  This function customises Org-Mode."
  (save-excursion (goto-char head-pos)
                  (if (outline-next-heading)
                      (point)
                    (point-max))))


(defun alb-org-star-pos (item-pos)
  "Return position of bullet in list item

Let ITEM-POS be the start of the line of a list item.  Return the
position of bullet.  This function customises Org-Mode."
  (save-excursion (goto-char item-pos)
                  (looking-at org-list-full-item-re)
                  (match-beginning 1)))


(defun alb-org-backward-up-item-pos (curr-pos item-pos struct parents)
  "Find first visible ancestor of CURR-POS in STRUCT

Let CURR-POS be the position of point, ITEM-POS be the start of
an enclosing list item, STRUCT be the enclosing list structure as
returned by `org-list-struct`, and PARENTS be the alist returned
by `org-list-parents-alist`.  Recur on the parents of ITEM-POS
searching for a visible ancestor.  If found, return the position
of the bullet of the ancestor list item.  Do not move point.
This function customises Org-Mode."
  (if item-pos
      (let ((star-pos (alb-org-star-pos item-pos))
            (next-pos (org-list-get-parent item-pos struct parents)))
        (or (and (< star-pos curr-pos)
                 (alb-org-visible-pos star-pos))
            (alb-org-backward-up-item-pos curr-pos next-pos struct parents)))))


(defun alb-org-backward-up-text-pos (curr-pos entry-struct)
  "Find first visible ancestor of CURR-POS in ENTRY-STRUCT

Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  In
turn, visit the bullet of each enclosing list item, and the first
non-whitespace character of the immediately preceeding preamble
until a visible character is located.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (if entry-struct
      (let ((text-pos (caar entry-struct))
            (struct (cdar entry-struct)))
        (cond
         (struct
          (let ((star-pos (alb-org-star-pos (org-list-get-top-point struct)))
                (last-pos (org-list-get-bottom-point struct)))
            (cond
             ((and (< text-pos star-pos)
                   (< text-pos curr-pos)
                   (<= curr-pos star-pos))
              (alb-org-visible-pos text-pos))
             ((and (< star-pos curr-pos)
                   (<= curr-pos last-pos))
              (or (alb-org-backward-up-item-pos curr-pos (org-in-item-p) struct
                                                (org-list-parents-alist struct))
                  (alb-org-visible-preamble-pos text-pos star-pos)))
             ((< last-pos curr-pos)
              (alb-org-backward-up-text-pos curr-pos (cdr entry-struct))))))
         ((< text-pos curr-pos)
          (alb-org-visible-pos text-pos))))))


(defun alb-org-backward-up-head-pos (curr-pos head-pos)
  "Find first visible ancestor of CURR-POS

Let CURR-POS be the position of point, and HEAD-POS be the start
of the enclosing heading.  Assume HEAD-POS < CURR-POS.  In turn,
visit the bullet of each enclosing list item, the first
non-whitespace character of the immediately preceeding preamble,
and the first character in the heading until a visible character
is located.  If found, return the position.  Do not move point.
This function customises Org-Mode."
  (let ((text-pos (alb-org-text-pos head-pos))
        (past-pos (alb-org-past-pos head-pos)))
    (or (and (< text-pos curr-pos)
             (alb-org-backward-up-text-pos curr-pos (alb-org-entry-structure
                                                     text-pos past-pos)))
        (alb-org-visible-pos head-pos))))


(defun alb-org-backward-up-heading-pos (head-pos)
  "Find first visible ancestor heading of HEAD-POS

Let HEAD-POS be the start of the enclosing heading.  Search
upward for a visible ancestor heading.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (save-excursion (goto-char head-pos)
                  (org-up-heading-safe)
                  (and (outline-on-heading-p)
                       (point))))


(defun alb-org-backward-up-structure ()
  "Move backward out one level of Org-Mode structure

This function is intended to be the analogue of
`backward-up-list` for the case of navigating Org-Mode
structures.  It navigates headings and plain lists, with text
outside of a list being a special case.  Text preceeding a list
is treated as a list item (a preamble) of a virtual enclosing
list.  Hence, starting from a nested list item, successive calls
visit enclosing list items, then the preamble preceeding the
list, before reaching the heading.  This function customises
Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (next-pos (if (= curr-pos head-pos)
                       (alb-org-backward-up-heading-pos head-pos)
                     (or (alb-org-backward-up-head-pos curr-pos head-pos)
                         (alb-org-backward-up-heading-pos head-pos)))))
    (if next-pos
        (goto-char next-pos))))


(defun alb-org-down-item-pos (curr-pos item-pos struct parents)
  "Find first visible descendant of CURR-POS in STRUCT

Let CURR-POS be the position of point, ITEM-POS be the start of
an enclosing list item, STRUCT be the enclosing list structure as
returned by `org-list-struct`, and PARENTS be the alist returned
by `org-list-parents-alist`.  Test for a visible child of
ITEM-POS.  If found, return the position of the bullet of the
child list item.  Do not move point.  This function customises
Org-Mode."
  (if item-pos
      (let ((star-pos (alb-org-star-pos item-pos))
            (next-pos (car (org-list-get-children item-pos struct parents))))
        (if (< curr-pos star-pos)
            (alb-org-visible-pos star-pos)
          (alb-org-down-item-pos curr-pos next-pos struct parents)))))


(defun alb-org-down-text-pos (curr-pos entry-struct)
  "Find first visible descendant of CURR-POS in ENTRY-STRUCT

Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  If
CURR-POS falls before a preamble, stop at the first character of
the preamble.  If CURR-POS falls before a list, stop at the first
bullet.  If CURR-POS falls within a list item, stop at the first
bullet of any nested list.  If found and visible, return the
position.  Do not move point.  This function customises
Org-Mode."
  (if entry-struct
      (let ((text-pos (caar entry-struct))
            (struct (cdar entry-struct)))
        (cond
         ((< curr-pos text-pos)
          (alb-org-visible-pos text-pos))
         (struct
          (let ((star-pos (alb-org-star-pos (org-list-get-top-point struct)))
                (last-pos (org-list-get-bottom-point struct)))
            (cond
             ((< curr-pos star-pos)
              (alb-org-visible-pos star-pos))
             ((< curr-pos last-pos)
              (alb-org-down-item-pos curr-pos (org-in-item-p) struct
                                     (org-list-parents-alist struct)))
             (t
              (alb-org-down-text-pos curr-pos (cdr entry-struct))))))))))


(defun alb-org-down-head-pos (curr-pos head-pos)
  "Find first visible descendant heading of CURR-POS

Let CURR-POS be the position of point, and HEAD-POS be the start
of the enclosing heading.  Visit the next visible heading.  If
found at a deeper level than the current heading, return the
position.  Do not move point.  This function customises
Org-Mode."
  (let ((curr-level (org-current-level)))
    (save-excursion (outline-next-visible-heading 1)
                    (and (outline-on-heading-p)
                         (< curr-level (org-current-level))
                         (point)))))


(defun alb-org-down-structure ()
  "Move down one level of Org-Mode structure

This function is intended to be the analogue of `down-list` for
the case of navigating Org-Mode structures.  It navigates
headings and plain lists, with text outside of a list being a
special case.  Text preceeding a list is treated as a list
item (a preamble) of a virtual enclosing list.  Hence, starting
from a heading, successive calls visit the preamble preceeding
the list, before visiting nested list items.  This function
customises Org-Mode."
  (interactive)
  (if (outline-invisible-p)
      (alb-org-backward-up-structure))
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (text-pos (alb-org-text-pos head-pos))
         (past-pos (alb-org-past-pos head-pos))
         (entry-struct (alb-org-entry-structure text-pos past-pos))
         (next-pos (cond ((= text-pos past-pos)
                          (alb-org-down-head-pos curr-pos head-pos))
                         ((< curr-pos text-pos)
                          (or (alb-org-down-text-pos curr-pos entry-struct)
                              (alb-org-down-head-pos curr-pos head-pos)))
                         (t
                          (alb-org-down-text-pos curr-pos entry-struct)))))
    (if next-pos
        (goto-char next-pos))))


(defun alb-org-backward-item-pos (curr-pos item-pos struct prevs parents)
  "Find first visible predecessor of CURR-POS in STRUCT

XXX!  This function customises Org-Mode."
  (if item-pos
      (let ((star-pos (alb-org-star-pos item-pos))
            (next-item-pos (or (org-list-get-prev-item item-pos struct prevs)
                               (org-list-get-parent item-pos struct parents))))
        (or (and (< star-pos curr-pos)
                 (alb-org-visible-pos star-pos))
            (alb-org-backward-item-pos curr-pos next-item-pos
                                       struct prevs parents)))))


(defun alb-org-backward-text-pos (curr-pos entry-struct)
  "Find first visible predecessor of CURR-POS in ENTRY-STRUCT

XXX!  This function customises Org-Mode."
  (if entry-struct
      (let ((text-pos (caar entry-struct))
            (struct (cdar entry-struct)))
        (cond
         ((<= curr-pos text-pos)
          nil)
         (struct
          (let ((star-pos (alb-org-star-pos (org-list-get-top-point struct)))
                (last-pos (org-list-get-bottom-point struct)))
            (cond
             ((<= curr-pos star-pos)
              (alb-org-visible-preamble-pos text-pos star-pos))
             ((< curr-pos last-pos)
              (or (alb-org-backward-item-pos curr-pos (org-in-item-p) struct
                                             (org-list-prevs-alist struct)
                                             (org-list-parents-alist struct))
                  (alb-org-visible-preamble-pos text-pos star-pos)))
             (t
              (or (alb-org-backward-text-pos curr-pos (cdr entry-struct))
                  (alb-org-visible-pos (alb-org-star-pos
                                        (org-list-get-last-item
                                         (caar struct) struct
                                         (org-list-prevs-alist struct))))
                  (alb-org-visible-preamble-pos text-pos star-pos))))))
         (t
          (alb-org-visible-pos text-pos))))))


(defun alb-org-backward-head-pos (curr-pos head-pos)
  "Find first visible predecessor of CURR-POS

XXX!  Let CURR-POS be the position of point, and HEAD-POS be the
start of the enclosing heading.  Assume HEAD-POS < CURR-POS.  In
turn, visit the bullet of each enclosing list item, the first
non-whitespace character of the immediately preceeding preamble,
and the first character in the heading until a visible character
is located.  If found, return the position.  Do not move point.
This function customises Org-Mode."
  (let ((text-pos (alb-org-text-pos head-pos))
        (past-pos (alb-org-past-pos head-pos)))
    (cond
     ((<= curr-pos text-pos)
      (alb-org-visible-pos head-pos))
     ((< text-pos curr-pos)
      (or (alb-org-backward-text-pos curr-pos (alb-org-entry-structure
                                               text-pos past-pos))
          (alb-org-visible-pos head-pos))))))


(defun alb-org-backward-heading-pos (head-pos)
  "Find first visible predecessor heading of HEAD-POS

Let HEAD-POS be the position of the enclosing heading.  Search
backward for a visible predecessor heading.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (save-excursion (goto-char head-pos)
                  (org-backward-same-level 1)
                  (and (outline-on-heading-p)
                       (point))))


(defun alb-org-backward-structure ()
  "Move backward in the Org-Mode structure

This function is intended to be the analogue of `backward-sexp`
for the case of navigating Org-Mode structures.  It navigates
headings and plain lists, with text outside of a list being a
special case.  Text preceeding a list is treated as a list
item (a preamble) of a virtual enclosing list.  This function
customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (next-pos (if (= curr-pos head-pos)
                       (alb-org-backward-heading-pos head-pos)
                     (or (alb-org-backward-head-pos curr-pos head-pos)
                         (alb-org-backward-heading-pos head-pos)))))
    (if next-pos
        (goto-char next-pos))))


(defun alb-org-forward-preamble-visit (curr-pos entry-struct)
  "Find CURR-POS in a preamble and move to the next preamble

XXX  This function customises Org-Mode."
  (if entry-struct
      (let ((text-pos (caar entry-struct)))
        (if (< curr-pos text-pos)
            (goto-char text-pos)
          (alb-org-forward-preamble-visit curr-pos (cdr entry-struct))))
    (outline-next-visible-heading 1)))


(defun alb-org-forward-item-visit (curr-pos item-pos entry-struct)
  "Find CURR-POS in a list and move to the next item

XXX  This function customises Org-Mode."
  (if entry-struct
      (let ((struct (cdar entry-struct)))
        (if (assoc item-pos struct)
            (let* ((prevs (org-list-prevs-alist struct))
                   (star-pos (alb-org-star-pos item-pos))
                   (next-pos (org-list-get-next-item item-pos struct prevs)))
              (cond
               ((< curr-pos star-pos )
                (goto-char star-pos))
               (next-pos
                (goto-char next-pos)
                (looking-at "[[:space:]]*")
                (goto-char (match-end 0)))))
          (alb-org-forward-item-visit curr-pos item-pos (cdr entry-struct))))))


(defun alb-org-forward-structure ()
  "Move forward in the Org-Mode structure

This function is intended to be the analogue of `forward-sexp`
for the case of navigating Org-Mode structures.  It navigates
headings and plain lists, with text outside of a list being a
special case.  Text preceeding a list is treated as a list
item (a preamble) of a virtual enclosing list.  This function
customises Org-Mode."
  (interactive)
  (if (outline-invisible-p)
      (alb-org-forward-up-structure))
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (eofh-pos (save-excursion (goto-char head-pos)
                                   (looking-at alb-re-org-heading)
                                   (match-end 0)))
         (text-pos (alb-org-text-pos head-pos)))
    (cond
     ((< curr-pos eofh-pos)
      (org-forward-same-level 1))
     ((< curr-pos text-pos)
      (goto-char text-pos))
     (t
      (let* ((past-pos (alb-org-past-pos head-pos))
             (item-pos (org-in-item-p))
             (entry-struct (alb-org-entry-structure text-pos past-pos)))
        (if item-pos
            (alb-org-forward-item-visit curr-pos item-pos entry-struct)
          (alb-org-forward-preamble-visit curr-pos entry-struct)))))))


(defun alb-org-context-activity (tags)
  "Return the activity context embedded in the TAGS string

This function customises Org-Mode."
  (apply 'concat (mapcar (lambda (s) (if (string-match "^act_.*" s)
                                         (match-string 0 s)
                                       ""))
                         (split-string tags ":"))))


(defun alb-org-context-agenda (tags)
  "Return the agenda context embedded in the TAGS string

This function customises Org-Mode."
  (apply 'concat (mapcar (lambda (s) (if (string-match "^@.*" s)
                                         (match-string 0 s)
                                       ""))
                         (split-string tags ":"))))


(defun alb-org-agenda-cmp-context-activity (a b)
  "Compare the agenda entries A and B based on the activity context

This function customises Org-Mode."
  (let ((a-tag (if (string-match alb-re-org-heading a)
                   (alb-org-context-activity (match-string 5 a))
                 ""))
        (b-tag (if (string-match alb-re-org-heading b)
                   (alb-org-context-activity (match-string 5 b))
                 "")))
    (cond ((string< a-tag b-tag) -1)
          ((string< b-tag a-tag) 1))))


(defun alb-org-agenda-cmp-context-agenda (a b)
  "Compare the agenda entries A and B based on the agenda context

This function customises Org-Mode."
  (let ((a-tag (if (string-match alb-re-org-heading a)
                   (alb-org-context-agenda (match-string 5 a))
                 ""))
        (b-tag (if (string-match alb-re-org-heading b)
                   (alb-org-context-agenda (match-string 5 b))
                 "")))
    (cond ((string< a-tag b-tag) -1)
          ((string< b-tag a-tag) 1))))


(defun alb-org-sort-rank (properties)
  "Return the rank of a todo item from its PROPERTIES

The rank defines the first step in an approximated reverse
chronological order.  The rank gives the todo state an
interpretation in this order.  All =DUTY= and =HOLD= items are
newer than all =TODO= items, which are newer than all =NEXT=
items, which are newer than all =WAIT= items, which are newer
than all =DONE= and =STOP= items.  This function customises
Org-Mode."
  (let ((todo (cdr (assoc "TODO" properties))))
    (cond ((not todo)
           0)
          ((or (string= todo "DUTY") (string= todo "HOLD"))
           1)
          ((string= todo "TODO")
           2)
          ((string= todo "NEXT")
           3)
          ((string= todo "WAIT")
           4)
          ((or (string= todo "DONE") (string= todo "STOP"))
           5))))


(defun alb-org-sort-timestamp (properties)
  "Return the time stamp of a todo item from its PROPERTIES

The timestamp defines the second step in an approximated reverse
chronological order.  =DUTY= and =HOLD= items are timestamped by
effort, =TODO= and =NEXT= items are timestamped by deadline, and
timestamps for =WAIT=, =DONE=, and =STOP= items are drawn from
the logbook.  This function customises Org-Mode."
  (let ((todo (cdr (assoc "TODO" properties))))
    (cond ((not todo)
           '(0 0))
          ((or (string= todo "DUTY") (string= todo "HOLD"))
           (if (assoc "Effort" properties)
               (let ((effort (map 'list 'string-to-number
                                  (split-string
                                   (cdr (assoc "Effort" properties))
                                   ":"))))
                 (list 0 (+ (* 3600 (car effort)) (* 60 (cadr effort)))))
             '(0 0)))
          ((or (string= todo "TODO") (string= todo "NEXT")
               (string= todo "WAIT"))
           (if (assoc "DEADLINE" properties)
               (date-to-time (cdr (assoc "DEADLINE" properties)))
             '(0 0)))
          ((or (string= todo "DONE") (string= todo "STOP"))
           (if (assoc "TIMESTAMP_IA" properties)
               (date-to-time (cdr (assoc "TIMESTAMP_IA" properties)))
             '(0 0))))))


(defun alb-org-sort-string ()
  "Generate a string for sorting the entry at point

Returns a string that can be sorted alphabetically to retrieve
the lexical order over the rank of the todo state as defined by
`alb-org-sort-rank`, the reverse of the time of the todo item as
defined by `alb-org-sort-timestamp`, and the heading of the
entry.  This function customises Org-Mode."
  (let* ((properties (org-entry-properties))
         (rank (alb-org-sort-rank properties))
         (timestamp (alb-org-sort-timestamp properties))
         (sec-hi (- #xffff (car timestamp)))
         (sec-lo (- #xffff (cadr timestamp)))
         (title (nth 4 (org-heading-components))))
    (concat (format "%1d#%4x-%4x#%s" rank sec-hi sec-lo title))))


(defun alb-org-widen ()
  "Widen to the whole buffer and centre the headline"
  (interactive)
  (widen)
  (recenter))


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
             (match-string 2 value)))
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
