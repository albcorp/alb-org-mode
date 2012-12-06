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


(defconst alb-re-org-heading-and-ws
  (concat alb-re-org-heading
          alb-re-org-metadata
          "[[:space:]]*")
  "Regexp matching an Org-Mode heading, and following metadata
and whitespace with anchoring to the start of line.  It consumes
5 subexpressions as follows.

1. (optional) Asterisks
2. (optional) TODO keyword
3. (optional) Priority
4. (mandatory) Heading text including (optional) statistics and
   trailing whitespace
5. (optional) Tags")


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


(defun alb-org-entry-structure-visit (last-pos)
  "Recursively collect the structure below a heading

See `alb-org-entry-structure`."
  (let* ((text-pos (if (looking-at "[[:space:]]*")
                       (match-end 0)))
         (item-pos (if (re-search-forward (org-item-beginning-re) last-pos t)
                       (match-beginning 0))))
    (cond
     ((<= last-pos text-pos)
      nil)
     ((not item-pos)
      (list (cons text-pos nil)))
     (t
      (let ((struct (progn (goto-char item-pos)
                           (org-list-struct))))
        (cons (cons text-pos struct)
              (progn (goto-char (org-list-get-bottom-point struct))
                     (alb-org-entry-structure-visit last-pos))))))))


(defun alb-org-entry-structure (text-pos last-pos)
  "Collect the structure below a heading

Let TEXT-POS be the position of the first non-whitespace
character in a non-empty entry, and LAST-POS be a position after
the entry.  Define the *entry* as the text between the end of a
heading's meta-data, and the start of the next heading or end of
buffer.  Define the *preamble* as the possibly empty text before
each top-level list in the entry.  Parse the entry into a
sequence of preamble-list pairs represented by an alist that maps
the position of the first non-whitespace character to the alist
returned by `org-list-struct`.  Note that an empty preamble is
identified by the fact that the position of the preamble is the
position of the top of the list.  This function customises
Org-Mode."
  (save-excursion
    (goto-char text-pos)
    (beginning-of-line)
    (alb-org-entry-structure-visit last-pos)))


(defun alb-org-visible-pos (pos)
  "Return POS only if it is visible

This function customises Org-Mode."
  (save-excursion (goto-char pos)
                  (and (not (outline-invisible-p))
                       (point))))


(defun alb-org-backward-up-item-pos (curr-pos item-pos struct parents)
  "Find CURR-POS in a list and move backward and up

Let CURR-POS be the position of point, ITEM-POS be the start of
an enclosing list item, STRUCT be the enclosing list structure as
returned by `org-list-struct`, and PARENTS be the alist returned
by `org-list-parents-alist`.  Recur on the parents of ITEM-POS
searching for a visible ancestor.  If found, return the position
of the bullet of the ancestor list item.  Do not move point.
This function customises Org-Mode."
  (if item-pos
      (let ((star-pos (save-excursion (goto-char item-pos)
                                      (looking-at "[[:space:]]*")
                                      (match-end 0))))
        (if (< star-pos curr-pos)
            (or (alb-org-visible-pos star-pos)
                (alb-org-backward-up-item-pos
                 curr-pos (org-list-get-parent item-pos struct parents)
                 struct parents))
          (alb-org-backward-up-item-pos
           curr-pos (org-list-get-parent item-pos struct parents)
           struct parents)))))


(defun alb-org-backward-up-text-pos (curr-pos entry-struct)
  "Find CURR-POS in text following a heading and move backward and up

Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  In
turn, visit the bullet of each enclosing list item, and the first
non-whitespace character of the immediately preceeding preamble
until a visible character is located.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (if entry-struct
      (let ((text-pos (car (car entry-struct)))
            (struct (cdr (car entry-struct))))
        (cond
         (struct
          (let ((star-pos (save-excursion (goto-char
                                           (org-list-get-top-point struct))
                                          (looking-at "[[:space:]]*")
                                          (match-end 0)))
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
                  (and (< text-pos star-pos)
                       (alb-org-visible-pos text-pos))))
             ((< last-pos curr-pos)
              (alb-org-backward-up-text-pos curr-pos (cdr entry-struct))))))
         ((< text-pos curr-pos)
          (alb-org-visible-pos POS)
          (save-excursion (goto-char text-pos)
                          (and (not (outline-invisible-p))
                               (point))))))))


(defun alb-org-backward-up-head-pos (curr-pos head-pos)
  "Move backward out one level of Org-Mode structure

Let CURR-POS be the position of point, and HEAD-POS be the start
of the preceding heading.  In turn, visit the bullet of each
enclosing list item, the first non-whitespace character of the
immediately preceeding preamble, and the first character in the
heading until a visible character is located.  If found, return
the position.  Do not move point.  This function customises
Org-Mode."
  (let ((text-pos (save-excursion (goto-char head-pos)
                                  (looking-at alb-re-org-heading-and-ws)
                                  (match-end 0)))
        (last-pos (save-excursion (goto-char head-pos)
                                  (if (outline-next-heading)
                                      (point)
                                    (point-max)))))
    (if (<= curr-pos text-pos)
        (alb-org-visible-pos head-pos)
      (or (alb-org-backward-up-text-pos curr-pos (alb-org-entry-structure
                                                  text-pos last-pos))
          (alb-org-visible-pos head-pos)))))


(defun alb-org-backward-up-structure ()
  "Move backward out one level of Org-Mode structure

This function is intended to be the analogue of
`backward-up-list` for the case of navigating Org-Mode
structures.  It navigates headings and plain lists, with text
outside of a list being a special case.  Each sequence of
paragraphs outside a list is treated as a list item of a virtual
enclosing list.  Hence, starting from a nested list item,
successive calls visit enclosing list items, then the preamble
preceeding the list, before reaching the heading.  This function
customises Org-Mode."
  (interactive)
  (let ((curr-pos (point))
        (head-pos (save-excursion (outline-back-to-heading t)
                                  (point))))
    (if (= curr-pos head-pos)
        (org-up-heading-safe)
      (let ((next-pos (alb-org-backward-up-head-pos curr-pos head-pos)))
        (if next-pos
            (goto-char next-pos)
          (org-up-heading-safe))))))


(defun alb-org-down-item-pos (curr-pos item-pos struct parents)
  "Find CURR-POS in a list and move down

XXX Let CURR-POS be the position of point, ITEM-POS be the start
of an enclosing list item, STRUCT be the enclosing list structure
as returned by `org-list-struct`, and PARENTS be the alist
returned by `org-list-parents-alist`.  Recur on the parents of
ITEM-POS searching for a visible ancestor.  If found, return true
and leave point at the bullet of the ancestor list item.
Otherwise, return nil.  This function customises Org-Mode."
  (if item-pos
      (let ((star-pos (save-excursion (goto-char item-pos)
                                      (looking-at "[[:space:]]*")
                                      (match-end 0))))
        (if (< curr-pos star-pos)
            (alb-org-visible-pos star-pos)
          (alb-org-down-item-pos curr-pos
                                 (car (org-list-get-children item-pos struct
                                                             parents))
                                 struct parents)))))


(defun alb-org-down-text-pos (curr-pos entry-struct)
  "Find CURR-POS in text following a heading and move down

XXX Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  XXX In
turn, visit the bullet of each enclosing list item, and the first
non-whitespace character of the immediately preceeding preamble
until a visible character is located.  Return the visibility of
the final position.  This function customises Org-Mode."
  (if entry-struct
      (let ((text-pos (car (car entry-struct)))
            (struct (cdr (car entry-struct))))
        (cond
         ((< curr-pos text-pos)
          (alb-org-visible-pos text-pos))
         (struct
          (let ((star-pos (save-excursion
                            (goto-char (org-list-get-top-point struct))
                            (looking-at "[[:space:]]*")
                            (match-end 0)))
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
  "Move down one level of Org-Mode structure

XXX Returns true if able to find a spot.  Leaves point at the
spot and returns the position, or leaves poit unmoved and returns
nil.  Let HEAD-POS be the start of the preceding heading.  In
turn, visit the bullet of each enclosing list item, the first
non-whitespace character of the immediately preceeding preamble,
and the first character in the heading until a visible character
is located.  Return the visibility of the final position. This
function customises Org-Mode."
  (let ((curr-level (org-current-level)))
    (save-excursion
      (outline-next-visible-heading 1)
      (and (outline-on-heading-p)
           (< curr-level (org-current-level))
           (not (outline-invisible-p))
           (point)))))


(defun alb-org-down-structure ()
  "Move down one level of Org-Mode structure

This function is intended to be the analogue of `down-list` for
the case of navigating Org-Mode structures.  It navigates
headings and plain lists, with text outside of a list being a
special case.  Each sequence of paragraphs preceeding a top-level
list in the entry is treated as a list item of a virtual
enclosing list.  Hence, starting from a heading, successive calls
visit the preamble preceeding the list, before visiting nested
list items.  This function customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (save-excursion (outline-back-to-heading)
                                   (point)))
         (text-pos (save-excursion (goto-char head-pos)
                                   (looking-at alb-re-org-heading-and-ws)
                                   (match-end 0)))
         (last-pos (save-excursion (goto-char head-pos)
                                   (if (outline-next-heading)
                                       (point)
                                     (point-max))))
         (entry-struct (alb-org-entry-structure text-pos last-pos))
         (next-pos (cond ((outline-invisible-p)
                          (alb-org-down-head-pos curr-pos head-pos))
                         ((= text-pos last-pos)
                          (alb-org-down-head-pos curr-pos head-pos))
                         ((< curr-pos text-pos)
                          (or (alb-org-down-text-pos curr-pos entry-struct)
                              (alb-org-down-head-pos curr-pos head-pos)))
                         (t
                          (alb-org-down-text-pos curr-pos entry-struct)))))
    (if next-pos
        (goto-char next-pos))))


(defun alb-org-backward-preamble-visit (curr-pos entry-struct)
  "Find CURR-POS in a preamble and move to the previous preamble

XXX  This function customises Org-Mode."

  (let ((prev-text-pos (car (car entry-struct)))
        (tail-entry-struct (cdr entry-struct)))
    (if tail-entry-struct
        (let ((curr-text-pos (car (car tail-entry-struct))))
          (if (and (< prev-text-pos curr-pos) (<= curr-pos curr-text-pos))
              (goto-char prev-text-pos)
            (alb-org-backward-preamble-visit curr-pos tail-entry-struct)))
      (if (< prev-text-pos curr-pos)
          (goto-char prev-text-pos)
        (outline-back-to-heading)))))


(defun alb-org-backward-item-visit (curr-pos item-pos entry-struct)
  "Find CURR-POS in a list and move to the next item

XXX  This function customises Org-Mode."
  (if entry-struct
      (let ((struct (cdr (car entry-struct))))
        (if (assoc item-pos struct)
            (let* ((prevs (org-list-prevs-alist struct))
                   (star-pos (save-excursion (goto-char item-pos)
                                             (looking-at "[[:space:]]*")
                                             (match-end 0)))
                   (pred-pos (org-list-get-prev-item item-pos struct prevs)))
              (cond
               ((< star-pos curr-pos )
                (goto-char star-pos))
               (pred-pos
                (goto-char pred-pos)
                (looking-at "[[:space:]]*")
                (goto-char (match-end 0)))))
          (alb-org-backward-item-visit curr-pos item-pos (cdr entry-struct))))
    (outline-back-to-heading)))


(defun alb-org-backward-structure ()
  "Move backward in the Org-Mode structure

This function is intended to be the analogue of `backward-sexp`
for the case of navigating Org-Mode structures.  It navigates
headings and plain lists, with text outside of a list being a
special case.  Each sequence of paragraphs outside a list is
treated as a list item of a virtual enclosing list.  This
function customises Org-Mode."
  (interactive)
  (if (outline-invisible-p)
      (outline-back-to-heading)
    (let* ((curr-pos (point))
           (head-pos (save-excursion (outline-back-to-heading)
                                     (point)))
           (text-pos (save-excursion (goto-char head-pos)
                                     (looking-at alb-re-org-heading)
                                     (goto-char (match-end 0))
                                     (looking-at alb-re-org-metadata)
                                     (goto-char (match-end 0))
                                     (looking-at "[[:space:]]*")
                                     (match-end 0))))
      (cond
       ((= curr-pos head-pos)
        (org-backward-same-level 1))
       ((< curr-pos text-pos)
        (goto-char head-pos))
       (t
        (let* ((last-pos (save-excursion (goto-char head-pos)
                                         (if (outline-next-heading)
                                             (point)
                                           (point-max))))
               (item-pos (org-in-item-p))
               (entry-struct (alb-org-entry-structure text-pos last-pos)))
          (if item-pos
              (alb-org-backward-item-visit curr-pos item-pos entry-struct)
            (alb-org-backward-preamble-visit curr-pos entry-struct))))))))


(defun alb-org-forward-preamble-visit (curr-pos entry-struct)
  "Find CURR-POS in a preamble and move to the next preamble

XXX  This function customises Org-Mode."
  (if entry-struct
      (let ((text-pos (car (car entry-struct))))
        (if (< curr-pos text-pos)
            (goto-char text-pos)
          (alb-org-forward-preamble-visit curr-pos (cdr entry-struct))))
    (outline-next-visible-heading 1)))


(defun alb-org-forward-item-visit (curr-pos item-pos entry-struct)
  "Find CURR-POS in a list and move to the next item

XXX  This function customises Org-Mode."
  (if entry-struct
      (let ((struct (cdr (car entry-struct))))
        (if (assoc item-pos struct)
            (let* ((prevs (org-list-prevs-alist struct))
                   (star-pos (save-excursion (goto-char item-pos)
                                             (looking-at "[[:space:]]*")
                                             (match-end 0)))
                   (succ-pos (org-list-get-next-item item-pos struct prevs)))
              (cond
               ((< curr-pos star-pos )
                (goto-char star-pos))
               (succ-pos
                (goto-char succ-pos)
                (looking-at "[[:space:]]*")
                (goto-char (match-end 0)))))
          (alb-org-forward-item-visit curr-pos item-pos (cdr entry-struct))))))


(defun alb-org-forward-structure ()
  "Move forward in the Org-Mode structure

This function is intended to be the analogue of `forward-sexp`
for the case of navigating Org-Mode structures.  It navigates
headings and plain lists, with text outside of a list being a
special case.  Each sequence of paragraphs outside a list is
treated as a list item of a virtual enclosing list.  This
function customises Org-Mode."
  (interactive)
  (if (outline-invisible-p)
      (outline-next-visible-heading 1)
    (let* ((curr-pos (point))
           (head-pos (save-excursion (outline-back-to-heading)
                                     (point)))
           (eofh-pos (save-excursion (goto-char head-pos)
                                     (looking-at alb-re-org-heading)
                                     (match-end 0)))
           (text-pos (save-excursion (goto-char eofh-pos)
                                     (looking-at alb-re-org-metadata)
                                     (goto-char (match-end 0))
                                     (looking-at "[[:space:]]*")
                                     (match-end 0))))
      (cond
       ((< curr-pos eofh-pos)
        (org-forward-same-level 1))
       ((< curr-pos text-pos)
        (goto-char text-pos))
       (t
        (let* ((last-pos (save-excursion (goto-char head-pos)
                                         (if (outline-next-heading)
                                             (point)
                                           (point-max))))
               (item-pos (org-in-item-p))
               (entry-struct (alb-org-entry-structure text-pos last-pos)))
          (if item-pos
              (alb-org-forward-item-visit curr-pos item-pos entry-struct)
            (alb-org-forward-preamble-visit curr-pos entry-struct))))))))


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
