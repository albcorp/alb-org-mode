;;
;; alb-org-mode/alb-org-behavior.el
;;
;;     Copyright (C) 2010-2016 Andrew Lincoln Burrow
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
;;   - Functions to configure Org-Mode
;;

;;
;;
;; PROVIDED FEATURE
;; ---------------------------------------------------------------------
;;

(provide 'alb-org-behavior)

;;
;;
;; REQUIRED FEATURES
;; ---------------------------------------------------------------------
;;

(require 'org)

;;
;;
;; CONSTANT DECLARATIONS
;; ---------------------------------------------------------------------
;;

(defconst alb-org-heading-incoming
  "01-Think"
  "Title of the Org-Mode heading designated as the tree
containing incoming tasks.")

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
  "\\(DUTY\\|TODO\\|WAIT\\|HOLD\\|STOP\\|DONE\\)"
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

(defconst alb-re-org-heading-incoming
  (concat "^"
          "\\(\\*+\\)? *"
          alb-re-org-priority "? *"
          alb-org-heading-incoming " *"
          "\\(:[a-z:@_]+:\\)? *"
          "$")
  "Regexp matching the Org-Mode heading designated as the tree
containing incoming tasks.  The regexp is anchored to the start
and end of line, and consumes 3 subexpressions as follows.

1. (optional) Asterisks
2. (optional) Priority
3. (optional) Tags")

(defconst alb-re-org-metadata
  (concat "\\(?:\n[ \t]*"
          "\\(?:"
          "\\(?:DEADLINE\\|SCHEDULED\\):[ \t]+<" alb-re-org-date ">"
          "\\(?:[ \t]+\\(?:DEADLINE\\|SCHEDULED\\):[ \t]+<"
          alb-re-org-date ">\\)?"
          "\\|"
          ":\\(?:LOGBOOK\\|PROPERTIES\\):[ \t]*\\(?:\n.*\\)*?\n[ \t]*:END:"
          "\\)[ \t]*"
          "\\)*")
  "Regexp matching the possibly-empty properties metadata for a
heading. The match includes the newline terminating the headline,
but excludes the newline terminating the metadata.  It consumes 0
subexpressions.")

;;
;;
;; CUSTOMISATION
;; ---------------------------------------------------------------------
;;

(defgroup alb-org nil
  "Customise aspects of Org-Mode for albcorp"
  :group 'org)

(defcustom alb-org-abbrev-separtr-re
  "[-_~ \t\n\r,;]+"
  "Regexp used to split the string-to-abbreviate into words"
  :group 'alb-org
  :type 'regexp)

(defcustom alb-org-abbrev-illegal-re
  "[^-_a-zA-Z0-9+]"
  "Regexp used to recognise illegal characters in the string-to-abbreviate"
  :group 'alb-org
  :type 'regexp)

(defcustom alb-org-abbrev-ignore-words
  '("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to" "with")
  "List of words to be removed from the string-to-abbreviate"
  :group 'alb-org
  :type '(repeat string))

(defcustom alb-org-abbrev-words-max
  5
  "Maximum number of words to use"
  :group 'alb-org
  :type 'integer)

(defcustom alb-org-abbrev-chars-max
  30
  "Maximum number of characters in the abbreviated string"
  :group 'alb-org
  :type 'integer)

(defcustom alb-org-abbrev-separator
  "_"
  "String separating different words in the abbreviated string")

(defcustom alb-org-project-dirname-patterns
  '(("^Work on =\\([A-Z]+-[0-9]+\\)=: \\*\\([^*]+\\)\\*\\s-:foc_\\([^:]*\\):*.*$"
     . "~/Desktop/10-Files/\\3/\\1")
    ("^Work on =\\([a-z_-]+\\)= issue \\([0-9]+\\)\\: \\*\\([^*]+\\)\\*\\s-:foc_\\([^:]*\\):*.*$"
     . "~/Desktop/10-Files/\\4/\\1/\\2")
    ("^\\(.*\\)\\s-+:foc_\\([^:]*\\):.*$"
     . "~/Desktop/10-Files/\\2/<<\\1>>")
    ("^.*$"
     . "~/Desktop/10-Files"))
  "Pattern-replacement pairs to transform headings to project directory names

Each pair comprises a regular expression and a replacement
string.  These are passed to `replace-regexp-in-string`, and then
each string enclosed inside double angle brackets is transformed
using `alb-org-abbrev`.  The input text is the concatenation of
the heading without its TODO keywords and tags, a space, and all
inherited tags as returned by `org-entry-properties`.  The
patterns are tested in turn, and the first pattern that matches
the input is transformed into the project directory filename.
You should include a pair that matches all inputs, and outputs
the default directory."
  :group 'alb-org
  :type '(repeat (cons regexp string)))

;;
;;
;; FACE DECLARATIONS
;; ---------------------------------------------------------------------
;;

(defface alb-org-keyword-duty
  '((t :foreground "CornflowerBlue" :weight bold))
  "Face used for DUTY keyword."
  :group 'org-faces)

(defface alb-org-keyword-todo
  '((t :foreground "PaleGreen" :weight bold))
  "Face used for TODO keyword."
  :group 'org-faces)

(defface alb-org-keyword-wait
  '((t :foreground "LightGoldenrod" :weight bold))
  "Face used for WAIT keyword."
  :group 'org-faces)

(defface alb-org-keyword-hold
  '((t :foreground "grey" :weight bold))
  "Face used for HOLD keyword."
  :group 'org-faces)

(defface alb-org-keyword-stop
  '((t :foreground "grey30" :weight bold))
  "Face used for STOP keyword."
  :group 'org-faces)

(defface alb-org-keyword-done
  '((t :foreground "IndianRed" :weight bold))
  "Face used for DONE keyword."
  :group 'org-faces)

;;
;;
;; FUNCTION DEFINITIONS
;; ---------------------------------------------------------------------
;;

;;
;; Simplified key bindings
;;

(defun alb-org-key-bindings ()
  "Simplified key bindings for Org-Mode

These bindings attempt to realise simplified key bindings for
Org-Mode by realising the following principles.

* Employ the structure movement conventions of `paredit` namely,
  up, down, forward, and back
* Provide every key-binding in 2 forms a speed key, and an
  in-text binding such that the in-text binding is simply the
  speed key binding prefixed by C-c
* Discard access to unworkably complex concepts such as sparse
  trees.  Instead use helm, projectile, swoop, and the silver
  searcher"
 ;; Remove existing OrgMode key-bindings
 (define-key org-mode-map "\C-c\C-x" nil)
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
         ("p" . (org-speed-move-safe 'outline-previous-visible-heading))
         ("n" . (org-speed-move-safe 'outline-next-visible-heading))
         ("Outline structure editing")
         ("." . outline-mark-subtree)
         ("B" . org-move-subtree-up)
         ("F" . org-move-subtree-down)
         ("U" . (progn (org-promote) (outline-back-to-heading)))
         ("D" . (progn (org-demote) (outline-back-to-heading)))
         ("i" . alb-org-insert-heading-before)
         ("j" . alb-org-insert-heading-after)
         ("c" . org-capture)
         ("w" . org-refile)
         ("^" . (org-sort-entries nil ?f 'alb-org-sort-string))
         ("Clock commands")
         ("I" . org-clock-in)
         ("O" . org-clock-out)
         ("X" . org-clock-cancel)
         ("Meta-data editing")
         ("t" . org-todo)
         (":" . org-set-tags-command)
         ("S" . org-schedule)
         ("E" . org-set-effort)
         ("#" . alb-org-update-headline-statistics)
         ("l" . org-store-link)
         ("Project files")
         ("~" . alb-org-project-files)
         ("R" . alb-org-project-readme)
         ("Agenda views")
         ("v" . org-agenda)
         ("Exporting and publishing")
         ("e" . org-export-dispatch))))

;;
;; String functions
;;

(defun alb-org-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun alb-org-abbrev (input)
  "Convert INPUT (a sentence) to a suitable abbreviation.  In
particular, an abbreviation for the basename of a file.

See `alb-org-abbrev-separtr-re`, `alb-org-abbrev-illegal-re`,
`alb-org-abbrev-ignore-words`, `alb-org-abbrev-words-max`,
`alb-org-abbrev-chars-max`, and `alb-org-abbrev-separator` for
the customisable parameters.

Copied in spirit from \"reftex.el\"."
  (let ((init-words (split-string input alb-org-abbrev-separtr-re))
        final-words
        word
        (abbrev-re  (concat
                     "\\`\\("
                     (make-string 4 ?.) ; Minimum chars to include.
                     "[^aeiou]*"        ; Chars before abbrev point in word.
                     "\\)"
                     "[aeiou]"          ; Chars after  abbrev point in word.
                     (make-string 1 ?.) ; Minimum chars to exclude.
                     )))
    ;; Remove words from the ignore list or with funny characters.
    (while (setq word (pop init-words))
      (setq word (downcase word))
      (cond
       ((member word alb-org-abbrev-ignore-words)
        )
       ((string-match alb-org-abbrev-illegal-re word)
        (setq word (replace-match "" nil nil word))
        (while (string-match alb-org-abbrev-illegal-re word)
          (setq word (replace-match "" nil nil word)))
        (push word final-words))
       (t
        (push word final-words))))
    (setq final-words (nreverse final-words))

    ;; Restrict number of words.
    (if (> (length final-words) alb-org-abbrev-words-max)
        (setcdr (nthcdr (1- alb-org-abbrev-words-max) final-words) nil))

    ;; Abbreviate words.
    (setq final-words
          (mapcar
           (function
            (lambda (w) (if (string-match abbrev-re w)
                            (match-string 1 w)
                          w)))
           final-words))

    ;; Construct string with alb-org-abbrev-separators.
    (setq input
          (mapconcat 'identity final-words alb-org-abbrev-separator))

    ;; Shorten if still too long.
    (setq input
          (if (> (length input) alb-org-abbrev-chars-max)
              (substring input 0 alb-org-abbrev-chars-max)
            input))))

;;
;; Whitespace cleanup
;;

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

;;
;; Structure navigation
;;

(defun alb-org-entry-structure-visit (past-pos)
  "Recursively collect the entry structure upto PAST-POS

Collect the preamble and list at point and advance point to the
end of the list.  Recur on the space remaining before PAST-POS.
See `alb-org-entry-structure` for further details."
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

Define an *entry* as the text between the end of a heading's
meta-data, and the start of the next heading or the end of
buffer.  Define a *preamble* as the possibly empty text before
each top-level list in the entry.  Let TEXT-POS be the position
of the first non-whitespace character in an entry, and PAST-POS
be the first position after the entry.  Parse the entry into a
sequence of preamble-list pairs represented by an alist that maps
the position of the first non-whitespace character to the alist
returned by `org-list-struct`.  An preamble is empty when the
position of the preamble is the position of the top of the list.
This function customises Org-Mode."
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

(defun alb-org-up-item-pos (curr-pos item-pos struct parents)
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
            (next-item-pos (org-list-get-parent item-pos struct parents)))
        (or (and (< star-pos curr-pos)
                 (alb-org-visible-pos star-pos))
            (alb-org-up-item-pos curr-pos next-item-pos struct parents)))))

(defun alb-org-up-text-pos (curr-pos entry-struct)
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
         ((<= curr-pos text-pos)
          nil)
         (struct
          (let ((star-pos (alb-org-star-pos (org-list-get-top-point struct)))
                (past-pos (org-list-get-bottom-point struct)))
            (cond
             ((<= curr-pos star-pos)
              (alb-org-visible-preamble-pos text-pos star-pos))
             ((<= curr-pos past-pos)
              (or (alb-org-up-item-pos curr-pos (org-in-item-p) struct
                                       (org-list-parents-alist struct))
                  (alb-org-visible-preamble-pos text-pos star-pos)))
             (t
              (alb-org-up-text-pos curr-pos (cdr entry-struct))))))
         (t
          (alb-org-visible-pos text-pos))))))

(defun alb-org-up-head-pos (curr-pos head-pos)
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
             (alb-org-up-text-pos curr-pos (alb-org-entry-structure text-pos
                                                                    past-pos)))
        (alb-org-visible-pos head-pos))))

(defun alb-org-up-heading-pos (head-pos)
  "Find first visible ancestor heading of HEAD-POS

Let HEAD-POS be the start of the enclosing heading.  Search
upward for a visible ancestor heading.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (save-excursion (goto-char head-pos)
                  (org-up-heading-safe)
                  (and (outline-on-heading-p)
                       (point))))

(defun alb-org-up-structure ()
  "Move backward out one level of Org-Mode structure

Search backward for the first visible ancestor list item,
preamble, or heading.  Place point at the bullet of a list item,
or first character of a preamble or heading.  This function is
intended to be the analogue of `backward-up-list` for the case of
navigating Org-Mode structures.  It navigates headings and plain
lists, with text outside of a list being a special case.  Text
preceeding a list is treated as a list item (a preamble) of a
virtual enclosing list.  This function customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (next-pos (if (= curr-pos head-pos)
                       (alb-org-up-heading-pos head-pos)
                     (or (alb-org-up-head-pos curr-pos head-pos)
                         (alb-org-up-heading-pos head-pos)))))
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
            (next-item-pos (car (org-list-get-children item-pos struct
                                                       parents))))
        (if (< curr-pos star-pos)
            (alb-org-visible-pos star-pos)
          (alb-org-down-item-pos curr-pos next-item-pos struct parents)))))

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
                (past-pos (org-list-get-bottom-point struct)))
            (cond
             ((< curr-pos star-pos)
              (alb-org-visible-pos star-pos))
             ((< curr-pos past-pos)
              (alb-org-down-item-pos curr-pos (org-in-item-p) struct
                                     (org-list-parents-alist struct)))
             (t
              (alb-org-down-text-pos curr-pos (cdr entry-struct))))))))))

(defun alb-org-down-heading-pos (head-pos)
  "Find first visible descendant heading of HEAD-POS

Let HEAD-POS be the start of the enclosing heading.  Visit the
next visible heading.  If found at a deeper level than the
current heading, return the position.  Do not move point.  This
function customises Org-Mode."
  (save-excursion (goto-char head-pos)
                  (let ((curr-level (org-current-level)))
                    (outline-next-visible-heading 1)
                    (and (outline-on-heading-p)
                         (< curr-level (org-current-level))
                         (point)))))

(defun alb-org-down-structure ()
  "Move down one level of Org-Mode structure

Search forward for the first visible descendant list item,
preamble, or heading.  Place point at the bullet of a list item,
or first character of a preamble or heading.  This function is
intended to be the analogue of `down-list` for the case of
navigating Org-Mode structures.  It navigates headings and plain
lists, with text outside of a list being a special case.  Text
preceeding a list is treated as a list item (a preamble) of a
virtual enclosing list.  This function customises Org-Mode."
  (interactive)
  (if (outline-invisible-p)
      (alb-org-up-structure))
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (text-pos (alb-org-text-pos head-pos))
         (past-pos (alb-org-past-pos head-pos))
         (entry-struct (alb-org-entry-structure text-pos past-pos))
         (next-pos (cond ((= text-pos past-pos)
                          (alb-org-down-heading-pos head-pos))
                         ((< curr-pos text-pos)
                          (or (alb-org-down-text-pos curr-pos entry-struct)
                              (alb-org-down-heading-pos head-pos)))
                         (t
                          (alb-org-down-text-pos curr-pos entry-struct)))))
    (if next-pos
        (goto-char next-pos))))

(defun alb-org-backward-item-pos (curr-pos item-pos struct prevs parents)
  "Find first visible predecessor of CURR-POS in STRUCT

Let CURR-POS be the position of point, ITEM-POS be the start of
an enclosing list item, STRUCT be the enclosing list structure as
returned by `org-list-struct`, PREVS be the alist returned by
`org-list-prevs-alist`, and PARENTS be the alist returned by
`org-list-parents-alist`.  Recur on the preceding siblings and
parent of ITEM-POS searching for a visible predecessor.  If
found, return the position of the bullet of the predecessor list
item.  Do not move point.  This function customises Org-Mode."
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

Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  In
turn, visit the bullet of each preceding list item, and the first
non-whitespace character of the immediately preceeding preamble
until a visible character is located.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (if entry-struct
      (let ((text-pos (caar entry-struct))
            (struct (cdar entry-struct)))
        (cond
         ((<= curr-pos text-pos)
          nil)
         (struct
          (let ((star-pos (alb-org-star-pos (org-list-get-top-point struct)))
                (past-pos (org-list-get-bottom-point struct)))
            (cond
             ((<= curr-pos star-pos)
              (alb-org-visible-preamble-pos text-pos star-pos))
             ((<= curr-pos past-pos)
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

Let CURR-POS be the position of point, and HEAD-POS be the start
of the enclosing heading.  Assume HEAD-POS < CURR-POS.  In turn,
visit the bullet of each preceding list item, the first
non-whitespace character of the immediately preceeding preamble,
and the first character in the heading until a visible character
is located.  If found, return the position.  Do not move point.
This function customises Org-Mode."
  (let ((text-pos (alb-org-text-pos head-pos))
        (past-pos (alb-org-past-pos head-pos)))
    (or (and (< text-pos curr-pos)
             (alb-org-backward-text-pos curr-pos (alb-org-entry-structure
                                                  text-pos past-pos)))
        (alb-org-visible-pos head-pos))))

(defun alb-org-backward-heading-pos (head-pos)
  "Find first visible predecessor heading of HEAD-POS

Let HEAD-POS be the position of the enclosing heading.  Search
backward for a visible predecessor heading.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (save-excursion (goto-char head-pos)
                  (org-backward-heading-same-level 1)
                  (and (outline-on-heading-p)
                       (point))))

(defun alb-org-backward-structure ()
  "Move backward in the Org-Mode structure

Search backward for the first visible predecessor list item,
preamble, or heading at the same level in the structure.  Place
point at the bullet of a list item, or first character of a
preamble or heading.  This function is intended to be the
analogue of `backward-sexp` for the case of navigating Org-Mode
structures.  It navigates headings and plain lists, with text
outside of a list being a special case.  Text preceeding a list
is treated as a list item (a preamble) of a virtual enclosing
list.  This function customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (next-pos (if (= curr-pos head-pos)
                       (alb-org-backward-heading-pos head-pos)
                     (or (alb-org-backward-head-pos curr-pos head-pos)
                         (alb-org-backward-heading-pos head-pos)))))
    (if next-pos
        (goto-char next-pos))))

(defun alb-org-forward-item-pos (curr-pos item-pos struct prevs parents)
  "Find first visible successor of CURR-POS in STRUCT

Let CURR-POS be the position of point, ITEM-POS be the start of
an enclosing list item, STRUCT be the enclosing list structure as
returned by `org-list-struct`, PREVS be the alist returned by
`org-list-prevs-alist`, and PARENTS be the alist returned by
`org-list-parents-alist`.  Recur on the succeeding siblings and
suceeding sibling of the parent of ITEM-POS searching for a
visible successor.  If found, return the position of the bullet
of the ancestor list item.  Do not move point.  This function
customises Org-Mode."
  (if item-pos
      (let ((star-pos (alb-org-star-pos item-pos))
            (next-item-pos (org-list-get-next-item item-pos struct prevs))
            (parent-item-pos (org-list-get-parent item-pos struct parents)))
        (or (and (< curr-pos star-pos)
                 (alb-org-visible-pos star-pos))
            (alb-org-forward-item-pos curr-pos next-item-pos
                                      struct prevs parents)
            (alb-org-forward-item-pos curr-pos parent-item-pos
                                      struct prevs parents)))))

(defun alb-org-forward-text-pos (curr-pos entry-struct)
  "Find first visible successor of CURR-POS in ENTRY-STRUCT

Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  In
turn, visit the bullet of each succeeding list item, the first
non-whitespace character of the immediately suceeding preamble, and the
until a visible character is located.  If found, return the
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
                (past-pos (org-list-get-bottom-point struct)))
            (cond
             ((< curr-pos star-pos )
              (or (alb-org-visible-pos star-pos)
                  (alb-org-forward-text-pos curr-pos (cdr entry-struct))))
             ((< curr-pos past-pos)
              (or (alb-org-forward-item-pos curr-pos (org-in-item-p) struct
                                            (org-list-prevs-alist struct)
                                            (org-list-parents-alist struct))
                  (alb-org-forward-text-pos curr-pos (cdr entry-struct))))
             (t
              (alb-org-forward-text-pos curr-pos (cdr entry-struct))))))))))

(defun alb-org-forward-head-pos (curr-pos head-pos)
  "Find first visible successor of CURR-POS

Let CURR-POS be the position of point, and HEAD-POS be the start
of the enclosing heading.  Assume HEAD-POS < CURR-POS.  In turn,
visit the bullet of each suceeding list item, the first
non-whitespace character of the immediately succeeding preamble,
and the first character outside of the entry.  If found, return
the position.  Do not move point.  This function customises
Org-Mode."
  (let ((text-pos (alb-org-text-pos head-pos))
        (past-pos (alb-org-past-pos head-pos)))
    (if (< curr-pos text-pos)
        (alb-org-visible-pos text-pos)
      (or (alb-org-forward-text-pos curr-pos (alb-org-entry-structure
                                              text-pos past-pos))
          (alb-org-visible-pos past-pos)))))

(defun alb-org-forward-heading-pos (head-pos)
  "Find first visible successor heading of HEAD-POS

Let HEAD-POS be the position of the enclosing heading.  Search
forward for a visible successor heading.  If found, return the
position.  Do not move point.  This function customises
Org-Mode."
  (save-excursion (goto-char head-pos)
                  (org-forward-heading-same-level 1)
                  (and (outline-on-heading-p)
                       (point))))

(defun alb-org-forward-structure ()
  "Move forward in the Org-Mode structure

Search forward for the first visible successor list item,
preamble, or heading at the same level in the structure.  Place
point at the bullet of a list item, or first character of a
preamble or heading.  This function is intended to be the
analogue of `forward-sexp` for the case of navigating Org-Mode
structures.  It navigates headings and plain lists, with text
outside of a list being a special case.  Text preceeding a list
is treated as a list item (a preamble) of a virtual enclosing
list.  This function customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (next-pos (if (= curr-pos head-pos)
                       (alb-org-forward-heading-pos head-pos)
                     (or (alb-org-forward-head-pos curr-pos head-pos)
                         (alb-org-forward-heading-pos head-pos)))))
    (if next-pos
      (goto-char next-pos))))

(defun alb-org-prev-item-pos (curr-pos item-pos struct prevs parents)
  "Find first visible list item before CURR-POS in STRUCT

Let CURR-POS be the position of point, ITEM-POS be the start of a
list item, STRUCT be the enclosing list structure as returned by
`org-list-struct`, PREVS be the alist returned by
`org-list-prevs-alist`, and PARENTS be the alist returned by
`org-list-parents-alist`.  Recur on the succeeding sibling and
first child of ITEM-POS, searching for the visible list item that
is the closest predecessor of CURR-POS.  If found, return the
position of the bullet of the list item.  Do not move point.
This function customises Org-Mode."
  (if item-pos
      (let ((star-pos (alb-org-star-pos item-pos))
            (past-pos (org-list-get-item-end item-pos struct))
            (sibling-pos (org-list-get-next-item item-pos struct prevs))
            (child-pos (car (org-list-get-children item-pos struct parents))))
        (or (and (< past-pos curr-pos)
                 (alb-org-prev-item-pos curr-pos sibling-pos struct
                                        prevs parents))
            (alb-org-prev-item-pos curr-pos child-pos struct
                                   prevs parents)
            (and (< star-pos curr-pos)
                 (alb-org-visible-pos star-pos))))))

(defun alb-org-prev-text-pos (curr-pos entry-struct)
  "Find first visible preamble or list item before CURR-POS in ENTRY-STRUCT

Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  Search
backward from CURR-POS: visit the bullet of each list item, and
the first non-whitespace character of each preamble.  If a
visible position is found, return it.  Do not move point.  This
function customises Org-Mode."
  (if entry-struct
      (let ((text-pos (caar entry-struct))
            (struct (cdar entry-struct)))
        (if struct
            (let ((item-pos (org-list-get-top-point struct))
                  (past-pos (org-list-get-bottom-point struct)))
              (or (and (< past-pos curr-pos)
                       (alb-org-prev-text-pos curr-pos (cdr entry-struct)))
                  (alb-org-prev-item-pos curr-pos item-pos struct
                                         (org-list-prevs-alist struct)
                                         (org-list-parents-alist struct))
                  (and (< text-pos curr-pos)
                       (alb-org-visible-pos text-pos))))
          (and (< text-pos curr-pos)
               (alb-org-visible-pos text-pos))))))

(defun alb-org-prev-head-pos (curr-pos head-pos)
  "Find first visible preamble or list item before CURR-POS

Let CURR-POS be the position of point, and HEAD-POS be the start
of the enclosing heading.  Assume HEAD-POS < CURR-POS.  Search
backward from CURR-POS: visit the bullet of each list item, the
first non-whitespace character of each preamble, and the first
character of the heading.  If a visible position is found, return
it.  Do not move point.  This function customises Org-Mode."
  (let ((text-pos (alb-org-text-pos head-pos))
        (past-pos (alb-org-past-pos head-pos)))
    (or (and (< text-pos curr-pos )
             (alb-org-prev-text-pos curr-pos
                                    (alb-org-entry-structure text-pos
                                                             past-pos)))
        (alb-org-visible-pos head-pos))))

(defun alb-org-prev-heading-pos (head-pos)
  "Find first visible heading before HEAD-POS

Let HEAD-POS be the position of the enclosing heading.  Search
backward for a visible heading.  If found, return the position.
Do not move point.  This function customises Org-Mode."
  (save-excursion (goto-char head-pos)
                  (outline-previous-visible-heading 1)
                  (and (outline-on-heading-p)
                       (point))))

(defun alb-org-prev-structure ()
  "Move to the previous element in the Org-Mode structure

Search backward for the first visible list item, preamble, or
heading regardless of level in the structure.  Place point at the
bullet of a list item, or first character of a preamble or
heading.  This function navigates headings and plain lists, with
text outside of a list being a special case.  Text preceeding a
list is treated as a list item (a preamble) of a virtual
enclosing list.  This function customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (prev-head-pos (alb-org-prev-heading-pos head-pos))
         (next-pos (or (and (< head-pos curr-pos)
                            (alb-org-prev-head-pos curr-pos head-pos))
                       (and prev-head-pos
                            (alb-org-prev-head-pos curr-pos prev-head-pos)))))
    (if next-pos
        (goto-char next-pos))))

(defun alb-org-next-item-pos (curr-pos item-pos struct prevs parents)
  "Find first visible list item after CURR-POS in STRUCT

Let CURR-POS be the position of point, ITEM-POS be the start of a
list item, STRUCT be the enclosing list structure as returned by
`org-list-struct`, PREVS be the alist returned by
`org-list-prevs-alist`, and PARENTS be the alist returned by
`org-list-parents-alist`.  Recur on the first child and
succeeding sibling of ITEM-POS searching for the visible list
item that is the closest succesor of CURR-POS.  If found, return
the position of the bullet of the list item.  Do not move point.
This function customises Org-Mode."
  (if item-pos
      (let ((star-pos (alb-org-star-pos item-pos))
            (child-pos (car (org-list-get-children item-pos struct parents)))
            (sibling-pos (org-list-get-next-item item-pos struct prevs)))
        (or (and (< curr-pos star-pos)
                 (alb-org-visible-pos star-pos))
            (alb-org-next-item-pos curr-pos child-pos struct
                                   prevs parents)
            (alb-org-next-item-pos curr-pos sibling-pos struct
                                   prevs parents)))))

(defun alb-org-next-text-pos (curr-pos entry-struct)
  "Find first visible preamble or list iteam after CURR-POS in ENTRY-STRUCT

Let CURR-POS be the position of point, and ENTRY-STRUCT be the
alist returned by `alb-org-entry-structure`.  Recur on
ENTRY-STRUCT searching for a containing preamble or list.  Search
forward from CURR-POS: visit the bullet of each list item, and
the first non-whitespace character of each preamble.  If a
visible position is found, return it.  Do not move point.  This
function customises Org-Mode."
  (if entry-struct
      (let ((text-pos (caar entry-struct))
            (struct (cdar entry-struct)))
        (if struct
            (let ((item-pos (org-list-get-top-point struct))
                  (past-pos (org-list-get-bottom-point struct)))
              (or (and (< curr-pos text-pos)
                       (alb-org-visible-pos text-pos))
                  (and (< curr-pos past-pos)
                       (alb-org-next-item-pos curr-pos item-pos struct
                                              (org-list-prevs-alist struct)
                                              (org-list-parents-alist struct)))
                  (alb-org-next-text-pos curr-pos (cdr entry-struct))))
          (and (< curr-pos text-pos)
               (alb-org-visible-pos text-pos))))))

(defun alb-org-next-head-pos (curr-pos head-pos)
  "Find first visible preamble or list item after CURR-POS

Let CURR-POS be the position of point, and HEAD-POS be the start
of the enclosing heading.  Assume HEAD-POS < CURR-POS.  Search
forward from CURR-POS: visit the bullet of each list item, the
first non-whitespace character of each preamble, and the first
character outside of the entry.  If a visible position is found,
return it.  Do not move point.  This function customises
Org-Mode."
  (let ((text-pos (alb-org-text-pos head-pos))
        (past-pos (alb-org-past-pos head-pos)))
    (if (< curr-pos text-pos)
        (alb-org-visible-pos text-pos)
      (or (alb-org-next-text-pos curr-pos
                                 (alb-org-entry-structure text-pos past-pos))
          (alb-org-visible-pos past-pos)))))

(defun alb-org-next-heading-pos (head-pos)
  "Find first visible heading after HEAD-POS

Let HEAD-POS be the position of the enclosing heading.  Search
forward for a visible heading.  If found, return the position.
Do not move point.  This function customises Org-Mode."
  (save-excursion (goto-char head-pos)
                  (outline-next-visible-heading 1)
                  (and (outline-on-heading-p)
                       (point))))

(defun alb-org-next-structure ()
  "Move to the next element in the Org-Mode structure

Search forward for the first visible list item, preamble, or
heading regardless of level in the structure.  Place point at the
bullet of a list item, or first character of a preamble or
heading.  This function navigates headings and plain lists, with
text outside of a list being a special case.  Text preceeding a
list is treated as a list item (a preamble) of a virtual
enclosing list.  This function customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (next-pos (or (alb-org-next-head-pos curr-pos head-pos)
                       (alb-org-next-heading-pos head-pos))))
    (if next-pos
      (goto-char next-pos))))

;;
;; Heading navigation
;;

(defun alb-org-up-heading ()
  "Move to the parent heading

Search backward for the first visible ancestor heading.  Place
point at the first character of the located heading.  This
function customises Org-Mode."
  (interactive)
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (text-pos (alb-org-text-pos head-pos))
         (next-pos (if (= curr-pos head-pos)
                       (alb-org-up-heading-pos head-pos)
                     (or (alb-org-visible-pos head-pos)
                         (alb-org-up-heading-pos head-pos)))))
    (if next-pos
        (goto-char next-pos))))

(defun alb-org-down-heading ()
  "Move to the first child heading

Search forward for the first visible descendant heading.  Place
point at the first character of the located heading.  This
function customises Org-Mode."
  (interactive)
  (if (outline-invisible-p)
      (alb-org-up-structure))
  (let* ((curr-pos (point))
         (head-pos (alb-org-head-pos curr-pos))
         (next-pos (alb-org-down-heading-pos head-pos)))
    (if next-pos
        (goto-char next-pos))))

;;
;; Column view customization
;;

(defun alb-org-columns-modify-value-for-display-function (column-title value)
  "Modify values for display in column view

The mappings are designed to make schedules, effort estimates,
and elapsed time easier to follow in column view.

- when COLUMN-TITLE is =Task= remove tags from =ITEM= value; and
- when COLUMN-TITLE is =Project= remove tags from =ITEM= value."
  (cond ((string= column-title "Project")
         (if (string-match alb-re-org-heading value)
             (concat (match-string 1 value) " " (match-string 4 value))))
        ((string= column-title "Task")
         (if (string-match alb-re-org-heading value)
             (concat (match-string 1 value) " " (match-string 4 value))))
        ((string= column-title "X")
         (cond ((string= value "DUTY") "u")
               ((string= value "TODO") "t")
               ((string= value "WAIT") "w")
               ((string= value "HOLD") "h")
               ((string= value "STOP") "S")
               ((string= value "DONE") "D")))))

;;
;; Agenda customization
;;

(defun alb-org-agenda-cmp-first (a b)
  "Compare a pair of formatted agenda entries after splitting off the first word

Split the formatted agenda items `a` and `b` and compare the
first word from each.  This function customises Org-Mode."
  (let ((a-first (nth 0 (split-string a)))
        (b-first (nth 0 (split-string b))))
      (cond ((string< a-first b-first)
          -1)
         ((string< b-first a-first)
          1))))

(defun alb-org-agenda-prefix-activity ()
  "Construct a prefix for an agenda from the activity tag

This function customises Org-Mode."
  (let* ((props (org-entry-properties))
         (tags (cdr (assoc "ALLTAGS" props))))
    (if (string-match ":\\(act_[^:]*\\):" tags)
        (match-string-no-properties 1 tags))))

(defun alb-org-agenda-prefix-context ()
  "Construct a prefix for an agenda from the context tag

This function customises Org-Mode."
  (let* ((props (org-entry-properties))
         (tags (cdr (assoc "ALLTAGS" props))))
    (if (string-match ":\\(@[^:]*\\):" tags)
        (match-string-no-properties 1 tags))))

(defun alb-org-agenda-prefix-schedule ()
  "Construct a prefix for an agenda from the SCHEDULED value

This function customises Org-Mode."
  (let* ((props (org-entry-properties))
         (scheduled (cdr (assoc "SCHEDULED" props))))
    (if (stringp scheduled)
        scheduled
      "")))

(defun alb-org-agenda-prefix-timestamp ()
  "Construct a prefix for an agenda from the last timestamp

This function customises Org-Mode."
  (let* ((props (org-entry-properties))
         (timestamp (cdr (assoc "TIMESTAMP_IA" props))))
    (if (stringp timestamp)
        timestamp
      "")))

(defun alb-org-agenda-prefix-catalog ()
  "Construct a prefix for each entry in the catalog of areas of focus

This function customises Org-Mode."
  (let ((level (org-current-level)))
    (cond ((= level 1)
           "\n\n")
          ((= level 2)
           "\n")
          ((= level 3)
           "[ ] "))))

;;
;; Capture customization
;;

(defun alb-org-locate-incoming ()
  "Place point at tree containing incoming tasks

This function customises Org-Mode."
  (goto-char (point-min))
  (if (re-search-forward alb-re-org-heading-incoming nil t)
      (goto-char (match-beginning 0))
    (outline-next-heading)))

(defun alb-org-locate-heading ()
  "Place point at enclosing heading, or top heading in the file

This function customises Org-Mode."
  (if (org-before-first-heading-p)
      (outline-next-heading)
    (outline-back-to-heading t)))

(defun alb-org-locate-focus-sentinel ()
  "Place point at focus sentinel

Insert area of focus heading at start of list of areas of focus.
Therefore, places point at first tree after the incoming tree.
This function customises Org-Mode."
  (alb-org-locate-incoming)
  (outline-forward-same-level 1))

(defun alb-org-locate-duty-sentinel ()
  "Place point at DUTY sentinel

Insert TODO heading in ``DUTY`` before first ``DUTY`` at start of
enclosing area of focus heading, or in the incoming tasks tree.
Therefore, places point at first child heading of the enclosing
level 2 heading.  This function customises Org-Mode."
  (alb-org-locate-heading)
  (while (< 2 (org-current-level))
    (outline-up-heading 1 t))
  (if (not (= 2 (org-current-level)))
      (alb-org-locate-incoming))
  (outline-next-heading))

(defun alb-org-locate-hold-sentinel ()
  "Place point at TODO sentinel

Insert TODO heading in ``TODO`` before first ``HOLD`` state
heading in enclosing area of focus heading, or in the incoming
tasks tree.  Therefore, places point at first child heading of
the enclosing level 2 heading that is not in the ``DUTY`` state.
This function customises Org-Mode."
  (alb-org-locate-heading)
  (while (< 2 (org-current-level))
    (outline-up-heading 1 t))
  (if (not (= 2 (org-current-level)))
      (alb-org-locate-incoming))
  (outline-next-heading)
  (while (and (= 3 (org-current-level))
              (member (cdr (assoc "TODO" (org-entry-properties)))
                      '("DUTY")))
    (outline-forward-same-level 1)))

(defun alb-org-locate-todo-sentinel ()
  "Place point at TODO sentinel

Insert TODO heading in ``TODO`` before first ``TODO`` state
heading in enclosing area of focus heading, or in the incoming
tasks tree.  Therefore, places point at first child heading of
the enclosing level 2 heading that is in neither a ``DUTY`` or
``HOLD`` state.  This function customises Org-Mode."
  (alb-org-locate-heading)
  (while (< 2 (org-current-level))
    (outline-up-heading 1 t))
  (if (not (= 2 (org-current-level)))
      (alb-org-locate-incoming))
  (outline-next-heading)
  (while (and (= 3 (org-current-level))
              (member (cdr (assoc "TODO" (org-entry-properties)))
                      '("DUTY" "HOLD")))
    (outline-forward-same-level 1)))

(defun alb-org-locate-link-sentinel ()
  "Place point at link sentinel

XXX Insert link at start of enclosing TODO heading or area of
focus heading, or in the incoming tasks tree.  This function
customises Org-Mode."
  (alb-org-locate-heading)
  (while (< 3 (org-current-level))
    (outline-up-heading 1 t))
  (org-show-entry)
  (alb-org-down-structure))

;;
;; Heading sorting
;;

(defun alb-org-sort-rank (properties)
  "Return the rank of a todo item from its PROPERTIES

The rank defines the first step in an approximated reverse
chronological order.  The rank gives the todo state an
interpretation in this order.  All =DUTY= items are newer than
all =HOLD= items, which are newer than all =TODO= and =WAIT=
items, which are newer than all =DONE= and =STOP= items.  This
function customises Org-Mode."
  (let ((todo (cdr (assoc "TODO" properties))))
    (cond ((not todo)
           0)
          ((string= todo "DUTY")
           1)
          ((string= todo "HOLD")
           2)
          ((or (string= todo "TODO") (string= todo "WAIT"))
           3)
          ((or (string= todo "DONE") (string= todo "STOP"))
           4))))

(defun alb-org-sort-timestamp (properties)
  "Return the time stamp of a todo item from its PROPERTIES

The timestamp defines the second step in an approximated reverse
chronological order.  =HOLD= items are timestamped by scheduled
time.  =DUTY=, =TODO=, =WAIT=, =STOP=, and =DONE= items are
timestamped from the first keyword-less inactive timestamp in the
entry.  In practice, this is the timestamp of the last TODO state
transition.  All other items are set to epoch.  This function
customises Org-Mode."
  (let ((todo (cdr (assoc "TODO" properties))))
    (cond ((and (assoc "SCHEDULED" properties)
                (string= todo "HOLD"))
           (date-to-time (cdr (assoc "SCHEDULED" properties))))
          ((and (assoc "TIMESTAMP_IA" properties)
                (or (string= todo "DUTY") (string= todo "TODO")
                    (string= todo "WAIT") (string= todo "STOP")
                    (string= todo "DONE")))
           (date-to-time (cdr (assoc "TIMESTAMP_IA" properties))))
          (t
           '(0 0)))))

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

;;
;; Widen
;;

(defun alb-org-widen ()
  "Widen to the whole buffer and centre the headline"
  (interactive)
  (widen)
  (recenter))

;;
;; Structure editing
;;

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

;;
;; Content editing
;;

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
;; Meta-data editing
;;

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
;; Project filing
;;

(defun alb-org--project-dirname-visit (patterns text)
  "Recur on the project directory name PATTERNS to locate match for TEXT"
  (cond
   ((not patterns)
    "~/")
   ((string-match-p (caar patterns) text)
    (let ((fname (replace-regexp-in-string (caar patterns) (cdar patterns)
                                           text t)))
      (while (string-match "<<\\(.*?\\)>>" fname)
        (let ((old (regexp-quote (match-string 0 fname)))
              (new (alb-org-abbrev (match-string 1 fname))))
          (setq fname (replace-regexp-in-string old new fname t))))
      fname))
   (t
    (alb-org--project-dirname-visit (cdr patterns) text))))

(defun alb-org-project-dirname ()
  "Determine the filename of the project directory"
  (interactive)
  (alb-org--project-dirname-visit
   alb-org-project-dirname-patterns
   (substring-no-properties (concat (org-get-heading t t) " "
                                    (cdr (assoc "ALLTAGS"
                                                (org-entry-properties)))))))

(defun alb-org--project-files-visit (filename)
  "Recur on the parents of FILENAME to locate accessible directory"
  (if (file-accessible-directory-p filename)
      (find-name-dired filename "*")
    (alb-org--project-files-visit (directory-file-name
                                   (file-name-directory filename)))))

(defun alb-org-project-files ()
  "Interact with the project files using `dired`"
  (interactive)
  (alb-org--project-files-visit (expand-file-name (alb-org-project-dirname))))

(defun alb-org-project-readme ()
  "Visit the project README file"
  (interactive)
  (find-file (concat (alb-org-project-dirname) "/README.rst")))

;; Local Variables:
;; mode: emacs-lisp
;; End:
