;;
;; alb-org-mode/alb-org-mode-start.el
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
;;
;; DEFERRED CONFIGURATION
;; ---------------------------------------------------------------------
;;

(eval-after-load 'org
 '(progn
    ;; Require org-capture templates
    (require 'org-capture)

    ;; Require functions to customise behavior
    (require 'alb-org-behavior)))


;; Local Variables:
;; mode: emacs-lisp
;; End:
