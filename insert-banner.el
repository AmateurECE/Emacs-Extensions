;;; insert-banner.el --- Easily document my code    -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Ethan D. Twardy

;; Author: Ethan D. Twardy <ethan.twardy@gmail.com>
;; Keywords: lisp
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add some usful functions to document my code, namely by inserting file
;; headers, function headers, and section headers to separate code.

;;; Code:

;; CREATED:     06/16/2017
;; LAST EDITED: 10/28/2021

(defgroup banner-comments nil
  "Group for the custom attributes that apply to insert-banner.el")

(defcustom file-banner-license-notice nil
  "If this is set to t, insert-file-banner will insert a license notice."
  :type 'boolean
  :group 'banner-comments)

(defcustom insert-banner-indent-column 20
  "The column number to indent all fields to by default."
  :type 'integer
  :group 'banner-comments)

(defcustom file-copyright-license nil
  "The license that the programmer wishes to use. Choices are provided."
  :type 'hook
  :options '(file-gplv3-license file-mit-license)
  :group 'banner-comments)

(defconst file-copyright-notice
  (concat "Copyright Date, " (getenv "GIT_AUTHOR_NAME")))

(defconst file-gplv3-license
  "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.")

(defconst file-gplv2-license
  "\
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.")

(defconst file-mit-license
  "\
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.")

(defvar my-name (getenv "GIT_AUTHOR_NAME"))
(defvar my-email (getenv "GIT_AUTHOR_EMAIL"))
(defvar my-signature (concat my-name " <" my-email ">"))

(defcustom define-guard-prefix ""
  "Prefix to use for every define guard in all headers"
  :type 'string
  :group 'banner-comments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definitions
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-define-guards
;;
;; DESCRIPTION:	    Inserts define guards into the current file at the current
;;		    position.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun insert-define-guards ()
  "Inserts define guards into the current file at the current position."
  (setq file-name (short-buffer-file-name))
  (setq guard-name-regexp (concat
			   "\\<"
			   (rx (group (one-or-more (any word punct))))
			   "\\.hp*\\>"))
  (string-match guard-name-regexp file-name)
  (setq guard-name (concat
                    define-guard-prefix
                    (replace-regexp-in-string
                     (regexp-quote "-") "_"
                     (upcase (match-string 1 file-name)))
                    "_H"))
  (insert "#ifndef " guard-name "\n")
  (insert "#define " guard-name "\n\n")
  (insert "#endif // " guard-name "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-end-of-file-marker
;;
;; DESCRIPTION:	    Inserts an end-of-file marker at the end of the file.
;;
;; ARGUMENTS:	    nl: (string) -- char printed at new line.
;;		    sym: (string) -- standard comment char.
;;		    stt: (string) -- char that starts & ends a comment. Only
;;			used in c-mode.
;;
;; RETURN:	    void
;;
;; NOTES:	    none.
;;;
(defun insert-end-of-file-marker (nl sym stt)
  "Inserts an end-of-file marker at the end of the file."
  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 79)
    (setq iter 78))

  (let (val)
    (dotimes (num iter val)
      (insert sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    short-buffer-file-name
;;
;; DESCRIPTION:	    Return the shortened version of buffer-file-name.
;;
;; ARGUMENTS:	    void
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun short-buffer-file-name ()
  "Return the shortened version of buffer-file-name."
  (buffer-name (window-buffer (minibuffer-selected-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-and-tab
;;
;; DESCRIPTION:	    Insert 'field' then tab, and then insert 'strings'.
;;
;; ARGUMENTS:	    field: The string to be printed at the line beginning.
;;		    strings: The rest of the strings to be printed.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun insert-and-tab (field &rest strings)
  "Insert `field' into the current buffer, indent to column
`insert-banner-insert-column', and then insert `strings'."
  (insert field)
  (indent-to-column insert-banner-indent-column)
  (while strings
    (insert (car strings))
    (setq strings (cdr strings))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    get-file-banner-license
;;
;; DESCRIPTION:	    Returns the license, as a string.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun get-file-banner-license ()
  "Returns the license notice, as a string."
  (cond
   ((eq file-copyright-license 'file-gplv3-license)
    file-gplv3-license)
   ((eq file-copyright-license 'file-mit-license)
    file-mit-license)
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    end-of-comment
;;
;; DESCRIPTION:	    This function returns a relatively accurate (but not exact)
;;		    position for the end of the current comment, or nil.
;;		    is not currently in a comment.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    relative end of comment position, or nil if (point) is not
;;		    in a comment.
;;
;; NOTES:	    none.
;;;
(defun end-of-comment ()
  "This function returns a relatively accurate (but not exact) position for the
end of the current comment, or nil if point is not currently in a comment."
  (save-excursion
    (let ((face (remq nil `(,(get-char-property (point) 'read-face-name)
			    ,(get-char-property (point) 'face)
			    ,(plist-get (text-properties-at (point)) 'face))))
	  (p (point)))
      (while (or (memq 'font-lock-comment-face face)
		 (memq 'font-lock-comment-delimiter-face face))
	(setq face (remq nil
			 `(,(get-char-property (point) 'read-face-name)
			   ,(get-char-property (point) 'face)
			   ,(plist-get (text-properties-at (point)) 'face))))
	(forward-line))
      (if (eq p (point))
	  nil
	(point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    update-last-edited-date
;;
;; DESCRIPTION:	    If the file was written by me, and corresponds to my
;;		    commenting style, then update the last edited date to be
;;		    today (Called before save-buffer).
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    Uses save excursion, but we'll have to see how moving point
;;		    affects the position of the buffer.
;;		    TODO: Investigate behavior in diff-mode
;;		    TODO: Does not work with module banner in python mode
;;;
(defun update-last-edited-date ()
  "Update the last edited date, if the file was written by me."
  (save-excursion
    (let ((inhibit-redisplay t)
	  eos c)
      (beginning-of-buffer)
      (setq eos (end-of-comment))
      (when (re-search-forward my-name eos 'keep-point)
	(re-search-forward "LAST EDITED:" eos 'keep-point)
	(forward-whitespace 1)
	(setq c (char-after (point)))
	(when (and (not (null c)) (<= c ?9) (>= c ?0))
	  (while (not (eq (setq c (char-after (point))) ?\n))
	    (delete-char 1))
	  (insert (shell-command-to-string "echo -n $(date +%m/%d/%Y)")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Banners
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    generic-file-banner
;;
;; DESCRIPTION:	    Function for inserting a generic file-banner. This function
;;		    is called whenever the buffer it is called from is in one
;;		    of the major-modes that supports this. These modes are:
;;			* c-mode
;;			* sh-mode
;;			* txt-mode
;;			* perl-mode
;;			* emacs-lisp-mode
;;			* asm-mode'
;;			* latex-mode'
;;			* matlab-mode
;;			* python-mode'
;;		    ' - Note: These modes are supported by this function, but
;;		    may have specific implementations for function or section
;;		    headers.
;;		    All other supported modes have individual implementations.
;;
;; ARGUMENTS:	    nl: (string) -- char printed at new line.
;;		    sym: (string) -- standard comment char.
;;		    stt: (string) -- char that starts & ends a comment. Only
;;			used in c-mode.
;;
;; RETURN:	    nil.
;;
;; NOTES:	    none.
;;;
(defun generic-file-banner (nl sym stt)
  "File Banner function for many major modes"

  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 79)
    (setq iter 78))
  
  (let (val)
    (dotimes (num iter val)
      (insert sym)))

  (insert "\n" nl)
  (insert-and-tab " NAME:" name "\n" nl "\n" nl)
  (insert-and-tab " AUTHOR:" my-signature "\n" nl "\n" nl)
  (insert-and-tab " DESCRIPTION:")
  (save-excursion
    (insert "\n" nl "\n" nl)
    (insert-and-tab " CREATED:" date nl "\n" nl)
    (insert-and-tab " LAST EDITED:" date)
    (if  file-banner-license-notice
	(progn
	  (insert nl "\n" nl " ")
	  (let ((notice (get-file-banner-license))
		(cpydate file-copyright-notice)
		(date (shell-command-to-string "date +%Y")))
	    (if (null notice)
		(error "Must select a license to use."))
	    (setq cpydate (replace-regexp-in-string "Date" date cpydate))
	    (insert (replace-regexp-in-string "\n" "" cpydate) "\n")
	    (insert nl "\n")
	    (dolist (line (split-string notice "\n"))
              (if (eq line "")
                  (insert nl "\n")
	        (insert nl " " line "\n"))))))
    (if (string-equal sym ";")
	(insert sym sym sym)
      (insert nl sym sym))
    (when (not (null stt)) (insert stt))
    (insert "\n\n")

    ;; If our file is a C Header, add include define guards
    (if (string-match "\\.hp*$" (short-buffer-file-name))
	(progn
	  (insert-define-guards)
	  (insert "\n")))
    (insert-end-of-file-marker nl sym stt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    ubt-file-banner
;;
;; DESCRIPTION:	    Insert a file banner for the U-Boot scripting language.
;;
;; ARGUMENTS:	    nl: (string) -- char printed at new line.
;;		    sym: (string) -- standard comment char.
;;		    stt: (string) -- char that starts & ends a comment. Only
;;			used in c-mode.
;;
;; RETURN:	    none.
;;
;; NOTES:	    ubt-file-banner does not support license banners.
;;;
(defun ubt-file-banner (nl sym stt)
  "Insert a file banner at the top of a U-Boot Script file"

  (setq iter 79)
  (let (val)
    (dotimes (num iter val)
      (insert sym)))

  (insert "\n" nl)
  (insert-and-tab " NAME:" name "\n" nl "\n" nl)
  (insert-and-tab " AUTHOR:" my-signature "\n" nl "\n" nl)
  (insert " DESCRIPTION:")
  (indent-to-column 20)
  (save-excursion
    (insert "\n" nl "\n" nl)
    (insert " CREATED:	    " date)
    (insert nl "\n" nl)
    (insert " LAST EDITED:	    " date)
    (insert nl "\n" nl)
    (insert " DEPENDENCIES:	    \n")
    (insert nl sym sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    fortran-file-banner
;;
;; DESCRIPTION:	    This function inserts a banner according to fortran style.
;;
;; ARGUMENTS:	    sym: C or !, according to the fortran version.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun fortran-file-banner (sym)
  "Insert a file banner in the FORTRAN style."
  (insert sym "\n" sym)
  (insert-and-tab " NAME:" name "\n" sym "\n" sym " DESCRIPTION:")
  (indent-to-column 20)
  (save-excursion
    (insert "\n" sym "\n" sym)
    (insert-and-tab " CREATED:" date sym "\n" sym)
    (insert-and-tab " LAST EDITED:" date sym "\n")
    (if file-banner-license-notice
	(progn
	  (insert sym "\n" sym " ")
	  (let ((notice (get-file-banner-license))
		(cpydate file-copyright-notice)
		(date (shell-command-to-string "date +%Y")))
	    (if (null notice)
		(error "Must select a license to use."))
	    (setq cpydate (replace-regexp-in-string "Date" date cpydate))
	    (insert (replace-regexp-in-string "\n" "" cpydate) "\n")
	    (insert sym "\n")
	    (dolist (line (split-string notice "\n"))
	      (insert sym " " line "\n"))
	    (insert sym "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    markdown-file-banner
;;
;; DESCRIPTION:	    Insert a file banner in the Markdown style.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun markdown-file-banner ()
  "Insert a file banner in the Markdown style."
  (insert "<!--\n")
  (insert-and-tab " NAME:" name "\n\n" " DESCRIPTION:")
  (indent-to-column 20)
  (save-excursion
    (insert "\n\n")
    (insert-and-tab " CREATED:" date "\n")
    (insert-and-tab " LAST EDITED:" date)
    (if file-banner-license-notice
	(progn
	  (insert "\n")
	  (let ((notice (get-file-banner-license))
		(cpydate file-copyright-notice)
		(date (shell-command-to-string "date +%Y")))
	    (if (null notice)
		(error "Must select a license to use."))
	    (setq cpydate (replace-regexp-in-string "Date" date cpydate))
	    (insert " " (replace-regexp-in-string "\n" "" cpydate) "\n\n")
	    (dolist (line (split-string notice "\n"))
	      (insert " " line "\n")))))
    (insert "-->")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    java-file-banner
;;
;; DESCRIPTION:	    This function inserts a JavaDoc file banner in the style
;;		    preferred by my CS2321 class.
;;
;; ARGUMENTS:	    none.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun java-file-banner ()
  "Insert a file banner in the JavaDoc style."
  (insert "\n/**")
  (insert-and-tab "\n * NAME:" name)
  (insert-and-tab "\n *\n * DESCRIPTION:")
  (save-excursion
    (insert-and-tab "\n *\n * @author" my-signature)
    (insert-and-tab "\n *\n * CREATED:" date)
    (insert-and-tab " *\n * LAST EDITED:" date " */")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-file-banner
;;
;; DESCRIPTION:	    This function is responsible for inserting the banner
;;		    that you see at the beginning of every one of my source
;;		    files.
;;
;; ARGUMENTS:	    name: The name of the function
;;
;; RETURN:	    void.
;;
;; NOTES:	    TODO: insert-file-banner doesn't work for C++
;;		    TODO: Create end-of-file marker
;;;
(defun insert-file-banner ()
  "Insert a banner at the top of a file"
  (interactive)
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)
  (setq date (shell-command-to-string "date +%m/%d/%Y"))
  (setq name buffer-file-name)
  (string-match "/\\([^/]*\\)$" buffer-file-name)
  (setq name (match-string 1 buffer-file-name))
  (cond
   ((or (eq major-mode 'asm-mode)
	(eq major-mode 'dts-mode)
	(eq major-mode 'bison-mode)
	(eq major-mode 'yacc-mode)
        (eq major-mode 'flex-mode))
    (generic-file-banner " *" "*" "/"))
   ((or (eq major-mode 'c++-mode)
        (eq major-mode 'c-mode)
        (eq major-mode 'scss-mode)
	(eq major-mode 'verilog-mode)
        (eq major-mode 'rust-mode))
    (generic-file-banner "//" "/" nil))
   ((eq major-mode 'emacs-lisp-mode)
    (generic-file-banner ";;" ";" nil))
   ((or (eq major-mode 'latex-mode) (eq major-mode 'matlab-mode))
    (generic-file-banner "%" "%" nil))
   ((eq major-mode 'ubt-mode)
    (ubt-file-banner "#" "#" nil))
   ((eq major-mode 'f90-mode)
    (fortran-file-banner "!"))
   ((eq major-mode 'fortran-mode)
    (fortran-file-banner "C"))
   ((eq major-mode 'markdown-mode)
    (markdown-file-banner))
   ((eq major-mode 'java-mode)
    (java-file-banner))
   ((eq major-mode 'rjsx-mode)
    (generic-file-banner "//" "/" nil))
   ((eq major-mode 'ess-r-mode)
    (generic-file-banner "##" "#" nil))
   (t (generic-file-banner "#" "#" nil)))) ;; Default case

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Header
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    generic-function-header
;;
;; DESCRIPTION:	    Function for inserting a generic function-banner. This
;;		    function is called whenever the buffer it is called from is
;;		    one of the major-modes that supports this. These modes are:
;;			* c-mode
;;			* sh-mode
;;			* txt-mode
;;			* perl-mode
;;			* emacs-lisp-mode
;;			* matlab-mode
;;		    ' - Note: These modes are supported by this function, but
;;		    may have specific implementations for file or section
;;		    headers.
;;		    All other supported modes have individual implementations.
;;
;; ARGUMENTS:	    nl, sym, stt: An arbitrary arrangement of characters that
;;			marginally resemble the comment character for the lang.
;;                  name: Name of the function
;;
;; RETURN:	    None.
;;
;; NOTES:	    None.
;;;
(defun generic-function-header (nl sym stt name)
  "Inserts the generic function header"
  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 79)
    (setq iter 78))
  (let (val)
    (dotimes (num iter val)
      (insert sym)))
  (insert "\n" nl)
  (insert-and-tab " FUNCTION:" name "\n" nl "\n" nl)
  (insert-and-tab " DESCRIPTION:")
  (save-excursion
    (insert "\n" nl "\n" nl)
    (insert-and-tab " ARGUMENTS:" "\n" nl "\n" nl)
    (insert-and-tab " RETURN:" "\n")
    (if (string-equal sym ";")
	(insert sym sym sym)
      (insert nl sym sym))
    (when (not (null stt)) (insert stt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    latex-function-header
;;
;; DESCRIPTION:	    Inserts a function header for LaTeX.
;;
;; ARGUMENTS:       name: Name of the function
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun latex-function-header (name)
  "Inserts a command header for LaTeX."
  
  (let (val)
    (dotimes (num 79 val)
      (insert "%")))
  (insert "% Command:	    " name "\n")
  (insert "% Function:	    ")
  (setq currpos (point))
  (insert "\n")
  (insert "% Arguments:	    \n")
  (goto-char currpos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    asm-function-header
;;
;; DESCRIPTION:	    Insert an assembly subroutine header.
;;
;; ARGUMENTS:	    name: Name of the function
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun asm-function-header (name)
  "Insert an Assembly subroutine header"

  (insert "/")
  (let (val)
    (dotimes (num 78 val)
      (insert "*")))
  (insert "\n *")
  (insert-and-tab " SUBROUTINE:" name "\n *\n *")
  (insert-and-tab " DESCRIPTION:")
  (save-excursion
    (insert "\n *\n *")
    (insert-and-tab " REGISTER USAGE:" "\n *\n *")
    (insert-and-tab " RETURN:" "\n ***/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    python-function-header
;;
;; DESCRIPTION:	    Inserts a PyDoc Function "header" inside of a function
;;		    definition.
;;
;; ARGUMENTS:       name: Name of the function
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun python-function-header (name)
  "Inserts a PyDoc function header in the Google style."
  ;; Not interactive.
  (insert "\"\"\"" name ":\n")
  (indent-for-tab-command)
  (setq currpos (point))
  (insert "\n\n")
  (indent-for-tab-command)
  (insert "Args:\n")
  (indent-for-tab-command)
  (insert "\t\n\n")
  (indent-for-tab-command)
  (insert "Returns:\n")
  (indent-for-tab-command)
  (insert "\t\n\n")
  (indent-for-tab-command)
  (insert "Raises:\n")
  (indent-for-tab-command)
  (insert "\t\n")
  (indent-for-tab-command)
  (insert "\"\"\"")
  (goto-char currpos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    java-function-header
;;
;; DESCRIPTION:	    Inserts a JavaDoc function header.
;;
;; ARGUMENTS:	    name: Name of the function
;;
;; RETURN:	    none
;;
;; NOTES:	    none.
;;;
(defun java-function-header (name)
  "Inserts a JavaDoc function header in the style required by my CS2321 class."
  ;; Not interactive
  (indent-for-tab-command)
  (insert "/**\n* ")
  (indent-for-tab-command)
  (insert name "\n* ")
  (indent-for-tab-command)
  (save-excursion
    (insert "Don't forget your TCJ comment!\n* ")
    (dolist (elm '(("@param" "\n* ")
		   ("@return" "\n* ")
		   ("@throws" "\n*/"))
		 t)
      (indent-for-tab-command)
      (insert-and-tab (car elm) (car (cdr elm))))
    (indent-for-tab-command)
    (insert "\n@TimeComplexity(\"O(/* TODO */)\")")
    (indent-for-tab-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-function-header
;;
;; DESCRIPTION:	    This function inserts the headers that are at the beginning
;;		    of each one of my functions (like here.)
;;
;; ARGUMENTS:	    name: The name of the function.
;;
;; RETURN:	    void.
;;
;; NOTES:	    none.
;;;
(defun insert-function-header (name)
  "Insert a header at the top of a function"
  (interactive "sFunction-Name: \n")
  (setq nl nil)
  (setq sym nil)
  (setq stt nil)
  (cond
   ((or (eq major-mode 'bison-mode)
	(eq major-mode 'yacc-mode)
        (eq major-mode 'flex-mode))
    (generic-function-header " *" "*" "/" name))
   ((or (eq major-mode 'c-mode)
        (eq major-mode 'verilog-mode)
        (eq major-mode 'js-mode)
        (eq major-mode 'c++-mode))
    (generic-function-header "//" "/" nil name))
   ((eq major-mode 'emacs-lisp-mode)
    (generic-function-header ";;" ";" nil name))
   ((eq major-mode 'matlab-mode)
    (generic-function-header "%" "%" nil name))
   ((eq major-mode 'latex-mode)
    (latex-function-header name))
   ((eq major-mode 'asm-mode)
    (asm-function-header name))
   ((eq major-mode 'python-mode)
    (python-function-header name))
   ((eq major-mode 'java-mode)
    (java-function-header name))
   ;; Default case
   (t (generic-function-header "#" "#" nil name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section Header
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    generic-section-header
;;
;; DESCRIPTION:	    Inserts a generic section-header. This function exists
;;		    mostly in case I want to change the format for a particular
;;		    mode in the future. Currently it supports all major modes.
;;
;; ARGUMENTS:	    nl: (string) -- char printed at new line.
;;		    sym: (string) -- standard comment char.
;;		    stt: (string) -- char that starts & ends a comment. Only
;;			used in c-mode.
;;                  name: Name of the section
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun generic-section-header (nl sym stt name)
  "Insert a generic-section header."
  (when (not (null stt)) (insert stt))
  (if (null stt)
      (setq iter 79)
    (setq iter 78))

  (let (val)
    (dotimes (num iter val)
      (insert sym)))

  (insert "\n" nl " " name "\n")
  (if (string-equal sym ";")
      (insert sym sym sym)
    (insert nl sym sym))
  (when (not (null stt)) (insert stt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    java-section-header
;;
;; DESCRIPTION:	    Insert a JavaDoc section header.
;;
;; ARGUMENTS:	    name: Name of the section header
;;
;; RETURN:	    none
;;
;; NOTES:	    none
;;;
(defun java-section-header (name)
  "Insert a JavaDoc Section header."
  (indent-for-tab-command)
  (insert "/")
  (while (< (current-column) 78)
    (insert "*"))
  (insert "\n* " name)
  (indent-for-tab-command)
  (insert "\n***/")
  (indent-for-tab-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-section-header
;;
;; DESCRIPTION:	    This function inserts the headers that separate each part
;;		    of the source code. Section guidelines are outlined in
;;		    each of the respective languages' coding style guidelines.
;;
;; ARGUMENTS:	    name: The name of the section.
;;
;; RETURN:	    void.
;;
;; NOTES:	    none
;;;
(defun insert-section-header (name)
  "Insert a section header"
  (interactive "sSection-Name: \n")

  (cond
   ((or (eq major-mode 'asm-mode)
	(eq major-mode 'dts-mode)
	(eq major-mode 'bison-mode)
	(eq major-mode 'yacc-mode)
        (eq major-mode 'flex-mode))
    (generic-section-header " *" "*" "/" name))
   ((or (eq major-mode 'c++-mode)
        (eq major-mode 'c-mode)
	(eq major-mode 'verilog-mode)
        (eq major-mode 'js-mode)
        (eq major-mode 'scss-mode)
        (eq major-mode 'rust-mode))
    (generic-section-header "//" "/" nil name))
   ((eq major-mode 'emacs-lisp-mode)
    (generic-section-header ";;" ";" nil name))
   ((or (eq major-mode 'latex-mode)
	(eq major-mode 'matlab-mode))
    (generic-section-header "%" "%" nil name))
   ((eq major-mode 'java-mode)
    (java-section-header name))
   ;; Default case
   (t (generic-section-header "#" "#" nil name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class Docs
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    python-class-header
;;
;; DESCRIPTION:	    Insert a PyDoc class header in the Google style.
;;
;; ARGUMENTS:	    name: The name of the class.
;;
;; RETURN:	    None.
;;
;; NOTES:	    None.
;;;
(defun python-class-header (name)
  "Inset a Google Style Python Class Header."
  (let ((name name))
    (insert "\"\"\"" name "\n")
    (indent-for-tab-command)
    (setq currpos (point))
    (insert "\n\n")
    (indent-for-tab-command)
    (insert "Attributes:\n")
    (indent-for-tab-command)
    (insert "\t\n")
    (indent-for-tab-command)
    (insert "\"\"\"")
    (goto-char currpos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    java-class-header
;;
;; DESCRIPTION:	    Insert a JavaDoc class header.
;;
;; ARGUMENTS:	    name: The name of the class.
;;
;; RETURN:	    none.
;;
;; NOTES:	    none.
;;;
(defun java-class-header (name)
  "Inserts a JavaDoc class header."
  (indent-for-tab-command)
  (insert "/**\n* ")
  (indent-for-tab-command)
  (insert name "\n* ")
  (indent-for-tab-command)
  (save-excursion
    (insert-and-tab "\n  * @param")
    (indent-for-tab-command)
    (insert "\n*/")
    (indent-for-tab-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTION:	    insert-class-header
;;
;; DESCRIPTION:	    Insert a header for a class.
;;
;; ARGUMENTS:	    name: The name of the class.
;;
;; RETURN:	    None.
;;
;; NOTES:	    None.
;;;
(defun insert-class-header (name)
  "Insert a class header"
  (interactive "sClass Name: \n")
  (cond
   ((eq major-mode 'python-mode)
    (python-class-header name))
   ((eq major-mode 'java-mode)
    (java-class-header name))
   ;; Default case
   (t
    (message "Support for this mode has not been implemented"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS
;;;

(if (string= "" (getenv "GIT_AUTHOR_NAME"))
    (warn (concat "GIT_AUTHOR_NAME is not defined in the environment. "
                  "This package may not work correctly.")))
(if (string= "" (getenv "GIT_AUTHOR_EMAIL"))
    (warn (concat "GIT_AUTHOR_EMAIL is not defined in the environment. "
                  "This package may not work correctly.")))

(global-unset-key (kbd "C-b"))
(global-unset-key (kbd "C-b b"))
(global-set-key (kbd "C-b b") 'insert-file-banner)
(global-unset-key (kbd "C-b f"))
(global-set-key (kbd "C-b f") 'insert-function-header)
(global-unset-key (kbd "C-b h"))
(global-set-key (kbd "C-b h") 'insert-section-header)
(global-unset-key (kbd "C-b c"))
(global-set-key (kbd "C-b c") 'insert-class-header)
(global-unset-key (kbd "C-x C-s"))
(global-set-key (kbd "C-x C-s")
		'(lambda ()
		   (interactive)
		   (update-last-edited-date) ;; insert-banner.el
		   (save-buffer)))

(provide 'insert-banner)
;;; insert-banner.el ends here
