;;; local-snippet.el --- Local emacs config         -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Ethan D. Twardy

;; Author: Ethan D. Twardy <ethan.twardy@gmail.com>
;; Keywords: lisp
;; Version: 0.1.0

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

;; CREATED:     10/04/2021
;; LAST EDITED: 10/04/2021

(defun load-local-snippets ()
  "Iterate over elements of the current path, loading found .emacs files."
  (let ((iterator "") (snippet ""))
    (dolist (element (split-string buffer-file-name "/") t)
      (if (not (eq element ""))
          (progn
            (setq iterator (concat iterator "/" element))
            (setq snippet (concat iterator "/.emacs"))
            (if (and (file-exists-p snippet)
                     (not (string= snippet user-init-file)))
                (load-file snippet))))))
  )

(provide 'local-snippet)
;;; insert-banner.el ends here
