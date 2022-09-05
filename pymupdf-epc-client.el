;;; pymupdf-epc-client.el --- Manipulate pdf files with pymupdf-epc-server  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Daniel Nicolai

;; Author: Daniel Nicolai <dalanicolai@fedora>
;; Keywords: multimedia, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'epc)
(require 'svg)

(defcustom pymupdf-epc-executable "pymupdf-epc-server.py"
  "Name of pymupdf-epc-server python file.")

(defvar pymupdf-epc-server nil)

(defvar pymupdf-epc-info-commands '(pymupdf-epc-number-of-pages
                                    pymupdf-epc-toc
                                    pymupdf-epc-page-structured-text))

(defsubst list-to-cons (pair-list)
  (cons (nth 0 pair-list)
        (nth 1 pair-list)))

(defun pymupdf-epc-info (function &optional arg)
  (interactive (list (completing-read "Select info type: "
                                      pymupdf-epc-info-commands)
                     current-prefix-arg))
  (pp (pcase (intern function)
        ('pymupdf-epc-page-structured-text
         (call-interactively #'pymupdf-epc-page-structured-text))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*pymupdf-epc-info*")))
  (when arg (pop-to-buffer "*pymupdf-epc-info*")))

(defun pymupdf-epc-start-server (&optional local)
  (interactive)
  (let ((server (epc:start-epc "python" (list pymupdf-epc-executable))))
    (unless local
      (setq pymupdf-epc-server server))
    server))

(defun pymupdf-epc-kill-server (&optional local)
  (interactive)
  (epc:stop-epc pymupdf-epc-server))

(defun pymupdf-epc-restart-server (&optional local)
  (interactive)
  (epc:manager-restart-process pymupdf-epc-server))

(defun pymupdf-epc-test ()
  (interactive)
  (pp (epc:call-sync pymupdf-epc-server 'test nil)))

(defun pymupdf-epc-init (&optional file)
  (interactive "fSelect pdf file: ")
  (epc:call-sync pymupdf-epc-server 'open (list (or file (buffer-file-name)))))

(defun pymupdf-epc-number-of-pages ()
  (interactive)
  (epc:call-sync pymupdf-epc-server 'number_of_pages nil))

(defun pymupdf-epc-page-structured-text (&optional page detail)
  (interactive "nEnter page number: ")
  (epc:call-sync pymupdf-epc-server 'page_structured_text (list page (symbol-name detail))))

(defun pymupdf-epc-page-sizes ()
  (interactive)
  (let ((sizes (epc:call-sync pymupdf-epc-server 'pagesizes nil)))
        (mapcar #'list-to-cons sizes)))

(defun pymupdf-epc-page-base64-image-data (page width)
  (interactive "nEnter page number: ")
  (epc:call-sync pymupdf-epc-server 'renderpage (list page width)))

(defun pymupdf-epc-toc ()
  (interactive)
  (epc:call-sync pymupdf-epc-server 'toc nil))

(defun pymupdf-epc-get-annots (page)
  (interactive "nEnter page number: ")
  (epc:call-sync pymupdf-epc-server 'get_annots (list page)))

(provide 'pymupdf-epc-client)
;;; pymupdf-epc-client.el ends here
