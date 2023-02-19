;;; doc-pymupdf-client.el --- Manipulate pdf files with doc-pymupdf-server  -*- lexical-binding: t; -*-

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

(defcustom doc-pymupdf-executable "doc-pymupdf-server.py"
  "Name of doc-pymupdf-server python file.")

(defvar doc-pymupdf-server nil)

(defvar doc-pymupdf-info-commands '(doc-pymupdf-number-of-pages
                                    doc-pymupdf-toc
                                    doc-pymupdf-metadata
                                    doc-pymupdf-page-structured-text))

(defsubst list-to-cons (pair-list)
  (cons (nth 0 pair-list)
        (nth 1 pair-list)))

(defun doc-pymupdf-info (function &optional arg)
  (interactive (list (completing-read "Select info type: "
                                      doc-pymupdf-info-commands)
                     current-prefix-arg))
  (pp (pcase (intern function)
        ('doc-pymupdf-page-structured-text
         (call-interactively #'doc-pymupdf-page-structured-text))
        (var (funcall var)))
      (when arg
        (get-buffer-create "*doc-pymupdf-info*")))
  (when arg (pop-to-buffer "*doc-pymupdf-info*")))

(defun doc-pymupdf-start-server (&optional local)
  (interactive)
  (let ((server (epc:start-epc "python" (list doc-pymupdf-executable))))
    (unless local
      (setq doc-pymupdf-server server))
    server))

(defun doc-pymupdf-kill-server (&optional local)
  (interactive)
  (epc:stop-epc doc-pymupdf-server))

(defun doc-pymupdf-restart-server (&optional local)
  (interactive)
  (epc:manager-restart-process doc-pymupdf-server))

(defun doc-pymupdf-test ()
  (interactive)
  (pp (epc:call-sync doc-pymupdf-server 'test (list 'hello))))

(defun doc-pymupdf-init (&optional file)
  (interactive "fSelect pdf file: ")
  (epc:call-sync doc-pymupdf-server 'open (list (or file (buffer-file-name)))))

(defun doc-pymupdf-number-of-pages ()
  (interactive)
  (epc:call-sync doc-pymupdf-server 'number_of_pages nil))

(defun doc-pymupdf-page-structured-text (&optional page detail)
  (interactive (let ((last-page (doc-pymupdf-number-of-pages)))
                 (list (read-number (format "Select page(s) (max %s): " last-page)
                                    (or (doc-scroll-current-page) 1))
                       (intern (completing-read "Select detail: "
                                                '(plain djvu blocks words xml))))))
  (when (eq detail 'plain) (setq detail nil))
  (let ((text (epc:call-sync doc-pymupdf-server
                             'page_structured_text
                             (list page (symbol-name detail)))))
    (if (eq detail 'xml)
        (with-temp-buffer
          (insert text)
          (xml-parse-region))
      text)))

(defun doc-pymupdf-restructure-text (text)
  "Convert structured text to djvu text structure."
  (mapcar (lambda (e)
            (let ((type (car e)))
              (pcase type
                ('page (let-alist (nth 1 e)
                         (append (list type 0 0
                                       (string-to-number .width) (string-to-number .height))
                                 (doc-pymupdf-restructure-text
                                  (delete "\n" (nthcdr 3 e))))))
                ((or 'line 'block) (append (cons type (mapcar #'string-to-number
                                                              (split-string (cdar (nth 1 e)))))
                                           (doc-pymupdf-restructure-text
                                            (pcase type
                                              ('block (delete "\n" (nthcdr 3 e)))
                                              ('line (delete "\n" (nthcdr 3 (nth 3 e))))))))
                ('char (let-alist (nth 1 e)
                         (let ((coord-string (split-string .quad)))
                           `(,type
                             ,@(mapcar (lambda (n)
                                         (string-to-number (nth n coord-string)))
                                       '(0 1 6 7))
                             ,.c)))))))
          text))

(defun doc-pymupdf-page-sizes ()
  (interactive)
  (let ((sizes (epc:call-sync doc-pymupdf-server 'pagesizes nil)))
        (mapcar #'list-to-cons sizes)))

(defun doc-pymupdf-page-svg-data (page text)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-server 'renderpage_svg (list page text)))

(defun doc-pymupdf-page-base64-image-data (page width)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-server 'renderpage_data (list page width)))

(defun doc-pymupdf-page-image-file (page width path)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-server 'renderpage_file (list page width path)))

(defun doc-pymupdf-toc ()
  (interactive)
  (epc:call-sync doc-pymupdf-server 'toc nil))

(defun doc-pymupdf-metadata ()
  (interactive)
  (epc:call-sync doc-pymupdf-server 'metadata nil))

(defun doc-pymupdf-get-annots (page)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-server 'get_annots (list page)))

(defun doc-pymupdf-add-annot (page style edges)
  (interactive "nEnter page number: ")
  (epc:call-sync doc-pymupdf-server 'addannot (list page style edges)))

(defun doc-pymupdf-search (pattern &optional start-page end-page)
  (interactive "nEnter pattern: ")
  (epc:call-sync doc-pymupdf-server 'search (list pattern start-page end-page)))

(defun doc-pymupdf-swipe (pattern)
  (interactive "nEnter pattern: ")
  (epc:call-sync doc-pymupdf-server 'swipe (list pattern)))

(provide 'doc-pymupdf-client)
;;; doc-pymupdf-client.el ends here
