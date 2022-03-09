;;; inspirehep-pdf.el --- Companion to use inspirehep.el with pdf-mode -*- lexical-binding: t; -*-
;;
;; Created: March 09, 2022
;; Modified: March 09, 2022
;; Version: 0.0.1
;; Keywords: bib docs
;; Homepage: https://github.com/aikrahguzar/inspirehep.el
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; A companion package to use the search provided by inspirehep.el with the
;; pdf-mode.
;;
;;; Code:
(require 'inspirehep)
(require 'pdf-tools)
(require 'pdf-links)

;;;###autoload
(defun inspirehep-pdf-uri-action (link)
  "If the LINK is from arxiv search inspire else use default action.
This functions can be used as a replacement for `pdf-links-browse-uri-function'"
       (let* ((url (url-generic-parse-url link))
              (host (url-host url))
              (file (url-filename url))
              (arxiv-id (when (string-match-p "arxiv" host) (string-remove-prefix "/abs/" file)))
              (doi (when (string-match-p "doi" host) (string-remove-prefix "/" file))))
         (if arxiv-id (inspirehep-lookup (concat "arxiv:" arxiv-id))
           (if doi (inspirehep-lookup (concat "doi:" doi)) (pdf-links-browse-uri-default link)))))

;;;###autoload
(defun inspirehep-pdf-search-inspire (char str) "Search inspire for the selected STR choosing type based on CHAR."
       (interactive (list (read-char "Search for selections as:  [a] Author   [d] doi   [e] edit   [t] title   [x] arXiv id  (Otherwise freeform)")
                          (car (pdf-view-active-region-text))))
       (if (equal char ?e) (inspirehep-lookup (read-string "InspireHEP: " str))
         (inspirehep-lookup (concat (pcase char (?a "a \"") (?d "doi:") (?t "t \"") (?x "arxiv:") (_ "")) str "\"")))
       (pdf-view-deactivate-region))

;;;###autoload
(defun inspirehep-pdf-get-inspire-record () "Get the inspire record using the arxiv id on the first page." (interactive)
       (if-let ((buf-name (concat "* INSPIREHEP PDF *" (buffer-name)))
                (buffer (get-buffer buf-name)))
           (display-buffer buf-name)
         (if-let ((str (map-elt (car (pdf-info-search-string "arXiv" 1)) 'text))
                  (beg (+ 6 (string-match "arXiv" str)))
                  (id (substring-no-properties str beg (string-match "v" str beg))))
             (inspirehep--lookup-url (concat "https://inspirehep.net/api/arxiv/" id) (inspirehep--make-results-buffer buf-name) 'single-record (concat "arxiv:" id))
           (message "No arxiv id found for the current PDF."))))


(provide 'inspirehep-pdf)
;;; inspirehep-pdf.el ends here
