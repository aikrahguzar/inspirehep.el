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

;;;; Variables
(defvar inspirehep-pdf--popup-windows nil)

;;;; Minor mode
;;;###autoload
(define-minor-mode inspirehep-pdf-record-mode "Minor mode for viewing the INSPIRE record of an article alongside its pdf."
  :global t :lighter "PDF with INSPIRE record" :group 'inspirehep
    (if (not inspirehep-pdf-record-mode) (progn (remove-hook 'window-buffer-change-functions #'inspirehep-pdf-show-record)
                                              (remove-hook 'kill-buffer-hook #'inspirehep-pdf--kill-record)
                                              (when (window-live-p (cdr inspirehep-pdf--popup-windows)) (delete-window (cdr inspirehep-pdf--popup-windows))))
    (inspirehep-pdf-show-record) (add-hook 'window-buffer-change-functions #'inspirehep-pdf-show-record)
    (add-hook 'kill-buffer-hook #'inspirehep-pdf--kill-record)))

(defun inspirehep-pdf-show-record (&rest _) "Show the correct record in the popup window."
       (if-let (((eq major-mode #'pdf-view-mode))
                (buf-name (inspirehep-pdf-get-record)))
           (progn (setq inspirehep-pdf--popup-windows (cons (selected-window) (get-buffer-window buf-name)))
                  (pdf-view-redisplay (car inspirehep-pdf--popup-windows)))
         (when (and (equal (selected-window) (car inspirehep-pdf--popup-windows)) (window-live-p (cdr inspirehep-pdf--popup-windows)))
           (delete-window (cdr inspirehep-pdf--popup-windows)))))

(defun inspirehep-pdf--kill-record () "Kill the buffer showing inspirehep record for PDF."
       (when-let (((eq major-mode #'pdf-view-mode))
                  (buf-name (concat "* INSPIREHEP PDF *" (buffer-name)))
                  ((get-buffer buf-name)))
         (kill-buffer buf-name)))

;;;; Searching from a PDFView buffer
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
(defun inspirehep-pdf-search (char str) "Search inspire for the selected STR choosing type based on CHAR."
       (interactive (list (read-char "Search for selections as:  [a] Author   [d] DOI   [e] Edit   [t] Title   [x] arXiv id  (Otherwise freeform)")
                          (car (pdf-view-active-region-text))))
       (if (equal char ?e) (inspirehep-lookup (read-string "InspireHEP: " str))
         (inspirehep-lookup (concat (pcase char (?a "a \"") (?d "doi:") (?t "t \"") (?x "arxiv:") (_ "")) str "\"")))
       (pdf-view-deactivate-region))

;;;###autoload
(defun inspirehep-pdf-get-record () "Get the inspire record using the arxiv id on the first page." (interactive)
       (if-let ((buf-name (concat "* INSPIREHEP PDF *" (buffer-name)))
                (buffer (get-buffer buf-name)))
           (progn (display-buffer buf-name) buf-name)
         (if-let ((str (map-elt (car (pdf-info-search-string "arXiv" 1)) 'text))
                  (beg (+ 6 (string-match "arXiv" str)))
                  (id (substring-no-properties str beg (string-match "v" str beg)))
                  (url (concat "https://inspirehep.net/api/arxiv/" id)))
             (progn (inspirehep--lookup-url url 'single-record (concat "arxiv:" id) (inspirehep--make-results-buffer buf-name))
                    (display-buffer buf-name) buf-name)
           (message "No arxiv id found for the current PDF.") nil)))

;;;; Acting on the buffer showing the record
(defun inspirehep-pdf--with-record-buffer (interactive-p func &rest args)
  "Call FUNC with ARGS on the buffer showing inspire record.
The call is interactive if INTERACTIVE-P is non-nill."
  (with-current-buffer (concat "* INSPIREHEP PDF *" (buffer-name)) (if interactive-p (call-interactively func) (funcall func args))))

(defun inspirehep-pdf-jump-to-reference (ref) "Jump to reference REF in the record buffer."
       (interactive (list (pcase current-prefix-arg
                            (0 (string-join (pdf-view-active-region-text)))
                            ((pred numberp) (number-to-string current-prefix-arg))
                            (_ (read-string "Jump to reference:")))))
       (inspirehep-pdf--with-record-buffer nil #'inspirehep-jump-to-reference ref))

(defun inspirehep-pdf-insert-bibtex () "Insert bibtex for the currently selected entry into the target buffer."
       (inspirehep-pdf--with-record-buffer t #'inspirehep-insert-bibtex))

(defun inspirehep-pdf-download-pdf () "Download the pdf for the currently selected entry."
       (inspirehep-pdf--with-record-buffer t #'inspirehep-download-pdf))

(defun inspirehep-pdf-insert-and-download () "Download the pdf and insert bibtex for the currently selected entry."
       (inspirehep-pdf--with-record-buffer t #'inspirehep-insert-and-download))

(provide 'inspirehep-pdf)
;;; inspirehep-pdf.el ends here
