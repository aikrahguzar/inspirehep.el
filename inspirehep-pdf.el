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
(require 'companion-mode)

;;;; Variables
(defvar inspirehep-pdf-manipulate-record-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'inspirehep-pdf-insert-bibtex)
    (define-key map (kbd "d") #'inspirehep-pdf-download-pdf)
    (define-key map (kbd "s") #'inspirehep-insert-and-download)
    (define-key map (kbd "f") #'inspirehep-pdf-search)
    (define-key map (kbd "a") #'inspirehep-pdf-search-author)
    (define-key map (kbd "c") #'inspirehep-pdf-search-citations)
    (define-key map (kbd "v") #'inspirehep-pdf-view-entry)
    (define-key map (kbd "b") #'inspirehep-pdf-select-target-buffer)
    map)
  "A keymap for commands that act on the inspirehep record from a pdf buffer.")

(defvar-local inspirehep-pdf--references-cache 'uninitialized)

;;;; Utilities
(defun inspirehep-pdf--parse-ref (str) "Parse STR into a list of references."
       (cond ((string-match-p "," str) (seq-mapcat #'inspirehep-pdf--parse-ref (split-string str "," t (rx space))))
             ((string-match-p "[–—-]" str) (let ((numlist (seq-map #'string-to-number (split-string str "[–—-]"))))
                                             (seq-map #'number-to-string (number-sequence (car numlist) (cadr numlist)))))
             (t (list str))))

(defun inspirehep-pdf--refs-on-page (page) "Find the references on the PAGE of the pdf."
       (let ((page-text (pdf-info-gettext page '(0 0 1 1)))
             (beg-pos 1) (refs))
         (while (string-match (rx space ?\[ (0+ (or alnum ?- ?– ?— ?, space)) ?\]) page-text beg-pos)
           (setq beg-pos (match-end 0))
           (cl-callf2 seq-concatenate 'list refs (inspirehep-pdf--parse-ref (string-trim (match-string 0 page-text) (rx (0+ space) ?\[) (rx (0+ space) ?\])))))
         refs))

(defun inspirehep-pdf--find-references () "Return the list of the references with annotation."
       (let ((this-page (pdf-info-number-of-pages))
             (refs nil))
         (while this-page (cl-callf2 map-merge 'hash-table refs (inspirehep-pdf--format-refs-on-page this-page))
                (setq this-page (unless (or (pdf-info-search-regexp "^[[:space:]]*References[[:space:]]*$" this-page) (= this-page 1)) (1- this-page))))
         (setq inspirehep-pdf--references-cache refs)))
;; (rxt-elisp-to-pcre (rx line-start (0+ space) "References" (0+ space) line-end))

(defun inspirehep-pdf--format-refs-on-page (page) "Format the references on the PAGE for disaply in `completing-read'."
       (let ((refs (pdf-info-search-regexp "^[[:space:]]*\\[[[:alnum:]]+\\][[:space:]]+.*$" page)))
         (seq-map (lambda (res) (let* ((text (substring-no-properties (map-elt res 'text)))
                                  (ann (progn (string-match (rx (0+ space) ?\[ (0+ alnum) ?\]) text) (substring text (match-end 0))))
                                  (key (string-trim (match-string 0 text) (rx (0+ space) ?\[) (rx (0+ space) ?\]))))
                             (cons key (concat (propertize (truncate-string-to-width key 16 nil ?\s) 'face 'bold)
                                               (propertize (string-trim ann) 'face 'font-lock-comment-face)))))
                  refs)))
;; (rxt-elisp-to-pcre (rx line-start (0+ space) ?\[ (1+ alnum) ?\] (0+ not-newline) line-end))

(defun inspirehep-pdf--select-ref (refs) "Select a reference among REFS using completing read."
       (when (eq inspirehep-pdf--references-cache 'uninitialized) (inspirehep-pdf--find-references))
         (car (split-string (completing-read "Select a reference: "
                                             (seq-map (lambda (cand) (gethash cand inspirehep-pdf--references-cache (propertize cand 'face 'bold))) refs)))))

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
       (if-let ((str (map-elt (car (pdf-info-search-string "arXiv" 1)) 'text))
                (beg (+ 6 (string-match "arXiv" str)))
                (id (substring-no-properties str beg (string-match "v" str beg)))
                (url (concat "https://inspirehep.net/api/arxiv/" id))
                (buf (get-buffer-create (concat "* INSPIREHEP PDF *" (buffer-name)))))
           (progn (with-current-buffer buf (inspirehep-mode)) (inspirehep--lookup-url url 'single-record (concat "arxiv:" id) buf) buf)
         (message "No arxiv id found for the current PDF.") nil))

;;;###autoload
(defun inspirehep-pdf-companion-hook () "Hook to add to `pdf-view-mode'."
       (companion-mode-set #'inspirehep-pdf-get-record nil t))

;;;; Acting on the buffer showing the record
(defun inspirehep-pdf--with-record-buffer (is-interactive func &rest args)
  "Call FUNC with ARGS on the buffer showing inspire record.
The call is interactive if IS-INTERACTIVE is non-nill."
  (with-selected-window (companion-mode-companion-window) (if is-interactive (call-interactively func) (apply func args))))

(defun inspirehep-pdf-jump-to-reference (ref) "Jump to reference REF in the record buffer."
       (interactive (list (pcase current-prefix-arg
                            (0 (string-join (pdf-view-active-region-text)))
                            ((pred numberp) (number-to-string current-prefix-arg))
                            (_ (inspirehep-pdf--select-ref (inspirehep-pdf--refs-on-page (pdf-view-current-page)))))))
       (inspirehep-pdf--with-record-buffer nil #'inspirehep-hide-jump-show ref t))

;;;;;; Bibtex and download
(defun inspirehep-pdf-insert-bibtex () "Insert bibtex for the currently selected entry into the target buffer." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-insert-bibtex))

(defun inspirehep-pdf-download-pdf () "Download the pdf for the currently selected entry." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-download-pdf))

(defun inspirehep-pdf-insert-and-download () "Download the pdf and insert bibtex for the currently selected entry." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-insert-and-download))

(defun inspirehep-pdf-select-target-buffer () "Select the buffer to insert bibtex entries in." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-select-target-buffer))

;;;;;; Search
(defun inspirehep-pdf-search-author () "Search an author of the selected entry." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-search-author))

(defun inspirehep-pdf-search-citations () "Search for literature citing the selected entry." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-search-citations))

(defun inspirehep-pdf-view-entry () "View detailed record for the selected entry." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-view-entry))

;;;;Misc
(defun inspirehep-pdf-toggle-details () "Toggle details for the selected entry." (interactive)
       (inspirehep-pdf--with-record-buffer t #'inspirehep-toggle-details))

(provide 'inspirehep-pdf)
;;; inspirehep-pdf.el ends here
