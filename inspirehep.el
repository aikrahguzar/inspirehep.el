;;; inspirehep.el --- Inspirehep backend for inspirehep -*- lexical-binding: t; -*-
;;
;; Created: January 18, 2022
;; Modified: January 18, 2022
;; Version: 0.0.1
;; Keywords: bib
;; Url: https://github.com/aikrahguzar/inspirehep.el
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;Use the JSON api provided by inspirehep to integrate it with inspirehep.el
;;
;;
;;; Code:

;;;; Packages
(require 'bibtex)
(require 'reftex)
(require 'mm-decode)
(require 'browse-url)
(require 'hl-line)
(require 'json)
(require 'url-queue)
(require 'reftex)
(require 'let-alist)
(require 'seq)

;;;; Variables, groups and faces
;;;;;; Customize
(defgroup inspirehep nil "inspirehep in emacs using inspirehep.el" :group 'communication)

(defcustom inspirehep-download-directory nil "Directory to download pdf files in" :group 'inspirehep :type 'directory)

(defcustom inspirehep-authors-limit 10
  "Maximum number of authors to display per paper."
  :group 'inspirehep
  :type 'integer)

(defcustom inspirehep-target-buffer-function #'inspirehep-find-target
  "Function used to determine the target buffer for searches"
  :group 'inspirehep
  :type 'function)

(defcustom inspirehep-search-result-marker "● " "Indicator of a search result."
  :group 'inspirehep
  :type 'string)

(defcustom inspirehep-insert-all t "Determines whether all results are inserted in the buffer.
If `nil' we fetch only the first page of results. If `non-nil' all the pages
are fectched one after another and inserted into the buffer as they arrive."
  :group 'inspirehep
  :type 'boolean)

(defface inspirehep-results-header-face
  '((t :height 1.5 :weight bold :inherit font-lock-preprocessor-face))
  "Face used for general search results header in `inspirehep-mode'."
  :group 'inspirehep)

;;;;;; defconst, defvar and defvar-local
(defconst inspirehep--selection-mode-name-base "INSPIRE search results")

(defvar inspirehep--download-queue nil "Store the files being downloaded")

(defvar inspirehep-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'inspirehep--selection-previous)
    (define-key map (kbd "C-p") #'inspirehep--selection-previous)
    (define-key map (kbd "<down>") #'inspirehep--selection-next)
    (define-key map (kbd "C-n") #'inspirehep--selection-next)
    (define-key map (kbd "RET") #'inspirehep--selection-browse)
    (define-key map (kbd "<C-return>") #'inspirehep--selection-browse-direct)
    (define-key map (kbd "C-RET") #'inspirehep--selection-browse-direct)
    (define-key map (kbd "b") #'inspirehep-select-target-buffer)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "r") #'inspirehep-lookup)
    (define-key map (kbd "c") #'inspirehep-search-citations)
    (define-key map (kbd "a") #'inspirehep-search-author)
    (define-key map (kbd "d") #'inspirehep-download-pdf)
    (define-key map (kbd "i") #'inspirehep-insert-bibtex)
    (define-key map (kbd "O") #'inspirehep-show-details-all)
    (define-key map (kbd "H") #'inspirehep-hide-details-all)
    map)
  "Keybindings for Inspirehep search results.")

(defvar inspirehep--search-history nil)

(defvar inspirehep-search-parameters "&size=25&sort=mostrecent&fields=titles.title,authors.full_name,authors.ids,abstracts,arxiv_eprints,documents.url,texkeys"
  "Determines the number of results, sort order and fields retrieved.")

(defvar inspirehep--time-stamps nil "Variable for rate limiting requests.")

(defvar-local inspirehep--target-buffer nil
  "Buffer into which BibTeX entries should be inserted.
This variable is local to each search results buffer.")

(defvar-local inspirehep--search-terms nil
  "Keywords that led to a page of inspirehepgraphic search results.")

(defvar-local inspirehep--backend nil
  "Backend that produced a page of inspirehepgraphic search results.")

(defvar-local inspirehep--link-next nil
  "Links to the next page of search results")

(defvar-local inspirehep-bibtex-entries nil "Stores the bibtex entries obtained from inspirehep for the current query")

;;;; Major mode
(defun inspirehep--selection-mode-name ()
  "Compute a modeline string for `inspirehep-mode'."
  (concat inspirehep--selection-mode-name-base
          (if (bufferp inspirehep--target-buffer)
              (format " (→ %s)"
                      (buffer-name inspirehep--target-buffer))
            "")))

(define-derived-mode inspirehep-mode fundamental-mode inspirehep--selection-mode-name-base
  "Browse inspirehepgraphic search results.
\\{inspirehep-mode-map}"
  (hl-line-mode 1)
  (visual-line-mode)
  (setq-local truncate-lines nil)
  (setq-local outline-regexp inspirehep-search-result-marker)
  (setq-local buffer-read-only t)
  (setq-local mode-name '(:eval (inspirehep--selection-mode-name)))
  (setq-local revert-buffer-function #'inspirehep-revert-buffer))

;;;; Utilities

(defun inspirehep--beginning-of-response-body ()
  "Move point to beginning of response body."
  (goto-char (point-min))
  (unless (re-search-forward "^\n" nil t)
    (error "Invalid response from server: %S" (buffer-string))))

(defun inspirehep-response-as-utf-8 ()
  "Extract body of response."
  (set-buffer-multibyte t)
  (decode-coding-region (point) (point-max) 'utf-8 t))

(eval-and-compile
  (define-error 'inspirehep--url-error "URL retrieval error."))

(defun inspirehep--throw-on-unexpected-errors (errors allowed-errors)
  "Throw an url-error for any error in ERRORS not in ALLOWED-ERRORS."
  (dolist (err errors)
    (cond ((eq (car err) 'url-queue-timeout)
           (signal 'inspirehep--url-error 'timeout))
          ((not (member err allowed-errors))
           (signal 'inspirehep--url-error err)))))

(defun inspirehep--plist-get-all (plist prop)
  "Get all the values for PROP in PLIST"
  (let (res)
    (while plist
      (if (eq prop (pop plist))
          (push (pop plist) res)
        (pop plist)))
    (nreverse res)))

(defun inspirehep--extract-errors (events)
  "Extract errors from EVENTS."
  (seq-map #'cdr (inspirehep--plist-get-all events :error)))

(defun inspirehep-generic-url-callback (callback &rest allowed-errors)
  "Make an `url'-ready callback from CALLBACK.
CALLBACK is called with no arguments; the buffer containing the
server's response is current at the time of the call, and killed
after the call returns.  Call CLEANUP-FUNCTION before checking
for errors.  If the request returns one of the errors in
ALLOWED-ERRORS, CALLBACK is instead called with one argument, the
list of allowed errors that occurred instead of a buffer.  If the
request returns another error, an exception is raised."
  (lambda (events)
    (let ((target-buffer (current-buffer)))
      (unwind-protect
          (progn
            (condition-case err
                (if-let* ((errors (inspirehep--extract-errors events)))
                    (progn
                      (inspirehep--throw-on-unexpected-errors errors allowed-errors)
                      (funcall callback errors))
                  (inspirehep--beginning-of-response-body)
                  (delete-region (point-min) (point))
                  (funcall callback))
              (error (message "Error while processing request: %S" err))))
        (kill-buffer target-buffer)))))

(defun inspirehep-find-target ()
  "Find the target file using the major mode of current buffer"
  (pcase major-mode
    ('latex-mode (find-file-noselect (progn (reftex-access-scan-info t)
                                            (ignore-errors (car (reftex-get-bibfile-list))))))
    ('bibtex-mode (current-buffer))
    ('inspirehep-mode inspirehep--target-buffer)))

(defmacro inspirehep--with-text-property (prop value &rest body)
  "Set PROP to VALUE on text inserted by BODY."
  (let ((beg-var (make-symbol "beg")))
    `(let ((,beg-var (point)))
       ,@body
       (put-text-property ,beg-var (point) ,prop ,value))))

(defmacro inspirehep-with-fontification (face &rest body)
  "Apply FACE to text inserted by BODY."
  (let ((beg-var (make-symbol "beg")))
    `(let ((,beg-var (point)))
       ,@body
       (font-lock-append-text-property ,beg-var (point) 'face ,face))))

(defmacro inspirehep-make-overlay (&rest body)
  "Put the overlay indacting details over text inserted by BODY ."
  (let ((beg-var (make-symbol "beg")))
    `(let ((,beg-var (point)))
       ,@body
       (overlay-put (make-overlay ,beg-var (point)) 'details t))))

(defun inspirehep-details-overlay () "Return the details overlay of the entry at point"
       (seq-find (lambda (ov) (overlay-get ov 'details)) (overlays-at (1- (cdr (get-text-property (point) 'inspirehep-entry-bounds))))))

;;;; Interaction
(defun inspirehep--selection-move (move-fn search-fn &optional recenter-p arg)
  "Move using MOVE-FN, then call SEARCH-FN and go to first match."
  (let ((target nil))
    (save-excursion
      (funcall move-fn)
      (when (funcall search-fn (concat "^" inspirehep-search-result-marker) nil t)
        (setq target (match-end 0))))
    (when target (goto-char target)))
  (when recenter-p (recenter arg)))

(defun inspirehep--selection-first ()
  "Move to first search result."
  (goto-char (point-min))
  (inspirehep--selection-move #'ignore #'re-search-forward))

(defun inspirehep--selection-metadata-at-point ()
  "Return the metadata of the entry at point."
  (or (get-text-property (point) 'inspirehep-metadata)
      (user-error "No entry at point")))

(defun inspirehep-lookup-at-point (key) "Lookup the value of KEY in the metadata for the entry at point."
       (map-elt (inspirehep--selection-metadata-at-point) key))

;;;; Printing search results
(defun inspirehep-insert-with-prefix (prefix &rest strs)
  "Like INSERT with PREFIX and STRS, but set `wrap-prefix'.
That is, the inserted text gets a `wrap-prefix' made of enough
white space to align with the end of PREFIX."
  (declare (indent 1))
  (inspirehep--with-text-property 'wrap-prefix (make-string (length prefix) ?\s)
    (apply #'insert prefix strs)))

(defun inspirehep--insert-detail (prefix items newline)
  "Insert PREFIX followed by ITEMS, if ITEMS has non-empty entries.
If ITEMS is a list or vector, join its entries with “, ”.  If
NEWLINE is non-nil, add a newline before the main text."
  (when (or (vectorp items) (listp items))
    (setq items (string-join items ", ")))
  (unless (seq-empty-p items)
    (when newline (insert "\n"))
    (inspirehep-insert-with-prefix prefix items)))

(defun inspirehep--prepare-authors (authors)
  "Cleanup and join list of AUTHORS."
  (let* ((authors (seq-map #'string-trim authors))
         (num-authors (length authors)))
    ;; Only truncate when significantly above limit
    (when (> num-authors (+ 2 inspirehep-authors-limit))
      (let* ((last (nthcdr inspirehep-authors-limit authors)))
        (setcar last (format "… (%d more)" (- num-authors inspirehep-authors-limit)))
        (setcdr last nil)))
    (if authors (string-join authors ", ")
      "(no authors)")))

(defun inspirehep--prepare-title (title &optional year)
  "Cleanup TITLE and add YEAR for presentation to the user."
  (concat (if-let ((clean-title (when title (string-clean-whitespace title)))
                   (seq-empty-p title))
              title "(no title)")
          (if year (format " [%s]" year) "")))

(defun inspirehep--browse-url (button)
  "Open web browser on page pointed to by BUTTON."
  (browse-url (button-get button 'target)))

(defun inspirehep-make-url-button (url &optional label)
  "Make a text button pointing to URL.
With non-nil LABEL, use that instead of URL to label the button."
  (unless (seq-empty-p url)
    (with-temp-buffer
      (insert-text-button (or label url)
                          'target url
                          'follow-link t
                          'action #'inspirehep--browse-url)
      (buffer-string))))

(defun inspirehep--search-results-header (&optional loading-p)
  "Compute a header for the current `selection-mode' buffer.
With LOADING-P, mention that results are being loaded."
  (concat (if (cdr inspirehep--search-terms)
            (cdr inspirehep--search-terms)
            (concat "INSPIRE HEP search results for " "“" (car inspirehep--search-terms) "”"))
          (if loading-p " (loading…)" "")))

(defun inspirehep--insert-header (header)
  "Prettify and insert HEADER in current buffer."
  (when header
    (inspirehep--with-text-property 'line-spacing 0.5
      (inspirehep--with-text-property 'line-height 1.75
        (inspirehep-with-fontification 'inspirehep-results-header-face
          (insert header "\n"))))))

;;;;;; Insert resut
(defun inspirehep-insert-result-title (saved title year) "Insert the TITLE and YEAR for the entry"
       (inspirehep-with-fontification (if saved 'match 'bold)
          (inspirehep-insert-with-prefix inspirehep-search-result-marker (inspirehep--prepare-title title year))))

(defun inspirehep-insert-result (item saved-p &optional no-sep)
  "Print a (prepared) inspirehepgraphic search result ITEM.
With NO-SEP, do not add space after the record.

This command expects ITEM to be a single alist, in the following format:

  ((title . \"Title of entry\")
   (authors . (\"Author 1\" \"Author 2\" …))
   (container . \"Where this was published (which journal, conference, …)\")
   (type . \"Type of document (journal paper, proceedings, report, …)\")
   (category . \"Category of this document (aka primary topic)\")
   (publisher . \"Publisher of this document\")
   (references . \"Identifier(s) of this document (DOI, DBLP id, Handle, …)\")
   (open-access-status . \"Open access status of this document\")
   (url . \"Relevant URL\")
   (year . \"Publication year as a string, if available\")
   (direct-url . \"Direct URL of paper (typically PDF)\"))

Each of `container', `type', `category', `publisher',
`references', and `open-access-status' may be a list; in that
case, entries of the list are displayed comma-separated.  All
entries are optional.

`crossref--extract-interesting-fields' and `dblp--extract-interesting-fields'
provide examples of how to build such a result."
  (let ((beg (point)))
    (inspirehep--with-text-property 'inspirehep-saved saved-p
     (inspirehep--with-text-property 'inspirehep-metadata item
      (let-alist item
        (inspirehep-insert-result-title saved-p .title .year)
        (insert "\n")
        (inspirehep-with-fontification 'font-lock-function-name-face
          (inspirehep-insert-with-prefix "  " (inspirehep--prepare-authors .authors)))
        (inspirehep-make-overlay
         (inspirehep--insert-detail "  " .abstract t)
         (inspirehep-with-fontification 'font-lock-comment-face
           (inspirehep--insert-detail "  In: " .container t)
           (inspirehep--insert-detail "  Publisher: " .publisher t)
           (inspirehep--insert-detail "  arXiv: " .arXiv t)
           (inspirehep--insert-detail "  URL: " (list (inspirehep-make-url-button .url)
                                                      (inspirehep-make-url-button .direct-url))
                                      t))))))
    (put-text-property beg (point) 'inspirehep-entry-bounds (cons beg (point))))
  (unless no-sep (insert "\n\n")))

;;;;;; Results buffer construction
(defun inspirehep--make-results-buffer (target-buffer search-terms &optional results-buffer)
  "Set up the results buffer for TARGET-BUFFER, SEARCH-TERMS and BACKEND."
  (with-current-buffer (get-buffer-create (or results-buffer "* INSPIRE HEP *"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (inspirehep-mode)
      (setq inspirehep--target-buffer target-buffer)
      (setq inspirehep--search-terms search-terms)
      (inspirehep--insert-header (inspirehep--search-results-header t))
      (setq buffer-read-only t)
      (current-buffer))))

(defun inspirehep-insert-results (items keys) "Insert results into the current buffer"
    (seq-do (lambda (item) (inspirehep-insert-result item (not (null (seq-intersection (map-elt item 'tex-keys) keys))))) items))

(defun inspirehep-insert-results-new-query (items keys)
  "Populate current buffer with ITEMS then display it."
    (erase-buffer)
    (inspirehep--insert-header (inspirehep--search-results-header))
    (inspirehep-insert-results items keys)
    (inspirehep--selection-first)
    (set-buffer-modified-p nil)
  (pop-to-buffer (current-buffer))
  (hl-line-highlight))

;; (defun inspirehep--display-results (&rest _)
;;   "Disaply results of a search."
;;   (inspirehep-insert-results (inspirehep-make-entries)))

(defun inspirehep--callback (results-buffer &optional insert-p insert-all-p)
  "Generate a search results callback for RESULTS-BUFFER.
Results are parsed with (BACKEND 'parse-buffer)."
  (inspirehep-generic-url-callback
   (lambda () ;; no allowed errors, so no arguments
     "Parse results of inspirehepgraphic search."
     (let ((parsed-data (inspirehep-parse-search-results))
           (inhibit-read-only t))
       (inspirehep-retrieve-bibtex results-buffer (nth 0 parsed-data))
       (with-current-buffer results-buffer
         (setq inspirehep--link-next (nth 1 parsed-data))
         (if insert-p (save-excursion (goto-char (point-max))
                                      (inspirehep-insert-results (nth 2 parsed-data) (inspirehep-target-buffer-keys)))
           (inspirehep-insert-results-new-query (nth 2 parsed-data) (inspirehep-target-buffer-keys))
           (inspirehep--selection-first))
         (when insert-all-p (inspirehep-next-page t t)))))))

(defun inspirehep-re-insert-entry-at-point (keys) "Insert the entry at point again comparing against KEYS."
       (let* ((bounds (get-text-property (point) 'inspirehep-entry-bounds))
              (saved (seq-contains-p keys (inspirehep-lookup-at-point 'identifier)))
              (inhibit-read-only t))
         (put-text-property (car bounds) (cdr bounds) 'inspirehep-saved saved)
         (setcar (get-text-property (car bounds) 'face) (if saved 'match 'bold))))

(defun inspirehep-revert-buffer (&rest _) "Revert buffer, by updating information about saved entries"
       (inspirehep--selection-first)
       (let ((current (point))
             (keys (inspirehep-target-buffer-keys)))
         (while current (inspirehep-re-insert-entry-at-point keys) (setq current (inspirehep--selection-next))))
       (inspirehep--selection-first)
       (set-buffer-modified-p nil))

;;;; Searching
(defun inspirehep--lookup-url (url query target-buffer insert-p &optional insert-all-p)
  "Display results from URL"
  (let* ((current (time-convert (current-time) 'integer))
         (len (length inspirehep--time-stamps))
         (elapsed (if (< 6 len) (- current (car (last inspirehep--time-stamps))) 0)))
    (if (or (> 7 len) (< 6 elapsed))
        (let ((results-buffer (if insert-p (current-buffer)
                                (inspirehep--make-results-buffer (funcall inspirehep-target-buffer-function) query target-buffer))))
          (cl-callf2 cons current inspirehep--time-stamps)
          (unless (> 7 len) (cl-callf butlast inspirehep--time-stamps))
          (url-queue-retrieve url (inspirehep--callback results-buffer insert-p insert-all-p)) results-buffer)
      (run-with-timer (- 6 elapsed) nil #'inspirehep--lookup-url url query target-buffer insert-p insert-all-p))))

;;;; Dealing with JSON from inspirehep
(defun inspirehep-query-url (query) "Prepare url for an inspirehep search"
       (concat "https://inspirehep.net/api/literature?q=" (url-encode-url query) inspirehep-search-parameters))

(defun inspirehep--author-with-id (auth) "Put AUTH with id as a text property on name"
       (let ((name (gethash "full_name" auth))
             (id  (seq-find (lambda (x) (equal "INSPIRE BAI" (gethash "schema" x))) (gethash "ids" auth))))
         (propertize (string-join (reverse (split-string name "," t " ")) " ")
                     'inspirehep-author-id (if id (gethash "value" id) name))))

(defun inspirehep-parse-entry (entry) "Parse an entry retrieved from an inspirehep query in the inspirehep.el format"
       (let* ((metadata (gethash "metadata" entry))
              (id (map-nested-elt metadata '("texkeys" 0)))
              (id-end (nth 1 (split-string id ":" t " ")))
              (arxiv-id (map-nested-elt metadata '("arxiv_eprints" 0 "value")))
              (title (map-nested-elt metadata '("titles" 0 "title")))
              (auths (seq-map (lambda (x) (split-string (gethash "full_name" x) "," t " ")) (gethash "authors" metadata)))
              (auths-last (if (> (length auths) 4) (concat (caar auths) " et al") (string-join (seq-map #'car auths) ", "))))
         (list (cons 'identifier id)
               (cons 'title title)
               (cons 'authors (seq-map #'inspirehep--author-with-id (gethash "authors" metadata)))
               (cons 'tex-keys (map-elt metadata "texkeys"))
               (cons 'arXiv (seq-map (lambda (e) (concat (map-nested-elt e '("categories" 0)) ":" (gethash "value" e))) (gethash "arxiv_eprints" metadata)))
               (cons 'url (concat "https://inspirehep.net/literature/" (gethash "id" entry)))
               (cons 'arxiv-url (when arxiv-id (concat "https://arxiv.org/abs/" arxiv-id)))
               (cons 'direct-url (if arxiv-id (concat "https://export.arxiv.org/pdf/" arxiv-id) (map-nested-elt metadata '("documents" 0 "url"))))
               (cons 'citations-url (concat (map-nested-elt entry '("links" "citations")) inspirehep-search-parameters))
               (cons 'abstract  (map-elt (let ((as (map-elt metadata "abstracts"))) (map-elt as (1- (length as)))) "value"))
               (cons 'filename (string-replace "\\" "" (concat auths-last " - " (substring title 0 (min 70 (length title))) " - " id-end ".pdf"))))))

(defun inspirehep-target-buffer-keys () "Key present in the target buffer."
       (seq-map #'car (when inspirehep--target-buffer (with-current-buffer inspirehep--target-buffer (bibtex-parse-keys)))))

(defun inspirehep-make-entries (json) "Parse a json response from inspirehep"
        (list (map-nested-elt json '("links" "bibtex")) (map-nested-elt json '("links" "next"))
             (seq-map #'inspirehep-parse-entry (map-nested-elt json '("hits" "hits")))))

(defun inspirehep-parse-search-results () "Extract structured data from inspirehep response."
       (inspirehep-response-as-utf-8) (inspirehep-make-entries (json-parse-buffer)))

;;;; Dealing with BibTeX from inspirehep
(defun inspirehep-parse-bibtex () "Parse bibtex entries retrieved from inspirehep"
       (seq-map (lambda (entry) (let ((beg (1+ (string-match "{" entry))) (end (string-match "," entry)))
                             (cons (substring-no-properties entry beg end) (concat "\n@" entry)))) (split-string (buffer-string) "\n@" t)))

(defun inspirehep-bibtex-callback (buffer)
  (lambda () (let ((bibtex (inspirehep-parse-bibtex))) (with-current-buffer buffer (cl-callf2 map-merge 'hash-table inspirehep-bibtex-entries bibtex)))))

(defun inspirehep-retrieve-bibtex (buffer link) "Retrieve the bibtex entries corresponding to current query"
       (url-queue-retrieve link (inspirehep-generic-url-callback (inspirehep-bibtex-callback buffer)) nil t))

;;;; Downloading Files
(defun inspirehep-download (url newname &optional count)
  "Copy URL to NEWNAME asynchronously. Both arguments must be strings.
Signal a `file-already-exists' error if file NEWNAME already
exists."
  (if (file-exists-p newname) (signal 'file-already-exists (list "File already exists" newname))
    (if inspirehep--download-queue (cl-callf2 cons (list url newname) inspirehep--download-queue)
      (cl-callf2 cons (list url newname) inspirehep--download-queue) (inspirehep--download (if count count 1) url newname))))

(defun inspirehep--download (count url newname) "Helper for `inspirehep-download'"
       (if (> count 5) (progn (message "Failed to download %s" url) (inspirehep--downlaod-next))
         (url-queue-retrieve url #'inspirehep--download-callback (list count url newname))))

(defun inspirehep--downlaod-next () "Download the next file in queue"
       (cl-callf butlast inspirehep--download-queue)
       (when inspirehep--download-queue (apply #'inspirehep--download 1 (car (last inspirehep--download-queue)))))

(defun inspirehep--download-callback (status count url newname) "Callback for inspirehep-download"
       (if-let ((errors (plist-get status :error)))
           (progn (princ errors) (run-with-timer 2 nil #'inspirehep--download (1+ count) url newname))
         (if-let ((handle (mm-dissect-buffer t))
                  ((equal (mm-handle-media-type handle) "application/pdf"))
                  (mm-attachment-file-modes (default-file-modes)))
             (progn (mm-save-part-to-file handle newname) (kill-buffer) (mm-destroy-parts handle))
           (run-with-timer 12 nil #'inspirehep-download url newname (1+ count)))
         (inspirehep--downlaod-next)))

;;;; Interactive Commands
;;;;;; Search
;;;###autoload
(defun inspirehep-lookup (&optional query insert-all-p)
  "Perform a search using BACKEND, and QUERY.
Prompt for any missing or nil arguments.  BACKEND should be a
function obeying the interface described in the docstring of
`inspirehep-backends'.  Returns the buffer in which results will be
inserted."
  (interactive (list (read-string "InspireHEP: " nil 'inspirehep--search-history) inspirehep-insert-all))
  (inspirehep--lookup-url (inspirehep-query-url query) (list query) (when (eq major-mode #'inspirehep-mode) (current-buffer)) nil insert-all-p))

(defun inspirehep-next-page (insert-p &optional insert-all-p) "Return the next page of search results for the current query" (interactive (list t))
       (if-let (((eq major-mode #'inspirehep-mode)) (url inspirehep--link-next))
           (inspirehep--lookup-url url inspirehep--search-terms (current-buffer) insert-p insert-all-p)
         (message "Already at the last page of current search")))

(defun inspirehep-search-citations (insert-all-p) "Get the citations for the current selection" (interactive (list inspirehep-insert-all))
       (inspirehep--lookup-url (inspirehep-lookup-at-point 'citations-url)
                               (cons (car inspirehep--search-terms) (concat "Citations for " "“" (inspirehep-lookup-at-point 'title) "”"))
                               (current-buffer) nil insert-all-p))

(defun inspirehep-search-author (insert-all-p) "Search the articles by the author at point" (interactive (list inspirehep-insert-all))
       (let* ((author-at-point (get-text-property (point) 'inspirehep-author-id))
              (auth (if author-at-point author-at-point
                      (let* ((candidates  (inspirehep-lookup-at-point 'authors))
                             (candidate (completing-read "Select an Author: " candidates  nil t)))
                        (get-text-property 0 'inspirehep-author-id (car (member candidate candidates)))))))
         (inspirehep--lookup-url (inspirehep-query-url (concat "a " auth))
                                 (cons (car inspirehep--search-terms) (concat "Articles by " "“" auth "”"))
                                 (current-buffer) nil insert-all-p)))

;;;;;; Bibtex and PDF
(defun inspirehep-insert-bibtex (key saved) "Insert the bibtex corresponding to KEY in the target buffer"
       (interactive (list (inspirehep-lookup-at-point 'identifier) (inspirehep-lookup-at-point 'saved)))
       (unless saved (let ((bib (map-elt inspirehep-bibtex-entries key)))
                       (with-current-buffer inspirehep--target-buffer (goto-char (point-max)) (insert bib) (save-buffer)))
               (inspirehep-re-insert-entry-at-point (inspirehep-target-buffer-keys))))

(defun inspirehep-download-pdf (url name) "Download the pdf using the metadata.
The file is stored in the directory specified by `inspirehep-download-directory'"
       (interactive (list (inspirehep-lookup-at-point 'direct-url) (concat inspirehep-download-directory (inspirehep-lookup-at-point 'filename))))
       (inspirehep-download url name))

(defun inspirehep-insert-and-download () "Insert the bibtex entry and download the pdf from METADATA"
       (interactive)
       (call-interactively #'inspirehep-download-pdf) (call-interactively #'inspirehep-insert-bibtex))

;;;;;; Moving around in buffer
(defun inspirehep--selection-browse ()
  "Open the web page of the current entry in a web browser."
  (interactive)
  (if-let* ((url (inspirehep-lookup-at-point 'url)))
      (browse-url url)
    (user-error "This record does not contain a URL")))

(defun inspirehep--selection-browse-direct ()
  "Open the full text of the current entry in a web browser."
  (interactive)
  (if-let* ((url (inspirehep-lookup-at-point 'direct-url)))
      (browse-url url)
    (user-error "This record does not contain a direct URL (try arXiv or HAL)")))

(defun inspirehep--selection-next ()
  "Move to next search result."
  (interactive)
  (inspirehep--selection-move #'end-of-line #'re-search-forward t 2))

(defun inspirehep--selection-previous ()
  "Move to previous search result."
  (interactive)
  (inspirehep--selection-move #'beginning-of-line #'re-search-backward t 2))

;;;;;; Hiding and showing details
(defun inspirehep-toggle-details (ov) "Toggle the visibility of details for current entry" (interactive (list (inspirehep-details-overlay)))
       (overlay-put ov 'invisible (not (overlay-get ov 'invisible))))

(defun inspirehep-hide-details-all () "Hide the details for all entries" (interactive)
       (seq-do (lambda (ov) (when (overlay-get ov 'details) (overlay-put ov 'invisible t))) (overlays-in (point-min) (point-max))))

(defun inspirehep-show-details-all () "Show the details for all entries" (interactive)
       (seq-do (lambda (ov) (when (overlay-get ov 'details) (overlay-put ov 'invisible nil))) (overlays-in (point-min) (point-max))))

;;;;;; Miscellaneous
(defun inspirehep-select-target-buffer (buffer-name)
  "Change buffer in which BibTeX results will be inserted.
BUFFER-NAME is the name of the new target buffer."
  (interactive (list (read-buffer "Buffer to insert entries into: ")))
  (let ((buffer (get-buffer buffer-name)))
    (if (buffer-local-value 'buffer-read-only buffer)
        (user-error "%s is read-only" (buffer-name buffer))
      (setq inspirehep--target-buffer buffer))))

(defun inspirehep-kill-buffers ()
  "Kill all `inspirehep-mode' buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (eq (buffer-local-value 'major-mode buf)
                   'inspirehep-mode))
      (kill-buffer buf))))

(provide 'inspirehep)

;;; inspirehep.el ends here
