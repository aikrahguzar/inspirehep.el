;;; inspirehep.el --- Emacs frontend for INSPIRE HEP -*- lexical-binding: t; -*-
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
(require 'let-alist)
(require 'seq)

;;;; Variables, groups and faces
;;;;;; Customize
(defgroup inspirehep nil "INSPIRE HEP search in Emacs." :group 'communication)

(defcustom inspirehep-download-directory nil "Directory to download pdf files in." :group 'inspirehep :type 'directory)

(defcustom inspirehep-authors-limit 10
  "Maximum number of authors to display per paper."
  :group 'inspirehep
  :type 'integer)

(defcustom inspirehep-target-buffer-function #'inspirehep-find-target
  "Function used to determine the target buffer for searches."
  :group 'inspirehep
  :type 'function)

(defcustom inspirehep-search-result-marker "● " "Indicator of a search result."
  :group 'inspirehep
  :type 'string)

(defcustom inspirehep-insert-all t
  "Determines whether all results are inserted in the buffer.
If nil we fetch only the first page of results. If non-nil all the pages
are fectched one after another and inserted into the buffer as they arrive."
  :group 'inspirehep
  :type 'boolean)

(defface inspirehep-results-header-face
  '((t :height 1.5 :weight bold :inherit font-lock-preprocessor-face))
  "Face used for general search results header in `inspirehep-mode'."
  :group 'inspirehep)

;;;;;; defconst, defvar and defvar-local
(defconst inspirehep--selection-mode-name-base "INSPIRE search results")

(defvar inspirehep--download-queue nil "Store the files being downloaded.")

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
    (define-key map (kbd "e") #'inspirehep-view-entry)
    (define-key map (kbd "d") #'inspirehep-download-pdf)
    (define-key map (kbd "i") #'inspirehep-insert-bibtex)
    (define-key map (kbd "O") #'inspirehep-show-details-all)
    (define-key map (kbd "H") #'inspirehep-hide-details-all)
    map)
  "Keybindings for Inspirehep search results.")

(defvar inspirehep--search-history nil)

(defvar inspirehep-search-parameters '((fields "titles.title" "authors.full_name" "authors.ids" "abstracts" "arxiv_eprints" "documents.url" "texkeys"
                                               "preprint_date" "imprints.date")
                                       (size   . 25) (sort   . "mostrecent"))
  "Determines the number of results, sort order and fields retrieved.")

(defvar inspirehep--time-stamps nil "Variable for rate limiting requests.")

(defvar-local inspirehep--target-buffer nil
  "Buffer into which BibTeX entries should be inserted.
This variable is local to each search results buffer.")

(defvar-local inspirehep--references-to-insert nil
  "Local variable containing references yet to be inserted for a single record.")

(defvar-local inspirehep--search-data nil
  "Keywords that led to a page of inspirehepgraphic search results.")

(defvar-local inspirehep--link-next nil
  "Link to the next page of search results.")

(defvar-local inspirehep-bibtex-entries nil "Stores the bibtex entries obtained from inspirehep for the current query.")

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

(defun inspirehep--throw-on-unexpected-errors (errors)
  "Throw an url-error for any error in ERRORS."
  (dolist (err errors)
    (cond ((eq (car err) 'url-queue-timeout) (signal 'inspirehep--url-error 'timeout))
          (t (signal 'inspirehep--url-error err)))))

(defun inspirehep--plist-get-all (plist prop)
  "Get all the values for PROP in PLIST ."
  (let (res) (while plist (if (eq prop (pop plist)) (push (pop plist) res) (pop plist)))))

(defun inspirehep-map-apply (map keys func &rest args)
  "Return FUNC applied to value of nested KEYS and ARGS in MAP unless nill."
  (let ((val (map-nested-elt map keys))) (when val (apply func val args))))

(defun inspirehep--extract-errors (events)
  "Extract errors from EVENTS."
  (seq-map #'cdr (inspirehep--plist-get-all events :error)))

(defun inspirehep-find-target ()
  "Find the target file using the major mode of current buffer."
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

(defmacro inspirehep-make-overlay (invisible &rest body)
  "Put the overlay indacting details over text inserted by BODY.
Set the invisible property of the overlay to INVISIBLE."
  (let ((beg-var (make-symbol "beg"))
        (ov-var (make-symbol "ov")))
    `(let ((,beg-var (point)))
       ,@body
       (let ((,ov-var (make-overlay ,beg-var (point))))
         (overlay-put ,ov-var 'details t) (overlay-put ,ov-var 'invisible ,invisible) ,ov-var))))

(defun inspirehep-details-overlay () "Return the details overlay of the entry at point."
       (seq-find (lambda (ov) (overlay-get ov 'details)) (overlays-at (1- (cdr (get-text-property (point) 'inspirehep-entry-bounds))))))

;;;; Interaction
(defun inspirehep--selection-move (move-fn search-fn)
  "Move using MOVE-FN, then call SEARCH-FN and go to first match."
  (let ((target nil))
    (save-excursion
      (funcall move-fn)
      (when (funcall search-fn (concat "^\\(?:" (regexp-quote inspirehep-search-result-marker) "\\|\\[[[:alnum:]]*]\\)") nil t)
        (setq target (match-end 0))))
    (prog1 target (when target (goto-char target)
                        (if-let ((entry-end (cdr (get-text-property target 'inspirehep-entry-bounds)))
                                 ((not (pos-visible-in-window-p entry-end))))
                            (set-window-start (selected-window) (save-excursion (beginning-of-line) (point))))))))

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
(defun inspirehep-insert-with-prefix (prefix wrap &rest strs)
  "Like INSERT with PREFIX and STRS, but also set `wrap-prefix' to WRAP."
  (declare (indent 1))
  (inspirehep--with-text-property 'wrap-prefix wrap (apply #'insert prefix strs)))

(defun inspirehep--insert-detail (prefix items newline &optional wrap)
  "Insert PREFIX followed by ITEMS, if ITEMS has non-empty entries.
If ITEMS is a list or vector, join its entries with “, ”.  If NEWLINE is
non-nil, add a newline before the main text. If WRAP is given use it as
`wrap-prefix' otherwise use spaces equal to the length of PREFIX"
  (when (or (vectorp items) (listp items))
    (setq items (string-join items ", ")))
  (unless (seq-empty-p items)
    (when newline (insert "\n"))
    (inspirehep-insert-with-prefix prefix (if wrap wrap (make-string (length prefix) ?\s)) items)))

(defun inspirehep--prepare-authors (auths)
  "Cleanup and join list of authors AUTHS."
  (let* ((authors (seq-map #'string-trim auths))
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

(defun inspirehep--insert-header (header &optional match-p)
  "Prettify and insert HEADER in current buffer.
If MATCH-P is non-nill add match to the faces."
  (when header
    (inspirehep--with-text-property 'line-spacing 0.5
      (inspirehep--with-text-property 'line-height 1.75
        (inspirehep-with-fontification (list (when match-p 'match) 'inspirehep-results-header-face)
          (insert header "\n"))))))

;;;;;; Insert resut
(defun inspirehep-insert-result-title (saved-p title year marker)
  "Insert the TITLE and YEAR for the entry.
Non-nil SAVED-P means that the entry is present in the target buffer. MARKER is
inserted at the beginning."
  (inspirehep-with-fontification (if saved-p 'match 'bold)
                                 (inspirehep-insert-with-prefix marker (make-string (string-width marker) ?\s) (inspirehep--prepare-title title year))))

(defun inspirehep-insert-result (item saved-p &optional marker details-invisible-p)
  "Print a (prepared) inspirehep search result ITEM.
With NO-SEP, do not add space after the record. Non-nil SAVED-P means that
the entry is present in the target buffer. MARKER is inserted at the beginning
and defaults to `inspirehep-search-result-marker'. Non-nil DETAILS-INVISIBLE-P
means that details overlay starts as invisible"
  (let* ((beg (point))
        (marker (or marker inspirehep-search-result-marker))
        (spaces (make-string (string-width marker) ?\s)))
    (inspirehep--with-text-property 'inspirehep-saved saved-p
     (inspirehep--with-text-property 'inspirehep-metadata item
      (let-alist item
        (inspirehep-insert-result-title saved-p .title .year marker)
        (insert "\n")
        (inspirehep-with-fontification 'font-lock-function-name-face
          (inspirehep-insert-with-prefix spaces spaces (inspirehep--prepare-authors .authors)))
        (inspirehep-make-overlay details-invisible-p
         (inspirehep--with-text-property 'line-prefix spaces
           (inspirehep--insert-detail "" .abstract t spaces))
         (inspirehep-with-fontification 'font-lock-comment-face
           (inspirehep--insert-detail (concat spaces "In: ") .container t)
           (inspirehep--insert-detail (concat spaces "Publisher: ") .publisher t)
           (inspirehep--insert-detail (concat spaces "arXiv: ") .arXiv t)
           (inspirehep--insert-detail (concat spaces "URL: ") (list (inspirehep-make-url-button .url)
                                                      (inspirehep-make-url-button .direct-url)) t))))))
    (put-text-property beg (point) 'inspirehep-entry-bounds (cons beg (point))))
  (insert "\n\n"))

(defun inspirehep-detailed-record (item saved-p)
  "Print a (prepared) inspirehep record ITEM.
Non-nil SAVED-P means that the entry is present in the target buffer.
This is meant to show a single literature record and erases the buffer."
  (inspirehep--with-text-property 'inspirehep-saved saved-p
     (inspirehep--with-text-property 'inspirehep-metadata item
      (let-alist item
        (inspirehep--insert-header (inspirehep--prepare-title .title .year) saved-p)
        (inspirehep-with-fontification 'font-lock-function-name-face
          (insert (inspirehep--prepare-authors .authors)))
        (inspirehep-with-fontification 'bold (insert "\n\n" "Abstract:"))
        (inspirehep--insert-detail "" .abstract t)
        (inspirehep-with-fontification 'font-lock-comment-face
           (inspirehep--insert-detail "In: " .container t)
           (inspirehep--insert-detail "Publisher: " .publisher t)
           (inspirehep--insert-detail "arXiv: " .arXiv t)
           (inspirehep--insert-detail "URL: " (list (inspirehep-make-url-button .url)
                                                      (inspirehep-make-url-button .direct-url)) t))))))

(defun inspirehep--insert-reference (ref parsed-refs keys) "Insert a reference REF using PARSED-REFS and KEYS."
       (let* ((rfid (map-elt ref 'recid))
              (pref (seq-find (lambda (p) (equal (map-elt p 'recid) rfid)) parsed-refs))
              (label (concat "[" (map-elt ref 'label "") "] ")))
         (if pref (prog1 t (inspirehep-insert-result pref (not (null (seq-intersection (map-elt pref 'tex-keys) keys))) label t))
           (unless rfid (prog1 t (inspirehep-with-fontification 'font-lock-comment-face
                                    (if-let (raw-ref (map-elt ref 'reference))
                                        (progn (inspirehep--insert-detail label raw-ref nil (make-string (length label) ?\s))
                                               (insert "\n\n"))
                                      (inspirehep-insert-result ref nil label t))))))))

;;;;;; Results buffer construction
(defun inspirehep--make-results-buffer ()
  "Make a new inspirehep-mode buffer."
  (let ((target-buffer (funcall inspirehep-target-buffer-function)))
    (with-current-buffer (generate-new-buffer "* INSPIRE HEP *")
      (erase-buffer) (inspirehep-mode) (setq buffer-read-only t inspirehep--target-buffer target-buffer) (current-buffer))))

(defun inspirehep-insert-results (items keys)
  "Insert result ITEMS into the current buffer.
KEYS should be the list of keys present in the target buffer."
  (seq-do (lambda (item) (inspirehep-insert-result item (not (null (seq-intersection (map-elt item 'tex-keys) keys))))) items))

(defun inspirehep-insert-results-new-query (num items keys)
  "Populate current buffer with ITEMS then display it.
KEYS should be the list of keys present in the target buffer
and NUM the total number of results."
  (inspirehep--renew-buffer)
  (inspirehep--insert-header (concat (nth 1 inspirehep--search-data) " (" (number-to-string num) " total results)"))
  (inspirehep-insert-results items keys)
  (inspirehep--selection-first)
  (pop-to-buffer (current-buffer))
  (hl-line-highlight))

(defun inspirehep--renew-buffer () "Setup buffer for a new page."
  (erase-buffer) (delete-all-overlays)
  (setq inspirehep-bibtex-entries nil inspirehep--references-to-insert nil))

(defun inspirehep-re-insert-entry-at-point (keys) "Insert the entry at point again comparing against KEYS."
       (let* ((bounds (get-text-property (point) 'inspirehep-entry-bounds))
              (saved (seq-contains-p keys (inspirehep-lookup-at-point 'identifier)))
              (inhibit-read-only t))
         (put-text-property (car bounds) (cdr bounds) 'inspirehep-saved saved)
         (setcar (get-text-property (car bounds) 'face) (if saved 'match 'bold))))

(defun inspirehep-revert-buffer (&rest _) "Revert buffer, by updating information about saved entries."
       (inspirehep--selection-first)
       (let ((current (point))
             (keys (inspirehep-target-buffer-keys)))
         (while current (inspirehep-re-insert-entry-at-point keys) (setq current (inspirehep--selection-next))))
       (inspirehep--selection-first)
       (set-buffer-modified-p nil))

;;;; Callbacks
(defun inspirehep-generic-url-callback (results-buffer parser hash callback)
  "Make an `url'-ready callback from CALLBACK.
CALLBACK is called with one argument which is the parsed data obtained by
calling PARSER without any argument on the server's response. RESULTS-BUFFER is
current when the CALLBACK is called. It is called only if HASH matches the one
in RESULTS-BUFFER."
  (lambda (events)
    (let ((target-buffer (current-buffer)))
      (unwind-protect
          (condition-case err
              (if-let* ((errors (inspirehep--extract-errors events)))
                  (progn
                    (inspirehep--throw-on-unexpected-errors errors))
                (inspirehep--beginning-of-response-body)
                (delete-region (point-min) (point))
                (let ((parsed-data (progn (inspirehep-response-as-utf-8) (funcall parser)))
                      (inhibit-read-only t))
                  (with-current-buffer results-buffer
                    (when (= hash (nth 2 inspirehep--search-data)) (funcall callback parsed-data)))))
            (error (message "Error while processing request: %S" err)))
        (kill-buffer target-buffer)))))

(defun inspirehep--search-callback (parsed-data) "Insert search results using PARSED-DATA."
  (let ((result-type (car inspirehep--search-data)))
    (inspirehep--lookup-url (car parsed-data) (current-buffer) 'bibtex)
    (pcase result-type
      ((or 'append-page 'append-all) (save-excursion (goto-char (point-max)) (inspirehep-insert-results (nth 3 parsed-data) (inspirehep-target-buffer-keys))))
      ((or 'new-page 'new-all) (inspirehep-insert-results-new-query (nth 2 parsed-data) (nth 3 parsed-data) (inspirehep-target-buffer-keys)))
      (_ (message "INSPIRE HEP: Invalid result type")))
    (setq inspirehep--link-next (nth 1 parsed-data))
    (when (or (eq result-type 'new-all) (eq result-type 'append-all))
      (if inspirehep--link-next (inspirehep-next-page 'append-all) (set-buffer-modified-p nil) (message "Done inserting the search results.")))))

(defun inspirehep--insert-single-record (parsed-data)
  "Inserted the single literature record in PARSED-DATA."
  (let* ((recids (seq-filter #'identity (seq-map (lambda (ref) (map-elt ref 'recid)) (nth 1 parsed-data))))
         (refurls (seq-map (lambda (ids) (inspirehep-query-url (concat "recid:(" (string-join ids " or ") ")") 25)) (seq-partition recids 25))))
    (when refurls (inspirehep--lookup-url (car refurls) (current-buffer) 'single-record-references))
    (inspirehep--lookup-url (nth 2 parsed-data) (current-buffer) 'bibtex)
    (inspirehep--renew-buffer)
    (inspirehep-detailed-record (car parsed-data) (seq-contains-p (inspirehep-target-buffer-keys) (map-elt (car parsed-data) 'identifier)))
    (insert "\n\n")
    (inspirehep-with-fontification 'bold (insert "References:" "\n"))
    (setq inspirehep--references-to-insert (nth 1 parsed-data))
    (setq inspirehep--link-next refurls)))

(defun inspirehep--insert-references (parsed-data)
  "Insert references in PARSED-DATA."
  (cl-callf cdr inspirehep--link-next)
  (when inspirehep--link-next (inspirehep--lookup-url (car inspirehep--link-next) (current-buffer) 'single-record-references))
  (inspirehep--lookup-url (car parsed-data) (current-buffer) 'bibtex)
  (save-excursion (let ((keys (inspirehep-target-buffer-keys))
                        (continue t))
                    (goto-char (point-max))
                    (while (and continue inspirehep--references-to-insert)
                      (if (inspirehep--insert-reference (car inspirehep--references-to-insert) (nth 3 parsed-data) keys)
                          (cl-callf cdr inspirehep--references-to-insert)
                        (setq continue nil)))))
  (unless inspirehep--link-next (set-buffer-modified-p nil) (message "Done inserting the record.")))

;;;; Searching
(defun inspirehep--lookup-url (url results-buffer result-type &optional query)
  "Display results from URL.
QUERY is used to construct the header. The RESULTS-BUFFER determines where
bibtex entries are inserted. RESULT-TYPE determines how the results are shown."
  (let* ((current (time-convert (current-time) 'integer))
         (len (length inspirehep--time-stamps))
         (elapsed (if (< 14 len) (- current (car (last inspirehep--time-stamps))) 0)))
    (if (or (> 15 len) (< 6 elapsed))
        (with-current-buffer results-buffer
          (if query (setq inspirehep--search-data (list result-type query (sxhash-equal (cons query current)))) (setcar inspirehep--search-data result-type))
          (cl-callf2 cons current inspirehep--time-stamps)
          (unless (> 15 len) (cl-callf butlast inspirehep--time-stamps))
          (let ((functions (pcase result-type ('single-record '(inspirehep--insert-single-record . inspirehep-parse-single-record))
                                              ('single-record-references '(inspirehep--insert-references . inspirehep-parse-search-results))
                                              ('bibtex '(inspirehep-merge-bibtex . inspirehep-parse-bibtex))
                                              (_ '(inspirehep--search-callback . inspirehep-parse-search-results)))))
            (url-queue-retrieve url (inspirehep-generic-url-callback results-buffer (cdr functions) (nth 2 inspirehep--search-data) (car functions)) nil t)))
      (run-with-timer (- 6 elapsed) nil #'inspirehep--lookup-url url results-buffer result-type query))))

;;;; Dealing with JSON from inspirehep
(defun inspirehep--search-parameters (&optional size sort fields) "See `inspirehep-query-url' for SIZE, SORT and FIELDS."
       (concat "&size=" (number-to-string (or size (map-elt inspirehep-search-parameters 'size)))
          "&sort=" (or sort (map-elt inspirehep-search-parameters 'sort)) "&fields=" (string-join (or fields (map-elt inspirehep-search-parameters 'fields)) ",")))

(defun inspirehep-query-url (query &optional size sort &rest fields)
  "Prepare url for an inspirehep search for QUERY.
SIZE is the number of entries per page, SORT is the sort order and the FIELDS
determines the fields from the response. If absent they are deterimend using
`inspirehep-search-parameters'."
  (concat "https://inspirehep.net/api/literature?q=" (url-hexify-string query) (inspirehep--search-parameters size sort fields)))

(defun inspirehep--author-with-id (auth) "Put AUTH with id as a text property on name."
       (let ((name (gethash "full_name" auth))
             (id  (seq-find (lambda (x) (equal "INSPIRE BAI" (gethash "schema" x))) (gethash "ids" auth))))
         (propertize (string-join (reverse (split-string name "," t " ")) " ")
                     'inspirehep-author-id (if id (gethash "value" id) name))))

(defun inspirehep-parse-entry (entry) "Parse an ENTRY retrieved from an inspirehep query."
       (let* ((metadata (or (gethash "metadata" entry) entry))
              (id (or (map-nested-elt metadata '("texkeys" 0)) (map-elt entry "id")))
              (id-end (when id (nth 1 (split-string id ":" t " "))))
              (arxiv-id (map-nested-elt metadata '("arxiv_eprints" 0 "value")))
              (title (or (map-nested-elt metadata '("titles" 0 "title")) (map-nested-elt metadata '("title" "title")) ""))
              (date (or (map-elt metadata "preprint_date") (map-nested-elt metadata '("imprints" 0 "date"))))
              (auths (seq-map (lambda (x) (split-string (gethash "full_name" x) "," t " ")) (gethash "authors" metadata)))
              (auths-last (if (> (length auths) 4) (concat (caar auths) " et al") (string-join (seq-map #'car auths) ", "))))
         (list (cons 'identifier id)
               (cons 'title title)
               (cons 'year (when date (truncate-string-to-width date 4)))
               (cons 'authors (seq-map #'inspirehep--author-with-id (gethash "authors" metadata)))
               (cons 'tex-keys (map-elt metadata "texkeys"))
               (cons 'arXiv (seq-map (lambda (e) (concat (map-nested-elt e '("categories" 0)) ":" (gethash "value" e))) (gethash "arxiv_eprints" metadata)))
               (cons 'url (inspirehep-map-apply entry '("id") (lambda (id) (concat "https://inspirehep.net/literature/" id))))
               (cons 'recid (gethash "id" entry))
               (cons 'arxiv-url (when arxiv-id (concat "https://arxiv.org/abs/" arxiv-id)))
               (cons 'direct-url (if arxiv-id (concat "https://export.arxiv.org/pdf/" arxiv-id) (map-nested-elt metadata '("documents" 0 "url"))))
               (cons 'citations-url (inspirehep-map-apply entry '("links" "citations") #'concat (inspirehep--search-parameters)))
               (cons 'abstract  (map-elt (let ((as (map-elt metadata "abstracts"))) (map-elt as (1- (length as)))) "value"))
               (cons 'filename (string-replace "\\" "" (concat auths-last " - " (substring title 0 (min 70 (length title))) " - " id-end ".pdf"))))))

(defun inspirehep-parse-reference (ref) "Parse the reference REF."
       (if-let ((url (map-nested-elt ref '("record" "$ref"))))
           (list (cons 'recid (when url (url-file-nondirectory url))) (cons 'label (map-nested-elt ref '("reference" "label"))))
         (let* ((reference (map-elt ref "reference"))
                (label (map-elt reference "label")))
           (if (or (and (map-contains-key reference "title") (map-contains-key reference "authors")) (not (map-contains-key ref "raw_refs")))
               (append (inspirehep-parse-entry reference) (list (cons 'label label)))
             (let* ((raw (map-nested-elt ref '("raw_refs" 0 "value") ""))
                    (authorlist (seq-map #'inspirehep--author-with-id (map-elt reference "authors")))
                    (start-pos (when (and raw label) (1+ (string-search " " raw (string-search label raw))))))
               (list (cons 'reference (propertize (substring raw start-pos) 'inspirehep-metadata (list (cons 'authors authorlist)))) (cons 'label label)))))))

(defun inspirehep-parse-single-record () "Parse single literature records from inpire."
       (let ((json (json-parse-buffer)))
         (list (inspirehep-parse-entry json) (seq-map #'inspirehep-parse-reference (map-nested-elt json '("metadata" "references")))
               (map-nested-elt json '("links" "bibtex")))))

(defun inspirehep-target-buffer-keys () "Key present in the target buffer."
       (seq-map #'car (when inspirehep--target-buffer (with-current-buffer inspirehep--target-buffer (bibtex-parse-keys)))))

(defun inspirehep-make-entries (json) "Parse a JSON response from inspirehep."
        (list (map-nested-elt json '("links" "bibtex")) (map-nested-elt json '("links" "next")) (map-nested-elt json '("hits" "total"))
             (seq-map #'inspirehep-parse-entry (map-nested-elt json '("hits" "hits")))))

(defun inspirehep-parse-search-results () "Extract structured data from inspirehep response." (inspirehep-make-entries (json-parse-buffer)))

;;;; Dealing with BibTeX from inspirehep
(defun inspirehep-parse-bibtex () "Parse bibtex entries retrieved from inspirehep."
       (seq-map (lambda (entry) (let ((beg (1+ (string-match "{" entry))) (end (string-match "," entry)))
                             (cons (substring-no-properties entry beg end) (concat "\n@" entry)))) (split-string (buffer-string) "@" t "\n")))

(defun inspirehep-merge-bibtex (bibtex) "Add entries in BIBTEX to `inspirehep-bibtex-entries'." (cl-callf2 map-merge 'hash-table inspirehep-bibtex-entries bibtex))

;;;; Downloading Files
(defun inspirehep-download (url newname &optional count)
  "Copy URL to NEWNAME asynchronously. Both arguments must be strings.
Signal a `file-already-exists' error if file NEWNAME already exists.
COUNT is used to keep track of retries. Download is aborted after 5 retries."
  (if (file-exists-p newname) (signal 'file-already-exists (list "File already exists" newname))
    (if inspirehep--download-queue (cl-callf2 cons (list url newname) inspirehep--download-queue)
      (cl-callf2 cons (list url newname) inspirehep--download-queue) (inspirehep--download (if count count 1) url newname))))

(defun inspirehep--download (count url newname)
  "Helper for `inspirehep-download'. See it for COUNT URL and NEWNAME."
       (if (> count 5) (progn (message "Failed to download %s" url) (inspirehep--downlaod-next))
         (url-queue-retrieve url #'inspirehep--download-callback (list count url newname) t)))

(defun inspirehep--downlaod-next () "Download the next file in queue."
       (cl-callf butlast inspirehep--download-queue)
       (when inspirehep--download-queue (apply #'inspirehep--download 1 (car (last inspirehep--download-queue)))))

(defun inspirehep--download-callback (status count url newname)
  "Callback for `inspirehep-download`.
See it for COUNT URL and NEWNAME. STATUS is as in `url-retrieve'."
       (if-let ((errors (plist-get status :error)))
           (progn (princ errors) (run-with-timer 12 nil #'inspirehep--download (1+ count) url newname))
         (if-let ((handle (mm-dissect-buffer t))
                  ((equal (mm-handle-media-type handle) "application/pdf"))
                  (mm-attachment-file-modes (default-file-modes)))
             (progn (mm-save-part-to-file handle newname) (kill-buffer) (mm-destroy-parts handle))
           (run-with-timer 12 nil #'inspirehep-download url newname (1+ count))))
       (inspirehep--downlaod-next))

;;;; Interactive Commands
;;;;;; Search
;;;###autoload
(defun inspirehep-lookup (&optional query all-p)
  "Perform a search for QUERY.
If ALL-P is non-nil insert all results otherwise only the first page."
  (interactive (list (read-string "InspireHEP: " nil 'inspirehep--search-history) inspirehep-insert-all))
  (inspirehep--lookup-url (inspirehep-query-url query) (if (eq major-mode #'inspirehep-mode) (current-buffer) (inspirehep--make-results-buffer))
                          (if all-p 'new-all 'new-page) (concat "INSPIRE HEP search results for " "“" query "”")))

(defun inspirehep-next-page (result-type)
  "Insert the next page of search results for the current query.
If RESULT-TYPE is `append-page` or `append-all` insert them at the end of
current buffer. Otherwise insert them after earsing the buffer. If RESULT-TYPE
is `append-all` insert all pages one by one."
  (interactive (list 'append-page))
  (if-let (((eq major-mode #'inspirehep-mode)) (url inspirehep--link-next))
      (inspirehep--lookup-url url (current-buffer) result-type)
    (message "Already at the last page of current search")))

(defun inspirehep-search-citations (all-p)
  "Get the citations for the current selection.
If ALL-P is non-nil insert all results otherwise only the first page."
  (interactive (list (when inspirehep-insert-all 'all)))
       (inspirehep--lookup-url (inspirehep-lookup-at-point 'citations-url) (current-buffer) (if all-p 'new-all 'new-page)
                               (concat "Citations for " "“" (inspirehep-lookup-at-point 'title) "”")))

(defun inspirehep-search-author (all-p)
  "Search the articles by the author at point.
If ALL-P is non-nil insert all results otherwise only the first page."
  (interactive (list inspirehep-insert-all))
  (let* ((author-at-point (get-text-property (point) 'inspirehep-author-id))
         (auth (if author-at-point author-at-point
                 (let* ((candidates  (inspirehep-lookup-at-point 'authors))
                        (candidate (if (cdr candidates) (completing-read "Select an Author: " candidates  nil t) (car candidates))))
                   (get-text-property 0 'inspirehep-author-id (car (member candidate candidates)))))))
    (inspirehep--lookup-url (inspirehep-query-url (concat "a " auth)) (current-buffer) (if all-p 'new-all 'new-page)
                            (concat "Articles by " "“" auth "”"))))

(defun inspirehep-view-entry (recid) "View the details of the literature record with id RECID." (interactive (list (inspirehep-lookup-at-point 'recid)))
       (inspirehep--lookup-url (concat "https://inspirehep.net/api/literature/" recid) (current-buffer) 'single-record (concat "recid:" recid)))

;;;;;; Bibtex and PDF
(defun inspirehep-insert-bibtex (key) "Insert the bibtex corresponding to KEY in the target buffer unless SAVED ."
       (interactive (list (inspirehep-lookup-at-point 'identifier)))
       (when-let ((bib (map-elt inspirehep-bibtex-entries key))
                  (target (or inspirehep--target-buffer (call-interactively #'inspirehep-select-target-buffer)))
                  ((not (get-text-property (point) 'inspirehep-saved))))
         (with-current-buffer target (goto-char (point-max)) (insert bib) (save-buffer)))
       (inspirehep-re-insert-entry-at-point (inspirehep-target-buffer-keys)))

(defun inspirehep-download-pdf (url name) "Download the pdf from URL .
The file is stored as NAME in the directory `inspirehep-download-directory'"
       (interactive (list (inspirehep-lookup-at-point 'direct-url) (concat inspirehep-download-directory (inspirehep-lookup-at-point 'filename))))
       (inspirehep-download url name))

(defun inspirehep-insert-and-download () "Insert the current bibtex entry and download the pdf."
       (interactive)
       (with-demoted-errors "Downlaod: %S" (call-interactively #'inspirehep-download-pdf)) (call-interactively #'inspirehep-insert-bibtex))

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
    (user-error "This record does not contain a direct URL")))

(defun inspirehep--selection-next () "Move to next search result." (interactive)
  (or (inspirehep--selection-move #'end-of-line #'re-search-forward) (goto-char (point-max)))
  (when (pos-visible-in-window-p (point-max)) (save-excursion (goto-char (point-max)) (recenter -2))))

(defun inspirehep--selection-previous () "Move to previous search result." (interactive)
  (or (inspirehep--selection-move #'beginning-of-line #'re-search-backward) (goto-char (point-min))))

;;;;;; Hiding and showing details
(defun inspirehep-toggle-details (ov) "Toggle the visibility of details overlay OV for current entry." (interactive (list (inspirehep-details-overlay)))
       (overlay-put ov 'invisible (not (overlay-get ov 'invisible))))

(defun inspirehep-hide-details-all () "Hide the details for all entries." (interactive)
       (seq-do (lambda (ov) (when (overlay-get ov 'details) (overlay-put ov 'invisible t))) (overlays-in (point-min) (point-max))))

(defun inspirehep-show-details-all () "Show the details for all entries." (interactive)
       (seq-do (lambda (ov) (when (overlay-get ov 'details) (overlay-put ov 'invisible nil))) (overlays-in (point-min) (point-max))))

;;;;;; Miscellaneous
(defun inspirehep-select-target-buffer (buffer-name)
  "Change buffer in which BibTeX results will be inserted.
BUFFER-NAME is the name of the new target buffer."
  (interactive (list (read-buffer "Buffer to insert entries into: " nil t
                                  (lambda (b) (eq 'bibtex-mode (buffer-local-value 'major-mode (get-buffer (if (stringp b) b (car b)))))))))
  (let ((buffer (get-buffer buffer-name)))
    (if (buffer-local-value 'buffer-read-only buffer)
        (user-error "%s is read-only" (buffer-name buffer))
      (setq inspirehep--target-buffer buffer)
      (revert-buffer))))

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
