;;; org-roam-fz.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fz
;; Version: 0.12.3
;; Keywords: org-roam, convenience
;; Package-Requires: ((emacs "30.1") (org-roam "20250218.1722"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package provides the Folgezettel support for Org Roam.
;;
;; A note on terminology: Within the code base, Zettelkasten (the first letter
;; capitalized) refers to the knowlege management method, while zettelkasten
;; (the first letter uncapitalized) refers to a slip box/collection of notes.
;;
;;; Code:

(require 'org-roam)

(defgroup org-roam-fz nil
  "Settings for `org-roam-fz'."
  :group 'extensions
  :link '(url-link "https://github.com/okomestudio/org-roam-fz/org-roam-fz.el"))

(defvar org-roam-fz-zk 'default
  "The name of the zettelkasten.")

(defcustom org-roam-fz-tags-default nil
  "List of default tags for notes in the current zettelkasten."
  :type '(list string)
  :group 'org-roam-fz)

(defcustom org-roam-fz-add-tags-in-heading t
  "Set t to add tags to heading on insert."
  :type 'boolean
  :group 'org-roam-fz)

(defcustom org-roam-fz-overlays-render-fid
  #'org-roam-fz-overlays-render-fid-default
  "The function that renders fID.
This is a function that takes a single string argument ID."
  :type 'function
  :group 'org-roam-fz)

(defcustom org-roam-fz-overlays-render-id nil
  "The function that renders an ID.
This is a function that takes a single string argument ID."
  :type 'function
  :group 'org-roam-fz)

(defcustom org-roam-fz-overlays-text-placement 'before-string
  "Which side of overlay to show rendered fID text."
  :type '(choice 'before-string 'after-string)
  :group 'org-roam-fz)

(defface org-roam-fz-overlay
  `((t :inherit fixed-pitch
       :foreground ,(face-attribute 'shadow :foreground)
       :background ,(face-attribute 'shadow :background)))
  "Font face used for fID overlays.")

;;; Zettelkastens (as Slip Boxes) Utility

(defun org-roam-fz-zk-dir--expand (zk-dir)
  "Expand ZK-DIR to its full path."
  (expand-file-name
   (if (file-name-absolute-p zk-dir)
       zk-dir
     (file-name-concat org-roam-directory zk-dir))))

(defun org-roam-fz-zettelkastens--parse (&optional zettelkastens)
  "TBD."
  (interactive)
  (pcase-dolist (`(,zk . ,vars) (or zettelkastens org-roam-fz-zettelkastens))
    (if-let* ((dir (and (stringp (alist-get 'zk-dir vars))
                        (org-roam-fz-zk-dir--expand (alist-get 'zk-dir vars)))))
        (let ((vs `((org-roam-fz-zk . ,zk))))
          (pcase-dolist (`(,k . ,v) vars)
            (push (cons (intern (concat "org-roam-fz-" (symbol-name k))) v) vs))
          (dir-locals-set-class-variables zk `((org-mode . ,vs)))
          (dir-locals-set-directory-class dir zk))
      (warn "Must set zk-dir"))))

(defun org-roam-fz-zk-dir (&optional zk)
  "The directory path of the zettelkasten ZK.
The optional ZK defaults to `org-roam-fz-zk',"
  (let ((zk (or zk org-roam-fz-zk)))
    (org-roam-fz-zk-dir--expand
     (or (alist-get 'zk-dir (alist-get zk org-roam-fz-zettelkastens))
         (symbol-name zk)))))

(defun org-roam-fz-zettelkastens--choose (&optional prompt)
  "Choose a zettelkasten from `org-roam-fz-zettelkastens'.
The optional PROMPT string overrides the default message."
  (if-let* ((candidates (mapcar #'car org-roam-fz-zettelkastens)))
      (intern (completing-read (or prompt "Choose a zettelkasten: ")
                               candidates nil t))
    org-roam-fz-zk))

(defcustom org-roam-fz-zettelkastens nil
  "The alist mapping zettelkasten to its directory."
  :type '(repeat (cons symbol (repeat (cons symbol string))))
  :group 'org-roam-fz)

;;; Structures

(cl-defmethod org-roam-node-fid ((node org-roam-node))
  "Access fID of NODE via struct CL-X."
  (and (org-roam-fz-fid--string-parsable-p (org-roam-node-id node))
       (org-roam-node-id node)))

(cl-defmethod org-roam-node-zk ((node org-roam-node))
  "Access zettelkasten name of NODE via struct CL-X."
  (and (org-roam-fz-fid--string-parsable-p (org-roam-node-id node))
       (org-roam-fz-fid--render (org-roam-fz-fid-make (org-roam-node-id node))
                                'zk)))

(cl-defstruct
    (org-roam-fz-fid
     (:constructor org-roam-fz-fid-make
                   (id &aux
                       (parts (if (org-roam-fz-fid--string-parsable-p id)
                                  (split-string id "-")
                                (error "Malformatted ID (%s)" id)))
                       (alnum (nth 0 parts))
                       (zk (nth 1 parts))))
     (:copier org-roam-fz-fid-copy
              (fid &aux
                   (alnum (org-roam-fz-fid-alnum fid))
                   (zk (org-roam-fz-fid-zk fid)))))
  "Folgezettel ID structure.

- `alnum' is the alphanumeric part of fID.
- `zk' is the zettelkasten name."
  alnum zk)

(defvar org-roam-fz-fid--string-regexp
  "^\\([0-9]+.\\)?\\([0-9]+[a-z]+\\)*\\([0-9]+\\)?-\\([^-]+\\)$"
  "The regexp format for fID when stored as string.")

(defun org-roam-fz-fid--string-parsable-p (id)
  "Return non-nil if string ID is parsable into an fID."
  (and (stringp id)
       (string-match org-roam-fz-fid--string-regexp id)))

(defun org-roam-fz-fid--string-parse-zk (id)
  "Return zettelkasten name from string ID."
  (and (stringp id)
       (string-match org-roam-fz-fid--string-regexp id)
       (match-string 4 id)))

(defun org-roam-fz-fid--alnum-split (alnum)
  "Split ALNUM to the alternating alpha-numeric components.
For example, \"12.1a\" will be split to '(\"a\" \"1\" \".\" \"12\")."
  (let ((subs alnum) pos part parts)
    ;; MSD and comma get a special treatment.
    (setq pos (string-match "^\\([0-9]+\\)\\." subs))
    (when pos
      (setq part (match-string 1 subs))
      (push part parts)
      (push "." parts)
      (setq subs (substring subs (1+ (length part)))))

    ;; The rest are alternating alphanumeric components.
    (while (> (length subs) 0)
      (setq pos (string-match "^\\([0-9]+\\|[a-z]+\\)" subs))
      (if (null pos)
          (error "Malformatted FID (%s)" alnum))
      (setq part (match-string 1 subs))
      (push part parts)
      (setq subs (substring subs (length part))))
    parts))

(defun org-roam-fz-fid--alnum-join (comps)
  "Join split alnum components COMPS."
  (string-join (reverse comps)))

(defun org-roam-fz-fid--exists (fid)
  "Return non-nil if FID already exists in the org-roam database."
  (org-roam-id-find (org-roam-fz-fid--render fid 'full)))

(defun org-roam-fz-fid--render (fid &optional mode)
  "Render FID fully as string in the render MODE.
MODE is one of the following symbols:

  - `full' for alnum-zk
  - `alnum' (default) for alnum only
  - `zk' for zk name only"
  (pcase (or mode 'alnum)
    ('alnum (org-roam-fz-fid-alnum fid))
    ('zk (org-roam-fz-fid-zk fid))
    ('full (concat (org-roam-fz-fid-alnum fid) "-" (org-roam-fz-fid-zk fid)))
    (_ (error "Invalid render mode (%s)" mode))))

(defun org-roam-fz-fid--alnum-inc (alnum-part)
  "Increment the fID component ALNUM-PART by one.
When ALNUM-PART is numeric, an integer 1 is added. When ALNUM-PART is
alphabet, an integer 1 is added to its ASCII code. When it reaches 'z',
it becomes 'aa'; then 'ab', 'ac', ..., etc."
  (if (not (null (string-match "[[:digit:]]+" alnum-part)))
      (number-to-string (1+ (string-to-number alnum-part)))
    (let* ((digits (string-to-list alnum-part))
           (base (1+ (- ?z ?a)))
           (n (1+ (apply
                   '+ (mapcar
                       (lambda (i)
                         (* (expt base (- (1- (length digits)) i))
                            (- (nth i digits) (1- ?a))))
                       (number-sequence 0 (1- (length digits))))))))
      (setq digits nil)
      (while (> n 0)
        (push (+ ?a (1- (mod n base))) digits)
        (setq n (/ n base)))
      (concat digits))))

(defun org-roam-fz-fid--lsd-add (fid)
  "Add an least-significant digit component to FID.
Adding an LSD component to an fID means 12.4 becomes 12.4a, for example."
  (let* ((fid (org-roam-fz-fid-copy fid))
         (comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid))))
    (push (if (string-match "[[:digit:]]+" (nth 0 comps)) "a" "1") comps)
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join comps))
    fid))

(defun org-roam-fz-fid--lsd-inc (fid)
  "Increment the least-significant digit of FID by one.
Incrementing the LSD of an fID means 12.4 becomes 12.5, for example."
  (let* ((fid (org-roam-fz-fid-copy fid))
         (comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid))))
    (setcar (nthcdr 0 comps) (org-roam-fz-fid--alnum-inc (nth 0 comps)))
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join comps))
    fid))

(defun org-roam-fz-fid--msd-inc (fid)
  "Increment the most-significant digit of FID by one.
Incrementing the MSD of an fID means 12.4 becomes 13.4, for example."
  (let* ((fid (org-roam-fz-fid-copy fid))
         (comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid)))
         (n (1- (length comps))))
    (setcar (nthcdr n comps) (org-roam-fz-fid--alnum-inc (nth n comps)))
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join comps))
    fid))

(defun org-roam-fz-fid--msd-n (fid n)
  "Take the first N digits of FID from the MSD."
  (let* ((fid (org-roam-fz-fid-copy fid))
         (comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid))))
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join (last comps n)))
    fid))

(cl-defun org-roam-fz-fid--new (&optional zk)
  "Find an unused new fID in the zettelkasten ZK.
If not given, ZK defaults to `org-roam-fz-zk'."
  (let ((fid (org-roam-fz-fid-make (format "1.1-%s" (or zk org-roam-fz-zk)))))
    (while (org-roam-fz-fid--exists fid)
      (setq fid (org-roam-fz-fid--msd-inc fid)))
    fid))

(cl-defun org-roam-fz-fid--follow-up (fid)
  "Find an unused follow-up fID relative to FID."
  (setq fid (org-roam-fz-fid--lsd-add fid))
  (while (org-roam-fz-fid--exists fid)
    (setq fid (org-roam-fz-fid--lsd-inc fid)))
  fid)

(cl-defun org-roam-fz-fid--related (fid)
  "Find an unused related fID relateive to FID."
  (setq fid (org-roam-fz-fid--msd-n fid 3))
  (while (org-roam-fz-fid--exists fid)
    (setq fid (org-roam-fz-fid--lsd-inc fid)))
  fid)

(cl-defun org-roam-fz-fid-at-point ()
  "Get the fID of the Org structure at point."
  (if (not (derived-mode-p '(org-mode))) (error "Not in org document"))
  (let* ((elmt (org-element-context))
         (id (or
              ;; If the point is on an ID link, extract its value.
              (and (org-element-type-p elmt 'link)
                   (string= (org-element-property :type elmt) "id")
                   (org-element-property :path elmt)))))
    (or (and (org-roam-fz-fid--string-parsable-p id)
             (org-roam-fz-fid-make id))
        (let ((id (org-roam-id-at-point)))
          (and (org-roam-fz-fid--string-parsable-p id)
               (org-roam-fz-fid-make id))))))

(cl-defun org-roam-fz-fid-get-create (&optional force)
  "Get or create fID for the node at point.
The function returns the ID of node at point when it exists. If ID does not
exist, it will be created.

When FORCE is non-nil, a new ID is created, even when an ID already exists for
the node."
  (interactive "P")
  (let ((epom (point)))
    (if-let* ((id (and (null force) (org-entry-get epom "ID"))))
        id
      (let* ((fid (if-let* ((fid (org-roam-fz-fid-make
                                  (org-entry-get epom "ID" 'inherit))))
                      (org-roam-fz-fid--follow-up fid)
                    (org-roam-fz-fid--new)))
             (id (org-roam-fz-fid--render fid 'full)))
        (org-entry-put epom "ID" id)
        (org-with-point-at epom
          (org-id-add-location
           id (or org-id-overriding-file-name
                  (buffer-file-name (buffer-base-buffer)))))))))

;;; Overlays

(defun org-roam-fz-overlays-render-fid-default (fid)
  "The default FID renderer function."
  (format "[%s]"
          (or (and (string= org-roam-fz-zk
                            (org-roam-fz-fid-zk fid))
                   (org-roam-fz-fid--render fid 'alnum))
              (concat (org-roam-fz-fid-alnum fid)
                      "(" (org-roam-fz-fid-zk fid) ")")
              "Error")))

(defun org-roam-fz-overlays--format (id)
  "Format ID overlay."
  (let ((fid (ignore-errors (org-roam-fz-fid-make id))))
    (or (when (and org-roam-fz-overlays-render-fid fid)
          (funcall org-roam-fz-overlays-render-fid fid))
        (when org-roam-fz-overlays-render-id
          (funcall org-roam-fz-overlays-render-id id)))))

(defun org-roam-fz-overlays--put (beg end text)
  "Add TEXT for the overlay from BEG to END."
  (let* ((ov (make-overlay beg end))
         (propertized-text (propertize text 'face 'org-roam-fz-overlay))
         (text (pcase org-roam-fz-overlays-text-placement
                 ('before-string (concat propertized-text " "))
                 ('after-string (concat " " propertized-text)))))
    (overlay-put ov org-roam-fz-overlays-text-placement text)
    (overlay-put ov 'category 'folgezettel)
    (overlay-put ov 'evaporate t)))

(defun org-roam-fz-overlays--in-title ()
  "Add fID overlays for title.
ID is extracted from the file property."
  (save-excursion
    (goto-char (point-min))
    (let* ((id (org-roam-id-at-point))
           (rendered (org-roam-fz-overlays--format id)))
      (when (and rendered
                 (re-search-forward "^#\\+TITLE:[ \t]*\\(.*\\)$" nil t))
        (org-roam-fz-overlays--put (match-beginning 1)
                                   (match-end 1)
                                   rendered)))))

(defun org-roam-fz-overlays--in-headlines ()
  "Add fID overlays for headlines.
IDs are extracted when headlines have IDs as their properties."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (let* ((id (org-element-property :ID headline))
             (rendered (org-roam-fz-overlays--format id)))
        (when rendered
          (org-roam-fz-overlays--put
           (+ (org-element-property :begin headline)
              (org-element-property :level headline)
              1)
           (org-element-property :end headline)
           rendered))))))

(defun org-roam-fz-overlays--in-links ()
  "Add fID overlays for links.
IDs are extracted from link paths."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "id")
        (let* ((id (org-element-property :path link))
               (rendered (org-roam-fz-overlays--format id)))
          (when rendered
            (org-roam-fz-overlays--put (org-element-property :begin link)
                                       (org-element-property :end link)
                                       rendered)))))))

(defun org-roam-fz-link-insert ()
  "Insert fIDs for all the links in the current buffer."
  (interactive)
  (when (org-roam-buffer-p)
    (dolist (link (reverse (org-element-map (org-element-parse-buffer) 'link #'identity)))
      (when (string= (org-element-property :type link) "id")
        (let* ((id (org-element-property :path link))
               (rendered (org-roam-fz-overlays--format id)))
          (when rendered
            (let ((pos (org-element-property :contents-begin link)))
              (save-excursion
                (beginning-of-buffer)
                (forward-char (1- pos))
                (insert rendered)))))))))

(defun org-roam-fz-overlays-add ()
  "Add fID overlays in the current buffer."
  (when (org-roam-buffer-p)
    (org-roam-fz-overlays--in-title)
    (org-roam-fz-overlays--in-links)
    (org-roam-fz-overlays--in-headlines)))

(defun org-roam-fz-overlays-remove ()
  "Remove fID overlays in the current buffer."
  (when (org-roam-buffer-p)
    (remove-overlays (point-min) (point-max) 'category 'folgezettel)))

(defun org-roam-fz-overlays-refresh ()
  "Refresh fID overlays in the current buffer."
  (org-roam-fz-overlays-remove)
  (org-roam-fz-overlays-add))

;;; Org Roam Capture Template

(defcustom org-roam-fz-capt-follow-up-header
  ":PROPERTIES:\n:ID: ${id}\n:END:\n#+title: ${title}\n\n"
  "Header for the follow-up note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capt-follow-up-template
  "%?\n--------\n- Previous: ${backlink}\n--------\n- See ... for ..."
  "Template string or function for the follow-up note capture template."
  :type '(choice function string)
  :group 'org-roam-fz)

(defcustom org-roam-fz-capt-new-header
  ":PROPERTIES:\n:ID: ${id}\n:END:\n#+title: ${title}\n\n"
  "Header for the new-topic note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capt-new-template
  "%?\n--------\n- See ... for ..."
  "Template string or function for the new-topic note capture template."
  :type '(choice function string)
  :group 'org-roam-fz)

(defcustom org-roam-fz-capt-related-header
  ":PROPERTIES:\n:ID: ${id}\n:END:\n#+title: ${title}\n\n"
  "Header for the related-topic note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capt-related-template
  "%?\n--------\n- See ... for ..."
  "Template string or function for the related-topic note capture template."
  :type '(choice function string)
  :group 'org-roam-fz)

(cl-defun org-roam-fz-prepare-capture (mode)
  "Use this as a function template in a capture template.
MODE is one of `new', `follow-up', and `related'."
  (let* (capture-node capture-info)
    (cond
     ((eq mode 'new)
      (let* ((fid (org-roam-fz-fid--new))
             (node (org-roam-node-create
                    :id (org-roam-fz-fid--render fid 'full)
                    :title (org-roam-node-title org-roam-capture--node))))
        (setq capture-node node)
        (setq capture-info `( :zk ,(org-roam-fz-fid--render fid 'zk) ))))

     ((or (eq mode 'follow-up) (eq mode 'related))
      (let* ((fid-gen (cl-case mode
                        ('follow-up #'org-roam-fz-fid--follow-up)
                        ('related #'org-roam-fz-fid--related)))
             (fid-at-point (org-roam-fz-fid-at-point))
             (node (org-roam-node-read
                    (when fid-at-point
                      (org-roam-node-title
                       (org-roam-node-from-id
                        (org-roam-fz-fid--render fid-at-point 'full))))
                    (lambda (node)
                      (org-roam-fz-fid--string-parsable-p
                       (org-roam-node-id node)))
                    nil
                    t
                    "Reference node: "))
             (fid (org-roam-fz-fid-make (org-roam-node-id node)))
             (fid-next (apply fid-gen `(,fid)))
             (zk (org-roam-fz-fid--render fid-next 'zk))
             (node-next
              (org-roam-node-create
               :id (org-roam-fz-fid--render fid-next 'full)
               :title (org-roam-node-title org-roam-capture--node))))
        (setq capture-node node-next)
        (setq capture-info `( :zk ,zk
                              :backlink ,(format "[[id:%s][%s]]"
                                                 (org-roam-fz-fid--render fid 'full)
                                                 (org-roam-node-title node))) )))

     (t (error "Unknown mode: %s" mode)))

    (setq org-roam-capture--node capture-node)
    (setq org-roam-capture--info
          (plist-put org-roam-capture--info
                     :title (org-roam-node-title capture-node)))
    (setq org-roam-capture--info (append org-roam-capture--info capture-info))))

(cl-defun org-roam-fz-capt--merge (templates)
  "Merge TEMPLATES to the buffer-local `org-roam-capture-templates'.
The TEMPLATES and `org-roam-capture-templates' are each sorted by their
keys (i.e., the CAR of each element), then merged and saved to
`org-roam-capture-templates'.

An element like `(KEY nil)' in TEMPLATES will remove the entry for KEY.

See the documentation for `org-roam-capture-templates' for the template format."
  (unless (local-variable-p 'org-roam-capture-templates)
    (let* ((capts (make-local-variable 'org-roam-capture-templates))
           (old-items (sort (symbol-value capts)
                            :lessp (lambda (x y) (string< (car x) (car y)))))
           (new-items (sort templates
                            (lambda (x y) (string< (car x) (car y)))))
           result)
      (while (and old-items new-items)
        (setq result (if (string< (caar old-items) (caar new-items))
                         (append result (list (pop old-items)))
                       (if (string= (caar old-items) (caar new-items))
                           (progn
                             (pop old-items)
                             (append result (list (pop new-items))))
                         (append result (list (pop new-items)))))))
      (setq result (append result old-items new-items))
      (set capts (seq-filter (lambda (it) (cadr it)) result)))))

(defvar org-roam-fz-target-filename "${id}/${slug}.org"
  "Filename for the new note.")

(defcustom org-roam-fz-capt-extra nil
  "Extra capture templates to merge with `org-roam-capture-template'."
  :type '(repeat (cons string string))
  :group 'org-roam-fz)

(cl-defun org-roam-fz-capt-bind ()
  "Prepare the `org-roam' capture template for `org-roam-fz'.
Use `org-roam-fz-capt-extra' to add extra template entries. This function binds
`org-roam-capture-template' as a buffer-local variable."
  (when (bound-and-true-p org-roam-fz-mode)
    (org-roam-fz-capt--merge
     `(,@org-roam-fz-capt-extra
       ("z" ,(format "In the zettelkasten (%s) ..." org-roam-fz-zk))
       ("zf" "... add a follow-up topic" plain
        (function ,(if (functionp org-roam-fz-capt-follow-up-template)
                       org-roam-fz-capt-follow-up-template
                     (lambda ()
                       (org-roam-fz-prepare-capture 'follow-up)
                       org-roam-fz-capt-follow-up-template)))
        :target (file+head ,(file-name-concat (org-roam-fz-zk-dir)
                                              org-roam-fz-target-filename)
                           ,org-roam-fz-capt-follow-up-header)
        :unnarrowed t)
       ("zn" "... add a new topic" plain
        (function ,(if (functionp org-roam-fz-capt-new-template)
                       org-roam-fz-capt-new-template
                     (lambda ()
                       (org-roam-fz-prepare-capture 'new)
                       org-roam-fz-capt-new-template)))
        :target (file+head ,(file-name-concat (org-roam-fz-zk-dir)
                                              org-roam-fz-target-filename)
                           ,org-roam-fz-capt-new-header)
        :unnarrowed t)
       ("zr" "... add a related topic" plain
        (function ,(if (functionp org-roam-fz-capt-related-template)
                       org-roam-fz-capt-related-template
                     (lambda ()
                       (org-roam-fz-prepare-capture 'related)
                       org-roam-fz-capt-related-template)))
        :target (file+head ,(file-name-concat (org-roam-fz-zk-dir)
                                              org-roam-fz-target-filename)
                           ,org-roam-fz-capt-related-header)
        :unnarrowed t)))))

(cl-defun org-roam-fz--kill-on-capture ()
  "Kill the node info to the kill ring after new node capture."
  (let ((title (plist-get org-roam-capture--info :title)))
    (when title
      (kill-new title t))))

(cl-defun org-roam-fz--post-insert-proc (id desc)
  "Add tags when insert is at Org heading.
Add this function to the hook `org-roam-post-node-insert-hook'. ID and
DESC are passed from the hook and for the node being inserted."
  (when (and
         org-roam-fz-add-tags-in-heading
         (org-roam-fz-fid--string-parsable-p id)
         (org-at-heading-p)
         (string-match-p "^\\s-*$"
                         (buffer-substring-no-properties (point)
                                                         (line-end-position))))
    (let* ((node (org-roam-node-from-id id))
           (tags (cl-set-difference (org-roam-node-tags node)
                                    org-roam-fz-tags-default
                                    :test #'equal)))
      (when tags
        (insert (format " :%s:" (string-join tags ":")))))))

;;; Public Functions

(cl-defun org-roam-fz-index-node-visit (&optional _arg)
  "Visit the index note for the current zettelkasten.
If exists, the command jumps to the first link pointing to the current note.

With the prefix argument, the command will prompt for a zettelkasten."
  (interactive "P")
  (if-let* ((this-node (org-roam-node-at-point))
            (index-id
             (let* ((zk (pcase _arg
                          ('(4) (org-roam-fz-zettelkastens--choose))
                          (_ org-roam-fz-zk)))
                    (props (cdr (assoc zk org-roam-fz-zettelkastens))))
               (alist-get 'index-id props)))
            (index-node (org-roam-node-from-id index-id)))
      (progn
        (org-roam-node-visit index-node)
        (when-let* ((pattern (format "\\[\\[id:%s\\]\\(\\[[^]]+\\]\\)?\\]"
                                     (org-roam-node-id this-node))))
          (goto-char (point-min))
          (re-search-forward pattern nil t)))
    (warn "Index node not found")))

(cl-defun org-roam-fz-refresh-link ()
  "Refresh hyperlink at point with node title."
  (interactive)
  (when-let* ((context (org-element-context))
              (type (org-element-property :type context))
              (is-id-link (and (eq (org-element-type context) 'link)
                               (string= type "id")))
              (id (org-element-property :path context))
              (beg (org-element-property :begin context))
              (end (org-element-property :end context))
              (desc (org-roam-node-title (org-roam-node-from-id id))))
    (goto-char beg)
    (delete-region beg end)
    (insert (format "[[%s:%s][%s]]" type id desc))
    (run-hook-with-args 'org-roam-post-node-insert-hook id desc)))

(defun org-roam-fz-random-node (&optional zk)
  "Visit a random Folgezettel note in the zettelkasten named ZK."
  (interactive (list (read-string "Name of zettelkasten: " org-roam-fz-zk)))
  (let ((org-roam-fz-zk (or zk org-roam-fz-zk)))
    (condition-case err
        (org-roam-node-random
         t
         (lambda (node)
           (let ((id (org-roam-node-id node)))
             (and (org-roam-fz-fid--string-parsable-p id)
                  (string= (org-roam-fz-fid--string-parse-zk id)
                           org-roam-fz-zk)))))
      (error
       (if (string= (error-message-string err) "Sequence cannot be empty")
           (message "No note found for the zettelkasten named '%s'!" org-roam-fz-zk)
         (error (error-message-string err)))))))

;; Import to the zettelkasten

(defun org-roam-fz-import--replace-id (old-id new-id)
  "Replace OLD-ID referenced via backlinks with NEW-ID.
This function looks for the `org-node' with OLD-ID and errors out if not found.
The return value is the list of buffers visiting files being modified. To commit
the changes, perform buffer save separately."
  (if-let* ((node (org-roam-node-from-id old-id)))
      (let* ((backlinks (org-roam-backlinks-get node :unique t))
             (ptn (concat "\\[\\[id:" old-id "\\]\\(\\[\\(.*?\\)\\]\\)?\\]"))
             modified-buffers source-node-path)
        (dolist (backlink backlinks)
          (setq source-node (org-roam-backlink-source-node backlink))
          (with-current-buffer
              (find-file-noselect (org-roam-node-file source-node))
            (goto-char (point-min))
            (while (re-search-forward ptn nil t)
              (replace-match (if-let* ((full-match (match-string 0))
                                       (desc (match-string 2)))
                                 (format "[[id:%s][%s]]" new-id desc)
                               (format "[[id:%s]]" new-id))))
            (push (current-buffer) modified-buffers)))
        modified-buffers)
    (error "ID %s is not associated with any `org-roam' node" old-id)))

(defun org-roam-fz-import--change-id (old-id new-id)
  "Replace all instances of OLD-ID with NEW-ID in `org-roam' files."
  (if-let* ((node (org-roam-node-from-id old-id)))
      (let ((buffer (progn (org-roam-node-visit node) (current-buffer)))
            ;; TODO(2025-09-02): The help for `org-roam-node-visit' says it
            ;; returns the buffer, but it doesn't. Send a fix?
            buffers)
        (org-entry-put node "ID" new-id)

        ;; Update the IDs found in backlinks:
        (setq buffers (append (org-roam-fz-import--replace-id old-id new-id)
                              `(,buffer)))

        ;; Preview and save.
        ;;
        ;; NOTE: save-some-buffers might be simpler.
        (dolist (buffer buffers)
          (with-current-buffer buffer
            (when (buffer-modified-p)
              ;; TODO(2025-08-07): Optionally prompt the user to save buffer or
              ;; not, using `read-char-choice'. The initial attempt failed due
              ;; to the function not reading input character reliably.
              (save-buffer)
              (org-roam-db-update-file (buffer-file-name buffer))))))
    (warn "Node with ID %s does not exit" old-id)))

(defun org-roam-fz-move-note (&optional node zk)
  "Move NODE to the zettelkasten ZK."
  (interactive (list (org-roam-node-at-point)
                     (org-roam-fz-zettelkastens--choose)))
  (if-let* ((id (org-roam-node-id node))
            (fid (org-roam-fz-fid-make id))
            (file (org-roam-node-file node))
            (parent-dir (file-name-directory file)))
      (let* ((fid-as-str (org-roam-fz-fid--render fid 'full))
             (new-parent (file-name-as-directory
                          (org-roam-fz-zk-dir zk)))
             (new-file (file-name-concat new-parent
                                         fid-as-str
                                         (file-name-nondirectory file))))
        (make-directory new-parent t) ; ensure directory existence
        (org-roam-db-clear-file file)
        (rename-file parent-dir (file-name-concat new-parent fid-as-str) 1)
        (when-let* ((buffer (find-buffer-visiting file)))
          (with-current-buffer buffer
            (set-visited-file-name new-file)
            (revert-buffer :ignore-auto :noconfirm)))
        (org-roam-db-update-file new-file))
    (error "No file associated with the node")))

(defun org-roam-fz-after-import-note--add-tags (node)
  "Add filetags to NODE."
  (when-let* ((buffer (find-buffer-visiting (org-roam-node-file node))))
    (with-current-buffer buffer
      (org-roam-set-keyword
       "filetags"
       (org-make-tag-string
        (seq-uniq
         (append org-roam-fz-tags-default
                 (split-string (or (org-roam--get-keyword "filetags") "")
                               ":" t))))))))

(defvar org-roam-fz-after-import-note-hook
  '(org-roam-fz-after-import-note--add-tags)
  "A hook runs after `org-roam-fz-import-note'.
The hook takes the import node as an argument.")

(defun org-roam-fz-pick-available-fid (&optional zk)
  "Pick a free fID in the zettelkasten ZK."
  (interactive (list (org-roam-fz-zettelkastens--choose)))
  (let ((prompt
         (format "Pick (n)ew, (r)elated, (f)ollow-up fID in '%s' or (q)uit: "
                 zk))
        (choices '(?n ?r ?f ?q))
        resp result)
    ;; This while loop is to work around the issue of `read-char-choice'
    ;; accepting a non-CHARS character while input method is active.
    (while (not (member resp choices))
      (setq resp (read-char-choice prompt choices)))
    (unless (eq resp ?q)
      (setq result
            (if (eq resp ?n)
                (org-roam-fz-fid--new zk)
              (when-let*
                  ((node (org-roam-node-read
                          nil
                          (lambda (node)
                            (when-let* ((id (org-roam-node-id node)))
                              (and (org-roam-fz-fid--string-parsable-p id)
                                   (equal (org-roam-fz-fid--string-parse-zk id)
                                          (symbol-name zk)))))))
                   (id (org-roam-fz-fid-make (org-roam-node-id node))))
                (pcase resp
                  (?f (org-roam-fz-fid--follow-up id))
                  (?r (org-roam-fz-fid--related id))))))
      (message "r %s" result)
      result)))

(defun org-roam-fz--zettelkasten-env (zk)
  "TBD."
  ;; (interactive (list (cdr (org-roam-fz-zettelkastens--choose))))
  (with-temp-buffer
    (setq default-directory
          (org-roam-fz-zk-dir--expand
           (alist-get 'zk-dir (alist-get zk org-roam-fz-zettelkastens))))
    (org-mode)
    (condition-case err
        (progn
          (hack-dir-local-variables-non-file-buffer)
          (list :zk org-roam-fz-zk :target-filename org-roam-fz-target-filename))
      (error (message "Error: %s" err)))))

(defun org-roam-fz-import-note (node zk)
  "Import NODE into zettelkasten ZK.
This function goes through the following steps:

  1. Prompt the user for a zettelkasten
  2. Pick a new Folgezettel ID (new, related, or follow-up)
  3. Replace existing backlinks with the new fID
  4. Move the note to the directory picked in (1)

The user will be prompted a few times for input along the way."
  (interactive (list (org-roam-node-at-point)
                     (org-roam-fz-zettelkastens--choose)))
  (let ((dir-zk (org-roam-fz-zk-dir zk)) fid)
    (unless (file-directory-p dir-zk)
      (user-error "Error: %s is not a valid directory" dir-zk))

    ;; Set the name of target zettelkasten to `zk':
    ;; (let ((target-env (org-roam-fz--zettelkasten-env zk)))
    ;;   (setq target-filename (plist-get target-env :target-filename)))

    ;; Generate the fID:
    (setq fid (org-roam-fz-pick-available-fid zk))
    (when (null fid)
      (error "Cannot generate fID"))

    ;; At this point, we have the target zk and the new fid both set.

    (let ((old-id (org-roam-node-id node))
          (new-id (org-roam-fz-fid--render fid 'full)))
      (org-roam-fz-import--change-id old-id new-id)

      (let* ((node (org-roam-node-from-id new-id))
             (level (org-roam-node-level node)))
        ;; A file-level node gets moved, while a subtree gets extracted:
        (if (> level 0)
            (let ((org-roam-extract-new-file-path
                   (file-name-concat (org-roam-fz-zk-dir zk) org-roam-fz-target-filename))
                  (org-roam-fz-zk zk))
              ;; Extracting the subtree.

              ;; TODO(2025-09-02): Copy files (just ones referenced within the
              ;; subtree) to the target directory.
              (with-current-buffer
                  ;; `org-roam-node-visit', despite what the docstring says,
                  ;; does not return the visited buffer.
                  (progn (org-roam-node-visit node) (current-buffer))
                (org-roam-fz-extract-subtree)))
          ;; Moving the file-level node.
          (org-roam-fz-move-note node zk)))

      (let ((node (org-roam-node-from-id new-id)))
        (run-hook-with-args 'org-roam-fz-after-import-note-hook node)))))

(defun org-roam-fz-change-fid ()
  "Change fID of the current node."
  (interactive)
  (if-let* ((node (save-excursion
                    (goto-char (point-min)) (org-roam-node-at-point)))
            (fid (org-roam-fz-pick-available-fid org-roam-fz-zk))
            (old-id (org-roam-node-id node))
            (new-id (org-roam-fz-fid--render fid 'full)))
      (progn
        (org-roam-fz-import--change-id old-id new-id)
        (org-roam-fz-move-note (org-roam-node-from-id new-id) org-roam-fz-zk))
    (error "Cannot generate fID")))

(defun org-roam-fz-extract-subtree ()
  "Extract current subtree at point to a new file in current zettelkasten."
  (interactive)
  (let* ((dir (alist-get 'zk-dir
                         (alist-get org-roam-fz-zk
                                    org-roam-fz-zettelkastens)))
         (org-roam-extract-new-file-path
          (file-name-concat dir "${id}" "${slug}.org")))
    ;; TODO(2025-11-08): Move referenced files in the same directory.
    (org-roam-extract-subtree)))

;;; Define Minor Mode

(defun org-roam-fz--activate ()
  "Activate the minor mode."
  ;; TODO: Use `before/after-change-functions'?
  (add-hook 'hack-local-variables-hook #'org-roam-fz-capt-bind)
  (add-hook 'org-roam-post-node-insert-hook #'org-roam-fz--post-insert-proc)
  (add-hook 'after-change-major-mode-hook #'org-roam-fz-overlays-refresh)
  (add-hook 'after-save-hook #'org-roam-fz-overlays-refresh)
  (add-hook 'before-revert-hook #'org-roam-fz-overlays-remove)
  (add-hook 'org-roam-capture-new-node-hook #'org-roam-fz--kill-on-capture)
  (org-roam-fz-overlays-refresh))

(defun org-roam-fz--deactivate ()
  "Deactivate the minor mode."
  (org-roam-fz-overlays-remove)
  (remove-hook 'org-roam-capture-new-node-hook #'org-roam-fz--kill-on-capture)
  (remove-hook 'before-revert-hook #'org-roam-fz-overlays-remove)
  (remove-hook 'after-save-hook #'org-roam-fz-overlays-refresh)
  (remove-hook 'after-change-major-mode-hook #'org-roam-fz-overlays-refresh)
  (remove-hook 'org-roam-post-node-insert-hook #'org-roam-fz--post-insert-proc)
  (remove-hook 'hack-local-variables-hook #'org-roam-fz-capt-bind)
  (unload-feature 'org-roam-fz) ; remove symbols based on feature
  (unintern 'org-roam-node-fid)) ; remove ones still left after unload

;;;###autoload
(define-minor-mode org-roam-fz-mode
  "A minor mode for using Folgezettel for org-roam IDs."
  :group 'org-roam
  :lighter "org-roam-fz-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-S-l") #'org-roam-fz-refresh-link)
            (define-key map (kbd "C-c C-i") #'org-roam-fz-index-node-visit)
            map)
  (if org-roam-fz-mode (org-roam-fz--activate) (org-roam-fz--deactivate)))

(provide 'org-roam-fz)
;;; org-roam-fz.el ends here
