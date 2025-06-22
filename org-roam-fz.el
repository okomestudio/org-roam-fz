;;; org-roam-fz.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fz
;; Version: 0.5.1
;; Keywords: org-roam, convenience
;; Package-Requires: ((emacs "30.1") (org-roam "20250218.1722"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package provides the Folgezettel support for Org Roam.
;;
;;; Code:

(require 'org-roam)

(defgroup org-roam-fz nil
  "Settings for `org-roam-fz'."
  :group 'extensions
  :link '(url-link "https://github.com/okomestudio/org-roam-fz/org-roam-fz.el"))

(defcustom org-roam-fz-zk "default"
  "The name of default Zettelkasten."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-target-filename "${zk}/${id}/${slug}.org"
  "Filename for the new note."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-master-index-node-id nil
  "Org Roam node ID of the master index for this Zettelkasten.
This ID is used by `org-roam-fz-master-index-node-visit' to jump quickly
to the master index note."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-add-tags-in-heading t
  "Set t to add tags to heading on insert."
  :type 'boolean
  :group 'org-roam-fz)

(defcustom org-roam-fz-add-tags-exclude nil
  "List of tags to exclude on `org-roam-fz-add-tags-in-heading'."
  :type '(list string)
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

(defface org-roam-fz-overlay
  `((t :inherit fixed-pitch
       :foreground ,(face-attribute 'shadow :foreground)
       :background ,(face-attribute 'shadow :background)))
  "Font face used for fID overlays.")

;;; Structures

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
- `zk' is the Zettelkasten name."
  alnum zk)

(defvar org-roam-fz-fid--string-regexp
  "^\\([0-9]+.\\)?\\([0-9]+[a-z]+\\)*\\([0-9]+\\)?-\\([^-]+\\)$"
  "The regexp format for fID when stored as string.")

(defun org-roam-fz-fid--string-parsable-p (id)
  "Return non-nil if string ID is parsable into an fID."
  (and (stringp id)
       (string-match org-roam-fz-fid--string-regexp id)))

(defun org-roam-fz-fid--string-parse-zk (id)
  "Return Zettelkasten name from string ID."
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

;;; Structure method for `org-roam-node-display-template'

(cl-defmethod org-roam-node-fid ((node org-roam-node))
  "Access fID of `org-roam-node' struct CL-X."
  (let* ((id (org-roam-node-id node))
         (fid (and (org-roam-fz-fid--string-parsable-p id) id)))
    (if fid (org-roam-fz-overlays--format fid))))

;;; Functions to expose for Org Roam capture template

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
             (fid-at-point (org-roam-fz--fid-at-point))
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

(cl-defun org-roam-fz--fid-at-point ()
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

(cl-defun org-roam-fz-fid--new ()
  "Find an unused new fID."
  (let ((fid (org-roam-fz-fid-make (format "1.1-%s" org-roam-fz-zk))))
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

(defun org-roam-fz-capt-follow-up (keys description &rest rest)
  "Get the capture template for the follow-up note.
KEYS and DESCRIPTION are string. REST items are spliced at the end. Use
custom variables `org-roam-fz-capt-*' to control output."
  `(,keys
    ,description
    plain
    (function ,(if (functionp org-roam-fz-capt-follow-up-template)
                   org-roam-fz-capt-follow-up-template
                 (lambda ()
                   (org-roam-fz-prepare-capture 'follow-up)
                   org-roam-fz-capt-follow-up-template)))
    :target (file+head ,org-roam-fz-target-filename
                       ,org-roam-fz-capt-follow-up-header)
    :unnarrowed t
    ,@rest))

(defun org-roam-fz-capt-new (keys description &rest rest)
  "Get the capture template for the new topic note.
KEYS and DESCRIPTION are string. REST items are spliced at the end. Use
custom variables `org-roam-fz-capt-*' to control output."
  `(,keys
    ,description
    plain
    (function ,(if (functionp org-roam-fz-capt-new-template)
                   org-roam-fz-capt-new-template
                 (lambda ()
                   (org-roam-fz-prepare-capture 'new)
                   org-roam-fz-capt-new-template)))
    :target (file+head ,org-roam-fz-target-filename
                       ,org-roam-fz-capt-new-header)
    :unnarrowed t
    ,@rest))

(defun org-roam-fz-capt-related (keys description &rest rest)
  "Get the capture template for the related topic note.
KEYS and DESCRIPTION are string. REST items are spliced at the end. Use
custom variables `org-roam-fz-capt-*' to control output."
  `(,keys
    ,description
    plain
    (function ,(if (functionp org-roam-fz-capt-related-template)
                   org-roam-fz-capt-related-template
                 (lambda ()
                   (org-roam-fz-prepare-capture 'related)
                   org-roam-fz-capt-related-template)))
    :target (file+head ,org-roam-fz-target-filename
                       ,org-roam-fz-capt-related-header)
    :unnarrowed t
    ,@rest))

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
                                    org-roam-fz-add-tags-exclude
                                    :test #'equal)))
      (when tags
        (insert (format " :%s:" (string-join tags ":")))))))

;;; Public functions

(cl-defun org-roam-fz-master-index-node-visit ()
  "Visit the master index note for the current Zettelkasten."
  (interactive)
  (org-roam-node-open (org-roam-node-from-id org-roam-fz-master-index-node-id)))

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
  "Visit a random Folgezettel note in the Zettelkasten named ZK."
  (interactive (list (read-string "Zettelkasten name: " org-roam-fz-zk)))
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
           (message "No note found for Zettelkasten named '%s'!" org-roam-fz-zk)
         (error (error-message-string err)))))))

;;; Define minor mode

(defun org-roam-fz--activate ()
  "Activate the minor mode."
  ;; TODO: Use `before/after-change-functions'?
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
  (unload-feature 'org-roam-fz) ; remove symbols based on feature
  (unintern 'org-roam-node-fid)) ; remove ones still left after unload

;;;###autoload
(define-minor-mode org-roam-fz-mode
  "A minor mode for using Folgezettel for org-roam IDs."
  :group 'org-roam
  :lighter "org-roam-fz-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-S-l") #'org-roam-fz-refresh-link)
            (define-key map (kbd "C-c C-i") #'org-roam-fz-master-index-node-visit)
            map)
  (if org-roam-fz-mode (org-roam-fz--activate) (org-roam-fz--deactivate)))

(provide 'org-roam-fz)
;;; org-roam-fz.el ends here
