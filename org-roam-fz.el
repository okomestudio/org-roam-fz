;;; org-roam-fz.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fz
;; Version: 0.1
;; Keywords: development, convenience
;; Package-Requires: ((emacs "29.1") (org-roam "20250218.1722"))
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

(defcustom org-roam-fz-overlays-render-fid #'org-roam-fz-overlays-render-fid-default
  "The function that renders fID.
This is a function that takes a single string argument ID."
  :type 'function
  :group 'org-roam-fz)

(defcustom org-roam-fz-overlays-render-id nil
  "The function that renders an ID.
This is a function that takes a single string argument ID."
  :type 'function
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-follow-up-file
  "%(org-roam-fz-zk)/%(org-roam-fz-fid-follow-up)/${slug}.org"
  "Filename for the follow-up note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-follow-up-header
  (concat ":PROPERTIES:\n"
          ":ID: %(org-roam-fz-fid)\n"
          ":END:\n"
          "#+title: ${title}\n\n")
  "Header for the follow-up note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-follow-up-template
  (lambda ()
    (let* ((node (org-roam-node-from-id (org-roam-fz-fid)))
           (fid (org-roam-node-id node))
           (desc (org-roam-node-title node)))
      (concat "%?\n"
              "--------\n"
              "- Previous: [[id:" fid "][" desc "]]\n"
              "--------\n"
              "- See ... for ...")))
  "Template string or function for the follow-up note capture template."
  :type '(choice function string)
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-new-file
  "%(org-roam-fz-zk)/%(org-roam-fz-fid-new)/${slug}.org"
  "Filename for the new-topic note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-new-header
  (concat ":PROPERTIES:\n"
          ":ID: %(org-roam-fz-fid)\n"
          ":END:\n"
          "#+title: ${title}\n\n")
  "Header for the new-topic note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-new-template "%?\n--------\n- See ... for ..."
  "Template string or function for the new-topic note capture template."
  :type '(choice function string)
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-related-file
  "%(org-roam-fz-zk)/%(org-roam-fz-fid-related)/${slug}.org"
  "Filename for the related-topic note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-related-header
  (concat ":PROPERTIES:\n"
          ":ID: %(org-roam-fz-fid)\n"
          ":END:\n"
          "#+title: ${title}\n\n")
  "Header for the related-topic note capture template."
  :type 'string
  :group 'org-roam-fz)

(defcustom org-roam-fz-capture-template-related-template "%?\n--------\n- See ... for ..."
  "Template string or function for the related-topic note capture template."
  :type '(choice function string)
  :group 'org-roam-fz)

;;; Structures

(cl-defstruct
    (org-roam-fz-fid
     (:constructor org-roam-fz-fid-make
                   (id &aux
                       (parts (if (org-roam-fz-fid--string-parsable-p id)
                                  (split-string id "-")
                                (error "Malformatted ID (%s)" id)))
                       (alnum (nth 0 parts))
                       (zk (nth 1 parts)))))
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
  (let ((comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid))))
    (push (if (string-match "[[:digit:]]+" (nth 0 comps)) "a" "1") comps)
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join comps))
    fid))

(defun org-roam-fz-fid--lsd-inc (fid)
  "Increment the least-significant digit of FID by one.
Incrementing the LSD of an fID means 12.4 becomes 12.5, for example."
  (let ((comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid))))
    (setcar (nthcdr 0 comps) (org-roam-fz-fid--alnum-inc (nth 0 comps)))
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join comps))
    fid))

(defun org-roam-fz-fid--msd-inc (fid)
  "Increment the most-significant digit of FID by one.
Incrementing the MSD of an fID means 12.4 becomes 13.4, for example."
  (let* ((comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid)))
         (n (1- (length comps))))
    (setcar (nthcdr n comps) (org-roam-fz-fid--alnum-inc (nth n comps)))
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join comps))
    fid))

(defun org-roam-fz-fid--msd-n (fid n)
  "Take the first N digits of FID from the MSD."
  (let* ((comps (org-roam-fz-fid--alnum-split (org-roam-fz-fid-alnum fid))))
    (setf (org-roam-fz-fid-alnum fid) (org-roam-fz-fid--alnum-join (last comps n)))
    fid))

;;; Overlays

(defun org-roam-fz-overlays-render-fid-default (fid)
  "The default FID renderer function."
  (format "[%s] " (or (and (string= org-roam-fz-zk (org-roam-fz-fid-zk fid))
                           (org-roam-fz-fid--render fid 'alnum))
                      (concat (org-roam-fz-fid-alnum fid)
                              "(" (org-roam-fz-fid-zk fid) ")"))))

(defun org-roam-fz-overlays--format (id)
  "Format ID overlay."
  (let ((fid (ignore-errors (org-roam-fz-fid-make id))))
    (or (when (and org-roam-fz-overlays-render-fid fid)
          (funcall org-roam-fz-overlays-render-fid fid))
        (when org-roam-fz-overlays-render-id
          (funcall org-roam-fz-overlays-render-id id)))))

(defun org-roam-fz-overlays--in-title ()
  "Add fID overlays for title.
ID is extracted from the file property."
  (let* ((id (save-excursion (beginning-of-buffer) (org-roam-id-at-point)))
         (rendered (org-roam-fz-overlays--format id))
         (pos (save-excursion
                (beginning-of-buffer)
                (re-search-forward "#\\+title: " nil t))))
    (when (and pos rendered)
      (let ((ov (make-overlay pos pos)))
        (overlay-put ov 'before-string rendered)
        (overlay-put ov 'category 'folgezettel)))))

(defun org-roam-fz-overlays--in-headlines ()
  "Add fID overlays for headlines.
IDs are extracted when headlines have IDs as their properties."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (let* ((id (org-element-property :ID headline))
             (rendered (org-roam-fz-overlays--format id)))
        (when rendered
          (let* ((pos (save-excursion
                        (goto-char (org-element-property :begin headline))
                        (re-search-forward (org-element-property :raw-value headline))
                        (match-beginning 0)))
                 (ov (make-overlay pos pos)))
            (overlay-put ov 'before-string rendered)
            (overlay-put ov 'category 'folgezettel)))))))

(defun org-roam-fz-overlays--in-links ()
  "Add fID overlays for links.
IDs are extracted from link paths."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) "id")
        (let* ((id (org-element-property :path link))
               (rendered (org-roam-fz-overlays--format id)))
          (when rendered
            (let* ((pos (org-element-property :begin link))
                   (ov (make-overlay pos pos)))
              (overlay-put ov 'before-string rendered)
              (overlay-put ov 'category 'folgezettel))))))))

(defun org-roam-fz-overlays--on-delete-char (&rest _)
  "Advise `delete-char' to delete overlapping overlays.
The overlay(s) at point are removed if exist."
  (let ((pos (point)))
    (when (overlays-in pos pos)
      (remove-overlays pos pos 'category 'folgezettel))))

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

(defvar org-roam-fz--id nil
  "Temporary storage for ID used by at-point note creation functions.")

(defun org-roam-fz--id-set (&rest _)
  "Set ID from the Org document position at point.
When the point is on a link, the function tries to extract its ID
string. Otherwise, the function gets the ID of Org roam node at point.

This function advises `org-roam-node-find'."
  (setq
   org-roam-fz--id
   (if (derived-mode-p '(org-mode))
       (let* ((elmt (org-element-context))
              (id (or
                   ;; If the point is on an ID link, extract its value.
                   (and (org-element-type-p elmt 'link)
                        (string= (org-element-property :type elmt) "id")
                        (org-element-property :path elmt))

                   ;; If the point is on a headline, see if the previous
                   ;; heading is an ID link. If so, extract its value.
                   (and (org-element-type-p elmt 'headline)
                        (save-mark-and-excursion
                          (org-previous-visible-heading 1)
                          (org-end-of-line)
                          (setq elmt (org-element-context))
                          t)
                        (and (org-element-type-p elmt 'link)
                             (string= (org-element-property :type elmt) "id")
                             (org-element-property :path elmt))))))
         (or (and (org-roam-fz-fid--string-parsable-p id) id)
             (org-roam-id-at-point))))))

(defun org-roam-fz-fid ()
  "Render fID as string."
  (symbol-value 'org-roam-fz--id))

(defun org-roam-fz-zk ()
  "Get the value of `org-roam-fz-zk'."
  (symbol-value 'org-roam-fz-zk))

(defun org-roam-fz-fid-prompt (&optional render-mode)
  "Prompt user for a new fID and render.
See `org-roam-fz-fid--render' for the available values for RENDER-MODE."
  (let ((prompt "Folgezettel ID: ")
        fid input)
    (while (null fid)
      (setq input (read-string prompt))
      (setq fid (org-roam-fz-fid-make (format "%s-%s" input org-roam-fz-zk)))
      (if (org-roam-fz-fid--exists fid)
          (setq prompt (format "('%s' is taken) Folgezettel ID: " input)
                fid nil)))
    (setq org-roam-fz--id (org-roam-fz-fid--render fid 'full))
    (org-roam-fz-fid--render fid render-mode)))

(defun org-roam-fz-fid-new (&optional render-mode)
  "Render the fID for a new-topic zettel.
See `org-roam-fz-fid--render' for the available values for RENDER-MODE."
  (let ((fid (org-roam-fz-fid-make (format "1.1-%s" org-roam-fz-zk))))
    (while (org-roam-fz-fid--exists fid)
      (setq fid (org-roam-fz-fid--msd-inc fid)))
    (setq org-roam-fz--id (org-roam-fz-fid--render fid 'full))
    (org-roam-fz-fid--render fid render-mode)))

(defun org-roam-fz-fid-follow-up (&optional render-mode)
  "Render the fID for a follow-up topic zettel.
See `org-roam-fz-fid--render' for the available values for RENDER-MODE."
  (if (not (and (boundp 'org-roam-fz--id) org-roam-fz--id))
      (org-roam-fz-fid-prompt render-mode)
    (let ((fid (org-roam-fz-fid-make org-roam-fz--id)))
      (setq fid (org-roam-fz-fid--lsd-add fid))
      (while (org-roam-fz-fid--exists fid)
        (setq fid (org-roam-fz-fid--lsd-inc fid)))
      (setq org-roam-fz--id (org-roam-fz-fid--render fid 'full))
      (org-roam-fz-fid--render fid render-mode))))

(defun org-roam-fz-fid-related (&optional render-mode)
  "Render the fID for a related-topic zettel.
See `org-roam-fz-fid--render' for the available values for RENDER-MODE."
  (if (not (and (boundp 'org-roam-fz--id) org-roam-fz--id))
      (org-roam-fz-fid-prompt render-mode)
    (let ((fid (org-roam-fz-fid-make org-roam-fz--id)))
      (org-roam-fz-fid--msd-n fid 3)
      (while (org-roam-fz-fid--exists fid)
        (setq fid (org-roam-fz-fid--lsd-inc fid)))
      (setq org-roam-fz--id (org-roam-fz-fid--render fid 'full))
      (org-roam-fz-fid--render fid render-mode))))

(defun org-roam-fz-capture-template-follow-up (keys description &rest rest)
  "Get the capture template for the follow-up note.
KEYS and DESCRIPTION are string. REST items are spliced at the end. Use
custom variables `org-roam-fz-capture-template-*' to control output."
  `(,keys
    ,description
    plain
    ,(if (functionp org-roam-fz-capture-template-follow-up-template)
         `(function ,org-roam-fz-capture-template-follow-up-template)
       org-roam-fz-capture-template-follow-up-template)
    :target (file+head ,org-roam-fz-capture-template-follow-up-file
                       ,org-roam-fz-capture-template-follow-up-header)
    :unnarrowed t
    ,@rest))

(defun org-roam-fz-capture-template-new (keys description &rest rest)
  "Get the capture template for the new topic note.
KEYS and DESCRIPTION are string. REST items are spliced at the end. Use
custom variables `org-roam-fz-capture-template-*' to control output."
  `(,keys
    ,description
    plain
    ,(if (functionp org-roam-fz-capture-template-new-template)
         `(function ,org-roam-fz-capture-template-new-template)
       org-roam-fz-capture-template-new-template)
    :target (file+head ,org-roam-fz-capture-template-new-file
                       ,org-roam-fz-capture-template-new-header)
    :unnarrowed t
    ,@rest))

(defun org-roam-fz-capture-template-related (keys description &rest rest)
  "Get the capture template for the related topic note.
KEYS and DESCRIPTION are string. REST items are spliced at the end. Use
custom variables `org-roam-fz-capture-template-*' to control output."
  `(,keys
    ,description
    plain
    ,(if (functionp org-roam-fz-capture-template-related-template)
         `(function ,org-roam-fz-capture-template-related-template)
       org-roam-fz-capture-template-related-template)
    :target (file+head ,org-roam-fz-capture-template-related-file
                       ,org-roam-fz-capture-template-related-header)
    :unnarrowed t
    ,@rest))

;;; Other public functions

(defun org-roam-fz-random-node (arg &rest kwargs)
  "Visit a random Folgezettel note in the Zettelkasten.
When called with the `\\[universal-argument]' prefix ARG, the user will
be prompted for ZK. When called non-interactively, ZK can also be given
as a KWARGS, such that ':zk <zk>'."
  (interactive "P")
  (let ((org-roam-fz-zk (pcase arg
                          ('(4) (read-string "Zettelkasten name: "))
                          (_ (or (plist-get kwargs :zk) org-roam-fz-zk)))))
    (condition-case err
        (org-roam-node-random
         nil
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
  (advice-add #'org-roam-node-find :before #'org-roam-fz--id-set)
  ;; TODO: Use `before/after-change-functions'?
  (advice-add #'delete-char :after #'org-roam-fz-overlays--on-delete-char)
  (add-hook 'after-change-major-mode-hook #'org-roam-fz-overlays-refresh)
  (add-hook 'after-save-hook #'org-roam-fz-overlays-refresh)
  (add-hook 'before-revert-hook #'org-roam-fz-overlays-remove)
  (org-roam-fz-overlays-refresh))

(defun org-roam-fz--deactivate ()
  "Deactivate the minor mode."
  (org-roam-fz-overlays-remove)
  (remove-hook 'before-revert-hook #'org-roam-fz-overlays-remove)
  (remove-hook 'after-save-hook #'org-roam-fz-overlays-refresh)
  (remove-hook 'after-change-major-mode-hook #'org-roam-fz-overlays-refresh)
  (advice-remove #'delete-char #'org-roam-fz-overlays--on-delete-char)
  (advice-remove #'org-roam-node-find #'org-roam-fz--id-set)
  (unload-feature 'org-roam-fz)      ; remove symbols based on feature
  (unintern 'org-roam-node-fid)      ; remove ones still left after unload
  )

;;;###autoload
(define-minor-mode org-roam-fz-mode
  "A minor mode for using Folgezettel for org-roam IDs."
  :group 'org-roam
  :lighter "org-roam-fz-mode"
  :keymap nil
  (if org-roam-fz-mode
      (org-roam-fz--activate)
    (org-roam-fz--deactivate)))

(provide 'org-roam-fz)
;;; org-roam-fz.el ends here
