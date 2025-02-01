;;; org-roam-fz.el --- Folgezettel for Org Roam  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-fz
;; Version: 0.1
;; Keywords: development, convenience
;; Package-Requires: ((emacs "29.1") (org-roam "20250111.252"))
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

(defcustom org-roam-fz-zk "default"
  "The name of default Zettelkasten."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-fz-overlays-format "[%s] "
  "The string format for displaying fID."
  :type 'string
  :group 'org-roam)

(defcustom org-roam-fz-overlays-render-fid #'org-roam-fz-overlays-render-fid-default
  "The function that renders fID.
This is a function that takes a single string argument ID."
  :type 'function
  :group 'org-roam)

(defcustom org-roam-fz-overlays-render-id nil
  "The function that renders an ID.
This is a function that takes a single string argument ID."
  :type 'function
  :group 'org-roam)

;;; Structures

(cl-defstruct
    (org-roam-fz-fid
     (:constructor org-roam-fz-fid-new
                   (id &aux
                       (parts (let ((parts (split-string id "-")))
                                (if (not (= (length parts) 2))
                                    (error "Malformatted ID (%s)" id))
                                parts))
                       (alnum (nth 0 parts))
                       (zk (nth 1 parts)))))
  "Folgezettel ID structure.

- `alnum' is the alphanumeric part of fID.
- `zk' is the Zettelkasten name."
  alnum zk)

(defun org-roam-fz-fid--split-alnum (alnum)
  "Split ALNUM to the alternating alpha-numeric components.
For example, \"12.1a\" will be split to '(\"a\" \"1\" \"12.\")."
  (let ((subs alnum) pos part parts)
    (setq pos (string-match "^\\([0-9]+\\.\\)" subs))
    (when pos
      (setq part (match-string 1 subs))
      (push part parts)
      (setq subs (substring subs (length part))))
    (while (> (length subs) 0)
      (setq pos (string-match "^\\([0-9]+\\|[a-z]+\\)" subs))
      (if (null pos)
          (error "Malformatted FID (%s)" s))
      (setq part (match-string 1 subs))
      (push part parts)
      (setq subs (substring subs (length part))))
    parts))

(defun org-roam-fz-fid--exists (fid)
  "Return non-nil if FID already exists in the org-roam database."
  (org-roam-id-find (org-roam-fz-fid--render fid)))

(defun org-roam-fz-fid--render (fid &optional mode)
  "Render FID fully as string in the render MODE.
MODE is one of the following symbols:

  - `full' for alnum-zk
  - `alnum' (default) for alnum only
  - `zk' for zk name only"
  (pcase (or mode 'alnum)
    ('alnum (org-roam-fz-fid-alnum fid))
    ('zk (org-roam-fz-fid-zk fid))
    ('full (concat (org-roam-fz-fid-alnum fid) "-" (org-roam-fz-fid-zk fid)))))

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

(defun org-roam-fz-fid--child-add (fid)
  "Add a child component to FID.
Adding a child to an fID means 12.4 becomes 12.4a, for example."
  (let ((comps (org-roam-fz-fid--split-alnum (org-roam-fz-fid-alnum fid))))
    (push (if (string-match "[[:digit:]]+" (nth 0 comps)) "a" "1") comps)
    (setf (org-roam-fz-fid-alnum fid) (string-join (reverse comps)))
    fid))

(defun org-roam-fz-fid--sibling-inc (fid)
  "Increment the sibling of FID by one.
Incrementing the sibling of an fID means 12.4 becomes 12.5, for example."
  (let ((comps (org-roam-fz-fid--split-alnum (org-roam-fz-fid-alnum fid))))
    (setcar (nthcdr 0 comps) (org-roam-fz-fid--alnum-inc (nth 0 comps)))
    (setf (org-roam-fz-fid-alnum fid) (string-join (reverse comps)))
    fid))

;;; Overlays

(defun org-roam-fz-overlays-render-fid-default (fid)
  "The default FID renderer function."
  (if (string= org-roam-fz-zk (org-roam-fz-fid-zk fid))
      (org-roam-fz-fid--render fid 'alnum)
    (format "%s(%s)"
            (org-roam-fz-fid-alnum fid)
            (org-roam-fz-fid-zk fid))))

(defun org-roam-fz-overlays--format (id)
  "Format ID overlay."
  (let* ((fid (ignore-errors (org-roam-fz-fid-new id)))
         (rendered (if fid
                       (when org-roam-fz-overlays-render-fid
                         (funcall org-roam-fz-overlays-render-fid fid))
                     (when org-roam-fz-overlays-render-id
                       (funcall org-roam-fz-overlays-render-id id)))))
    (when rendered
      (format org-roam-fz-overlays-format rendered))))

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

;;; Functions to expose for Org Roam capture template

(defun org-roam-fz--id-set (&rest _)
  "Set ID from the document position at point.
This is an advice for `org-roam-node-find'."
  (setq org-roam-fz--id (org-roam-id-at-point)))

(defun org-roam-fz-fid ()
  "Render fID as string."
  (symbol-value 'org-roam-fz--id))

(defun org-roam-fz-zk ()
  "Get the value of `org-roam-fz-zk'."
  (symbol-value 'org-roam-fz-zk))

(defun org-roam-fz-fid-prompt (&optional render-mode)
  "Prompt user for a new fID and render.
See `org-roam-fz-fid--render' for the available values for RENDER-MODE."
  (let ((render-mode (or render-mode 'alnum))
        (prompt "Folgezettel ID: ")
        fid input)
    (while (null fid)
      (setq input (read-string prompt))
      (setq fid (org-roam-fz-fid-new (format "%s-%s" input org-roam-fz-zk)))
      (if (org-roam-fz-fid--exists fid)
          (setq prompt (format "('%s' is taken) Folgezettel ID: " input)
                fid nil)))
    (setq org-roam-fz--id (org-roam-fz-fid--render fid 'full))
    (org-roam-fz-fid--render fid render-mode)))

(defun org-roam-fz-fid-follow-up (&optional render-mode)
  "Render the fID for a follow up zettel.
See `org-roam-fz-fid--render' for the available values for RENDER-MODE."
  (if (not (and (boundp 'org-roam-fz--id) org-roam-fz--id))
      (org-roam-fz-fid-prompt render-mode)
    (let ((render-mode (or render-mode 'alnum))
          (fid (org-roam-fz-fid-new org-roam-fz--id)))
      (setq fid (org-roam-fz-fid--child-add fid))
      (while (org-roam-fz-fid--exists fid)
        (setq fid (org-roam-fz-fid--sibling-inc fid)))
      (setq org-roam-fz--id (org-roam-fz-fid--render fid 'full))
      (org-roam-fz-fid--render fid render-mode))))

;;; Define minor mode

(defun org-roam-fz--activate ()
  "Activate the minor mode."
  (advice-add #'org-roam-node-find :before #'org-roam-fz--id-set)
  (advice-add #'delete-char :after #'org-roam-fz-overlays--on-delete-char)
  (add-hook 'after-save-hook #'org-roam-fz-overlays-refresh)
  (add-hook 'before-revert-hook #'org-roam-fz-overlays-remove)
  (org-roam-fz-overlays-refresh))

(defun org-roam-fz--deactivate ()
  "Deactivate the minor mode."
  (org-roam-fz-overlays-remove)
  (remove-hook 'before-revert-hook #'org-roam-fz-overlays-remove)
  (remove-hook 'after-save-hook #'org-roam-fz-overlays-refresh)
  (advice-remove #'delete-char #'org-roam-fz-overlays--on-delete-char)
  (advice-remove #'org-roam-node-find #'org-roam-fz--id-set))

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
