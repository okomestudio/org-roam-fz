;;; test-org-roam-fz.el --- Tests for org-roam-fz  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Commentary:
;;
;; Test `org-roam-fz'.
;;
;;; Code:

(require 'buttercup)
(require 'org-roam-fz)

;;; Setup

(setq org-roam-fz-zk "zktest")
(defun setup-org-roam-db ()
  "Set up Org Roam database using the test data."
  (setq org-roam-directory (expand-file-name "./tests/zktest/"))
  ;; NOTE: The cache database will be found under
  ;; .eldev/version/emacs-dir.
  (org-roam-db-sync))

(setup-org-roam-db)

(defun to-fid (alnum &optional zk)
  "Construct string fID from its component.
ALNUM is required, but ZK is optional when `org-roam-fz-zk' is assumed."
  (format "%s-%s" alnum (or zk org-roam-fz-zk)))

(defun fid-make (alnum &optional zk)
  "Construct an fID from its component.
ALNUM is required, but ZK is optional when `org-roam-fz-zk' is assumed."
  (org-roam-fz-fid-make (to-fid alnum zk)))

;;; Tests

(describe
 "org-roam-fz-fid-make"
 :var ((fid (org-roam-fz-fid-make "12.3ab4-default")))
 (it "creates Folgezettel ID object"
     (expect (org-roam-fz-fid-alnum fid) :to-equal "12.3ab4")
     (expect (org-roam-fz-fid-zk fid) :to-equal "default")))

(describe
 "org-roam-fz-fid-make"
 (it "fails to create fID from a malformatted ID"
     (expect (org-roam-fz-fid-make "#%#") :to-throw 'error)))

(describe
 "org-roam-fz-fid-copy"
 :var ((fid (org-roam-fz-fid-make "12.3ab4-default")))
 (it "copies Folgezettel ID object"
     (let ((fid-copy (org-roam-fz-fid-copy fid)))
       (expect (not (eq fid fid-copy)) :to-equal t)
       (expect (org-roam-fz-fid-alnum fid-copy)
               :to-equal (org-roam-fz-fid-alnum fid))
       (expect (org-roam-fz-fid-zk fid-copy)
               :to-equal (org-roam-fz-fid-zk fid)))))

(describe
 "org-roam-fz-fid--string-parsable-p"
 (it "returns non-nil if string ID is fID-parsable"
     (expect (org-roam-fz-fid--string-parsable-p (to-fid "1.1"))
             :not :to-equal nil))
 (it "returns nil if string ID is not fID-parsable"
     (expect (org-roam-fz-fid--string-parsable-p "notparsable")
             :to-equal nil)))

(describe
 "org-roam-fz-fid--string-parse-zk"
 (it "returns the Zettelkasten name from fID-parsable string ID"
     (expect (org-roam-fz-fid--string-parse-zk (to-fid "2.9" "zk"))
             :to-equal "zk")))

(describe
 "org-roam-fz-fid--alnum-split"
 (it "splits fID string into components"
     (expect (org-roam-fz-fid--alnum-split "12.2a")
             :to-equal '("a" "2" "." "12")))
 (it "splits fID string without the comma component into components"
     (expect (org-roam-fz-fid--alnum-split "2a3c")
             :to-equal '("c" "3" "a" "2")))
 (it "throws an error on invalid fID string"
     (expect (org-roam-fz-fid--alnum-split "@5*2a3c")
             :to-throw 'error)))

(describe
 "org-roam-fz-fid--alnum-join"
 :var* ((expected "12.2a")
        (comps (org-roam-fz-fid--alnum-split expected)))
 (it "join fID string components"
     (expect (org-roam-fz-fid--alnum-join comps) :to-equal expected)))

(describe
 "org-roam-fz-fid--exists"
 (it "returns non-nil when fID exists"
     (expect (org-roam-fz-fid--exists (org-roam-fz-fid-make (to-fid "1.1")))
             :not :to-equal nil))
 (it "returns nil when fID does not exist"
     (expect (org-roam-fz-fid--exists (org-roam-fz-fid-make (to-fid "99.1")))
             :to-equal nil)))

(describe
 "org-roam-fz-fid--render"
 :var ((fid (org-roam-fz-fid-make (to-fid "12.3a" "zk"))))
 (it "renders an fID in all modes"
     (expect (org-roam-fz-fid--render fid 'alnum) :to-equal "12.3a")
     (expect (org-roam-fz-fid--render fid 'zk) :to-equal "zk")
     (expect (org-roam-fz-fid--render fid 'full) :to-equal (to-fid "12.3a" "zk")))
 (it "throws an error for an unrecognized mode"
     (expect (org-roam-fz-fid--render fid 'unknown) :to-throw 'error)))

(describe
 "org-roam-fz-fid--alnum-inc"
 (it "increments alphanumeric ID component"
     (expect (org-roam-fz-fid--alnum-inc "19") :to-equal "20")
     (expect (org-roam-fz-fid--alnum-inc "a") :to-equal "b")
     (expect (org-roam-fz-fid--alnum-inc "z") :to-equal "aa")
     (expect (org-roam-fz-fid--alnum-inc "aaz") :to-equal "aba")
     (expect (org-roam-fz-fid--alnum-inc "acz") :to-equal "ada")
     (expect (org-roam-fz-fid--alnum-inc "zzz") :to-equal "aaaa")))

(describe
 "org-roam-fz-fid--lsd-add"
 (it "adds a child to fID"
     (cl-loop for (input expected)
              in '(("12.3" "12.3a")
                   ("123ab4c5d" "123ab4c5d1")
                   ("123ab4c5" "123ab4c5a"))
              do (expect (org-roam-fz-fid--lsd-add (fid-make input))
                         :to-equal (fid-make expected)))))

(describe
 "org-roam-fz-fid--lsd-inc"
 (it "increments the LSD of fID by one"
     (cl-loop for (input expected)
              in '(("12" "13")
                   ("12.3" "12.4")
                   ("123ab4c5d" "123ab4c5e")
                   ("123ab4c5" "123ab4c6"))
              do (expect (org-roam-fz-fid--lsd-inc (fid-make input))
                         :to-equal (fid-make expected)))))

(describe
 "org-roam-fz-fid--msd-inc"
 (it "increments the MSD of fID by one"
     (cl-loop for (input expected)
              in '(("12" "13")
                   ("12.3" "13.3")
                   ("123ab4c5d" "124ab4c5d")
                   ("123ab4c5" "124ab4c5"))
              do (expect (org-roam-fz-fid--msd-inc (fid-make input))
                         :to-equal (fid-make expected)))))

(describe
 "org-roam-fz-fid--msd-n"
 (it "take the first n digits of fID from the MSD"
     (cl-loop for (init n expected)
              in '(("12.3a5c" 1 "12")
                   ("12.3a5c" 2 "12.")
                   ("12.3a5c" 3 "12.3"))
              do (expect (org-roam-fz-fid--msd-n (fid-make init) n)
                         :to-equal (fid-make expected)))))

(describe
 "org-roam-fz-fid--new"
 :var* (;; the fID "1.1" exists, so MSD will be incremented
        (expected (fid-make "2.1")))
 (it "finds an fID for a new topic"
     (expect (org-roam-fz-fid--new) :to-equal expected)))

(describe
 "org-roam-fz-fid--related"
 :var ((tries '(("1.1a3b4" "1.2")
                ("1.1" "1.2")
                ("1.1a5" "1.2"))))
 (cl-loop
  for (init expected) in tries
  do
  (let ((init (fid-make init))
        (expected (fid-make expected)))
    (it (format "finds an fID for a topic related to %s" init)
        (expect (org-roam-fz-fid--related init) :to-equal expected)))))

(describe
 "org-roam-fz-fid--follow-up"
 :var (;; (alnum "12.1a")
       ;; (alnum-incremented "12.1a1")
       (tries '(("12.1a" "12.1a1")
                ("12.1c3" "12.1c3a"))))
 (cl-loop
  for (init expected) in tries
  do
  (let ((init (fid-make init))
        (expected (fid-make expected)))
    (it (format "find an fID for a topic following up to %s" init)
        (expect (org-roam-fz-fid--follow-up init) :to-equal expected)))))

(describe
 "org-roam-fz-capt-follow-up"
 :var ((keys "f")
       (description "note for a follow-up topic")
       (file "file")
       (header "header"))
 (cl-loop
  for (template expected-template)
  in '(("template" (lambda ()
                     (org-roam-fz-prepare-capture 'follow-up)
                     "template"))
       ((lambda () "template") (lambda () "template")))
  do
  (it (format "renders a follow-up note template from custom variables"
              " (template: %s)" template)
      (setopt org-roam-fz-target-filename file
              org-roam-fz-capt-follow-up-template template
              org-roam-fz-capt-follow-up-header header)
      (expect (org-roam-fz-capt-follow-up keys description)
              :to-equal `( ,keys ,description plain
                           (function ,expected-template)
                           :target (file+head ,file ,header)
                           :unnarrowed t )))))

(describe
 "org-roam-fz-capt-new"
 :var* ((keys "n")
        (description "note for a new topic")
        (file "file")
        (header "header"))
 (cl-loop
  for (template expected-template)
  in '(("template" (lambda ()
                     (org-roam-fz-prepare-capture 'new)
                     "template"))
       ((lambda () "template") (lambda () "template")))
  do
  (it (format "renders a new template from custom variables"
              " (template: %s)" template)
      (setopt org-roam-fz-target-filename file
              org-roam-fz-capt-new-template template
              org-roam-fz-capt-new-header header)
      (expect (org-roam-fz-capt-new keys description)
              :to-equal `( ,keys ,description plain
                           (function ,template)
                           :target (file+head ,file ,header)
                           :unnarrowed t )))))

(describe
 "org-roam-fz-overlays-render-fid-default"
 :var (fid)
 (it "renders fID correctly"
     (pcase-dolist
         (`(,init ,expected)
          `((,(to-fid "12.3a" "nondefault") "[12.3a(nondefault)]")
            (,(to-fid "12.3a") "[12.3a]")))
       (setq fid (org-roam-fz-fid-make init))
       (expect (org-roam-fz-overlays-render-fid-default fid)
               :to-equal expected))))

(describe
 "org-roam-fz-random-node"
 (it "can set zk noninteractively"
     (expect (org-roam-fz-random-node) :to-equal nil)
     (expect (org-roam-fz-random-node org-roam-fz-zk) :to-equal nil)
     (expect (org-roam-fz-random-node "supplied") :to-equal nil)))

;;; test-org-roam-fz.el ends here
