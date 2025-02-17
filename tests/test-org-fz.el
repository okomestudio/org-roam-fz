;;; test-org-roam-fz.el --- Tests for org-roam-fz  -*- lexical-binding: t -*-
;;
;; Package-Requires: ((buttercup))
;;
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'org-roam-fz)
(require 'cl)

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
 "org-roam-fz-fid--split-alnum"
 (it "splits fID string into components"
     (expect (org-roam-fz-fid--split-alnum "12.2a")
             :to-equal '("a" "2" "." "12")))
 (it "splits fID string without the comma component into components"
     (expect (org-roam-fz-fid--split-alnum "2a3c")
             :to-equal '("c" "3" "a" "2")))
 (it "throws an error on invalid fID string"
     (expect (org-roam-fz-fid--split-alnum "@5*2a3c")
             :to-throw 'error)))

(describe
 "org-roam-fz-fid--exists"
 (it "returns non-nil when fID exists"
     (expect (org-roam-fz-fid--exists
              (org-roam-fz-fid-make (format "1.1-%s" org-roam-fz-zk)))
             :not :to-equal nil))
 (it "returns nil when fID does not exist"
     (expect (org-roam-fz-fid--exists
              (org-roam-fz-fid-make (format "99.1-%s" org-roam-fz-zk)))
             :to-equal nil)))

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
 "org-roam-fz-fid--child-add"
 (it "adds a child to fID"
     (dolist (try '(("12.3-default" "12.3a-default")
                    ("123ab4c5d-default" "123ab4c5d1-default")
                    ("123ab4c5-default" "123ab4c5a-default")))
       (cl-destructuring-bind
           (input expected) try
         (expect
          (org-roam-fz-fid--child-add (org-roam-fz-fid-make input))
          :to-equal (org-roam-fz-fid-make expected))))))

(describe
 "org-roam-fz-fid--sibling-inc"
 (it "adds a sibling to fID"
     (dolist (try '(("12.3-default" "12.4-default")
                    ("123ab4c5d-default" "123ab4c5e-default")
                    ("123ab4c5-default" "123ab4c6-default")))
       (cl-destructuring-bind
           (input expected) try
         (expect
          (org-roam-fz-fid--sibling-inc (org-roam-fz-fid-make input))
          :to-equal (org-roam-fz-fid-make expected))))))

(describe
 "org-roam-fz-fid-prompt"
 :var* ((alnum "12.1a"))
 (it "renders fID from user input"
     (cl-letf (((symbol-function 'read-string) (lambda (s) alnum)))
       (expect (org-roam-fz-fid-prompt 'alnum)
               :to-equal alnum)
       (expect (org-roam-fz-fid-prompt 'zk)
               :to-equal org-roam-fz-zk)
       (expect (org-roam-fz-fid-prompt 'full)
               :to-equal (concat alnum "-" org-roam-fz-zk)))))

(describe
 "org-roam-fz-fid-follow-up"
 :var* ((org-roam-db-location "roam-test.db")
        (org-roam-directory "/tmp")
        (alnum "12.1a")
        (alnum-incremented "12.1a1")
        (org-roam-fz-zk "zk"))
 (before-each
  (setq org-roam-fz--id (concat alnum "-" org-roam-fz-zk)))

 (it "renders alnum of the follow-up fID"
     (expect (org-roam-fz-fid-follow-up 'alnum) :to-equal alnum-incremented))
 (it "renders zk of the follow-up fID"
     (expect (org-roam-fz-fid-follow-up 'zk) :to-equal org-roam-fz-zk))
 (it "renders full of the follow-up fID"
     (expect (org-roam-fz-fid-follow-up 'full)
             :to-equal (concat alnum-incremented "-" org-roam-fz-zk))))

(describe
 "org-roam-fz-fid-follow-up"
 :var* ((alnum "12.1a")
        (org-roam-fz-zk "zk"))
 (before-each
  (setq org-roam-fz--id nil))
 (it "falls back to org-roam-fz-fid-prompt ig org-roam-fz--id not set"
     (cl-letf (((symbol-function 'read-string) (lambda (s) alnum)))
       (expect (org-roam-fz-fid-follow-up 'full)
               :to-equal (concat alnum "-" org-roam-fz-zk)))))

(describe
 "org-roam-fz-capture-template-follow-up"
 :var* ((keys "f")
        (description "note for a follow-up topic")
        (template (lambda () "template"))
        (file "file")
        (header "header")
        (expected `(,keys ,description plain (function ,template)
                          :target (file+head ,file ,header)
                          :unnarrowed t)))
 (it "renders a follow-up note template from custom variables"
     (setopt org-roam-fz-capture-template-follow-up-template template
             org-roam-fz-capture-template-follow-up-file file
             org-roam-fz-capture-template-follow-up-header header)
     (expect (org-roam-fz-capture-template-follow-up keys description)
             :to-equal expected)))

(describe
 "org-roam-fz-capture-template-new"
 :var* ((keys "n")
        (description "note for a new topic")
        (template "template")
        (file "file")
        (header "header")
        (expected `(,keys ,description plain ,template
                          :target (file+head ,file ,header)
                          :unnarrowed t)))
 (it "renders a new template from custom variables"
     (setopt org-roam-fz-capture-template-new-template template
             org-roam-fz-capture-template-new-file file
             org-roam-fz-capture-template-new-header header)
     (expect (org-roam-fz-capture-template-new keys description)
             :to-equal expected)))

;;; test-org-fz.el ends here
