;;; citar-org-roam.el --- Citar/org-roam integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <bdarcus@gmail.com>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: May 22, 2022
;; Version: 0.2
;; SPDX-License-Identifier: GPL-3.0-or-later
;; SPDX-FileCopyrightText: 2022 Bruce D'Arcus
;; Homepage: https://github.com/emacs-citar/citar-org-roam
;; Package-Requires: ((emacs "27.1") (org-roam "2.2") (citar "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A minor-mode to integrate 'citar' and 'org-roam'.
;;
;;  Provides functions for:
;;  
;;  1. updating the 'citar' UI from the 'org-roam' database
;;  2. using org-roam to open or create notes
;;
;;; Code:

(require 'org-roam)
(require 'citar)

(defcustom citar-org-roam-subdir "references"
  "Org-roam subdirectory to place reference notes."
  :group 'citar
  :group 'citar-org-roam
  :type 'string)

(defcustom citar-org-roam-note-title-template
  "${author editor} :: ${title}"
  "The citar template to use for formatting new note titles."
  :group 'citar
  :group 'citar-org-roam
  :type 'string)

;; REVEIW experimental config
(defconst citar-org-roam-notes-config
  (list :name "Org-Roam Notes"
        :category 'org-roam-node
        :items #'citar-org-roam--get-candidates
        :hasitems #'citar-org-roam-has-notes
        :open #'citar-org-roam-open-note
        :create #'citar-org-roam--create-capture-note
        :annotate #'citar-org-roam--annotate))

(defvar citar-notes-source)
(defvar citar-notes-sources)
(defvar embark-default-action-overrides)

;;; Functions

(defun citar-org-roam--has-note-p (key &optional _entry)
  "Return non-nil if a KEY has an associated org-roam ref note."
  (let ((ref-node (org-roam-node-from-ref (concat "@" key))))
    (when ref-node t)))

(defun citar-org-roam-has-notes ()
  "Return function to check for notes.
When given a citekey, return non-nil if there's an associated
note."
  ;; Lookup performance for this function needs to be as fast as possible, so we
  ;; use a hash-table.
  (let ((hasnotes (make-hash-table :test 'equal)))
    (dolist (citekey (citar-org-roam-keys-with-notes))
      (puthash citekey t hasnotes))
    (lambda (citekey)
      (gethash citekey hasnotes))))

(defun citar-org-roam-keys-with-notes ()
  "Return a list of keys with associated note(s)."
  (mapcar #'car (org-roam-db-query
                 [:select ref :from refs :where (= type "cite")])))

(defun citar-org-roam-cited (reference)
  "Return a list of notes that cite the REFERENCE."
  (interactive (list (citar-select-ref
                      :filter (citar-has-notes))))
  (let* ((ids
         (org-roam-db-query [:select * :from citations
                             :where (= cite-key $s1)] reference))
         ;; TODO candidates need to be more useful
         (note
          (if ids
              (completing-read "Note: " ids)
            (message "No notes cite this reference."))))
    ;; TODO need to open the note.
    note))

(defun citar-org-roam-open-note (key-id)
  "Open or creat org-roam node for KEY-ID."
  (let ((id (substring-no-properties
             (cadr (split-string key-id)))))
    (citar-org-roam-open-note-from-id id)))

(defun citar-org-roam-open-note-from-id (node-id)
  "Open note from NODE-ID."
  (let ((ref-node (org-roam-node-from-id node-id)))
    (org-roam-node-visit ref-node)))

(defun citar-org-roam-ref-add ()
  "Add a roam_ref to the node at point.

This is just a wrapper for 'org-roam-ref-add'."
  (interactive)
  (let ((ref (citar-select-ref)))
    (org-roam-ref-add (concat "@" ref))))

(defun citar-org-roam--get-ref-nodes-for-key (key)
  "Return ref node ids for KEY."
  (when-let ((ref-node-ids
              (org-roam-db-query
               [:select [ref node-id] :from refs
                :where (= ref $s1)] key)))
    ref-node-ids))

(defun citar-org-roam-citekey-for-node-id (node-id)
  "Return citekey for NODE-ID."
  (caar
   (org-roam-db-query
    [:select ref :from refs :where (= node-id $s1)] node-id)))

(defun citar-org-roam--get-ref-nodes ()
  "Return all ref nodes as id and ref pair."
  (when-let ((ref-nodes
              (org-roam-db-query
               [:select [ref node-id] :from refs])))
    ref-nodes))

(defun citar-org-roam--annotate (candidate)
  "Annotate the CANDIDATE."
  (when-let* ((pair (split-string
                     (substring-no-properties candidate)))
              (nodeid (cadr pair))
              (citekey (car pair))
              (node (org-roam-node-from-id nodeid))
              (ref (org-roam-db-query
                    [:select ref :from refs
                     :where (= node-id $s1)] nodeid)))
    (concat
     "          " ; not thrilled with this
     (truncate-string-to-width
      (propertize citekey 'face 'citar-highlight) 15 nil 32)
     (propertize (org-roam-node-title node) 'face 'citar))))

(defun citar-org-roam--get-candidates (&optional keys)
  "Return ref node candidate list, optionally filtered by KEYS.

Each candidate is a 'citekey' + 'node-id' string, separated by a
space."
  ;; REVIEW experimental
  (let ((refs (if keys
                  (mapcan #'citar-org-roam--get-ref-nodes-for-key keys)
                (citar-org-roam--get-ref-nodes))))
    (mapcar
     (lambda (ref)
       (let* ((citekey (car ref))
              (nodeid (cadr ref)))
         (concat
          citekey " " (propertize nodeid 'invisible t))))
     refs)))

(defun citar-org-roam-select-ref ()
  "Return org-roam node-id for ref candidate."
  ;; TODO just a demo ATM
  (completing-read "ref:" (citar-org-roam--get-candidates)))

(defun citar-org-roam--create-capture-note (citekey entry)
    "Open or create org-roam node for CITEKEY and ENTRY."
   ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
    (let ((title (citar-format--entry
                   citar-org-roam-note-title-template entry)))
     (org-roam-capture-
      :templates
      '(("r" "reference" plain "%?" :if-new
         (file+head
          ;; REVIEW not sure the file name shoud be citekey alone.
          "%(concat
 (when citar-org-roam-subdir (concat citar-org-roam-subdir \"/\")) \"${citekey}.org\")"
          ":PROPERTIES:
:ROAM_REFS: @${citekey}
:END:
 #+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t))
      :info (list :citekey citekey)
      :node (org-roam-node-create :title title)
      :props '(:finalize find-file))))

(defvar citar-org-roam--orig-source citar-notes-source)

(defun citar-org-roam-setup ()
  "Setup 'citar-org-roam-mode'."
  (citar-register-notes-source
   'citar-org-roam citar-org-roam-notes-config)
  (setq citar-notes-source 'citar-org-roam))

(defun citar-org-roam-reset ()
  "Reset 'citar-org-roam-mode' to default."
  (setq citar-notes-source citar-org-roam--orig-source)
  (citar-remove-notes-source 'citar-org-roam))

;;;###autoload
(define-minor-mode citar-org-roam-mode
  "Toggle citar-org-roam-mode."
  :global t
  :group 'citar
  :lighter " citar-org-roam"
  (if citar-org-roam-mode (citar-org-roam-setup)
    (citar-org-roam-reset)))

(provide 'citar-org-roam)
;;; citar-org-roam.el ends here
