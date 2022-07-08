;;; citar-org-roam.el --- Citar/org-roam integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <bdarcus@gmail.com>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: May 22, 2022
;; Version: 0.1
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
(defvar citar-org-roam-notes-config
  `(:name "Org-Roam Ref Notes"
    :category org-roam-node
    :hasnote ,#'citar-org-roam-has-notes
    :open ,#'citar-org-roam-open-note
    :annotate ,#'citar-org-roam--annotate
    :items ,#'citar-org-roam--get-candidates))

(defvar citar-notes-source)
(defvar citar-notes-sources)
(defvar embark-default-action-overrides)

;;; Functions

(defun citar-org-roam--has-note-p (key &optional _entry)
  "Return non-nil if a KEY has an associated org-roam ref note."
  (let ((ref-node (org-roam-node-from-ref (concat "@" key))))
    (when ref-node t)))

(defun citar-org-roam-has-notes (&optional _entries)
  "Return function to check for notes.
When given a citekey, return non-nil if there's an associated
note."
  (let* ((hasnotes (citar-org-roam-keys-with-notes))
         (l (length hasnotes)))
    ;; REVIEW this function needs to be fast. Accessing the db for each key is
    ;; slow. So we collect the keys in one go.
    ;;
    ;; I also borrowed this idea from 'delete-dups' to use a hash-table when
    ;; there are a lot of references with notes; over 100. Is this sound for
    ;; this use case? I think so because this function should be recreated each
    ;; time 'citar-select-ref' is called?
    (if (> l 100)
        (let ((hash (make-hash-table :test #'equal :size l)))
          (dolist (key hasnotes)
            (puthash key t hash))
          (lambda (citekey)
            (gethash citekey hash)))
      (lambda (citekey)
        (member citekey hasnotes)))))

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

(defun citar-org-roam-open-note (id-key entry)
  "Open or creat org-roam node for ID-KEY and ENTRY."
  (let* ((key (car (split-string id-key " ")))
         (ref-node-ids
          (flatten-list
           (org-roam-db-query
            [:select node-id :from refs
             :where (= ref $s1)] key))))
    (if ref-node-ids
        (dolist (id ref-node-ids)
          (let ((ref-node (org-roam-node-from-id id)))
            (org-roam-node-open ref-node)))
      (citar-org-roam--create-capture-note key entry))))

(defun citar-org-roam-open-note-from-id (node-id)
  "Open note from NODE-ID."
  (let ((ref-node (org-roam-node-from-id node-id)))
    (org-roam-node-open ref-node)))

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
  (let ((refs nil))
    (progn
      (if keys
          (dolist (key keys)
            (let ((nodes (citar-org-roam--get-ref-nodes-for-key key)))
              (dolist (node nodes)
                (push node refs))))
        (setq refs (citar-org-roam--get-ref-nodes)))
      (mapcar
       (lambda (ref)
         (let* ((citekey (car ref))
                (nodeid (cadr ref)))
           (concat
            citekey " " (propertize nodeid 'invisible t))))
       refs))))

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
          "%(concat citar-org-roam-subdir \"/\" \"${citekey}.org\")"
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
