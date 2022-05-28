;;; citar-org-roam.el --- Citar/org-roam integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Bruce D'Arcus
;;
;; Author: Bruce D'Arcus <bdarcus@gmail.com>
;; Maintainer: Bruce D'Arcus <bdarcus@gmail.com>
;; Created: May 22, 2022
;; Modified: May 22, 2022
;; Version: 0.0.1
;; Homepage: https://github.com/bdarcus/citar-org-roam
;; Package-Requires: ((emacs "27.1") (org-roam "2.2") (citar "0.9.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A tiny minor-mode to integrate 'citar' and 'org-roam'.
;;
;;  Provides custom functions for:
;;  
;;  1. updating the 'citar' UI from the 'org-roam' database
;;  2. uses org-roam to open the notes
;;
;;; Code:

(require 'org-roam)
(require 'citar)

(defvar 'citar-open-notes-functions)
(defvar 'citar-create-note-function)
(defvar 'citar-has-note-functions)

;;; Functions

(defun citar-org-roam--has-note-p (key &optional _entry)
  "Return non-nil if a KEY has an associated org-roam ref note."
  (let ((ref-node (org-roam-node-from-ref (concat "@" key))))
    (when ref-node t)))

(defun citar-org-roam-has-note ()
  "Return a predicate function that ..."
  (let ((keys (make-hash-table :test #'equal)))
    ;; Store keys that have notes in hash table
    (dolist (record (org-roam-db-query
                     [:select ref :from refs :where (= type "cite")]))
      (puthash (car record) t keys))
    ;; Return predicate that queries hash table for given key
    (lambda (citekey _entry)
      (gethash citekey keys))))

(defun citar-org-roam-keys-with-notes ()
  "Return a list of keys with associated note(s)."
  (mapcar #'car (org-roam-db-query
                 [:select ref :from refs :where (= type "cite")])))

(defun citar-org-roam-cited (reference)
  "Return a list of notes that cite the REFERENCE."
  (interactive (list (citar-select-ref
                      :rebuild-cache current-prefix-arg
                      :filter (citar-has-note))))
  (let* ((ids
         (org-roam-db-query [:select * :from citations
                             :where (= cite-key $s1)]
                            (car reference)))
         ;; TODO candidates need to be more useful
         (note
          (if ids
              (completing-read "Note: " ids)
            (message "No notes cite this reference."))))
    ;; TODO need to open the note.
    note))

(defun citar-org-roam-open-note (key &optional _entry)
  "Open org-roam node for KEY."
  ;; NOTE I'm unsure what happens if there are multiple notes.
  ;;
  ;;  Ideally this would open multiple nodes/ref, but that would be dependent
  ;;  on:
  ;;
  ;;  https://github.com/org-roam/org-roam/issues/2202
  ;;
  ;;  ALso, this might be smarter; for example, maybe it could open an existing
  ;;  note that does not have a roam_ref, and offer to add it?

  (let ((ref-node (org-roam-node-from-ref (concat "@" key))))
    (when ref-node
      (org-roam-node-open ref-node))))

(defun citar-org-roam-ref-add (reference)
  "Add a roam_ref for REFERENCE to the node at point.

This is just a wrapper for 'org-roam-ref-add'."
  (interactive (list (citar-select-ref)))
  (let ((key (car reference)))
    (org-roam-ref-add (concat "@" key))))

(defun citar-org-roam--create-note (key entry _)
  "Create org-roam node for KEY with ENTRY."
  ;; adapted from https://jethrokuan.github.io/org-roam-guide/#orgc48eb0d
  ;; REVIEW I'm not happy with this ATM, and may remove it.
  (let ((title (citar--format-entry-no-widths
                entry "${author editor} :: ${title}")))
    (org-roam-capture- :templates
                       '(("r" "reference" plain "%?" :if-new
                          (file+head
                           (concat (car citar-notes-paths)) "/${citekey}.org" ; FIX
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey key
                                   :node (org-roam-node-create :title title)
                                   :props '(:finalize find-file)))))

;;; Minor mode
;; REVIEW I'm not sure on the details below, or of this is even needed.
;;        Suggestions welcome.

(defun citar-org-roam-setup ()
  "Setup 'citar-org-roam-mode'."
  ;; REVIEW if I use add-to-list here, it will run both functions.
  (setq citar-open-note-functions '(citar-org-roam-open-note))
  (add-to-list 'citar-has-note-functions 'citar-org-roam-has-note))
  ;; the create-note function doesn't work ATM, so don't change from default
  ;; (setq citar-create-note-function 'citar-org-roam--create-note))

(defun citar-org-roam-reset ()
  "Reset 'citar-org-roam-mode' to default."
  ;; TODO this should be smarter.
  (setq citar-open-note-functions '(citar-org-format-note-default))
  (delete 'citar-org-roam-has-note citar-has-note-functions))
  ;;(kill-local-variable 'citar-create-note-function))

;;;###autoload
(define-minor-mode citar-org-roam-mode
  "Toggle citar-org-roam-mode."
  :global t
  :group 'citar
  :lighter " citar"
  (if citar-org-roam-mode (citar-org-roam-setup)
    (citar-org-roam-reset)))

(provide 'citar-org-roam)
;;; citar-org-roam.el ends here
