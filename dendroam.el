;;; dendroam.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Victor Rodriguez
;;
;; Author: Victor Rodriguez <https://github.com/vrodriguez>
;; Maintainer: Victor Rodriguez <vrodriguez@confluent.io>
;; Created: April 26, 2021
;; Modified: April 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/vrodriguez/dendroam
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(provide 'dendroam)

;;Node custom getters
(cl-defmethod org-roam-node-hierarchy-title (node)
  "Gets node title excluding the hierarchy and capitalize it."
  (capitalize (car (last (split-string (org-roam-node-title node) (rx "."))))))

(cl-defmethod org-roam-node-spacing (node)
  (string-pad " " (- 25 (length (funcall 'dendroam-format-hierarchy (org-roam-node-file node))))))

(defun dendroam-format-hierarchy (file)
  "Formats node's path, to get the hierarchy without the title
where title will be the last child of the hierarchy:
from the filename this.is.a.hierarchy.note-title.org
returns this.is.a.hierarchy"
  (let* ((base-name (file-name-base file))
         (hierarchy-p (> (cl-count ?\. base-name) 0))
         (hierarchy-no-title (file-name-base base-name)))
    (if hierarchy-p
        (concat hierarchy-no-title ".")
      "")))

(cl-defmethod org-roam-node-hierarchy (node)
  "Gets hierarchy of NODE by file name."
  (funcall 'dendroam-format-hierarchy (org-roam-node-file node)))

(cl-defmethod org-roam-node-current-file (node)
  "Gets the name of the current NODE."
  (file-name-base (org-roam-node-file node)))

;; Refactor functions

(defun dendroam-fetch-same-hierarchy-files (hierarchy)
  "Gets all the nodes that share the same HIERARCHY totally or parcially"
  (let ((files (mapcar #'car (org-roam-db-query [:select [file]
                                                         :from nodes
                                                         :where (like file $r1)]
                                                (concat "%" hierarchy "%")))))
    files))

(defun dendroam-refactor-hierarchy (&optional current)
  "Prompts the user to change the hierarchy of the current file
node and updates its hierarchy and the hierarchy of all the nodes
that have it. if CURRENT is non-nil, the list of updated files is just
the current file."
  (interactive)
  (let* ((initial-file
          (file-name-nondirectory (buffer-file-name)))
         (initial-slug
          (file-name-base initial-file))
         (new-slug (file-name-base
                    (read-string (format "Refactor (from %s to ): " initial-slug) initial-slug)))
         (initial-slug-no-title
          (file-name-base initial-slug))
         (files-to-upd (if current
                           `(,initial-file)
                         (dendroam-fetch-same-hierarchy-files
                          initial-slug-no-title))))

    (dolist (file files-to-upd)
      (let ((new-file
             (replace-regexp-in-string initial-slug-no-title new-slug file)))
        (rename-file file new-file)
        (if (equal buffer-file-name file)
            (progn
              (kill-current-buffer)
              (find-file new-file)))))))

(defvar-local org-roam-slug-trim-chars
    '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
      768 ; U+0300 COMBINING GRAVE ACCENT
      769 ; U+0301 COMBINING ACUTE ACCENT
      770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
      771 ; U+0303 COMBINING TILDE
      772 ; U+0304 COMBINING MACRON
      774 ; U+0306 COMBINING BREVE
      775 ; U+0307 COMBINING DOT ABOVE
      776 ; U+0308 COMBINING DIAERESIS
      777 ; U+0309 COMBINING HOOK ABOVE
      778 ; U+030A COMBINING RING ABOVE
      780 ; U+030C COMBINING CARON
      795 ; U+031B COMBINING HORN
      803 ; U+0323 COMBINING DOT BELOW
      804 ; U+0324 COMBINING DIAERESIS BELOW
      805 ; U+0325 COMBINING RING BELOW
      807 ; U+0327 COMBINING CEDILLA
      813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
      814 ; U+032E COMBINING BREVE BELOW
      816 ; U+0330 COMBINING TILDE BELOW
      817 ; U+0331 COMBINING MACRON BELOW
      )
  "Characters to remove in the slug of an org roam node.")

;; Org roam overrides to allow these features
(eval-after-load "org-roam"
  '(cl-defmethod org-roam-node-slug ((node org-roam-node))
     "Override. Generates a dendron-like slug from *title*
this expects an input like: lang.elisp.what is nil
and will create a file wih the name: lang.elisp.what-is-nil"
     (let ((title (org-roam-node-title node))) ; Gets the title of the node
       ;; Defining simple functions
       (cl-flet* ((nonspacing-mark-p (char) ; Checks if character is an accent / combining thing
                    (memq char org-roam-slug-trim-chars))
                  (strip-nonspacing-marks (s) ; Decompose the string and remove all accents
                    (ucs-normalize-NFC-string
                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                 (ucs-normalize-NFD-string s)))))
                  (cl-replace (title pair) ; Replace parts in the title with another thing
                    (replace-regexp-in-string (car pair) (cdr pair) title)))
         ;; Defining replacement pairs
         (let* ((pairs `((,(rx (+ (not (any "." alnum digit)))) . "-")  ;; convert anything not alphanumeric except "."
                         (,(rx (+ " ")) . "-")    ;; remove whitespaces
                         (,(rx "_" (* "_")) . "-")  ;; remove sequential underscores
                         (,(rx line-start "_") . "")  ;; remove starting underscore
                         (,(rx "_" line-end) . "")))  ;; remove ending underscore
                (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
           (downcase slug))))))
;;; dendroam.el ends here
