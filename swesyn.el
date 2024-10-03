;;; swesyn.el --- Find Swedish Synonyms (based on swesaurus.xml swedish dataset)  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Erik Karlsson


;; Using lexicon (swesaurus.xml)from:
;;
;;   Swesaurus Dataset
;;   Språkbanken Text,
;;   Lexicon published 2024 via Språkbanken Text
;;   Ett svenskt ordnät
;;   <https://doi.org/10.23695/w5ww-x964>



;; Author: Erik Karlsson 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Interactively look up swedish synonyms
;;

;;; Code:

(require 'ivy)
(require 'sqlite)


(defvar swesyn-db-path (expand-file-name "./swesaurus.db")
  "Path to the SQLite database for converted swesaurus.xml")

(defvar swesyn-db-connection nil
  "SQLite database connection.")


(defvar swesyn-history nil
  "Synonym history")

(defun swesyn-open-connection nil
  "Connect to the SQLite synonyms database."
  (unless swesyn-db-connection
    (setq swesyn-db-connection (sqlite-open swesyn-db-path))))

(defun swesyn-close-connection nil
  "Connect to the SQLite synonyms database."
  (when swesyn-db-connection
    (sqlite-close swesyn-db-path)))

(defun swesyn-query-synonyms (word)
  "Query the swedish synonym SQLite database for swedish synonyms of WORD.
Returns a list of synonyms as strings."
  (swesyn-open-connection)
  (let ((result (sqlite-select swesyn-db-connection
          "SELECT synonyms FROM synonyms WHERE word = ?;" (list word))))
  (if result
      (split-string (caar result) "|")
    nil)))
(provide 'swesyn-query-synonyms)

(defun swesyn-insert-synonym nil
  "Interactively query for a synonym insert it at point."
  (interactive)
  
  (let* ((current-word (thing-at-point 'word t))
         (selected-word (ivy-read  "Find synonyms for: " '()
                                   :initial-input current-word))         
         (synonyms (swesyn-query-synonyms selected-word)))

    (if synonyms
        (ivy-read
         "Select synonym: " synonyms
         :action (lambda (synonym)
                   (when synonym
                     (let ((bounds (bounds-of-thing-at-point 'word)))
                       (when bounds
                         (delete-region (bounds-of-thing-at-point 'word)))
                       (insert synonym)))))
      (message "No synonyms found for '%s'." current-word)))) 

(provide 'swesyn-insert-synonym)

(defun swesyn-add-new-synonym (word new-synonym)
  "Add a `NEW-SYNONYM' for `WORD' in database."
  
  (swesyn-open-connection)

  (let ((current-synonyms (swesyn-query-synonyms word)))
    (if current-synonyms
        ;; Append the new synonym
        (let ((updated-synonyms
               (string-join (append current-synonyms
                                    (list new-synonym))
                            "|")))          
          (sqlite-execute swesyn-db-connection
                           "UPDATE synonyms SET synonyms = ? WHERE word = ?;"
                           (list updated-synonyms word))         
          (message "Added synonym '%s' to the word '%s'." new-synonym word))
      
      ;; Create a new entry
      (sqlite-execute swesyn-db-connection "INSERT INTO synonyms (word, synonyms) VALUES (?, ?);"
                      (list word new-synonym))
     (message "Inserted new word '%s' with synonym '%s'." word new-synonym))))

(provide 'swesyn-add-new-synonym)

(defun swesyn-new-synonym nil
  "Interactively add a new synonym to the SQLite database.
Prompts the user for a word and the new synonym."
  (interactive)

  (let ((word        (read-string "Word: "))
        (new-synonym (read-string "New synonym: ")))
    
    (insert-new-synonym word new-synonym)))

(provide 'swesyn-new-synonym)


(defvar swesyn-mode-map (make-sparse-keymap)
 "Keymap for `swesyn-mode`.")

(define-key swesyn-mode-map (kbd "C-c C-s") #'swesyn-insert-synonym)

(define-minor-mode swesyn-mode
  "Minor mode for looking up Swedish synonyms.
This mode defines `C-c s`, which prompts for a word looking up synonyms in a
SQLite database inserting the match.

- Activate the mode using `M-x swesyn-mode`.
- Use `C-c s` to query for synonyms when the mode is active.

The SQLite database should be properly configured with a table named `synonyms`
that includes two columns: `word` and `synonyms`."
  :lighter " SweSyn"
  :after-hook (connect-to-synonyms-db)
  :before-hook (close-synonyms-db)
  :keymap swesyn-mode-map
  (add-hook 'kill-buffer-hook
            #'swesyn-close-connection))

;; (add-hook 'text-mode-hook 'swesyn-mode)

(provide 'swesyn)
;;; swedish-find-synonyms.el ends here
