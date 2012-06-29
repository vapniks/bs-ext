;;; bs-ext.el --- Extensions to emacs buffer-selection library (bs.el)

;; Filename: bs-ext.el
;; Description: Extensions to emacs buffer-selection library (bs.el)
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-06-28 14:23:56
;; Version: 0.1
;; Last-Updated: 2012-06-28 14:23:56
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/bs-ext.el
;; Keywords: convenience files
;; Compatibility: GNU Emacs 24.0.50.2
;;
;; Features that might be required by this library:
;;
;; bs.el
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; Extensions to emacs buffer-selection library (bs.el)
;; 

;;; Installation:
;;
;; Put bs-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'bs-ext)

;;; Customize:
;;
;; 
;;
;; All of the above can customized by:
;;      M-x customize-group RET bs RET
;;

;;; Change log:
;;	
;; 2012/06/28
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;; Setup regexp configuration. 
;; Create method for dynamically creating new configurations, adding buffers to them, and creating keys for them.

;;; Require
(require 'bs)

;;; Code:

(defcustom bs-ext-config-keys nil
  "Alist of (KEY . CONFIG) pairs.
CONFIG is the name of a configuration listed in `bs-configurations', and KEY is a key that loads that config when pressed
in the *buffer-selection* buffer."
  :type '(alist :key-type (string :tag "Key"
                                  :help-echo (lambda (w) (concat "The key to press to load this configuration. Must not be one of the following (already used) keys:\n"
                                                                 (mapconcat 'identity (loop for key being the key-codes of bs-mode-map
                                                                                            collect (single-key-description key)) " ")))
                                  :match (lambda (w key)
                                           (or (member key (mapcar 'car bs-ext-config-keys))
                                               (not (member key (loop for key being the key-codes of bs-mode-map
                                                                      collect (single-key-description key)))))))
                :value-type (string :tag "Config name"
                                    :help-echo (lambda (w)
                                                 (concat "The name of the configuration to load. Must be one of the following:\n"
                                                         (mapconcat 'car bs-configurations " ")))
                                    :match (lambda (w config) (member config (mapcar 'car bs-configurations)))))
  :set 'bs-ext-set-keys
  :group 'bs)

(defun bs-ext-set-keys (symb val)
  "Set the key-bindings for the different configurations.
This function is used for setting the keys after saving the customization buffer for `bs-ext-config-keys'.
If called in other code then SYMB should be 'bs-ext-config-keys and val should be bs-ext-config-keys."
  (loop for (key . name) in val
        do (define-key bs-mode-map (read-kbd-macro key)
             `(lambda nil (interactive) (bs--show-with-configuration ,name))))
  (set-default symb val))

(defcustom bs-ext-show-configs-header t
  "Whether or not to show the configs header line (as returned by `bs-ext-show-configs-header')."
  :type 'boolean
  :group 'bs)

(defun bs-ext-show-configs-header ()
  "Insert header line showing configuration names and corresponding keys."
  (let* ((line (mapconcat (lambda (conf)
                            (let* ((name (car conf))
                                   (key (car (rassoc name bs-ext-config-keys)))
                                   (item (if key (concat name "(" key ")") name)))
                              (if (equal name bs-current-configuration)
                                  (propertize item 'face font-lock-comment-face) ;(list :background "black" :foreground "red"))
                                item)))
                          bs-configurations " "))
         (numlines (ceiling (/ (float (length line)) (window-width)))))
    (insert " " line "\n")
    (setq bs-header-lines-length (+ numlines 2))))

;; We need to redefine this function (originally defined in bs.el) so that it also inserts the configs header line
(defun bs-show-in-buffer (list)
  "Display buffer list LIST in buffer *buffer-selection*.
Select buffer *buffer-selection* and display buffers according to current
configuration `bs-current-configuration'.  Set window height, fontify buffer
and move point to current buffer."
  (setq bs-current-list list)
  (switch-to-buffer (get-buffer-create "*buffer-selection*"))
  (bs-mode)
  (let* ((inhibit-read-only t)
	 (map-fun (lambda (entry)
		    (length (buffer-name entry))))
	 (max-length-of-names (apply 'max
				     (cons 0 (mapcar map-fun list))))
	 (name-entry-length (min bs-maximal-buffer-name-column
				 (max bs-minimal-buffer-name-column
				      max-length-of-names))))
    (erase-buffer)
    (setq bs--name-entry-length name-entry-length)
    (bs--show-header)
    (dolist (buffer list)
      (bs--insert-one-entry buffer)
      (insert "\n"))
    (delete-char -1)
    (bs--set-window-height)
    (font-lock-fontify-buffer)
    (goto-char (point-min))
    (if bs-ext-show-configs-header
        (bs-ext-show-configs-header)
      (setq bs-header-lines-length 2))
    (bs--goto-current-buffer)    
    (bs-apply-sort-faces)
    (set-buffer-modified-p nil)))

(defun bs-prev-config-aux (start-name list)
  "Get the previous assoc before START-NAME in list LIST.
Will return the last if START-NAME is at start."
  (let ((assocs list)
	(length (length list))
	pos)
    (while (and assocs (not pos))
      (when (string= (car (car assocs)) start-name)
	(setq pos (- length (length assocs))))
      (setq assocs (cdr assocs)))
    (if (eq pos 0)
	(nth (1- length) list)
      (nth (1- pos) list))))

(defun bs-prev-config (name)
  "Return previous configuration with respect to configuration with name NAME."
  (bs-prev-config-aux name bs-configurations))

(defun bs-select-previous-configuration (&optional start-name)
  "Apply previous configuration to START-NAME and refresh buffer list.
If START-NAME is nil the current configuration `bs-current-configuration'
will be used."
  (interactive)
  (let ((config (bs-prev-config (or start-name bs-current-configuration))))
    (bs-set-configuration (car config))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-message-without-log "Selected configuration: %s" (car config))))

(defvar bs-ext-regexp ".*"
  "Regexp with which to match buffer names for buffer show `regexp' configuration.")

(defvar bs-ext-regexp-history '()
  "History list for use in aleblanc/bs-set-regexp.")

(defun bs-ext-set-regexp (regexp)
  "Set the value of bs-ext-regexp - a regexp to match buffer names for the regexp configuration."
  (interactive (list (read-string "Regexp to match buffer names: " nil 'bs-ext-regexp-history)))
  (setq bs-ext-regexp regexp)
  (add-to-list 'bs-ext-regexp-history 'regexp))

;; ("regexp" nil
;;  (lambda
;;    (buf)
;;    (string-match aleblanc/bs-regexp
;;                  (buffer-name buf)))
;;  nil
;;  (lambda
;;    (buf)
;;    (not
;;     (string-match aleblanc/bs-regexp
;;                   (buffer-name buf))))
;;  nil)



;; Set some new keys
(define-key bs-mode-map (kbd "<left>") 'bs-select-previous-configuration)
(define-key bs-mode-map (kbd "<right>") 'bs-select-next-configuration)
(define-key bs-mode-map (kbd "x") 'bs-delete)
;; Set the config keys
(bs-ext-set-keys 'bs-ext-config-keys bs-ext-config-keys)



(provide 'bs-ext)

;;; bs-ext.el ends here

