;; emp.el --- EMP (Emacs Message Passing) -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 yelobat
;;
;; Author: yelobat
;; Maintainer: yelobat
;; Created: September 07, 2024
;; Version: 0.0.1
;; Homepage: https://github.com/yelobat/emp
;; Package-Requires: ((emacs "28.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(defvar emp-name nil)
(defvar emp-buffer nil)
(defvar emp-host nil)
(defvar emp-port nil)
(defvar emp-sentinel nil)
(defvar emp-filter nil)
(defvar emp-process nil)
(defvar emp-eot (string #o4))
(defvar emp-delimiter (string #x2c))

(defun emp-either (a b)
  (if a a b))

(defun emp-connect (name buffer host port &optional sentinel filter)
  "Connect EMP to your server."
  (interactive)
  (if emp-process
      (message "A connection already exists.")
    (condition-case err
	(let ((process (make-network-process :name name
                                             :buffer buffer
                                             :family 'ipv4
                                             :host host
                                             :service port
                                             :sentinel (emp-either
							sentinel
							'emp-listen-sentinel)
                                             :filter (emp-either
						      filter
						      'emp-listen-filter))))
          (if process
              (setq emp-name name
		    emp-buffer buffer
		    emp-host host
		    emp-port port
		    emp-sentinel sentinel
		    emp-filter filter
		    emp-process process)
            (error "Failed to create network process")))
      (error
       (setq emp-name nil
             emp-buffer nil
             emp-host nil
             emp-port nil
             emp-sentinel nil
             emp-filter nil
             emp-process nil)
       (message "Connection failed: %s" (error-message-string err)))))
  )

(defun emp-define-eot (eot-token)
  "Defines the end of transmission token used by EMP."
  (interactive)
  (setq emp-eot eot-token))

(defun emp-define-delimiter (delimiter-token)
  "Defines the delimiter token used by EMP."
  (interactive)
  (setq emp-delimiter delimiter-token))

(defun emp-listen-sentinel (proc string)
  "Determines if server has terminated."
  (when (string-match "connection broken by remote peer" string)
    (with-current-buffer emp-buffer (erase-buffer))
    (setq emp-name nil
          emp-buffer nil
          emp-host nil
          emp-port nil
          emp-sentinel nil
          emp-filter nil
          emp-process nil)
    (message (format "client %s has quit" proc))))

(defun emp-listen-filter (proc string)
  "Processes messages on retrieval."
  (with-current-buffer emp-buffer
    (goto-char (point-min))
    (insert emp-delimiter)
    (insert string)))

(defun emp-clear-buffer ()
  (when emp-buffer
    (with-current-buffer emp-buffer (erase-buffer))
    ))

(defun emp-prepare-message (message)
  (concat message emp-eot)
  )

(defun emp-message (message)
  (when emp-process
  (process-send-string emp-process (emp-prepare-message message))))


