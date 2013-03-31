;;; -*- coding: utf-8 mode: emacs-lisp -*-
;;; node-console.el --- Execute javascript by node.js from Emacs

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/node-console
;; Version: 0.0.1
;; Package-Requires: ((helm "20130105") (popwin "20130321"))
;; Keywords: javascript

;;; License:
;; This program is free software: you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Requirement
;; This program is used node.js, so you need the program.
;; You can download from `http://nodejs.org/'.
;; And This program is depending on helm.el and popwin.el.
;; so you need that programs too.

;;; Usage
;; (add-to-list 'load-path "path/to/node-console")
;; (require 'node-console)
;; (eval-after-load "js2-mode"
;;   '(progn
;;      (define-key js2-mode-map (kbd "C-c C-c") 'node-console)))

(eval-when-compile (require 'cl))
(require 'popwin)
(require 'helm)
(require 'json)

(defvar node-console-v8-options '())
(defvar node-console-default-environment '(NODE_ENV NODE_PATH))
(defvar node-console-javascript-mode "js2-mode")

(defvar helm-node-console-v8-options-source
  '(((name . "helm-v8-options")
     (candidates . (lambda ()
                     (let* ((console-options
                             (split-string
                              (shell-command-to-string
                               "node --v8-options | grep '  --'") "\n")))
                       (loop with result = '()
                             for option in node-console-v8-options do
                             (loop for opt in console-options
                                   for o = opt
                                   if (string-match option opt)
                                   do (node-console-fontify-char
                                       o 0 (length o)
                                       'font-lock-warning-face))
                             finally return console-options))))
     (action . (lambda (line)
                 (let* ((specified-option
                         (loop  for string in (split-string line " ")
                                if (string-match "--.+" string)
                                do (return (match-string 0 string)))))
                   (loop with result = '()
                         for current-option in node-console-v8-options
                         if (equal current-option specified-option)
                         do (return
                             (node-console-delete-option specified-option))
                         else collect current-option into result
                         finally (setq node-console-v8-options
                                       (push specified-option result)))
                   node-console-v8-options)))
     (nohighlight))))

(defun node-console-delete-option (specified-option)
  (setq node-console-v8-options
        (loop for option in node-console-v8-options
              unless (equal option specified-option)
              collect option))
  (or node-console-v8-options "node-console: all v8 option has just deleted"))

(defun node-console-fontify-char (string idx idx2 face)
  (if (fboundp 'put-text-property)
      (put-text-property  idx idx2 'face face string)))

;;;###autoload
(defun helm-node-console-v8-options ()
  (interactive)
  (helm
   :buffer "*helm v8 options*"
   :prompt "v8 options: "
   :sources helm-node-console-v8-options-source
   :candidates-in-buffer))

(defun node-console-file-ok-p ()
  (or (string-match "\.js$" buffer-file-name)
      (equal node-console-javascript-mode major-mode)))

;;;###autoload
(defun node-console ()
  (interactive)
  (if current-prefix-arg
      (helm-node-console-v8-options)
    (lexical-let*
        ((region
          (when (region-active-p)
            (copy-region-as-kill (region-beginning) (region-end))
            (replace-regexp-in-string "'" "\'" (car kill-ring))))
         (file   (when (node-console-file-ok-p)
                   buffer-file-name))
         (option (mapconcat 'identity node-console-v8-options " "))
         (environment (node-console-extract-environment
                       node-console-default-environment))
         (node (concat environment " node " option " "))
         (command (if region
                      (concat node "-e '" region "'")
                    (concat node file))))
      (popwin:popup-buffer
       (get-buffer-create "*node-console*") :position 'top)
      (erase-buffer)
      (insert (node-console-print command)))))

(defun node-console-print (command)
  (shell-command-to-string (concat "echo " command "; " command)))

(defun node-console-extract-start-script ()
  (interactive)
  (lexical-let*
      ((current (current-buffer))
       (json-file (node-console-locate-package-json))
       json)
    (when json-file
      (save-current-buffer
      (with-temp-buffer
        (find-file json-file)
        (setq json (buffer-string))))
      (switch-to-buffer current)
      (first
       (loop for data in (json-read-from-string json)
             for attribute = (car data)
             if (and (equal attribute 'scripts)
                     (equal 'start (caadr data))
                     (cdadr data))
             collect it)))))

(defun node-console-extract-environment (environment)
  (interactive)
  (let ((script (node-console-extract-start-script)))
    (when script
      (typecase environment
        (list
         (mapconcat 'identity
                    (loop for e in environment
                          collect (node-console-extract-environment e))
                    " "))
        (symbol (let* ((script (node-console-extract-start-script))
                       (env    (symbol-name environment)))
                  (string-match (concat env "=[^ ]+") script)
                  (match-string 0 script)))))))

(defun node-console-locate-package-json ()
  (interactive)
  (loop with dirs = (split-string default-directory "/")
        with file-name = ""
        for name in dirs
        if (string< "" name)
        do (setq file-name (concat file-name "/" name))
        if (file-exists-p (concat file-name "/package.json"))
        do (return (concat file-name "/package.json"))
        finally return nil))

(provide 'node-console)

;;; node-console.el ends here
