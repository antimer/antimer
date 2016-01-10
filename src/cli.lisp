(in-package :cl-user)
(defpackage antimer.cli
  (:use :cl)
  (:export :main)
  (:documentation "The command-line interface."))
(in-package :antimer.cli)

;;; Arguments

(defparameter +version+
  #.(asdf:component-version (asdf:find-system :saga)))

(defparameter +options+
  `((("help")
     :type boolean
     :documentation "Show usage and exit.")
    (("version")
     :type boolean
     :documentation "Show version information and exit.")))

;;; Functions

(defun show-usage ()
  "Show the usage screen."
  (format t "Usage: antimer [COMMAND] [arg...]~%")
  (format t "       antimer [ --help | --version ]~%")
  (format t "~%Options:~%")
  (command-line-arguments:show-option-help +options+)
  (format t "~%Commands:
  serve        Run the server
  make-admin   Create an administrator user~%"))

(defmacro with-wiki ((wiki) &body body)
  `(let ((,wiki (make-instance 'antimer.wiki:wiki
                               :directory (uiop:getcwd)
                               :plugins (list
                                         (make-instance 'antimer.db:database)))))
     ,@body))

(defun serve ()
  "The serve command."
  (with-wiki (wiki)
    (antimer.wiki:start wiki)
    (lucerne:start antimer.app:app :port 8000)
    ;; Wait forever
    (loop (sleep 1000))
    (antimer.wiki:stop wiki)))

(defun prompt (text)
  (format t "~A: " text)
  (finish-output)
  (read-line))

(defun make-admin ()
  "The make-admin command."
  (with-wiki (wiki)
    (antimer.wiki:start wiki)
    (let ((username (prompt "Username"))
          (email (prompt "Email"))
          (password (prompt "Password"))
          (password-repeat (prompt "Repeat password")))
      (unless (string= password password-repeat)
        (error "Passwords don't match."))
      (antimer.db:create-user username
                              :email email
                              :plaintext-password password
                              :adminp t))))

(defun entry (&key help version arguments)
  (let ((command (first arguments)))
    (if command
        (alexandria:switch (command :test #'string=)
          ("serve"
           (serve))
          ("make-admin"
           (make-admin))
          (t
           (error "Unknown command: ~A" command)))
        (cond
          (help
           (show-usage))
          (version
           (format t "~A~%" +version+))
          (t
           ;; No commands or flags, show usage
           (show-usage))))))

(defun main ()
  (setf *debugger-hook*
        #'(lambda (condition hook)
            (declare (ignore hook))
            (format *error-output* "Error: ~A~%" condition)
            (finish-output *error-output*)
            (uiop:quit -1)))
  (command-line-arguments:handle-command-line
   +options+
   'entry
    :command-line uiop:*command-line-arguments*
    :name "antimer"
    :rest-arity :arguments))
