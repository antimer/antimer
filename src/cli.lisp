(in-package :cl-user)
(defpackage antimer.cli
  (:use :cl)
  (:export :main)
  (:documentation "The command-line interface."))
(in-package :antimer.cli)

(defparameter +version+
  #.(asdf:component-version (asdf:find-system :saga)))

(defparameter +options+
  `((("help")
     :type boolean
     :documentation "Show usage and exit.")
    (("version")
     :type boolean
     :documentation "Show version information and exit.")))

(defun show-usage ()
  "Show the usage screen."
  (format t "Usage: antimer [COMMAND] [arg...]~%")
  (format t "       antimer [ --help | --version ]~%")
  (format t "~%Options:~%")
  (command-line-arguments:show-option-help +options+)
  (format t "~%Commands:
  serve     Run the server~%"))

(defun serve ()
  "The serve command."
  (antimer.wiki:start
   (make-instance 'antimer.wiki:wiki
                  :directory (uiop:getcwd)
                  :plugins (list
                            (make-instance 'antimer.db:database)))))

(defun entry (&key help version arguments)
  (let ((command (first arguments)))
    (if command
        (alexandria:switch (command :test #'string=)
          ("serve"
           (serve))
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
            (uiop:quit -1)))
  (command-line-arguments:handle-command-line
   +options+
   'entry
    :command-line uiop:*command-line-arguments*
    :name "antimer"
    :rest-arity :arguments))
