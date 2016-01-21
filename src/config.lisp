(in-package :cl-user)
(defpackage antimer.config
  (:use :cl)
  (:import-from :alexandria
                :compose)
  (:export :config
           :config-plugins
           :parse
           :register-default-plugin)
  (:documentation "Parse the configuration."))
(in-package :antimer.config)

(defclass config ()
  ((plugins :reader config-plugins
            :initarg :plugins
            :type list
            :documentation "A list of plugin objects."))
  (:documentation "The configuration for a wiki."))

(defparameter *default-plugins* (list))

(defun find-by-class (instance sequence)
  "Find an element of sequence that has the same class as instance."
  (find (class-name (class-of instance))
        sequence
        :key (compose #'class-name #'class-of)
        :test #'eq))

(defun register-default-plugin (instance)
  "Add a plugin instance to the *default-plugins*, if it doesn't exist."
  (unless (find-by-class instance *default-plugins*)
    (push instance *default-plugins*)))

(defun parse (pathname)
  "Parse the configuration from a YAML file."
  (unless (probe-file pathname)
    (error "No config.yaml file."))
  (let ((data (yaml:parse pathname))
        (*read-eval* nil))
    (make-instance 'config
                   :plugins
                   (reverse
                    (parse-plugins (gethash "plugins" data))))))

(defun parse-plugins (plugins)
  (let ((parsed (mapcar #'parse-plugin plugins)))
    (unless (= (length (remove-duplicates parsed
                                          :key (compose #'class-name #'class-of)
                                          :test #'eq))
               (length plugins))
      (error "Can't have more than one instance of the same plugin."))
    (append (remove-if #'null
                       (mapcar #'(lambda (plugin)
                                   (if (find-by-class plugin parsed)
                                       ;; Plugin supplied
                                       nil
                                       ;; Plugin not supplied, add it
                                       plugin))
                               *default-plugins*))
            parsed)))

(defun parse-plugin (plugin)
  "Parse a plugin."
  (flet ((read-symbol (string)
           (read-from-string (format nil ":~A" string))))
    (let ((class-name (read-from-string (or (gethash "name" plugin)
                                            (error "Plugin name not specified."))))
          (arguments (mapcar #'(lambda (pair)
                                 (cons (read-symbol (first pair))
                                       (rest pair)))
                             (alexandria:hash-table-alist (or (gethash "args" plugin)
                                                              (make-hash-table))))))
      (apply #'make-instance class-name (alexandria:alist-plist arguments)))))
