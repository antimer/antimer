(in-package :cl-user)
(defpackage antimer.static
  (:use :cl)
  (:import-from :antimer.wiki
                :wiki-static-directory)
  (:export :static-pathname
           :existsp
           :copy-file
           :copy-directory)
  (:documentation "Tools for static files."))
(in-package :antimer.static)

(defun static-pathname (wiki pathname)
  (merge-pathnames pathname (wiki-static-directory wiki)))

(defun existsp (wiki pathname)
  (and (probe-file (static-pathname wiki pathname)) t))

(defun copy-file (wiki source destination)
  "Copy a file from @cl:param(source), an absolute pathname to any file, to
@cl:param(destination), a relative pathname within the static directory."
  (assert (uiop:absolute-pathname-p source))
  (assert (uiop:relative-pathname-p destination))
  (let ((target (static-pathname wiki destination)))
    (ensure-directories-exist (uiop:pathname-directory-pathname target))
    (uiop:copy-file source target)))

(defun copy-directory (wiki source destination)
  "Copy everything under source to destination."
  (assert (uiop:directory-pathname-p source))
  (assert (uiop:absolute-pathname-p source))
  (assert (uiop:directory-pathname-p destination))
  (assert (uiop:relative-pathname-p destination))
  (let ((destination (static-pathname wiki destination)))
    (ensure-directories-exist destination)
    (fad:walk-directory source
                        #'(lambda (pathname)
                            (unless (equal pathname source)
                              (let* ((relative-path (uiop:subpathp pathname source))
                                     (target (merge-pathnames relative-path
                                                              destination)))
                                (if (uiop:directory-pathname-p pathname)
                                    ;; Ensure an equivalent directory exists
                                    (ensure-directories-exist target)
                                    ;; Copy the absolute source file to the target
                                    (uiop:copy-file pathname target)))))
                      :directories :breadth-first
                      :follow-symlinks nil)
    destination))
