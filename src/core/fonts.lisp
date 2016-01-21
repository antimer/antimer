(in-package :cl-user)
(defpackage antimer.fonts
  (:use :cl)
  (:import-from :antimer.plugin
                :plugin
                :name
                :short-description
                :data-directory
                :on-event)
  (:import-from :antimer.event
                :startup
                :shutdown)
  (:export :fonts)
  (:documentation "A plugin to download fonts."))
(in-package :antimer.fonts)

;;; Plugin definition

(defclass fonts (plugin)
  ()
  (:default-initargs
   :directory-name "fonts")
  (:documentation "The font downloader plugin."))

(defmethod name ((plugin fonts))
  "Fonts")

(defmethod short-description ((plugin fonts))
  "Downloads fonts.")

(antimer.config:register-default-plugin (make-instance 'fonts))

;;; Events

(defparameter +bembo-commit+
  "7e8f02dadcc23ba42b491b39e5bdf16e7b383031")

(defparameter +bembo-base-url+
  (format nil "https://github.com/edwardtufte/et-book/raw/~A/et-book" +bembo-commit+))

(defmethod on-event ((plugin fonts) (event startup))
  "On startup, ensure all the fonts are downloaded."
  (antimer.log:info :fonts "Downloading fonts")
  (let ((fonts (list "et-bembo-bold-line-figures"
                     "et-bembo-display-italic-old-style-figures"
                     "et-bembo-roman-line-figures"
                     "et-bembo-roman-old-style-figures"
                     "et-bembo-semi-bold-old-style-figures")))
    (flet ((font-url (font)
             (format nil "~A/~A/~A.ttf" +bembo-base-url+ font font)))
      (dolist (font fonts)
        (let* ((file (merge-pathnames #p"font.ttf"
                                      (data-directory plugin)))
               (dest (parse-namestring (format nil "fonts/et-book/~A.ttf" font))))
          (unless (antimer.static:existsp dest)
            (trivial-download:download (font-url font) file :quiet t)
            (antimer.static:copy-file file dest)
            (delete-file file)))))))
