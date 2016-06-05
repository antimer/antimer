(in-package :cl-user)
(defpackage antimer.plugin.fonts
  (:use :cl :antimer.plugin)
  (:import-from :antimer.wiki
                :wiki
                :wiki-temporary-directory)
  (:export :fonts-plugin)
  (:documentation "The font downloader plugin."))
(in-package :antimer.plugin.fonts)

;;; Classes

(defclass fonts-plugin (static-generator)
  ()
  (:documentation "A font downloader plugin."))

;;; Variables

(defparameter +bembo-commit+
  "7e8f02dadcc23ba42b491b39e5bdf16e7b383031")

(defparameter +bembo-base-url+
  (format nil "https://github.com/edwardtufte/et-book/raw/~A/et-book" +bembo-commit+))

;;; Methods

(defmethod generate ((wiki wiki) (plugin fonts-plugin))
  "Ensure all the fonts are downloaded."
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
                                      (wiki-temporary-directory wiki)))
               (dest (parse-namestring (format nil "fonts/et-book/~A.ttf" font))))
          (unless (antimer.static:existsp wiki dest)
            (trivial-download:download (font-url font) file :quiet t)
            (antimer.static:copy-file wiki file dest)
            (delete-file file)))))))
