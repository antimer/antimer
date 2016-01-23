(in-package :cl-user)
(defpackage antimer.search
  (:use :cl)
  (:shadow :search)
  (:import-from :antimer.plugin
                :plugin
                :name
                :short-description
                :data-directory
                :on-event)
  (:import-from :antimer.event
                :startup
                :shutdown)
  (:export :search)
  (:documentation "The search engine plugin."))
(in-package :antimer.search)

;;; Plugin definition

(defclass search (plugin)
  ((port :accessor plugin-port :type integer)
   (%process :accessor plugin-process))
  (:default-initargs
   :directory-name "search")
  (:documentation "The search engine plugin."))

(defmethod name ((plugin search))
  "Search Engine")

(defmethod short-description ((plugin search))
  "The search engine plugin.")

(antimer.config:register-default-plugin (make-instance 'search))

;;; Elastic tools

(defparameter +url+
  "https://download.elasticsearch.org/elasticsearch/release/org/elasticsearch/distribution/zip/elasticsearch/2.1.1/elasticsearch-2.1.1.zip")

(defparameter +sha1+
  "f62733386bb0ea8b1e34e920de3e86684b6a8cee")

(defun sha1 (file)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :sha1 file)))

(defun verify-file (file)
  (let ((hash (sha1 file)))
    (if (string= +sha1+ hash)
        t
        (error "Verification failed."))))

(defun elastic-directory (plugin)
  (merge-pathnames #p"elasticsearch-2.1.1/" (data-directory plugin)))

(defun elastic-binary (plugin)
  (merge-pathnames (if (uiop:os-windows-p)
                       #p"elasticsearch.bat"
                       #p"elasticsearch")
                   (merge-pathnames #p"bin/"
                                    (elastic-directory plugin))))

(defun download-elastic (plugin)
  (let ((archive (merge-pathnames #p"elastic.zip" (data-directory plugin))))
    (antimer.log:info :search "Downloading Elasticsearch")
    (trivial-download:download +url+ archive :quiet t)
    (antimer.log:info :search "Verifying download")
    (verify-file archive)
    (antimer.log:info :search "Extracting Elasticsearch")
    (trivial-extract:extract-zip archive)
    (delete-file archive)
    (trivial-exe:ensure-executable (elastic-binary plugin))))

(defun configure-elastic (plugin port)
  (let ((file (merge-pathnames #p"config/elasticsearch.yml"
                               (elastic-directory plugin))))
    (with-open-file (stream file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "http.port: ~D~%" port))))

(defun start-elastic (plugin)
  (with-slots (%process) plugin
    (setf %process
          (external-program:start (namestring (elastic-binary plugin))
                                  nil))))

;;; Elastic tools

(defclass server ()
  ((host :reader server-host
         :initarg :host
         :initform "localhost"
         :type string)
   (port :reader server-port
         :initarg :port
         :initform 9300
         :type integer))
  (:documentation "An Elastic server."))

(defun server-uri (server string)
  (with-slots (host port) server
    (format nil "http://~A:~D/~A" host port string)))

(defun article->json (article)
  (cl-json:encode-json-to-string
   (list (cons "title" (antimer.db:article-title article))
         (cons "text" (antimer.db:article-source article)))))

(defun index-article (server id title text)
  (drakma:http-request (server-uri server (format nil "antimer/article/~D" id))
                       :method :put
                       :content
                       (cl-json:encode-json-to-string
                        (list (cons "title" title)
                              (cons "text" text)))))

(defun search-articles (server query)
  (let* ((request `((:query .
                            ((:match .
                               ((:text . ,query)))))
                    (:fields .
                             ("title"))
                    (:highlight .
                                ((:fields .
                                          ((:text . ,(make-hash-table))))))))
         (response (drakma:http-request (server-uri server "antimer/article/_search")
                                        :method :get
                                        :content
                                        (cl-json:encode-json-to-string request))))
    (mapcar #'(lambda (hit)
                (list :title (first (rest (assoc :title (rest (assoc :fields hit)))))
                      :excerpts (rest (assoc :text (rest (assoc :highlight hit))))))
            (rest
             (assoc :hits
                    (rest
                     (assoc :hits
                            (cl-json:decode-json-from-string
                             (babel:octets-to-string response)))))))))

(defun shutdown (server)
  (drakma:http-request (server-uri server "_shutdown")
                       :method :post))

;;; Events

(defmethod on-event ((plugin search) (event startup))
  "On startup, download Elasticsearch if necessary and start it."
  (unless (slot-boundp plugin '%process)
    (unless (probe-file (elastic-directory plugin))
      (download-elastic plugin))
    (let ((port (find-port:find-port)))
      (setf (plugin-port plugin) port)
      (configure-elastic plugin port)
      (antimer.log:info :search "Starting Elasticsearch on port ~D" port)
      (start-elastic plugin))))

(defmacro with-server ((server plugin) &body body)
  `(with-slots (port) ,plugin
     (let ((,server (make-instance 'server :host "localhost" :port port)))
       ,@body)))

(defmethod on-event ((plugin search) (event shutdown))
  "On shutdown, kill the process."
  (when (slot-boundp plugin '%process)
    (with-server (server plugin)
      (antimer.log:info :search "Shutting down Elasticsearch")
      (shutdown server))))

(defmethod on-event ((plugin search) (event antimer.db:update-article))
  "Receive a document, index its raw text."
  (antimer.log:info :search "Indexing article")
  (with-accessors ((id antimer.db:event-id)
                   (slug antimer.db:event-slug)
                   (doc antimer.db:event-document)) event
    (let ((title (common-doc:title doc))
          (text (antimer.doc:raw-text doc)))
      (with-server (server plugin)
        (antimer.log:info :search "Indexing article ~S (~,2f kilobytes of text)"
                          title
                          (/ (length text) 1024))
        (index-article server id title text)))))
