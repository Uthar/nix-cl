(defpackage org.lispbuilds.nix/repository/quicklisp-projects
  (:documentation "Ulitity for working on the quicklisp-projects repository")
  (:use :cl)
  (:import-from :sqlite)
  (:import-from :uiop)
  (:import-from :str)
  (:import-from :alexandria-2 :line-up-first :line-up-last)
  (:import-from
   :org.lispbuilds.nix/database/sqlite
   :sqlite-database
   :init-db
   :database-url
   :init-file)
  (:import-from
   :org.lispbuilds.nix/api
   :import-lisp-packages)
  (:import-from
   :org.lispbuilds.nix/util
   :replace-regexes
   :comment)
  (:local-nicknames
   (:a :alexandria)
   (:p :lparallel)
   (:json :com.inuoe.jzon)
   (:http :dexador))
  (:export :quicklisp-projects-repository))

(in-package org.lispbuilds.nix/repository/quicklisp-projects)

(defun sh (command)
  (let ((ld-library-path (uiop:getenv "LD_LIBRARY_PATH")))
    (setf (uiop:getenv "LD_LIBRARY_PATH") "")
    (unwind-protect
         (uiop:run-program command :output t :error-output t)
      (setf (uiop:getenv "LD_LIBRARY_PATH") ld-library-path))))

(defun clone-projects-repo ()
  (sh "git clone https://github.com/quicklisp/quicklisp-projects"))

(defparameter *broken-projects*
  (list
   ;; SSL issues (mostly mfiano.net)
   "avl-tree"
   "binary-parser"
   "binary-search-tree"
   "convolution-kernel"
   "cricket"
   "cubic-bezier"
   "doubly-linked-list"
   "dungen"
   "dynamic-array"
   "flac-metadata"
   "freebsd-ffi"
   "gfxmath"
   "glsl-metadata"
   "grid-formation"
   "identifier-pool"
   "mfiano-utils"
   "origin"
   "osmpbf"
   "patchwork"
   "quad-tree"
   "random-uuid"
   "red-black-tree"
   "seedable-rng"
   "shadow"
   "slot-map"
   "sparse-set"
   "stripe"
   "tile-grid"
   "umbra"
   ;; Does not exist (hg bitbucket)
   "cl-abstract-classes"
   "map-set"
   "big-string"
   "cl-gap-buffer"
   "cl-generic-arithmetic"
   "cl-locatives"
   "cl-ntriples"
   "cl-stopwatch"
   "cl-string-complete"
   "clod"
   "defrec"
   "dynamic-collect"
   "interface"
   "json-responses"
   "letrec"
   "parameterized-function"
   "recur"
   "synonyms"
   "template"
   ;; Does not exist (http 404)
   "hu.dwim.perec"
   "hu.dwim.presentation"
   "hu.dwim.rdbms"
   "hu.dwim.web-server"
   "metacopy"
   ;; Does not exist (domain name)
   "org-sampler"
   "s-xml-rpc"
   ;; Requires HTTP password
   "cl-rollback"
   "cl-facts"
   "cl-lessp"
   "cl-libfarmhash"
   "cl-libhoedown"
   "plain-odbc"
   "trivial-string-template"
   ;; Git warning: Could not find remote branch stable to clone.
   "claw"
   ))

(defvar *conn* nil
  "Current database connection")

(defun init-sqlite-connection ()
  "Initializes the SQLite database file handle"
  (let ((pathname (make-pathname
                   :directory '(:relative)
                   :name "quicklisp-projects"
                   :type "sqlite")))
    (setf *conn* (sqlite:connect pathname :busy-timeout 5))
    (sqlite:execute-non-query
     *conn*
     (concatenate 'string
                  "create table if not exists QuicklispProjects"
                  " (name unique not null, kind not null, url unique not null,"
                  " broken default 0)"))
    (sqlite:execute-non-query
     *conn*
     "create table if not exists Downloads
      (name unique not null, path not null)")
    (sqlite:execute-non-query
     *conn*
     "create table if not exists Metadata
      (project not null, name unique not null, meta not null)")))

(defun load-projects ()
  "Run from root of quicklisp-projects to import project data into SQLite"
  (let* ((pathname (make-pathname
                    :directory
                    '(:relative "quicklisp-projects" "projects" :wild)))
         (projects (directory pathname)))
    (dolist (project projects)
      (let* ((pathname (make-pathname
                        :defaults project
                        :name "source"
                        :type "txt"))
             (name (a:lastcar (pathname-directory pathname)))
             (source-line (with-open-file (stream pathname)
                            (read-line stream)))
             (kind (subseq source-line 0 (position #\Space source-line)))
             (url (subseq source-line (1+ (position #\Space source-line)))))
        (sqlite:execute-non-query
         *conn*
         (concatenate 'string
                      "insert or ignore into QuicklispProjects "
                      "(name, kind, url) values (?,?,?)")
         name kind url))))
  (dolist (project-name *broken-projects*)
    (sqlite:execute-non-query
     *conn*
     "update QuicklispProjects set broken = 1 where name = ?"
     project-name)))

(defparameter *download-directory*
  (make-pathname :directory '(:relative "downloads")))

(defvar *download-pathname* nil
  "The target pathname to download the current source into")

(defun remove-suffix (string suffix)
  (subseq string 0 (search suffix string :from-end t)))

(defgeneric download-source (kind url)
  (:documentation "Download the sources of a project to disk"))

(defmethod download-source :before (kind url)
  (format t "Downloading ~a source ~a~%" kind url))

(defmethod download-source ((kind (eql :git)) url)
  (uiop:run-program (format nil "git clone ~a ~a" url *download-pathname*)))

(defmethod download-source ((kind (eql :https)) url)
  "Downloads and unpacks a tarball to *download-pathname*"
  (let* ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (multiple-value-bind (http-stream)
        (http:get url :force-binary t :want-stream t)
      (uiop:with-temporary-file (:stream stream
                                 :pathname pathname
                                 :element-type '(unsigned-byte 8)
                                 :type "tar.gz")
        (loop for read = (read-sequence buffer http-stream)
              while (plusp read)
              do (write-sequence buffer stream :end read))
        (finish-output stream)
        (ensure-directories-exist *download-pathname*)
        (sh (format nil "tar -C ~a -xf ~a" *download-pathname* pathname))))))

(defmethod download-source ((kind (eql :http)) url)
  (download-source :https url))

(defun github-latest-release (owner repo)
  (line-up-last
   (dex:get (format nil
                    "https://api.github.com/repos/~a/~a/releases/latest"
                    owner repo)
            :headers '(("Accept" . "application/vnd.github+json")))
   (json:parse)
   (gethash "tarball_url")))

(defun github-latest-tag (owner repo)
  (flet ((fetch-page (page)
           (line-up-first
            (format
             nil
             "https://api.github.com/repos/~a/~a/tags?per_page=100&page=~a"
             owner repo page)
            (http:get :headers '(("Accept" . "application/vnd.github+json")))
            (json:parse))))
    (let ((all-pages (list)))
      (loop for page-number = 1 then (1+ page-number)
            for page = (fetch-page page-number)
            while (not (a:emptyp page))
            do (a:appendf all-pages page))
      (line-up-last
       (sort all-pages #'string> :key (lambda (tag)
                                        (gethash "name" tag)))
       (a:first-elt)
       (gethash "tarball_url")))))

(defmethod download-source ((kind (eql :latest-github-release)) url)
  (let* ((owner (third (str:split #\/ url :omit-nulls t)))
         (repo (remove-suffix
                (fourth (str:split #\/ url :omit-nulls t))
                ".git"))
         (tarball-url (github-latest-release owner repo)))
    (download-source :https tarball-url)))

(defmethod download-source ((kind (eql :latest-github-tag)) url)
  (let* ((owner (third (str:split #\/ url :omit-nulls t)))
         (repo (line-up-first
                (str:split #\/ url :omit-nulls t)
                (fourth)
                (remove-suffix ".git")))
         (tarball-url (github-latest-tag owner repo)))
    (download-source :https tarball-url)))

(defparameter +gitlab-api-url+ "https://gitlab.com/api/v4")

(defun gitlab-latest-release (owner repo)
  (let* ((id (line-up-last
              (format nil "~a/projects/~a%2F~a" +gitlab-api-url+ owner repo)
              (http:get)
              (json:parse)
              (gethash "id")))
         (latest (line-up-first
                  (format nil "~a/projects/~a/releases" +gitlab-api-url+ id)
                  (http:get)
                  (json:parse)
                  (sort #'string> :key (lambda (release)
                                         (gethash "released_at" release)))
                  (a:first-elt))))
    (line-up-last
     latest
     (gethash "assets")
     (gethash "sources")
     (remove-if-not (lambda (x) (string-equal (gethash "format" x) "tar.gz")))
     (a:first-elt)
     (gethash "url"))))

(defmethod download-source ((kind (eql :latest-gitlab-release)) url)
  (let* ((owner (third (str:split #\/ url :omit-nulls t)))
         (repo (line-up-first
                (fourth (str:split #\/ url :omit-nulls t))
                (remove-suffix ".git")))
         (tarball-url (gitlab-latest-release owner repo)))
    (download-source :https tarball-url)))

(defmethod download-source ((kind (eql :mercurial)) url)
  (uiop:run-program (format nil "hg clone ~a ~a" url *download-pathname*)))

(defmethod download-source ((kind (eql :branched-git)) url)
  (destructuring-bind (url branch)
      (str:split #\Space url)
    (uiop:run-program (format nil
                              "git clone --depth 1 --branch ~A ~A ~A"
                              branch url *download-pathname*))))

(defmethod download-source ((kind (eql :tagged-git)) url)
  (download-source :branched-git url))

(defmethod download-source ((kind (eql :ediware-http)) url)
  (download-source :git (format nil "https://github.com/edicl/~A" url)))

(defmethod download-source ((kind (eql :kmr-git)) url)
  (download-source :git (format nil "http://git.kpe.io/~A.git" url)))

;; FIXME: empty directory after checkout - Try looking at stderr
(defmethod download-source ((kind (eql :svn)) url)
  (sh (format nil "svn checkout ~A ~A" url *download-pathname*)))

(defmethod download-source ((kind (eql :darcs)) url)
  (sh (format nil "darcs get ~A ~A" url *download-pathname*)))

(defmethod download-source ((kind (eql :single-file)) url)
  (format t "IGNORING single-file system~%"))

(defmethod download-source :around (kind url)
  (restart-case
      (call-next-method)
    (skip ()
      :report (lambda (stream) (format stream "Skip downloading ~a" url))
      nil)
    (retry ()
      :report (lambda (stream) (format stream "Retry downloading ~a" url))
      (download-source kind url))))

(defvar *downloads-kernel* (lparallel:make-kernel 5))

(defun download-all-projects ()
  (ensure-directories-exist *download-directory*)
  (let* ((lparallel:*kernel* *downloads-kernel*)
         (channel (lparallel:make-channel))
         (projects (sqlite:execute-to-list
                    *conn*
                    (concatenate 'string
                                 "select * from QuicklispProjects"
                                 " where broken = 0"
                                 ;; Could handle it in the future by creating
                                 ;; a asdf system with one component...
                                 " and kind <> 'single-file'"))))
    (lparallel:task-handler-bind
        ((error (lambda (e)
                  (warn "Skipping because of ~A" e)
                  (a:when-let ((restart (find-restart 'skip)))
                    (invoke-restart restart)))))
      (dolist (project projects)
        (lparallel:submit-task
         channel
         (lambda ()
           (destructuring-bind (name kind url broken)
               project
             (declare (ignorable broken))
             (let ((*download-pathname*
                     (merge-pathnames
                      (make-pathname :directory `(:relative ,name))
                      *download-directory*)))
               (unless (probe-file *download-pathname*)
                 (download-source (a:make-keyword (string-upcase kind)) url)
                 (a:when-let ((truename
                               (uiop:probe-file*
                                *download-pathname*
                                :truename t)))
                   (sqlite:execute-non-query
                    *conn*
                    "insert or ignore into Downloads values (?,?)"
                    name
                    (format nil "~A" truename))))))))))
    (dotimes (_ (length projects))
      (lparallel:receive-result channel))))

(defun discover-systems ()
  (dolist (project (sqlite:execute-to-list
                    *conn* "select * from Downloads"))
    ;; (break)
    (destructuring-bind (name path)
        project
      (uiop:collect-sub*directories
       path
       (constantly t)
       (constantly t)
       (lambda (dir)
         (let* ((files (uiop:directory-files dir))
                (asds (remove-if-not
                       (lambda (file)
                         (string-equal "asd" (pathname-type file)))
                       files)))
           (dolist (asd asds)
             (restart-case
                 (progn
                   (format t "checking asd '~A' in project '~A'~%" asd name)
                   (ignore-errors (asdf:load-asd asd))
                   (let* ((system-name (pathname-name asd))
                          ;; Assumes upstream recommendation of one system per
                          ;; asd is followed
                          (system (asdf:find-system system-name))
                          (meta (make-hash-table :test 'equal)))
                     (setf (gethash "license" meta)
                           (or (asdf:system-license system) 'NULL))
                     (setf (gethash "description" meta)
                           (or (asdf:system-description system) 'NULL))
                     (setf (gethash "depends-on" meta)
                           (coerce (asdf:system-depends-on system) 'vector))
                     (setf (gethash "version" meta)
                           (asdf:system-version system))
                     (sqlite:execute-non-query
                      *conn*
                      (concatenate 'string
                                   "insert into Metadata (project, name, meta)"
                                   " values (?,?,?)")
                      name system-name (json:stringify meta))))
               (skip ()
                 :report "Skip this asd"
                 (format t "WARN: Skipping asd ~A~%" asd)
                 nil)))))))))

(comment

  (init-sqlite-connection)
  (clone-projects-repo)
  (load-projects)
  (download-all-projects)
  
  (handler-bind ((error (lambda (e) (invoke-restart 'skip))))
    (discover-systems))
  
  (sqlite:disconnect *conn*)

  (github-latest-release "slime" "slime")
  (github-latest-tag "slime" "slime")

  )

(defclass quicklisp-projects-repository () ())

;; (defmethod import-lisp-packages ((repository quicklisp-projects-repository)
;;                                  (database sqlite-database))

;;   (let* ((db (sqlite:connect (database-url database))))

;;     (flet ((sql (sql &rest params)
;;              (apply #'sqlite:execute-to-list db sql params)))

;;       ;; Ensure database schema
;;       (init-db db (init-file database))

;;       "insert or ignore into system(name,version,asd) values (?,?,?)"
;;       "insert or ignore into sha256(url,hash) values (?,?)"
;;       "insert or ignore into src values
;;       ((select id from sha256 where url=?),
;;       (select id from system where name=? and version=?))"
;;       "insert or ignore into dep values
;;       ((select id from system where name=? and version=?),
;;       (select id from system where name=? and version=?))"
