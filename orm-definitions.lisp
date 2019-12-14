(clsql:file-enable-sql-reader-syntax)
(clsql:def-view-class link ()
  ((id :type integer :accessor link-id
       :db-kind :key     ; primary-key doesn't work for some reason
       :db-constraints :auto-increment)
   (url :type (string 256)
        :accessor link-url
        :initarg :url
        :db-constraints :not-null)
   (title :type (string 256)
          :accessor link-title
          :initarg :title
          :db-constraints :not-null)
   (description :type (string 512)
                :accessor link-description
                :initarg :description
                :db-constraints :not-null)
   (newbie-p :type boolean
             :accessor link-newbie-p
             :initarg :newbie-p
             :db-constraints :not-null)))

(clsql:def-view-class link-themes ()
  ((link-id :type integer
            :initarg :link-id
            :db-kind :key)
   (theme-id :type integer
             :initarg :theme-id
             :db-kind :key)))

(clsql:def-view-class theme ()
  ((name :type (string 256)
         :accessor theme-name
         :initarg :name
         :db-constraints :not-null)
   (id :type integer :accessor theme-id
       :db-kind :key
       :db-constraints :auto-increment)))

(declaim (ftype (function (link) list) link-themes))
(defun link-themes (link)
  (let ((link-id (link-id link)))
    (clsql:select [theme name] :from [link-themes]
                  :flatp t
                  :inner-join [theme]
                  :on [and [= [link-themes link-id] link-id]
                           [= [theme id] [link-themes theme-id]]])))
