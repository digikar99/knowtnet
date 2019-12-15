(ql:quickload '(:clsql
                :alexandria
                :iterate
                :trivia
                :cl-markup
                :reader
                :parenscript) :silent t)
(use-package :iterate)
(import '(ps:chain ps:@))


(defun $ (object) (write-to-string object))
(defun concat (&rest strings) (apply #'concatenate 'string strings))
(setq *compile-verbose* nil
      *compile-print* nil)

(load "config.lisp")
(push (car (uiop:directory* "."))
      clsql-sys:*foreign-library-search-paths*)

(clsql:connect *ktdb-connection-spec* :database-type :mysql :if-exists :old)
(load *orm-definition-file*)
(defvar links (clsql:select 'link :flatp t))
(defvar themes (clsql:select 'theme :flatp t))
(load *base-file*)

(format t "~%Preparations Done. Running run-me.lisp~%")
(format t "Total links: ~D~%" (length links))

;; generate link-files
(iter (for link in links)
      (let* ((theme (car (link-themes link)))
             (link-file (concat *data-directory* ($ (link-id link)))))
        (with-tidy-xml link-file (generate-browse-link-box link theme))))

;; generate index.html
(with-tidy-xml "index.html"
  (let ((cl-markup:*auto-escape* nil))
    (cl-markup:html5
     (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:link :href "https://fonts.googleapis.com/icon?family=Material+Icons+Outlined"
             :rel "stylesheet")
      (:link :href "https://fonts.googleapis.com/icon?family=Material+Icons"
             :rel "stylesheet")
      (:link :href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
             :rel "stylesheet")
      (:link :href "css/miscellaneous.css" :rel "stylesheet")
      ;; (:link :rel "stylesheet" :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/css/bootstrap.min.css")
      ;; (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js" ())
      ;; (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/js/bootstrap.min.js" ())
      (:script :async t :src "https://www.googletagmanager.com/gtag/js?id=UA-152031047-1" ())
      (:script "
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-152031047-1');")
      (:title "Knowledge Transfer Network")
      (:script :src "css/jquery-3.4.1.min.js" ())
     (:title "Knowledge Transfer Network - Serverless")
     (:link :rel "stylesheet" :href "css/front.css" :type "text/css")
     (:link :rel "stylesheet" :href "css/link-box.css" :type "text/css")
     (generate-responsive-info-panel)
     (:div :id "responsive-browse-link-boxes" ())
     (generate-loader)
     (:script :type "text/javascript"
              (generate-browse-link-box-js)))))



