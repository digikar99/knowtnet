(defparameter +front-page+ "/")
(defparameter +signup-page+ "/signup")

(defparameter +logout-server-page+ "/logout-server")
(defparameter +rating-page-up+ "/api/rating/up/")
(defparameter +open+ "/api/open/")
(defparameter +link-list+ "/api/link-list") ; non-parameterized
(defparameter +known+ "/api/knowledge/known/")

;; (defparameter +rating-page-down+ "/api/rating/down")
(defparameter +add-theme+ "/add_theme")
(defparameter +sharelink-page+ "/sharelink")
(defparameter +sharelink-title-page+ "/sharelink-title")
(defparameter +sharelink-server-page+ "/sharelink-server")
(defparameter +user-page+ "/user")
(defparameter +user-server-page+ "/user-server")
(defparameter +filter-front-page+ "/filter")
(defparameter +known-links-page+ "/known-links")
(defparameter +filter-known-page+ "/filter-known")
(defparameter +logout-all-persistent-server-page+ "/logout-persistent-server")
(defparameter +about-page+ "/about")
(defparameter *ktdb-connection-spec*
  ;; file not in git; alternatively a text-based database may be used
  (with-open-file (f "ktdb-connection-spec.lisp") (read f)))

(defparameter *orm-definition-file* "orm-definitions.lisp")
(defparameter *utilities-file* "utilities.lisp")
(defparameter *data-directory* "data/")
