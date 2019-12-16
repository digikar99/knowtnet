(reader:enable-reader-syntax 'lambda)

(defmacro with-tidy-xml (file-name string)
  (let ((file-name-sym (gensym)))
    `(let ((,file-name-sym ,file-name))
       (with-open-file (f ,file-name-sym
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
         (write-string ,string f))
       (uiop:run-program (concat "tidy -m -xml -i " ,file-name-sym)
                         :ignore-error-status t))))

(defun generate-theme-selection-box (&optional selected-theme)
  (cl-markup:markup*
   (append `(:select :name "theme" :required "required"
                     (:option :value ""
                              "Select Theme"))
           (loop for theme in (mapcar #'theme-name themes)
              collect (if (string= selected-theme theme)
                          `(:option :selected t :value ,theme ,theme)
                          `(:option :value ,theme ,theme))))))

(defun generate-browse-link-box (link theme)
     (declare (type link link))
     (trivia:let-match* 
         (((link url newbie-p id title description) link)
          (str-id ($ id))
          (cl-markup:*auto-escape* nil))
       (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (assert (not cl-markup:*auto-escape*))
       (cl-markup:markup*
        `(:div :class "link-box"
               (:div :class "link-url"
                     (:a :href ,url
                         :target "_blank"
                         ,title))
               (:div :class "link-theme" ,theme)
               (:div :class "link-level" (if ,newbie-p "Newbie" ""))
               (:br)
               (:div :class "link-description"
                     :onclick ,(concat "pingAndOpen('" url "')")
                     :target "_blank"
                     (:div :class "read-more"
                           (:span :class "read-more-text" "READ MORE"))
                     (:div :class "description-contents"
                           ,description))
               (:div :class "link-actions"
                     (:div :class "link-action"
                           :style "background-color:#e6ffcc;"
                           :onclick "javascript:getLinkBoxWithLinkAction(this).remove()"
                         "IGNORE"
                         (:br)
                         "for now")
                     (:div :class "link-action"
                           :style "background-color:#e6ffcc;"
                           :onclick ,(concat "javascript:markAsKnownAndRemove(this, "
                                             str-id ")")
                         "MARK"
                         (:br)
                         "as known")
                     (:div :class "link-share"
                           (:i :class "material-icons-outlined link-share-icon"
                               :onclick "javascript:shareLink(this)"
                               "share"
                               (:span :class "link-share-tooltip" "COPY LINK"))))))))

(defun generate-loader ()
  (cl-markup:markup (:div :id "loader-background" (:div :id "loader" ()))))

(defun generate-responsive-info-panel-js ()
  (declare (sb-ext:muffle-conditions cl:warning))
  (ps:ps
    (defun get-info-panel-menu ()
      (document.get-element-by-id "info-panel-menu"))
    (defun hide-info-panel ()
      (setf (chain (get-info-panel-menu) style.width) "0px"))
    (defun show-info-panel ()
      (setf (chain (get-info-panel-menu) style.width) "80vw"))
    (defun info-panel-visible-p ()
      (not (string= (chain (get-info-panel-menu) style.width) "0px")))
    
    (chain ($ "#info-panel-menu-btn")
           (click
            λ(if (info-panel-visible-p)
                 (hide-info-panel)
                 (show-info-panel))))))

(defun generate-browse-link-box-js (&key theme-name user)
  (declare (sb-ext:muffle-conditions cl:warning))
  (parenscript:ps*

   `(defvar *known-links-var-name* "known")
   `(if (aref local-storage *known-links-var-name*)
        (defvar *known-links*
          (|JSON.parse| (+ "[" (aref local-storage *known-links-var-name*) "]")))
        (defvar *known-links*
          (loop for i from 0 to ,(length links)
             collect 0)))

   `(defvar *fetch-known* nil)

   `(defvar *theme-link-id-list-hash-table*
      (ps:create ,@(iter (for (theme link-id-list) in-hashtable
                              *theme-link-id-list-hash-table*)
                         (appending `(,theme ',link-id-list)))))
   
   `(defun mark-as-known-and-remove (trigger-elt id)
      (setf (aref *known-links* id) 1)
      (setf (aref local-storage *known-links-var-name*) *known-links*)
      (chain (get-link-box-with-link-action trigger-elt) (remove)))

   `(defun known-p (id) (aref *known-links* id))

   `(defun clear-known-links ()
      (setf *known-links* (loop for i from 0 to ,(length links)
                             collect 0))
      (setf (aref local-storage *known-links-var-name*) *known-links*)
      (window.location.reload))

   ;; set *fetchable-links* on every selected-theme update
   `(defvar *fetchable-links* (loop for i from 0 to ,(length links)
                                 if (not (known-p i))
                                 collect i))
   
   `(defvar *waiting-for-ajax* nil)

   `(chain ($ document)
           (on "scroll"
               (lambda (event) 
                 (when (and (>= (+ (@ window inner-height)
                                   (@ window scroll-y))
                                (@ document body offset-height))
                            (not *waiting-for-ajax*))
                   ((@ console log) "bottom")
                   (get-link-from-server)))))

   `(defun random-int (max-int)
      (|Math.floor| (* (|Math.random|) max-int)))

   `(defvar *selected-theme* "")
   
   `(defun get-link-from-server ()
      (when (and (< 1 (length *fetchable-links*))
                 (not *waiting-for-ajax*))
        (setq *waiting-for-ajax* t)    
        (let* ((idx (1+ (random-int (length *fetchable-links*))))
               (link-id (aref *fetchable-links* idx)))
          ($.get
           (ps:create url (+ "data/" (|String| link-id))
                      async nil
                      success (lambda (response-text)
                                (console.log ($ response-text))
                                (console.log link-id "fetched")
                                (chain *fetchable-links* (splice idx 1))
                                (chain ((@ document get-element-by-id)
                                        "responsive-browse-link-boxes")
                                       (insert-adjacent-h-t-m-l "beforeend"
                                                                response-text)))))
          (setq *waiting-for-ajax* nil))))

   `(defun try-making-scrollable ()
      (loop for i from 0 to 3
         do (get-link-from-server)))

   `(chain ($ ".link-rating")
           (find "a")
           (click ,(if user
                       `λ(update-link-rating ($ this))
                       'prompt-for-login)))

   `(defun get-link-box-with-link-action (elt)
      (chain ($ elt) (parent) (parent)))
   `(defun get-theme-of-link-box (link-box)
      (chain link-box (children) 1 inner-text))
   `(defun get-link-box-with-upvote-btn (elt)
      (chain ($ elt) (parent) (parent)))
   `(defun get-link-box-with-share-btn (elt)
      (chain ($ elt) (parent) (parent) (parent)))
   `(defun get-url-of-link-box (link-box) ; jquery element
      (chain link-box
             (get 0)
             first-element-child
             first-element-child
             href))
   `(defun get-title-of-link-box (link-box) ; jquery element
      (chain link-box
             (get 0)
             first-element-child
             first-element-child
             inner-html))

   `((@ ($ ".link-action") click)
     ;; mark-as-known is handled by pinging the appropriate link
     ;; - perhaps a bad design choice
     (lambda ()
       (console.log ($ this))
       (chain (get-link-box-with-link-action this) (remove))))

   `(defun update-selected-theme ()
      ;; update selected-theme
      (setf *selected-theme* (chain ($ "#filter-form option:selected") (html)))

      ;; update fetchable-links
      (let ((link-list (aref *theme-link-id-list-hash-table*
                             *selected-theme*)))
        (if (null link-list)
            (setf *fetchable-links* (loop for i from 1 to ,(length links)
                                       collect i))
            (setf *fetchable-links* (|JSON.parse| (|JSON.stringify| link-list)))))
      (setf *fetchable-links* (loop for id in *fetchable-links*
                                 for known = (known-p id)
                                 if (or (and *fetch-known* known)
                                        (and (not *fetch-known*)
                                             (not known)))
                                 collect id))
      (chain *fetchable-links* (unshift 0))

      ;; delete all children
      (chain ($ "#responsive-browse-link-boxes") (html ""))

      ;; get some children to try making it scrollable
      (try-making-scrollable))

   `(defun prompt-for-login ()
      ((@ ($ "#login-prompt") show)))

   `((@ ($ "#close-login-prompt") click)
     λ((@ ($ "#login-prompt") hide)))

   `(defun hide-loader ()
      (setf (chain (document.get-element-by-id "loader-background")
                   style.display)
            "none"))
   `(defun show-loader ()
      (setf (chain (document.get-element-by-id "loader-background")
                   style.display)
            "block"))
   
   `(defun share-link (e)
     (let* ((link-box (get-link-box-with-share-btn e))
            (link-url (get-url-of-link-box link-box)))
         (console.log link-box)
         (console.log link-url)
         ;; only works over https!
         (if navigator.share
             (progn (show-loader)
               (set-timeout hide-loader 1000)
               (chain navigator
                      (share (ps:create title document.title
                                        text (get-title-of-link-box link-box)
                                        url link-url))
                      (then hide-loader)
                      (catch (lambda (e)
                               (console.log e)
                               (hide-loader)))))
             (chain navigator
                    clipboard
                    (write-text link-url)
                    (then λ(console.log "copied")
                          (lambda (err) (console.error err)))))))

   `((@ console log)
     ((@ ($ "#browse-link-boxes") height))
     ((@ ($ window) height)))

   `(chain ($ "img") (width "90%"))

   `(defun ping-and-open (url)
      ;; ($.ajax (+ ,+open+ id))
      (console.log url " opened")
      (window.open url))
   
   `(defun sleep (ms)
      (ps:new |Promise|))

   `(defun toggle-known ()
      (setf *fetch-known* (not *fetch-known*))
      (update-selected-theme)
      (hide-info-panel)
      (chain ($ "#toggle-known-btn") (html (if *fetch-known*
                                               "View Unknown Links"
                                               "View Known Links"))))

   `(try-making-scrollable)
   ;; `(chain ($ document)
   ;;         (ready
   ;;          λ(loop for i from 0 to 5
   ;;              do (if (< ((@ ($ "#responsive-browse-link-boxes") height))
   ;;                        ((@ ($ window) height)))
   ;;                     (progn (get-link-from-server)
   ;;                            (console.log "called"))
   ;;                     (console.log "dont-call")))))
   ))

(defun generate-filter-form (&key front-page-form selected-theme known-filter-page-form)
  (let ((cl-markup:*auto-escape* nil))
    (cl-markup:markup*
     (if front-page-form
         `(:form :id "front-page-form" :action +front-page+ :method "GET"
                 (:input :type "submit" :id "front-page-btn"
                         :value "Return to Front Page"))
         `(:span))
     `(:div :id "filter-form" :method "GET"
            ,(generate-theme-selection-box selected-theme)
            (:input :type "submit" :id "filter-btn"
                    :class "material-icons"
                    :onclick "updateSelectedTheme()"
                    :value "search")))))

(defun generate-responsive-info-panel (&key about-page)
  ;; HTML generated from other functions needs to be included
  ;; Here, username is one place where injection can take place.
  ;; Therefore, have it escaped when it was inserted into the db!
  (let ((cl-markup:*auto-escape* nil))
    (if about-page
        (cl-markup:markup*
         `(:div :id "responsive-info-panel" :class "text-center"
                (:div :id "site-title"
                      (:span :id "site-title-text" "KnowTNet"))
                (:p :class "text-center"
                    (:a :href ,+front-page+ "Go back"))))
        (cl-markup:markup*
         `(:div :id "responsive-info-panel"
                (:div :id "site-title"
                      (:span :id "site-title-text" :class "text-left" "KnowTNet")
                      (:span :id "site-title-spacer" :class "visible-small" ())
                      (:i :id "info-panel-menu-btn"
                          :class "material-icons text-right visible-small" "menu"))
                ,(generate-filter-form)
                (:br)
                (:br)
                (:br)
                (:br)
                (:div :id "username" :class "text-center" "Welcome to KnowTNet!")
                (:div :id "info-panel-menu" ()
                      (:a :href ,+front-page+ "Home")
                      (:a :id "toggle-known-btn" :onclick "toggleKnown()" "View Known Links")
                      (:a :href ,+about-page+ "About Us")
                      (:div :id "info-panel-spacer" ())
                      (:a :onclick "clearKnownLinks()" "Clear Known Links")
                      (:p :id "ktn" :class "text-center"
                          "KNOWLEDGE TRANSFER NETWORK"))
                ,(generate-loader)
                (:br :class "visible-large")
                (:br :class "visible-large")
                (:br :class "visible-large")
                (:script :type "text/javascript" ,(generate-responsive-info-panel-js)))))))


