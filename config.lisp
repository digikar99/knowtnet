(defparameter +front-page+ "index.html")

;; (defparameter +rating-page-down+ "/api/rating/down")
(defparameter +about-page+ "about.html")
(defparameter *ktdb-connection-spec*
  ;; file not in git; alternatively a text-based database may be used
  (with-open-file (f "ktdb-connection-spec.lisp") (read f)))

(defparameter *orm-definition-file* "orm-definitions.lisp")
(defparameter *base-file* "base.lisp")
(defparameter *data-directory* "data/")

(defparameter *about-text*
  (cl-markup:markup
   (:p "There is a lot of content on the internet; however, human life is short. Just 80 years. With one-third of our life lost in sleep, that leaves us with about 54 years. Subtract another 12 for our kids. 42. (The answer to everything? :P)")
   (:p "People spend as much as "
       (:a :href "https://www.broadbandsearch.net/blog/average-daily-time-on-social-media"
           "2 hours")
       " each day on social media - about 60000 hours in our lifetime!")
   (:p "We won't go into the "
       (:a :href "https://www.businessinsider.in/strategy/new-study-destroys-malcolm-gladwells-famous-10000-hour-rule/articleshow/37721084.cms"
           "10000 hour-rule")
       ", but would point out that time is precious, and putting in time in the things that matter most to you makes a difference.")
   (:p "In our quest, we propose that there are only a few limited things on the internet that make a difference to the way you live your life, to the way you approach things. Some of these things have an economic benefit, while some social.")
   (:p "Newbies in various fields can be lost while looking for help. However, if they know a few core things, may be, they won't be lost, and would then look into the right places, perhaps, after doing some homework.")
   (:p "It is with this vision this website is in existence - to create a repository of \"must-know\" knowledge of the world. This excludes things that continually renew themselves. A very broad example would be News - not everyone needs to know what happened when of all times. There are much better sites - news sites, Reddit, Quora and much other platforms - much better than us. What would be of relevance for us, instead, would be the places to discuss news itself. And so, the links to these websites, the links to the specific Spaces, Subreddits and Forums where people discuss the news, where you should go if you want to understand or formulate viewpoints of any current happenings.")
   (:p "Things of relavance, then, include the \"essences\" of various things: people, sciences, mathematics, laws and ethics, money and economics, career advice. You choose the more narrower topics like your country or occupation or the places relevant to you.")
   (:p "Our aim is to not have a hundred opinions stating the same things again and again. Our aim is to avoid repetivity that plagues most sites - you'd know it if you have been on the internet for more than a while.")
   (:p "Thank you for visiting us! You can drop us a feedback at "
       (:a :href "https://docs.google.com/forms/d/e/1FAIpQLScP88qGXeg1X0GSYLYjMbvqex8FpWvQ4twZLKnEFbDYWUG8wg/viewform?usp=sf_link"
           :target "_blank"
           "this place")
       ".")
   (:br)
   (:br)))
