
;just a tips on trees with sharing structure:
(defn xconj [t v]
  (cond
    (nil? t)       {:val v, :L nil, :R nil}
    (< v (:val t)) {:val (:val t),
                    :L   (xconj (:L t) v),
                    :R   (:R t)}
    :else          {:val (:val t),
                    :L   (:L t),
                    :R   (xconj (:R t) v)}
    ))

(defn fetch-url[address]
  (with-open [stream (.openStream (java.net.URL. address))]
    (let  [buf (java.io.BufferedReader. 
                (java.io.InputStreamReader. stream))]
      (apply str (line-seq buf)))))

(defn fetch-url-lines [address]
  (with-open [stream (.openStream (java.net.URL. address))]
    (let  [buf (java.io.BufferedReader. 
                (java.io.InputStreamReader. stream))]
      (line-seq buf))))


(fetch-url "http://pastebin.com/archive")

(defn filtr [string] 
  (filter #(re-find #"<a href=(.*?)>" %) string)
)

(defn filtr [string] 
  (clojure.string/split string #"<a href=(.*?)>")
)

(doseq [ line (filtr (fetch-url "http://pastebin.com/archive"))] (println line))

(defn filtr2 [string]
  (clojure.strin/replace string "\".*" "")
)

;;;;;;;;;;;;

(use '(clojure.contrib duck-streams
                       java-utils
                       str-utils))

(import '(java.net URL
                   URLConnection
                   HttpURLConnection
                   UnknownHostException))

(defn check-url [url]
  (str (re-sub #"^(?i)http:/+" "" url)
       ":"
       (try
        (let [c (cast HttpURLConnection
                      (.openConnection (URL. url)))]
          (if (= 200 (.getResponseCode c))
            1
            0))
        (catch UnknownHostException _
          0))))

(defn check-urls-from-file [filename]
  (doseq [line (map check-url
                    (read-lines (as-file filename)))]
    (println line)))

;;;;;

(println (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))
(use 'pl.danieljanus.tagsoup)
(parse "http://example.com")

;tag
;attributes
;children

;;;;;;;;;;;
-------------------------
clojure.core/vec
([coll])
  Creates a new vector containing the contents of coll.
nil
user=> (doc vector)
-------------------------
clojure.core/vector
([] [a] [a b] [a b c] [a b c d] [a b c d & args])
  Creates a new vector containing the args.
-------------------------
;;;;;;;;;;;

(ns in.grok.history.html-parser
  (:require [clojure.contrib.logging :as log])
  (:import [org.htmlcleaner HtmlCleaner]
           [org.apache.commons.lang StringEscapeUtils]))

(defn parse-page
  "Given the HTML source of a web page, parses it and returns the :title
and the tag-stripped :content of the page. Does not do any encoding
detection, it is expected that this has already been done."
  [page-src]
  (try
   (when page-src
     (let [cleaner (new HtmlCleaner)]
       (doto (.getProperties cleaner) ;; set HtmlCleaner properties
         (.setOmitComments true)
         (.setPruneTags "script,style"))
       (when-let [node (.clean cleaner page-src)]
         {:title (when-let [title (.findElementByName node "title", true)]
                     (-> title
                         (.getText)
                         (str)
                         (StringEscapeUtils/unescapeHtml)))
          :content (-> node
                       (.getText)
                       (str)
                       (StringEscapeUtils/unescapeHtml))})))
   (catch Exception e
     (log/error "Error when parsing" e))))

;;;;;;;;;;;;;;;;;;;;;;;

; Let's try to parse a webpage using destructuring in a smart way.

; the library
(use 'pl.danieljanus.tagsoup)

; the page
(def foo (parse "http://www.example.com"))
(def clojure-com (parse "http://clojure.com"))

; the destructure functions
(defn tri-vec [parsed-page] (let [[tag property-map & more :as all] parsed-page] [tag property-map (vec more) all]))
(defn tri-map [parsed-props] (let [[tag property-map & more :as all] parsed-props] {:tag tag :property-map property-map :more (vec more) :all all}))

; the parser function
(defn parz [src] 
  (let [ttt (tri-vec src)]
    (let [tag (ttt 0) property-map (ttt 1) more (ttt 2)] 
      (if more
        (if (keyword? tag )
          (if (= tag (keyword "a"))
            { :more  more :href (property-map :href)}
            (map parz more)
          )   
          ;(apply str src)
        )   
      )   
    )   
  )
)
(remove nil? (flatten (parz foo)))
(remove nil? (flatten (parz clojure-com)))

; (map #(str "http://"%1 (%2 :href)) "www.example.com" rez)

