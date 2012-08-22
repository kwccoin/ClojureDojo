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

(get (get (get (parse "http://www.example.com") 2) 2) 2)

(def tst [:a 0 
            [:b 1 
              [:c 2 "end"]]])

(defn deep [parsed] 
  (if (string? parsed)
  ;(if (= :a (tag parsed))
    parsed
    (let [v parsed] 
      (deep (get v 2)))
))

(doseq [[t a c] parsed] 
  (if (and t a c)
  (println "tag: " t " attr: " a " childs: " c)))

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


