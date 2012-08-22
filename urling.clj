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
              [:c 2 "c end"]
              [:d 2 "d end"]
              [:e 2 "e end"]]])

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

;NOPE
(defn clea [tst] 
  (loop [[f s t & o] tst,
         ] 
    (println " ---> f:" f "| s:" s "| t:" t )
    (if o 
       (println "  ---> o:" o))
    ;(if (vector? t) 
    (if (vector? t) 
      (recur t)
      ;(recur o)))) 
    ;(if o 
    ;  (recur o)))) 
;NOPE
(defn clea [tri]
  (loop [[t r i] tri]
    (println " ---> t:" t "| r:" r "| i:" i )
    (for [el i] (println "  --> el: " el))
    (recur i)
    ))

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

