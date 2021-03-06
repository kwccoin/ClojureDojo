(ns my-app.core
  (:require [clj-http.client :as client])
  (:require [clj-time.core :as ctime]))

(def api-key (apply str (drop-last (slurp "/home/eddie/Downloads/api-key"))))
(def mongolab (str "https://api.mongolab.com/api/1/databases/bookmarks/collections/library?apiKey=" api-key))

(if (= 200 (get (client/post mongolab
  {:body (str 
  " {
  \"chapter\": \"9\",
  \"title\": \"Types, protocols, and records\",
  \"paragraph\": \"9.3.2\",
  \"book-page\": \"195\",
  \"doc-page\": \"222\",
  \"date-added\": \""(ctime/now)\"" 
  } ")
   :headers {"X-Api-Version" "2"}
   :content-type :json
   :socket-timeout 1000
   :conn-timeout 1000
   :accept :json}) :status)) :posted :error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; user=> (client/get "https://api.mongolab.com/api/1/databases/bookmarks/collections/library?apiKey=****" {:accept :json})

;{:trace-redirects ["https://api.mongolab.com/api/1/databases/bookmarks/collections/library?apiKey=****"], :request-time 1455, :status 200, :headers {"date" "Tue, 18 Sep 2012 20:03:12 GMT", "server" "Apache/2.2.22 (Ubuntu)", "access-control-allow-credentials" "true", "access-control-allow-origin" "*", "content-length" "124", "connection" "close", "content-type" "application/json;charset=utf-8"}, :body "[ { \"_id\" : { \"$oid\" : \"****\"} , \"name\" : \"zero\" , \"title\" : \"zeroTitle\" , \"dateAdded\" : \"2012/06/27\"} ]"}
; user=> 

#_(
; /databases
;   GET - lists databases linked to the authenticated
;         account
; /databases/<d>
;   GET - lists sub-services for database <d>
; /databases/<d>/collections
;   GET - lists collections in database <d>
; /databases/<d>/collections/<c>
;   GET - lists objects in collection <c>
;   POST - inserts a new object into collection <c>
; /databases/<d>/collections/<c>/<id>
;   GET - returns object with _id <id>
;   PUT - modifies object (or creates if new)
;   DELETE - deletes object with _id <id>
; /databases/<d>/collections/<c>?[q=<query>][&c=true]
;   [&f=<fields>][&fo=true][&s=<order>]
;   [&sk=<skip>][&l=<limit>]
;   GET - lists all objects matching these
;         optional params:
;     q: JSON queryreference
;     c: returns the result count for this query
;     f: set of fields to be returned in each object
;        (1—include; 0—exclude)
;        e.g. { "name" : 1, "email": 1 } OR 
;        e.g. { "comments" : 0 } 
;    fo: return a single object from the result set
;        (same as 'findOne()' in the mongo shell) 
;     s: sort order (1—asc; -1—desc) 
;        i.e. { <field> : <order> }
;    sk: number of results to skip in the result set
;     l: limit for the number of results
; /databases/<d>/collections/<c>?[q=<query>][&m=true]
;   [&u=true]
;   PUT - updates one or all objects matching the query.
;         payload should contain modifier operations
;     q: JSON queryreference
;     m: apply update to all objects in result set
;        (by default, only one is updated)
;     u: insert if none match the query (upsert)
)
