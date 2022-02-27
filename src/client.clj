(ns client
  (:require [clojure.data.json :as json]
            [clj-http.client :as client]
            [clojure.set :as set]
            [java-time :as t]))

;;https://testnet.binance.vision/
;;https://binance-docs.github.io/apidocs/spot/en/#change-log

;; --------------------------------------------------------------------------------
;;
;; Spot API URL
(def api-endpoints {:api "https://api.binance.com/api"
                    :api1 "https://api1.binance.com/api"
                    :api2 "https://api2.binance.com/api"
                    :api3 "https://api3.binance.com/api"
                    :wss "wss:// stream.binance.com:9443/ws"
                    :wss-stream "wss:// stream.binance.com:9443/stream"})

;; Spot Test Network URL
(def api-endpoints-test {:api "https://testnet.binance.vision/api"
                         :wss "wss://testnet.binance.vision/ws"
                         :wss-stream "wss://testnet.binance.vision/stream"})

(def version "/v3")

;; --------------------------------------------------------------------------------
;; API and Security keys
(def bin-keys (read-string (slurp "/Users/mendel/.crypto/binance")))
(def bin-test-keys (read-string (slurp "/Users/mendel/.crypto/binance-test")))

;; --------------------------------------------------------------------------------
;;
(defn make-url
  ([api-endpoints path]
   (str (:api api-endpoints) version path))
  ([api-endpoints path query]
   (str (:api api-endpoints) version path)))

;;:cached :request-time :repeatable? :protocol-version :streaming? :http-client :chunked? :reason-phrase
;; :headers :orig-content-encoding :status :length :body :trace-redirects)
(defn get-ok
  [{:keys [opts status headers] :as resp}]
  ;;(println "\nRESP" (keys resp)) (clojure.pprint/pprint resp)
  (println "\nstatus" status)
  (println "\nheaders") (clojure.pprint/pprint headers)
  ;;(println "\nopts" opts)
  (println "\nRESPONSE")
  (clojure.pprint/pprint (->> resp :body json/read-str)))

(defn get-error
  [resp]
  (println "\nRESP" (keys resp)) (clojure.pprint/pprint resp)
  ;;(println "\nstatus" status)
  ;;(println "\nheaders" headers)
  ;;(if error (println "Error on" opts "\nERROR\n" error)
  ;;          (println "Success on" opts))
  )

(defn get-async
  [url ok-fn error-fn]
  (client/get url {:async? true} ok-fn error-fn))

(defn get-sync
  [url query]
  (client/get url {:query-params query}))

(defn body [res] (->> res :body json/read-str))

;; --------------------------------------------------------------------------------
;; Time
(defn timestamp [] (System/currentTimeMillis))
(defn utc+ [hs date-time] (t/plus date-time (t/hours hs)))
(defn human-readable-time
  ([timestamp]
   (t/instant timestamp))
  ([+hs timestamp]
   (->> timestamp human-readable-time (utc+ +hs))))

[(timestamp)
 (str (t/offset-date-time))
 (str (t/instant (timestamp)))]
(->> 1645990118821 human-readable-time (utc+ 1))
(->> 1645990118821 (human-readable-time 1))
(t/instant 1645990400838)
(t/offset-date-time)
(t/zoned-date-time)

;; --------------------------------------------------------------------------------
;; Paths
(def path-exchange-info "/exchangeInfo")
(def ping "/ping")
(def the-time "/time")
(def trades "/trades")
(def price "/ticker/price")
(def book-ticker "/ticker/bookTicker")

;; --------------------------------------------------------------------------------
;; Playground
(comment
 (let [api-endpoints api-endpoints-test
       path path-exchange-info
       path trades
       url (make-url api-endpoints path)
       _ (println "\nURL" url)
       ;;trades
       res (get-sync url {"symbol" "BTCUSDT" "limit" "10"})
       ;;res (get-sync url {"symbol" "BTCUSDT"})
       meta (dissoc res :body)
       ;;symbols (->> (get body "symbols")
       ;;             (map #(get % "symbol")))
       ]
   ;;(keys res)
   ;;(assoc res :body (body res))
   (body res)
   ;;symbols
   ))
