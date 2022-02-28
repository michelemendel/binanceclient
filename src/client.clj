(ns client
  (:require [clojure.data.json :as json]
            [clj-http.client :as client]
            [clojure.set :as set]
            [java-time :as t]
            [clojure.walk :as walk]
            [clojure.string :as str])
  (:import (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)
           (org.apache.commons.codec.binary Base64)))

;;https://testnet.binance.vision/
;;https://binance-docs.github.io/apidocs/spot/en/#change-log

;; --------------------------------------------------------------------------------
;;
;; Spot API URL
(def api {:api "https://api.binance.com/api"
          :api1 "https://api1.binance.com/api"
          :api2 "https://api2.binance.com/api"
          :api3 "https://api3.binance.com/api"
          :wss "wss:// stream.binance.com:9443/ws"
          :wss-stream "wss:// stream.binance.com:9443/stream"
          :bin-keys (read-string (slurp "/Users/mendel/.crypto/binance"))})

;; Spot Test Network URL
(def api-test {:api "https://testnet.binance.vision/api"
               :wss "wss://testnet.binance.vision/ws"
               :wss-stream "wss://testnet.binance.vision/stream"
               :bin-keys (read-string (slurp "/Users/mendel/.crypto/binance-test"))})

(def version "/v3")

;; --------------------------------------------------------------------------------
;; API and Security keys
;;(def bin-keys (read-string (slurp "/Users/mendel/.crypto/binance")))
;;(def bin-test-keys (read-string (slurp "/Users/mendel/.crypto/binance-test")))

;; --------------------------------------------------------------------------------
;; Time
(defn timestamp [] (System/currentTimeMillis))

(defn utc+ [hrs date-time] (t/plus date-time (t/hours hrs)))

(defn inst-time
  ([timestamp]
   (inst-time 0 timestamp))
  ([+hrs timestamp]
   (->> timestamp t/instant (utc+ +hrs))))

(defn str-time
  ([timestamp]
   (str-time 0 timestamp))
  ([+hrs timestamp]
   (str (inst-time +hrs timestamp))))

(let [ts (timestamp)]
  [;;ts
   ;;(str (t/offset-date-time))
   ;;(str (t/instant (timestamp)))
   (str (->> ts inst-time (utc+ 1)))
   (str-time ts)
   (str-time 1 ts)
   ;;(t/offset-date-time)
   ;;(t/zoned-date-time)
   ])


;; --------------------------------------------------------------------------------
;;
(defn make-url
  ([api path]
   (str (:api api) version path))
  ([api path query]
   (str (:api api) version path)))

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
  ([url]
   (client/get url))
  ([url query]
   (client/get url {:query-params query}))
  ([url query headers]
   (client/get url {:query-params query
                    :headers headers})))

(defn post-sync
  [url body headers]
  (let [full-body {:body (json/write-str body)
                   :headers headers
                   :content-type :application/x-www-form-urlencoded
                   :accept :json}]
    ;;todo mendel remove
    (println "\n")
    (clojure.pprint/pprint full-body)
    (client/post url full-body)))

(defn nice-body [res] (update res :body json/read-str))

(defn kwordize
  [xs]
  (if (sequential? xs)
    (map walk/keywordize-keys xs)
    (walk/keywordize-keys xs)))

(defn hr-time
  [+hrs xs]
  (let [readable (fn [x]
                   (cond-> x
                           (:time x) (update :time (partial str-time +hrs))))]
    (if (sequential? xs)
      (mapv readable xs)
      (readable xs))))

(defn- hmac
  "Calculate HMAC signature for given data.
  https://gist.github.com/jhickner/2382543"
  [^String key ^String data]
  (let [hmac-sha-256 "HmacSHA256"
        signing-key (SecretKeySpec. (.getBytes key) hmac-sha-256)
        mac (doto (Mac/getInstance hmac-sha-256) (.init signing-key))]
    (String. (Base64/encodeBase64
              (.doFinal mac (.getBytes data)))
             "UTF-8")))

(defn- sign
  [api body]
  (let [secret-key (->> api :bin-keys :BINANCE_API_SECRET)
        string-to-sign (->> body
                            (map (fn [[k v]] (str k "=" v)))
                            (str/join "&"))]
    (assoc body "signature" (hmac secret-key string-to-sign))))

;; --------------------------------------------------------------------------------
;; Paths

(def path-exchange-info "/exchangeInfo")
(def ping "/ping")
(def the-time "/time")
(def trades "/trades")
(def price "/ticker/price")
(def book-ticker "/ticker/bookTicker")
;; Trade
(def test-trade "/order/test")

;; --------------------------------------------------------------------------------
;; Playground

(comment
 ;; Test trade
 (let [api api-test
       url (make-url api test-trade)
       body {"symbol" "BTCBUSD"
             "side" "BUY"
             "type" "MARKET"
             "timeInForce" "GTC"
             "quantity" 0.001
             "recvWindow" 5000
             "timestamp" (timestamp)}
       headers {"X-MBX-APIKEY" (->> api :bin-keys :BINANCE_API_KEY)}]
   (println "\nURL" url)
   (->> (post-sync url (sign api body) headers)
        ;;nice-body
        ;;:body
        ;;kwordize
        ;;(hr-time 1)
        )))

(comment
 ;; Path Exchange Info - symbols
 (let [api-endpoints api-test
       url (make-url api-endpoints path-exchange-info)]
   (println "\nURL" url)
   (->> (nice-body (get-sync url))
        :body
        kwordize
        :symbols
        (map :symbol)
        clojure.pprint/pprint)))

(comment
 ;; Trades
 (let [api-endpoints api-test
       url (make-url api-endpoints trades)]
   (println "\nURL" url)
   (->> (nice-body (get-sync url {"symbol" "BTCBUSD" "limit" "1"}))
        :body
        kwordize
        (hr-time 1))))
