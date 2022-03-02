(ns client
  (:require [clojure.data.json :as json]
            [clj-http.client :as client]
            [clojure.set :as set]
            [java-time :as t]
            [clojure.walk :as walk]
            [clojure.string :as str])
  (:import (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)
           (java.net URLEncoder)
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
               ;;(read-string (slurp "/Users/mendel/.crypto/binance-test"))
               :bin-keys {:BINANCE_API_KEY "DH42FxVc3Je3Dr4LTpZJOSZtBSEn7jSNBT5xkIheLOjyoi8fCx71WVOvSSs9jImE"
                          :BINANCE_API_SECRET "Ok4BiGha91D6s4emwrc2xaASQp0Y6i66FloDc98Ml81Tb9nZRvpHX8SWQuIoO3YL"}})


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

(defn hr-time
  [+hrs xs]
  (let [readable (fn [x]
                   (cond-> x
                           (:time x) (update :time (partial str-time +hrs))))]
    (if (sequential? xs)
      (mapv readable xs)
      (readable xs))))

;; --------------------------------------------------------------------------------
;; Utils

(defn deserialize-body [res] (update res :body json/read-str))

(defn kwordize
  [xs]
  (if (sequential? xs)
    (map walk/keywordize-keys xs)
    (walk/keywordize-keys xs)))

(defn stringify-keys
  [xs]
  (if (sequential? xs)
    (map walk/stringify-keys xs)
    (walk/stringify-keys xs)))

(defn hmac-sha-256
  "Calculate HMAC signature for given data.
  https://gist.github.com/jhickner/2382543"
  [^String key ^String data]
  (let [hmac-sha-256 "HmacSHA256"
        hmac-key (SecretKeySpec. (.getBytes key) hmac-sha-256)
        hmac (doto (Mac/getInstance hmac-sha-256) (.init hmac-key))
        signature (.doFinal hmac (.getBytes data))]
    ;; Hex
    (format "%x" (BigInteger. signature))
    ;; UTF-8
    ;;(String. (Base64/encodeBase64 signature) "UTF-8")
    ))

(defn- sign
  [api query]
  (let [secret-key (->> api :bin-keys :BINANCE_API_SECRET)
        string-to-sign (client/generate-query-string query)]
    ;;todo mendel remove
    (println "\nSECRET KEY" secret-key)
    (println "\nSTRING-TO-SIGN\n" string-to-sign)
    (assoc query :signature (hmac-sha-256 secret-key string-to-sign))))

;; --------------------------------------------------------------------------------
;;
(defn make-url
  [api path]
  (str (:api api) version path))

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
  [api url query headers]
  (let [url+query (str url "?" (client/generate-query-string query))
        body {;;:query (json/write-str query)
              ;;:form-params (client/generate-query-string query)
              ;;:form-params query
              ;;:as :x-www-form-urlencoded
              :headers {"X-MBX-APIKEY" (str (->> api :bin-keys :BINANCE_API_KEY))}
              :content-type :application/x-www-form-urlencoded
              :accept :json}]
    ;;todo mendel remove
    (println "\nURL+QUERY" url+query)
    (println "SIGNED QUERY") (clojure.pprint/pprint query)
    (println "BODY") (clojure.pprint/pprint body)

    (println "\nCURL\n" (format "curl -H 'X-MBX-APIKEY: DH42FxVc3Je3Dr4LTpZJOSZtBSEn7jSNBT5xkIheLOjyoi8fCx71WVOvSSs9jImE' -X POST 'https://testnet.binance.vision/api/v3/order/test?%s'" (client/generate-query-string query)))

    (println "\nGET SIGNATURE\n" (format "echo -n '%s' | openssl dgst -sha256 -hmac %s" (client/generate-query-string (dissoc query :signature)) (->> api :bin-keys :BINANCE_API_SECRET)))

    ;;(client/post url+query body)
    ))

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
       query {:symbol "BTCBUSD"
              :side "BUY"
              :type "MARKET"
              ;;:timeInForce "GTC"
              :quantity 0.001
              :recvWindow 20000
              :timestamp (timestamp)}]
   (->> (post-sync api url (sign api query) {})
        ;;nice-query
        ;;:query
        ;;kwordize
        ;;(hr-time 1)
        )))

(comment
 ;; Path Exchange Info - symbols
 (let [api api-test
       url (make-url api path-exchange-info)]
   (println "\nURL" url)
   (->> (deserialize-body (get-sync url))
        :body
        kwordize
        :symbols
        (map :symbol)
        clojure.pprint/pprint)))

(comment
 ;; Trades
 (let [api api-test
       url (make-url api trades)]
   (println "\nURL" url)
   (->> (deserialize-body (get-sync url {"symbol" "BTCBUSD" "limit" "1"}))
        :body
        kwordize
        (hr-time 1))))


;;apiKey	  vmPUZE6mv9SD5VNHk4HlWFsOr6aKE2zvsw0MuIgwCIPy6utIco14y7Ju91duEh8A
;;secretKey	NhqPtmdSJYdKjVHjA7PZj4Mge3R5YNiP1e3UZjInClVN65XAbvqqM6A7H5fATj0j

;;HMAC SHA256 signature
;;echo -n "symbol=LTCBTC&side=BUY&type=LIMIT&timeInForce=GTC&quantity=1&price=0.1&recvWindow=5000&timestamp=1499827319559" | openssl dgst -sha256 -hmac "NhqPtmdSJYdKjVHjA7PZj4Mge3R5YNiP1e3UZjInClVN65XAbvqqM6A7H5fATj0j"
;;c8db56825ae71d6d79447849e617115f4a920fa2acdcab2b053c4b2838bd6b71

;;My data
;;echo -n "symbol=BTCBUSD&side=BUY&type=MARKET&timeInForce=GTC&quantity=0.001&recvWindow=5000&timestamp=1646253400101" | openssl dgst -sha256 -hmac "Ok4BiGha91D6s4emwrc2xaASQp0Y6i66FloDc98Ml81Tb9nZRvpHX8SWQuIoO3YL"
;;04c19b476f154e00fd9d735551e28e2a01ebb560cd2047516c9d325389d189e9

;;curl command
;;curl -H "X-MBX-APIKEY: vmPUZE6mv9SD5VNHk4HlWFsOr6aKE2zvsw0MuIgwCIPy6utIco14y7Ju91duEh8A" -X POST 'https://api.binance.com/api/v3/order' -d 'symbol=LTCBTC&side=BUY&type=LIMIT&timeInForce=GTC&quantity=1&price=0.1&recvWindow=5000&timestamp=1499827319559&signature=c8db56825ae71d6d79447849e617115f4a920fa2acdcab2b053c4b2838bd6b71'

;;My curl command
;;curl -H "X-MBX-APIKEY: DH42FxVc3Je3Dr4LTpZJOSZtBSEn7jSNBT5xkIheLOjyoi8fCx71WVOvSSs9jImE" -X POST 'https://testnet.binance.vision/api/v3/order/test?symbol=BTCBUSD&side=BUY&type=MARKET&timeInForce=GTC&quantity=0.001&recvWindow=50000&timestamp=1646256437694&signature=-40c539779c9de2803199800090d6a8b9bdd8a235b8b95d7accd76705ddcb03b2'


