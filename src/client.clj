(ns client
  (:require [clojure.data.json :as json]
            [clj-http.client :as client]
            [clojure.set :as set]
            [java-time :as t]
            [clojure.walk :as walk]
            [clojure.string :as str])
  (:import (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)))

;;https://testnet.binance.vision/
;;https://binance-docs.github.io/apidocs/spot/en/#change-log

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
    ;; UTF-8
    ;;(String. (Base64/encodeBase64 signature) "UTF-8")
    ;; Hex
    ;; The 1 is to make sure it's positive, since we can't have negative valued signatures.
    (format "%x" (BigInteger. 1 signature))))

(defn- sign
  [api query]
  (let [secret-key (->> api :bin-keys :BINANCE_API_SECRET)
        string-to-sign (client/generate-query-string query)]
    ;;todo mendel remove
    ;;(println "\nSECRET KEY" secret-key)
    ;;(println "\nSTRING-TO-SIGN\n" string-to-sign)
    (assoc query :signature (hmac-sha-256 secret-key string-to-sign))))

;; --------------------------------------------------------------------------------
;;
;; Spot API URL
(def api {:api "https://api.binance.com"
          :api1 "https://api1.binance.com"
          :api2 "https://api2.binance.com"
          :api3 "https://api3.binance.com"
          :wss "wss:// stream.binance.com:9443/ws"
          :wss-stream "wss:// stream.binance.com:9443/stream"
          :bin-keys (read-string (slurp "/Users/mendel/.crypto/binance"))})

;; Spot Test Network URL
(def api-test {:api "https://testnet.binance.vision"
               :wss "wss://testnet.binance.vision/ws"
               :wss-stream "wss://testnet.binance.vision/stream"
               ;;(read-string (slurp "/Users/mendel/.crypto/binance-test"))
               :bin-keys {:BINANCE_API_KEY "DH42FxVc3Je3Dr4LTpZJOSZtBSEn7jSNBT5xkIheLOjyoi8fCx71WVOvSSs9jImE"
                          :BINANCE_API_SECRET "Ok4BiGha91D6s4emwrc2xaASQp0Y6i66FloDc98Ml81Tb9nZRvpHX8SWQuIoO3YL"}})

(def base-paths {:api "/api"
                 :sapi "/sapi"})
(def http-methods {:get client/get
                   :post client/post})

;; --------------------------------------------------------------------------------
;; API and Security keys
;;(def bin-keys (read-string (slurp "/Users/mendel/.crypto/binance")))
;;(def bin-test-keys (read-string (slurp "/Users/mendel/.crypto/binance-test")))


;; --------------------------------------------------------------------------------
;; Operations

(def path-exchange-info {:path "/exchangeInfo"
                         :method :get
                         :base-path :api
                         :version 3
                         :sign? false})
(def ping "/ping")
(def the-time "/time")
(def trades {:path "/trades"
             :method :get
             :base-path :api
             :version 3
             :sign? false})
(def price "/ticker/price")
(def book-ticker "/ticker/bookTicker")
;; Trade
(def test-trade {:path "/order/test"
                 :method :post
                 :base-path :api
                 :version 3
                 :sign? true})
;; User data
(def account {:path "/account"
              :method :get
              :base-path :api
              :version 3
              :sign? true})

;; --------------------------------------------------------------------------------
;;
(defn make-url
  [api {:keys [base-path path version] :as op}]
  (str (:api api) (base-path base-paths) "/v" version path))

(defn request
  [api {:keys [method sign?] :as op} query headers]
  (let [url (make-url api op)
        query-params (->> (cond-> query
                                  sign? (#(->> (assoc % :timestamp (timestamp))
                                               (sign api)))))
        hdrs (cond->> headers
                      sign? (merge {"X-MBX-APIKEY" (str (->> api :bin-keys :BINANCE_API_KEY))
                                    :content-type :application/x-www-form-urlencoded}))
        data {:query-params query-params
              :headers (merge hdrs
                              {:accept :json})}]

    ;;todo mendel remove
    (println "\nOPERATION" op)
    (println "URL" url)
    (println "DATA") (clojure.pprint/pprint data)

    ;;(println "\nCURL\n" (format "curl -H 'X-MBX-APIKEY: %s' -X %s 'https://testnet.binance.vision/api/v3/order/test?%s'"
    ;;                            (->> api :bin-keys :BINANCE_API_KEY)
    ;;                            (method {:get "GET" :post "POST"})
    ;;                            (client/generate-query-string query-params)))

    ((method http-methods) url data)))

;; --------------------------------------------------------------------------------
;; Playground

(comment

 (request api-test account {:recvWindow 20000} {})
 (->> (request api-test trades {:symbol "BTCBUSD" :limit "1"} {})
      deserialize-body)
 (->> (request api-test path-exchange-info {} {})
      deserialize-body)
 (request api-test test-trade {:symbol "BTCBUSD"
                               :side "BUY"
                               :type "MARKET"
                               :quantity 0.001
                               :recvWindow 20000} {}))



;; --------------------------------------------------------------------------------
;; Examples from
;; https://binance-docs.github.io/apidocs/spot/en/#signed-trade-user_data-and-margin-endpoint-security

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


;; --------------------------------------------------------------------------------
;; Stuff I don't use right now

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

