(ns client
  (:require [clojure.data.json :as json]
            [clj-http.client :as client]
            [clojure.set :as set]
            [java-time :as t]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import (javax.crypto Mac)
           (javax.crypto.spec SecretKeySpec)
           (java.io FileReader)))

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

;;Thanks to https://stackoverflow.com/questions/29585928/how-to-substitute-path-to-home-for
(defn bash [command] (sh "bash" "-c" command))
(defn expand [path] (:out (bash (str "echo -n " path))))
(defn bin-bash [curl] (->> (bash curl) :out json/read-str))

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
  [config query]
  (let [secret-key (->> config :bin-keys :BINANCE_API_SECRET)
        string-to-sign (client/generate-query-string query)]
    ;;todo mendel remove
    ;;(println "\nSECRET KEY" secret-key)
    ;;(println "\nSTRING-TO-SIGN\n" string-to-sign)
    (assoc query :signature (hmac-sha-256 secret-key string-to-sign))))

;; --------------------------------------------------------------------------------
;; API and Security keys
;;(read-string (slurp (expand "~/.crypto/binance")))
;;(read-string (slurp (expand "~/.crypto/binance-test")))

;; --------------------------------------------------------------------------------
;;
;; Spot API URL
(def config {:api "https://api.binance.com"
             :api1 "https://api1.binance.com"
             :api2 "https://api2.binance.com"
             :api3 "https://api3.binance.com"
             :wss "wss:// stream.binance.com:9443/ws"
             :wss-stream "wss:// stream.binance.com:9443/stream"
             :bin-keys (read-string (slurp (expand "~/.crypto/binance")))})

;; Spot Test Network URL
(def config-test {:api "https://testnet.binance.vision"
                  :wss "wss://testnet.binance.vision/ws"
                  :wss-stream "wss://testnet.binance.vision/stream"
                  :bin-keys (read-string (slurp (expand "~/.crypto/binance-test")))})

(def base-paths {:api "/api"
                 :sapi "/sapi"})

(def http-methods {:get client/get
                   :post client/post})


;; --------------------------------------------------------------------------------
;; Operations

;; Market Data
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

(def account {:path "/account"
              :method :get
              :base-path :api
              :version 3
              :sign? true})

;; Margin
;;/sapi/v1/margin/priceIndex
(def margin-priceIndex {:path "/margin/priceIndex"
                        :method :get
                        :base-path :sapi
                        :version 1
                        :sign? false})

;; Wallet
;; Sub-Account
;; Stream


;; --------------------------------------------------------------------------------
;;
(defn make-url
  [config {:keys [base-path path version] :as op}]
  (str (:api config) (base-path base-paths) "/v" version path))

(defn- make-query-params
  [config query sign?]
  (cond-> query
          sign? (-> identity
                    (assoc :timestamp (timestamp))
                    (as-> q (sign config q)))))

(defn- make-headers
  [config headers sign?]
  (cond->> headers
           sign? (merge {"X-MBX-APIKEY" (str (->> config :bin-keys :BINANCE_API_KEY))
                         :content-type :application/x-www-form-urlencoded})))

(defn make-curl
  [config {:keys [method sign?] :as op} query]
  (format "curl%s%s -X %s '%s?%s'"
          (if sign? (format " -H 'X-MBX-APIKEY: %s'" (->> config :bin-keys :BINANCE_API_KEY))
                    "")
          " -H 'accept: application/json'"
          (method {:get "GET" :post "POST"})
          (make-url config op)
          (client/generate-query-string (make-query-params config query sign?))))

(defn request
  [config {:keys [method sign?] :as op} params headers]
  (let [url (make-url config op)
        data {:query-params (make-query-params config params sign?)
              :headers (make-headers config (merge headers {:accept :json}) sign?)}]

    ;;todo mendel remove
    (println "\nOPERATION" op)
    (println "URL" url)
    (println "DATA") (clojure.pprint/pprint data)

    ((method http-methods) url data)))

;; --------------------------------------------------------------------------------
;; Playground

(comment

 (deserialize-body (request config-test account {:recvWindow 20000} {}))
 (bin-bash (make-curl config-test account {:recvWindow 20000}))

 (->> (request config-test trades {:symbol "BTCBUSD" :limit "1"} {})
      deserialize-body)
 (bin-bash (make-curl config-test trades {:symbol "BTCBUSD" :limit "1"}))

 (->> (request config-test path-exchange-info {} {})
      deserialize-body)
 (bin-bash (make-curl config-test path-exchange-info {}))

 (request config-test test-trade {:symbol "BTCBUSD"
                                  :side "BUY"
                                  :type "MARKET"
                                  ;;:timeInForce "GTC"
                                  :quantity 0.001
                                  :recvWindow 2000} {})
 (bash (make-curl config-test test-trade {:symbol "BTCBUSD"
                                         :side "BUY"
                                         :type "MARKET"
                                         ;;:timeInForce "GTC"
                                         :quantity 0.001
                                         :recvWindow 2000}))

 (request config-test margin-priceIndex {:symbol "BTCBUSD"} {})
 (make-curl config-test margin-priceIndex {:symbol "BTCBUSD"})
 (bash (make-curl config-test margin-priceIndex {:symbol "BTCBUSD"}))
 )



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

