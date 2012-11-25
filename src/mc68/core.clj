(ns mc68.core
  (:require [twitter.oauth]
            [twitter.api.restful])
  (:import (twitter.callbacks.protocols SyncSingleCallback)))

(defn tweet-mc68 [msg]
  (let [creds (twitter.oauth/make-oauth-creds
               "h20bKkS1RdDYtZeinOyHw"
               "nS5nMR0ZbMAQAJA8Tcd6GL76qpWPvC2vPSuJN5MY4"
               "969500324-EKgYL4bcB2Tv1ovaux5aaaBTGqj36ekrJFq7aLnt"
               "pPASxxTT4xr0JzwaOobY0XcTzl1BME9iyU7CK3VRnw")]
    (twitter.api.restful/update-status :oauth-creds creds :params {:status msg})))

(defn on-enable [plugin]
  #_(when-not swank*
    (def swank* (swank.swank/start-repl 4005))))

(defn async-player-chat-event [evt]
  (let [pname (.getName (.getPlayer evt))]
    (when-let [msg (second (first (re-seq #"^t:\s(.*)" (.getMessage evt))))]
      (future
        (tweet-mc68 (format "<%s>: %s" pname msg))))))
