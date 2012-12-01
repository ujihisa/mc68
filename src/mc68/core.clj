(ns mc68.core
  (:require [twitter.oauth]
            [twitter.api.restful]
            [swank.swank])
  (:import (twitter.callbacks.protocols SyncSingleCallback)
           [org.bukkit Bukkit]))

(defn tweet-mc68 [msg]
  (let [creds (twitter.oauth/make-oauth-creds
               "h20bKkS1RdDYtZeinOyHw"
               "nS5nMR0ZbMAQAJA8Tcd6GL76qpWPvC2vPSuJN5MY4"
               "969500324-EKgYL4bcB2Tv1ovaux5aaaBTGqj36ekrJFq7aLnt"
               "pPASxxTT4xr0JzwaOobY0XcTzl1BME9iyU7CK3VRnw")]
    (twitter.api.restful/update-status :oauth-creds creds :params {:status msg})))

(defn ujm []
  (Bukkit/getPlayer "ujm"))
(defn mozukusoba []
  (Bukkit/getPlayer "mozukusoba"))
(defn sixeight []
  (Bukkit/getPlayer "Sixeight6680"))
(defn ast []
  (Bukkit/getPlayer "ast924"))
(defn raa []
  (Bukkit/getPlayer "raa0121"))

(defn projectile-hit-event [evt]
  (defn hp2damage [n]
    (inc (Math/pow (/ (- 20 n) 4) 2)))
  (let [proj (.getEntity evt)
        shooter (.getShooter proj)]
    (when (and
            (instance? org.bukkit.entity.Arrow proj)
            (instance? org.bukkit.entity.Player shooter))
      (doseq [t (.getNearbyEntities proj 5 3 5)
              :when (instance? org.bukkit.entity.Monster t)]
        (.damage t
                 (if (= (ujm) shooter)
                   19
                   (hp2damage (.getHealth shooter)))
                 shooter)))))

(defn player-login-event [evt]
  (let [player (.getPlayer evt)]
    (future
      (tweet-mc68 (format "%s logged in" (.getDisplayName player))))))

(defn async-player-chat-event [evt]
  (let [pname (.getName (.getPlayer evt))]
    (future
      (tweet-mc68 (format "<%s>: %s" pname (.getMessage evt))))
    #_(when-let [msg (second (first (re-seq #"^t:\s(.*)" (.getMessage evt))))]
      (future
        (tweet-mc68 (format "<%s>: %s" pname msg))))))

(defonce swank* nil)
(defonce plugin* nil)
(defn on-enable [plugin]
  (def plugin* plugin)
  (when-not swank*
    (def swank* (swank.swank/start-repl 4006))))

