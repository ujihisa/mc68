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

(defonce plugin* nil)

#_(def egg-throwing (ref false))
(defn projectile-launch-event [evt]
  (let [projectile (.getEntity evt)
        shooter (.getShooter projectile)]
    (when (and
            (instance? org.bukkit.entity.Egg projectile)
            (instance? org.bukkit.entity.Player shooter)
            (= org.bukkit.Material/EGG (.getType (.getItemInHand shooter))))
      #_(.sendMessage shooter (format "egg %d" (.getAmount (.getItemInHand shooter))))
      #_(dotimes [_ (.getAmount (.getItemInHand shooter))]
        (.launchProjectile shooter org.bukkit.entity.Egg))
      (dotimes [i (.getAmount (.getItemInHand shooter))]
        (.scheduleSyncDelayedTask
          (Bukkit/getScheduler)
          plugin*
          #(.launchProjectile shooter org.bukkit.entity.Egg)
          i))
      (.setItemInHand shooter nil)
      #_(.sendMessage shooter (format "egg %d" (.getAmount (.getItemInHand shooter)))))))

(defn player-interact-event [evt]
  (when (and
          (.getPlayer evt)
          (= org.bukkit.Material/AIR (.getType (.getItemInHand (.getPlayer evt)))))
    (let [block (.getClickedBlock evt)]
      (when (= org.bukkit.Material/WOOL (.getType block))
        (.setData block (mod (inc (.getData block)) 8))))))

(def random-block-candidates
  [org.bukkit.Material/SANDSTONE org.bukkit.Material/CLAY]
  #_( org.bukkit.Material/NETHERRACK))

(defn explosion-prime-event [evt]
  (when (instance? org.bukkit.entity.TNTPrimed (.getEntity evt))
    "default tnt radius is 4"
    (.setRadius evt 5)))

(defn tnt-explode-event-without-lapis [evt]
  (let [blocks (vec (.blockList evt))]
    (doseq [block blocks]
      (let [fblock (.spawnFallingBlock (.getWorld block) (.getLocation block) (.getType block) (.getData block))]
        (.setVelocity fblock (doto (.getVelocity fblock) (.add (org.bukkit.util.Vector. (/ (rand) 2.0) (rand) (/ (rand) 2.0))))))
      (.setType block org.bukkit.Material/AIR))
    #_(when (not-empty (filter #(= org.bukkit.Material/GOLD_BLOCK (.getType %)) blocks))
      (.setCancelled evt true)
      (doseq [block blocks]
        (.setType block (rand-nth random-block-candidates))))))

(defn entity-explode-event [evt]
  (when (instance? org.bukkit.entity.TNTPrimed (.getEntity evt))
    (let [lapises (filter #(= org.bukkit.Material/LAPIS_BLOCK (.getType %)) (vec (.blockList evt)))]
      (if (empty? lapises)
        (tnt-explode-event-without-lapis evt)
        (do
          (.setCancelled evt true)
          (doseq [lapis lapises
                  :let [loc (.getLocation lapis)]
                  [x y z loc-around] (for [[x y z] [[0 0 1] [0 1 0] [1 0 0] [0 0 -1] [0 -1 0] [-1 0 0]]]
                                       [x y z (.add (.clone loc) x y z)])
                  :when (#{org.bukkit.Material/REDSTONE_TORCH_ON org.bukkit.Material/REDSTONE_TORCH_OFF}
                          (.getType (.getBlock loc-around)))
                  :let [[replace-to-type replace-to-data] (let [replace-to (.getBlock (.add (.clone loc-around) x y z))]
                                                            [(.getType replace-to) (.getData replace-to)])
                        [blocks-line blocks-leftover] (split-with #(not= org.bukkit.Material/LAPIS_BLOCK (.getType %))
                                                                  (map #(.getBlock %)
                                                                       (take 200 (iterate #(doto % (.add x y z)) (.clone loc-around)))))]
                  :when (not (#{org.bukkit.Material/DIAMOND_ORE org.bukkit.Material/DIAMOND_BLOCK} replace-to-type))
                  :when (when-let [firstleftover (first blocks-leftover)]
                          (= org.bukkit.Material/LAPIS_BLOCK (.getType firstleftover)))]
            (.strikeLightningEffect (.getWorld loc-around) loc-around)
            (doseq [block blocks-line]
              (.setType block replace-to-type)
              (.setData block replace-to-data))))))))

(defn player-move-event [evt]
  (let [player (.getPlayer evt)]
    (when (and
            (.isSneaking player)
            (= org.bukkit.Material/LADDER (.getType (.getBlock (.getLocation player))))
            (< (.getY (.getFrom evt)) (.getY (.getTo evt))))
      (if (= 15 (.getLightFromSky (.getBlock (.getLocation player))))
        (let [n (if (= org.bukkit.Material/LADDER (.getType (.getBlock (.add (.getLocation player) 0 1 0))))
                  (if (= org.bukkit.Material/LADDER (.getType (.getBlock (.add (.getLocation player) 0 2 0))))
                    3
                    2)
                  1)]
          (.teleport player (doto (.getTo evt) (.add 0 n 0))))
        (.setVelocity player (org.bukkit.util.Vector. 0 5 0))))))

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
(defn on-enable [plugin]
  (def plugin* plugin)
  (when-not swank*
    (def swank* (swank.swank/start-repl 4006))))

