(ns mc68.core
  (:require [twitter.oauth]
            [twitter.api.restful]
            [swank.swank])
  (:import [org.bukkit Bukkit DyeColor Material]
           [org.bukkit.material Wool Dye]
           [org.bukkit.entity Animals Arrow Blaze Boat CaveSpider Chicken
            ComplexEntityPart ComplexLivingEntity Cow Creature Creeper Egg
            EnderCrystal EnderDragon EnderDragonPart Enderman EnderPearl
            EnderSignal ExperienceOrb Explosive FallingSand Fireball Fish
            Flying Ghast Giant HumanEntity IronGolem Item LightningStrike
            LivingEntity MagmaCube Minecart Monster MushroomCow NPC Painting
            Pig PigZombie Player PoweredMinecart Projectile Sheep Silverfish
            Skeleton Slime SmallFireball Snowball Snowman Spider Squid
            StorageMinecart ThrownPotion TNTPrimed Vehicle Villager
            Villager$Profession WaterMob Weather Wolf Zombie Ocelot
            Bat]
           [org.bukkit.event.entity EntityDamageByEntityEvent
            EntityDamageEvent$DamageCause CreatureSpawnEvent$SpawnReason]
           [org.bukkit.potion Potion PotionEffect PotionEffectType]
           [org.bukkit.inventory ItemStack]
           [org.bukkit.util Vector]
           [org.bukkit Location Effect]
           [org.bukkit.block Biome]
           [org.bukkit.event.block Action]
           [org.bukkit.enchantments Enchantment])
  (:import #_(twitter.callbacks.protocols SyncSingleCallback)
           [de.ntcomputer.minecraft.controllablemobs.api ControllableMobs]
           [de.ntcomputer.minecraft.controllablemobs.api.actions ActionState
            ActionType]
           [de.ntcomputer.minecraft.controllablemobs.api.aibehaviors
            EntityFilter AIAttackMelee AIAttackRanged AIBehavior AIFleeSun
            AIFloat AILookAtEntity AIRandomLookaround AIRandomStroll
            AIRestrictSun AITargetBehavior AITargetBehaviorEx AITargetHurtBy
            AITargetNearest]))

(defonce plugin* nil)

(defmacro later [& exps]
  `(.scheduleSyncDelayedTask
     (Bukkit/getScheduler)
     mc68.core/plugin*
     (fn [] ~@exps)
     0))

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
            (instance? Arrow proj)
            (instance? Player shooter))
      (doseq [t (.getNearbyEntities proj 5 3 5)
              :when (instance? Monster t)]
        (.damage t
                 (if (= (ujm) shooter)
                   19
                   (hp2damage (.getHealth shooter)))
                 shooter)))))

#_(def egg-throwing (ref false))
(defn projectile-launch-event [evt]
  (let [projectile (.getEntity evt)
        shooter (.getShooter projectile)]
    (when (and
            (instance? Egg projectile)
            (instance? Player shooter)
            (= Material/EGG (.getType (.getItemInHand shooter))))
      #_(.sendMessage shooter (format "egg %d" (.getAmount (.getItemInHand shooter))))
      #_(dotimes [_ (.getAmount (.getItemInHand shooter))]
        (.launchProjectile shooter Egg))
      (dotimes [i (.getAmount (.getItemInHand shooter))]
        (.scheduleSyncDelayedTask
          (Bukkit/getScheduler)
          plugin*
          #(.launchProjectile shooter Egg)
          i))
      (.setItemInHand shooter nil)
      #_(.sendMessage shooter (format "egg %d" (.getAmount (.getItemInHand shooter)))))))

(defn player-interact-entity-event [evt]
  (let [target (.getRightClicked evt)
        player (.getPlayer evt)]
    (condp instance? target
      Villager
      (when (= Material/FLINT_AND_STEEL (.getType (.getItemInHand player)))
        (.setCancelled evt true)
        (.setFireTicks target 800))
      nil)))

(def mozukusoba-house
  (Location. (Bukkit/getWorld "world")
             -51.53295508074419
             63.0
             351.7543264263179
             -359.43036
             2.2501302))

(def city-entrance
  (Location. (Bukkit/getWorld "city")
             -45.0 66.0 81.0 0.0 0.0))

(defn player-interact-event [evt]
  (let [player (.getPlayer evt)]
    (when-let [block (.getClickedBlock evt)]
      (condp = (.getType block)
        Material/STONE_PLATE
        (when (= org.bukkit.event.block.Action/PHYSICAL (.getAction evt))
          (when (every? (fn [[x y z]]
                          (= Material/GOLD_BLOCK (.getType (.getBlock (.add (.getLocation block) x y z)))))
                        [[0 3 0] [2 0 0] [-2 0 0] [0 0 2] [0 0 -2]])
            (if (= "world" (.getName (.getWorld player)))
              (do
                (.teleport player city-entrance)
                (.teleport player city-entrance))
              (do
                (.teleport player mozukusoba-house)
                (.teleport player mozukusoba-house)))))
        Material/WOOL
        (when (= Material/AIR (.getType (.getItemInHand player)))
          (if (= org.bukkit.event.block.Action/RIGHT_CLICK_BLOCK (.getAction evt))
            (.setData block (mod (dec (.getData block)) 16))
            (.setData block (mod (inc (.getData block)) 16))))
        nil))))

(def random-block-candidates
  [Material/SANDSTONE Material/CLAY]
  #_( Material/NETHERRACK))

(defn explosion-prime-event [evt]
  (when (instance? TNTPrimed (.getEntity evt))
    "default tnt radius is 4"
    (.setRadius evt 5)))

(defn tnt-explode-event-without-lapis [evt]
  (let [blocks (vec (.blockList evt))]
    (doseq [block blocks]
      (let [ftype (.getType block)
            ftype2 ({Material/GRASS Material/DIRT}
                     ftype ftype)
            fblock (.spawnFallingBlock (.getWorld block) (.getLocation block) ftype2 (.getData block))]
        (.setVelocity fblock (doto (.getVelocity fblock) (.add (org.bukkit.util.Vector. 0.0 1.5 0.0))))
        (when (= Material/GRASS (.getMaterial fblock))
          (.setDropItem fblock )))
      (.setType block Material/AIR))
    #_(when (not-empty (filter #(= Material/GOLD_BLOCK (.getType %)) blocks))
      (.setCancelled evt true)
      (doseq [block blocks]
        (.setType block (rand-nth random-block-candidates))))))

(defn entity-explode-event [evt]
  (when (instance? TNTPrimed (.getEntity evt))
    (let [lapises (filter #(= Material/LAPIS_BLOCK (.getType %)) (vec (.blockList evt)))]
      (if (empty? lapises)
        (tnt-explode-event-without-lapis evt)
        (do
          (.setCancelled evt true)
          (doseq [lapis lapises
                  :let [loc (.getLocation lapis)]
                  [x y z loc-around] (for [[x y z] [[0 0 1] [0 1 0] [1 0 0] [0 0 -1] [0 -1 0] [-1 0 0]]]
                                       [x y z (.add (.clone loc) x y z)])
                  :when (#{Material/REDSTONE_TORCH_ON Material/REDSTONE_TORCH_OFF}
                          (.getType (.getBlock loc-around)))
                  :let [[replace-to-type replace-to-data] (let [replace-to (.getBlock (.add (.clone loc-around) x y z))]
                                                            [(.getType replace-to) (.getData replace-to)])
                        [blocks-line blocks-leftover] (split-with #(not= Material/LAPIS_BLOCK (.getType %))
                                                                  (map #(.getBlock %)
                                                                       (take 200 (iterate #(doto % (.add x y z)) (.clone loc-around)))))]
                  :when (not (#{Material/DIAMOND_ORE Material/DIAMOND_BLOCK} replace-to-type))
                  :when (when-let [firstleftover (first blocks-leftover)]
                          (= Material/LAPIS_BLOCK (.getType firstleftover)))]
            (.strikeLightningEffect (.getWorld loc-around) loc-around)
            (doseq [block blocks-line]
              (.setType block replace-to-type)
              (.setData block replace-to-data))))))))

(defn player-toggle-sneak-event [evt]
  (when (.isSneaking evt)
    (when-let [vehicle (.getVehicle (.getPlayer evt))]
      (when (instance? Boat vehicle)
        (.setVelocity vehicle (doto (.getVelocity vehicle) (.setY 0.8)))))))

(defn player-move-event [evt]
  (let [player (.getPlayer evt)]
    (when (and
            (.isSneaking player)
            (= Material/LADDER (.getType (.getBlock (.getLocation player))))
            (< (.getY (.getFrom evt)) (.getY (.getTo evt))))
      (if (= 15 (.getLightFromSky (.getBlock (.getLocation player))))
        (let [n (if (= Material/LADDER (.getType (.getBlock (.add (.getLocation player) 0 1 0))))
                  (if (= Material/LADDER (.getType (.getBlock (.add (.getLocation player) 0 2 0))))
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

(defn entity-damage-event [evt]
  (condp = (.getCause evt)
    org.bukkit.event.entity.EntityDamageEvent$DamageCause/FALL
    (let [entity (.getEntity evt)
          loc (.getLocation entity)
          block (.getBlock loc)
          block-below (.getBlock (.add (.clone loc) 0 -1 0))]
      (when (instance? Player entity)
        (cond
          (= Material/GRASS (.getType block-below))
          (do
            (.setCancelled evt true)
            (.setType block-below Material/DIRT))
          (= Material/BED_BLOCK (.getType block))
          (do
            (.setCancelled evt true)
            (when-not (.isSneaking entity)
              (.setVelocity entity (doto (.getVelocity entity) (.setY (+ 0.9))))))
          #_(later
            (.setVelocity entity (let [v (.getVelocity entity)]
                                   (.setY v (* (.getY v) -0.9))
                                   v))))))
    nil))

(defonce swank* nil)
(defn on-enable [plugin]
  (def plugin* plugin)
  (when-not swank*
    (def swank* (swank.swank/start-repl 4006))))

(defn spawn-safe [loc klass]
  (let [e (.spawn (.getWorld loc) loc klass)
        m (ControllableMobs/assign e true)]
    #_(.clearAIBehaviors m)
    (.addAIBehavior m (AIFloat.))
    [e m]))
