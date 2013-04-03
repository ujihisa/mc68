(ns mc68.core
  (:require [twitter.oauth]
            [twitter.api.restful]
            [swank.swank]
            [cloft.sound :as s]
            [cloft.loc :as loc]
            [cloft.material :as m]
            [cloft.cloft :as c]
            [clj-http.client])
  (:import [org.bukkit Bukkit DyeColor Material Color Location Effect]
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
            Bat Hanging]
           [org.bukkit.event.entity EntityDamageByEntityEvent
            EntityDamageEvent$DamageCause CreatureSpawnEvent$SpawnReason]
           [org.bukkit.potion Potion PotionEffect PotionEffectType]
           [org.bukkit.inventory ItemStack CraftingInventory]
           [org.bukkit.util Vector]
           [org.bukkit.block Biome]
           [org.bukkit.event.block Action]
           [org.bukkit.enchantments Enchantment]
           [org.dynmap DynmapCommonAPI]
           [org.dynmap.markers MarkerSet])
  #_(:import #_(twitter.callbacks.protocols SyncSingleCallback)
           [de.ntcomputer.minecraft.controllablemobs.api ControllableMobs]
           [de.ntcomputer.minecraft.controllablemobs.api.actions ActionState
            ActionType]
           [de.ntcomputer.minecraft.controllablemobs.api.aibehaviors
            EntityFilter AIAttackMelee AIAttackRanged AIBehavior AIFleeSun
            AIFloat AILookAtEntity AIRandomLookaround AIRandomStroll
            AIRestrictSun AITargetBehavior AITargetBehaviorEx AITargetHurtBy
            AITargetNearest]))

(defonce plugin* nil)
(def creative (atom #{}))

(def oauth
  (try
    (clojure.string/split (slurp (clojure.java.io/resource "oauth.txt")) #"\n" 4)
    (catch Exception e nil)))

(defmacro later [sec & exps]
  `(.scheduleSyncDelayedTask
     (Bukkit/getScheduler)
     mc68.core/plugin*
     (fn [] ~@exps)
     (int (* 20 ~sec))))

(defn tweet-mc68 [msg]
  (when oauth
    (let [creds (apply twitter.oauth/make-oauth-creds oauth)]
      (twitter.api.restful/update-status :oauth-creds creds :params {:status msg}))))

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
(defn bgnori []
  (Bukkit/getPlayer "bgnori"))
(defn sasanomiya []
  (Bukkit/getPlayer "sasanomiya"))
(defn momonga []
  (Bukkit/getPlayer "supermomonga"))

(defn marker-api []
  (.getMarkerAPI (.getPlugin (Bukkit/getPluginManager) "dynmap")))

(defn a-marker-set []
  (first (into #{} (.getMarkerSets (marker-api)))))

(defn new-pin [location label description icon persistant]
  (.createMarker (a-marker-set) label description (.getName (.getWorld location))
                 (.getX location) (.getY location) (.getZ location)
                 icon persistant))

(def special-arrows (atom #{}))
(defn projectile-hit-event [evt]
  (defn hp2damage [n]
    (inc (Math/pow (/ (- 20 n) 4) 2)))
  (let [proj (.getEntity evt)
        shooter (.getShooter proj)]
    (condp instance? proj
      Arrow
      (condp instance? shooter
        Player
        (do
          (when (@special-arrows proj)
            (.remove proj))
          (doseq [t (.getNearbyEntities proj 5 3 5)
                  :when (and
                          (instance? Monster t)
                          (not (instance? Giant t)))]
            (.damage t
                     (if (= (ujm) shooter)
                       19
                       (hp2damage (.getHealth shooter)))
                     shooter)))
        Skeleton
        (cond
          (= Material/GOLD_HELMET (.getType (.getHelmet (.getEquipment shooter))))
          (do
            (.teleport shooter (.getLocation proj))
            (.remove proj))
          (= Material/DIAMOND_HELMET (.getType (.getHelmet (.getEquipment shooter))))
          (.remove proj)
          :else nil)
        nil)
      nil)))

(def magic-point (atom {}))

(def show-current-mp-last (atom {}))
(defn show-current-mp [player]
  (let [pname (.getDisplayName player)
        mp (@magic-point pname)]
    (when (not= (@show-current-mp-last pname) mp)
      (.sendMessage player (format "Current MP: %d" mp))
      (swap! show-current-mp-last assoc pname mp))))

(defn player-item-held-event [evt]
  (let [player (.getPlayer evt)
        new-slot (.getNewSlot evt)]
    (when-let [item (get (.getContents (.getInventory player)) new-slot)]
      (when (= Material/BOOK_AND_QUILL (.getType item))
        (show-current-mp player)))))

(def skeleton-shooting (ref false))
(defn projectile-launch-event [evt]
  (let [projectile (.getEntity evt)
        shooter (.getShooter projectile)]
    (condp instance? projectile
      Egg
      (when (and
              (instance? Player shooter)
              (= Material/EGG (.getType (.getItemInHand shooter))))
        #_(.sendMessage shooter (format "egg %d" (.getAmount (.getItemInHand shooter))))
        #_(dotimes [_ (.getAmount (.getItemInHand shooter))]
          (.launchProjectile shooter Egg))
        (dotimes [i (.getAmount (.getItemInHand shooter))]
          (later i
            (.launchProjectile shooter Egg)))
        (.setItemInHand shooter nil)
        #_(.sendMessage shooter (format "egg %d" (.getAmount (.getItemInHand shooter)))))
      Arrow
      (condp instance? shooter
        Skeleton
        (when-not @skeleton-shooting
          (cond
            (= Material/IRON_HELMET (.getType (.getHelmet (.getEquipment shooter))))
            (future
              (dotimes [_ 5]
                (Thread/sleep 500)
                (later 0
                  (dosync (ref-set skeleton-shooting true))
                  (.launchProjectile shooter Arrow)
                  (dosync (ref-set skeleton-shooting false)))))
            (= Material/DIAMOND_HELMET (.getType (.getHelmet (.getEquipment shooter))))
            (do
              (dosync (ref-set skeleton-shooting true))
              (dotimes [_ 80]
                (let [rand1 (fn [] (* 0.8 (- (rand) 0.5)))
                      new-arrow (.launchProjectile shooter Arrow)]
                  (.setVelocity new-arrow (.add (.getVelocity projectile)
                                                (Vector. (rand1) (rand1) (rand1))))))
              (dosync (ref-set skeleton-shooting false)))
            :else nil))
        Player
        (if (or
              (.isOnGround shooter)
              #_(= -0.0784000015258789 (.getY (.getVelocity shooter)))
              (.isLiquid (.getBlock (.getLocation shooter))))
          (when (and
                  (or
                    (and
                      (or (ujm) (mozukusoba))
                      (#{(ujm) (mozukusoba)} shooter))
                    (= "world_supermomonga" (.getName (.getWorld shooter))))
                  (not (.isSneaking shooter)))
            (swap! special-arrows conj projectile))
          (later 0
            (.setVelocity shooter (.multiply (.getVelocity projectile) -0.7))))
        nil)
      nil)))

(def mp-max 100)
(defn player-bed-enter-event [evt]
  (let [player (.getPlayer evt)]
    (swap! magic-point assoc (.getDisplayName player) mp-max)))

(defn entity-shoot-bow-event [evt]
  #_(prn 'ok evt))

(defn- consume-item [player]
  (when-not (@creative (.getName player))
    (let [itemstack (.getItemInHand player)
          amount (.getAmount itemstack)]
      (if (= 1 amount)
        (.remove (.getInventory player) itemstack)
        (.setAmount itemstack (dec amount))))))

(defn player-ride-spider [player target]
  (when (nil? (.getTarget target))
    (.setPassenger target player)))

(defn player-give-slimeball-to-slime [player target]
  (when (= m/slime-ball (.getType (.getItemInHand player)))
    (consume-item player)
    (.setSize target (min 10 (inc (.getSize target))))))

(defn player-interact-entity-event [evt]
  (let [target (.getRightClicked evt)
        player (.getPlayer evt)]
    (condp instance? target
      Villager
      (when (= Material/FLINT_AND_STEEL (.getType (.getItemInHand player)))
        (.setCancelled evt true)
        (.setFireTicks target 800))
      Zombie
      (when (= Material/ROTTEN_FLESH (.getType (.getItemInHand player)))
        (consume-item player)
        (when (= 0 (rand-int 10))
          (.spawn (.getWorld target) (.getLocation target) Giant)
          (.remove target)))
      Spider
      (player-ride-spider player target)
      Slime
      (player-give-slimeball-to-slime player target)
      Painting
      (let [art (rand-nth (vec (org.bukkit.Art/values)))]
        (.sendMessage player (str art))
        (.setArt target art))
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

(defn- blockface2loc [block]
  (let [face (.getFacing (.getData (.getState block)))]
    (.subtract (.getLocation block) (.getModX face) (.getModY face) (.getModZ face))))

(def weapons
  [Material/AIR
   Material/WOOD_SWORD Material/WOOD_SWORD
   Material/IRON_SWORD Material/IRON_SWORD
   Material/GOLD_SWORD Material/GOLD_SWORD
   Material/DIAMOND_SWORD
   Material/BOW Material/BOW
   Material/BOW])

(defn creature-spawn-event [evt]
  (let [entity (.getEntity evt)]
    (condp instance? entity
      PigZombie
      (when (= "world_nether" (.getName (.getWorld entity)))
        (when (= (rand-int 3) 0)
          (.setCancelled evt true)
          (.setPowered (loc/spawn (.getLocation evt) Creeper) true)))
      Zombie
      (do
        (condp = (.getSpawnReason evt)
          CreatureSpawnEvent$SpawnReason/NATURAL
          (do
            (when (= 0 (rand-int 3))
              (let [[new-cp new-hp] (rand-nth [[Material/IRON_CHESTPLATE 50]
                                               [Material/GOLD_CHESTPLATE 50]
                                               [Material/DIAMOND_CHESTPLATE 100]])]
                (.setChestplate (.getEquipment entity) (ItemStack. new-cp 1))
                (.setMaxHealth entity new-hp)
                (.setHealth entity new-hp)))
            (when (= 0 (rand-int 10))
              (.spawn (.getWorld entity) (.getLocation entity) Zombie))
            (let [eq (.getEquipment entity)]
              (.setItemInHand eq (ItemStack. (rand-nth weapons) 1))
              (def helmets
                {Material/AIR nil
                 Material/LEATHER_HELMET nil
                 Material/STONE_PLATE nil
                 Material/PUMPKIN nil
                 Material/TNT nil
                 Material/SKULL_ITEM (range 0 5)})
              (let [[helmet data] (rand-nth (vec helmets))
                    is (ItemStack. helmet 1)]
                (when data
                  (.setData (.getData is) (rand-nth data)))
                (.setHelmet eq is))))
          CreatureSpawnEvent$SpawnReason/SPAWNER
          (.setChestplate (.getEquipment entity) nil)
          nil))
      Skeleton
      (when (= CreatureSpawnEvent$SpawnReason/NATURAL (.getSpawnReason evt))
        (case (rand-int 4)
          0
          (.setHelmet (.getEquipment entity) (ItemStack. Material/IRON_HELMET 1))
          1
          (.setHelmet (.getEquipment entity) (ItemStack. Material/GOLD_HELMET 1))
          2
          (.setHelmet (.getEquipment entity) (ItemStack. Material/DIAMOND_HELMET 1))
          nil))
      nil)))

(defn item-spawn-event [evt]
  (let [item (.getEntity evt)]
    (when (= Material/STATIONARY_WATER (.getType (.getItemStack item)))
      (.setCancelled evt true))))

(defn item-despawn-event [evt]
  #_(let [item (.getEntity evt)
        is (.getItemStack item)]
    (prn (.getType (.getBlock (.getLocation item))))
    (when (= Material/FIRE (.getType (.getBlock (.getLocation item))))
      (prn 'despawn (.getType is)))))

(defn illusion [player duration material1 material2 cont]
  (let [loc (.getLocation player)
        locs (for [y [1 0 -1 #_( 2 -2)]
                   x [0 -1 1 #_(-2)]
                   z [0 -1 1 #_(-2)]]
               (.add (.clone loc) x y z))]
    (future
      (doseq [[l1 l2] (map vector locs (rest locs))]
        #_(Thread/sleep duration)
        (Thread/sleep (* 3 duration))
        (later 0
          (.sendBlockChange player l1 material1 (byte 0))
          #_(.sendBlockChange player l2 material2 (byte 0))))
      (cont))))

#_(defn illusion-rand [player materials]
  (let [loc (.getLocation player)]
    (doseq [x (range -2 3)
            y (range -2 3)
            z (range -2 3)
            :let [l (.add (.clone loc) x y z)]]
      (.sendBlockChange player l (rand-nth materials) (byte 0)))))

(def combust-item-queue (atom []))

(defn entity-combust-event [evt]
  (let [entity (.getEntity evt)]
    (when (instance? Item entity)
      (let [is (.getItemStack entity)]
        (condp = (.getType is)
          m/smooth-brick
          (when (= 3 (.getData (.getData is)))
            #_(fixme! duplicate)
            (dotimes [_ (.getAmount is)]
              (let [loc (.getLocation entity)]
              (loc/play-effect loc Effect/ENDER_SIGNAL nil)
              (loc/play-sound loc s/piston-extend 1.0 0.5)
              (let [velo (.getVelocity entity)
                    new-is (ItemStack. m/cobblestone 64)
                    new-item (.dropItem (.getWorld entity) (.add (.getLocation entity) 0.0 0.9 0.0) new-is)]
                (.remove entity)
                (later 0 (.setVelocity new-item (.add (.multiply velo 1.5)
                                                    (Vector. 0.0 0.5 0.0))))))))
          m/cobblestone
          (when (= 64 (.getAmount is))
            (let [loc (.getLocation entity)]
              (loc/play-effect loc Effect/ENDER_SIGNAL nil)
              (loc/play-sound loc s/piston-extend 1.0 0.5)
              (let [velo (.getVelocity entity)
                    new-is (.toItemStack (org.bukkit.material.SmoothBrick. m/smooth-brick (byte 3)) 1)
                    new-item (.dropItem (.getWorld entity) (.add (.getLocation entity) 0.0 0.9 0.0) new-is)]
                (.remove entity)
                (later 0 (.setVelocity new-item (.add (.multiply velo 1.5)
                                                    (Vector. 0.0 0.5 0.0)))))))
          Material/ROTTEN_FLESH
          (when (= [Material/REDSTONE Material/REDSTONE Material/REDSTONE]
                   (vec (take 3 @combust-item-queue)))
            (let [loc (.getLocation entity)
                  klass (rand-nth [Zombie Skeleton Spider Enderman Creeper Blaze Squid PigZombie Ghast])]
              (loc/play-effect loc Effect/ENDER_SIGNAL nil)
              (loc/play-sound loc s/cat-hiss 1.0 1.0)
              (future
                (dotimes [_ 5]
                  (Thread/sleep 1000)
                  (later 0 (.spawn (.getWorld loc) loc klass))))))
          Material/RAW_BEEF
          (let [velo (.getVelocity entity)
                new-item (.dropItem (.getWorld entity) (.add (.getLocation entity) 0.0 0.9 0.0) (ItemStack. Material/COOKED_BEEF (.getAmount is)))]
            (.remove entity)
            (later 0 (.setVelocity new-item (.add (.multiply velo 1.5)
                                                (Vector. 0.0 0.5 0.0)))))
          Material/COOKED_BEEF
          (do
            (.remove entity)
            (.dropItem (.getWorld entity) (.add (.getLocation entity) 0.0 0.9 0.0) (ItemStack. Material/RAW_BEEF (.getAmount is))))
          nil)
        (swap! combust-item-queue conj (.getType is))
        (future
          (Thread/sleep 5000)
          (swap! combust-item-queue rest))))))

#_(defn entity-combust-by-block-event [evt]
  (prn 'entity-combust-by-block-event)
  (let [entity (.getEntity evt)]
    (when (instance? Item entity)
      (let [is (.getItemStack entity)
            block (.getCombustr evt)]
        (.sendMessage (ujm) (prn-str 'combust block is))))))

(defn block-physics-event [evt]
  (comment (let [block (.getBlock evt)]
    (when (and
            (= Material/STATIONARY_WATER (.getChangedType evt))
            (= (byte 7) (.getData block)))
      (prn 'ok)))))

(defn player-drink-milk [player]
  (let [is (.getItemInHand player)]
    (when (= Material/MILK_BUCKET (.getType is))
      (let [new-mp (min mp-max (+ 50 (@magic-point (.getDisplayName player))))]
        (swap! magic-point assoc (.getDisplayName player) new-mp)
        (show-current-mp player)
        (.setItemInHand player (ItemStack. Material/BUCKET 1))))))

(def loc-san-francisco (Location. (Bukkit/getWorld "world") -349 71 975))

(defn player-physical-wood-plate [player block]
  (let [loc (.getLocation block)]
    (when (= (Location. (.getWorld loc) -70 57 277) loc)
      (.teleport player loc-san-francisco))))

(defn player-interact-block-event [evt player block]
  (condp = (.getType block)
    m/furnace
    (do
      (when (and
              (= m/clay-ball (.getType (.getItemInHand player)))
              (= (ujm) player))
        (let [inv (.getInventory (.getState block))]
          (when (nil? (.getSmelting inv))
            (.setSmelting inv (ItemStack. m/clay-ball 64))
            (.setFuel inv (ItemStack. m/coal 50))))))
    m/jukebox
    (do
      (when (= Action/RIGHT_CLICK_BLOCK (.getAction evt))
        (when (and
                (= m/gold-record (.getType (.getItemInHand player)))
                (not (.isPlaying (.getState block))))
          (future
            (Thread/sleep 100)
            (later 0
              (doseq [v (.getNearbyEntities player 15 15 15)
                      :when (instance? Villager v)]
                (.damage v 30)))))))
    m/sponge
    (when (= Action/RIGHT_CLICK_BLOCK (.getAction evt))
      (when (> 20 (.getFoodLevel player))
        (.setFoodLevel player 30)
        (.setType block (rand-nth [m/dirt m/coal-ore m/iron-ore m/lapis-ore m/snow m/obsidian]))
        (when (= 0 (rand-int 100))
          (dotimes [_ 10]
            (loc/spawn (.getLocation block) Villager)))))
    m/wood-plate
    (when (= Action/PHYSICAL (.getAction evt))
      (player-physical-wood-plate player block))
    m/stone-plate
    (when (= Action/PHYSICAL (.getAction evt))
      (when (every? (fn [[x y z]]
                      (= Material/GOLD_BLOCK (.getType (.getBlock (.add (.getLocation block) x y z)))))
                    [[0 3 0] [2 0 0] [-2 0 0] [0 0 2] [0 0 -2]])
        (if (= "world" (.getName (.getWorld player)))
          (future
            (loc/play-sound (.getLocation player) s/eat 1.0 1.0)
            (later 0
              (doseq [x (range -10 11)
                      y (range -3 5)
                      z (range -10 11)
                      :let [l (.add (.getLocation block) x y z)
                            type-from (.getType (.getBlock l))
                            type-to (get {m/air nil m/torch m/redstone-torch-on} type-from m/stone)]
                      :when type-to]
                (.sendBlockChange player l type-to (byte 0))))
            (Thread/sleep 2000)
            (later 0
              (.strikeLightningEffect (.getWorld block) (.getLocation block))
              (.strikeLightningEffect (.getWorld city-entrance) city-entrance)
              (.teleport player city-entrance)
              (.teleport player city-entrance)))
          (later 0
            (.strikeLightningEffect (.getWorld block) (.getLocation block))
            (.strikeLightningEffect (.getWorld mozukusoba-house) mozukusoba-house)
            (.teleport player mozukusoba-house)
            (.teleport player mozukusoba-house)))))
    Material/WOOL
    (when (= Material/AIR (.getType (.getItemInHand player)))
      (if (= Action/RIGHT_CLICK_BLOCK (.getAction evt))
        (.setData block (mod (dec (.getData block)) 16))
        (.setData block (mod (inc (.getData block)) 16))))
    (do
      #_(when (= (ujm) player)
        (.sendMessage (ujm) "hi")
        (doseq [x (range -20 20)
                y (range -10 10)
                z (range -20 20)
                :let [b (.getBlock (.add (.getLocation (ujm)) x y z))]
                :when (= Material/GOLD_BLOCK (.getType b))]
          (doseq [y2 (range 0 30)
                  :let [b2 (.getBlock (.add (.getLocation (ujm)) x (+ y y2) z))]]
            (.setType b2 Material/STONE))))
      (when (and
            (#{Material/REDSTONE_TORCH_ON Material/REDSTONE_TORCH_OFF} (.getType block))
            (= Material/GOLD_INGOT (.getType (.getItemInHand player))))
      (let [block-to-copy (.getBlock (blockface2loc block))]
        (when-not (#{Material/GOLD_BLOCK Material/GOLD_ORE Material/DIAMOND_ORE Material/DIAMOND_BLOCK m/sponge}
                    (.getType block-to-copy))
          (consume-item player)
          (.setType block Material/AIR)
          (doseq [x (range -4 5) z (range -4 5)
                  :let [b (.getBlock (.add (.clone (.getLocation block-to-copy)) x 0 z))]
                  :when (#{Material/AIR Material/LONG_GRASS} (.getType b))]
            (.playEffect (.getWorld b) (.getLocation b) Effect/MOBSPAWNER_FLAMES nil)
            (.setType b (.getType block-to-copy))
            (.setData b (.getData block-to-copy))
            (when (= Material/MOB_SPAWNER (.getType block-to-copy))
              (.setSpawnedType (.getState b) (.getSpawnedType (.getState block-to-copy)))))))))))

(defn player-use-diamond-hoe [player action]
  #_(when (= (ujm) player)
    (doseq [x (range -2 3)
            y (range 10 70)
            z (range -2 3)
            :let [loc (doto (.add (.getLocation (first (.getLastTwoTargetBlocks player (java.util.HashSet. #{(byte 0)}) 100))) x 0 z)
                        (.setY y))]
            #_(:when (< 30 (.getY loc)))
            :let [b (.getBlock loc)]
            #_(:when (#{Material/AIR} (.getType b)))]
      (.setType b Material/AIR)))
  #_(when (#{Action/LEFT_CLICK_AIR
           Action/LEFT_CLICK_BLOCK}
          action)
    (let [hoe (.getItemInHand player)
          dura (.getDurability hoe)]
      (if (< 1562 dura)
        (consume-item player)
        (.setDurability hoe (+ 5 dura))))
    (let [target-block (first (.getLastTwoTargetBlocks player (java.util.HashSet. #{(byte 0)}) 100))
          vect (.normalize (.toVector (.subtract (.getLocation target-block)
                                                 (.getLocation player))))
          w (.spawnFallingBlock (.getWorld player)
                                (.add (.getLocation player) (.multiply vect 3))
                                Material/STATIONARY_WATER (byte 7))]
      (.setVelocity w (.multiply vect 0.7)))))

(defn modify-foodlevel [player f]
  (assert (instance? Player player) player)
  (.setFoodLevel player (f (.getFoodLevel player))))

(defn player-eat-gold-melon [player]
  (when (and
          (= m/speckled-melon (.getType (.getItemInHand player)))
          (not (.hasPotionEffect player PotionEffectType/FAST_DIGGING)))
    (.addPotionEffect player (PotionEffect. PotionEffectType/FAST_DIGGING (* 20 61) 0))
    (modify-foodlevel player inc)
    (loc/play-sound (.getLocation player) s/eat 1.0 1.0)
    (consume-item player)
    (.sendMessage player "fast digging!")))

(defn yaw->xz [yaw]
  [(- (Math/sin (* yaw Math/PI 1/180)))
   (Math/cos (* yaw Math/PI 1/180))])

(defn inventory-open-event [evt]
  #_(let [player (.getPlayer evt)]
    (when (not= "ujm" (.getDisplayName player))
      (let [loc (.getLocation player)
            [x z] (yaw->xz (.getYaw loc))]
        (.add loc x 0 z)
        (.setYaw loc (+ 180 (.getYaw loc)))
        (when (= m/air (.getType (.getBlock loc)))
          (.teleport (ujm) loc))))))

(defn inventory-close-event [evt]
  nil)

(defn player-interact-event [evt]
  (let [player (.getPlayer evt)]
    #_(when (= "ujm" (.getDisplayName player))
      (.teleport (ujm) (let [[x z] (yaw->xz (.getYaw (.getLocation (ujm))))]
                         (.add (.getLocation (ujm)) x 0.0 z))))
    (when (#{Action/RIGHT_CLICK_AIR Action/RIGHT_CLICK_BLOCK} (.getAction evt))
      (player-drink-milk player)
      (player-eat-gold-melon player))
    (when-let [block (.getClickedBlock evt)]
      (player-interact-block-event evt player block))
    #_(when (= Material/WOOD_HOE (.getType (.getItemInHand player)))
      (let [blocks (.getLastTwoTargetBlocks player (java.util.HashSet. #{(byte 0) (byte 20) (byte 102)}) 100)]
        (doseq [[x y z] [[-1 0 0] [1 0 0] [0 -1 0] [0 1 0] [0 0 -1] [0 0 1]]
                :let [b (.getBlock (.add (.getLocation (second blocks)) x y z))]
                :when (= Material/AIR (.getType b))]
          (.setType b Material/FIRE))))
    (when (= m/written-book (.getType (.getItemInHand player)))
      (when (and
              (= Action/LEFT_CLICK_BLOCK (.getAction evt))
              (= "system" (.getAuthor (.getItemMeta (.getItemInHand player)))))
        (let [block (.getClickedBlock evt)
              face (.getBlockFace evt)
              loc (.add (.getLocation block) (.getModX face) (.getModY face) (.getModZ face))]
          (future
            (doseq [x (if (= 0 (.getModX face))
                        (range -1 2)
                        [0])
                    y (if (= 0 (.getModY face))
                        (range -1 2)
                        [0])
                    z (if (= 0 (.getModZ face))
                        (range -1 2)
                        [0])
                    :let [new-type m/wood]]
              (Thread/sleep 100)
              (later 0
                (loc/play-effect loc Effect/MOBSPAWNER_FLAMES nil)
                (let [target-block (.getBlock (.add (.clone loc) x y z))]
                  (when (= m/air (.getType target-block))
                    (.setType target-block new-type)))))))))
    (when (= Material/IRON_HOE (.getType (.getItemInHand player)))
      (when (#{Action/LEFT_CLICK_AIR
               Action/LEFT_CLICK_BLOCK}
              (.getAction evt))
        #_(let [blocks (.getLastTwoTargetBlocks player (java.util.HashSet. #{(byte 0) (byte 20) (byte 102)}) 100)]
          (let [loc (.getLocation (first blocks))]
            (doseq [x (range -3 4)
                    y (range -3 4)
                    z (range -3 4)
                    :let [b (.getBlock (.add (.clone loc) x y z))]
                    :when (= m/stone (.getType b))]
              (.breakNaturally b (ItemStack. m/diamond-pickaxe 1))))
          #_(doseq [block blocks]
            (.strikeLightningEffect (.getWorld block) (.getLocation block)))
          #_(.teleport player (.getLocation (first blocks)))
          #_(.spawn (.getWorld (first blocks)) (.getLocation (first blocks)) Zombie)
          #_(when (= Material/AIR (.getType (first blocks)))
            (.teleport (ujm) (.getLocation (first blocks)))
            #_(let [b (first blocks)]
              (.generateTree (.getWorld b) (.getLocation b) org.bukkit.TreeType/BIG_TREE))
            #_(.setType (first blocks) Material/TORCH)
            #_(.strikeLightningEffect (.getWorld (first blocks)) (.getLocation (first blocks)))))))
    (when (= Material/GOLD_HOE (.getType (.getItemInHand player)))
      (when (#{Action/RIGHT_CLICK_AIR
               Action/RIGHT_CLICK_BLOCK}
              (.getAction evt))
        (when (= 0 (rand-int 5))
          (let [hoe (.getItemInHand player)
                dura (.getDurability hoe)]
            (if (< 33 dura)
              (consume-item player)
              (.setDurability hoe (inc dura)))))
        (dotimes [_ 3]
          (let [p (.launchProjectile player Snowball)]
            (.setFireTicks p 200)
            (.setVelocity p (doto (.getVelocity p)
                              (.add  (Vector. (- (rand) 0.5) 0.0 (- (rand) 0.5)))
                              (.multiply 0.4)))))))
    (when (= Material/DIAMOND_HOE (.getType (.getItemInHand player)))
      (player-use-diamond-hoe player (.getAction evt)))))

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
        (.setVelocity fblock (doto (.getVelocity fblock) (.add (Vector. 0.0 1.5 0.0))))
        (when (= Material/GRASS (.getMaterial fblock))
          (.setDropItem fblock )))
      (.setType block Material/AIR))
    #_(when (not-empty (filter #(= Material/GOLD_BLOCK (.getType %)) blocks))
      (.setCancelled evt true)
      (doseq [block blocks]
        (.setType block (rand-nth random-block-candidates))))))

(defn entity-explode-event [evt]
  (let [entity (.getEntity evt)]
    (when (instance? TNTPrimed entity)
      (if (when-let [vehicle (.getVehicle entity)]
            (instance? Minecart vehicle))
        (comment
          (.setCancelled evt true)
          (let [vehicle (.getVehicle entity)]
            (later 0 (.setPassenger vehicle (.spawn (.getWorld vehicle) (.getLocation vehicle) TNTPrimed)))))
        (let [lapises (filter #(= Material/LAPIS_BLOCK (.getType %)) (vec (.blockList evt)))]
          (.setCancelled evt true)
          (if (empty? lapises)
            (tnt-explode-event-without-lapis evt)
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
                (.setData block replace-to-data)))))))))

(defn player-toggle-sprint-event [evt]
  (let [player (.getPlayer evt)]
    (if (.isSprinting evt)
      (if (= (ujm) player)
        (.setWalkSpeed player 0.35)
        (.setWalkSpeed player 0.3))
      (.setWalkSpeed player 0.2))))

(defn water? [btype]
  (#{Material/STATIONARY_WATER Material/WATER} btype))

(def water-tubing (atom #{}))
(defn water-tube [player]
  (let [loc0 (.getLocation player)]
    (when (and
            (not (@water-tubing player))
            (water? (.getType (.getBlock loc0)))
            (= Material/GLASS (.getType (.getBlock (.add (.clone loc0) 0 -1 0)))))
      (let [[target-loc len]
            (loop [loc loc0 path [loc0]]
              (if (> 700 (count path))
                (let [newlocs (for [#_([x y z] [[-1 0 0] [0 -1 0] [0 0 -1] [1 0 0] [0 1 0] [0 0 1]])
                                    [x z] [[-1 0] [0 -1] [1 0] [0 1]]
                                    :let [y 0]
                                    :let [l (.add (.clone loc) x y z)]
                                    :when (and
                                            (water? (.getType (.getBlock l)))
                                            (= Material/GLASS (.getType (.getBlock (.add (.clone l) 0 -1 0)))))
                                    :when (every? #(not= l %) path)]
                                l)]
                  (if-let [newloc (first newlocs)]
                    (recur newloc (cons newloc path))
                    [loc (count path)]))
                [loc (count path)]))]
        (when (not= target-loc loc0)
          (swap! water-tubing conj player)
          (loc/play-sound loc0 s/wither-spawn 1.0 0.0)
          (illusion player (* 0.5 len) m/portal m/pumpkin (fn []
            (swap! water-tubing disj player)
              (later 0
              (.teleport player target-loc)
              (let [c (.getChunk loc0)]
                (doseq [x (range -1 2)
                        z (range -1 2)]
                  (.refreshChunk (.getWorld c) (+ x (.getX c)) (+ z (.getZ c))))))
                                                    )))))))

(def sneak-players (atom {}))
(defn player-toggle-sneak-event [evt]
  (when (.isSneaking evt)
    (let [player (.getPlayer evt)]
      (when (not (.isOnGround player))
        (when-let [painting (first
                              (filter #(instance? Painting %)
                                      (.getNearbyEntities player 0.5 0.5 0.5)))]
          (.sendMessage player (str (.getArt painting)))
          (let [loc-to
                (case (.getName (.getWorld player))
                  "world" (.getSpawnLocation (Bukkit/getWorld "world_supermomonga"))
                  mozukusoba-house)]
            (.load (.getChunk loc-to))
            (.teleport player loc-to))))
      (swap! sneak-players update-in [player] (fnil inc 0))
      (future
        (Thread/sleep 2000)
        (swap! sneak-players update-in [player] dec))
      (when (= 5 (@sneak-players player))
        (.setFallDistance player 0.0)
        (.setVelocity player (doto (.getVelocity player) (.setY 1.4))))
      (when-let [vehicle (.getVehicle player)]
        (when (instance? Boat vehicle)
          (.setVelocity vehicle (doto (.getVelocity vehicle) (.setY 0.8)))))
      (water-tube player))))

(def colors-i (atom 0))
(def colors
  [Color/AQUA Color/BLACK Color/BLUE Color/FUCHSIA Color/GRAY Color/GREEN
   Color/LIME Color/MAROON Color/NAVY Color/OLIVE Color/ORANGE Color/PURPLE
   Color/RED Color/SILVER Color/TEAL Color/WHITE Color/YELLOW])

(defn ujm-walk []
  #_(let [b (.getBlock (.add (.getLocation (ujm)) 0 0 -1))]
    (when (= m/air (.getType b))
      (.setType b m/step)
      (.setData b 4)))
  #_(let [b (.getBlock (.add (.getLocation (ujm)) 0 -1 0))]
    (when (#{m/grass} (.getType b))
      (.setType b m/dirt)
      #_(.setData b 12)))
  #_(doseq [y (range 0 2)
          :let [b (.getBlock (.add (.getLocation (ujm)) 0 y 1))]
          :when (#{m/stone m/coal m/dirt m/gravel m/iron-ore} (.getType b))]
    (.breakNaturally b (ItemStack. m/diamond-pickaxe 1)))
  #_(doseq [x (range -2 3) y (range -1 1) z (range -2 3)
          :let [b (.getBlock (.add (.getLocation (ujm)) x y z))]]
    (when (#{m/dirt m/stone m/grass m/coal-ore m/gravel m/sand} (.getType b))
      (.setType b m/wool)
      (.setData b 8)))
  #_(doseq [[x y z] [[-1 0 0] [1 0 0] [0 -1 0] [0 1 0] [0 0 -1] [0 0 1]]
          :let [b (.getBlock (.add (.getLocation (ujm)) x y z))]]
    (when (#{Material/STONE Material/COAL_ORE Material/IRON_ORE} (.getType b))
      (future
        (Thread/sleep 100)
        (later 0 (.setType b Material/AIR)))))
  #_(when (.isSprinting (ujm))
    (doseq [[x z] [[-1 0] [1 0] [0 -1] [0 1]]
            :let [b (.getBlock (.add (.getLocation (ujm)) x -1 z))]
            :when (= Material/SMOOTH_BRICK (.getType b))]
      (future
        (Thread/sleep 1000)
        (later 0 (.setType b Material/WATER)))))
  #_(let [b (.getBlock (.getLocation (ujm)))]
    (when (= Material/AIR (.getType b))
      (.setType b Material/VINE)))
  #_(when (.isSprinting (ujm))
    (let [b (.getBlock (.getLocation (ujm)))]
      (future
        (Thread/sleep 2000)
        (later 0 (.setType b Material/FENCE)))))
  #_(when (.isSprinting (ujm))
    (let [loc1 (.add (.getLocation (ujm)) 0 -1 0)
          loc2 (.add (.getLocation (ujm)) 0 -2 0)]
      (.setType (.getBlock loc1) Material/WATER)
      (.setType (.getBlock loc2) Material/GLASS)))
  #_(doseq [x (range -2 3)
          y (range 0 6)
          z (range -2 3)
          :let [b (.getBlock (.add (.getLocation (ujm)) x y z))]
          #_(:when (.isLiquid b))]
    (.setType b Material/AIR #_(.getType (.getItemInHand (ujm))))
    #_(.setData b (.getData (.getData (.getItemInHand (ujm)))))))

(defn player-move-event [evt]
  (let [player (.getPlayer evt)]
    (when (= (ujm) player)
      (ujm-walk))
    #_(when (= (mozukusoba) (.getPlayer evt))
      (when (.isSprinting (mozukusoba))
        (doseq [#_([x y z] [[-1 0 0] [1 0 0] [0 -1 0] [0 1 0] [0 0 -1] [0 0 1]])
                x (range -2 3)
                y (range 0 4)
                z (range -2 3)
                :let [b (.getBlock (.add (.getLocation (mozukusoba)) x y z))]]
          (when (#{Material/DIRT Material/STONE Material/COAL_ORE Material/IRON_ORE} (.getType b))
            (.setType b Material/AIR)#_(future
              (Thread/sleep 200)
              (later 0 (.setType b Material/AIR)))))))
    (when (= Material/TORCH (.getType (.getItemInHand player)))
      (doseq [x (range 0 1)
              z (range 0 1)
              :let [b (.getBlock (.add (.getLocation player) x 0 z))]
              :when (and
                      (= Material/AIR (.getType (.getBlock (.getLocation player))))
                      (#{Material/GRASS Material/STONE Material/SAND} (.getType (.getBlock (.add (.getLocation player) x -1 z))))
                      (> 8 (.getLightLevel b)))]
        (consume-item player)
        (.setType b Material/TORCH)))
    (if (and
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
        (.setVelocity player (Vector. 0 5 0)))
      (when (= "world" (.getName (.getWorld player)))
        (let [to (.getTo evt)
            down (.add (.clone to) 0 -1 0)]
        (when (and
                (not (.isFlying player))
                (= Material/SAND (.getType (.getBlock down)))
                (= Material/AIR (.getType (.getBlock to)))
                (< 0.1 (let [x (Math/abs (.getX to))]
                         (- x (int x))) 0.9)
                (< (let [y (Math/abs (.getY to))]
                     (- y (int y))) 0.1)
                (< 0.1 (let [z (Math/abs (.getZ to))]
                         (- z (int z))) 0.9))
          (.setTo evt (.add (.clone to) 0 -1 0))))))))

(def chestplates
  #{Material/LEATHER_CHESTPLATE
    Material/IRON_CHESTPLATE
    Material/CHAINMAIL_CHESTPLATE
    Material/GOLD_CHESTPLATE
    Material/DIAMOND_CHESTPLATE})

(defn entity-death-event [evt]
  (let [entity (.getEntity evt)
        killer (.getKiller entity)]
    (condp instance? entity
      Zombie
      (when (instance? Player killer)
        (let [helmet (.getHelmet (.getEquipment entity))
              weapon (.getItemInHand (.getEquipment entity))]
          (when (= 0 (rand-int 2))
            (loc/drop-item (.getLocation entity) (ItemStack. m/torch (inc (rand-int 3)))))
          (when (= Material/TNT (.getType helmet))
            (.createExplosion (.getWorld entity) (.getLocation entity) 0.1 false))
            (when (= Material/BOW (.getType weapon))
              (.setDroppedExp evt (+ 10 (.getDroppedExp evt))))))
      Giant
      (.setDroppedExp evt 100)
      Player
      (let [inv (.getInventory entity)]
        (when-let [helmet (.getHelmet inv)]
          (when (= m/tnt (.getType helmet))
            (.setHelmet inv nil)
            (loc/spawn (.getLocation entity) TNTPrimed))))
      nil)))

(defn player-login-event [evt]
  (let [player (.getPlayer evt)]
    (future
      (let [pname (.getDisplayName player)]
        (swap! magic-point assoc pname (get @magic-point pname mp-max))
        (tweet-mc68 (format "%s logged in" pname))))))

(defn async-player-chat-event [evt]
  (let [player (.getPlayer evt)
        pname (.getName (.getPlayer evt))
        msg (.getMessage evt)]
    (when (< 1 (count msg))
      (future
        (tweet-mc68 (format "<%s>: %s" pname msg))
        (clj-http.client/post "http://lingr.com/api/room/say?app_key=rCTrCc" {:form-params {:room "computer_science" :text (format "#_%s %s" pname msg) :session "xB0GjR"}})))
    #_(when (and
            (= "glass-helmet")
            (= m/glass (.getType (.getItemInHand player)))
            (nil? (.getHelmet (.getInventory player))))
      (consume-item player)
      (.setHelmet (.getInventory player) (ItemStack. m/glass 1)))
    #_(when-let [msg (second (first (re-seq #"^t:\s(.*)" (.getMessage evt))))]
      (future
        (tweet-mc68 (format "<%s>: %s" pname msg))))))

(defn dynmap-web-chat-event [evt]
  (let [msg (.getMessage evt)]
    (when-let [[_ xs _ zs] (re-matches #"\{\"x\":([^,]+),\"y\":([^,]+),\"z\":([^,]+)}" msg)]
      (let [x (Double/parseDouble xs)
            z (Double/parseDouble zs)
            loc (Location. (Bukkit/getWorld "world") x 71 z)]
        (later 3
               (.strikeLightningEffect (.getWorld loc) loc)))
      (.setCancelled evt true))))

(def ml-insts
  '#{me you message accelerate into heal up down right left hp delay teleport})

(defn ml-valid? [tokens]
  (every? (fn [t]
            (or (ml-insts t)
                (string? t)
                (number? t)))
          tokens))

(defn ml-push [item stack]
  (ref-set stack (cons item @stack)))

(defn ml-pop [stack]
  (let [[x & xs] @stack]
    (ref-set stack xs)
    x))

(defn ml-using-mp [player mp f]
  (let [new-mp (- (@magic-point (.getDisplayName player)) mp)]
    (if (or
          (@creative (.getDisplayName player))
          (< 0 new-mp))
      (do
        (swap! magic-point assoc (.getDisplayName player) new-mp)
        (f))
      (.sendMessage player "No MP left!"))))

(defn ml-eval [player target stack tokens]
  (let [[token & tokens] tokens]
    (when token
      (if (or (string? token) (number? token))
        (dosync
          (ml-push token stack)
          (ml-eval player target stack tokens))
        (condp = token
          'you (dosync
                 (ml-push target stack)
                 (ml-eval player target stack tokens))
          'me (dosync
                (ml-push player stack)
                (ml-eval player target stack tokens))
          'message (dosync
                     (let [x1 (ml-pop stack)
                           x2 (ml-pop stack)]
                       (.sendMessage x1 (str x2))
                       (ml-eval player target stack tokens)))
          'heal (dosync
                  (let [x1 (ml-pop stack)
                        x2 (ml-pop stack)]
                    (if (< x2 0)
                      (.sendMessage player "heal has to use a positive number!")
                      (ml-using-mp player x2
                                   (fn []
                                     (.setHealth x1 (min (+ x2 (.getHealth x1)) (.getMaxHealth x1)))
                                     (ml-eval player target stack tokens))))))
          'into (dosync
                  (let [x1 (ml-pop stack)
                        x2 (ml-pop stack)
                        loc1 (if (instance? Location x1)
                               x1
                               (.getLocation x1))
                        loc2 (if (instance? Location x2)
                               x2
                               (.getLocation x2))]
                    (ml-push (.toVector (.subtract (.clone loc1) loc2))
                             stack)
                    (ml-eval player target stack tokens)))
          'accelerate (ml-using-mp
                        player 2
                        (fn []
                          (dosync
                            (let [x1 (ml-pop stack)
                                  x2 (ml-pop stack)]
                              (.setVelocity x1
                                            (let [v (.getVelocity x1)]
                                              (.add v x2)
                                              (.setX v (* 0.3 (.getX v)))
                                              (.setZ v (* 0.3 (.getZ v)))
                                              (.setY v (min 0.75 (.getY v))))))
                            (ml-eval player target stack tokens))))
          'teleport (ml-using-mp
                      player 3
                      (fn []
                        (dosync
                          (let [x1 (ml-pop stack)
                                x2 (ml-pop stack)
                                loc (if (instance? Location x2)
                                      x2
                                      (.getLocation x2))]
                            (if (.isSolid (.getType (.getBlock loc)))
                              (.sendMessage player "The destination block is solid.")
                              (.teleport x1 loc))))))
          'up (dosync
                (let [x (ml-pop stack)
                      loc (if (instance? Location x)
                            x
                            (.getLocation x))]
                  (ml-push (.add (.clone loc) 0 1 0) stack)
                  (ml-eval player target stack tokens)))
          'down (dosync
                  (let [x (ml-pop stack)
                        loc (if (instance? Location x)
                              x
                              (.getLocation x))]
                    (ml-push (.add (.clone loc) 0 -1 0) stack)
                    (ml-eval player target stack tokens)))
          'right (dosync
                   (let [x (ml-pop stack)
                         loc (.getLocation x)
                         vect (.toVector (.subtract (.clone loc) (.getLocation player)))
                         v-x (.getX vect)
                         v-z (.getZ vect)
                         vect2 (.normalize (Vector. (- v-z) (.getY vect) v-x))]
                     (ml-push (.add (.clone loc) vect2) stack)
                     (ml-eval player target stack tokens)))
          'left (dosync
                  (let [x (ml-pop stack)
                        loc (.getLocation x)
                        vect (.toVector (.subtract (.clone loc) (.getLocation player)))
                        v-x (.getX vect)
                        v-z (.getZ vect)
                        vect2 (.normalize (Vector. v-z (.getY vect) (- v-x)))]
                    (ml-push (.add (.clone loc) vect2) stack)
                    (ml-eval player target stack tokens)))
          'delay (future
                   (try
                     (Thread/sleep (min 10000 (* 100 (dosync (ml-pop stack)))))
                     (later 0 (ml-eval player target stack tokens))
                     (catch Exception e (prn 'in-delay e))))
          'hp (dosync
                (ml-push (.getHealth (ml-pop stack)) stack)
                (ml-eval player target stack tokens))
          (prn 'must-not-happen token tokens))))))

(defn player-use-book-event [player target book]
  (let [tokens (map (fn [x] (try (read-string x) (catch Exception e nil)))
                    (clojure.string/split (clojure.string/join " " (.getPages (.getItemMeta book))) #"\s+"))
        stack (ref [])]
    (if (ml-valid? tokens)
      (later 0
        (try
          (ml-eval player target stack tokens)
          (show-current-mp player)
          (catch Exception e (prn e))))
      (prn 'invalid-tokens tokens))))

(defn zombie-chestplate-break-handler [evt entity attacker]
  (when (and
          (instance? Zombie entity)
          (not (instance? PigZombie entity))
          (chestplates (.getType (.getChestplate (.getEquipment entity)))))
    (when (<= (.getHealth entity) (.getDamage evt))
      (.setCancelled evt true)
      (let [new-entity (.spawn (.getWorld entity) (.getLocation entity) Zombie)]
        (.setArmorContents (.getEquipment new-entity) (.getArmorContents (.getEquipment entity)))
        (.setChestplate (.getEquipment new-entity) nil)
        (.addPotionEffect new-entity (PotionEffect. PotionEffectType/SPEED 400 2))
        (dotimes [_ (.getMaxHealth entity)]
          (.setExperience (.spawn (.getWorld entity) (.getLocation entity) ExperienceOrb)
                          1))
        (.damage new-entity 1 attacker))
      (.remove entity))))

(defn inventory-click-event [evt]
  #_(let [inventory (.getInventory evt)
        is (.getCurrentItem evt)
        slot (.getSlot evt)
        player (.getWhoClicked evt)]
    (when (not inventory)
      (prn 'omg inventory))
    (when (and
            (instance? Player player)
            (= (ujm) player)
            inventory
            (instance? CraftingInventory inventory))
      1
      (cond
        (= slot 0) (do
                     (.clear inventory)
                     (.setCursor evt (ItemStack. m/book-and-quill 1)))
        (>= 9 slot) 1#_(prn slot player (.getType is) (.getHolder inventory))))))

(def entity-damage-event-registered (atom []))
(defmacro cons-entity-damage-event [& exps]
  `(swap! entity-damage-event-registered #(cons (fn ~@exps) %)))
(defmacro conj-entity-damage-event [& exps]
  `(swap! entity-damage-event-registered conj (fn ~@exps)))

(conj-entity-damage-event [evt]
  'gold-block-no-falling-damage
  (let [entity (.getEntity evt)
        loc (.getLocation entity)]
    (when (and (instance? LivingEntity entity)
             (= m/gold-block
                (.getType (.getBlock (.add (.clone loc) 0.0 -1.0 0.0)))))
      (.setDamage evt 0)
      (let [loc (.getLocation entity)]
        (later 0
          (.teleport entity loc)
          (.setVelocity entity (Vector. 0 0 0)))
        (when-let [attacker (try (.getDamager evt) (catch Exception e nil))]
          (when (instance? LivingEntity attacker)
            #_(loc/play-sound (.getLocation attacker) s/door-close 1.0 2.0)
            (loc/play-sound (.getLocation (ujm)) s/piston-extend 1.0 2.0)
            (let [velo (.normalize (.toVector
                                     (.subtract (.getLocation attacker)
                                                (.clone loc)))) ]
              (.setY velo 0.2)
              (later 0 (.setVelocity attacker velo))))))
      :stop)))

(conj-entity-damage-event [evt]
  'other-stuff
  (let [entity (.getEntity evt)
        loc (.getLocation entity)]
    (condp = (.getCause evt)
      #_(EntityDamageEvent$DamageCause/CONTACT)
      #_(when (and
                (instance? Villager entity)
                (= 0 (.getHealth entity)))
        (c/broadcast "A villager silently died on a cactus."))
      EntityDamageEvent$DamageCause/FALL
      (when (instance? LivingEntity entity)
        (condp instance? entity
          Spider
          (.setCancelled evt true)
          (let [block (.getBlock loc)
                block-below (.getBlock (.add (.clone loc) 0 -1 0))]
            (cond
              (= Material/GRASS (.getType block-below))
              (do
                (.setCancelled evt true)
                (.setType block-below Material/DIRT)
                (.setVelocity entity (.add (.getVelocity entity) (Vector. 0.0 0.4 0.0))))
              (= Material/BED_BLOCK (.getType block))
              (do
                (.setCancelled evt true)
                (when-not (.isSneaking entity)
                  (.setVelocity entity (doto (.getVelocity entity)
                                         (.setY 0.9)))))))))
      EntityDamageEvent$DamageCause/PROJECTILE
      (let [attacker (.getDamager evt)]
        (when-let [shooter (.getShooter attacker)]
          #_(when (and (instance? Player shooter)
                       (instance? Player entity))
            (loc/explode (.getLocation entity) 0 false))
          (when (and
                  (instance? Snowball attacker)
                  (< 0 (.getFireTicks attacker)))
            (.setFireTicks entity 200)
            (.damage entity 1 shooter)))
        (when (and
                (instance? Slime entity)
                (instance? Arrow attacker))
          (.remove attacker)
          (.setCancelled evt true))
        (when (instance? Giant entity)
          (let [weapon (if-let [shooter (.getShooter attacker)]
                         (when (instance? Player shooter)
                           (.getItemInHand shooter))
                         nil)]
            (dotimes [i (rand-nth [0 0 0 3 5])]
              (.setItemInHand (.getEquipment
                                (.spawn (.getWorld entity) (.getLocation entity) Zombie))
                              weapon)))))
      EntityDamageEvent$DamageCause/ENTITY_ATTACK
      (when-let [attacker (.getDamager evt)]
        (zombie-chestplate-break-handler evt entity attacker)
        (when (and
                (instance? Player attacker)
                (.getItemInHand attacker)
                (= Material/BOOK_AND_QUILL (.getType (.getItemInHand attacker))))
          (player-use-book-event attacker entity (.getItemInHand attacker))))
      EntityDamageEvent$DamageCause/BLOCK_EXPLOSION
      (condp instance? entity
        Minecart (.setCancelled evt true)
        Player (when (.isSneaking entity)
                 (.setCancelled evt true))
        nil)
      EntityDamageEvent$DamageCause/DROWNING
      (when (and
              (instance? Player entity)
              (.getHelmet (.getInventory entity))
              (= m/glass (.getType (.getHelmet (.getInventory entity)))))
        (.setCancelled evt true))
      nil)))

(defn entity-damage-event [evt]
  (every? #(not= (% evt) :stop) @entity-damage-event-registered))

(defn sign-change-event [evt]
  (let [lines (vec (.getLines evt))
        player (.getPlayer evt)
        sign (.getBlock evt)]
    (when (= "dynmap" (first lines))
      (prn 'new-pin player)
      (new-pin (.getLocation sign)
               (second lines)
               (clojure.string/join "\n" (rest lines))
               (.getMarkerIcon (marker-api) "small-ujm")
               false))))

(defn player-drop-item-event [evt]
  (let [player (.getPlayer evt)
        item (.getItemDrop evt)
        is (.getItemStack item)]
    (when (and
            (= -90.0 (.getPitch (.getLocation player)))
            (nil? (.getHelmet (.getInventory player)))
            (= 1 (.getAmount is)))
      (.sendMessage player (format "equipping %s" (.getType is)))
      (later 0
        (.setHelmet (.getInventory player) is)
        (.closeInventory player)
        (.remove item)))))

(defn hanging-break-by-entity-event [evt]
  (let [^Hanging hanging (.getEntity evt)
        remover (.getRemover evt)]
    (when (and
            (instance? Painting hanging)
            (instance? Player remover))
      (prn 'player-broke-paint (.getArt hanging)))))

(defn tmp-fence2wall []
  (later 0 (doseq [x (range -10 11)
                 z (range -10 11)
                 :let [loc (.add (.getLocation (ujm)) x 0 z)]
                 :when (= Material/FENCE (.getType (.getBlock loc)))
                 :let [t (rand-nth [Material/WOOL Material/LOG])]
                 y (range 0 7)
                 :let [b (.getBlock (.add (.clone loc) 0 y 0))]]
           (.setType b t))))

(defn teleport-without-angle [entity location]
  (.setYaw location (.getYaw (.getLocation entity)))
  (.setPitch location (.getPitch (.getLocation entity)))
  (.teleport entity location)
  #_(when-let [vehicle (.getVehicle entity)]
    (.teleport vehicle location)))

(defn periodically1tick []
  (doseq [arrow @special-arrows
          :let [player (.getShooter arrow)]]
    (if (.isDead arrow)
      (swap! special-arrows disj arrow)
      (let [loc (.getLocation arrow)]
        #_(swap! arrow-locs conj (.getLocation ar))
        (.scheduleSyncDelayedTask
          (Bukkit/getScheduler)
          mc68.core/plugin*
          #_(doseq [[x y z] [[0 0 0] [0 0 1] [0 0 -1] [0 1 0] [0 -1 0] [1 0 0] [-1 0 0]]
                   :let [b (.getBlock (.add (.clone loc) x (- y 2) z))]
                   :when (= Material/AIR (.getType b))]
             (.setType b Material/SMOOTH_BRICK))
          #(teleport-without-angle player loc)
          5)))))

(defn leather-colored [material color]
  "material is like Material/LEATHER_CHESTPLATE,
   color is like Color/WHITE"
  (let [is (ItemStack. material 1)]
    (.setItemMeta is (doto (.getItemMeta is)
                       (.setColor color)))
    is))

(defn periodically1sec []
  #_(.launchProjectile (momonga) Arrow)
  (when-let [player (ujm)]
    (when-let [armor (.getChestplate (.getInventory player))]
      (when (= Material/LEATHER_CHESTPLATE (.getType armor))
        (.setItemMeta
          armor
          (doto (.getItemMeta armor)
            (.setColor (get colors
                            (swap! colors-i #(rem (inc %) (count colors)))))))
        (.setChestplate (.getInventory player) armor))))
  (when (= 0 (rand-int 0))
    (doseq [zombie (.getEntitiesByClass (Bukkit/getWorld "world") Zombie)
            :when (= Material/BOW (.getType (.getItemInHand (.getEquipment zombie))))]
      (when-let [target (.getTarget zombie)]
        (when (= 0 (rand-int 3))
          (.launchProjectile zombie Arrow)))))
  (when (= 0 (rand-int 3))
    (doseq [spider (.getEntitiesByClass (Bukkit/getWorld "world") Spider)
            :let [target (.getTarget spider)]
            :when target]
      (.setVelocity spider
                    (.multiply (.toVector
                                 (.subtract (.add (.getLocation target) 0 2 0) (.getLocation spider)))
                               0.2))
      (.playEffect (.getWorld spider) (.getLocation spider) Effect/ENDER_SIGNAL nil)
      (.playSound (.getWorld spider) (.getLocation spider) s/spider-idle 1.0 0.0)))
  (when (= 0 (rand-int 3))
    (doseq [player (Bukkit/getOnlinePlayers)
            slime (filter #(instance? Slime %)
                           (.getNearbyEntities player 10 5 10))
            :when (= 0 (rand-int 2))
            :when (< 1 (.getSize slime))
            :let [vect (.normalize (.toVector
                                     (.subtract (.getLocation player)
                                                (.getLocation slime))))]
            :let [arrow (.launchProjectile slime Arrow)]]
      (.setVelocity arrow vect)
      (when (< 2 (.getSize slime))
        (later 0 (.setFireTicks arrow 20))))))

(defonce swank* nil)
(defonce t* nil)
(defn on-enable [plugin]
  (def plugin* plugin)
  (swap! creative conj "ujm")
  (when-not swank*
    (def swank* (swank.swank/start-repl 4006)))
  (.scheduleSyncRepeatingTask (Bukkit/getScheduler) plugin* #'periodically1sec 0 20)
  (def t*
    (.scheduleSyncRepeatingTask (Bukkit/getScheduler) plugin* #'periodically1tick 0 1)))

#_(defn spawn-safe [loc klass]
  (let [e (.spawn (.getWorld loc) loc klass)
        m (ControllableMobs/assign e true)]
    #_(.clearAIBehaviors m)
    #_(.addAIBehavior m (AIFloat.))
    [e m]))

#_(doseq [x (range -5 6) y (range -2 3) z (range -5 6)
          :let [loc (.add (.getLocation (ujm)) x y z) block (.getBlock loc)]
          :when (= Material/STONE (.getType block))]
  (.setType block Material/GLASS))


; memo
; 
; Bat
; Blaze
; CaveSpider
; Spider
; Chicken
; Creeper
; Enderman
; Ghast
; Giant
; IronGolem
; MagmaCube
; MushroomCow
; Cow
; Ocelot
; Pig
; PigZombie
; Zombie
; Player
; Sheep
; Silverfish
; Skeleton
; Slime
; Snowman
; Squid
; Villager
; Wolf
; 
; EnderDragon
; Witch
; Wither
; vim: set lispwords+=later :
