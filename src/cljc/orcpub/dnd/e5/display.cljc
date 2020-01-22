(ns orcpub.dnd.e5.display
  (:require [clojure.string :as s]
            [orcpub.common :as common]
            [orcpub.dnd.e5.weapons :as weapons]
            [orcpub.dnd.e5.character.equipment :as char-equip]))

(def sources
  {:phb {:abbr "PHB" }
   :vgm {:abbr "VGM" }
   :scag {:abbr "SCAG"}
   :dmg {:abbr "DMG" }
   :ee {:abbr "EE"}
   :cos {:abbr "COS"}
   :ua-revised-ranger {:abbr "UA-Revised-Ranger" }
   :ua-cleric {:abbr "UA-Cleric" }
   :ua-artificer {:abbr "UA-Artificer" }
   :ua-eberron {:abbr "UA-Eberron"}
   :ua-race-feats {:abbr "UA-Race-Feats" }
   :ua-skill-feats {:abbr "UA-Skill-Feats"}
   :ua-waterborne {:abbr "UA-Waterborne"}
   :ua-mystic {:abbr "UA-Mystic" }
   :ua-bard {:abbr "UA-Bard" }
   :ua-revised-subclasses {:abbr "UA-Revised-Subclasses" }
   :ua-trio-of-subclasses {:abbr "UA-Trio-of-Subclasses" }
   :ua-revised-class-options {:abbr "UA-Revised-Class-Options" }
   :ua-feats {:abbr "UA-Feats"}
   :ua-warlock-and-wizard {:abbr "UA-Warlock-and-Wizard"}
   :ua-fighter {:abbr "UA-Fighter"}
   :ua-sorcerer {:abbr "UA-Sorcerer" }
   :ua-gothic-heroes {:abbr "UA-Gothic-Heroes" }
   :ua-starter-spells {:abbr "UA-Starter-Spells"}})

(def plural-map
  {:feet :feet})

(defn equipment-name [equipment-map equipment-kw]
  (or (:name (equipment-map equipment-kw))
      (if (string? equipment-kw)
        equipment-kw
        (common/kw-to-name equipment-kw))))

(defn unit-amount-description [{:keys [units amount singular plural] :or {amount 1 plural (plural-map units)}}]
  (str amount " " (if (not= 1 amount)
                    (if plural
                      (common/safe-name plural)
                      (str (common/safe-name units) "s"))
                    (if singular
                      (common/safe-name singular)
                      (str (common/safe-name units))))))

(defn duration-description [{:keys [concentration] :as duration}]
  (str (if concentration "conc. ") (unit-amount-description duration)))

(defn get-source [source]
  (sources (or source :phb)))

(defn source-description [source page]
  (str "see " (:abbr (get-source source)) " " page))

(defn frequency-description [{:keys [units amount] :or {amount 1}}]
  (str
   (case amount
     1 "once"
     2 "twice"
     (str amount " times"))
   "/"
   (s/replace (common/safe-name units) #"-" " ")))

(defn attack-description [{:keys [description summary attack-type area-type damage-type damage-die damage-die-count damage-modifier attack-modifier save save-dc page source] :as attack}]
  (let [summary (or summary description)
        attack-mod-str (if attack-modifier (str (common/bonus-str attack-modifier) " to hit, "))]
    (str
     (if summary (str summary ", "))
     (case attack-type
       :area (case area-type
               :line (str (:line-width attack) " x " (:line-length attack) " ft. line, ")
               :cone (str (:length attack) " ft. cone, ")
               nil)
       :ranged (str "ranged, " attack-mod-str)
       (str "melee, " attack-mod-str))
     (or damage-die-count
         (::weapons/damage-die-count attack))
     "d"
     (or damage-die
         (::weapons/damage-die attack))
     (if damage-modifier (common/mod-str damage-modifier))
     " "
     (if damage-type (common/safe-name damage-type))
     " damage"
     (if save (str ", DC" save-dc " " (common/safe-name save) " save"))
     (if page (str " (" (source-description source page) ")")))))

(defn action-description [{:keys [description summary source page duration range frequency qualifier]}]
  (str
   (or summary description)
   (if (or range duration frequency)
     (str
      " ("
      (s/join ", "
              (remove
               nil?
               [qualifier
                (if range (str "range " (unit-amount-description range)))
                (if duration (str "lasts " (duration-description duration)))
                (if frequency (str "use " (frequency-description frequency)))
                (if page (source-description source page))]))
      ")"))))
