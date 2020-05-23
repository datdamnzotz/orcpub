(ns orcpub.dnd.e5.exports
  (:require [re-frame.core :refer [subscribe dispatch dispatch-sync]]

;TO-DO remove unused namespaces..
            [reagent.core :as r]
            [orcpub.route-map :as routes]
            [orcpub.common :as common]
            [orcpub.entity :as entity]
            [orcpub.components :as comps]
            [orcpub.entity-spec :as es]
            [orcpub.pdf-spec :as pdf-spec]
            [orcpub.dice :as dice]
            [orcpub.entity.strict :as se]
            [orcpub.dnd.e5.subs :as subs]
            [orcpub.dnd.e5.equipment-subs]
            [orcpub.dnd.e5.character :as char]
            [orcpub.dnd.e5.backgrounds :as bg]
            [orcpub.dnd.e5.languages :as langs]
            [orcpub.dnd.e5.selections :as selections]
            [orcpub.dnd.e5.races :as races]
            [orcpub.dnd.e5.classes :as classes]
            [orcpub.dnd.e5.feats :as feats]
            [orcpub.dnd.e5.units :as units]
            [orcpub.dnd.e5.party :as party]
            [orcpub.dnd.e5.character.random :as char-random]
            [orcpub.dnd.e5.character.equipment :as char-equip]
            [cljs.pprint :refer [pprint]]
            [orcpub.registration :as registration]
            [orcpub.dnd.e5 :as e5]
            [orcpub.dnd.e5.magic-items :as mi]
            [orcpub.dnd.e5.damage-types :as damage-types]
            [orcpub.dnd.e5.monsters :as monsters]
            [orcpub.dnd.e5.encounters :as encounters]
            [orcpub.dnd.e5.combat :as combat]
            [orcpub.dnd.e5.spells :as spells]
            [orcpub.dnd.e5.skills :as skills]
            [orcpub.dnd.e5.equipment :as equip]
            [orcpub.dnd.e5.weapons :as weapon]
            [orcpub.dnd.e5.armor :as armor]
            [orcpub.dnd.e5.display :as disp]
            [orcpub.dnd.e5.template :as t]
            [orcpub.dnd.e5.views-2 :as views-2]
            [orcpub.template :as template]
            [orcpub.dnd.e5.options :as opt]
            [orcpub.dnd.e5.events :as events]
            [clojure.string :as s]
            [cljs.reader :as reader]
            [orcpub.user-agent :as user-agent]
            [cljs.core.async :refer [<! timeout]]
            [bidi.bidi :as bidi]
            [camel-snake-kebab.core :as csk])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn profs-paragraph-export [profs prof-map title]
  (if (seq profs)
    (str
     title
     (s/join "; " (map (fn [p]
                         (let [prof (prof-map p)]
                           (if prof
                             (:name prof)
                             (s/capitalize (name p)))))
                       (sort profs))))))

(defn find-first [f coll]
  (first (drop-while (complement f) coll)))

(defn generateID
        ([] (generateID 19))
        ([n]
           (let [chars (map char (concat (range 48 58) (range 65 91) (range 97 123))) ; a-zA-Z0-9
                 id (take n (repeatedly #(rand-nth chars)))]
            (str "-" (reduce str id)))))

;========
; Our Standard Export - Format to be used by other applications ;]
; Stab at schema_version 1 
; (probably bugs like custom items don't come in like in the PDF export since reuse of those functions)
(defn orcpub-export-json-handler [built-char id print-character-sheet? print-spell-cards? print-prepared-spells? print-large-abilities?]
  (let [character {:schema_version 1
                   :player_name (char/player-name built-char)
                   :character [{:character_name (char/character-name built-char)
                                :race (char/race built-char)
                                :subrace (str (char/subrace built-char))
                                :alignment (char/alignment built-char)

                                :classes (char/levels built-char)

                                :experience (char/xps built-char)
                                :characteristics [{:age (char/age built-char)
                                                   :height (char/height built-char)
                                                   :weight (char/weight built-char)
                                                   :eyes (char/eyes built-char)
                                                   :skin (char/skin built-char)
                                                   :hair (char/hair built-char)
                                                   :sex (char/sex built-char)
                                                   :speed (pdf-spec/speed built-char)
                                                   :flaws (char/flaws built-char)
                                                   :personality_trait_1 (str (char/personality-trait-1 built-char))
                                                   :personality-trait-2 (str (char/personality-trait-2 built-char))
                                                   :character_backstory (char/description built-char)
                                                   :ideals (char/ideals built-char)
                                                   :bonds (char/bonds built-char)
                                                   :factions_and_organizations (char/faction-name built-char)
                                                   :faction_image_url (char/faction-image-url built-char)}]

                                :proficiency_bonus (str (es/entity-val built-char :prof-bonus))
                                :background (char/background built-char)
                                :ac @(subscribe [::char/current-armor-class id])
                                :ac-max (es/entity-val built-char :armor-class-with-armor)

                                :initiative_bonus (es/entity-val built-char :initiative)

                                :hp [{:hp_current (char/current-hit-points built-char)
                                      :hp_max (char/max-hit-points built-char)}]

                                :abilities_bonuses [{:abilities (char/ability-values built-char)
                                                     :bonuses (char/ability-bonuses built-char)}]

                                :save_bonuses (char/save-bonuses built-char)

                                :saving_throw_advantages (es/entity-val built-char :saving-throw-advantages)

                                :damage_resistances (char/damage-resistances built-char)

                                :damage_vulnerabilities (char/damage-vulnerabilities built-char)

                                :immunities (char/immunities built-char)

                                :condition_immunities (char/condition-immunities built-char)

                                :passive_perception (int (es/entity-val built-char :passive-perception))

                                :proficiencies [{:all (pdf-spec/other-profs-field built-char)
                                                 :tool (profs-paragraph-export (map first (char/tool-proficiencies built-char)) equip/tools-map "")
                                                 :weapon (profs-paragraph-export (char/weapon-proficiencies built-char) weapon/weapons-map "")
                                                 :armor (profs-paragraph-export (char/armor-proficiencies built-char) armor/armor-map "")
                                                 :language (profs-paragraph-export (char/languages built-char) @(subscribe [::langs/language-map]) "")}]

                                :treasure (pdf-spec/treasure-fields built-char)

                                :image_url (char/image-url built-char)

                                :attacks (pdf-spec/attacks-and-spellcasting-fields built-char)
                                :skills (pdf-spec/skill-fields built-char)
                                :traits (pdf-spec/traits-fields built-char)
                                
                                ; TO-DO bug here need to dig into this
                                ;:equipment (pdf-spec/equipment-fields built-char)
                                :equipment [{:equipment (es/entity-val built-char :equipment)
                                             :armor (es/entity-val built-char :armor)
                                             :magic-armor (es/entity-val built-char :magic-armor)
                                             :magic-items (es/entity-val built-char :magic-items)
                                             :weapons (sort (es/entity-val built-char :weapons))
                                             :magic-weapons (sort (es/entity-val built-char :magic-weapons))}]

                                ; TO-DO Spells. um yah... it's a nightmare to pull spells and details
                                ;:spells [{:spellcasting_fields (pdf-spec/spellcasting-fields built-char print-prepared-spells?)}]
                                :spells [{:spellcasting_fields "TO-DO Spells. um yah... it's a nightmare to pull spells and details"}]}]}]
    #(dispatch
      [::e5/save-to-json (str "(" (char/player-name built-char) ") " (char/character-name built-char) "-dmv") character])))

;========
; Foundry VTT
; Stab at schema_version 1 
; (probably bugs like custom items don't come in like in the PDF export since reuse of those functions)
(defn foundryvtt-export-json-handler [built-char id print-character-sheet? print-spell-cards? print-prepared-spells? print-large-abilities?]
  (let [save-bonuses @(subscribe [::char/save-bonuses id])
        saving-throws @(subscribe [::char/saving-throws id])
        abilities @(subscribe [::char/abilities id])
        skill-prof-bonuses (es/entity-val built-char :skill-prof-bonuses)
        skill-modifiers (es/entity-val built-char :skill-bonuses)
        skill-profs (es/entity-val built-char :skill-profs)
        skill-expertise @(subscribe [::char/skill-expertise id])

       ;Character sheet only supports 1 class, so pull the first one.
        class-key (key (first (char/levels built-char)))
        class (select-keys (char/levels built-char) [class-key])
        class-name (get-in class [class-key :class-name])
        class-level (get-in class [class-key :class-level])
        class-subclass-name (get-in class [class-key :subclass-name])

        ;Begin Json 
        character {:_id (generateID 16)
                   :name (str "(" (char/player-name built-char) ") " (char/character-name built-char))
                   :type "character"
                   :data {:abilities
                          {:str {:value (::char/str @(subscribe [::char/abilities id]))
                                 :proficient (if (saving-throws ::char/str) 1 0)
                                 :mod (es/entity-val built-char :str-mod)
                                 :prof (es/entity-val built-char :prof-bonus)
                                 :saveBonus 0
                                 :checkBonus 0
                                 :save (::char/str @(subscribe [::char/save-bonuses id]))}
                           :dex {:value (::char/dex @(subscribe [::char/abilities id]))
                                 :proficient (if (saving-throws ::char/dex) 1 0)
                                 :mod (es/entity-val built-char :dex-mod)
                                 :prof (es/entity-val built-char :prof-bonus)
                                 :saveBonus 0
                                 :checkBonus 0
                                 :save (::char/dex @(subscribe [::char/save-bonuses id]))}
                           :con {:value (::char/con @(subscribe [::char/abilities id]))
                                 :proficient (if (saving-throws ::char/con) 1 0)
                                 :mod (es/entity-val built-char :con-mod)
                                 :prof (es/entity-val built-char :prof-bonus)
                                 :saveBonus 0
                                 :checkBonus 0
                                 :save (::char/con @(subscribe [::char/save-bonuses id]))}
                           :int {:value (::char/int @(subscribe [::char/abilities id]))
                                 :proficient (if (saving-throws ::char/int) 1 0)
                                 :mod (es/entity-val built-char :int-mod)
                                 :prof (es/entity-val built-char :prof-bonus)
                                 :saveBonus 0
                                 :checkBonus 0
                                 :save (::char/int @(subscribe [::char/save-bonuses id]))}
                           :wis {:value (::char/wis @(subscribe [::char/abilities id]))
                                 :proficient (if (saving-throws ::char/wis) 1 0)
                                 :mod (es/entity-val built-char :wis-mod)
                                 :prof (es/entity-val built-char :prof-bonus)
                                 :saveBonus 0
                                 :checkBonus 0
                                 :save (::char/wis @(subscribe [::char/save-bonuses id]))}
                           :cha {:value (::char/cha @(subscribe [::char/abilities id]))
                                 :proficient (if (saving-throws ::char/cha) 1 0)
                                 :mod (es/entity-val built-char :cha-mod)
                                 :prof (es/entity-val built-char :prof-bonus)
                                 :saveBonus 0
                                 :checkBonus 0
                                 :save (::char/cha @(subscribe [::char/save-bonuses id]))}}
                          :attrbutes {:ac {:value (str @(subscribe [::char/armor-class id]))
                                           :min 0}
                                      :hp {:value (char/current-hit-points built-char)
                                           :min 0
                                           :max (char/max-hit-points built-char)
                                           :temp ""
                                           :tempmax ""}
                                      :init {:value 0
                                             :bonus 0
                                             :mod (es/entity-val built-char :initiative)
                                             :prof 0
                                             :total (es/entity-val built-char :initiative)}
                                      :speed {:value (str (pdf-spec/speed built-char) " ft.")}
                                      :prof (es/entity-val built-char :prof-bonus)
                                      :hd 0; to-do (::hit-die (levels)) ;:levels {:wizard {:class-name "Wizard", :class-level 1, :hit-die 6}},
                                      :spelldc 0; to-do
                                      }
                          :details {:alignment (char/alignment built-char)
                                    :biography {:value ""
                                                :public ""}
                                    :race (str (char/race built-char) (if (char/subrace built-char) (str " (" (char/subrace built-char) ")")))
                                    :background (char/background built-char)
                                    :xp {:value (char/xps built-char)
                                         :min 0}
                                    :trait (str (char/personality-trait-1 built-char) "<br><br>" (char/personality-trait-2 built-char))
                                    :ideal (char/ideals built-char)
                                    :bond (char/bonds built-char)
                                    :flaw (char/flaws built-char)
                                    :level class-level}
                          :skills {:acr {:value (if (skill-profs :acrobatics) 1 0)
                                         :ability "dex"
                                         :bonus (:acrobatics skill-prof-bonuses)
                                         :mod (:acrobatics skill-modifiers)
                                         :prof (if (skill-profs :acrobatics) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:acrobatics skill-prof-bonuses) (:acrobatics skill-modifiers) (if (skill-profs :dexterity) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :ani {:value (if (skill-profs :animal-handling) 1 0)
                                         :ability "wis"
                                         :bonus (:animal-handling skill-prof-bonuses)
                                         :mod (:animal-handling skill-modifiers)
                                         :prof (if (skill-profs :wisdom) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:animal-handling skill-prof-bonuses) (:animal-handling skill-modifiers) (if (skill-profs :animal-handling) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :arc {:value (if (skill-profs :arcana) 1 0)
                                         :ability "int"
                                         :bonus (:arcana skill-prof-bonuses)
                                         :mod (:arcana skill-modifiers)
                                         :prof (if (skill-profs :arcana) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:arcana skill-prof-bonuses) (:arcana skill-modifiers) (if (skill-profs :arcana) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :ath {:value (if (skill-profs :athletics) 1 0)
                                         :ability "str"
                                         :bonus (:athletics skill-prof-bonuses)
                                         :mod (:athletics skill-modifiers)
                                         :prof (if (skill-profs :athletics) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:athletics skill-prof-bonuses) (:athletics skill-modifiers) (if (skill-profs :athletics) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :dec {:value (if (skill-profs :deception) 1 0)
                                         :ability "cha"
                                         :bonus (:deception skill-prof-bonuses)
                                         :mod (:deception skill-modifiers)
                                         :prof (if (skill-profs :deception) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:deception skill-prof-bonuses) (:deception skill-modifiers) (if (skill-profs :deception) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :his {:value (if (skill-profs :history) 1 0)
                                         :ability "int"
                                         :bonus (:history skill-prof-bonuses)
                                         :mod (:history skill-modifiers)
                                         :prof (if (skill-profs :history) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:history skill-prof-bonuses) (:history skill-modifiers) (if (skill-profs :history) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :ins {:value (if (skill-profs :insight) 1 0)
                                         :ability "wis"
                                         :bonus (:insight skill-prof-bonuses)
                                         :mod (:insightv skill-modifiers)
                                         :prof (if (skill-profs :insight) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:insight skill-prof-bonuses) (:insight skill-modifiers) (if (skill-profs :insight) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :itm {:value (if (skill-profs :intimidation) 1 0)
                                         :ability "cha"
                                         :bonus (:intimidation skill-prof-bonuses)
                                         :mod (:intimidation skill-modifiers)
                                         :prof (if (skill-profs :intimidation) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:intimidation skill-prof-bonuses) (:intimidation skill-modifiers) (if (skill-profs :intimidation) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :inv {:value (if (skill-profs :investigation) 1 0)
                                         :ability "int"
                                         :bonus (:investigation skill-prof-bonuses)
                                         :mod (:investigation skill-modifiers)
                                         :prof (if (skill-profs :investigation) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:investigation skill-prof-bonuses) (:investigation skill-modifiers) (if (skill-profs :investigation) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :med {:value (if (skill-profs :medicine) 1 0)
                                         :ability "wis"
                                         :bonus (:medicine skill-prof-bonuses)
                                         :mod (:medicine skill-modifiers)
                                         :prof (if (skill-profs :medicine) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:medicine skill-prof-bonuses) (:medicine skill-modifiers) (if (skill-profs :medicine) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :nat {:value (if (skill-profs :nature) 1 0)
                                         :ability "wis"
                                         :bonus (:nature skill-prof-bonuses)
                                         :mod (:nature skill-modifiers)
                                         :prof (if (skill-profs :nature) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:nature skill-prof-bonuses) (:nature skill-modifiers) (if (skill-profs :nature) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :per {:value (if (skill-profs :perception) 1 0)
                                         :ability "cha"
                                         :bonus (:perception skill-prof-bonuses)
                                         :mod (:perception skill-modifiers)
                                         :prof (if (skill-profs :perception) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:perception skill-prof-bonuses) (:perception skill-modifiers) (if (skill-profs :perception) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :prf {:value (if (skill-profs :performance) 1 0)
                                         :ability "cha"
                                         :bonus (:performance skill-prof-bonuses)
                                         :mod (:performance skill-modifiers)
                                         :prof (if (skill-profs :performance) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:performance skill-prof-bonuses) (:performance skill-modifiers) (if (skill-profs :performance) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :rel {:value (if (skill-profs :religion) 1 0)
                                         :ability "int"
                                         :bonus (:religion skill-prof-bonuses)
                                         :mod (:religion skill-modifiers)
                                         :prof (if (skill-profs :religion) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:religion skill-prof-bonuses) (:religion skill-modifiers) (if (skill-profs :religion) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :slt {:value (if (skill-profs :sleight-of-hand) 1 0)
                                         :ability "dex"
                                         :bonus (:sleight-of-hand skill-prof-bonuses)
                                         :mod (:sleight-of-hand skill-modifiers)
                                         :prof (if (skill-profs :sleight-of-hand) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:sleight-of-hand skill-prof-bonuses) (:sleight-of-hand skill-modifiers) (if (skill-profs :sleight-of-hand) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :ste {:value (if (skill-profs :stealth) 1 0)
                                         :ability "dex"
                                         :bonus (:stealth skill-prof-bonuses)
                                         :mod (:stealth skill-modifiers)
                                         :prof (if (skill-profs :stealth) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:stealth skill-prof-bonuses) (:stealth skill-modifiers) (if (skill-profs :stealth) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}

                                   :sur {:value (if (skill-profs :survival) 1 0)
                                         :ability "wis"
                                         :bonus (:survival skill-prof-bonuses)
                                         :mod (:survival skill-modifiers)
                                         :prof (if (skill-profs :survival) (es/entity-val built-char :prof-bonus) 0)
                                         :total (+ (:survival skill-prof-bonuses) (:survival skill-modifiers) (if (skill-profs :survival) (es/entity-val built-char :prof-bonus) 0))
                                         :passive (int (es/entity-val built-char :passive-perception))}}
                          :traits {:size "med" ;to-do grab :size and trim ? need to find out valid values here...
                                   :senses (if (es/entity-val built-char :darkvision) "Darkvision" "")
                                   :languages {:value @(subscribe [::char/languages id])}
                                   :di {:value @(subscribe [::char/damage-immunities id])}
                                   ;:dr {:value (if @(subscribe [::char/damage-resistances id]) @(subscribe [::char/damage-resistances id]) "")  }
                                   :dv {:value @(subscribe [::char/damage-vulnerabilities id])}
                                   :weaponProf {:value [@(subscribe [::char/weapon-profs id])]}
                                   :armorProf {:value [@(subscribe [::char/armor-profs id])]}
                                   :toolProf {:value [@(subscribe [::char/tool-profs id])]}}}

                           ;:abilities_bonuses [{:abilities (char/ability-values built-char)
                           ;                     :bonuses (char/ability-bonuses built-char)}]

                           ;:character_name (char/character-name built-char)
                           ;:race (char/race built-char)
                           ;:subrace (str (char/subrace built-char))
                           ;:alignment (char/alignment built-char)

                           ;:classes (char/levels built-char)

                           ;:experience (char/xps built-char)
                           ;:characteristics [{:age (char/age built-char)
                           ;                   :height (char/height built-char)
                           ;                   :weight (char/weight built-char)
                           ;                   :eyes (char/eyes built-char)
                           ;                   :skin (char/skin built-char)
                           ;                   :hair (char/hair built-char)
                           ;                   :sex (char/sex built-char)
                           ;                   :speed (pdf-spec/speed built-char)
                           ;                   :flaws (char/flaws built-char)
                           ;                   :personality_trait_1 (str (char/personality-trait-1 built-char))
                           ;                   :personality-trait-2 (str (char/personality-trait-2 built-char))
                           ;                   :character_backstory (char/description built-char)
                           ;                   :ideals (char/ideals built-char)
                           ;                   :bonds (char/bonds built-char)
                           ;                   :factions_and_organizations (char/faction-name built-char)
                           ;                   :faction_image_url (char/faction-image-url built-char)}]

                           ;:proficiency_bonus (str (es/entity-val built-char :prof-bonus))
                           ;:background (char/background built-char)
                           ;:ac @(subscribe [::char/current-armor-class id])
                           ;:ac-max (es/entity-val built-char :armor-class-with-armor)

                           ;:initiative_bonus (es/entity-val built-char :initiative)

                           ;:hp [{:hp_current (char/current-hit-points built-char)
                           ;      :hp_max (char/max-hit-points built-char)}]

                           ;:abilities_bonuses [{:abilities (char/ability-values built-char)
                           ;                     :bonuses (char/ability-bonuses built-char)}]

                           ;:save_bonuses (char/save-bonuses built-char)

                           ;:saving_throw_advantages (es/entity-val built-char :saving-throw-advantages)

                           ;:damage_resistances (char/damage-resistances built-char)

                           ;:damage_vulnerabilities (char/damage-vulnerabilities built-char)

                           ;:immunities (char/immunities built-char)

                           ;:condition_immunities (char/condition-immunities built-char)

                           ;:passive_perception (int (es/entity-val built-char :passive-perception))

                           ;:proficiencies [{:all (pdf-spec/other-profs-field built-char)
                           ;                 :tool (profs-paragraph-export (map first (char/tool-proficiencies built-char)) equip/tools-map "")
                           ;                 :weapon (profs-paragraph-export (char/weapon-proficiencies built-char) weapon/weapons-map "")
                           ;                 :armor (profs-paragraph-export (char/armor-proficiencies built-char) armor/armor-map "")
                           ;                 :language (profs-paragraph-export (char/languages built-char) @(subscribe [::langs/language-map]) "")}]

                           ;:treasure (pdf-spec/treasure-fields built-char)

                           ;:image_url (char/image-url built-char)

                           ;:attacks (pdf-spec/attacks-and-spellcasting-fields built-char)
                           ;:skills (pdf-spec/skill-fields built-char)
                           ;:traits (pdf-spec/traits-fields built-char)

                                ; TO-DO bug here need to dig into this
                                ;:equipment (pdf-spec/equipment-fields built-char)
                           ;:equipment [{:equipment (es/entity-val built-char :equipment)
                           ;             :armor (es/entity-val built-char :armor)
                           ;             :magic-armor (es/entity-val built-char :magic-armor)
                           ;             :magic-items (es/entity-val built-char :magic-items)
                           ;             :weapons (sort (es/entity-val built-char :weapons))
                           ;             :magic-weapons (sort (es/entity-val built-char :magic-weapons))}]

                                ; TO-DO Spells. um yah... it's a nightmare to pull spells and details
                                ;:spells [{:spellcasting_fields (pdf-spec/spellcasting-fields built-char print-prepared-spells?)}]
;                           :spells [{:spellcasting_fields "TO-DO Spells. um yah... it's a nightmare to pull spells and details"}]
                   }]
    #(dispatch
      [::e5/save-to-json (str "(" (char/player-name built-char) ") " (char/character-name built-char) "-dmv") character])))


;========
; TO-DO
; DDB export format?


;========
; D&D 5E by Roll20 - Single class sheet export example
; https://roll20.zendesk.com/hc/en-us/articles/360037773573
;
; This is attempt at importing characters using the VTT Enhancement Suite
; https://ssstormy.github.io/roll20-enhancement-suite/
; Character sheet only supports 1 class, so pull the first one.
;
; Looking at https://github.com/Roll20/roll20-character-sheets/blob/master/DD5thEditionLegacy/src/js/versioning.js
; looks like the character goes through several version upgrades when loaded
; So setting the version is important. 


(defn dd5eroll20-export-json-handler [built-char id print-character-sheet?
                           print-spell-cards?
                           print-prepared-spells?
                           print-large-abilities?]
  (let [character (let [
                         ac-with-armor-fn (char/armor-class-with-armor built-char)
                         all-armor-inventory (mi/equipped-armor-details (char/all-armor-inventory built-char))
                         equipped-armor (armor/non-shields all-armor-inventory)
                         equipped-shields (armor/shields all-armor-inventory)
                         all-armor-classes (for [armor (conj equipped-armor nil)
                                                 shield (conj equipped-shields nil)]
                                             (ac-with-armor-fn armor shield))
                         max-armor-class (apply max all-armor-classes)

                         ;Character sheet only supports 1 class, so pull the first one.
                         class-key (key (first (char/levels built-char)))
                         class (select-keys (char/levels built-char) [class-key])
                         class-name (get-in class [class-key :class-name])
                         class-level (get-in class [class-key :class-level])
                         class-subclass-name (get-in class [class-key :subclass-name])

                   ; TO-DO
                   ;      current-ac @(subscribe [::char/current-armor-class id])
                   ;      levels (char/levels built-char)
                   ;      classes (char/classes built-char)
                   ;      con-mod (es/entity-val built-char :con-mod)
                   ;      total-hit-dice (->> levels
                   ;                          vals
                   ;                          (reduce
                   ;                           (fn [levels-per-die level]
                   ;                             (update levels-per-die (:hit-die level) (fnil #(+ % (:class-level level)) 0)))
                   ;                           {})
                   ;                          (sort-by key)
                   ;                          (map #(str (val %) "x(1d" (key %) "+" con-mod ")"))
                   ;                          (s/join "\n"))
                   ;      speed (pdf-spec/speed built-char)]
                         ]
                      {:schema_version 1
                       :name (str "(" (char/player-name built-char) ") " (char/character-name built-char))
                       :bio ""
                       :oldId "" ; not needed for schema 1?
                       :avatar ""
                       :gmnotes "" ; not needed for schema 1?
                       :defaulttoken "" ; not needed for schema 1?
                       :tags "" ; not needed for schema 1?
                       :controlledby "" ; not needed for schema 1
                       :inplayerjournals "all" ; not needed for schema 1
                       :attribs [{:name "l1mancer_status"
                                  :current "completed"
                                  :max ""
                                  :id (generateID)}
                                 {:name "version"
                                  :current 1.0
                                  :max  ""
                                  :id (generateID)}
                                 {:name "mancer_confirm_flag"
                                  :current ""
                                  :max ""
                                  :id (generateID)}
                                 {:name "mancer_confirm"
                                  :current "on"
                                  :max ""
                                  :id (generateID)}
                                 {:name "charactermancer_step"
                                  :current ""
                                  :max ""
                                  :id (generateID)}

                                 {:name "race"
                                  :current (str (char/race built-char))
                                  :max ""
                                  :id (generateID)}
                                 {:name "race_display"
                                  :current (str (char/subrace built-char))
                                  :max ""
                                  :id (generateID)}
                                 {:name "subrace"
                                  :current (str (char/subrace built-char))
                                  :max ""
                                  :id (generateID)}

                                 {:name "alignment"
                                  :current (char/alignment built-char)
                                  :max ""
                                  :id (generateID)}

                                 {:name "class"
                                  :current class-name :max ""
                                  :id (generateID)}

                                 {:name "class_display"
                                  :current (str class-name " " class-level)
                                  :max ""
                                  :id (generateID)}

                                 {:name "level"
                                  :current class-level
                                  :max ""
                                  :id (generateID)}

                                 {:name "base_level"
                                  :current class-level
                                  :max ""
                                  :id (generateID)}

                                 {:name "experience"
                                  :current (char/xps built-char)
                                  :max ""
                                  :id (generateID)}

                                 {:name "age" :current (char/age built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "height" :current (char/height built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "weight" :current (char/weight built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "eyes" :current (char/eyes built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "skin" :current (char/skin built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "hair" :current (char/hair built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "sex" :current (char/sex built-char)
                                  :max ""
                                  :id (generateID)}

                                 {:name "pb"
                                  :current (str (es/entity-val built-char :prof-bonus))
                                  :max ""
                                  :id (generateID)}
                                 {:name "background" :current (char/background built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "ac"
                                  :current (str @(subscribe [::char/armor-class id]))
                                  :max (str max-armor-class)
                                  :id (generateID)}

                                 {:name "hitdietype"
                                  :current "" :max ""
                                  :id (generateID)}
                                 #_{:name "hitdie_final"
                                    :current "@{hitdietype}" :max ""}
                                 #_{:name "hit_dice"
                                    :current "" :max ""}
                                 #_:hd #_(->> (char/levels built-char)
                                              vals
                                              (reduce
                                               (fn [levels-per-die level]
                                                 (update levels-per-die (:hit-die level) (fnil #(+ % (:class-level level)) 0)))
                                               {})
                                              (sort-by key)
                                              (map #(str (val %) "x(1d" (key %) "+" (es/entity-val built-char :con-mod) ")"))
                                              (s/join "\n"))
                                 {:name "initiative_bonus"
                                  :current (es/entity-val built-char :initiative)
                                  :max ""
                                  :id (generateID)}
                                 {:name "speed"
                                  :current (pdf-spec/speed built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "hp"
                                  :current  (char/current-hit-points built-char)
                                  :max (char/max-hit-points built-char)
                                  :id (generateID)}

                                 {:name "strength"
                                  :current (::char/str @(subscribe [::char/abilities]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "strength_base"
                                  :current (::char/str @(subscribe [::char/ability-scores-option-value]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "strength_mod"
                                  :current (es/entity-val built-char :str-mod)
                                  :max ""
                                  :id (generateID)}
                                 ;{:name "strength_save_prof"
                                 ; :current "(@{pb})"}

                                 {:name "dexterity"
                                  :current (::char/dex @(subscribe [::char/abilities]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "dexterity_base"
                                  :current (::char/dex @(subscribe [::char/ability-scores-option-value]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "dexterity_mod"
                                  :current (es/entity-val built-char :dex-mod)
                                  :max ""
                                  :id (generateID)}

                                 {:name "constitution"
                                  :current (::char/con @(subscribe [::char/abilities]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "constitution_base"
                                  :current (::char/con @(subscribe [::char/ability-scores-option-value]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "constitution_mod"
                                  :current (es/entity-val built-char :con-mod)
                                  :max ""
                                  :id (generateID)}
                                 ;{:name "constitution_save_prof"
                                 ; :current "(@{pb})"}

                                 {:name "intelligence"
                                  :current (::char/int @(subscribe [::char/abilities]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "intelligence_base"
                                  :current (::char/int @(subscribe [::char/ability-scores-option-value]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "intelligence_mod"
                                  :current (es/entity-val built-char :int-mod)
                                  :max ""
                                  :id (generateID)}

                                 {:name "wisdom"
                                  :current (::char/wis @(subscribe [::char/abilities]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "wisdom_base"
                                  :current (::char/wis @(subscribe [::char/ability-scores-option-value]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "wisdom_mod"
                                  :current (es/entity-val built-char :wis-mod)
                                  :max ""
                                  :id (generateID)}

                                 {:name "charisma"
                                  :current (::char/cha @(subscribe [::char/abilities]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "charisma_base"
                                  :current (::char/cha @(subscribe [::char/ability-scores-option-value]))
                                  :max ""
                                  :id (generateID)}
                                 {:name "charisma_mod"
                                  :current (es/entity-val built-char :cha-mod)
                                  :max ""
                                  :id (generateID)}

                                 {:name "passive" :current (int (es/entity-val built-char :passive-perception))
                                  :max ""
                                  :id (generateID)}

                                 {:name "flaws" :current (char/flaws built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "personality_traits" :current (str (char/personality-trait-1 built-char) "\n\n" (char/personality-trait-2 built-char))
                                  :max ""
                                  :id (generateID)}

                                 {:name "character_backstory" :current (char/description built-char)
                                  :max ""
                                  :id (generateID)}

                                 {:name "ideals" :current (char/ideals built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "bonds" :current (char/bonds built-char)
                                  :max ""
                                  :id (generateID)}

                                 {:name "allies_and_organizations" :current (char/faction-name built-char)
                                  :max ""
                                  :id (generateID)}
                                 {:name "character_backstory" :current (char/description built-char)
                                  :max ""
                                  :id (generateID)}

                                 ;To-do
                                 ;:other-proficiencies (pdf-spec/other-profs-field built-char)
                                 ;:tool-proficiencies (pdf-spec/profs-paragraph (map first (char/tool-proficiencies built-char)) equip/tools-map "Tool")
                                 ;:weapon-proficiencies (pdf-spec/profs-paragraph  (char/weapon-proficiencies built-char) weapon/weapons-map "Weapon")
                                 ;:armor-proficiencies (pdf-spec/profs-paragraph (char/armor-proficiencies built-char) armor/armor-map "Armor")

                                 ;:languages (pdf-spec/profs-paragraph (char/languages built-char) @(subscribe [::langs/language-map]) "Language")

                                 ;:treasure (pdf-spec/treasure-fields built-char)



                                 ;:image-url (char/image-url built-char)
                                 ;:image-url-failed (char/image-url-failed built-char)
                                 ;:faction-image-url (char/faction-image-url built-char)
                                 ;:faction-image-url-failed (char/faction-image-url-failed built-char)

                                 ;:attacks-and-spellcasting-fields (pdf-spec/attacks-and-spellcasting-fields built-char)
                                 ;:skill-fields (pdf-spec/skill-fields built-char)
                                 ;:abilities (char/ability-values built-char)
                                 ;:ability-bonuses (char/ability-bonuses built-char)
                                 ;:save-bonuses ((set (char/saving-throws built-char))
                                 ;               (reduce
                                 ;                (fn [saves key]
                                 ;                  (assoc saves (keyword (str (name key) "-save-check")) (boolean (key (char/saving-throws built-char)))))
                                 ;                {}
                                 ;                char/ability-keys))
                                 ;:traits-fields (pdf-spec/traits-fields built-char)
                                 ;:equipment-fields (pdf-spec/equipment-fields built-char)
                                  ;:spellcasting-fields-2 (pdf-spec/spellcasting-fields built-char print-prepared-spells?)
                                 ]
                       :abilities []})]
#(dispatch
    [::e5/save-to-json (str "(" (char/player-name built-char) ") " (char/character-name built-char) "vtt") character])))
