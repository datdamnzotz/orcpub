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
                                :save_bonuses ((set (char/saving-throws built-char))
                                               (reduce
                                                (fn [saves key]
                                                  (assoc saves (keyword (str (name key) "-save-check")) (boolean (key (char/saving-throws built-char)))))
                                                {}
                                                char/ability-keys))

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
; TO-DO
; DDB export format?


;========
; D&D 5E by Roll20 - Single class sheet export example
; https://roll20.zendesk.com/hc/en-us/articles/360037773573
;
; This is attempt at importing characters using the VTT 
; https://ssstormy.github.io/roll20-enhancement-suite/
; Character sheet only supports 1 class, so pull the first one.
;
; This really isn't functional since the Class, and Level don't import correctly, looks like Charactermancer messes it up.
; https://roll20.zendesk.com/hc/en-us/articles/360039644133-D-D-5e-Charactermancer
;
; Looking at https://github.com/Roll20/roll20-character-sheets/blob/master/DD5thEditionLegacy/src/js/versioning.js
; looks like the character goes through several version upgrades when loaded, and this version on github isn't 
; the one being run on roll 20 (it bumps to version 4.21 currently, where the above only upgrades to 2.7) 
; it's hard to see what they keep changing and why the import never sets the Level or Class. 
; *sigh*
; Leaving it here as an example.

;From Discord - when asking about the schema for the json format:
; I'll ask one of the community devs - just a sec
; (for the schemata)
; It's not currently publicly available.
; want to find a way to release it again, it's just been a problem with the current workflow and licensing agreements.
; so no. its reverse engineer or nothing

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
                       ;:oldId "" ; not needed for schema 1
                       :avatar ""
                       ;:gmnotes "" ; not needed for schema 1
                       ;:defaulttoken "" ; not needed for schema 1
                       ;:tags "" ; not needed for schema 1
                       ;:controlledby "" ; not needed for schema 1
                       ;:inplayerjournals "all" ; not needed for schema 1
                       :attribs [{:name "race"
                                  :current (str (char/race built-char))
                                  :max ""}
                                 {:name "race_display"
                                  :current (str (char/subrace built-char)) :max ""}
                                 {:name "alignment"
                                  :current (char/alignment built-char) :max ""}

                                 {:name "class"
                                  :current class-name :max ""}
                                 {:name "class_display"
                                  :current class-subclass-name :max ""}
                                 {:name "level"
                                  :current class-level :max ""}

                                 {:name "experience"
                                  :current (char/xps built-char) :max ""}

                                 {:name "age" :current (char/age built-char) :max ""}
                                 {:name "height" :current (char/height built-char) :max ""}
                                 {:name "weight" :current (char/weight built-char) :max ""}
                                 {:name "eyes" :current (char/eyes built-char) :max ""}
                                 {:name "skin" :current (char/skin built-char) :max ""}
                                 {:name "hair" :current (char/hair built-char) :max ""}
                                 {:name "sex" :current (char/sex built-char) :max ""}

                                 {:name "pb"
                                  :current (str (es/entity-val built-char :prof-bonus)) :max ""}
                                 {:name "background" :current (char/background built-char) :max ""}
                                 {:name "ac"
                                  :current (str @(subscribe [::char/armor-class id]))
                                  :max (str max-armor-class)}

                                 {:name "hitdietype"
                                  :current "" :max ""}
                                 {:name "hitdie_final"
                                  :current "@{hitdietype}" :max ""}
                                 {:name "hit_dice"
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
                                  :current (es/entity-val built-char :initiative) :max ""}
                                 {:name "speed"
                                  :current (pdf-spec/speed built-char) :max ""}
                                 {:name "hp"
                                  :current  (char/current-hit-points built-char)
                                  :max (char/max-hit-points built-char)}

                                 {:name "strength"
                                  :current (::char/str @(subscribe [::char/abilities])) :max ""}
                                 {:name "strength_base"
                                  :current (::char/str @(subscribe [::char/ability-scores-option-value])) :max ""}
                                 {:name "strength_mod"
                                  :current (es/entity-val built-char :str-mod) :max ""}
                                 ;{:name "strength_save_prof"
                                 ; :current "(@{pb})"}

                                 {:name "dexterity"
                                  :current (::char/dex @(subscribe [::char/abilities])) :max ""}
                                 {:name "dexterity_base"
                                  :current (::char/dex @(subscribe [::char/ability-scores-option-value])) :max ""}
                                 {:name "dexterity_mod"
                                  :current (es/entity-val built-char :dex-mod) :max ""}

                                 {:name "constitution"
                                  :current (::char/con @(subscribe [::char/abilities])) :max ""}
                                 {:name "constitution_base"
                                  :current (::char/con @(subscribe [::char/ability-scores-option-value])) :max ""}
                                 {:name "constitution_mod"
                                  :current (es/entity-val built-char :con-mod) :max ""}
                                 ;{:name "constitution_save_prof"
                                 ; :current "(@{pb})"}

                                 {:name "intelligence"
                                  :current (::char/int @(subscribe [::char/abilities])) :max ""}
                                 {:name "intelligence_base"
                                  :current (::char/int @(subscribe [::char/ability-scores-option-value])) :max ""}
                                 {:name "intelligence_mod"
                                  :current (es/entity-val built-char :int-mod) :max ""}

                                 {:name "wisdom"
                                  :current (::char/wis @(subscribe [::char/abilities])) :max ""}
                                 {:name "wisdom_base"
                                  :current (::char/wis @(subscribe [::char/ability-scores-option-value])) :max ""}
                                 {:name "wisdom_mod"
                                  :current (es/entity-val built-char :wis-mod) :max ""}

                                 {:name "charisma"
                                  :current (::char/cha @(subscribe [::char/abilities])) :max ""}
                                 {:name "charisma_base"
                                  :current (::char/cha @(subscribe [::char/ability-scores-option-value])) :max ""}
                                 {:name "charisma_mod"
                                  :current (es/entity-val built-char :cha-mod) :max ""}

                                 {:name "passive" :current (int (es/entity-val built-char :passive-perception)) :max ""}

                                 {:name "flaws" :current (char/flaws built-char) :max ""}
                                 {:name "personality_traits" :current (str (char/personality-trait-1 built-char) "\n\n" (char/personality-trait-2 built-char)) :max ""}

                                 {:name "character_backstory" :current (char/description built-char) :max ""}

                                 {:name "ideals" :current (char/ideals built-char) :max ""}
                                 {:name "bonds" :current (char/bonds built-char) :max ""}

                                 {:name "allies_and_organizations" :current (char/faction-name built-char) :max ""}
                                 {:name "character_backstory" :current (char/description built-char) :max ""}

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
