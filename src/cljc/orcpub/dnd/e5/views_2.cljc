(ns orcpub.dnd.e5.views-2
  (:require [orcpub.route-map :as routes]
            [clojure.string :as s]
            #?(:cljs [re-frame.core :refer [subscribe dispatch dispatch-sync]])))

(defn style [style]
  #?(:cljs style)
  #?(:clj (s/join
           "; "
           (map
            (fn [[k v]]
              (str (name k) ": " (if (keyword? v) (name v) v)))
            style))))

(defn svg-icon-2 [icon-name & [theme]]
  [:img.svg-icon
   {:src (str "/image/" icon-name ".svg")}])

(defn splash-page-button [title icon route & [handler]]
  [:a.splash-button
   (let [cfg {:style (style {:text-decoration :none
                             :color "#f0a100"})}]
     (if handler
       (assoc cfg :on-click handler)
       (assoc cfg :href (routes/path-for route))))
   [:div.splash-button-content
    {:style (style {:box-shadow "0 2px 6px 0 rgba(0, 0, 0, 0.5)"
                    :margin "5px"
                    :text-align "center"
                    :padding "10px"
                    :cursor :pointer
                    :display :flex
                    :align-items :center
                    :justify-content :space-around
                    :font-weight :bold})}
    [:div
     (svg-icon-2 icon 64 "dark")
     [:div
      [:span.splash-button-title-prefix "D&D 5e "] [:span title]]]]])

(defn splash-page-button2 [title icon route & [handler]]
  [:a.splash-button
   (let [cfg {:style (style {:text-decoration :none
                             :color "#f0a100"})}]
     (if handler
       (assoc cfg :on-click handler)
       (assoc cfg :href route)))
   [:div.splash-button-content
    {:style (style {:box-shadow "0 2px 6px 0 rgba(0, 0, 0, 0.5)"
                    :margin "5px"
                    :text-align "center"
                    :padding "10px"
                    :cursor :pointer
                    :display :flex
                    :align-items :center
                    :justify-content :space-around
                    :font-weight :bold})}
    [:div
     (svg-icon-2 icon 64 "dark")
     [:div
      [:span.splash-button-title-prefix "D&D 5e "] [:span title]]]]])

(defn legal-footer-sm []
  [:div.m-l-15.m-b-10.m-t-10.t-a-l
   ;[:span "© 2019 OrcPub"]
   [:a.m-l-5 {:href "/terms-of-use" :target :_blank} "Terms of Use"]
   [:a.m-l-5 {:href "/privacy-policy" :target :_blank} "Privacy Policy"]])

(def orange-style
  {:color :orange})

(defn legal-footer-2 []
  #_[:div
   {:style (style {:display :flex
                   :justify-content :space-between
                   :align-items :center
                   :flex-wrap :wrap
                   :padding "10px"})}
   [:div
    [:div {:style (style {:margin-bottom "5px"})} "Icons available on " [:a.orange {:href "http://game-icons.net"} "http://game-icons.net"]]]
   [:div.m-l-10 {:style (style {:margin-left "10px"})}
    [:a {:style (style orange-style) :href "https://github.com/Orcpub/orcpub/issues" :target :_blank} "Feedback/Bug Reports"]]
   [:div.m-l-10.m-r-10.p-10
    {:style (style {:margin-left "10px"
                    :margin-right "10px"
                    :padding "10px"})}
    [:a {:style (style orange-style) :href "/privacy-policy" :target :_blank} "Privacy Policy"]
    [:a.m-l-5 {:style (orange-style {:margin-left "5px"}) :href "/terms-of-use" :target :_blank} "Terms of Use"]]
   [:div.legal-footer
    [:p "Site Contact " [:a {:href "mailto:thDM@dungeonmastersvault.com"} "thDM@dungeonmastersvault.com"]]
    [:p "Wizards of the Coast, Dungeons & Dragons, D&D, and their logos are trademarks of Wizards of the Coast LLC in the United States and other countries. © Wizards. All Rights Reserved. OrcPub.com is not affiliated with, endorsed, sponsored, or specifically approved by Wizards of the Coast LLC."]]])

(defn splash-page []
  [:div.app
   {:style (style {:display :flex
                   :flex-direction :column})}
   [:div
    {:style (style
             {:display :flex
              :flex-grow 1
              :color :white
              :align-items :center
              :justify-content :space-around})}
    [:div.main-text-color.splash-page-content
     {:style (style {:font-family "sans-serif"})}
     [:div
      {:style (style {:display :flex
                      :justify-content :space-around})}
      [:img.h-72
       {:src "/image/dmv-logo.svg"}]]
     #_[:div
      {:style (style {:text-align :center
                      :text-shadow "1px 2px 1px black"
                      :font-weight :bold
                      :font-size "14px"
                      :height "48px"})}
      "orcpub version 2.5.0.1 (08/19/2019) - community edition"]
     [:div
      {:style (style
               {:display :flex
                :flex-wrap :wrap
                :justify-content :center
                :margin-top "10px"})}
      (splash-page-button
       "Character Builder / Sheet"
       "anvil-impact"
       routes/dnd-e5-char-builder-route)
      (splash-page-button
       "Character Builder for Newbs"
       "baby-face"
       routes/dnd-e5-newb-char-builder-route)
      (splash-page-button
        "Homebrew Content"
        "beer-stein"
        routes/dnd-e5-my-content-route)]
     [:div
      {:style (style
                {:display :flex
                 :flex-wrap :wrap
                 :justify-content :center
                 :margin-top "10px"})}
      (splash-page-button
       "Spells"
       "spell-book"
       routes/dnd-e5-spell-list-page-route)
      (splash-page-button
       "Monsters"
       "spiked-dragon-head"
       routes/dnd-e5-monster-list-page-route)
      (splash-page-button
       "Items"
       "all-for-one"
       routes/dnd-e5-item-list-page-route)
      (splash-page-button
       "Combat Tracker"
       "sword-clash"
       routes/dnd-e5-combat-tracker-page-route)
      ]
     [:div.m-l-20.m-r-20.f-w-b.f-s-14.container.m-b-10.main-text-color
      [:div
       [:div.content.bg-lighter.p-10.flex
        [:div.flex-grow-1
         [:div.t-a-c "Explorer's Guide to Wildemount coming March 17th!"]
         [:div.m-t-10
          [:a.m-5 {:target "_blank" :href "https://www.amazon.com/gp/product/0786966912/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0786966912&linkCode=as2&tag=dungeonmast0b-20&linkId=5e0a6e180ce3cef6a4eb0f004322d033"} [:img {:height "120px" :border "0" :src "//ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0786966912&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=dungeonmast0b-20"}]] [:img {:src "//ir-na.amazon-adsystem.com/e/ir?t=dungeonmast0b-20&l=am2&o=1&a=0786966912" :width "1" :height "1" :border "0" :alt "" :style { :border "none !important" :margin "0px !important"}}]
          [:a.m-5 {:target "_blank" :href "https://www.amazon.com/gp/product/0786966890/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0786966890&linkCode=as2&tag=dungeonmast0b-20&linkId=5310b4e05702fba3ba9bbf6531531ea3"} [:img {:height "120px" :border "0" :src "//ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0786966890&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=dungeonmast0b-20"}]] [:img {:src "//ir-na.amazon-adsystem.com/e/ir?t=dungeonmast0b-20&l=am2&o=1&a=0786966890" :width "1" :height "1" :border "0" :alt "" :style { :border "none !important" :margin "0px !important"}}]
          [:a.m-5 {:target "_blank" :href "https://www.amazon.com/gp/product/0786965606/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0786965606&linkCode=as2&tag=dungeonmast0b-20&linkId=35a7f9a44fa6b8ca84df51c39f563f02"} [:img {:height "120px" :border "0" :src "//ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0786965606&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=dungeonmast0b-20"}]] [:img {:src "//ir-na.amazon-adsystem.com/e/ir?t=dungeonmast0b-20&l=am2&o=1&a=0786965606" :width "1" :height "1" :border "0" :alt "" :style { :border "none !important" :margin "0px !important"}}]
          [:a.m-5 {:target "_blank" :href "https://www.amazon.com/gp/product/0786965622/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0786965622&linkCode=as2&tag=dungeonmast0b-20&linkId=ac4fc31252bb7c102f3752f24dce711b"} [:img {:height "120px" :border "0" :src "//ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0786965622&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=dungeonmast0b-20"}]] [:img {:src "//ir-na.amazon-adsystem.com/e/ir?t=dungeonmast0b-20&l=am2&o=1&a=0786965622" :width "1" :height "1" :border "0" :alt "" :style { :border "none !important" :margin "0px !important"}}]
          [:a.m-5 {:target "_blank" :href "https://www.amazon.com/gp/product/0786965614/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0786965614&linkCode=as2&tag=dungeonmast0b-20&linkId=711d30a7022ed50f25948f3afda1b5cd"} [:img {:height "120px" :border "0" :src "//ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0786965614&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=dungeonmast0b-20"}]] [:img {:src "//ir-na.amazon-adsystem.com/e/ir?t=dungeonmast0b-20&l=am2&o=1&a=0786965614" :width "1" :height "1" :border "0" :alt "" :style { :border "none !important" :margin "0px !important"}}]
          ]
         ]
        ;;[:i.fa.fa-times.p-10.pointer
        ;;{:on-click #(dispatch [:close-srd-message])}]
        ]]]
     [:div
      {:style (style
                {:display :flex
                 :flex-wrap :wrap
                 :justify-content :center
                 :margin-top "10px"})}
      (splash-page-button
       "Encounter Builder"
       "minions"
       routes/dnd-e5-encounter-builder-page-route)
      (splash-page-button
       "Monster Builder"
       "anatomy"
       routes/dnd-e5-monster-builder-page-route)
      (splash-page-button
       "Spell Builder"
       "gift-of-knowledge"
       routes/dnd-e5-spell-builder-page-route)
      (splash-page-button
       "Feat Builder"
       "vitruvian-man"
       routes/dnd-e5-feat-builder-page-route)
      (splash-page-button
       "Class Builder"
       "mounted-knight"
       routes/dnd-e5-class-builder-page-route)
      (splash-page-button
       "Race Builder"
       "woman-elf-face"
       routes/dnd-e5-race-builder-page-route)
      (splash-page-button
       "Background Builder"
       "ages"
       routes/dnd-e5-background-builder-page-route)]
     [:div
      {:style (style
                {:display :flex
                 :flex-wrap :wrap
                 :justify-content :center
                 :margin-top "10px"})}

      (splash-page-button2
        "NPC Generator"
        "monk-face"
        "/generator/npcgenerator")
      (splash-page-button2
        "City Generator"
        "elven-castle"
        "/generator/citygenerator")
      (splash-page-button2
        "Name Generator"
        "stone-tablet"
        "/generator/namegenerator")
      (splash-page-button2
        "Legend Generator"
        "giant-squid"
        "/generator/legendgenerator")
      (splash-page-button2
        "Rumor Generator"
        "discussion"
        "/generator/rumorgenerator")
      (splash-page-button2
        "Wanted Poster Generator"
        "wanted-reward"
        "/generator/wantedpostergenerator")
      (splash-page-button2
        "So you're looking for..."
        "cash"
        "/generator/resourcegenerator")]


     ]]
   [:div.legal-footer-parent
    {:style (style {:font-size "12px"
                    :color :white
                    :padding "10px"})}
    (legal-footer-2)]])
