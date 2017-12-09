(ns secret-santa-simulator.core
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.ratom :refer [reaction run!]]))

(def people ["Aladar" "Bela" "Cecil" "Dezso" "Elemer" "Ferenc" "Geza" "Hedvig" "Ida" "Janos" "Katalin" "Luca" "Marta" "Nora" "Orsolya" "Peter" "Q?" "Rita" "Sara" "Tamas" "Ubul" "Viktor" "W?" "Zoltan"])

(defonce people-count (atom 6))
(defonce people-indices (reaction (range @people-count)))
(defonce events-mtx (atom {}))
(def last-event (atom nil))
(def evt-mtx-max (reaction (apply max 1 (vals @events-mtx))))
(def evt-mtx-min (reaction (->> @events-mtx vals seq (apply min))))

(run! (swap! events-mtx #(reduce-kv (fn [acc i j] (update acc [i j] (fnil inc 0))) % @last-event)))
(run! @people-count (reset! events-mtx {}))

(defn- rand-perm [n]
  (loop [idx 0, out [], remaining (set @people-indices)]
    (cond (= remaining #{idx}) (recur 0 [] (set @people-indices))
          (seq remaining)      (let [x (rand-nth (seq remaining))]
                                 (if (= x idx)
                                   (recur idx out remaining)
                                   (recur (inc idx) (conj out x) (disj remaining x))))
          :otherwise out)))

(defn- make-rand-perm! [] (reset! last-event (rand-perm @people-count)))

(def play (atom nil))

(defn- config-component []
  [:div {:style {:padding "1em"}}
   [:span "Number of participants: "
    [:span {:style {:display :inline-block :width "30px"}} @people-count]
    [:button {:on-click #(swap! people-count inc)} "+"]
    (when (< 2 @people-count)
      [:button {:on-click #(swap! people-count dec)} "-"])]])

(defn- action-component []
  [:div {:style {:padding "1em"}}
   [:button {:on-click make-rand-perm!} "Make random permutation!"]
   [:button {:on-click #(dotimes [_ 200] (make-rand-perm!))} "Make 200 random permutations!"]
   [:button {:on-click #(dotimes [_ 2000] (make-rand-perm!))} "Make 2000 random permutations!"]])

(defn- animation-component []
  [:div
   (if @play
     [:button {:on-click #(do (.clearInterval js/window @play) (reset! play nil))} "Stop"]
     [:button {:on-click #(reset! play (.setInterval js/window make-rand-perm! 50))} "Start"])])

(defn- color [cnt]
  (if (nil? cnt) "white"
      (let [hue (* 120 (/ (- cnt @evt-mtx-min) (- @evt-mtx-max @evt-mtx-min)))]
        (str "hsl(" hue ",100%,50%)"))))

(defn- table-component []
  [:table
    [:thead
     [:tr
      [:td {:col-span 2 :row-span 2}]
      [:td {:col-span 99
            :style {:vertical-align "center"
                    :text-align "center"
                    :border-bottom "1px solid silver"}}
       [:i "Huzott fel"]]]
     [:tr
      (for [i @people-indices]
        [:th {:key (str "h" i) :style {:padding "0.5em"}} [:b (people i)]])]]
    [:tbody
     [:tr [:td {:row-span 99
                :style {:border-right "1px solid silver"
                        :padding-right "1em"}} [:i "Huzo fel"]]]
     (doall (for [i @people-indices]
              [:tr {:key (str "x" i)}
               [:td {:style {:text-align :right}} [:b (people i)]]
               (doall (for [j @people-indices
                            :let [cnt (get @events-mtx [i j])]]
                        [:td {:key (str "y" i j)
                              :style {:background-color (color cnt)
                                      :text-align       "center"
                                      :border-radius    "8px"}}
                         [:code (str cnt)]]))]))]])

(defn- view [] [:div [config-component] [action-component] [animation-component] [table-component]])

(reagent/render-component [view] (. js/document (getElementById "app")))

(defn on-js-reload [] )
