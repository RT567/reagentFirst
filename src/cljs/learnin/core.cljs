(ns learnin.core
  (:require
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]
   [reagent.session :as session]
   [reitit.frontend :as reitit]
   [clerk.core :as clerk]
   [accountant.core :as accountant]))

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/anotherPage" :anotherPage]
    ["/items"
     ["" :items]
     ["/:item-id" :item]]
    ["/about" :about]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

;; -------------------------
;; Page components

(def testAtom (reagent/atom 0))

(def rows 3)
(def cols 3)
(def row (vec (repeat cols " ")))
(def board3 (vec (repeat rows row)))

(def game (reagent/atom {:board board3;(boardCreator rows cols buttonGame game)
                         :whosTurn "X"
                         :winner "no one."}))

(defn straight-check
  [player board [x y] [dx dy] n]
  (every? true?
          (for [i (range n)]
            (= (get-in board [(+ (* i dy) y) (+ (* i dx) x)])
               player))))

(defn win-check
  [player board n]
  (some true?
        (for [x (range 3)
              y (range 3)
              dir [[0 1] [1 0] [1 1] [1 -1]]]
          (straight-check player board [x y] dir n))))

;write a fn that handles the click and checks
;takes board, whos turn, x and y
;checks if the position is valid
;swaps value at location
;swaps turn
;checks if the game is over
(defn external-handler1
  [x y]
  (let [{:keys [board whosTurn winner]} @game]
    (when
      (and (= " " (get-in board [y x]))
           (= winner "no one."))
       (do ;leaving here incase we make an if
         (swap! game assoc-in [:board y x] whosTurn)
         (swap! game assoc-in [:whosTurn] (#(if (= % "O") "X" "O") whosTurn))
         (when (win-check "X" (:board @game) 3)
           (swap! game assoc-in [:winner] "X!"))
         (when (win-check "O" (:board @game) 3)
           (swap! game assoc-in [:winner] "O!"))))))

(defn reseter
  []
  [:div [:input {:type "button"
                 :on-click #(do
                             (swap! game assoc-in [:board] board3)
                             (swap! game assoc-in [:whosTurn] "X")
                             (swap! game assoc-in [:winner] "no one."))
                 :value "Restart"}]])


(defn table []
  (fn []
    (let [{:keys [board whosTurn]}  @game
          nextTurn #(if (= % "O") "X" "O")]
     [:div "It is " whosTurn "'s turn!"
      [:table {:style {:background "black"
                       :text-align "center"}}
       (into [:tbody]
             (vec
              (for [y (range 3)]
                (into [:tr {:style {:border "1px solid black"
                                    :border-collapse "collapse"}}]
                      (vec
                       (for [x (range 3)]
                         [:td {:on-click #(external-handler1 x y)
                               :style {:border "1px black"
                                       :border-collapse "collapse"}};#(external-handler-table board whosTurn y x)}
                          
                          (get-in board [y x])]))))))]])))



(defn winner-announce []
  (fn []
    [:div
     [:p {:style {:visibility
                  (if (= "no one." (:winner @game)) 
                    "hidden"
                    "visible")}}
      "The winner is " (:winner @game)]]))

;(click-handler board whosTurn y x)

;create reset button

(defn externalHandler [atom]
 (fn []
   (swap! atom inc)))

;can you still just write normal functions to handle stuff?
(defn buttonWithExternalHandler []
  (fn [] 
    [:div
     [:button {:on-click (externalHandler testAtom)}
      "button to test external handling on-click"]
     [:p "the value of the button is: " @testAtom]]))


(defn squareClicked? []
  (fn []
    [:div 
     "The value of testAtom is: " @testAtom]))


(defn multi&display
  [number]
  [:div "Double the count is: " (* 2 number)])

(defn button
  [value]
  [:input {:type "button"
           :on-click #(swap! value inc)
           :value "clickeroo"}])

(defn anotherClassicTest
  []
  (let [counter (reagent/atom 0)]
    (fn []
      [:div
       [multi&display @counter]
       [button counter]])))

(def timingInfo
  (reagent/atom {:currentTime 0}))

(def IgotNothingButTime
  (reagent/atom 1))

(js/setInterval #(swap! IgotNothingButTime inc) 1000)

(defn timer3 []
  (fn []
  [:div @IgotNothingButTime " seconds have passed."]))

(defn timer-component []
  (let [current (:currentTime @timingInfo)]
    (js/setTimeout #(swap! timingInfo update-in [:currentTime] inc) 1000)
    [:div "Seconds elapsed " current]))

(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(def click-count (reagent/atom 0))

(defn countingComponent []
  (let [val (reagent/atom "name")] 
    (fn []
      [:div
       [:div "Enter your name " [atom-input val]]
       [:br]
       [:span [:input {:type "button" :value "click!"
                       :on-click #(swap! click-count inc)}]
        [:strong " Congratulations! "] @val ", you have clicked "
        [:span {:style {:color "green"}} @click-count] " times!"]])))

(defn asd []
  (let [current (:currentTime @timingInfo)]
    [:div "current time " current]))

(defn clicksPerSecond []
  (fn []
      [:div "Which means you are able to click " (/ @click-count @IgotNothingButTime) " times per second!"]))



(defn simpleComponent []
  [:div
   [:p "Small Paragraph"]
   [:p
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red"] " text."]])

;;;;;;;;;connect 4;;;;;;;;;;;;

(def connect4-board
  (vec (repeat 7 [])))

(def state (reagent/atom {:board connect4-board
                          :turn "r"
                          :winner 0}))


(defn win-check4
  [player board n]
  (some true?
        (for [x (range 0 6)
              y (range 0 7)
              dir [[0 1] [1 0] [1 1] [1 -1]]]
          (straight-check player board [x y] dir n))))

(defn ext-con4-handle
  [x]
  (let [{:keys [winner board turn]} @state]
    (when (and (>= 5 (count (get-in @state [:board x]))) (= winner 0))
      (swap! state update-in [:board x] (fn [a] (conj a turn)))
      (swap! state assoc-in [:turn] (if (= turn "r") "y" "r"))
      (prn board)
      (when
       (win-check4 "r" (:board @state) 4)
        (swap! state assoc-in [:winner] "Red")
        (prn "red wins!"))
      (when
       (win-check4 "y" (:board @state) 4)
        (swap! state assoc-in [:winner] "Yellow")))))

(defn connect4
  []
  (fn []
    [:svg
     {:view-box "0 0 70 60"
      :width 700
      :height 600
      :style {:background "blue"}
      :transform "rotate(180)"};
     (doall (for [i (range 0 7)      ;x
                  j (range 0 6)]     ;y
        ^{:key [i j]} [:circle {:r 4.1 :cx (+ 5 (* i 10)) :cy (+ 5 (* j 10))
                                :fill (case (get-in @state [:board i j])
                                        nil "lightgrey"
                                        "r" "red"
                                        "y" "yellow")
                                :on-click #(ext-con4-handle i)}]))]))

(defn winner-announce2 []
  (fn []
    [:div
     [:p {:style {:visibility
                  (if (= 0 (:winner @state))
                    "hidden"
                    "visible")}}
      "The winner is " (:winner @state) "!"]]))




;;; pages ;;;

(defn home-page []
  (fn []
    [:span.main
     [:h1 "Welcome to learnin"]
     [:br] [:hr]
     [table]
     [winner-announce]
     [reseter]
     [:br][:hr]
     [squareClicked?]
     [:br][:hr]
     [buttonWithExternalHandler]
     [:br][:hr]
     [:br][:hr]
     ;[list-of-nums]
     [:br][:hr]
     [:ul
      [:li [:a {:href (path-for :items)} "Items of learnin"]]
      [:li [:a {:href "/broken/link"} "Broken link"]]
      [:li [:button.green "test"]]]
     [simpleComponent]
     [:br][:hr][:br]
     [anotherClassicTest]]))


(defn another-Page []
  (fn []
    [:span.main
     [:h1 "Newest Version"]
     [countingComponent]
     [:br]
     [timer3]
     [clicksPerSecond]
     [:br]
     [:br]
     [:hr]
     [:h3 "broken stuff"]
     [timer-component]
     [asd]]))

(defn about-page []
  (fn [] [:span.main
          [:h1 "Connect Four"]
          [connect4]
          [winner-announce2]]))


(defn items-page []
  (fn []
    [:span.main
     [:h1 "The items of learnin"]
     [:ul (map (fn [item-id]
                 [:li {:name (str "item-" item-id) :key (str "item-" item-id)}
                  [:a {:href (path-for :item {:item-id item-id})} "Item: " item-id]])
               (range 1 5))]]))


(defn item-page []
  (fn []
    (let [routing-data (session/get :route)
          item (get-in routing-data [:route-params :item-id])]
      [:span.main
       [:h1 (str "Item " item " of learnin")]
       [:p [:a {:href (path-for :items)} "Back to the list of items"]]])))





;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :items #'items-page
    :item #'item-page
    :anotherPage #'another-Page))


;; -------------------------
;; Page mounting component

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      [:div
       [:header
        [:p 
         [:a {:href (path-for :index)} "Home"] " | "
         [:a {:href (path-for :about)} "About learnin"] " | "
         [:a {:href (path-for :anotherPage)} "The cooler page"]]]
       [page]
       [:footer
        [:br]
        [:hr]
        [:p "This project was generated by the "
         [:a {:href "https://github.com/reagent-project/reagent-template"} "Reagent Template"] "."]]])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (rdom/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
