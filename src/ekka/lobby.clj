(ns hrtime.lobby
  (:gen-class)
  (:use
   hrtime.layout-lib
   seesaw.dev
   seesaw.core
   seesaw.border
   seesaw.mig
   seesaw.make-widget)
  (:use hrtime.app-layout)
  (:use hrtime.conf-generator)
  (:require
   [hrtime.icon-library :as icon]
   [hrtime.dev-tools :refer [image-scale]]))


(defn make-header [] 
  (label :icon (image-scale icon/jarman-png 5)))


(defn make-body []
  (btn-start
   :text (lang :btn_run_app)
   :background "#ddd"
   :args [:listen [:mouse-clicked (fn [e] (if (hrtime.conf-generator/test-connection)
                                            (do
                                              (-> (doto hrtime-frame
                                                    (.setLocationRelativeTo nil)) pack! show!)
                                              (dispose! e))
                                            (do
                                              (config! e
                                                       :text (lang :btn_run_app)
                                                       :enabled? true)
                                              (hrtime.layout-lib/msg-error (lang :info_connection_fal) "Błąd połączenia")
)))]]))

(defn make-footer []
  (icon-btn :icon (image-scale icon/settings-64x64-png 40)
            :args [:tip "Przejdź do ustawień."
                   :listen [:mouse-clicked (fn [e] (-> (doto (config-frame)
                                                         (.setLocationRelativeTo nil)) pack! show!))]]))

(defn lobby-builder []
  (mig (vec (map vector 
                 [(flow-panel :items [(make-header)]
                              :align :center)
                  (mig [[(make-body)]] :args [:constraints ["" "10%[grow, fill]10%" "0%[]0%"]])
                  
                  (flow-panel :items [(make-footer)]
                              :align :right
                              :border (empty-border :right 20))]))
       :args [:constraints ["wrap 1" "0px[grow, fill]0px" "0px[grow]0px"]]))

(defn lobby-frame []
  (frame :title "Mr. Jarman - HRTime"
                        ;;  :on-close :exit
         :minimum-size [400 :by 500]
         :icon (seesaw.icon/icon (clojure.java.io/file hrtime.icon-library/calendar1-64-png))
         :content (lobby-builder)))

;; (defn lobby []
;;   (-> (doto lobby-frame
;;         (.setLocationRelativeTo nil)) pack! show!))


;; (lobby)



;; (def lis [{:one 1} {:two 2}{:one 1} {:two 2}])
;; (map (fn [pair] (str (first (first pair)))) lis)

;; (def asd [(label)])
;; (print (vec (map vector asd)))

;; (component-generator [{:k "v"} {:a "b"}])

;; (defn lobby-builder []
;;   (let [top (get-in (lobby-components) [:top])
;;         mid (get-in (lobby-components) [:middle])
;;         bot (get-in (lobby-components) [:bottom])]
;;     (mig [[(flow-panel :items (component-generator top))]
;;           [(flow-panel :items (component-generator mid))]
;;           [(flow-panel :items (component-generator bot) :align :right)]]
;;          :args [:constraints ["wrap 1" "0px[grow, fill]0px" "0px[fill]0px[grow]0px[fill]0px"]])
;;     ))

;; (lobby-builder)



;; Let over lambda
;; (defn lobby-conf []
;;   (let [lobby-map (lobby-comp)]
;;     (fn [x] (print x lobby-map))))

;; (def lb (lobby-conf))

;; (lb "Hello: ")
;; (lb "World: ")
