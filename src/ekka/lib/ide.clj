(ns ekka.lib.ide
  (:use seesaw.core
        seesaw.dev
        seesaw.mig
        seesaw.chooser
        seesaw.color
        seesaw.border
        seesaw.graphics
        seesaw.swingx
        seesaw.make-widget))


;; nieco podprawiłem ci funkcje, polecam
;; dopni jej do jakiegoś skrótu w VS CODE
(defmacro pm
  "Funkcja rozkladania makra w śriodowisku Visual Studio Code"
  [exp]
  (do (alter-var-root #'clojure.pprint/*print-suppress-namespaces* (constantly true))
      (clojure.pprint/pprint (macroexpand-1 exp))))


(native!)

; (defn display "Display function for opened window" 
;   [content]
;   (config! f :content content)
;   content)

; STYLE GLOBALNE
(def .bg "#fff")
(def .fg "#333")
(def .main_bar "#292929")
(def .mr "#999")
(def .jarman "#726ee5")
(def .jarman_light "#908cfa")
(def .focus "#89cc6c")


; Funkcje skracające ===================================

(defn div
  "Kontener pod elememty niczym div z html"
  ([] (border-panel :background .bg
                    :border 0
                    :center (label)))
  ([item] (border-panel :background .bg
                        :border 0
                        :center item))
  ([item style] (apply border-panel :background .bg
                       :center item
                       :border 0
                       style)))

(defn flow
  "Kontener pod elememty niczym div z html ale bez halign"
  ([] (flow-panel :background .bg
                  :border 0
                  :items [(label)]))
  ([item] (flow-panel :background .bg
                      :border 0
                      :items [item]))
  ([item style] (apply flow-panel :background .bg
                       :border 0
                       :items [item]
                       style)))

(defn mig
  "Skrócona wersja mig-panel, style można dodać jako ostatni parametr w formie wektora, np: (vector :border 5 :coś tam)"
  ([layout items] (mig-panel :constraints layout
                             :items items
                             :border 0
                             :background .bg))
  ([layout items style] (apply mig-panel :constraints layout
                               :items items
                               :border 0
                               :background .bg
                               style)))

(defn btn
  "Skrócona wersja labela, która posłuży za przycisk"
  ([txt] (label :text txt))
  ([txt style] (apply label :text txt
                      style))
  ([txt style listener] (apply label :text txt
                               :focusable? true
                               :listen listener
                               style))
  )

(defn ^sun.awt.image.ToolkitImage image-scale
  "Function scale image by percent size.
  Return `sun.awt.image.ToolkitImage` type.

  Example:
    (image-scale \"/path/to/photo\" 100)

  See more:
    javadoc `sun.awt.image.ToolkitImage`
    javadoc `sun.awt.Image`"
  ([image-path percent]
   {:pre [(not (empty? image-path))]}
   (let [image (.getImage (icon (clojure.java.io/file image-path)))
         scaler (comp int #(Math/ceil %) #(* % (/ percent 100.0)))]
     (.getScaledInstance image
                         (scaler (.getWidth image))
                         (scaler (.getHeight image))
                         java.awt.Image/SCALE_SMOOTH))))


; Style ================================================

(def .flex_panel (vector :background .bg
                         :foreground "#000"
                         :halign :center))

(def .login_input (vector :size [300 :by 30] 
                          :halign :center 
                          :border (line-border :thickness 2 :color .jarman)
                          :background "#fff"
                          :caret-color "#fff"))
 
(def .login_btn (vector :size [150 :by 35]
                        :halign :center
                        :background .jarman
                        :foreground "#fff"
                        :font "CourierNew-bold-22"))

(def .main_btn (vector :background .jarman 
                       :foreground "#fff" 
                       :size [20 :by 20]
                       :halign :center
                       :font "Arial-Bold-12"
                       :v-text-position :center
                       :h-text-position :center))

(def .main_title (vector :font "Gabriola-bold-22"
                         :border (empty-border :top 20)))

; Okno logowania ========================================
(def login-panel
  (flow (mig ["wrap 3"
             "0px[25%, grow, fill]0px[50%, grow, fill]0px[25%, grow, fill]0px"
             "0px[40%, grow, fill]0px[50px, fill]0px"]
            [[(flow)]
             [(mig ["wrap 1"
                    "0px[grow, fill, center]0px"
                    "20px[grow, fill]20px[45, fill]5px[45, fill]10px[45, fill]0px"]
                   [[(label :focusable? true :halign :center :background .bg :icon (image-scale "src/ekka/img/logo.png" 100))]
                    [(apply text  :listen [:focus-gained (fn [e] (config! e :border (line-border :thickness 2 :color .focus)))
                                           :focus-lost (fn [e] (config! e :border (line-border :thickness 2 :color .jarman)))] 
                            .login_input)]
                    [(apply password :listen [:focus-gained (fn [e] (config! e :border (line-border :thickness 2 :color .focus)))
                                              :focus-lost (fn [e] (config! e :border (line-border :thickness 2 :color .jarman)))]
                            .login_input)]
                    [(btn "Connect" .login_btn [:focus-gained (fn [e] (config! e :background .jarman_light :border (line-border :thickness 2 :color .jarman_light)))
                                                :focus-lost (fn [e] (config! e :background .jarman :border (line-border :thickness 2 :color .jarman)))
                                                :mouse-entered (fn [e] (config! e :background .jarman_light :border (line-border :thickness 2 :color .jarman_light)))
                                                :mouse-exited (fn [e] (config! e :background .jarman :border (line-border :thickness 2 :color .jarman)))
                                                ])]])] 
             [(flow)] 
             [(flow) "span"]])))

; Update okna ===========================================
; (display login-panel)

; Szablon/funkcja tworząca dla okno programu ============
(defn mainFrame
[left-menu title right-menu content] 
(frame :undecorated? true
       :content (mig ["wrap 1"
                      "0px[grow, fill]0px"
                      "0px[20, fill]0px[grow, fill]0px"]
                     [[(mig [""
                             "0px[grow, fill]0px"
                             "0px[grow, fill]0px"]
                            [[left-menu]
                             [title]
                             [right-menu]])]
                      [content]])
       :minimum-size [900 :by 560]))


; Build okna programu ===================================
(def f (mainFrame (horizontal-panel :background .main_bar
                                    :items [(apply label :text "Mr. "
                                                   :halign :right
                                                   :foreground .mr
                                                   :size [50 :by 20]
                                                   .main_title)
                                            (apply label :text "Jarman" 
                                                   :halign :left  
                                                   :foreground .jarman
                                                   :size [100 :by 20]
                                                   .main_title)]) 
                  (label :background .main_bar) 
                  (flow-panel :background .main_bar
                              :align :right
                              :items [(apply label :listen [:mouse-clicked (fn [e] ())]
                                             :text "_" .main_btn)
                                      (apply label :listen [:mouse-clicked (fn [e] ())]
                                             :text "[  ]" .main_btn)
                                      (apply label :listen [:mouse-clicked (fn [e] (dispose! e))]
                                             :text "X" .main_btn)])
                  login-panel))

(-> f pack! show!)
; Uruchomienie okna======================================

; (show-options (label))
; (show-events (text))