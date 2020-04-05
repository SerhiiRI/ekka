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
(require '[clojure.string :as string])


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
(def .color_bg "#fff")
(def .color_fg "#333")
(def .color_main_bar "#292929")
(def .color_jarman_grey .color_bg)
(def .color_jarman_dark "#726ee5")
(def .color_jarman_light "#96c1ea")
(def .color_jarman_dark "#29295e")
(def .color_focus "#89cc6c")

(import java.awt.GraphicsEnvironment)
(import java.awt.Font)

(defn Add_font "Funkcja dodaje czcionkę, lokalizacja czcionki src/ekka/resources/fonts/"
  [font_name] (.registerFont (. GraphicsEnvironment getLocalGraphicsEnvironment) (java.awt.Font/createFont (java.awt.Font/TRUETYPE_FONT) (clojure.java.io/file (string/join "" ["src/ekka/resources/fonts/" font_name])))))

(defn ShowFonts "Funkcja wyświetla listę dostępnych czcionek, ale nie zwraca ich."
  [] (map println (. (. GraphicsEnvironment getLocalGraphicsEnvironment) getAvailableFontFamilyNames)))

(defn GetFonts "Funkcja zwraca nazy dostęnych czcionek."
  [] (map identity (. (. GraphicsEnvironment getLocalGraphicsEnvironment) getAvailableFontFamilyNames)))

(defn Fonts_panel "Funkcja wyświetla okienko z czcionkami w swoim formacie."
  [& {:keys [txt size] :or {txt "Przykładowy tekst od Mr. Jarmana" size 16}}]
  (-> (frame :content (scrollable (vertical-panel :items (map (fn [font] (grid-panel
                                                                          :columns 2
                                                                          :items [(label :text font 
                                                                                         :font {:name (str font) :size size}
                                                                                         :border (empty-border :right 10)
                                                                                         :halign :right)
                                                                                  (label :text txt :font {:name (str font) :size 16})])) (GetFonts)))))
      pack!
      show!))

; (Fonts_panel :txt "сука блять" :size 20)
; (Add_font "Cattedrale.ttf")
; (ShowFonts)
; (GetFonts)

; Funkcje skracające ===================================

(defn div
  "Kontener pod elememty niczym div z html"
  ([] (border-panel :background .color_bg
                    :border 0
                    :center (label)))
  ([item] (border-panel :background .color_bg
                        :border 0
                        :center item))
  ([item style] (apply border-panel :background .color_bg
                       :center item
                       :border 0
                       style)))

(defn flow
  "Kontener pod elememty niczym div z html ale bez halign"
  ([] (flow-panel :background .color_bg
                  :border 0
                  :vgap 0
                  :hgap 0
                  :items [(label)]))
  ([item] (flow-panel :background .color_bg
                      :border 0
                      :vgap 0
                      :hgap 0
                      :items [item]))
  ([item style] (apply flow-panel :background .color_bg
                       :border 0
                       :vgap 0
                       :hgap 0
                       :items [item]
                       style)))

(defn mig
  "Skrócona wersja mig-panel, style można dodać jako ostatni parametr w formie wektora, np: (vector :border 5 :coś tam)"
  ([layout items] (mig-panel :constraints layout
                             :items items
                             :border 0
                             :background .color_bg))
  ([layout items style] (apply mig-panel :constraints layout
                               :items items
                               :border 0
                               :background .color_bg
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

(defn logo_title "Nazwa programu"
  [&{:keys [font_name 
            font_size
            width
            height 
            color_dark 
            color_light 
            color_bg
            valign] 
     :or {font_name "Gabriola" 
          font_size 22
          width 100
          height 20 
          color_dark .color_jarman_dark 
          color_light .color_jarman_light
          color_bg .color_bg
          valign ""}}]
  (mig [""
        (string/join ["10px[grow, fill" (if (= valign "center") (str ", center")) "]0px"])
        "0px[grow, fill]0px"]
       [[(mig [""
               "0px[shrink 0]0px[shrink 0]0px[shrink 0]0px"
               (string/join ["0px[shrink 0," .size_htitle "]0px"])]
              [[(label :text "Mr. "
                       :foreground color_dark
                       :font {:name font_name :size font_size}
                       :halign :right
                       :background color_bg)]
               [(label :text "Jar"
                       :foreground color_light
                       :font {:name font_name :size font_size}
                       :halign :right
                       :background color_bg)]
               [(label :text "man"
                       :foreground color_dark
                       :font {:name font_name :size font_size}
                       :halign :left
                       :background color_bg)]]
              (vector :background color_bg
                      :size [.size_wtitle :by .size_htitle]))]]
       (vector :background color_bg)))

; (show-options (mig-panel))

; Style ================================================

(def .flex_panel (vector :background .color_bg
                         :foreground "#000"
                         :halign :center))

(def .login_input (vector :size [300 :by 30] 
                          :halign :center 
                          :border (line-border :thickness 2 :color .color_jarman_dark)
                          :background "#fff"
                          :caret-color "#fff"
                          :font {:name "Cattedrale-Demo" :style :bold :size 14}))
 
(def .login_btn (vector :size [150 :by 35]
                        :halign :center
                        :background .color_jarman_dark
                        :foreground "#fff"
                        :font "CourierNew-bold-22"))

(def .main_btn (vector :background .color_jarman_dark 
                       :foreground "#fff" 
                       :size [20 :by 20]
                       :halign :center
                       :font "Arial-Bold-12"
                       :v-text-position :center
                       :h-text-position :center))

; Okno logowania ========================================
(def login-panel
  (flow (mig ["wrap 3"
             "0px[25%, grow, fill]0px[50%, grow, fill]0px[25%, grow, fill]0px"
             "0px[40%, grow, fill]0px[50px, fill]0px"]
            [[(flow)]
             [(mig ["wrap 1"
                    "0px[grow, fill, center]0px"
                    "20px[grow, fill]20px[45, fill]5px[45, fill]10px[45, fill]0px"]
                  ;  LOGO ========================================
                   [[(mig ["wrap 1"
                           "0px[grow, fill]0px[grow, fill]0px"
                           "0px[grow, fill]0px[grow, fill]0px"]
                      [[(label :focusable? true :halign :center :background .color_bg :icon (image-scale "src/ekka/resources/imgs/logo.png" 10))]
                       [(logo_title :valign "center" :font_size 40)]])]
                    [(apply text :text "User"
                            :listen [:focus-gained (fn [e] (config! e :border (line-border :thickness 2 :color .color_focus)))
                                     :focus-lost (fn [e] (config! e :border (line-border :thickness 2 :color .color_jarman_dark)))]
                            .login_input)]
                    [(apply password :text "Password"
                            :listen [:focus-gained (fn [e] (config! e :border (line-border :thickness 2 :color .color_focus)))
                                     :focus-lost (fn [e] (config! e :border (line-border :thickness 2 :color .color_jarman_dark)))]
                            .login_input)]
                    [(btn "Connect" .login_btn [:focus-gained (fn [e] (config! e :background .color_jarman_light :border (line-border :thickness 2 :color .color_jarman_light)))
                                                :focus-lost (fn [e] (config! e :background .color_jarman_dark :border (line-border :thickness 2 :color .color_jarman_dark)))
                                                :mouse-entered (fn [e] (config! e :background .color_jarman_light :border (line-border :thickness 2 :color .color_jarman_light) :cursor :hand))
                                                :mouse-exited (fn [e] (config! e :background .color_jarman_dark :border (line-border :thickness 2 :color .color_jarman_dark)))
                                                ])]])] 
             [(flow)] 
             [(flow) "span"]])))

; Update okna ===========================================
; (display login-panel)

; Szablon/funkcja tworząca dla okno programu ============
(defn mainFrame
[left-menu title right-menu content] 
(frame :undecorated? false
       :resizable? true
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
(def f (mainFrame (logo_title :color_dark .color_jarman :color_light .color_jarman_grey :color_bg .color_main_bar)
                  (label :background .color_main_bar :drag-enabled? true :listen [:mouse-clicked (fn [e](alert "123"))]) 
                  (flow-panel :background .color_main_bar
                              :align :right
                              :items [(apply label :listen [:mouse-clicked (fn [e] ())
                                                            :mouse-entered (fn [e] (config! e :background .color_jarman_light :border (line-border :thickness 2 :color .color_jarman_light) :cursor :hand))
                                                            :mouse-exited (fn [e] (config! e :background .color_jarman_dark :border (line-border :thickness 2 :color .color_jarman_dark)))]
                                             :text "_" .main_btn)
                                      (apply label :listen [:mouse-clicked (fn [e] ())
                                                            :mouse-entered (fn [e] (config! e :background .color_jarman_light :border (line-border :thickness 2 :color .color_jarman_light) :cursor :hand))
                                                            :mouse-exited (fn [e] (config! e :background .color_jarman_dark :border (line-border :thickness 2 :color .color_jarman_dark)))]
                                             :text "[  ]" .main_btn)
                                      (apply label :listen [:mouse-clicked (fn [e] (dispose! e))
                                                            :mouse-entered (fn [e] (config! e :background .color_jarman_light :border (line-border :thickness 2 :color .color_jarman_light) :cursor :hand))
                                                            :mouse-exited (fn [e] (config! e :background .color_jarman_dark :border (line-border :thickness 2 :color .color_jarman_dark)))]
                                             :text "X" .main_btn)])
                  login-panel))

(-> f pack! show!)
; Uruchomienie okna======================================


; (show-options (label))
; (show-options (flow-panel))
; (show-events (text))