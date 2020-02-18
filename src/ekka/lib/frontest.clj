(ns ekka.lib.ide
  (:use seesaw.core
        seesaw.dev
        seesaw.mig
        seesaw.chooser
        seesaw.color
        seesaw.make-widget))

;; nieco podprawiłem ci funkcje, polecam
;; dopni jej do jakiegoś skrótu w VS CODE
(defmacro pm
  "Funkcja rozkladania makra w śriodowisku Visual Studio Code"
  [exp]
  (do (alter-var-root #'clojure.pprint/*print-suppress-namespaces* (constantly true))
      (clojure.pprint/pprint (macroexpand-1 exp))))


(native!)
(def f (frame :title "Tester" :content "Test działania SeeSaw"))


(defn display "Display function for opened window" [content]
  (config! f :content content)
  content)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; podejście do budowania funkcji generacji buttonów ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (defmacro butt*
;   "Makro podejście"
;   [txt act]
;   `(button :text ~txt
;            :listen [:action (fn [~'e] (~@act))]))

; (def b (butt* "Hi" (alert "Jak to może działać?")))

; (defn butt
;   "Podejście typowo funkcujne, poprostu wrzuć funkcje którom ma wykorzystywać listener, ogólnei podejście zalecane"
;   [txt one-arg-action]
;   (button :text txt
;           :listen [:action one-arg-action]))

; (def b (butt "Hi" (fn [e] (alert "Jak to może działać?"))))

; (display b)
; (-> f pack! show!)


; (def mp (mig-panel
;          :constraints ["fill" "center"]
;          :items [[(mig-panel
;                    :constraints ["wrap"]
;                    :items [[(butt "Button 1" (fn [e] (alert "b1")))]
;                            [(butt* "Button 2" (alert "b2"))]])]
;                  ["Option 2"]]))

(show-options (canvas))



(defn nibybutt [bg-color txt] (label :background bg-color :text txt :size [100 :by 30]))
(def b1 (nibybutt "#8a5" "Łola Boga"))

(def mp (border-panel
         :north (horizontal-panel :items [b1] :background "#333")
         :center (button :text "Click me")
         :vgap 5 :hgap 5 :border 5
         :background "#333"))

(listen b1 :mouse-entered #(config! % :background (color 192 192 192) :foreground (color 0 0 0)))
; (listen b1 :mouse-exited  #(config! % :background (color 0 0 0)       :foreground (color 192 192 192)))
(listen b1 :mouse-exited  #(config! % :background (color 0 0 0)       :foreground (color 192 192 192)))

(show-events (label))

(display mp)
(-> f pack! show!)

; Macro działające na zasadzie (-> )
; (defmacro arr
;   ([last] last)
;   ([f & param] 
;     (let [wyraz (first param)
;           head (first wyraz)
;           body (rest wyraz)] 
;       `(arr 
;         (~head ~f ~@body)
;         ~@(rest param)))))



; (pm (arr 1 (+ 2) (+ 3)))
; (arr 1 (+ 2) (+ 3))
 
; ===================================================================================== Border panel + flow panel

; Makro na przycisk
; (defmacro menu_btn
;   [txt] `(label :text ~txt
;                 :background .bg_btn
;                 :foreground .fg
;                 :size [.btn_hsize :by .btn_vsize]
;                 :halign :center
;                 :font {:size 12}
;                 :border (line-border :right 2 :color .bg)))
; 

; (def app
;   (border-panel
;    :north (flow-panel 
;            :vgap 0
;            :hgap 0
;            :align :left
;            :background .bg 
;            :items [
;                    (menu_btn "File")
;                    (menu_btn "File")
;                    (menu_btn "File")
;                    ])
;    :center (button :text "Click me")
;    :background .bg))


; ===================================================================================== left-right-split


; PODOBNIE JAK WYŻEJ
; (def app
;   (border-panel :background .bg
;                 :center (left-right-split (border-panel :background "#40a1b3"
;                                                         :center (label :text "Mr. Jarman"
;                                                                        :border 0))
;                                           (border-panel :background "#d5e7eb"
;                                                         :center (label :text "Login Panel"
;                                                                        :border 0))
;                                           :divider-location 1/3)))



; ===================================================================================== Makro na własny przycisko-label


; Definicja własnego przycisku
; (defmacro menu_btn
;   [txt] `(label :text ~txt
;                 :background .bg_btn
;                 :foreground .fg
;                 :size [.btn_hsize :by .btn_vsize]
;                 :halign :center
;                 :font {:size 12}
;                 :border (line-border :right 2 :color .bg)))


; ===================================================================================== Własne style jako wektor później doczepiany


; Styl przycisku
; (def .btn (vector :background "#7d75bf"
;                         :foreground "#000"
;                         :halign :center
;                         :size [100 :by 25]
;                         :font {:size 12}
;                         :border (line-border 1 "#000")))

; Użycie takiego zdefiniowanego stylu dzieje się za pomocą takiej formułki (apply label :text "Mój tekst" .btn) co tworzy nam label z doczepinymi stylami


; ===================================================================================== Migpanel na wstępie


; MIG PANEL
;:constraints czyli [ styl łączenia kolumn
;                     styl kolumn czyli margines od lewej [styl kolumny 1] przestrzeń między kolumnami [styl kolumny 2] margines prawy ]
;                     styl wiersza czyli margines od góry [styl wiersza 1] przestrzeń między wierszami [styl wiersza 2] margines dolny ]
; (def app
;   (border-panel :background .bg
;                 :center (mig-panel :constraints ["wrap 2" 
;                                                  "0px[200px, fill]0px[grow, fill]0px"
;                                                  "0px[grow, fill]0px"]
;                                    :border 0
;                                    :items [[(border-panel :background "#40a1b3"
;                                                           :center (label :text "Mr. Jarman"
;                                                                          :border 0))]
;                                            [(border-panel :background "#d5e7eb"
;                                                           :center (label :text "Login Panel"
;                                                                          :border 0))]
;                                            ])))


; ===================================================================================== Migpanel test szablonu


; Używając mig panel można tworzyć layout niczym w gridzie css'owym albo nawet flexie
; 
; Styl przycisku
; (def .login_btn (vector :background "#7d75bf"
;                         :foreground "#000"
;                         :halign :center
;                         :font {:size 12}
;                         :border (line-border 1 "#000")))
; 
; MigLayout
; (def login-panel
; (border-panel :background .bg
; :center (mig-panel :constraints ["wrap 4"
;                                  "0px[grow, fill]0px[grow, fill]0px[grow, fill]0px[grow, fill]0px"
;                                  "0px[grow, fill]0px[50px, fill]0px"]
;                    :border 0
;                    :items [
;                            [(apply label :text "Left panel" .login_btn)]
;                            [(apply label :text "Center" .login_btn) "span 2"]
;                            [(apply label :text "Right panel" .login_btn)]
;                            [(apply label :text "Footer" .login_btn) "span"]
;                            ])))