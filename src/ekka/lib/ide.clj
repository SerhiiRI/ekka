(ns ekka.lib.ide
  (:use seesaw.core
        seesaw.dev
        seesaw.mig
        seesaw.chooser
        seesaw.color
        seesaw.border
        seesaw.make-widget))

;; nieco podprawiłem ci funkcje, polecam
;; dopni jej do jakiegoś skrótu w VS CODE
(defmacro pm
  "Funkcja rozkladania makra w śriodowisku Visual Studio Code"
  [exp]
  (do (alter-var-root #'clojure.pprint/*print-suppress-namespaces* (constantly true))
      (clojure.pprint/pprint (macroexpand-1 exp))))


(native!)
(def f (frame :title "Mr. Jarman" 
              :content (label 
                        :text "Test aplikacji Mr. Jarman."
                        :halign :center)
              :minimum-size [400 :by 300]))


(defn display "Display function for opened window" [content]
  (config! f :content content)
  content)

(def .item {:background "#8a4" :size [100 :by 30]})
; (display (makelabel "YEY" .item))
(show-options (label))

; STYLES
(def .bg "#fff")
(def .fg "#333")
(def .bg_btn "#eee")
(def .btn_vsize 25)
(def .btn_hsize 75)


(defmacro menu_btn
  [txt] `(label :text ~txt
                :background .bg_btn
                :foreground .fg
                :size [.btn_hsize :by .btn_vsize]
                :halign :center
                :font {:size 12}
                :border (line-border :right 2 :color .bg)))

(let [a  {:background "#333333" }]
  `(label (~@a)))

(def .btn (vector :background "#8ab" :foreground "#ff0000"))

(def app
  (border-panel :background .bg
                :center (mig-panel :constraints ["" 
                                                 "0px[800px, grow, fill]0px"
                                                 "0px[480px, grow, fill]0px"]
                                   :border 0
                                   :items [])))


; (display (label :text "EEE" :background .bg))
(display app)
(-> f pack! show!)