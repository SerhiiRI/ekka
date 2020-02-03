(ns hello-seesaw.core
  (:use seesaw.core
        seesaw.mig
        seesaw.chooser
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

(defmacro butt*
  "Makro podejście"
  [txt act]
  `(button :text ~txt
           :listen [:action (fn [~'e] (~@act))]))

; (def b (butt* "Hi" (alert "Jak to może działać?")))

(defn butt
  "Podejście typowo funkcujne, poprostu wrzuć funkcje którom ma wykorzystywać listener, ogólnei podejście zalecane"
  [txt one-arg-action]
  (button :text txt
          :listen [:action one-arg-action]))

; (def b (butt "Hi" (fn [e] (alert "Jak to może działać?"))))

; (display b)
; (-> f pack! show!)


(def mp (mig-panel
         :constraints ["fill" "center"]
         :items [[(mig-panel
                   :constraints ["wrap"]
                   :items [[(butt "Button 1" (fn [e] (alert "b1")))]
                           [(butt* "Button 2" (alert "b2"))]])]
                 ["Option 2"]]))

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


