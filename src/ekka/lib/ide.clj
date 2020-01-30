(ns hello-seesaw.core
 (:use seesaw.core
       seesaw.mig
       seesaw.chooser
       seesaw.make-widget))

(require '[clojure.pprint :as pp])
(alter-var-root #'pp/*print-suppress-namespaces* (constantly true))

(defmacro pm [exp]
  (pp/pprint (macroexpand-1 exp)))

(native!)

(def f (frame :title "Tester" :content "Test działania SeeSaw"))

(defn display [content]
  (config! f :content content)
  content)

(defmacro butt
  ([txt act] `(button :text ~txt
                     :listen [:action (fn [] (~@act))])))

(def b (butt "Hi" (alert "Jak to może działać?")))

(display b)
(-> f pack! show!)



(def mp (mig-panel
         :constraints ["fill" "center"]
         :items [[(mig-panel
                   :constraints ["wrap"]
                   :items [[(butt "Button 1" (alert "b1"))]
                           [(butt "Button 2" (alert "b2"))]])]
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

