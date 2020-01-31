(ns hello-seesaw.core
  (:use seesaw.core
        seesaw.mig
        seesaw.chooser
        seesaw.make-widget)
  (:require [clojure.string :as string]))

(seq {:suka "bliat"
      :other ["one" "two" "thee" "you" "back" "to" "me"]
      :other {:suka }
      :bliat true})

(defn generate-form [param-map]
  (let [[t1 t2] param-map]
      (cond
        (string? t2) (format "(panel (label %s) (areatext %s))" t1 t2)
        (number? t2) (format "(panel (label %s) (filed %s))" t1 t2)
        (boolean? t2) (format "(panel (label %s) (checkbox %s))" t1 t2)
        (map? t2) (format "(panel (label %s) %s" t1 (string/join " " (map generate-form t2))) 
        (seqable? t2) (format "(panel (label %s) (listview %s)" t1 t2))))

(map generate-form  {:suka "bliat"
                     ;; :other ["one" "two" "thee" "you" "back" "to" "me"]
                     :other {:suka true  :vasyl {:bliat true}}})


(native!)
(def f (frame :title "Tester" :content "Test dzia³ania SeeSaw"))


(defn display "Display function for opened window" [content]
  (config! f :content content)
  content)


(def f (frame :title "bliat"))

(defn display "Display function for opened window" [content]
  (config! f :content content)
  content)

(display (vertical-panel :items ["This" "is" "a" "vertical" "stack of" "JLabels"])
         ;; (grid-panel :border "Properties"
         ;;             :columns 2
         ;;             :items ["Nmae" (text "frank")
         ;;                     "address" (text "1234")])
         )

(-> f pack! show!)
