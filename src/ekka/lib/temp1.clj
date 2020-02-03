;;; Jou bro, jeśli nie wiedziałeś, to wewnątrz bibliteki `seesaw.dev` znajdują sie dwie funkcji:
;;; Dwie funkcje drukują to w STDOUT, czuli tak niby dają println. To funkcje wyłącznie dla
;;; podpowiadania "co?" i "jak?" robić z kompoenntem którego chcesz używać.
;;; 
;;; `show-events` - pokazuje wszystkie dostępne eventy dla wskazanego kompnentu
;;;  Example:
;;;      (show-events (text))
;;;
;;; `show-options` - pokazuje wszystkie dostępne opcje(po kluczu) do użycia
;;;  Example:
;;;      (show-options (checkbox))
;;;
;;; zajębistę, cnie? :)

(ns hello-seesaw.core
  (:use seesaw.dev
        seesaw.core
        seesaw.mig
        seesaw.chooser
        seesaw.make-widget)
  (:require [clojure.string :as string]))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defn is-selector?
  "Function as if keyword has *-[select|selector|listbox|slct|sel] in parameter name"
  [symbol-selector]
  {:pre [(keyword? symbol-selector)]}
  (let [select (last (string/split (str (symbol symbol-selector)) #"-"))]
    (some #(= select %) ["select" "selector" "listbox" "slct" "sel"])))

(defn transform-label-to-name
  "Transform some keyword type to text.
  Example:
  :some-text-field would be converted to string 'Some text field'"
  [some-keyword]
  {:pre [(keyword? some-keyword)]}
  (let [[x & rest] (string/join " " (string/split (str (symbol some-keyword)) #"-"))]
    (str (string/upper-case x) (apply str rest))))


;;;;;;;;;;;;;;;
;;; Layouts ;;;
;;;;;;;;;;;;;;;

;;; Tu się znajdują te layouty i jak ja ich używam. 

(defn vertical-list-config-panel [items]
  (grid-panel :columns (count items) :items items))
(defn vertical-config-panel [l component]
  (grid-panel :columns 2 :hgap -80 :items [(label :valign :top :text (transform-label-to-name l)) component]))
(defn horizontal-config-panel
  ([component-vec]
   (vertical-panel :items component-vec))
  ([component-vec border-string]
   (vertical-panel :items component-vec :border (transform-label-to-name border-string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration cuncurrent management functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ponieważ configuracja to typ typowo konkuręcyjny, napisałem
;;; kilka metod dla manipulacji nad configuracjami za ich pośr-
;;; iednictwem

(defn configuration-change [configuration-reference]
  (fn [keys value-to-replace]
    (dosync (ref-set configuration (assoc-in (deref configuration-reference) keys value-to-replace)))))

(defn configuration-on-event [configuration-reference]
  (fn [f]
   (f (deref configuration-reference))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI templates components ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; templates for simple text configuration option
(defn text-component [configuration-changer config-key-vector default-text]
  (text :text default-text
        :listen [:selection (fn [e] (when-let [t (text e)]
                                      (configuration-changer config-key-vector t)))]))

;;; templates for boolean type of confuguration parameter
(defn checkbox-component [configuration-changer config-key-vector default-t-f-value]
  (checkbox :selected? default-t-f-value
            :listen [:action #(configuration-changer config-key-vector (selection %))]))

;;; template for list of strings which normaly separates by `,` symbol.  
(defn text-list-component [configuration-changer config-key-vector default-config-vector-list]
  ;; the function do transformation 
  ;; from string to string vector, after update text:
  ;; "some, text, i, write" => ["some", "text", "i", "write"]
  ;; and save it to configurations
  (let [string-to-string-vector
        (fn [text] (vec (filter #(not (empty? %))
                            (map string/trim
                                 (string/split text #",")))))]
      (text :text (string/join ", " default-config-vector-list)
            :listen [:selection (fn [e] (when-let [t (text e)]
                                          (configuration-changer
                                           config-key-vector
                                           (string-to-string-vector t))))])))

;;; template for selecting one of list available options. Selected option is first options in list
(defn listbox-component [configuration-changer config-key-vector default-config-list-model]
  (let [] (fn  [item items]
            (let [i (.indexOf items item)]
              (if (> 0 i) items
                  (if (= i 0) items
                      (vec (concat (vector (nth items i))
                                   (vec (subvec items 0 i))
                                   (vec (subvec items (inc i) (count items))))))))))
  (listbox :model default-config-list-model
           :listen [:selection #(when-let [t (selection %)]
                                  (configuration-changer
                                   config-key-vector
                                   (listbox-select-from-items t default-config-list-model)))]))


;;; helper function which recursive build Panel depend on `param-map` parameter
(defn generate-form [config-changer key-deep-vec param-map]
  (let [[t1 t2] param-map
        get-k-vec #(conj key-deep-vec %)]
    (cond
      (string? t2)                            (vertical-config-panel t1 (text-component config-changer (get-k-vec t1) t2))
      (number? t2)                            (vertical-config-panel t1 (text-component config-changer (get-k-vec t1) t2))
      (boolean? t2)                           (vertical-config-panel t1 (checkbox-component config-changer (get-k-vec t1) t2))
      (and (is-selector? t1) (seqable? t2))   (vertical-config-panel t1 (listbox-component config-changer (get-k-vec t1) t2))
      (map? t2)                               (horizontal-config-panel (vec (map #(generate-form config-changer (get-k-vec t1) %) t2)) t1)
      (seqable? t2)                           (vertical-config-panel t1 (text-list-component config-changer (get-k-vec t1) t2)))))



(defn generate-configuration-form
  "Main function to generating configuration window
  Version 0.1
  - Nie implementuje co robić z daną konfiguracją, jeśli ktoś klinki \"Save config\", dla rozwiązania problemu trzeba przkazać lambdę wewnątrz funkcji `config-save` jako pierwszy argument, czyuli podminieć `#'println`. "
  [configuration]
  (let [main-configuration (ref configuration)
        back-up-configuration configuration
        config-changer (configuration-change main-configuration)
        config-save (configuration-on-event main-configuration)]
    (scrollable (horizontal-config-panel
               (conj (vec (map #(generate-form config-changer [] %) (deref main-configuration)))
                     (vertical-list-config-panel
                      [(button :text "Save config" :listen [:action (fn [e] (config-save #'println))])]))))))



;;;;;;;;;;;;;
;;; DEBUG ;;;
;;;;;;;;;;;;;

;;; debug functionality

(native!)

(def f (frame :title "bliat"))

(defn display "Display function for opened window" [content]
  (config! f :content content)
  content)

(display (generate-configuration-form {:one "bliat"
                                       :two "rhre"
                                       :other ["one" "two" "thee" "you" "back" "to" "me"]
                                       :other-selector ["one" "two" "thee" "you" "back" "to" "me"]
                                       :one-more true
                                       :StyleBox-1 {:change-style true
                                                    :Inbaded-Panel-2 {:styles-select ["dark" "ligth"]
                                                                      :color "#ffffff"
                                                                      :background-color "#111000"}}
                                       :StyleBox-2 {:change-style true
                                                    :Inbaded-Panel-2 {:color "#ffffff"
                                                                      :background-color "#111000"}}
                                       :costam "jeszcze"}))


(-> f pack! show!)


;;;;;;;;;;;;;
;;; TESTS ;;;
;;;;;;;;;;;;;

;;; test local UI component integration with cuncurrent model
;;; uncomment it to do tests

;; (display (text-component (configuration-change configuration) [:one] "bliat"))
;; (display (text-list-component (configuration-change configuration) [:one] ["one" "two" "three"]))
;; (display (checkbox-component (configuration-change configuration) [:one] true))
;; (display (listbox-component (configuration-change configuration) [:one] ["i" "love" "you" "all" "the" "time"]))
;; (display (horizontal-config-panel (vector (listbox-component (configuration-change configuration) [:one] ["i" "love" "you" "all" "the" "time"])) :suka-nachuj))
;; (display (vertical-config-panel :suka-nachuj (listbox-component (configuration-change configuration) [:one] ["i" "love" "you" "all" "the" "time"])))
