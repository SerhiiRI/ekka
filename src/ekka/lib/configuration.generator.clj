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


;;; Między innym, pamiętasz że mówiłem że clojure to język niezmienny. Probując zachować tą zasadę,
;;; Rich Hickey, realizując asynchroniczność, synchroniczność odwoływań, on potworzył typy
;;; referencyjne. Więc nie do końca clojure jest językiem niezmiennym. No to są typy
;;; specjlanie. Clojure, w odróźnieniu od innych języków ma tranzakcyjną model pamięci(on tylko
;;; jeden taki, ktory w swoim rdzeniu to ma). Jeśli rozumiesz jak dziala synchronizacja i
;;; wykorzystywanie pamięci wątkami to będzie łżej zrouzmieć to co ja mówie.

;;; Jeśli dwa wątki zapisują w jedną przestrzeń cokolwiek, to dla nich są mechanizmy sterownia. Np
;;; Lock - czyli synchroniczne podejście. Gdy jeden wątek/process czeka, a inny wykonuje działanie
;;; na lock-u. Asynchronicze to gdy jedna i druga wartość probują się dobić do jednej pamiąci bez
;;; kontroli Lock. Czyli jedna może przypadkiem zatrzeć rezultat innej. Więc dla tego są mechanizmy
;;; kontrolne. Clojure wykorzystuję tranzakcyjne podejście. Czyli jeśli tranzakcja była uszkodzona,
;;; to odwołaj transakcje i probuj do tej pory, dopóki się nie wykona kod w transakcje.

;;; Typy 'zmiennę' (bo też nie jest tak do konca):
;;              +-----------+-------------+
;;              |Coordinated|Uncoordinated|
;; +------------+-----------+-------------+
;; |Synchronous |Refs       |Atoms        |
;; +------------+-----------+-------------+
;; |Asynchronous|Vars       |Agents       |
;; +------------+-----------+-------------+

;; Coordinated - An operation that depends on cooperation from other operations (possibly, other
;; operations at least do not interfere with it) in order to produce correct results. For example, a
;; banking operation that involves more than one account.

;; Uncoordinated - An operation that does not affect other operations in any way. For example, when
;; downloading 100 Web pages concurrently, each operation does not affect the others.

;; Synchronous - When the caller's thread waits, blocks, or sleeps until it has access to a given
;; resource or context.

;; Asynchronous - Operations that can be started or scheduled without blocking the caller's thread.

;;; Teraz przyklad jak ja używam `ref`. Czyli prosty typ synchorniczy i skoordynowany:
;; Tworzymy zmienną zmienną

;; (def s (ref "some-value"))

;; otzymamy jej. Dla tego trzeba używać operatora biezpiecznego wyciągnęcia z wątku czegokolwiek

;; (deref ref) ;=> "some-value"

;; lub krócej

;; @ref ;=> "some-value"

;; Dla podmiany, warto używać (ref-set s "chuj"), i jedna rzeć. Ponieważ wszystkie dany rozporszonie
;; widoczne między wątkami, to dla podmiany czego kolwiek używaj bloka tranzakcyjnygo. (dosync
;; (ref-set s "chuj")), bez dosync cie nie puści, bo będzie to znaczyć że nie chcesz używać
;; transakcji, co jest niezbędne., a więc:

;; @ref ;=> "chuj"

;; używam tego dla tego żebym z komponentów i ich listenerów zmieniać samą mapę konfgiuracji


(ns ekka.configuration.generator
  (:use seesaw.dev
        seesaw.core
        seesaw.mig
        seesaw.chooser
        seesaw.border
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
  (grid-panel :background "#FFFFFF" :columns (count items) :items items :border (empty-border :top 5)))
(defn vertical-config-panel [l component]
  (grid-panel :background "#FFFFFF" :columns 2 :hgap -80 :items [(label :foreground "#363636" :valign :top :text (transform-label-to-name l) :border (empty-border :top 3 :left 6)) component]))
(defn horizontal-config-panel
  ([component-vec]
   (vertical-panel :background "#FFFFFF" :items component-vec))
  ([component-vec border-string]
   (vertical-panel :background "#FFFFFF" :items component-vec :border (compound-border  (transform-label-to-name border-string ) (empty-border :top 4 :bottom 3)))))

(show-options (label))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration cuncurrent management functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ponieważ configuracja to typ typowo konkuręcyjny, napisałem
;;; kilka metod dla manipulacji nad configuracjami za ich pośr-
;;; iednictwem

(defn configuration-change [configuration-reference]
  (fn [keys value-to-replace]
    (dosync (ref-set configuration-reference (assoc-in (deref configuration-reference) keys value-to-replace)))))

(defn configuration-on-event [configuration-reference]
  (fn [f]
   (f (deref configuration-reference))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI templates components ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn colorizator-text-component "Colorize component, by hexadecemal value" [target]
  (let [lower-str (string/lower-case (string/trim (text target)))
        smb-arr "0123456789abcdef"
        [hash & color] lower-str
        c (count color)]
    (if (and (= hash \#)
             (or (= c 3) (= c 6))
             (reduce #(and %1 (some (fn [_s] (= %2 _s)) smb-arr)) true color))
      (config! target
               :background lower-str
               :foreground (let [clr (apply str color)
                                 hex (read-string (if (= (count clr) 3)
                                                    (str "0x" clr)
                                                    (apply str "0x" (map first (partition 2 clr)))))]
                             (if (< hex 1365) "#FFF" "#000")))
      (config! target :background "#FFFFFF" :foreground "#000000"))))


;;; templates for simple text configuration option
(defn text-component [configuration-changer config-key-vector default-text]
  (colorizator-text-component (text :text default-text :background "#FFFFFF"
                     :listen [:selection (fn [e] (when-let [t (text e)]
                                                   (configuration-changer config-key-vector t)
                                                   (colorizator-text-component e)))])))


;;; templates for boolean type of confuguration parameter
(defn checkbox-component [configuration-changer config-key-vector default-t-f-value]
  (checkbox :selected? default-t-f-value :background "#FFFFFF"
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
      (text :text (string/join ", " default-config-vector-list) :background "#FFFFFF"
            :listen [:selection (fn [e] (when-let [t (text e)]
                                          (configuration-changer
                                           config-key-vector
                                           (string-to-string-vector t))))])))

;;; template for selecting one of list available options. Selected option is first options in list
(defn listbox-component [configuration-changer config-key-vector default-config-list-model]
  (let [listbox-select-from-items (fn [item items]
                                    (let [i (.indexOf items item)]
                                      (if (> 0 i) items
                                          (if (= i 0) items
                                              (vec (concat (vector (nth items i))
                                                           (vec (subvec items 0 i))
                                                           (vec (subvec items (inc i) (count items)))))))))]
    (selection! (listbox :model default-config-list-model :border (compound-border (line-border :color "#999999" :top 1 :left 1 :bottom 1 :right 1) (empty-border :right 1 :top 1 :bottom 1 :color "#999999" )) :background "#FFFFFF"
                         :listen [:selection #(when-let [t (selection %)]
                                                (configuration-changer
                                                 config-key-vector
                                                 (listbox-select-from-items t default-config-list-model)))]) (first default-config-list-model))))


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
  Version: 0.2
  Function generate GUI scrollable panel for `configuration` map parameter"
  ([configuration] {:pre [(map? configuration)]}
   (generate-configuration-form configuration #'println))
  ([configuration save-function] {:pre [(map? configuration)]}
   (let [main-configuration (ref configuration)
         back-up-configuration configuration
         config-changer (configuration-change main-configuration)
         config-save (configuration-on-event main-configuration)]
     (scrollable (horizontal-config-panel
                  (conj (vec (map #(generate-form config-changer [] %) (deref main-configuration)))
                        (vertical-list-config-panel
                         [(button :text "Save config" :listen [:action (fn [e] (config-save save-function))])])))))))



;;;;;;;;;;;;;
;;; DEBUG ;;;
;;;;;;;;;;;;;

;;; debug functionality

;; (native!)

;; (def f (frame :title "bliat"))

;; (defn display "Display function for opened window" [content]
;;   (config! f :content content)
;;   content)

;; (display (generate-configuration-form {:one "bliat"
;;                                        :two "rhre"
;;                                        :other ["one" "two" "thee" "you" "back" "to" "me"]
;;                                        :other-selector ["one" "two" "thee" "you" "back" "to" "me"]
;;                                        :one-more true
;;                                        :StyleBox-1 {:change-style true
;;                                                     :Inbaded-Panel-2 {:styles-select ["dark" "ligth"]
;;                                                                       :color "#ffffff"
;;                                                                       :background-color "#111000"}}
;;                                        :StyleBox-2 {:change-style true
;;                                                     :Inbaded-Panel-2 {:color "#ffffff"
;;                                                                       :background-color "#111000"}}
;;                                        :costam "jeszcze"}))

;; (-> f pack! show!)


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
