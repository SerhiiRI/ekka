(ns hrtime.conf-generator
  (:gen-class)
  (:use seesaw.dev
        seesaw.core
        seesaw.mig
        seesaw.chooser
        seesaw.tree
        seesaw.border
        seesaw.make-widget
        seesaw.widgets.rounded-label
        hrtime.layout-lib)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [hrtime.icon-library :as icon]
            [hrtime.dev-tools :refer [image-scale in?]]
            [hrtime.database-manager :as datamanager]
            [hrtime.config-manager :as cfg]
            [clojure.xml]))


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


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti  file-only class)
(defmethod file-only java.lang.String [f] (filter #(.isFile %) (file-seq (io/file f))))
(defmethod file-only clojure.lang.LazySeq [f] (filter #(.isFile %) f))
(defmethod file-only java.io.File [f] (filter #(.isFile %) f))

(defmacro def-if-keyword-rule [rule-name rules]
  {:pre [(and (seqable? rules) (not (string? rules)))]}
  (let [doc (format "\nDescription:\n\tFunction test if keyword had \"end with rule\"\nAccepted Rules:\n\t[%s]"
                    (string/join "|" rules))]
    `(def ~rule-name ~doc (fn [symbol-selector#]
                            {:pre [(keyword? symbol-selector#)]}
                            (let [select# (last (string/split (str (symbol symbol-selector#)) #"-"))]
                              (some #(= select# %) ~rules))))))

(def-if-keyword-rule is-selector?      ["select" "selector" "listbox" "slct" "sel"])
(def-if-keyword-rule is-documentation? ["documentation" "document" "doc" "info" "d" "meta"])
(def-if-keyword-rule is-database-conf? ["database" "db" "data"])

(defn transform-label-to-name
  "Transform some keyword type to text.
  Example:
  :some-text-field would be converted to string 'Some text field'"
  [some-keyword]
  {:pre [(keyword? some-keyword)]}
  (let [[x & rest] (string/join " " (string/split (str (symbol some-keyword)) #"-"))]
    (str (string/upper-case x) (apply str rest))))

(defn parse-to-type [s]
  (if (empty? s) nil
      (if (every? #(some (fn [x] (= x %)) [\1 \2 \3 \4 \5 \6 \7 \8 \9 \0]) s)
        (try (Integer/parseInt (re-find #"\A-?\d+" s))
             (catch Exception e (if (and (= (first s) \:) (not (in? (seq s) \ ))) (symbol s) s))
             (finally nil))
        (if (and (= (first s) \:) (not (in? (seq s) \ ))) (symbol s) s))))

;;;;;;;;;;;;;;;
;;; Layouts ;;;
;;;;;;;;;;;;;;;

;;; Tu się znajdują te layouty i jak ja ich używam. 
;; (defn hor-list-config-panel [items]
;;   (grid-panel :background (style :color_bg) :columns (count items) :items items :border (empty-border :top 5)))

(defn hor-config-panel [l component]
  (mig [[(label :text (transform-label-to-name l))]
        [component]]
       :args [:constraints ["" "50px[50%, grow, right]10px[40%, grow, fill, left]50px" "0px[]0px"]
              :background (style :color_bg)]))

(defn ver-config-panel
  ([component-vec]
   (mig component-vec :args [:background (style :color_bg)]))
  ([component-vec border-string]
   (ver-config-panel component-vec border-string nil))
  ([component-vec border-string toolbox]
   (mig (concat 
         (vec (map vector (filter some? [(label :text (transform-label-to-name border-string)
                                                :font {:style :bold :size 12}
                                                :border (line-border :top 2 :color "#aaa"))
                                         toolbox])))
         component-vec) 
        :args [:background (style :color_bg)
               :constraints ["wrap 1" "50px[grow, fill]0px" "10px[grow, fill]0px"]])))

(defn mig-vertical-panel [doc-key ui-component]
  (hrtime.layout-lib/mig [[ui-component]]))

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
      (config! target :background "#FFFFFF" :foreground (style :color_font_.txt)))))

;; (read-string "0xFFFFFF")
;; (read-string "0x000000")
;; (defn gradient-generator )
;; (Integer/toHexString (bit- 0xFF0000 0x251242))

;;; templates for simple text configuration option
(defn text-component [configuration-changer config-key-vector default-text]
  (colorizator-text-component (text :text default-text :background "#FFFFFF"
                                    :listen [:selection (fn [e] (when-let [t (parse-to-type (text e))]
                                                                 (configuration-changer config-key-vector t)
                                                                 (colorizator-text-component e)))])))


;;; templates for boolean type of confuguration parameter
(defn checkbox-component [configuration-changer config-key-vector default-t-f-value]
  (checkbox :selected? default-t-f-value :background (style :color_bg)
            :listen [:action #(configuration-changer config-key-vector (selection %))]))

;;; template for list of strings which normaly separates by `,` symbol.  
(defn text-list-component [configuration-changer config-key-vector default-config-vector-list]
  ;; the function do transformation 
  ;; from string to string vector, after update text:
  ;; "some, text, i, write" => ["some", "text", "i", "write"]
  ;; also work with types
  ;; "some, :suka, 123, write" => ["some", :suka, 123, "write"]
  ;; and save it to configurations
  (let [string-to-string-vector
        (fn [text] (vec (filter #(or (not (nil? %)) (not (empty? %))) (map (comp parse-to-type string/trim) (string/split text #",")))))]
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
    (selection! (listbox :model default-config-list-model 
                         :background "#FFFFFF"
                         :listen [:selection #(when-let [t (selection %)]
                                                (configuration-changer
                                                 config-key-vector
                                                 (listbox-select-from-items t default-config-list-model)))]) (first default-config-list-model))))

;;; template for selecting one of list available options. Selected option is first options in list
(defn- doc-string
  "String-like documentation inserted directly in CLJ config layout"
  [s] {:pre [(string? s)]} (label :text (str s) 
                                  :border (empty-border :left 50 :right 50)
                                  :font {:style :bold :size (- (style :font_size_m) 1)}))
(defn- doc-map
  "Map-like documentation inserted directly in CLJ config layout
  Can parse only map keys: `:description`, `:parameters`, `:header`.

  Example:
     (doc-map {:header \"Some Header\"
               :description \"description to program\"
               :parameters [\"list-of\" \"available\" \"parameters\" 1 2 3 \"which funciton can supply\"]})"
  [m] {:pre [(map? m)]}
  (let [{:keys [space header parameters description]} m
        ; TODO: Trzeba dorobić dynamiczną podmianę stylu
        ui-components (vec (map vector (filter some? [(if space (label :border (compound-border (empty-border :top 50) (line-border :bottom 2 :color "#000"))))
                                                      (if header (text :halign :left
                                                                       :text (str (:header m))
                                                                       :font (style :font_bold {:size (style :font_size_l) :color (style :color_font_doc)})
                                                                       :editable? false
                                                                       :border (empty-border :left 50 :right 50)
                                                                       :background (style :color_bg)))
                                                      (if description (mig [[(label :text "Description:"
                                                                                    :font (style :font_bold {:color (style :color_font_doc)}))]
                                                                            [(text :text (str (:description m))
                                                                                   :multi-line? true
                                                                                   :wrap-lines? true
                                                                                   :font (style :font_regular {:color (style :color_font_doc)})
                                                                                   :editable? false
                                                                                   :background (style :color_bg)
                                                                                   :border (empty-border :left 15 :right 15))]]
                                                                           :args [:border (empty-border :top 0 :bottom 0 :left 50 :right 50)]))
                                                      (if parameters (mig [[(label :text "Parameters:"
                                                                                   :font (hrtime.layout-lib/style :font_bold)
                                                                                   :border nil)]
                                                                           [(text :text (string/join "\n" (map (fn [item] (string/join item '("* " " "))) (:parameters m)))
                                                                                  :multi-line? true
                                                                                  :wrap-lines? true
                                                                                  :font (style :font_regular {:color (style :color_font_doc)})
                                                                                  :editable? false
                                                                                  :border (empty-border :left 15 :right 15)
                                                                                  :background (style :color_bg))]]
                                                                          :args [:border (empty-border :top 0 :bottom 0 :left 50 :right 50)]))])))]
    (hrtime.layout-lib/mig ui-components)))

(defn documentation-component "Router documentation compontent" [metadata]
  (let [ui-component (cond
                       (map? metadata) (doc-map  metadata)
                       (string? metadata) (doc-string metadata)
                       :else (label))]
    (mig [[ui-component]])))

(defn declare-button-list
  "Create button list for main config manipulation
  Example:
   (declare-button-list {\"save config\" (fn [e] e)})
  String mean text on button, and lambda mean what thay
  button on onClick event"
  [function-vector]
  (let [ui-component (reduce (fn [acc [name {:keys [function icon]}]]
                               (if (not (fn? function)) acc
                                   (conj acc (if icon
                                               [(btn :text name :user-data {:mode "s"}
                                                     :halign :center :background (style :color_bg_btn_config)
                                                     :onClick function)]
                                               [(btn :text name :user-data {:mode "s"}
                                                     :halign :center :background (style :color_bg_btn_config)
                                                     :onClick function)]))))
                             [] (seq function-vector))]
    (hrtime.layout-lib/mig ui-component :args [:constraints ["" "0px[grow, fill]0px" "0px[grow, fill]0px"] :border (line-border :top 2 :color (style :color_bg_btn_hover))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toolbox Components ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn database-toolbox [config-saver key-path ]
  (mig [[(declare-button-list {(lang :btn_test_connection)
                               {:icon icon/refresh-blue-64-png
                                :function
                                (fn [e]
                                  (println (config-saver #(get-in % key-path)))
                                  (if (and (config-saver #(get-in % key-path)) (datamanager/test-connection (config-saver #(get-in % key-path))))
                                    (do (alert (lang :info_connection_suc) :title "Info" :type :info :icon (image-scale icon/agree-64-png 30)))
                                    (do (alert (lang :info_connection_fal) :title "Info" :type :info :icon (image-scale icon/X-64x64-png 30))))
                                  )}})]]
       :args [:constraints ["" "0px[grow, fill]0px" "0px[grow, fill]0px[30, fill]0px"]]))


(def config_neh (hrtime.config-manager/config-file "database.edn"))
;; (config_neh)
(defn sql-connection [] (config_neh [:JDBC-mariadb-configuration-database]))
;; (sql-connection)
(defn test-connection []
  (datamanager/test-connection (sql-connection)))
(test-connection)

;;; helper function which recursive build Panel depend on `param-map` parameter
(defn generate-form [config-changer config-saver key-deep-vec param-map]
  (let [[tkey tval] param-map
        get-k-vec #(conj key-deep-vec %)]
    (cond
      (is-documentation? tkey)                  (documentation-component tval)
      (string? tval)                            (hor-config-panel tkey (text-component config-changer (get-k-vec tkey) tval))
      (number? tval)                            (hor-config-panel tkey (text-component config-changer (get-k-vec tkey) tval))
      (boolean? tval)                           (hor-config-panel tkey (checkbox-component config-changer (get-k-vec tkey) tval))
      (and (is-selector? tkey) (seqable? tval)) (hor-config-panel tkey (listbox-component config-changer (get-k-vec tkey) tval))
      (is-database-conf? tkey)                  (ver-config-panel (vec (map (fn [pair] [(generate-form config-changer config-saver (get-k-vec tkey) pair)]) tval)) tkey (database-toolbox config-saver (get-k-vec tkey)))
      (map? tval)                               (ver-config-panel (vec (map (fn [pair] [(generate-form config-changer config-saver (get-k-vec tkey) pair)]) tval)) tkey)
      (seqable? tval)                           (hor-config-panel tkey (text-list-component config-changer (get-k-vec tkey) tval))
      (keyword? tval)                           (hor-config-panel tkey (text-component config-changer (get-k-vec tkey) (str tval)))
      :else (label :text (format "[Error] %s %s" (prn-str tkey) (prn-str tval))) )))

(defn clojure-map-form
  "Main function to generating configuration window
  Version: 0.2
  Function generate GUI scrollable panel for `configuration` map parameter"
  ([configuration] {:pre [(map? @configuration)]}
   (clojure-map-form configuration #'println))
  ([configuration save-function] {:pre [(map? @configuration)]}
   (let [back-up-configuration @configuration 
         config-changer (configuration-change configuration)
         config-saver (configuration-on-event configuration)]
     (ver-config-panel
       (vec (map (fn [pair] [(generate-form config-changer config-saver [] pair)]) (deref configuration)))))))


(defn for-text-editor-btn
  [txt ico listener] (btn :text txt
                          :user-data {:mode "s"}
                          :halign :center
                          :background (style :color_bg_btn_config)
                          :underline (style :color_bg_btn_hover)
                          :hover-bg-color (style :color_bg_btn_hover)
                          :icon ico
                          :onClick listener))


(defn simple-text-editor
  "Contain a simpel text panel for editing in panel content"
  [txt file] (let [text-area (text :multi-line? true
                                   :wrap-lines? true
                                   :border (empty-border :thickness 3)
                                   :text @txt)
              max-font 70 min-font 0
              up-down-font (fn [e f-i-d] (let [f (float (f-i-d (.getSize (config e :font))))]
                                          (if (<= 0 f 70) (.deriveFont (config e :font) f))))
              set-font-size (fn [e f-size] (.deriveFont (config e :font) (float f-size)))
              ;; Przyciski dla edytora tekstu
              text-font-combobox (combobox :model (range 0 70 5) :listen [:action-performed (fn [e] (config! text-area :font (set-font-size text-area (selection e))))])
              up-text (for-text-editor-btn "" (image-scale hrtime.icon-library/a-up-grey-64x64-png 20) (fn [e] (config! text-area :font (up-down-font text-area inc))))
              down-text (for-text-editor-btn "" (image-scale hrtime.icon-library/a-down-grey-64x64-png 20) (fn [e] (config! text-area :font (up-down-font text-area dec))))
              ;; search-in-text (for-text-editor-btn "Search" (image-scale hrtime.icon-library/loupe-grey-64-png 40) [:action-performed (fn [e] e)])
              ]
          ;; TODO: Fajnie by było dodać nazwę pliku do labela
          (mig [[(mig [[(label :text (str file)
                               :font {:style :bold}
                               :background (style :color_bg_btn_config)
                               :border (compound-border (empty-border :left 10) (line-border :bottom 2 :color (style :color_bg_btn_hover))))]
                       [text-font-combobox] [up-text] [down-text] ;; [search-in-text]
                       ]
                      :args [:constraints ["wrap 1" "0px[grow, fill]0px" "0px[grow, fill]0px[30, fill]0px"]
                             :constraints ["" "0px[grow, fill]0px[fill]0px" "0px[30, fill]0px"]])]
                [(mig [[text-area]])]]
               :args [:constraints ["wrap 1" "0px[grow, fill]0px" "0px[30, fill]0px[grow, fill]0px"]])))

(defn view-by-extention
  "Get Swing view to file with specyfying extention
  For example:
    (view-by-extention \"dupa.txt\") create JtextArea,
  becouse this pattern not available in (cond) pattern
  list
  Every function must be only one argument, which do
  some manipulation on referece. The ref link would
  contain a loaded file datastruct
    (fn [c] (spit @c \"file-name\"))
  "
  [file config]
  (let [extension (last (string/split (str file) #"\."))]
    (condp in? extension
      ["clj" "edn"] (do (dosync (ref-set config (into (sorted-map) (clojure.edn/read-string (slurp file)))))
                        (mig [[(scrollable (clojure-map-form config) :border nil)]
                              [(mig [[(declare-button-list {(lang :btn_save)  {:function (fn [e] (if (dlg "Czy chcesz zapisać kofniguracje?" "Informacja" (fn [x] true) (fn [x] nil))
                                                                                                  (do (spit file (prn-str @config))
                                                                                                      (alert (lang :info_changes_saved_and_reload) :title "Info" :type :info :icon (image-scale icon/agree-64-png 60)))))
                                                                               :icon icon/agree-64-png}})]]
                                    :args [:constraints ["" "0px[grow, fill]0px" "0px[grow, fill]0px[30, fill]0px"]])]
                              ]
                             :args [:constraints ["wrap 1" "0px[grow, fill]0px" "0px[grow, fill]0px[30, fill]0px"]]))
      (do (dosync (ref-set config (slurp file)))
          (mig [[(scrollable (simple-text-editor config file) :border nil)]
                [(declare-button-list {(lang :btn_save)  {:function (fn [e] (if (dlg "Czy chcesz zapisać kofniguracje?" "Informacja" (fn [x] true) (fn [x] nil))
                                                                             (do (spit file @config)
                                                                                 (alert (lang :info_changes_saved) :title "Info" :type :info :icon (image-scale icon/agree-64-png 60)))))
                                                          :icon icon/agree-64-png}})]]
               :args [:constraints ["wrap 1" "0px[grow, fill]0px" "0px[grow, fill]0px[30, fill]0px"]])))))


(defn is-private-file? [file-name]
  (if (> (count file-name) 7)
     (= "private" (string/lower-case (apply str (take 7 file-name))))))

(def setting_icon [[(label :halign :center :icon (image-scale hrtime.icon-library/settings-128-png))]])

(defn TreeCellRenderer []
  (proxy [javax.swing.tree.DefaultTreeCellRenderer] []
    (^java.awt.Component getTreeCellRendererComponent [^javax.swing.JTree tree        ^java.io.File      value 
                                                       ^java.lang.Boolean select      ^java.lang.Boolean expanded
                                                       ^java.lang.Boolean leaf        ^java.lang.Integer row
                                                       ^java.lang.Boolean hasFocused]
     (let [icon-by-extension (fn [file-extension] 
                               (if-not (string? file-extension) icon/ban-grey-64-png
                                       (condp in? file-extension
                                         cfg/*markdown-files*  icon/txt3-64x64-png
                                         cfg/*xml-files*       icon/xml3-64x64-png
                                         cfg/*text-files*      icon/txt3-64x64-png
                                         cfg/*clojure-files*   icon/main-sourse3-64x64-png
                                         cfg/*config-files*    icon/file3-64x64-png
                                         cfg/*json-files*      icon/json3-64x64-png
                                         cfg/*csv-files*       icon/ban-64x64-png
                                         cfg/*shell-files*     icon/simple-file3-64x64-png
                                         cfg/*prop-files*      icon/file3-64x64-png
                                         icon/simple-file3-64x64-png)))           
           node-value value
           config-name (fn [file-name] (if-not (some #(= \. %) (seq file-name)) file-name
                                              (string/join "." (butlast (string/split (str file-name) #"\.")))))
           extension (fn [file-name] (if (some #(= \. %) (seq file-name)) 
                                      (last (string/split (str file-name) #"\."))))]
       (if leaf
         (btn :text (config-name (.getName (cast java.io.File node-value)))
              :icon (image-scale (icon-by-extension (extension (str node-value))) 30)
              :background "#FFF"
              :font (assoc (style :font_regular) :size 17)
              :border (empty-border :thickness 4))
         (if expanded
           (btn :text (.getName (cast java.io.File node-value))
                :icon (image-scale icon/folder1-64x64-png 30)
                :font (assoc (style :font_bold) :size 17)
                :border (empty-border :thickness 4))
           (btn :text (.getName (cast java.io.File node-value))
                :icon (image-scale icon/folder-close-grey-64x64-png 30)
                :background "#FFF"
                :font (assoc (style :font_bold) :size 17)
                :border (empty-border :thickness 4))))))))

(defn create-tree []
  (if (.isFile (io/file cfg/*configuration-path*))
    (mig [[(label :text (str "Konfiguracja do pliku -> " (.getName (io/file cfg/*configuration-path*)))
                  :font {:style :bold}
                  :background (style :color_bg_btn_config)
                  :border (compound-border (line-border :bottom 2 :color (style :color_bg_btn_hover))))]
          [(view-by-extention (io/file cfg/*configuration-path*) (ref nil))]]
         :args [])
    (let [config (ref nil)
          v-panel (mig setting_icon)
          sortedListFiles (fn [location-path] 
                            (let [file-list (.listFiles location-path)]
                              (concat (filter #(.isDirectory %) file-list)
                                      (filter #(and (not (is-private-file? (.getName %))) (not (.isDirectory %))) file-list))))
          tree-view (doto (tree :model  (simple-tree-model #(.isDirectory %)
                                                           sortedListFiles
                                                           (java.io.File. cfg/*configuration-path*))
                                :listen [:selection 
                                         (fn [x] (let [chosed-file (last (selection x))] 
                                                  (cond
                                                    (.isFile chosed-file) (config! v-panel :items [[(view-by-extention chosed-file config)]])
                                                    (.isDirectory chosed-file) (config! v-panel :items setting_icon))))])
                      (.setCellRenderer (TreeCellRenderer))
                      (.putClientProperty "JTree.lineStyle" "None"))]
      (left-right-split (scrollable tree-view)
                        v-panel
                        :divider-location 1/5))))


;;;;;;;;;;;;;
;;; DEBUG ;;;
;;;;;;;;;;;;;


;; (def debug-frame (frame :title "DEBUG WINDOW" :minimum-size [(style :frame_width) :by (style :frame_heigh)] :undecorated? false :content (label "empty")))
(defn config-frame []
  (binding [cfg/*configuration-path* "config/database.edn"]
    (frame :title "Mr. Jarman - Konfigurator"
           :minimum-size [500 :by 500]
           :undecorated? false
           :content (create-tree))))



;; (defn open-debug-window [] (-> debug-frame pack! show!))
;; (defn display [content]
;;   (config! debug-frame :content content)
;;   content)


;; (open-debug-window)
;; (display (create-tree))

;; (binding [cfg/*configuration-path* "config"]
;;   (display (create-tree)))

;; (binding [cfg/*configuration-path* "config/system.clj"]
;;   (display (create-tree)))

;; (display (database-toolbox nil))






;; (def debug-frame (frame :title "DEBUG WINDOW" :undecorated? false 
;;               :minimum-size [(style :frame_width) :by (style :frame_heigh)] :content (create-tree)))
;; (-> (doto debug-frame (.setLocationRelativeTo nil)) pack! show!)








