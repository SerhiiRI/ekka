(ns ekka.lib.datatool.key-value-tool
  (:require
   [clojure.string :as string]
   ;; [ekka.lib.datatool.sql-tool :as toolbox :refer [where-string]]
   [ekka.lib.datatool.sql-tool :as toolbox :include-macros true :refer :all]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; configuration variable ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *value-column-default-type* "default data type for key-value-table column 'value'" "TEXT")
(def *key-column-default-type* "default data type for key-value-table column 'key'" "varchar(100)")
(def *available-mariadb-engine-list* "set of available engines for key-value tables" ["MEMORY", "InnoDB", "CSV"])


(defn table-column
  ([col-name col-definition] (format "`%s` %s" col-name col-definition))
  ([col-name col-type col-definition] (format "`%s` %s %s" col-name col-type col-definition)))

(defn create-pair-table
  ([table-name]
   (create-pair-table table-name *key-column-default-type* *value-column-default-type*))
  ([table-name value-type]
   (create-pair-table table-name *key-column-default-type* value-type))
  ([table-name key-type value-type]
   (let [columns [(table-column "id" "bigint(20) unsigned NOT NULL AUTO_INCREMENT" )
                  (table-column "key" (format "%s NOT NULL" key-type))
                  (table-column "value" (format "%s DEFAULT NULL" value-type))
                  "PRIMARY KEY (`id`)"]]
     (binding [toolbox/*where-border* true]
       (format "CREATE TABLE IF NOT EXIST `%s` %s" (name table-name) 
               (toolbox/into-border (string/join ", " columns)))))))

(defn create-table [table-name & {:keys [engine key-type value-type]
                                  :or {key-type *key-column-default-type*
                                       value-type *value-column-default-type*
                                       engine "InnoDB"}}]
  (format "%s %s" (create-pair-table table-name key-type value-type)
          (format "ENGINE=%s DEFAULT CHARSET=utf8;"
                  (if (some #(= % engine) *available-mariadb-engine-list*)
                    engine "InnoDB"))))


(defn drop-table [table-name]
  (format "DROP TABLE IF EXISTS `%s`;" (name table-name)))

;; (create-table :suka :key-type "VARCHAR(20)" :engine "MEMORY")
;; (create-table :suka :key-type "VARCHAR(20)")
;; (create-table :suka :engine "MEMORY")
;; (create-table :suka)
;; (drop-table :suka)


(defrecord TableAccessors [getter setter destroyer creater cleaner keys values])



(defn create-getter [table-name]
  (fn [key] (select table-name :where (= :key key))))

(defn create-setter [table-name]
  (fn [key value] (vector (select table-name :where (= :key key))
                          (update table-name :set {:value value} :where (= :key key))
                          (insert table-name :values [{:key key :value value}]))))

(defn create-destroyer [table-name]
  (fn [] (drop-table table-name)))

(defn create-creater [table-name &{:keys [engine key-type value-type] :as all}]
  (fn [] (apply create-table table-name (mapcat identity all))))

(defn create-keys [table-name]
  (fn [] (select table-name :column [:key])))

(defn create-values [table-name]
  (fn [] (select table-name :column [:values])))

(defn create-cleaner [table-name &{:keys [engine key-type value-type] :as all}]
  (fn [] (vec (create-destroyer)
              (apply create-table table-name (mapcat identity all)))))


(defn table [table-name &{ :as all}]
  (println (apply create-table table-name (mapcat identity all)))
  (apply ->TableAccessors (map #(% table-name) [#'create-getter #'create-setter #'create-destroyer #'create-creater #'create-cleaner #'create-keys #'create-values])))



;; (table "config-table")
;; ;; To stworzy ci tabele i zwróci mapê funkcji do manipalacj± nad tabel±
;; ;; Funkcja tworzy tabele:
;; ;;
;; ;; -- SQL --
;; ;; CREATE TABLE IF NOT EXIST `config-table` (`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT, `key` varchar(100) NOT NULL, `value` TEXT DEFAULT NULL, PRIMARY KEY (`id`)) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ;; -- SQL --
;; ;; 
;; ;; Jako return zwraca mapêf funkcji dla roboty nad tabe³±. Przyklad

;; (let [T (table "config-table")
;;       funkcja-wydostawania-wartosci-po-kluczu (:getter T)
;;       funkcja-zapisyawania-wartosci-po-kluczu (:setter T)]
;;   (funkcja-wydostawania-wartosci-po-kluczu "jakis-klucz"))
;; ;; Kiedy ja podbne fizyczn± BD ona zwróci ci rezultat. 
;; ;; -- SQL --
;; ;; "SELECT * FROM config-table WHERE key = \"jakis-klucz\""
;; ;; -- SQL


;; ;;; Tu wymieniam wszystkie funkje które mozesz u¿ywaæ po tworzeniu tabeli
;; (let [{:keys [getter  :getter   setter :setter   destroyer :destroyer
;;               creater :creater cleaner :cleaner keys :keys
;;               values  :values] (table "config-table")}]
;;   ;; funkcja która dostanie ci varto¶æ po kluczu
;;   (getter "costam")
;;   ;; funkja któa zmiani ci varto¶æ po kluczu
;;   (setter "costam" 12)
;;   ;; funkja zniszczy tabele
;;   (destroyer)
;;   ;; funkcja zmiani parametry obecej tabeli lub przypizsê j±
;;   (creater :engine "InnoDB")
;;   ;; wyczy¶ci wszystko co jest w tabeli
;;   (cleaner)
;;   ;; wyci±ganie w liscie ci wszystkei kluczy
;;   (keys)
;;   ;; wyci±gnie w liscie ci wszystkie warto¶ci
;;   (values))



;; ;; tabela mo¿e byæ tworzona wed³ug twoich zapotrzebowañ
;; ;; standartowy typ danych klucza to `Varchar(120)`
;; ;; standartowy typ danych dla warto¶ci to `SMALLTEXT`
;; ;; silnik InnoDB - czyli zapisz do bazy
;; (table "config-table")

;; ;; no mo¿emy zmieniæ i typ klucza i warto¶ci 
;; (table "config-table" :key-type "BIGINT" :value-type "MEDIUMTEXT")

;; ;; mo¿emy zmieniæ to jak BD trzyma tabele.
;; ;; InnoDB - to typowy zapis (defautl) 
;; ;; CSV - to zapis do CSV pliku
;; ;; MEMORY - najszybszy sposób, bo trzyma tabele wa ramie. Je¶li potrzebujesz na szybko w RAM
;; (table "config-table" :key-type "varchar(20)" :value-type "BIGINT" :engine "InnoDB")
