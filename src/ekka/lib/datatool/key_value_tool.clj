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
(def *available-mariadb-engine-list* "set of available engines for key-value tables" ["MEMORY",
                                                                                      "InnoDB",
                                                                                      "CSV"])


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

(defn get-table [table-name]
  (println (create-table table-name :key-type "varchar(20)" :engine "memory"))
  (apply ->TableAccessors (map #(% table-name) [#'create-getter #'create-setter #'create-destroyer #'create-creater #'create-cleaner #'create-keys #'create-values])))

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



