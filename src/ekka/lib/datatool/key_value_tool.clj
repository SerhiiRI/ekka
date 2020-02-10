(ns ekka.lib.datatool.key-value-tool
  (:require
   [clojure.string :as string]
   ;; [ekka.lib.datatool.sql-tool :as toolbox :refer [where-string]]
   [ekka.lib.datatool.sql-tool :as toolbox :include-macros true
    :refer [where-string]]))


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
       (format "CREATE TABLE IF NOT EXIST `%s` %s" table-name 
               (toolbox/into-border (string/join ", " columns)))))))

(defn create-table [table-name & {:keys [engine key-type value-type]
                                  :or {key-type *key-column-default-type*
                                       value-type *value-column-default-type*} }]
  (format "%s %s" (create-pair-table table-name key-type value-type)
          (format "ENGINE=%s DEFAULT CHARSET=utf8;"
                  (if (some #(= % engine) *available-mariadb-engine-list*)
                    engine "InnoDB"))))

;;; to test 
(create-table "suka" :key-type "VARCHAR(20)" :engine "MEMORY")
(create-table "suka")


(defmacro get-key [key]
  `(toolbox/select :suka
                   :where `(= :key ~key)))

(get-key "suak")




(defmacro suka [k]
  `(toolbox/select :suka
                   :where (= :key ~k)))

(suka "123")




