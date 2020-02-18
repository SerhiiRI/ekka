(ns ekka.lib.datatool.user-module
  (:require
   [ekka.lib.datatool.sql-tool :as toolbox :include-macros true :refer :all]
   [clojure.string :as string]
   [clojure.java.jdbc :as jdbc]))




;; (def ^:dynamic mysql-db {:dbtype "mysql"
;;                          :dbname "ekka-test"
;;                          :user "root"
;;                          :password "123"})

(def ^:dynamic mysql-db {:dbtype "mysql"
                         :dbname "ekka-g1"
                         :user "root"
                         :password "123"})


(mysql-db (select :user
                  :inner-join {:permission :id_permission}))

;; (insert :user
;;         :values {:login "aleks-front" :password "123" :first_name "Aleksandr" :last_name "Sinkowski" :id_permission 2})

;; (insert :user
;;         :values {:id 1 :login "vasia" :password "123" :age 20})

;; (insert :user
;;         :values [[1, "vasia", "123", 20] [2, "vasia", "123", 20]])

;; (insert :user
;;         :values [{:id 1 :login "vasia" :password "123" :age 20} {:id 1 :login "vasia" :password "123" :age 20}])



;; (values-string "" {:a 1 :b 2 :c 3} "mitinko")
;; (values-string "" [{:a 1 :b 2 :c 3} {:a 4 :b 5 :c 6}] "mitinko")
;; (values-string "" [[1 1 1] [2 2 2]] "mitinko")
;; (values-string "" [1 1 1] "mitinko")

;; (defrecord User [login password permission_name configuration])

;; (map->User {})

;; (jdbc/execute! mysql-db (insert :user
;;         :values {:login "aleks-front" :password "123" :first_name "Aleksandr" :last_name "Sinkowski" :id_permission 2}))

;; (map :login (jdbc/query mysql-db (select :user
;;                                          :inner-join :permission)))




;; (jdbc/query mysql-db (select :user
;;                              :inner-join {:permission :id_permission}))

;; (jdbc/query mysql-db (select :user))




;;; Table generation script

;; (jdbc/execute! mysql-db (toolbox/create-database "ekka-g2"))

;; (jdbc/execute! mysql-db (toolbox/drop-database "ekka-g2"))

;; (binding [mysql-db (assoc-in mysql-db [:dbname] "ekka-g2")]
;;   (jdbc/execute! mysql-db enterpreneur)
;;   (jdbc/execute! mysql-db point_of_sale)
;;   (jdbc/execute! mysql-db cache_register)
;;   (jdbc/execute! mysql-db point_of_sale_group)
;;   (jdbc/execute! mysql-db point_of_sale_group_links)
;;   (jdbc/execute! mysql-db repair_contract)
;;   (jdbc/execute! mysql-db seals)
;;   (jdbc/execute! mysql-db service_contract)
;;   (jdbc/execute! mysql-db permission)
;;   (jdbc/execute! mysql-db user))




;; (jdbc/query mysql-db "SHOW DATABASES")
;; (jdbc/execute! mysql-db "CREATE DATABASE ekka_g2 CHARACTER SET = 'utf8' COLLATE = 'utf8_general_ci';")
(binding [mysql-db (assoc-in mysql-db [:dbname] "ekka-g2")]
  (jdbc/execute! mysql-db permission)
  (jdbc/execute! mysql-db user)
  (jdbc/execute! mysql-db enterpreneur)
  (jdbc/execute! mysql-db point_of_sale)
  (jdbc/execute! mysql-db cache_register)
  (jdbc/execute! mysql-db point_of_sale_group)
  (jdbc/execute! mysql-db point_of_sale_group_links)
  (jdbc/execute! mysql-db repair_contract)
  (jdbc/execute! mysql-db seal)
  (jdbc/execute! mysql-db service_contract)  )

(binding [mysql-db (assoc-in mysql-db [:dbname] "ekka-g2")]
  (jdbc/execute! mysql-db drop-all-table))


(def drop-all-table "" "DROP TABLE user, permission, service_contract, seal, repair_contract, point_of_sale_group_links, point_of_sale_group, cache_register, point_of_sale, enterpreneur;
")

;; (create-table :permission
;;               :columns [{:permission_name [:varchar-20 :default :null]}
;;                         {:configuration [:tinytext :nnull :default "'{}'"]}])

(def permission
  (create-table :permission
                :columns [{:permission_name [:varchar-20 :default :null]}
                          {:configuration [:tinytext :nnull :default "'{}'"]}]))

(def user
  (create-table :user
                :columns [{:login [:varchar-100 :nnull]}
                          {:password [:varchar-100 :nnull]}
                          {:first_name [:varchar-100 :nnull]}
                          {:last_name [:varchar-100 :nnull]}
                          {:id_permission [:bigint-120-unsigned :nnull]}]
                :foreign-keys [{:id_permission :permission} {:delete :cascade :update :cascade}]))

(def enterpreneur
  (create-table :enterpreneur
                :columns [{:ssreou [:tinytext :nnull]}
                          {:ownership_form [:varchar-100 :default :null]}
                          {:vat_certificate [:tinytext :default :null]}
                          {:individual_tax_number [:varchar-100 :default :null]}
                          {:director [:varchar-100 :default :null]}
                          {:accountant [:varchar-100 :default :null]}
                          {:legal_address [:varchar-100 :default :null]}
                          {:physical_address [:varchar-100 :default :null]}
                          {:contacts_information [:mediumtext :default :null]}]))

(def point_of_sale
  (create-table :point_of_sale
                :columns [{:id_enterpreneur [:bigint-20-unsigned :default :null]}
                          {:name [:varchar-100 :default :null]}
                          {:physical_address  [:varchar-100 :default :null]}
                          {:telefons  [:varchar-100 :default :null]}]
                :foreign-keys [{:id_enterpreneur :enterpreneur} {:update :cascade}]))

(def cache_register
  (create-table :cache_register
                :columns [ {:id_point_of_sale [:bigint-20 :unsigned :default :null]}
                          {:name [:varchar-100 :default :null]}
                          {:serial_number [:varchar-100 :default :null]}
                          {:fiscal_number [:varchar-100 :default :null]}
                          {:manufacture_date [:date :default :null]}
                          {:first_registration_date [:date :default :null]}
                          {:is_working [:tinyint-1 :default :null]}
                          {:version [:varchar-100 :default :null]}
                          {:id_dev [:varchar-100 :default :null]}
                          {:producer [:varchar-100 :default :null]}
                          {:modem [:varchar-100 :default :null]}
                          {:modem_model [:varchar-100 :default :null]}
                          {:modem_serial_number [:varchar-100 :default :null]}
                          {:modem_phone_number [:varchar-100 :default :null]}]
                :foreign-keys [{:id_point_of_sale :point_of_sale} {:delete :cascade :update :cascade}]))

(def point_of_sale_group
  (create-table :point_of_sale_group
                :columns [{:group_name [:varchar-100 :default :null]}
                          {:metadata [:mediumtext :default :null]}]))

(def point_of_sale_group_links
  (create-table :point_of_sale_group_links
                :columns [{:id_point_of_sale_group [:bigint-20-unsigned :default :null]}
                          {:id_point_of_sale [:bigint-20-unsigned :default :null]}]
                :foreign-keys [[{:id_point_of_sale_group :point_of_sale_group} {:delete :cascade :update :cascade}]
                               [{:id_point_of_sale :point_of_sale}]]))

(def seal
  (create-table :seal
                :columns [{:seal_number [:varchar-100 :default :null]}
                          {:to_date [:date :default :null]}]))

(def service_contract
  (create-table :service_contract
                :columns [{:id_point_of_sale [:bigint-20 :unsigned :default :null]}
                          {:register_contract_date [:date :default :null]}
                          {:contract_term_date [:date :default :null]}
                          {:money_per_month [:int-11 :default :null]}]
                :foreign-keys [{:id_point_of_sale :point_of_sale} {:delete :cascade :update :cascade}]))

(def repair_contract
  (create-table :repair_contract
                :columns [{:id_cache_register [:bigint-20 :unsigned :default :null]}
                          {:id_point_of_sale [:bigint-20 :unsigned :default :null]}
                          {:creation_contract_date [:date :default :null]}
                          {:last_change_contract_date [:date :default :null]}
                          {:contract_terms_date [:date :default :null]}
                          {:cache_register_register_date [:date :default :null]}
                          {:remove_security_seal_date [:datetime :default :null]}
                          {:cause_of_removing_seal [:mediumtext :default :null]}
                          {:technical_problem [:mediumtext :default :null]}
                          {:active_seal [:mediumtext :default :null]}]
                :foreign-keys [[{:id_cache_register :cache_register} {:delete :cascade :update :cascade}]
                               [{:id_point_of_register :point_of_sale} {:delete :cascade :update :cascade}]]))

;; (def user "" "
;; CREATE TABLE IF NOT EXISTS `user` (
;;   `id` bigint(20) NOT NULL AUTO_INCREMENT,
;;   `login` varchar(100) NOT NULL,
;;   `password` varchar(255) NOT NULL,
;;   `first_name` varchar(100) NOT NULL,
;;   `last_name` varchar(100) NOT NULL,
;;   `id_permission` bigint(20) unsigned NOT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `user_FK` (`id_permission`),
;;   CONSTRAINT `user_FK` FOREIGN KEY (`id_permission`) REFERENCES `permission` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
;; ) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8;
;; ")


;; (def permission "" "
;; CREATE TABLE IF NOT EXISTS `permission` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `permission_name` varchar(20) DEFAULT NULL,
;;   `configuration` tinytext NOT NULL DEFAULT '{}',
;;   PRIMARY KEY (`id`)
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_general_ci;")


;; (def enterpreneur "" "
;; CREATE TABLE IF NOT EXISTS `entrepreneur` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `ssreou` tinytext NOT NULL,
;;   `ownership_form` varchar(100) DEFAULT NULL,
;;   `vat_certificate` tinytext DEFAULT NULL,
;;   `individual_tax_number` varchar(100) DEFAULT NULL,
;;   `director` varchar(100) DEFAULT NULL,
;;   `accountant` varchar(100) DEFAULT NULL,
;;   `legal_address` varchar(100) DEFAULT NULL,
;;   `physical_address` varchar(100) DEFAULT NULL,
;;   `contacts_information` mediumtext DEFAULT NULL,
;;   PRIMARY KEY (`id`)
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;")


;; (def point_of_sale "" "
;; CREATE TABLE IF NOT EXISTS `point_of_sale` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `id_entrepreneur` bigint(20) unsigned DEFAULT NULL,
;;   `name` varchar(100) DEFAULT NULL,
;;   `physical_address` varchar(100) DEFAULT NULL,
;;   `telefons` varchar(100) DEFAULT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `point_of_sale_FK` (`id_entrepreneur`),
;;   CONSTRAINT `point_of_sale_FK` FOREIGN KEY (`id_entrepreneur`) REFERENCES `entrepreneur` (`id`) ON UPDATE CASCADE
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ")

(re-seq #"\`(.*?)\`" "
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `id_point_of_sale` bigint(20) unsigned DEFAULT NULL,
  `name` varchar(100) DEFAULT NULL,
  `serial_number` varchar(100) DEFAULT NULL,
  `fiscal_number` varchar(100) DEFAULT NULL,
  `manufacture_date` date DEFAULT NULL,
  `first_registration_date` date DEFAULT NULL,
  `is_working` tinyint(1) DEFAULT NULL,
  `version` varchar(100) DEFAULT NULL,
  `id_dev` varchar(100) DEFAULT NULL,
  `producer` varchar(100) DEFAULT NULL,
  `modem` varchar(100) DEFAULT NULL,
  `modem_model` varchar(100) DEFAULT NULL,
  `modem_serial_number` varchar(100) DEFAULT NULL,
  `modem_phone_number` varchar(100) DEFAULT NULL,
")

(string/replace 
 #"\n" "")

(let [llist (map (comp string/trim (fn [x] (string/replace x #"\n" ""))) (string/split "
  `id_point_of_sale` bigint(20) unsigned DEFAULT NULL,
  `register_contract_date` date DEFAULT NULL,
  `contract_term_date` date DEFAULT NULL,
  `money_per_month` int(11) DEFAULT NULL" #","))
      fff (map #(first (re-find #"\`(.*?)\`" %)) llist)
      ppp (map #(second (re-find #"\`(.*?)\`" %)) llist)]
  (vec (map #(apply hash-map %) (map #(vector (keyword %3) (vec (map keyword (string/split (string/lower-case (string/trim (second (string/split %1 (re-pattern %2))))) #" ")))) llist fff ppp))))






;; (def cache_register "" "
;; CREATE TABLE IF NOT EXISTS `cache_register` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `id_point_of_sale` bigint(20) unsigned DEFAULT NULL,
;;   `name` varchar(100) DEFAULT NULL,
;;   `serial_number` varchar(100) DEFAULT NULL,
;;   `fiscal_number` varchar(100) DEFAULT NULL,
;;   `manufacture_date` date DEFAULT NULL,
;;   `first_registration_date` date DEFAULT NULL,
;;   `is_working` tinyint(1) DEFAULT NULL,
;;   `version` varchar(100) DEFAULT NULL,
;;   `id_dev` varchar(100) DEFAULT NULL,
;;   `producer` varchar(100) DEFAULT NULL,
;;   `modem` varchar(100) DEFAULT NULL,
;;   `modem_model` varchar(100) DEFAULT NULL,
;;   `modem_serial_number` varchar(100) DEFAULT NULL,
;;   `modem_phone_number` varchar(100) DEFAULT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `cache_register_FK` (`id_point_of_sale`),
;;   CONSTRAINT `cache_register_FK` FOREIGN KEY (`id_point_of_sale`) REFERENCES `point_of_sale` (`id`) ON DELETE SET NULL ON UPDATE SET NULL
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ")




;; (def point_of_sale_group "" "
;; CREATE TABLE IF NOT EXISTS `point_of_sale_group` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `group_name` varchar(100) DEFAULT NULL,
;;   `metadata` mediumtext DEFAULT NULL,
;;   PRIMARY KEY (`id`)
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ")





;; (def point_of_sale_group_links "" "
;; CREATE TABLE IF NOT EXISTS `point_of_sale_group_links` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `id_point_of_sale_group` bigint(20) unsigned DEFAULT NULL,
;;   `id_point_of_sale` bigint(20) unsigned DEFAULT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `point_of_sale_group_links_FK` (`id_point_of_sale_group`),
;;   KEY `point_of_sale_group_links_FK_1` (`id_point_of_sale`),
;;   CONSTRAINT `point_of_sale_group_links_FK` FOREIGN KEY (`id_point_of_sale_group`) REFERENCES `point_of_sale_group` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
;;   CONSTRAINT `point_of_sale_group_links_FK_1` FOREIGN KEY (`id_point_of_sale`) REFERENCES `point_of_sale` (`id`)
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ")




;; (def repair_contract "" "
;; CREATE TABLE IF NOT EXISTS `repair_contract` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `id_cache_register` bigint(20) unsigned DEFAULT NULL,
;;   `id_point_of_sale` bigint(20) unsigned DEFAULT NULL,
;;   `creation_contract_date` date DEFAULT NULL,
;;   `last_change_contract_date` date DEFAULT NULL,
;;   `contract_terms_date` date DEFAULT NULL,
;;   `cache_register_register_date` date DEFAULT NULL,
;;   `remove_security_seal_date` datetime DEFAULT NULL,
;;   `cause_of_removing_seal` mediumtext DEFAULT NULL,
;;   `technical_problem` mediumtext DEFAULT NULL,
;;   `active_seal` mediumtext DEFAULT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `repair_contract_FK` (`id_cache_register`),
;;   KEY `repair_contract_FK_1` (`id_point_of_sale`),
;;   CONSTRAINT `repair_contract_FK` FOREIGN KEY (`id_cache_register`) REFERENCES `cache_register` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
;;   CONSTRAINT `repair_contract_FK_1` FOREIGN KEY (`id_point_of_sale`) REFERENCES `point_of_sale` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ")



;; (def seals "" "
;; CREATE TABLE IF NOT EXISTS `seal` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `sea_number` varchar(100) DEFAULT NULL,
;;   `to_date` date DEFAULT NULL,
;;   PRIMARY KEY (`id`)
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ")




;; (def service_contract "" "
;; CREATE TABLE IF NOT EXISTS `service_contract` (
;;   `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
;;   `id_point_of_sale` bigint(20) unsigned DEFAULT NULL,
;;   `register_contract_date` date DEFAULT NULL,
;;   `contract_term_date` date DEFAULT NULL,
;;   `money_per_month` int(11) DEFAULT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `service_contract_FK` (`id_point_of_sale`),
;;   CONSTRAINT `service_contract_FK` FOREIGN KEY (`id_point_of_sale`) REFERENCES `point_of_sale` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; ")



;; (def user "" "
;; CREATE TABLE IF NOT EXISTS `user` (
;;   `id` bigint(20) NOT NULL AUTO_INCREMENT,
;;   `login` varchar(100) NOT NULL,
;;   `password` varchar(255) NOT NULL,
;;   `first_name` varchar(100) NOT NULL,
;;   `last_name` varchar(100) NOT NULL,
;;   `id_permission` bigint(20) unsigned NOT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `user_FK` (`id_permission`),
;;   CONSTRAINT `user_FK` FOREIGN KEY (`id_permission`) REFERENCES `permission` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
;; ) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8;
;; ")


(def drop-all-table "" "
;; SET FOREIGN_KEY_CHECKS = 0;
DROP TABLE IF EXIST user, permission, service_contract, seals, repair_contract, point_of_sale_group_links, point_of_sale_group, cache_register, point_of_sale, enterpreneur;
")





