(ns ekka.lib.datatool.key-value-tool
  (:require
   [clojure.string :as string]
   [ekka.lib.datatool.sql-tool :as toolbox]
    ))



;; *!40101 SET @saved_cs_client     = @@character_set_client */;
;; /*!40101 SET character_set_client = utf8 */;
;; CREATE TABLE IF NOT EXISTS `user` (
;;   `id` bigint(20) NOT NULL AUTO_INCREMENT,
;;   `login` varchar(100) NOT NULL,
;;   `password` varchar(255) NOT NULL,
;;   `first_name` varchar(100) NOT NULL,
;;   `last_name` varchar(100) NOT NULL,
;;   `id_permission` bigint(20) unsigned NOT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `user_FK` (`id_permission`),
;;   CONSTRAINT `user_FK` FOREIGN KEY (`id_permission`) REFERENCES `permission` (`id`) ON UPDATE CASCADE
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;
;; /*!40101 SET character_set_client = @saved_cs_client */;


;; (toolbox/where)


