(ns datomic-schema.schema-test
  (:require [clojure.test :refer :all]
            [datomic-schema.schema :refer :all]
            [datomic-schema.schema-test-config :as test-config]))

(use-fixtures :once test-config/set-up-1 test-config/set-up-schemas)

(deftest part-test
  (testing "part*"
    (is (= :db.part/does-it-work? (part* :does-it-work?))))
  (testing "part"
    (is (= {:part :db.part/does-it-work?} (part #{:db.part/does-it-work?} :does-it-work?))))
  (testing "part-part*"
    (is (= (part :does-it-work?) (part (part* :does-it-work?))))))

(deftest schema-tests
  (testing "schema*"
    (is (= (schema* :my-schema
                    (part #{:db.part/my-part} :my-part)
                    (fields [username :string :indexed]
                            [pwd :string "Hashed password string"]
                            [email :string :indexed]
                            [status :enum [:pending :active :inactive :cancelled]]
                            [group :ref :many]))
           {:fields {"status" [:enum #{[:pending :active :inactive :cancelled]}], "username" [:string #{:indexed}], "email" [:string #{:indexed}], "pwd" [:string #{"Hashed password string"}], "group" [:ref #{:many}]}, :part :db.part/my-part, :name "my-schema", :basetype :my-schema, :namespace "my-schema"}))))
