(ns datomic-schema.schema-test-config
  (:require [clojure.test :refer :all]
            [datomic-schema.schema :refer :all]))

(defn set-up-1 [f]
  (defpart app)
  (f))

(defn set-up-schemas [f]
  (defpart app)

  (defschema user
    (part app)
    (fields
     [username :string :indexed]
     [pwd :string "Hashed password string"]
     [email :string :indexed]
     [status :enum [:pending :active :inactive :cancelled]]
     [group :ref :many]))

  (defschema group
    (part app)
    (fields
     [name :string]
        [permission :string :many]))
  (f))

