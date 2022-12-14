(ns scopula.spec
  (:require [scopula.core :as scopula]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]))

;; data schemas
(s/def :scopula/scope
  (s/and string? scopula/is-scope-format-valid?))


(s/def :scopula/root-scope
  (s/and string? scopula/is-scope-repr-path-valid?))

(s/def :scopula/path
  (s/and
   (s/coll-of string? :kind vector?)
   (s/cat :root-scope scopula/is-scope-repr-path-valid?
          :sub-scopes (s/* scopula/is-scope-repr-subpath-valid?))))

(s/def :scopula/access
  (s/coll-of #{:read :write} :kind set?))

(s/def :scopula/scope-repr
  (s/keys :req-un [:scopula/path :scopula/access]))

(s/def :scopula/alias
  (s/and string? scopula/is-scope-alias?))

(s/def :scopula/scopes
  (s/coll-of #(s/valid? :scopula/scope %)
             :kind set?))

(s/def :scopula/scope-or-alias
  (s/or :scopula/scope :scopula/alias))

(s/def :scopula/extended-scopes
  (s/coll-of :scopula/scope-or-alias
             :kind set?))

(s/def :scopula/aliases-dictionary
  (s/map-of :scopula/alias (s/and seq :scopula/scopes)))

;; functions specification
(s/fdef scopula/is-scope-format-valid?
  :args (s/cat :scope string?)
  :ret boolean?)

(s/fdef scopula/to-scope-repr
  :args (s/cat :scope :scopula/scope)
  :ret :scopula/scope-repr)

(s/fdef scopula/scope-root
  :args (s/cat :scope :scopula/scope)
  :ret :scopula/root-scope)

(s/fdef scopula/scopes-expand
  :args (s/cat :scopes :scopula/extended-scopes
               :aliases-dict :scopula/aliases-dictionary)
  :ret :scopula/scopes)

;; validate all fn specs!
(defn enable-scopula-spec-validation!
  "This helper wil activate spec validation for the scopula.core functions.

  You generally only want to do that for tests, but this could be useful in a deployed environment."
  []
  (doseq [instrumentable-scopula-symbol
          (->> (stest/instrumentable-syms)
               (filter #(= "scopula.core" (namespace %))))]
    (stest/instrument instrumentable-scopula-symbol)))

