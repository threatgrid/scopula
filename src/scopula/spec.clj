(ns scopula.spec
  (:require [scopula.core :as scopula]
            [clojure.spec.alpha :as s]))

(s/def :scopula/scope
  (s/and string? scopula/is-scope-format-valid?))

(s/def :scopula/alias
  (s/and string? scopula/is-scope-alias?))

(s/def :scopula/scopes
  (s/coll-of #(s/valid? :scopula/scope %)
             :kind set?))

(s/def :scopula/aliases-dictionary
  (s/map-of :scopula/alias (s/and seq :scopula/scopes)))

(s/fdef scopula/is-scope-format-valid?
  :args (s/cat :scope string?)
  :ret boolean?)
