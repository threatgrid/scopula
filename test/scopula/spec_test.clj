(ns scopula.spec-test
  (:require [clojure.test :refer [deftest are is testing]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [scopula.spec :as sut]
            [scopula.core :as core]))

(deftest scope-test
  (let [positives
        [["foo" "root scope only"]
         ["foo/bar" "scope with subscope"]
         ["foo-bar" "dash in root-scope"]
         ["foo.bar" "dot in root-scope"]
         ["foo/bar:read" ":read"]
         ["foo/bar:write" ":write"]
         ["foo/bar:rw" ":rw"]
         ["foo+bar:rw" "scope can contain a + but not start with a +, which is reserved to aliases"]
         ["foo/+bar:rw" "a subscope could start with + and will not be an alias"]
         ["foo/bar@hsome.dns/sub/url" "kind of uri-like scope"]]]
    (doseq [[x err] positives]
      (is (s/valid? :scopula/scope x) err)))

  (let [negatives
        [["+foo" "if it starts with + this is an alias"]
         ["oo:lo" "bad extension"]
         [":o:lo" "bad root scope"]
         ["f\"oo" "RFC prevent the use of \" char"]
         ["f\\oo" "RFC prevent the use of \\ char"]
         ["foo/bar:query" nil]
         ["foo/bar query" nil]
         ["foo/bar\nquery" nil]
         ["https://hsome.dns/sub/url" "The : in the url si not supported"]]]
    (doseq [[x err] negatives]
      (is (not (s/valid? :scopula/scope x)) err))))

(deftest alias-test
  (let [positives
        [["+foo" "root scope only"]
         ["+foo-bar" "dash in root-scope"]
         ["+foo.bar" "dot in root-scope"]
         ]]
    (doseq [[x err] positives]
      (is (s/valid? :scopula/alias x) err)))
  (let [negatives
        [["+foo/bar" "aliases cannot contain subscope"]
         ["+foo:read" "aliases cannot have :read"]
         ["+foo:write" "aliases cannot have :write"]
         ["+foo:rw" "aliases cannot have :rw"]
         ["oo:lo" "bad extension"]
         [":o:lo" "bad root scope"]
         ["f\"oo" "RFC prevent the use of \" char"]
         ["f\\oo" "RFC prevent the use of \\ char"]
         ["foo/bar:query" nil]
         ["foo/bar query" nil]
         ["foo/bar\nquery" nil]
         ["https://hsome.dns/sub/url" "The : in the url si not supported"]]]
    (doseq [[x err] negatives]
      (is (not (s/valid? :scopula/alias x)) err))))


(deftest scopes-test
  (let [positives
        [[#{} "empty set"]
         [#{"foo"} "single root scope"]
         [#{"foo" "foo/bar"} "root scope + subscope with conflict"]
         [#{"foo" "bar"} "two root scopes"]
         [#{"foo" "bar/baz"} "two scopes"]
         ]]
    (doseq [[x err] positives]
      (is (s/valid? :scopula/scopes x) err)))
  (let [negatives
        [[#{"+foo"} "contain an alias"]
         [["foo"] "vector"]
         ['("foo") "list"]
         [#{:foo "bar"} "does not contains a string"]
         [#{42 "bar"} "does not contains a string"]
         [#{'(\f \o \o) "bar"} "does not contains a string"]
         ]]
    (doseq [[x err] negatives]
      (is (not (s/valid? :scopula/scopes x)) err))))

(deftest aliases-dictionary-test
  (let [positives
        [[{} "empty case"]
         [{"+admin" #{"admin" "user"}} "normal use case with a single scope alias dictionary"]
         [{"+admin" #{"admin" "user"}
           "+user" #{"user:read"}}
          "two aliases"]]]
    (doseq [[x err] positives]
      (is (s/valid? :scopula/aliases-dictionary x) err)))
  (let [negatives
        [[{"+admin" #{} "+user" #{"user:read"}} "empty set of aliases"]
         [{"admin" #{"admin" "user"}} "not using an alias name as key"]
         [{"+admin" #{"admin" "+user"} "+user" #{"user:read"}} "contain another alias"]
         [{"+admin" #{"+admin" "user"} "+user" #{"user:read"}} "contain itself recursively"]]]
    (doseq [[x err] negatives]
      (is (not (s/valid? :scopula/aliases-dictionary x)) err))))

(defmacro fail-spec?
  [x & body]
  `
  (let [x# ~x]
    (try
      (do ~@body)
      (is false (str "case " x# " did not break spec"))
      (catch Exception e#
        (is (= :instrument
               (-> e# ex-data :clojure.spec.alpha/failure))
            (str "case " x# " thrown an exception but did not break spec."))))))

(deftest is-scope-format-valid-spec-test
  (stest/instrument `core/is-scope-format-valid?)
  (let [negatives
        [nil
         42
         :k
         [\f \o \o]
         'x]]
    (doseq [x negatives]
      (fail-spec? x (core/is-scope-format-valid? x)))))
