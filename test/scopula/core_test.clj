(ns scopula.core-test
  (:require [clojure.test :refer [deftest are is testing]]
            [scopula.core :as sut]))

(deftest is-sub-list-test
  (is (sut/is-sub-list? ["a"] ["a" "b"]))
  (is (sut/is-sub-list? ["a" "b"] ["a" "b"]))
  (is (not (sut/is-sub-list? ["a" "b"] ["a"]))))

(deftest is-subscopes-test
  (is (sut/is-subscope? "foo" "foo"))
  (is (sut/is-subscope? "foo:read" "foo"))

  (is (not (sut/is-subscope? "root/foo" "foo"))))

(deftest accepted-by-scopes-test

  (is (sut/accepted-by-scopes
       #{(sut/to-scope-repr "enrich")
         (sut/to-scope-repr "auth")}
       #{(sut/to-scope-repr "enrich")
         (sut/to-scope-repr "auth")}))

  (is (sut/accepted-by-scopes
       #{(sut/to-scope-repr "enrich")
         (sut/to-scope-repr "auth")}
       #{(sut/to-scope-repr "enrich")
         (sut/to-scope-repr "auth:read")}))

  (is (not (sut/accepted-by-scopes
            #{}
            #{(sut/to-scope-repr "enrich")
              (sut/to-scope-repr "auth")}))))

(deftest access-granted-test
  (testing "subset is accepted"
    (is (sut/access-granted #{"foo"} #{"foo"})
        "an identical set of scopes should match")
    (is (not (sut/access-granted #{"foo"} #{"foo" "bar"}))
        "A single scope when two are required should not be accepted")
    (is (not (sut/access-granted #{"bar"} #{"foo"})))
    (is (sut/access-granted #{"foo" "bar"} #{"foo"}))
    (is (sut/access-granted #{"foo" "bar"} #{"foo" "bar"}))
    (is (not (sut/access-granted #{"foo" "bar"} #{"foo" "bar" "baz"}))))
  (testing "superpath are accepted"
    (is (not (sut/access-granted #{"foo/bar"} #{"foo"})))
    (is (not (sut/access-granted #{"foo/bar/baz"} #{"foo"})))
    (is (not (sut/access-granted #{"foobar/baz"} #{"foo"}))))
  (testing "access are respected"
    (is (sut/access-granted #{"foo"}      #{"foo/bar:read"}     ))
    (is (sut/access-granted #{"foo"}      #{"foo/bar/baz:write"}))
    (is (sut/access-granted #{"foo"}      #{"foo/bar/baz:rw"}   ))
    (is (sut/access-granted #{"foo"}      #{"foo/bar/baz:rw"}   ))
    (is (sut/access-granted #{"foo:read"} #{"foo/bar/baz:read"} ))
    (is (not (sut/access-granted #{"foo:read"} #{"foo/bar/baz:write"})))
    (is (sut/access-granted #{"foo" "bar"} #{"foo/bar:read"}))
    (is (sut/access-granted #{"foo" "bar"}      #{"foo/bar/baz:write"}))
    (is (sut/access-granted #{"foo" "bar"}      #{"foo/bar/baz:rw"}   ))
    (is (sut/access-granted #{"foo" "bar"}      #{"foo/bar/baz:rw"}   ))
    (is (sut/access-granted #{"foo:read" "bar"} #{"foo/bar/baz:read"} ))
    (is (not (sut/access-granted #{"foo:read" "bar"} #{"foo/bar/baz:write"})))
    (is (sut/access-granted #{"foo" "bar"} #{"foo/bar:read" "bar"}     ))
    (is (sut/access-granted #{"foo" "bar"} #{"foo/bar/baz:write" "bar"}))
    (is (sut/access-granted #{"foo" "bar"} #{"foo/bar/baz:rw" "bar"}   ))
    (is (sut/access-granted #{"foo" "bar"} #{"foo/bar/baz:rw" "bar"}   ))
    (is (sut/access-granted #{"foo:read" "bar"} #{"foo/bar/baz:read" "bar"}))
    (is (not (sut/access-granted #{"foo:read" "bar"} #{"foo/bar/baz:write" "bar"})))))


(deftest root-scope-test
  (is (= "foo" (sut/root-scope "foo")))
  (is (= "foo" (sut/root-scope "foo:read")))
  (is (= "foo" (sut/root-scope "foo/bar:read"))))

(deftest is-root-scope-test
  (is (sut/is-root-scope? "foo"))
  (is (sut/is-root-scope? "foo:read"))
  (is (not (sut/is-root-scope? "foo/bar:read")))
  (is (not (sut/is-root-scope? "foo/bar")))
  (is (not (sut/is-root-scope? "foo/bar/baz"))))

(deftest normalize-scopes-test
  (is (= #{"foo/bar"}
         (sut/normalize-scopes #{"foo/bar/baz:read"
                                 "foo/bar:write"
                                 "foo/bar"})))
  (is (= #{"foo/bar"}
         (sut/normalize-scopes #{"foo/bar:read"
                                 "foo/bar:write"
                                 "foo/bar/tux"}))
      "Should take care of making the unions of the accesses and remove subsummed scopes")

  (is (= #{"foo/bar" "root"}
         (sut/normalize-scopes #{"foo/bar:read"
                                 "foo/bar:write"
                                 "foo/bar/tux"
                                 "root"}))
      "Should take care of making the unions of the accesses and remove subsummed scopes"))

(deftest add-scopes-test
  (is (= #{"foo" "bar"}
         (sut/add-scope "bar" #{"foo"})))

  (is (= #{"foo/bar" "root"}
         (sut/add-scope "foo/bar:read" #{"foo/bar:write" "root"}))
      "Should add scopes and take care of normalization"))

(deftest scopes-union-test
  (is (= #{"root2" "foo/bar" "root1"}
         (sut/scope-union #{"foo/bar:read" "root2"}
                          #{"foo/bar:write" "root1"}))
      "Should union the scopes and take care of normalization"))

(deftest raw-remove-root-scope-test
  (is (= #{"baz/quux"}
         (sut/raw-remove-root-scope "foo"
                                    #{"foo/baz:read"
                                      "foo/bar:write"
                                      "baz/quux"})))

  (is (= #{"baz/quux" "foo/baz:read"}
         (sut/raw-remove-root-scope "foo:write"
                                    #{"foo/baz:read"
                                      "foo/bar:write"
                                      "baz/quux"}))
      "Limit to write access removal")

  (is (= #{"foo/bar:write" "baz/quux"}
         (sut/raw-remove-root-scope "foo:read"
                                    #{"foo/bar:write" "baz/quux"
                                      "foo/baz:read"}))
      "Limit to read access removal")

  (is (= {:ex-data {:scope "foo/bar"}
          :ex-msg "We can't remove a sub scope, only root scope can be removed from a set of scopes, note access are supported"}
         (try
           (sut/raw-remove-root-scope "foo/bar"
                                      #{"foo/bar:write" "baz/quux"})
           (catch Exception e
             {:ex-msg (.getMessage e)
              :ex-data (ex-data e)})))
      "Non root scope removal should throw an exception"))

(deftest remove-root-scope-test
  (is (= #{"foo/bar"}
         (sut/remove-root-scope "baz"
                                #{"foo/bar:read"
                                  "foo/bar:write"
                                  "baz/quux"}))
      "Should take care of normalization"))


(deftest remove-root-scopes-test
  (is (= #{"foo/bar"}
         (sut/remove-root-scopes #{"baz:read"
                                   "baz:write"}
                                 #{"foo/bar:read"
                                   "foo/bar:write"
                                   "baz/quux"}))
      "Should take care of normalization on both inputs and outputs"))

(deftest scopes-superset-test
  (testing "root scopes"
    (is (sut/scopes-superset? #{} #{}))
    (is (sut/scopes-superset? #{"foo"} #{}))
    (is (sut/scopes-superset? #{"foo" "bar"} #{}))
    (is (sut/scopes-superset? #{"foo" "bar"} #{"foo"}))
    (is (sut/scopes-superset? #{"foo" "bar"} #{"foo" "bar"}))
    (is (not (sut/scopes-superset? #{"foo" "bar"} #{"foo" "bar" "baz"}))))
  (testing "sub scopes"
    (is (sut/scopes-superset? #{"foo"} #{"foo/foo-1"}))
    (is (sut/scopes-superset? #{"foo"} #{"foo/foo-1:read"}))
    (is (sut/scopes-superset? #{"foo"} #{"foo:read"}))
    (is (sut/scopes-superset? #{"foo"} #{"foo:read" "foo/foo-1"}))
    (is (not (sut/scopes-superset? #{"foo:read"}
                                   #{"foo:read" "foo/foo-1"}))))
  (testing "un-normalized scopes"
    (is (sut/scopes-superset? #{"foo:read" "foo:write"}
                              #{"foo:read" "foo/foo-1"}))))

(deftest scopes-subset-test
  (testing "root scopes"
    (is (sut/scopes-subset? #{} #{}))
    (is (sut/scopes-subset? #{} #{"foo"}))
    (is (sut/scopes-subset? #{} #{"foo" "bar"}))
    (is (sut/scopes-subset? #{"foo"} #{"foo" "bar"}))
    (is (sut/scopes-subset? #{"foo" "bar"} #{"foo" "bar"}))
    (is (not (sut/scopes-subset? #{"foo" "bar" "baz"} #{"foo" "bar"}))))

  (testing "sub scopes"
    (is (sut/scopes-subset? #{"foo/foo-1"} #{"foo"}))
    (is (sut/scopes-subset? #{"foo/foo-1:read"} #{"foo"}))
    (is (sut/scopes-subset? #{"foo:read"} #{"foo"}))
    (is (sut/scopes-subset? #{"foo:read" "foo/foo-1"} #{"foo"}))
    (is (not (sut/scopes-subset? #{"foo:read" "foo/foo-1"}
                                   #{"foo:read"}))))
  (testing "un-normalized scopes"
    (is (sut/scopes-subset? #{"foo:read" "foo/foo-1"}
                            #{"foo:read" "foo:write"}))))

(deftest scopes-difference-test
  (is (= #{"foo/foo-1"}
         (sut/scopes-difference #{"foo:read" "foo/foo-1"}
                                #{"foo:read"})))

  (is (= #{} (sut/scopes-difference #{"foo:read"}
                                    #{"foo:read"})))
  (is (= #{"baz"}
         (sut/scopes-difference #{"foo" "bar" "baz"}
                                #{"foo" "bar"})))
  (is (= #{"baz" "bar/bar-1"}
         (sut/scopes-difference #{"foo" "bar/bar-1" "baz"}
                                #{"foo" "bar:read"}))))
