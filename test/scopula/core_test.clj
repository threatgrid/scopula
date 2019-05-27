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
