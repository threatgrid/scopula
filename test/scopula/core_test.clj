(ns scopula.core-test
  (:require [clojure.test :refer [deftest are is testing]]
            [scopula.core :as sut]))

(deftest is-scope-format-valid-test
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
      (is (sut/is-scope-format-valid? x) err)))

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
      (is (not (sut/is-scope-format-valid? x)) err))))

(deftest scope-root-test
  (is "foo" (sut/scope-root "foo"))
  (is "foo+bar" (sut/scope-root "foo+bar/bar:read"))
  (is "foo" (sut/scope-root "foo/bar"))
  (is "foo" (sut/scope-root "foo/bar:read"))
  (is "foo" (sut/scope-root "foo:read"))
  (is "foo" (sut/scope-root "foo/bar/baz:read"))
  (is "foo-bar" (sut/scope-root "foo-bar/baz:read"))
  (is "foo.bar" (sut/scope-root "foo.bar/baz:read")))

(deftest is-sub-list-test
  (is (sut/is-sub-list? ["a"] ["a" "b"]))
  (is (sut/is-sub-list? ["a" "b"] ["a" "b"]))
  (is (not (sut/is-sub-list? ["a" "b"] ["a"]))))

(deftest is-subscopes-test
  (is (sut/is-subscope? "foo" "foo"))
  (is (sut/is-subscope? "foo:read" "foo"))
  (is (sut/is-subscope? "foo/bar:read" "foo"))
  (is (sut/is-subscope? "foo/bar:read" "foo/bar"))
  (is (sut/is-subscope? "foo/bar:read" "foo:read"))

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

(deftest add-scope-test
  (is (= #{"foo" "bar"}
         (sut/add-scope "bar" #{"foo"})))

  (is (= #{"foo"}
         (sut/add-scope "foo:read" #{"foo:write"})))

  (is (= #{"foo/bar" "root"}
         (sut/add-scope "foo/bar:read" #{"foo/bar" "root"}))
      "Should add scopes and take care of normalization"))

(deftest scopes-union-test
  (is (= #{"root2" "foo/bar" "root1"}
         (sut/scope-union #{"foo/bar:read" "root2"}
                          #{"foo/bar:write" "root1"}))
      "Should union the scopes and take care of normalization"))

(deftest scope-disj-test
  (is (= #{}
         (sut/scope-disj #{"foo/bar" "foo/baz:read"} "foo")))

  (is (= #{"foo/baz:read"}
         (sut/scope-disj #{"foo/bar" "foo/baz:read"} "foo/bar")))

  (is (= #{"foo/bar:write"}
         (sut/scope-disj #{"foo/bar"} "foo:read")))

  (is (= {:ex-msg
          "We can't remove a sub subscope of some other scope (access part is still supported)",
          :ex-data
          {:ex-origin :scopula.core/scopula,
           :ex-type :scopula.core/impossible-sub-scope-removal,
           :ex-full-msg
           "By nature we cannot remove a subscope from another scope. You tried to remove foo/bar/quux but this conflict with foo/bar",
           :scope "foo/bar/quux",
           :conflicting-scope "foo/bar"}}
         (try (sut/scope-disj #{"foo/bar" "foo/baz:read"} "foo/bar/quux")
              (catch Exception e
                {:ex-msg (.getMessage e)
                 :ex-data (ex-data e)})))))

(deftest scope-difference-test
  (is (= #{} (sut/scope-difference #{"foo:read"}
                                   #{"foo:read"})))

  (is (= #{"baz"}
         (sut/scope-difference #{"foo" "bar" "baz"}
                               #{"foo" "bar"})))

  ;; notice how this does not provide the same result as scopes-missing
  (is (= #{"foo/foo-1:write"}
         (sut/scope-difference #{"foo:read" "foo/foo-1"}
                               #{"foo:read"})))

  (is (= #{"baz" "bar/bar-1:write"}
         (sut/scope-difference #{"foo" "bar/bar-1" "baz"}
                               #{"foo" "bar:read"})))

  (is (= #{"foo/bar"} (sut/scope-difference
                       #{"foo/bar:read"
                         "foo/bar:write"
                         "baz/quux"}
                       #{"baz:read"
                         "baz:write"}))
      "Should take care of normalization on both inputs and outputs")

  (is (= {:ex-msg
          "We can't remove a sub subscope of some other scope (access part is still supported)",
          :ex-data
          {:ex-origin :scopula.core/scopula,
           :ex-type :scopula.core/impossible-sub-scope-removal,
           :ex-full-msg
           "By nature we cannot remove a subscope from another scope. You tried to remove foo/foo-1/sub:read but this conflict with foo/foo-1",
           :scope "foo/foo-1/sub:read",
           :conflicting-scope "foo/foo-1"}}
         (try (sut/scope-difference #{"foo/foo-1"}
                                    #{"foo/foo-1/sub:read"})
              (catch Exception e
                {:ex-msg (.getMessage e)
                 :ex-data (ex-data e)}))))

  (is (= {:ex-msg
          "We can't remove a sub subscope of some other scope (access part is still supported)",
          :ex-data
          {:ex-origin :scopula.core/scopula,
           :ex-type :scopula.core/impossible-sub-scope-removal,
           :ex-full-msg
           "By nature we cannot remove a subscope from another scope. You tried to remove foo/foo-1/sub:read but this conflict with foo/foo-1",
           :scope "foo/foo-1/sub:read",
           :conflicting-scope "foo/foo-1"}}
         (try (sut/scope-difference #{"foo/foo-1" "bar"}
                                    #{"foo/foo-1/sub:read"})
              (catch Exception e
                {:ex-msg (.getMessage e)
                 :ex-data (ex-data e)})))))

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

(deftest scopes-missing-test
  (is (= #{"foo/foo-1"}
         (sut/scopes-missing #{"foo:read" "foo/foo-1"}
                                #{"foo:read"})))

  (is (= #{} (sut/scopes-missing #{"foo:read"}
                                    #{"foo:read"})))
  (is (= #{"baz"}
         (sut/scopes-missing #{"foo" "bar" "baz"}
                                #{"foo" "bar"})))
  (is (= #{"baz" "bar/bar-1"}
         (sut/scopes-missing #{"foo" "bar/bar-1" "baz"}
                                #{"foo" "bar:read"}))))

(deftest scope-intersection-test
  (is (= "foo/bar:write"
         (sut/scope-intersection "foo:write" "foo/bar:write")))
  (is (nil? (sut/scope-intersection "foo" "bar"))))

(deftest scopes-interception-test
  (is (= #{"foo/bar:write"}
         (sut/scopes-intersection #{"foo:write" "bar:read"}
                                  #{"foo/bar" "bar:write"})))
  (is (= #{"bar" "foo/bar:write"}
         (sut/scopes-intersection #{"foo:write" "bar:read" "bar:write"}
                                  #{"foo/bar" "bar"}))
      "Check normalization")
  (is (= #{}
         (sut/scopes-intersection #{"foo:read" "bar:read"}
                                  #{"foo/bar:write" "bar:write"}))))

(deftest scopes-intersect?-test
  (is (not (sut/scopes-intersect? "foo:write" "foo:read")))
  (is (not (sut/scopes-intersect? "foo" "bar")))
  (is (not (sut/scopes-intersect? "foo" "bar/foo")))
  (is (sut/scopes-intersect? "foo" "foo"))
  (is (sut/scopes-intersect? "foo/bar" "foo"))
  (is (sut/scopes-intersect? "foo" "foo/bar"))
  (is (sut/scopes-intersect? "foo:write" "foo/bar"))
  (is (sut/scopes-intersect? "foo/bar" "foo:write")))

(deftest scopes-intercepting-test
  (is (= #{"foo:write"}
         (sut/scopes-intersecting #{"foo:write" "bar:read"}
                                  #{"foo/bar" "bar:write"})))
  (is (= #{}
         (sut/scopes-intersecting #{"foo:read" "bar:read"}
                                  #{"foo/bar:write" "bar:write"}))))

(deftest is-scopes-alias?
  (is (sut/is-scope-alias? "+foo"))
  (is (not (sut/is-scope-alias? "foo")))
  (is (not (sut/is-scope-alias? "+foo/bar")))
  (is (not (sut/is-scope-alias? "+foo:read")))
  (is (not (sut/is-scope-alias? "+foo/bar:read"))))

(deftest scopes-expand-test
  (is (= #{"foo:write" "bar"}
         (sut/scopes-expand #{"+admin"} {"+admin" #{"foo:write" "bar"}})))
  (is (= #{"foo:write" "bar" "baz"}
         (sut/scopes-expand #{"+admin" "baz"} {"+admin" #{"foo:write" "bar"}})))
  (is (= #{"foo:write" "bar" "baz" "subrole+x"}
         (sut/scopes-expand #{"+admin" "subrole+x" "baz"} {"+admin" #{"foo:write" "bar"}
                                                           "+x"     #{"x" "y"}})))
  (is (= #{"admin"}
         (sut/scopes-expand #{"admin"} {"admin" #{"foo"}}))
      "scope expansion should only be performed on scope aliases starting with +")

  (testing "missing scope alias"
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/scopes-expand #{"+admin"} {})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (sut/scopes-expand #{"+admin"} {"admin" #{"foo"}})))))

(deftest safe-scopes-expand-test
  (is (= #{"foo:write" "bar"}
         (sut/safe-scopes-expand #{"+admin"} {"+admin" #{"foo:write" "bar"}})))
  (is (= #{"foo:write" "bar" "baz"}
         (sut/safe-scopes-expand #{"+admin" "baz"} {"+admin" #{"foo:write" "bar"}})))
  (is (= #{"foo:write" "bar" "baz" "subrole+x"}
         (sut/safe-scopes-expand #{"+admin" "subrole+x" "baz"} {"+admin" #{"foo:write" "bar"}
                                                           "+x"     #{"x" "y"}})))
  (is (= #{"admin"}
         (sut/safe-scopes-expand #{"admin"} {"admin" #{"foo"}}))
      "scope expansion should only be performed on scope aliases starting with +")

  (testing "missing scope alias"
    (is (nil?(sut/safe-scopes-expand #{"+admin"} {})))
    (is (nil? (sut/safe-scopes-expand #{"+admin"} {"admin" #{"foo"}})))))

(deftest scopes-length-test
  (is (= 0 (sut/scopes-length #{})))
  (is (= 3 (sut/scopes-length #{"foo"})))
  (is (= 9 (sut/scopes-length #{"foo" "bar" "baz"})))
  (is (= 22 (sut/scopes-length #{"foo/bar/baz" "foo" "foo:read"})))
  (is (= 11 (sut/scopes-length #{"foo-bar-baz"}))))

(deftest scopes-compress-test
  (is (= #{"+admin" "baz"}
         (sut/scopes-compress #{"foo" "bar" "baz"}
                              {"+admin" #{"foo" "bar"}
                               "+foo" #{"foo"}}))
      "This test check that the biggest matching alias is preferred to improve compression")
  (is (= #{"+admin" "+baz" "x"}
         (sut/scopes-compress #{"foo" "bar" "baz" "x"}
                              {"+admin" #{"foo" "bar"}
                               "+baz" #{"baz"}})))

  (is (= #{"+admin" "+baz" "baz:write" "x"}
         (sut/scopes-compress #{"foo" "bar" "baz" "x"}
                              {"+admin" #{"foo" "bar"}
                               "+baz" #{"baz:read"}})))
  (is (= #{"foo" "bar" "baz" "+admin"}
         (sut/scopes-compress #{"foo" "bar" "baz" "x" "very-very-long-scope-name"}
                              {"+admin" #{"x" "very-very-long-scope-name"}
                               "+baz" #{"foo" "bar" "x"}}))
      (str "Example of potentially missing an opportunity to compress,"
           " but show that the length of the string of scopes is more important"
           " than the number of scopes."
           " This is still a pretty good-enough for most intended use cases.")))
