(ns scopula.core
  "Handle scope logic

  scopes are case sensitive strings without any space that represent and
  authorization access. From OAuth2 RFC (https://tools.ietf.org/html/rfc6749#section-3.3):

  > The value of the scope parameter is expressed as a list of space-
  > delimited, case-sensitive strings.  The strings are defined by the
  > authorization server.  If the value contains multiple space-delimited
  > strings, their order does not matter, and each string adds an
  > additional access range to the requested scope.
  >
  >   scope       = scope-token *( SP scope-token )
  >   scope-token = 1*( %x21 / %x23-5B / %x5D-7E )


  In order to manage a fine grained authorizations this lib use a convention
  for scope formats.
  For example, we often need to distinguish between a full scope that will provide
  full access to some resource, and read-only access.
  Sometime we also want to limit the access to some sub-resource.
  Here are some example for our convention:

  `users`                      full access to users resource
  `users/profile`              access to users profile only
  `users/profile:read`         access to users profile read-only
  `users/profile/email:write`  access to users profile only email write-only


  Mainly `:` is only authorized to split between access read/write/rw
  (nothing implies rw)

  Sub resources are separated by `/` we can

  This library provide helper functions to check that

  users scope will also grants `users/profile/email` and `users/profile:read`

  We also provide helpers to normalize set of scopes:

  >>> (normalize-scopes #{\"users\" \"users/profile/email:read\" \"admin\"})
  #{\"users\" \"admin\"}

  as `users/profile/email:read` is redundant it was removed.

  Note scopes are meant to be used in an OAuth2 access in mind and thus
  are generally manipulated as a set of scopes.

  scopes that do not have any subpath are called _root scopes_.

  This is important because it is easy to add, union scopes.
  But it is generally impossible to remove just a sub-scope as it would
  mean we should know all the sub-paths of some root-scope and add the difference.

  Scope are additive by their nature.
  "
  (:require [clojure
             [set :as set]
             [string :as string]]))

(def scope-regex #"^[^:\s\n]*(/[^:\s\n]*)*(:(read|write|rw))?$")

(defn is-scope-format-valid?
  [scope]
  (re-matches scope-regex scope))

(defn to-scope-repr
  "Transform a textual scope as an internal representation to help
  check rules typically

  > \"foo\"
  {:path [\"foo\"]
   :access #{:read :write}}

  > \"foo/bar/baz:write\"
  {:path [\"foo\" \"bar\" \"baz\"]
   :access #{:write}}"
  [txt]
  (let [[path access] (string/split txt #":")]
    {:path (string/split path #"/")
     :access (case access
               "read"  #{:read}
               "write" #{:write}
               "rw"    #{:read :write}
               nil     #{:read :write}
               #{})}))

(defn scope-root
  "display the root of a scope

  >>> (scope-root \"foo/bar:read\")
  foo
  "
  [scope]
  (-> scope to-scope-repr :path first))

(defn is-sub-list?
  "check scope-path-list starts with req-list"
  [lst super-lst]
  (let [n (count lst)]
    (= (take n super-lst) lst)))

(defn repr-is-subscope?
  "return true if the scope is a subscope of the super scope"
  [scope-to-check super-scope]
  (and (set/superset? (:access super-scope) (:access scope-to-check))
       (is-sub-list? (:path super-scope) (:path scope-to-check))))


(defn is-subscope?
  "return true if the scope-to-check is a subscope of the super-scope"
  [scope-to-check super-scope]
  (repr-is-subscope? (to-scope-repr scope-to-check)
                     (to-scope-repr super-scope)))

(defn scope-repr-to-str
  [{:keys [path access]}]
  (str (string/join "/" path)
       (condp = access
         #{:read} ":read"
         #{:write} ":write"
         "")))

(defn merge-accesses
  [[path reprs]]
  {:path path
   :access (apply set/union (map :access reprs))})

(defn repr-is-subsummed
  "return true if scope is contained by all the scopes

  Examples that return true:

  (is-subsummed \"foo\" #{\"foo\"})
  (is-subsummed \"foo:read\" #{\"foo\"})
  (is-subsummed \"foo/sub/scope\" #{\"foo\"})
  (is-subsummed \"foo/sub/scope:read\" #{\"foo\"})

  Examples that return false:

  (is-subsummed \"foo\" #{\"foo:read\" \"bar\"})
  (is-subsummed \"foo:read\" #{\"foo:read\" \"bar\"})
  (is-subsummed \"foo/sub/scope\" #{\"foo:read\"})"
  [repr-scope repr-scopes]
  (not (every? #(not (repr-is-subscope? repr-scope %)) repr-scopes)))

(defn repr-normalize-scopes
  [scopes-repr]
  (let [ssr (->> scopes-repr
                 (group-by :path)
                 (map merge-accesses)
                 set)]
    (->> ssr
         (filter #(not (repr-is-subsummed % (disj ssr %))))
         set)))

(defn normalize-scopes
  "Given a set of scope remove reduntant ones, and merge by access if possible"
  [scopes]
  (->> scopes
       (map to-scope-repr)
       repr-normalize-scopes
       (map scope-repr-to-str)
       set))

(defn accepted-by-scopes
  "scopes should be strings.
  if none of the string contains a `/` nor a `:`.
  It works as is a subset of.

  :scopes #{\"foo\" \"bar\"}
  only people with scopes which are super sets of
  #{\"foo\" \"bar\"}
  will be allowed to use the route.

  scopes are considered as path with read/write access.
  so \"foo/bar/baz:read\" is a sub-scope of \"foo\"
  and of \"foo:read\".

  So the more precise rule of access is.
  All mandatory scopes must be sub-scopes of at least one user scopes.

  Also mandatory the scopes and required should be normalized
  "
  [scopes required]
  (every? (fn [req-scope]
            (some #(repr-is-subscope? req-scope %) scopes))
          required))

(defn access-granted
  "check that the first parameter contains all the required scopes
  given as second parameter."
  [scopes required]
  (accepted-by-scopes (repr-normalize-scopes (map to-scope-repr scopes))
                      (repr-normalize-scopes (map to-scope-repr required))))

(def scopes-superset?
  "Returns true if the first set of scopes is a superset of the second set of scopes.
  This is another name for `access-granted` function"
  access-granted)

(defn scopes-subset?
  "flipped version of scopes-superset?.
  Returns true if the first set is a subset of the second set of scopes."
  [required scopes]
  (scopes-superset? scopes required))

(defn repr-scopes-missing
  "return the list of scopes that is not in "
  [scopes-1 scopes-2]
  (let [nsc-1 (repr-normalize-scopes scopes-1)
        nsc-2  (repr-normalize-scopes scopes-2)]
    (filter (fn [scope]
              (not (some #(repr-is-subscope? scope %) nsc-2)))
            nsc-1)))

(defn scopes-missing
  "return element of the first set of scopes removing those in the second set
  of scopes

  This is slightly different from scope-difference because

  ex:



  "
  [scopes-1 scopes-2]
  (->> (repr-scopes-missing (map to-scope-repr scopes-1)
                            (map to-scope-repr scopes-2))
       (map scope-repr-to-str)
       set))

(defn root-scope
  "display the root of a scope

  >>> (root-scope \"foo/bar:read\")
  foo
  "
  [scope]
  (-> scope to-scope-repr :path first))

(defn is-root-scope?
  [scope]
  (= 1
     (count (-> scope to-scope-repr :path))))


(defn add-scope
  "Add the scope to a set of scopes"
  [scope scopes]
  (normalize-scopes (cons scope scopes)))

(defn scope-union
  "Unionize two set of scopes"
  [scopes-1 scopes-2]
  (normalize-scopes (set/union scopes-1 scopes-2)))

(defn raw-remove-root-scope
  "remove a root scope from a set of scopes.
  You should, most of the time, use remove-root-scope"
  [root-scope-to-remove scopes]
  (if (is-root-scope? root-scope-to-remove)
    (->> (for [scope scopes]
           (when-not (is-subscope? scope root-scope-to-remove)
             scope))
         (remove nil?)
         set)
    (throw (ex-info "We can't remove a sub scope, only root scope can be removed from a set of scopes, note access are supported"
                    {:scope root-scope-to-remove}))))

(def remove-root-scope
  (comp normalize-scopes raw-remove-root-scope))


(defn repr-is-strict-subpath?
  "return true if the first argument is strictly a sub path of the second argument"
  [r1 r2]
  (let [n1 (count (:path r1))
        n2 (count (:path r2))]
    (and (> n1 n2)
         (= (take n2 (:path r1))
            (:path r2)))))

(defn raw-repr-scope-disj
  [rscopes rs-to-remove]
  (->> (for [rs rscopes]
         (do
           (when (repr-is-strict-subpath? rs-to-remove rs)
             (throw (ex-info "We can't remove a sub subscope of some other scope (access part is still supported)"
                             {:scope (scope-repr-to-str rs-to-remove)
                              :conflicting-scope (scope-repr-to-str rs)})))
           (if (is-sub-list? (:path rs-to-remove) (:path rs))
             (when-let [access (seq (set/intersection
                                     (:access rs)
                                     (set/difference
                                      #{:read :write}
                                      (:access rs-to-remove))))]
               {:path (:path rs)
                :access (set access)})
             rs)))
       (remove nil?)
       set))

(defn repr-scope-disj
  "remove a scope for a set of scopes.
  Will throw an exception if the scope to remove is a subscope of some scope in
  the scopeset"
  [repr-scopes rs-to-rm]
  (let [rr (repr-normalize-scopes repr-scopes)]
    (raw-repr-scope-disj rr rs-to-rm)))

(defn scope-disj
  "remove a scope from a set of scope. Throw an error if trying to remove a
  subscope of an existing scope"
  [scopes scope-to-remove]
  (let [rss (->> scopes (map to-scope-repr) set)
        rs-to-rm (to-scope-repr scope-to-remove)]
    (->> (repr-scope-disj rss rs-to-rm)
         (map scope-repr-to-str)
         set)))

(defn scope-difference
  "a lot similar to scopes-missing but take care of throwing an exception if
  some sub-scope cannot be removed. This would prevent an error when trying to
  reduce a set of scopes.


  (scope-difference #{\"foo/bar\"} #{\"foo:write\"})
  => #{\"foo/bar:read\"}

  keep foo/bar but removed all :write from super-scope foo."
  [scopes scopes-to-remove]
  (let [nrss (->> scopes (map to-scope-repr) set repr-normalize-scopes)
        nrsr (->> scopes-to-remove (map to-scope-repr) set repr-normalize-scopes)]
    (->> nrsr
         (reduce (fn [acc-repr-scopes rscope]
                   (raw-repr-scope-disj acc-repr-scopes rscope))
                 nrss)
         repr-normalize-scopes
         (map scope-repr-to-str)
         set)))

;; INTERSECTION

(defn repr-scope-intersection
  "return the maximal intersection between two sopes repr

  `(to-scope-repr \"foo:write\")` and `(to-scope-repr \"foo/bar\")`
  => `(to-scope-repr \"foo/bar:write\")`
  "
  [r1 r2]
  (let [access (set/intersection (:access r1) (:access r2))]
    (when (seq access)
      (let [n1 (count (:path r1))
            n2 (count (:path r2))
            n (min n1 n2)
            sub-path-1 (take n (:path r1))
            sub-path-2 (take n (:path r2))]
        (when (= sub-path-1 sub-path-2)
          {:access access
           :path (if (> n1 n2)
                   (:path r1)
                   (:path r2))})))))

(defn scope-intersection
  "return the maximal intersection between two sopes

  `foo:write` and `foo/bar` => `foo/bar:write`
  "
  [scope-1 scope-2]
  (let [r1 (to-scope-repr scope-1)
        r2 (to-scope-repr scope-2)]
    (some->> (repr-scope-intersection r1 r2)
             scope-repr-to-str)))

(defn repr-scopes-intersection
  "return the intersection between two set of scope"
  [sr1 sr2]
  (->> (for [r1 sr1
             r2 sr2]
         (repr-scope-intersection r1 r2))
       (remove nil?)
       (repr-normalize-scopes)))

(defn scopes-intersection
  "return the intersection between two set of scope"
  [scopes-1 scopes-2]
  (let [sr1 (-> (map to-scope-repr scopes-1) repr-normalize-scopes)
        sr2 (-> (map to-scope-repr scopes-2) repr-normalize-scopes)]
    (->> (repr-scopes-intersection sr1 sr2)
         (map scope-repr-to-str)
         set)))

(defn repr-scopes-intersect?
  "returns true if r1 and r2 intersect

  For example: `(to-scope-repr foo:write)` and `(to-scope-repr foo/bar)`
  intersect while neither of those scope repr is a subscope of another."
  [r1 r2]
  (and (boolean (seq (set/intersection (:access r1) (:access r2))))
       (let [n (min (count (:path r1))
                    (count (:path r2)))
             sub-path-1 (take n (:path r1))
             sub-path-2 (take n (:path r2))]
         (= sub-path-1 sub-path-2))))

(defn scopes-intersect?
  "returns true if scope-1 and scope-2 intersect

  For example: `foo:write` and `foo/bar` intersect while
  neither of those scope is a subscope of another."
  [scope-1 scope-2]
  (let [r1 (to-scope-repr scope-1)
        r2 (to-scope-repr scope-2)]
    (repr-scopes-intersect? r1 r2)))

(defn repr-scopes-intersecting
  "Asymetrical operation; returns the list of first scopes repr that intersect
  with some scopes repr of the second set of scopes repr"
  [rs-1 rs-2]
  (filter (fn [r-scope]
            (some #(repr-scopes-intersect? % r-scope) rs-2))
          rs-1))

(defn scopes-intersecting
  "Asymmetrical operation; returns the list of first scopes that intersect with
  some scopes of the second set of scopes"
 [scopes-1 scopes-2]
  (let [rs-1 (map to-scope-repr scopes-1)
        rs-2 (map to-scope-repr scopes-2)]
    (->> (repr-scopes-intersecting rs-1 rs-2)
         (map scope-repr-to-str)
         set)))
