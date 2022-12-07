(ns scopula.core
  "Handles scopes logic.

  Scopes are case-sensitive strings without any whitespace, that represent
  authorization access. From OAuth2 RFC (https://tools.ietf.org/html/rfc6749#section-3.3):

  > The value of the scope parameter is expressed as a list of space-
  > delimited, case-sensitive strings.  The strings are defined by the
  > authorization server.  If the value contains multiple space-delimited
  > strings, their order does not matter, and each string adds an
  > additional access range to the requested scope.
  >
  >   scope       = scope-token *( SP scope-token )
  >   scope-token = 1*( %x21 / %x23-5B / %x5D-7E )

  In order to manage fine-grained authorizations, this lib uses a convention
  for scope formats.
  For example, we often need to distinguish between a full scope that will provides
  full access to some resource, and read-only access.
  Sometimes we also want to limit the access to some sub-resource.
  Here are some examples of our convention:

  `users`                      full access to users resource
  `users/profile`              access to users profile only
  `users/profile:read`         access to users profile read-only
  `users/profile/email:write`  access to users profile only email write-only

  Mainly `:` is only authorized to split between access read/write/rw
  (nothing implies rw).

  Sub-resources can be separated by `/`.

  This library provides helper functions to check that
  a given scope will also grant e.g. `users/profile/email` and `users/profile:read`.

  We also provide helpers to normalize sets of scopes:

  >>> (normalize-scopes #{\"users\" \"users/profile/email:read\" \"admin\"})
  #{\"users\" \"admin\"}

  ...as `users/profile/email:read` is redundant, it is removed.

  Note that scopes are meant to be used in an OAuth2 access in mind, and thus
  are generally manipulated as a set of scopes.

  scopes that do not have any subpath are called _root scopes_.

  This is important because it is easy to add, union scopes.
  But it is generally impossible to remove just a sub-scope as it would
  mean we should know all the sub-paths of some root-scope and add the difference.

  Scope are additive by nature."
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(def allowed-chars-no-colon-no-slash "[!#-.0-9;-\\[\\]-~]")
(def allowed-chars-no-colon-no-slash-no-plus "[!#-*,-.0-9;-\\[\\]-~]")

(def allowed-root (str allowed-chars-no-colon-no-slash-no-plus
                       allowed-chars-no-colon-no-slash "*"))
(def allowed-word (str allowed-chars-no-colon-no-slash "+"))

(def scope-regex (re-pattern (str
                              "^" allowed-root ;; root-scope
                              "(/" allowed-word ")*" ;; path of sub-scopes
                              "(:(read|write|rw))?$" ;; read write or rw
                              )))

(defn is-scope-format-valid?
  [scope]
  (boolean (re-matches scope-regex scope)))

(defn to-scope-repr
  "Transforms a textual scope as an internal representation to help
  check rules, typically:

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
  "Displays the root of a scope.

  >>> (scope-root \"foo/bar:read\")
  foo
  "
  [scope]
  (-> scope to-scope-repr :path first))

(defn is-sub-list?
  "Does `super-lst` begin with `lst`?"
  [lst super-lst]
  (let [n (count lst)]
    (= (take n super-lst) lst)))

(defn repr-is-subscope?
  "Returns whether the `scope-to-check` is a subscope of the `super-scope`"
  [scope-to-check super-scope]
  (and (set/superset? (:access super-scope) (:access scope-to-check))
       (is-sub-list? (:path super-scope) (:path scope-to-check))))

(defn is-subscope?
  "Returns whether the `scope-to-check` is a subscope of the `super-scope`"
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
  "Return whether a scope is contained by all the scopes.

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
         (remove #(repr-is-subsummed % (disj ssr %)))
         set)))

(defn normalize-scopes
  "Given a set of scopes, remove reduntant ones, and merge them by access if possible."
  [scopes]
  (->> scopes
       (map to-scope-repr)
       repr-normalize-scopes
       (map scope-repr-to-str)
       set))

(defn accepted-by-scopes
  "`scopes` should be strings.
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

  Also mandatory the scopes and required should be normalized"
  [scopes required]
  (every? (fn [req-scope]
            (some #(repr-is-subscope? req-scope %) scopes))
          required))

(defn access-granted
  "Checks that the first parameter contains all the required scopes
  given as second parameter."
  [scopes required]
  (accepted-by-scopes (repr-normalize-scopes (map to-scope-repr scopes))
                      (repr-normalize-scopes (map to-scope-repr required))))

(def scopes-superset?
  "Returns true if the first set of scopes is a superset of the second set of scopes.
  This is another name for `access-granted` function"
  access-granted)

(defn scopes-subset?
  "Flipped version of scopes-superset?.
  Returns true if the first set is a subset of the second set of scopes."
  [required scopes]
  (scopes-superset? scopes required))

(defn repr-scopes-missing
  "Return the list of `scopes-1` that is not in `scopes-2`"
  [scopes-1 scopes-2]
  (let [nsc-1 (repr-normalize-scopes scopes-1)
        nsc-2  (repr-normalize-scopes scopes-2)]
    (remove (fn [scope]
              (some #(repr-is-subscope? scope %) nsc-2))
            nsc-1)))

(defn scopes-missing
  "Return the elements of the first set of scopes, removing those in the second set
  of scopes"
  [scopes-1 scopes-2]
  (->> (repr-scopes-missing (map to-scope-repr scopes-1)
                            (map to-scope-repr scopes-2))
       (map scope-repr-to-str)
       set))

(defn root-scope
  "Returns the root of a scope.

  >>> (root-scope \"foo/bar:read\")
  foo"
  [scope]
  (-> scope to-scope-repr :path first))

(defn is-root-scope?
  [scope]
  (= 1
     (count (-> scope to-scope-repr :path))))

(defn add-scope
  "Add the scope to a set of scopes."
  [scope scopes]
  (normalize-scopes (cons scope scopes)))

(def scope-cons add-scope)

(defn scope-union
  "Unionize two set of scopes."
  [scopes-1 scopes-2]
  (normalize-scopes (set/union scopes-1 scopes-2)))

;; ## Removing scopes

(defn repr-is-strict-subpath?
  "Return whether the first argument is strictly a sub-path of the second argument."
  [r1 r2]
  (let [n1 (count (:path r1))
        n2 (count (:path r2))]
    (and (> n1 n2)
         (= (take n2 (:path r1))
            (:path r2)))))

(defn repr-scope-remove
  "While inputs should be in repr form,
  removes the single scope `rs-to-remove` from the single scope `rs`.
  
  Returns `nil` if the two scopes do not intersect.

  If the operation is not possible (for example, remove `foo/bar` from `foo`)
  this function throw an exception.

  If the two scopes intersect for the path but their access is different that
  mean some access should be remove. As exemple, removing `foo:write` from
  `foo/bar/baz` should endup with `foo/bar/baz:read`."
  [rs rs-to-remove]
  (when (repr-is-strict-subpath? rs-to-remove rs)
    (throw (ex-info "We can't remove a sub subscope of some other scope (access part is still supported)"
                    {:ex-origin ::scopula
                     :ex-type ::impossible-sub-scope-removal
                     :ex-full-msg
                     (format (str "By nature we cannot remove a subscope from another scope. "
                                  "You tried to remove %s but this conflict with %s")
                             (scope-repr-to-str rs-to-remove)
                             (scope-repr-to-str rs))
                     :scope (scope-repr-to-str rs-to-remove)
                     :conflicting-scope (scope-repr-to-str rs)})))
  (if (is-sub-list? (:path rs-to-remove) (:path rs))
    (when-let [access (seq (set/intersection (:access rs)
                                             (set/difference #{:read :write}
                                                             (:access rs-to-remove))))]
      {:path (:path rs)
       :access (set access)})
    rs))

(defn raw-repr-scope-disj
  "Removes a scope from a set of scopes."
  [rscopes rs-to-remove]
  (set (keep #(repr-scope-remove % rs-to-remove) rscopes)))

(defn repr-scope-disj
  "Remove a scope for a set of scopes.
  Will throw an exception if the scope to remove is a subscope of some scope in
  the scopes set."
  [repr-scopes rs-to-rm]
  (let [rr (repr-normalize-scopes repr-scopes)]
    (raw-repr-scope-disj rr rs-to-rm)))

(defn scope-disj
  "Remove a scope from a set of scopes. Throw an error if trying to remove a
  subscope of an existing scope."
  [scopes scope-to-remove]
  (let [rss (->> scopes (map to-scope-repr) set)
        rs-to-rm (to-scope-repr scope-to-remove)]
    (->> (repr-scope-disj rss rs-to-rm)
         (map scope-repr-to-str)
         set)))

(defn scope-difference
  "Very similar to `scopes-missing`, but taking care of throwing an exception if
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
  "Returns the maximal intersection between two scopes reprs.

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
  "Returns the maximal intersection between two scopes.

  `foo:write` and `foo/bar` => `foo/bar:write` "
  [scope-1 scope-2]
  (let [r1 (to-scope-repr scope-1)
        r2 (to-scope-repr scope-2)]
    (some->> (repr-scope-intersection r1 r2)
             scope-repr-to-str)))

(defn repr-scopes-intersection
  "Return the intersection between two set of scopes."
  [sr1 sr2]
  (->> (for [r1 sr1
             r2 sr2]
         (repr-scope-intersection r1 r2))
       (remove nil?)
       (repr-normalize-scopes)))

(defn scopes-intersection
  "Return the intersection between two set of scopes."
  [scopes-1 scopes-2]
  (let [sr1 (-> (map to-scope-repr scopes-1) repr-normalize-scopes)
        sr2 (-> (map to-scope-repr scopes-2) repr-normalize-scopes)]
    (->> (repr-scopes-intersection sr1 sr2)
         (map scope-repr-to-str)
         set)))

(defn repr-scopes-intersect?
  "Returns whether r1 and r2 intersect.

  For example: `(to-scope-repr foo:write)` and `(to-scope-repr foo/bar)`
  intersect, while neither of those scopes reprs is a subscope of another."
  [r1 r2]
  (and (boolean (seq (set/intersection (:access r1) (:access r2))))
       (let [n (min (count (:path r1))
                    (count (:path r2)))
             sub-path-1 (take n (:path r1))
             sub-path-2 (take n (:path r2))]
         (= sub-path-1 sub-path-2))))

(defn scopes-intersect?
  "Returns whether scope-1 and scope-2 intersect.

  For example: `foo:write` and `foo/bar` intersect while
  neither of those scope is a subscope of another."
  [scope-1 scope-2]
  (let [r1 (to-scope-repr scope-1)
        r2 (to-scope-repr scope-2)]
    (repr-scopes-intersect? r1 r2)))

(defn repr-scopes-intersecting
  "Asymmetrical operation; returns the list of first scopes repr that intersect
  with some scopes repr of the second set of scopes repr."
  [rs-1 rs-2]
  (filter (fn [r-scope]
            (some #(repr-scopes-intersect? % r-scope) rs-2))
          rs-1))

(defn scopes-intersecting
  "Asymmetrical operation; returns the list of first scopes that intersect with
  some scopes of the second set of scopes."
 [scopes-1 scopes-2]
  (let [rs-1 (map to-scope-repr scopes-1)
        rs-2 (map to-scope-repr scopes-2)]
    (->> (repr-scopes-intersecting rs-1 rs-2)
         (map scope-repr-to-str)
         set)))

;; ## Scope aliases

(def scope-alias-regex
  (re-pattern (str "^[+]" allowed-word)))

(defn is-scope-alias?
  [scope]
  (boolean (re-matches scope-alias-regex scope)))


(defn is-scope-aliases-map?
  [aliases]
  (every? is-scope-alias? (keys aliases)))

(defn scopes-expand
  "Given a set of scopes containing scope aliases expand them.

  Scopes aliases will be replaced, so the output of scopes-expand should not contain
  any scope alias.

  If some scope alias is missing in the scope-aliases-map, scope expand will throw an exception."
  [scopes aliases]
  (->> (for [s scopes]
         (if (is-scope-alias? s)
           (or (get aliases s)
               (throw (ex-info (format "missing scope alias (%s)" s)
                               {:type ::scopes-expand-missing-alias
                                :scope-alias-missing s})))
           [s]))
       (apply concat)
       (set)))

(defn safe-scopes-expand
  "Same as scope expand but return nil instead of throwing and exception if a scope alias is missing"
  [scopes aliases]
  (try (scopes-expand scopes aliases)
       (catch clojure.lang.ExceptionInfo e
         (if (= ::scopes-expand-missing-alias (-> e ex-data :type))
           nil
           (throw e)))))

(defn- scopes-compress-first
  [scopes sorted-aliases]
  (let [[alias-name sd] (first (keep (fn [[alias-name ss]]
                                        (when (scopes-subset? ss scopes)
                                          (try [alias-name (scope-difference scopes ss)]
                                               (catch Exception _ nil))))
                                     sorted-aliases))]
    (if alias-name
      (conj sd alias-name)
      scopes)))

(defn scopes-length
  "Return the sum of the length of string in a set of scopes."
  [scopes]
  (reduce + (mapv count scopes)))

(defn scopes-compress
  "Given a set of scopes and a dictionary of scopes aliases
  use a fast heuristic to compress scopes with scope aliases.

  It is more important to have a fast function than an efficient one.
  The best possible compression is clearly an NP-complete problem.

  What we do, we first sort aliases by the size of string that would be generated to list all the scopes.
  So for example:

  ```
  {\"+foo\" {\"x\" \"y\"}
  \"+bar\" {\"very-long-name-for-a-scope\"}
  }
  ```

  the scope alias +bar will be preferred as even if the set contain fewer elements, the sum of the length
  of the scopes in the scopes set is longer.
  "
  [scopes aliases]
  (let [sorted-aliases (sort-by (comp #(- %) scopes-length second) aliases)
        compressed-scopes (scopes-compress-first scopes sorted-aliases)]
    (if (= scopes compressed-scopes)
      scopes
      (scopes-compress compressed-scopes sorted-aliases))))
