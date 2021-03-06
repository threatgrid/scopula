scopula
=======

A Clojure library designed to manage a scope convention to handle fine
grained authorization access.

OAuth2 make all the authorization access pass through a single
dimension: the scopes.

`scopes` are case sensitive strings without any space that represent and
authorization access. From OAuth2 RFC
(<https://tools.ietf.org/html/rfc6749#section-3.3>):

> The value of the scope parameter is expressed as a list of space-
> delimited, case-sensitive strings. The strings are defined by the
> authorization server. If the value contains multiple space-delimited
> strings, their order does not matter, and each string adds an
> additional access range to the requested scope.
>
>     scope       = scope-token *( SP scope-token )
>     scope-token = 1*( %x21 / %x23-5B / %x5D-7E )

In order to manage a fine grained authorizations this lib use a
convention for scope formats. For example, we often need to distinguish
between a full scope that will provide full access to some resource, and
read-only access. Sometimes we also want to limit the access to some
sub-resource. Here are some example for our convention:

  ----------------------------- -----------------------------------------------
  `users`                       full access to users resource
  `users/profile`               access to users profile only
  `users/profile:read`          access to users profile read-only
  `users/profile/email:write`   access to users profile only email write-only
  ----------------------------- -----------------------------------------------

Mainly `:` is only authorized to split between access
`read=/=write=/=rw` (nothing implies rw)

Sub resources are separated by `/` we can

This library provide helper functions to check that users scope will
also grants `users/profile/email` and `users/profile:read`

We also provide helpers to normalize set of scopes:

``` {.clojure}
>>> (normalize-scopes #{\"users\" \"users/profile/email:read\" \"admin\"})
#{\"users\" \"admin\"}
```

as `users/profile/email:read` is redundant it was removed.

Note scopes are meant to be used in an OAuth2 access in mind and thus
are generally manipulated as a set of scopes.

Scopes that do not have any subpath are called *root scopes*.

This is important because it is easy to add, union scopes. But it is
generally impossible to remove just a sub-scope as it would mean we
should know all the sub-paths of some root-scope and add the difference.
Scope are additive by their nature.

This is why the API only provide a remove-root-scope.

Usage
-----

Here is the API

-   `(is-scope-format-valid? str)`, return true if the string is a valid
    scope for our convention,
-   `(is-subscope? scope-to-check super-scope)`, return true if the
    provided scope is a subscope,
-   `(access-granted scopes required)`, return `true` if the `required`
    scope are \"contained\" in the `scopes`,
-   `(root-scope scope)` returns the root-scope part of a scope
-   `(is-root-scope? scope)` returns true if the scope is a root-scope
    (access are authorized)
-   `(normalize-scopes scopes)` remove all duplicate scopes,
    `normalize-scopes` is idempotent
    `(= identity (comp normalize-scopes normalize-scopes))`
-   `add-scope` to add one scope to a set of scopes
-   `scope-union` to make the union of two set of scopes
-   `remove-root-scope` to remove a root-scope (access accepted) from a
    set of scopes.
-   `remove-root-scopes` to remove some root-scopes (access accepted)
    from a set of scopes.

See [./test/scopula/core~test~.clj](./test/scopula/core_test.clj)

License
-------

Copyright © 2019 Cisco

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
