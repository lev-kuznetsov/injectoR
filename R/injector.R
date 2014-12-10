# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

# Dependency injection framework for R
# 
# Author: levk
(function () {
  # Binder

  # Global binder
  .binder <<- new.env (emptyenv ());

  # Creates binders. Optionally accepts a callback function which will be called with the
  # newly created binder and returns the result of the callback, if no callback is provided
  # then returns the binder 
  binder <<- function (parent = .binder, callback = function (binder) binder)
    callback (new.env (parent));

  # Scopes

  # The default scope, bindings of this scope are provisioned each time they are injected
  default <<- function (key, provider) provider;

  # Singleton scope, singleton bindings are provisioned once and cached 
  singleton <<- function (key, provider) {
    value <- NULL;
    function () if (is.null (value)) value <<- provider () else value;
  };

  # Eager singleton scope for immediate evaluation at definition time
  eager <<- function (key, provider) {
    value <- provider ();
    function () value;
  };

  # Defines a binding within the binder specified formed of key, factory and scope
  define <<- function (key, factory, scope = default, binder = .binder)
    binder[[ key ]] <- scope (key, function () inject (factory, binder));

  # Defines an accumulative binding injectable as a list, returns the attachment function
  # which accepts an injectable factory function to append to the binding
  collection <<- function (key, scope = default, binder = .binder) {
    factories <- NULL;
    define (key, function () {
      collection <- list ();
      for (factory in factories) collection [[ length (collection) + 1 ]] <- inject (factory, binder);
      return (collection);
    }, scope, binder);
    function (factory) factories <<- c (factories, factory);
  };

  # Shims a package defining each exported variable
  shim <<- function (package, prefix = '', suffix = '', root = NULL, binder = .binder, required = TRUE)
    tryCatch (for (export in getNamespaceExports (loadNamespace (package, lib.loc = root)))
               (function (export)
                 define (paste (prefix, export, suffix, sep = ''),
                         function () getExportedValue (space, export),
                         singleton, binder)) (export),
              error = function (error) if (required) stop (error) else return ());

  # Launches the injected callback
  inject <<- function (callback, binder = .binder) {
    arguments <- list ();
    errors <- list ();

    for (key in names (formals (callback)))
      if (!is.null (binder[[ key ]]))
        tryCatch (arguments[[ key ]] <- binder[[ key ]] (), error = function (chain) errors <<- c (errors, chain));

    if (length (errors) == 0) do.call (callback, arguments) else stop (errors);
  };
}) ();
