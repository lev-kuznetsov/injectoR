# Dependency injection framework for R
# 
# Author: levk

# Default framework environment global
.environment <- new.env ();

# Singleton scope handler; bindings of singleton scope are provided for once and cached
singleton <- function (key, provider, environment) {
  if (is.null (environment[[ key ]]$singleton))
    environment[[ key ]]$singleton <- provider ();

  environment[[ key ]]$singleton;
}

# Prototype scope handler, bindings of prototype scope are provided for every time they are injected
prototype <- function (key, provider, environment) {
  provider ();
}

# Binds a key
define <- function (key, dependencies = NULL, callback, scope = prototype, environment = .environment) {
  if (is.null (dependencies)) dependencies <- names (formals (callback));
  environment[[ key ]]$dependencies <- dependencies;
  environment[[ key ]]$callback <- callback;
  environment[[ key ]]$scope <- scope;
}

# Shims a legacy package by binding each exported variable after installing it
# from source into a directory managed by the framework
shim <- function (source, environment = .environment) {
  location = file.path (.libPaths ()[1],
                        gsub ("/", .Platform$file.sep,
                              gsub ("//", "/", 
                                    paste ("injectoR/repository/",
                                           gsub ("\\.", "/", 
                                                 gsub("[:?=]", "_", source)), sep = ""))));
  tryCatch ({
    if (!file.exists (location)) {
      download.file (source, "package");
      dir.create (location, showWarnings = FALSE, recursive = TRUE);
      install.packages ("package", repos = NULL, type = "source", lib = location);
    }
    package <- installed.packages (lib.loc = location)[[ 1 ]];
    library (package, character.only = TRUE, lib.loc = location);
    
    for (export in ls (paste ("package:", package, sep = "")))
      define (export,
              callback = function () { eval (parse (text = export)) },
              scope = singleton,
              environment = environment);
  }, error = function (error) {
    unlink (location, recursive = TRUE);
    stop (error);
  });
}

# Inject a callback function with dependencies
inject <- function (dependencies = NULL, callback, environment = .environment) {
  errors <- list ();
  onError <- function (error) { errors[[ length(errors) + 1 ]] <- error; }
  parameters <- list ();

  if (is.null (dependencies)) dependencies <- names (formals (callback));
  for (key in dependencies)
    if (is.null (environment[[ key ]]))
      onError (c ("Unbound key ", key))
    else tryCatch ({
        parameters[[ key ]] <- environment[[ key ]]$scope (key, function () {
                                                             inject (environment[[ key ]]$dependencies,
                                                                     environment[[ key ]]$callback);
                                                           }, environment);
      }, error = onError, warning = onError);

  if (length (errors) == 0) do.call (callback, parameters)
  else stop (errors);

}

#define ('greeting', callback = function () { 'from the first R DI framework' });
#define ('world', callback = function () { 'world' });
#define ('hello', c ('world'), callback = function (world) { c ('hello', world) });
#inject (callback = function (hello, greeting) { 
#  print (c ("HELLO=", hello));
#  print (c ("GREETING=", greeting));
#});

#shim ('http://cran.at.r-project.org/src/contrib/Archive/agrmt/agrmt_1.31.tar.gz');
#inject ('modes', function (modes) { print (modes); });