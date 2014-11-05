# TODO: Add comment
# 
# Author: levk

# Exports request()
(function () {
  .root = file.path (.libPaths () [1], "requester");

  # Downloads and installs 
  request <<- function (..., callback = NULL, root = .root) {
    locations <- NULL;

    for (source in c (...)) {
      locations <- c (locations,
                      location <- file.path (root,
                                             gsub ("/", .Platform$file.sep,
                                                   gsub ("\\.", "/", 
                                                         gsub("[:?=*#]", "_", source)))));
      if (!file.exists (location)) tryCatch ({
        download.file (source, 'package', method = 'curl');
        dir.create (location, showWarnings = FALSE, recursive = TRUE);
        install.packages ('package', repos = NULL, type = 'source', lib = location);
      }, finally = { unlink ('package') });
    }

    if (is.null (callback)) locations else callback (locations);
  };
}) ();

source ("/Users/levk/src/injectoR/R/injector2.R");

request ('http://cran.at.r-project.org/src/contrib/Archive/agrmt/agrmt_1.31.tar.gz',
         callback = function (packages) {
  shim ('agrmt', root = packages);
  
  inject (function (modes) {
    modes (c (.1, .2, .3, .4, .5, .444, .555))
  });
});
