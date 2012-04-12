#
# Copyright (c) 2008-2010, Revolution Analytics
#
# This is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA

registerDoSNOW <- function(cl) {
  setDoPar(doSNOW, cl, info)
}

info <- function(data, item) {
  switch(item,
         workers=length(data),  # XXX is this right?
         name='doSNOW',
         version=packageDescription('doSNOW', fields='Version'),
         NULL)
}

makeDotsEnv <- function(...) {
  list(...)
  function() NULL
}

.doSnowGlobals <- new.env(parent=emptyenv())

workerInit <- function(expr, exportenv, packages) {
  assign('expr', expr, .doSnowGlobals)
  assign('exportenv', exportenv, .doSnowGlobals)
  parent.env(.doSnowGlobals$exportenv) <- globalenv()

  tryCatch({
    for (p in packages)
      library(p, character.only=TRUE)

    NULL  # indicates success
  },
  error=function(e) {
    # a character string indicates an error
    conditionMessage(e)
  })
}

evalWrapper <- function(args) {
  exportEnv <- .doSnowGlobals$exportenv
  attach(exportEnv)
  on.exit(detach(exportEnv))
  lapply(names(args), function(n) assign(n, args[[n]], pos=.doSnowGlobals$exportenv))
  tryCatch(eval(.doSnowGlobals$expr, envir=.doSnowGlobals$exportenv), error=function(e) e)
}

comp <- if (getRversion() < "2.13.0") {
  function(expr, ...) expr
} else {
  compiler::compile
}

doSNOW <- function(obj, expr, envir, data) {
  cl <- data

  if (!inherits(obj, 'foreach'))
    stop('obj must be a foreach object')

  it <- iter(obj)
  argsList <- as.list(it)
  accumulator <- makeAccum(it)

  # setup the parent environment by first attempting to create an environment
  # that has '...' defined in it with the appropriate values
  exportenv <- tryCatch({
    qargs <- quote(list(...))
    args <- eval(qargs, envir)
    environment(do.call(makeDotsEnv, args))
  },
  error=function(e) {
    new.env(parent=emptyenv())
  })
  noexport <- union(obj$noexport, obj$argnames)
  getexports(expr, exportenv, envir, bad=noexport)
  vars <- ls(exportenv)
  if (obj$verbose) {
    if (length(vars) > 0) {
      cat('automatically exporting the following variables',
          'from the local environment:\n')
      cat(' ', paste(vars, collapse=', '), '\n')
    } else {
      cat('no variables are automatically exported\n')
    }
  }

  # compute list of variables to export
  export <- unique(obj$export)
  ignore <- intersect(export, vars)
  if (length(ignore) > 0) {
    warning(sprintf('already exporting variable(s): %s',
            paste(ignore, collapse=', ')))
    export <- setdiff(export, ignore)
  }

  # add explicitly exported variables to exportenv
  if (length(export) > 0) {
    if (obj$verbose)
      cat(sprintf('explicitly exporting variables(s): %s\n',
                  paste(export, collapse=', ')))

    for (sym in export) {
      if (!exists(sym, envir, inherits=TRUE))
        stop(sprintf('unable to find variable "%s"', sym))
      assign(sym, get(sym, envir, inherits=TRUE),
             pos=exportenv, inherits=FALSE)
    }
  }

  # compile the expression if we're using R 2.13.0 or greater
  xpr <- comp(expr, env=envir, options=list(suppressUndefined=TRUE))

  # send exports to workers
  r <- clusterCall(cl, workerInit, xpr, exportenv, obj$packages)
  for (emsg in r) {
    if (!is.null(emsg))
      stop('worker initialization failed: ', emsg)
  }

  # execute the tasks
  results <- clusterApplyLB(cl, argsList, evalWrapper)

  # call the accumulator with all of the results
  tryCatch(accumulator(results, seq(along=results)), error=function(e) {
    cat('error calling combine function:\n')
    print(e)
  })

  # check for errors
  errorValue <- getErrorValue(it)
  errorIndex <- getErrorIndex(it)

  # throw an error or return the combined results
  if (identical(obj$errorHandling, 'stop') && !is.null(errorValue)) {
    msg <- sprintf('task %d failed - "%s"', errorIndex,
                   conditionMessage(errorValue))
    stop(simpleError(msg, call=expr))
  } else {
    getResult(it)
  }
}
