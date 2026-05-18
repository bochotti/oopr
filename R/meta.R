## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Vector object, with some handy methods.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
vector <- \(mode, size = 0L)
{
  data <- base::vector(mode, size);
  rm(list = c("mode", "size"), envir = environment());
  makeActiveBinding("size", \( ) { return(length(data)); }, environment());
  get  <- \(i)    { return(data[i]); }
  set  <- \(i, x) { data[i] <<- x; }
  push <- \(x)    { data <<- c(data, x); }
  rmve <- \(i)    { data <<- if(is.logical(i)) data[!i] else data[-i]; }
  subs <- \(x)    { return(match(data, x, 0L) > 0L); }
  lock <- \( )
  {
    this <- parent.env(environment());
    keep <- c("data", "get", "size", "subs");
    rm(list = setdiff(names(this), keep), envir = this);
    lockEnvironment(this, bindings = TRUE);
    return(this);
  }
  return(`class<-`(environment(), "oopr_vector"));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::print oopr_vector
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
print.oopr_vector <- \(x, ...)
{
  print(x$data);
  return(invisible(x));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Meta object, which holds the meta on `oopr` members.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
meta <- \(size = 0L)
{
  names    <- vector("character", size);
  access   <- vector("character", size);
  method   <- vector("logical",   size);
  S3       <- vector("logical",   size);
  property <- vector("character", size);
  static   <- vector("logical",   size);
  class    <- vector("logical",   size);
  inherit  <- vector("character", size);
  virtual  <- vector("logical",   size);
  final    <- vector("logical",   size);
  rm(size);
  makeActiveBinding("size", \( ) { return(names$size); }, environment());

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  subs <- \(which, inverse = FALSE, ...)
  {
    dots <- list(...);
    this <- parent.env(environment());
    out  <- !logical(size);
    for(nm in names(dots))
    {
      if(match(nm, names(this), 0L) && inherits(this[[nm]], "oopr_vector"))
      {
        out <- out & this[[nm]]$subs(dots[[nm]]);
      }
    }
    if(inverse)
    {
      out <- !out;
    }
    if(!missing(which))
    {
      out <- this[[which]]$get(out);
    }
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  push <- \(...)
  {
    dots <- list(...);
    this <- parent.env(environment());
    for(nm in names(this))
    {
      if(!inherits(this[[nm]], "oopr_vector")) next;
      if(match(nm, names(dots), 0L))
      {
        this[[nm]]$push(dots[[nm]]);
      }
      else
      {
        this[[nm]]$push(base::vector(typeof(this[[nm]]$data), 1L));
      }
    }
    return(this);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  rmve <- \(i)
  {
    this <- parent.env(environment());
    for(nm in names(this))
    {
      if(!inherits(this[[nm]], "oopr_vector")) next;
      this[[nm]]$rmve(i);
    }
    return(this);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  lock <- \( )
  {
    this <- parent.env(environment());
    rm(list = c("push", "rmve", "lock"), envir = this);
    eapply(this, \(x) if(inherits(x, "oopr_vector")) { x$lock(); });
    lockEnvironment(this, bindings = TRUE);
    return(this);
  }

  return(structure(environment(), class = c("oopr_meta")));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::as.data.frame oopr_meta
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
as.data.frame.oopr_meta <- \(x, ...)
{
  x <- rev(eapply(x, identity));
  x <- x[vapply(x, inherits, logical(1L), "oopr_vector")];
  return(list2DF(lapply(x, `[[`, "data")));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::print oopr_meta
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
print.oopr_meta <- \(x, ...)
{
  print(as.data.frame(x));
  return(invisible(x));
}
