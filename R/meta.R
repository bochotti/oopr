## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @intern
#' Vector object, with some handy methods.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
vector <- \(mode, size = 0L)
{
  data <- base::vector(mode, size);

  rm(mode, size);
  makeActiveBinding("size", \( ) { return(length(data)); }, environment());

  get  <- \(i)    { return(data[i]); }
  set  <- \(i, x) { data[i] <<- x; }

  push <- \(x)    { data[size + 1L] <<- x; }
  peek <- \( )    { return(data[size]); }
  pop  <- \( )    { x <- peek(); data <<- data[-size]; return(x); }

  empty <- \( )   { return(size == 0L); }

  return(structure(environment(), class = "oopr_vector"));
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
  method   <- vector("logical", size);
  property <- vector("character", size);
  static   <- vector("logical", size);
  class    <- vector("list", size);
  inherit  <- vector("character", size);

  push     <- \(...)
  {
    dots <- list(...);
    this <- parent.env(environment());
    for(nm in names(this)[-c(1:2)])
    {
      if(!is.na(match(nm, names(dots))))
      {
        this[[nm]]$push(dots[[nm]]);
      }
      else
      {
        this[[nm]]$push(base::vector(typeof(this[[nm]]$data), 1L));
      }
    }
  }

  rm(size);
  makeActiveBinding("size", \( ) { return(names$size); }, environment());

  return(structure(environment(), class = "oopr_meta"));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::print oopr_meta
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
print.oopr_meta <- \(x, ...)
{
  print(list2DF(lapply(rev(eapply(x, identity)[-c(1:2)]), `[[`, "data")));
  return(invisible(x));
}
