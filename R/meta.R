## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' Vector object, with some handy methods.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
vector <- \(mode, size = 0L, data)
{
  data <- base::vector(mode, size);

  rm(mode, size);
  makeActiveBinding("size", \( ) { return(length(data)); }, environment());

  get  <- \(i)    { return(data[i]); }
  set  <- \(i, x) { data[i] <<- x; }

  push <- \(x)    { data <<- c(data, x); }
  peek <- \( )    { return(data[size]); }
  pop  <- \( )    { x <- peek(); data <<- data[-size]; return(x); }

  empty <- \( )   { return(size == 0L); }

  return(structure(environment(), class = "vector"));
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method base::print vector
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
print.vector <- \(x, ...)
{
  print(x$data);
  return(invisible(x));
}
