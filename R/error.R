## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @keywords internal
#' @aliases NULL
#' Object to collect error information.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
error <- \(call)
{
  class   <- vector("character");
  srcref  <- vector("list");
  message <- vector("character");
  makeActiveBinding("size", \( ) { return(class$size); }, environment());

  push <- \(cls, src, msg, ...)
  {
    class$push(cls);
    srcref$push(list(src));
    message$push(sprintf(msg, ...));
  }

  throw <- \()
  {
    for(i in seq_len(size))
    {
      msg <- paste(strwrap(message$get(i), prefix = "  "), collapse = '\n');
      if(!is.null(src <- srcref$get(i)[[1L]]))
      {
        file <- attr(src, "srcfile")$filename;
        row  <- src[1L];
        col  <- src[5L];
        top  <- sprintf("%s:%i:%i", file, row, col);
        if(nzchar(file) && !sink.number())
        {
          file <- normalizePath(file, winslash = '/', mustWork = FALSE);
          top <- sprintf(
            "\033]8;line=%i:col=%i;file://%s\007%s\033]8;;\007"
           ,row, col, file, top
          );
        }
        msg <- paste0(top, '\n', msg);
      }
      cat(msg, "\n\n", sep = "");
    }
    stop(errorCondition("Compilation errors", class = class$data, call = call));
  }

  return(environment());
}
