## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_roclet
#' @title Roxygen for oopr
#' @export
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr_roclet <- \( ) roxygen2::roclet("oopr")

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::roclet_preprocess roclet_oopr
#' @intern
#' Finds the roxy blocks containing `ooprC` objects.
#' Inserts `<-` into the call so roxygen2 will pickup a symbol.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
roclet_preprocess.roclet_oopr <- \(x, blocks, base_path)
{
  env <- get("env", envir = sys.frame(-3L));
  for(i in seq_along(blocks))
  {
    block <- blocks[[i]];
    if(!is.null(block$object))   next;
    if(!is.ooprcall(block$call)) next;
    name         <- match.call(oopr, block$call)$name;
    block$call   <- call("<-", as.name(name), block$call);
    class(block) <- c("roxy_block_oopr", "roxy_block");

    if(roxygen2::block_has_tags(block, "exportS3Method"))
    {
      rdname <- roxygen2::block_get_tag(block, c("name", "rdname"));
      for(tag in roxygen2::block_get_tags(block, "exportS3Method"))
      {
        tags <- list(rdname, tag);
        call <- sub("^\"(.*?)\"", "\\1", tag$raw);
        call <- as.name(sub(" ", ".", call));
        call <- call("<-", call, get(call, envir = env));
        blocks[[length(blocks) + 1L]] <- roxygen2::roxy_block(
          tags, block$file, block$line, call, NULL
        );
        rm <- vapply(block$tags, identical, logical(1L), tag);
        block$tags <- block$tags[!rm];
      }
    }
    blocks[[i]]  <- block;
  }
  assign("blocks", blocks, envir = sys.frame(-3), inherits = FALSE);
  class(x) <- "roclet_rd";
  return(x);
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::block_to_rd roxy_block_oopr
#' @intern
#' Allows specific handling of creating Rd files for `oopr` objects.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
block_to_rd.roxy_block_oopr <- \(block, base_path, env)
{
  NextMethod();
}
