## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @useDynLib oopr, .registration = TRUE
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
isname <- \(x, names = character(0L))
{
  .Call("isname", x, names, PACKAGE = "oopr");
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
iscall <- \(x, names = character(0L))
{
  .Call("iscall", x, names, PACKAGE = "oopr");
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @exportS3Method roxygen2::roxy_tag_parse
roxy_tag_parse.roxy_tag_hide <- \(x) { return(x); }
