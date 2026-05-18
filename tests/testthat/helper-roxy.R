create_blocks <- \(text)
{
  tmp <- tempfile();
  on.exit(unlink(tmp));
  cat(text, file = tmp);
  env <- roxygen2::env_file(tmp);
  blocks <- roxygen2::parse_file(tmp, env);
  for(i in seq_along(blocks))
  {
    blocks[[i]]$call <- call("<-", blocks[[i]]$call[[2L]], blocks[[i]]$call);
    blocks[[i]] <- roxygen2:::block_set_env(blocks[[i]], env);
  }
  return(blocks)
}
