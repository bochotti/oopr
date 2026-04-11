## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @name oopr_containers
#' @title Containers for oopr Instances
#' @description
#' Create a vector or key-value pair of `oopr` instances.
#'
#' @param ooprC `ooprC` \cr
#'              An `oopr` constructor object.
#'
#' @examples
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' oopr("test",,
#' {
#' test <- \(x) { this$x <- x; }
#' public:x <- 0L;
#' })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
NULL
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr_containers
#' @export
#' @examples
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' # create a vector
#' vec <- OoprVec(test);
#'
#' # $emplace will pass create a new instance
#' vec$emplace(, 1L);
#'
#' # $insert takes an existing class instance
#' vec$insert(0L, test(2L));
#'
#' print(vec);
#'
#' # $swap will swap elements
#' vec$swap(2L, 1L);
#' print(vec);
#'
#' # $apply can be used to loop over instances
#' vec$apply(\(obj) { obj$x <- obj$x + 1L; })
#' vec$apply(\(obj) { obj$x; })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprVec",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprVec <- \(ooprC)
{
  .Call(Cpp_oopr_vec_init, ooprC, this, FALSE);
  this$ooprC_ <- ooprC;
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:class <- \( ) { if(!is.null(this$ooprC_)) return(this$ooprC_@name); }
  get:empty <- \( ) { return(this$size == 0L); }
  get:size  <- \( ) { return(length(this$data_)); }
  get:data  <- \( ) { return(this$data_); }
  set:data  <- \(x)
  {
    stopifnot(is.list(x) && length(x) == this$size, this$isIdentical(x));
    this$data_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insert <- \(pos = this$size, x)
  {
    stopifnot(
      this$isSingleRoundNumeric(pos) && (pos == 0L || this$isInBounds(pos))
     ,is.oopr(x, this$class)
    );
    this$data_ <- append(this$data_, x, pos);
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  emplace <- \(. = this$size, ...)
  {
    this$insert(., do.call(this$ooprC_, args));
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  resize <- \(n)
  {
    stopifnot(this$isSingleRoundNumeric(n) && n >= 0L);
    if(n <= this$size)
    {
      this$data_ <- this$data_[seq_len(n)];
    }
    else
    {
      this$data_[seq.int(this$size + 1L, n)] <- rep(list(NULL), n - this$size);
    }
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  erase <- \(pos = this$size)
  {
    stopifnot(this$isSingleRoundNumeric(pos) && this$isInBounds(pos));
    this$data_[[pos]] <- NULL;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  swap <- \(pos1, pos2)
  {
    stopifnot(
      this$isSingleRoundNumeric(pos1) && this$isInBounds(pos1)
     ,this$isSingleRoundNumeric(pos2) && this$isInBounds(pos2)
    );
    this$data_[c(pos1, pos2)] <- this$data_[c(pos2, pos1)];
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  apply <- \(fun, ...)
  {
    return(lapply(this$data_, fun, ...));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @exportS3Method "[" OoprVec
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  S3:`[` <- \(i, j, ..., drop)
  {
    stopifnot(this$isSingleRoundNumeric(i) && this$isInBounds(i));
    return(this$data_[[i]]);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @exportS3Method "[<-" OoprVec
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  S3:`[<-` <- \(i, j, ..., value)
  {
    stopifnot(this$isSingleRoundNumeric(i) && this$isInBounds(i));
    this$data[[i]] <- value;
    return(this);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ooprC_ <- NULL;
  data_  <- list();

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isSingleRoundNumeric <- \(pos)
  {
    return(is.numeric(pos) && length(pos) == 1L && pos %% 1L == 0L);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isInBounds <- \(pos)
  {
    return(0 < pos && pos <= this$size)
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isIdentical <- \(x)
  {
    if(!identical(names(x), names(this$data_))) return(FALSE);
    test <- .mapply(list(x, this$data_), NULL, FUN = \(x, y)
    {
      if(is.null(y) && !is.null(x))
        is.oopr(x, this$class)
      else
        identical(x, y)
    });
    return(all(as.logical(test)));
  }
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr_containers
#' @export
#' @examples
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' # create a key-value pair
#' map <- OoprMap(test);
#' map$emplace("a", "a")$emplace("b", "b");
#' print(map);
#'
#' # apply is a two argument function
#' map$apply(\(k, o) { o$x <- toupper(k); });
#' map$apply(\(k, o) { o$x == k; });
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprMap",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprMap <- \(ooprC)
{
  .Call(Cpp_oopr_vec_init, ooprC, this, TRUE);
  this$ooprC_ <- ooprC;
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:class <- \( ) { if(!is.null(this$ooprC_)) return(this$ooprC_@name); }
  get:empty <- \( ) { return(this$size == 0L);    }
  get:size  <- \( ) { return(length(this$data_)); }
  get:data  <- \( ) { return(this$data_);         }
  set:data  <- \(x)
  {
    stopifnot(is.list(x) && length(x) == this$size, this$isIdentical(x));
    this$data_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  exists <- \(key)
  {
    stopifnot(this$isSingleCharacter(key));
    return(match(key, names(this$data_), 0L) > 0L);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insert <- \(key, x)
  {
    stopifnot(this$isSingleCharacter(key), is.oopr(x, this$class));
    this$data_[[key]] <- x;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  emplace <- \(., ...)
  {
    this$insert(., do.call(this$ooprC_, args));
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  erase <- \(key)
  {
    stopifnot(this$isSingleCharacter(key) && this$exists(key));
    this$data_[[key]] <- NULL;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  resize <- \(keys = character(0L))
  {
    this$data_ <- this$data_[keys];
    names(this$data_) <- keys;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  apply <- \(fun, ...)
  {
    names      <- names(this$data_);
    out        <- .mapply(fun, list(names, this$data_), list(...));
    names(out) <- names;
    return(out);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @exportS3Method "[" OoprMap
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  S3:`[` <- \(i, j, ..., drop)
  {
    stopifnot(this$exists(i));
    return(this$data_[[i]]);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @exportS3Method "[<-" OoprMap
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  S3:`[<-` <- \(i, j, ..., value)
  {
    stopifnot(this$exists(i));
    this$data[[i]] <- value;
    return(this);
  }

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
private:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  ooprC_ <- NULL;
  data_  <- list();

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isSingleCharacter <- \(key)
  {
    return(is.character(key) && length(key) == 1L && !is.na(key))
  }
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  isIdentical <- OoprVec@encl$this$isIdentical;
})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
