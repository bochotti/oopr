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
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
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
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' # create a vector
#' vec <- OoprVec(test);
#'
#' # $emplace create a new instance
#' vec$emplace(, 1L);
#'
#' # $insert passes existing instance
#' vec$insert(0L, test(2L));
#'
#' print(vec);
#'
#' # $swap will swap elements
#' vec$swap(2L, 1L);
#' print(vec);
#'
#' # $apply to loop over instances
#' vec$apply(\(x) { x$x <- x$x + 1L; })
#' vec$apply(\(x) { x$x; })
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
oopr("OoprVec",,
{
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @param ooprC `ooprC` \cr
#'        An `oopr` class.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
OoprVec <- \(ooprC)
{
  .Call(Cpp_oopr_cont_init, ooprC, this, FALSE);
  this$ooprC_ <- ooprC;
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field class `character(1L)` \cr
  #'              The name of the underlying `oopr` class.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:class <- \( )
  {
    return(this$ooprC_@name);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field empty `logical(1L)` \cr
  #'              Whether there are no instanced classes in the container.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:empty <- \( )
  {
    return(this$size == 0L);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field size `integer(1L)` \cr
  #'             The amount of instanced classes in the container.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:size  <- \( )
  {
    return(length(this$data_));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field data `list()` \cr
  #'             The container.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:data  <- \( )
  {
    return(this$data_);
  }
  set:data  <- \(x)
  {
    stopifnot(is.list(x) && length(x) == this$size, this$isIdentical(x));
    this$data_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Insert an already instanced class.
  #'
  #' @param pos `integer(1L)` \cr
  #'            The index to insert the class. The default is at the back.
  #'
  #' @param x   `oopr` \cr
  #'            An `oopr` instance of class `$class`.
  #'
  #' @returns
  #' `this` invisibly.
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
  #' @description
  #' Construct a new class into the container.
  #'
  #' @param .   `integer(1L)` \cr
  #'            As per `pos` argument for the `$insert` method.
  #'
  #' @param ... `varies` \cr
  #'            Arguments to pass to `$class`'s constructor.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  emplace <- \(. = this$size, ...)
  {
    this$insert(., do.call(this$ooprC_, args));
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Pre-allocate or destroy the container.
  #'
  #' @param n `integer(1L)` \cr
  #'          The desired length to make the container.
  #'
  #' @returns
  #' `this` invisibly.
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
  #' @description
  #' Remove a class from the container.
  #'
  #' @param pos `integer(1L)` \cr
  #'            The index in the container to remove.
  #'
  #' @returns
  #' `this` invisibly.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  erase <- \(pos = this$size)
  {
    stopifnot(this$isSingleRoundNumeric(pos) && this$isInBounds(pos));
    this$data_[[pos]] <- NULL;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Swap two elements of the container.
  #'
  #' @param pos1 `integer(1L)` \cr
  #'             Element to swap.
  #'
  #' @param pos2 `integer(1L)` \cr
  #'             Element to swap.
  #'
  #' @returns
  #' `this` invisibly.
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
  #' @description
  #' Apply a function over every element in the container.
  #'
  #' @param fun `function` \cr
  #'            Function to use, the first argument not in `...` will be
  #'            the containers class instances.
  #'
  #' @param ... `varies` \cr
  #'            Further arguments to `fun`.
  #'
  #' @returns
  #' `list()` of the outputs from `fun`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  apply <- \(fun, ...)
  {
    return(lapply(this$data_, fun, ...));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Access an element of the container.
  #'
  #' @param i     `integer(1L)` \cr
  #'              The element to access.
  #'
  #' @param j    *Not Used*
  #' @param ...  *Not Used*
  #' @param drop *Not Used*
  #'
  #' @returns
  #' An `oopr` object of class `$class`.
  #'
  #' @exportS3Method "[" OoprVec
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  S3:`[` <- \(i, j, ..., drop)
  {
    stopifnot(this$isSingleRoundNumeric(i) && this$isInBounds(i));
    return(this$data_[[i]]);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Assign to an element of the container.
  #'
  #' @param i     `integer(1L)` \cr
  #'              The element to assign to.
  #'
  #' @param j     *Not Used*
  #' @param ...   *Not Used*
  #' @param value `oopr` \cr
  #'              An `oopr` object of class `$class`.
  #'
  #' @details
  #' Can be used to set members of classes within the container, e.g.
  #' `x[i]$mem <- ...`.
  #'
  #' @returns
  #' `this` invisibly.
  #'
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprVec
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
#' @rdname oopr_containers
#' @export
#' @examples
#' ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
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
  .Call(Cpp_oopr_cont_init, ooprC, this, TRUE);
  this$ooprC_ <- ooprC;
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
public:
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$class
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:class <- \( )
  {
    return(this$ooprC_@name);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$empty
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:empty <- \( )
  {
    return(this$size == 0L);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$size
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:size  <- \( )
  {
    return(length(this$data_));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @field keys `character()` \cr
  #'             The keys within the container
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:keys  <- \( )
  {
    return(names(this$data_) %||% character(0L));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$data
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  get:data  <- \( )
  {
    return(this$data_);
  }
  set:data  <- \(x)
  {
    stopifnot(is.list(x) && length(x) == this$size, this$isIdentical(x));
    this$data_ <- x;
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @description
  #' Check whether a key exists.
  #'
  #' @param key `character(1L)` \cr
  #'            The key to check for
  #'
  #' @returns
  #' `logical(1L)`.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  exists <- \(key)
  {
    stopifnot(this$isSingleCharacter(key));
    return(match(key, names(this$data_), 0L) > 0L);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$insert
  #' @param key `character(1L)` \cr
  #'            The index to insert the class. The default is at the back.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  insert <- \(key, x)
  {
    stopifnot(this$isSingleCharacter(key), is.oopr(x, this$class));
    this$data_[[key]] <- x;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$emplace
  #' @param .   `character(1L)` \cr
  #'            As per `key` argument for the `$insert` method.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  emplace <- \(., ...)
  {
    this$insert(., do.call(this$ooprC_, args));
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$erase
  #' @param key `character(1L)` \cr
  #'            The index in the container to remove.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  erase <- \(key)
  {
    stopifnot(this$isSingleCharacter(key) && this$exists(key));
    this$data_[[key]] <- NULL;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$resize
  #' @param keys `character()` \cr
  #'             Keys to create / remain.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  resize <- \(keys = character(0L))
  {
    this$data_ <- this$data_[keys];
    names(this$data_) <- keys;
    return(invisible(this));
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @inherit OoprVec$apply
  #' @param fun `function` \cr
  #'            Function to apply with, the first two arguments being the
  #'            `key` and `value`, respectively.
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
  #' @inherit OoprVec$[
  #' @param i `character(1L)` \cr
  #'          The element to access.
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  S3:`[` <- \(i, j, ..., drop)
  {
    stopifnot(this$exists(i));
    return(this$data_[[i]]);
  }

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
  #' @exportS3Method "[<-" OoprMap
  #' @inherit OoprVec$[<-
  #' @param i `character(1L)` \cr
  #'          The element to assign to.
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

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
}) ## OoprMap
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
