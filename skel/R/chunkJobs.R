#' Chunks ids of jobs together, so that each chunk will be executed as a single job.
#'
#' Useful for very short jobs or jobs of varying runtime.
#' Id vector is randomly permuted before it is being chunked.
#' You can pass the result of this function to submitJobs.
#
#' @param ids [\code{integer}]\cr
#'   Vector of job ids to be chunked.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Preferred number of jobs in each chunk.
#'   Can not be used in combination with \code{n.chunks}
#' @param n.chunks [\code{integer(1)}]\cr
#'   Preferred number of chunks. 
#'   Can not be used in combination with \code{chunks.size}
#' @param shuffle [\code{logical(1)}]\cr
#'   Shuffle the vector of ids? 
#'   Default is \code{TRUE}.
#' @return [list of \code{character}]. List of id vectors.
#' @export
chunkJobs = function(ids, chunk.size, n.chunks, shuffle=TRUE) {
  ids = convertIntegers(ids)
  checkArg(ids, "integer", min.len=1L, na.ok=FALSE)
  chunk(ids, chunk.size, n.chunks, shuffle)
}

# FIXME: maybe push this to BBmisc?
chunk = function(x, chunk.size, n.chunks, shuffle=FALSE) {
  if (!xor(missing(chunk.size), missing(n.chunks)))
    stop("You must provide either chunk.size (x)or n.chunks")
 
  getOther = function(n, k) n %/% k + (n %% k > 0L)
  
  n = length(x)
  if (missing(chunk.size)) {
    n.chunks = convertInteger(n.chunks)
    checkArg(n.chunks, "integer", len=1L, lower=1L, na.ok=FALSE)
    chunk.size = getOther(n, n.chunks)
  } else { # missing(n.chunks)
    chunk.size = convertInteger(chunk.size)
    checkArg(chunk.size, "integer", len=1L, lower=1L, na.ok=FALSE)
    n.chunks = getOther(n, chunk.size)
  }

  if(shuffle)
    x = sample(x)
  split(x, head(rep(seq_len(n.chunks), each=chunk.size), n))
}
