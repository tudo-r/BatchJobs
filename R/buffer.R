# a simple preallocated stack.
buffer = function(type = "list", capacity = 0L, value = TRUE, init = NULL, ...) {
  if (is.null(init)) {
    st = vector(type, capacity)
    n = 0L
  } else {
    st = init
    n = length(init)
  }
  rm(type)
  force(capacity)
  force(value)
  rm(init)

  get = function() {
    head(st, n)
  }

  push = function(x) {
    if (n == capacity)
      clear()
    n <<- n + 1L
    st[[n]] <<- x
  }

  pop = function() {
    n <<- min(0L, n - 1L)
  }

  top = function() {
    if (n == 0L)
      return(vector(type, 0L))
    st[[n]]
  }

  clear = function() {
    if (is.function(value))
      ret = value(get(), ...)
    else
      ret = value
    n <<- 0L
    ret
  }

  pos = function() {
    n
  }

  empty = function() {
    n == 0L
  }

  full = function() {
    n == capacity
  }

  list(get = get, push = push, pop = pop, top = top, clear = clear, pos = pos, empty = empty, full = full)
}
