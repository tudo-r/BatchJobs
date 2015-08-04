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
  rm(init)
  force(capacity)
  force(value)
  ddd = list(...)

  list(
    get = function() {
      head(st, n)
    },
    push = function(x) {
      if (n == capacity)
        clear()
      n <<- n + 1L
      st[[n]] <<- x
    },
    clear = function() {
      if (is.function(value))
        ret = do.call(value, c(list(head(st, n)), ddd))
      else
        ret = value
      n <<- 0L
      ret
    },
    pos = function() {
      n
    },
    empty = function() {
      n == 0L
    }
  )
}
