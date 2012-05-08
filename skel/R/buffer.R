# a simple preallocated stack.
# if buffer reaches its capacity, fun is called on all elements in the buffer
# this also happens for manual clear() calls
buffer = function(type, capacity, fun=print, ...) {
  st = vector(type, capacity)
  n = 0L

  get = function() {
    head(st, n)
  }

  push = function(x) {
    n <<- n + 1L
    st[[n]] <<- x
    if (full())
      clear()
  }

  pop = function() {
    n <<- min(0L, n - 1L)
  }

  top = function() {
    if (empty())
      return(vector(type, 0L))
    st[[n]]
  }

  clear = function() {
    if (!empty())
      fun(get(), ...)
    n <<- 0L
  }

  size = function() {
    n
  }

  empty = function() {
    n == 0L
  }

  full = function() {
    n == capacity
  }

  list(get=get, push=push, pop=pop, top=top, clear=clear, size=size, empty=empty, full=full)
}
