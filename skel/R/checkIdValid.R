# check for a valid registry id:
# must start with an alpha-numeric
# then alpha-numerics, and underscores are allowed
# FIXME This is stupid. I want a minus:
# pattern = "^[0-9a-zA-Z]+[0-9a-zA-Z._-]*$"
checkIdValid = function(id) {
  pattern = "^[0-9a-zA-Z]+[0-9a-zA-Z._]*$"
  if (!grepl(pattern, id))
    stopf("Id does not comply with pattern %s: %s", pattern, id)
}
