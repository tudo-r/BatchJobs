# check for a valid registry id:
# must start with an alpha-numeric
# then alpha-numerics and underscores are allowed
# pattern = "^[0-9a-zA-Z]+[0-9a-zA-Z_-]*$"
# we must be quite restrictive here because we prefix
# the table name with the registry name
checkIdValid = function(id, allow.minus=TRUE) {
  if (allow.minus)
    pattern = "^[0-9a-zA-Z]+[0-9a-zA-Z_-]*$"
  else
    pattern = "^[0-9a-zA-Z]+[0-9a-zA-Z_]*$"
  if (!grepl(pattern, id))
    stopf("Id does not comply with pattern %s: %s", pattern, id)
}
