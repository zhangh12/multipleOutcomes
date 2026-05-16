
# Validate that data_index is a single positive integer (or integer-valued
# double). Returns the value coerced to integer. Called from every *_()
# constructor; the bounds check (data_index <= length(data)) lives later in
# jointCovariance.default() because that's the first place we know how many
# data frames were supplied.
validate_data_index <- function(data_index) {
  if (!is.numeric(data_index) ||
      length(data_index) != 1L ||
      is.na(data_index) ||
      data_index < 1 ||
      data_index != as.integer(data_index)) {
    stop("`data_index` must be a positive integer scalar; got ",
         deparse(data_index), call. = FALSE)
  }
  as.integer(data_index)
}
