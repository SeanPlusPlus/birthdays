################################################################################
run_sample <- function(sample_set_size, days, data) {
################################################################################

  for(sample_set_size in days) {

    # generate vector of random days in year
    random_days <- sample(days, sample_set_size, replace = T)

    # if any are duplicated return the size of the sample set
    if ((anyDuplicated(random_days) > 0)) {
      return(sample_set_size)
    }
  }
}

################################################################################
main <- function() {
################################################################################

  # to be returned
  min_ppl_to_get_a_match <- c()

  # get days in year
  days <- seq(1, 365, by = 1)

  # number of tests to be run
  tests <- seq(1, 10000, by = 1)

  # run tests
  for (t in tests) {

    # get first match and update our variable holding state
    match <- run_sample(sample_set_size, days, data)
    min_ppl_to_get_a_match <- c(min_ppl_to_get_a_match, match)
  }

  # get relative frequency of value in vector in r
  # http://stackoverflow.com/questions/17386931/get-relative-frequency-of-value-in-vector-in-r
  mydf <- structure(list(ID = 1:4, ResourceID = min_ppl_to_get_a_match), 
                  .Names = c("ID", "ResourceID"), class = "data.frame", 
                  row.names = c(NA, -4L))

  # display our data
  print(prop.table(rev(sort(table(mydf$ResourceID)))))
}
