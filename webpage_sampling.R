## CREATED BY: Dani Dagan ##
## Cite as: TBD ##

set.seed(1234)

total_pop <- 12612 # total number of trails
random_sample <- sort(sample.int(total_pop,(total_pop*.1), replace=FALSE))

# population is a df with a column for each forest (as defined and labeled by alltrails) and a column for how many total trail pages exist for that forest
population <- read.csv('forests_population.csv', header=TRUE)

sampling_vector <- c()

for (i in 1:nrow(population)) {
  forest_string <- population$forest[i]
  trails_value <- population$trails[i]
  temp <- paste0(forest_string, 1:trails_value)
    sampling_vector <- c(sampling_vector, temp)
}

sequence <- seq_len(length(sampling_vector))
sampling_df <- data.frame(sampling_vector, sequence)

sample <- sampling_df[random_sample, ]

# Outputs a vector with a name of the forest (as labeled in AllTrails) and a number