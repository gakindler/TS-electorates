# How to merge specific column values into a list

library(data.table)

df <- data.frame(field = c("A", "A", "B", "B", "C", "C"),
                 name = c("joe", "roe", "gro", "gro", "toe", "boe"),
                 unique_name_occurance = c("2", "2", "1", "1", "2", "2")
)

df.desired <- data.table(field = c("A", "B", "C"),
                 name = list("joe":"roe"), ("gro"), ("toe", "boe")),
                 unique_name_occurance = c("2", "1", "2")
)
