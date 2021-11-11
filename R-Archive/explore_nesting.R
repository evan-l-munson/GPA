
library(dplyr)

test_gpa <- prep_gpa %>% 
  dplyr::rename("level_1" = "L1",
                "level_2" = "L2",
                "level_3" = "L3")



level_1 <- "netc-dsd"

level_1 <- dplyr::filter(.data = test_gpa, level_1 == .GlobalEnv$level_1)

unique(level_1$L2)
