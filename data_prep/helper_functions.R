
SMALL_FARMS <- c(
  "FARM SALES: (LESS THAN 1,000 $)",
  "FARM SALES: (1,000 TO 2,499 $)",
  "FARM SALES: (2,500 TO 4,999 $)",
  "FARM SALES: (5,000 TO 9,999 $)",
  "FARM SALES: (10,000 TO 24,999 $)",
  "FARM SALES: (25,000 TO 49,999 $)",
  "FARM SALES: (50,000 TO 99,999 $)",
  "FARM SALES: (100,000 TO 249,999 $)"
)

NOT_SMALL_FARMS <- c(
  "FARM SALES: (250,000 TO 499,999 $)",
  "FARM SALES: (500,000 TO 999,999 $)",
  "FARM SALES: (1,000,000 OR MORE $)"
)



categorize_by_size <- function(df) {
  return(
    df %>%
      mutate(
        farm_size = ifelse(domaincat_desc %in% SMALL_FARMS, "Small", "Not small")
      )
  )
}