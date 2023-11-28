tables <- list()


# Big totals of the Portfolio 
tables$totals <- data.frame(
  title = "Table_Totals_Portafoglio",
  item = c("gbv.original", "gbv.residual", "principal", "n.borrowers", "n.loans", "mean gbv residual by loan", "mean gbv residual by borrower"),
  amount = c(
    sum(df0$loan$gbv.original, na.rm = TRUE),
    sum(df0$loan$gbv.residual, na.rm = TRUE),
    sum(df0$loan$principal, na.rm = TRUE),
    n_distinct(df0$loan$id.bor),
    nrow(df0$loan),
    mean(df0$loan$gbv.residual, na.rm = TRUE),
    sum(df0$loan$gbv.residual, na.rm = TRUE)/n_distinct(df0$loan$id.bor)
  )
)


#Corporate - individual 
tables$borrower_type <- 
  borrowers %>% 
  make_table_one_var(x= type.bor, title="Table_TypeBor")


#portfoglio, subportafoglio, n(borrowers) and gbvs
tables$totals_by_ptf <-
  borrowers %>%
  make_table(
    ptf, cluster.ptf,
    "Table_PTFs_GBV",
    n.loans, gbv.original, gbv.residual, principal
  )


#GBV ranges with n borrowers and sum gbv.residual
tables$borrower_gbv <- 
  borrowers %>% 
  make_table_one_var(x= range.gbv.residual,
                     title="Table_GBV_Ranges",
                     gbv.residual, n.loans)


#Loans status
tables$loan_status_ptf <- 
  df0$loan %>% 
    mutate(n.loans=1) %>%
    make_table(ptf, status,
               title="Table_ptf_status",
               n.loans, gbv.original, gbv.residual, principal)

#Loans type
tables$loan_type_ptf <- 
  df0$loan %>% 
  mutate(n.loans=1) %>%
  make_table(ptf, type,
             title="Table_ptf_type",
             n.loans, gbv.original, gbv.residual, principal)

#Loans vintage
tables$loan_vintage_ptf <- 
  borrowers %>%
  make_table(ptf, range.vintage,
             title="Table_ptf_vintage",
             n.loans, gbv.original, gbv.residual, principal)


#corporate or individual with clusters and GBV sums
tables$borrower_type_withGBV <- 
  borrowers %>%
  make_table(type.bor, range.gbv.residual,
             "Table_TypeBor_GBV",
             n.loans, gbv.original, gbv.residual, principal)


#In the table it's neccesary to filter by individual being I want to see the solvency(and it's for pf)
tables$borrower_solvency <- 
  borrowers %>%
  make_table(type.bor, status.bor,
             "Table_Solvency_GBV",
             gbv.original, gbv.residual, principal, n.loans)

# - area
tables$borrower_area <- 
  borrowers %>%
  make_table_one_var(area,
             "Table_Area",
             gbv.original, gbv.residual, principal, n.loans)

# - city/province/area
# - range.age + income
# - corp / type.pg / status.pg
# - guarantors yes/no with type guarantors and solvency.guarantor


combined_tables <- bind_rows(tables) %>%
  select(title, cluster, subcluster, item, amount)

write.csv(combined_tables, "data.csv")
