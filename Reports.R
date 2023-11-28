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

tables$totals_by_originator <-
  borrowers %>%
  make_table(
    super.originator, ptf,
    "Table_originator_ptf",
    n.loans, gbv.original, gbv.residual, principal
  )
tables$totals_by_originator %>% View()
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


# guarantors yes/no  with gbv
temp.vars$loans.guarantors <- df0$loan %>% select(-type, -status) %>% 
  left_join(df0$guarantees, by=c("id.bor", "id.group")) %>% group_by(id.loan) %>%
    summarise(
      across(c(originator, ptf, cluster.ptf, gbv.original, gbv.residual, principal), first),
      n_guarantees = sum(!is.na(id.guarantee)),
      total_amount_guaranteed = sum(amount.guarantee),
      type = first(factor(type,levels=c(type= "lien", "surety", "confidi", "pledge", "other"))),
      status = first(factor(status,levels=c(type= "valid", "expired", "ineffective", "canceled"))),
      origin.lien = as.character(origin.lien),
      origin.lien = ifelse("judicial" %in% origin.lien & "voluntary" %in% origin.lien, NA, first(origin.lien)),
      rank.lien=min(rank.lien),
      .groups = "drop"
    ) %>% distinct() %>% 
  mutate(flag_guarantor= ifelse(n_guarantees==0, "no", "yes"),
         range.gbv.residual = cut(gbv.residual, breaks = temp.vars$breaks, labels = temp.vars$labels, include.lowest = TRUE))
#temp.vars$loans.guarantors %>% View

tables$guarantor.with.gbv <- 
  temp.vars$loans.guarantors %>%
    mutate(n.loans=1) %>%
    make_table(flag_guarantor, range.gbv.residual,
             "Table_Guarantor_GBV",
             gbv.original, gbv.residual, principal, n.loans)
tables$guarantor.with.gbv %>% View()

# - guarantors yes/no with type guarantors and solvency.guarantor


combined_tables <- bind_rows(tables) %>%
  select(title, cluster, subcluster, item, amount)

write.csv(combined_tables, "data.csv")
