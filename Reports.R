tables <- list()

#####-- Loan and borrower level   -####
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
    ptf,super.originator,
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

###### -- Guarantors              --####
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


# - guarantors yes/no with type guarantors and solvency.guarantor
tables$guarantor.with.type <- 
  temp.vars$loans.guarantors %>%
  mutate(n.loans=1) %>%
  make_table(flag_guarantor, type,
             "Table_Guarantor_type",
             gbv.original, gbv.residual, principal, n.loans, n_guarantees)

tables$guarantor.with.status <- 
  temp.vars$loans.guarantors %>%
  mutate(n.loans=1) %>%
  make_table(flag_guarantor, status,
             "Table_Guarantor_Status",
             gbv.original, gbv.residual, principal, n.loans, n_guarantees)

tables$guarantor.with.lien <- 
  temp.vars$loans.guarantors %>%
  filter(type=="lien") %>%
  mutate(n.loans=1) %>%
  make_table(origin.lien, rank.lien,
             "Table_Guarantor_lien",
             gbv.original, gbv.residual, principal, n.loans) %>% mutate(subcluster = as.character(subcluster))




#### -- Agreement summary --####
#Collections has negative residuals -> in this cases, the amount paid is never bigger than the gbv.agreement
#In 95% of the cases, status=closed
#Only 24 cases have a difference between the amount paid in collections and in summary
# Nobody paid more than what they were supposed to according to collections (they did paid less, but I take it I have missing info)  
# temp.vars$negative.residuals <- df0$agreement.summary %>% filter(residual<0) %>% left_join(df0$collection %>% filter(type=="agreement"), by=c("id.bor", "id.group")) %>%
#   filter(date>date.start & date<date.end) %>%
#   group_by(id.bor) %>% summarise(residual = residual, paid=paid, amount.agreement =amount.agreement, paid.in.collections = sum(amount)) %>% distinct() %>% filter(amount.agreement!=paid.in.collections) %>%
#   mutate(amount.collection.smaller= ifelse(paid.in.collections<amount.agreement, "yes", "no")) 

temp.vars$agreement.summary <- df0$agreement.summary %>% 
  mutate(gbv.agreement = replace_na(gbv.agreement, 0),
         paid = ifelse(residual<0, amount.agreement, paid),
         residual=ifelse(residual<0,0,residual),
         
         discount = ifelse(gbv.agreement != 0 & gbv.agreement>amount.agreement, 1- amount.agreement/gbv.agreement,0) %>% round(2),
         range.discount = cut(
           discount,
           breaks = c(0.00, 0.25, 0.5, 0.6, 0.7, Inf),  # Adjusted breaks
           labels = c("0-25%", "25-50%", "50-60%", "60-70%", "70%+"),
           include.lowest = TRUE
         ),
         range.amount = cut(
           amount.agreement,
           breaks = c(0, 2500, 5000, 10000, 20000, Inf),  # Adjusted breaks
           labels = c("0-2.5k", "2.5k-5k", "5k-10k", "10k-20k", "20k+"),
           include.lowest = TRUE
         )
  )
#temp.vars$agreement.summary %>% View()


tables$agrement.status <- 
  temp.vars$agreement.summary %>%
  make_table_one_var(status,
             "Table_Agreement_Status",
             gbv.agreement, amount.agreement, paid)


tables$agrement.discount <- 
  temp.vars$agreement.summary %>%
  make_table_one_var(range.discount,
                     "Table_Agreement_discount",
                     gbv.agreement, amount.agreement, paid)

tables$agrement.status.discount <- 
  temp.vars$agreement.summary %>%
  make_table(range.discount, status,
                     "Table_Agreement_status_discount",
                     gbv.agreement, amount.agreement, paid)

tables$agrement.amountRange <- 
  temp.vars$agreement.summary %>%
  make_table_one_var(range.amount,
             "Table_Agreement_amountRange",
             gbv.agreement, amount.agreement, paid, n.instalment)
tables$agrement.amountRange %>% View()

#### -- PPT summary --####
#35 borrowers have a higher amount of paid in ppt.summary than in collections. 
#the sum of paid not considering them is 730k, considering them is 656k. Considering the amount PPT and amount residual it makes more sense to leave the Paid as it was, and not take the amount from collection
temp.vars$dif.amount <- df0$ppt.summary %>% left_join(df0$collection %>% filter(type=="ppt"), by = join_by(id.bor, id.group)) %>% 
  filter(date>date.start & date<date.end) %>% 
  group_by(id.bor) %>%
  summarise(
    paid.in.ppt = first(paid),
    paid.in.collections = sum(amount),
    date.start=first(date.start)
  ) %>% filter(paid.in.ppt != paid.in.collections) %>% mutate(diff= paid.in.collections-paid.in.ppt) 


temp.vars$ppt.summary <- df0$ppt.summary %>% 
  mutate(
    
     range.amount.ppt = cut(
       amount.ppt,
       breaks = c(0, 5000, 10000, 15000, 20000, Inf),  # Adjusted breaks
       labels = c("0-5k", "5k-10k", "10k-15k", "15k-20k", "20k+"),
       include.lowest = TRUE
     )
  )
temp.vars$ppt.summary %>% View()
tables$ppt.status <- 
  temp.vars$ppt.summary %>%
  make_table_one_var(status,
                     "Table_PPT_Status",
                     amount.ppt, paid, residual)

tables$ppt.range.amount <- 
  temp.vars$ppt.summary %>%
  make_table_one_var(range.amount.ppt,
                     "Table_PPT_Range_Amount",
                     amount.ppt, paid, residual)


tables$ppt.year <- 
  temp.vars$ppt.summary %>%
  make_table_one_var(year.ppt,
                     "Table_PPT_Year",
                     amount.ppt, paid, residual) %>% mutate(cluster= as.character(cluster))

#####
combined_tables <- bind_rows(tables) %>%
  select(title, cluster, subcluster, item, amount)

write.csv(combined_tables, "data.csv")
