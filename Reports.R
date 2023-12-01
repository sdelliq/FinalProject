tables <- list()
created.tables <- list()

#####-- Loan and borrower level             -####
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
    super.originator,ptf,
    "Table_originator_ptf",
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

tables$loan_status_updated_ptf <- 
  df0$loan %>% 
  make_table(ptf, status.updated,
             title="Table_ptf_updated_status")

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

#Type bor by portfolio
tables$loan_typebor_ptf <- 
  borrowers %>%
  make_table(ptf, type.bor,
             title="Table_ptf_bor",
             n.loans, gbv.original, gbv.residual, principal)

#corporate or individual with clusters and GBV sums
tables$borrower_type_withGBV <- 
  borrowers %>%
  make_table(type.bor, range.gbv.residual,
             "Table_TypeBor_GBV",
             n.loans, gbv.original, gbv.residual, principal)

#corporate or individual with clusters and GBV sums
tables$borrower_type_withGBV <- 
  borrowers %>%
  make_table(type.bor, range.gbv.residual,
             "Table_TypeBor_GBV",
             n.loans, gbv.original, gbv.residual, principal)


#In the table it's neccesary to filter by individual being I want to see the solvency(and it's for pf)
tables$borrower_solvency_pf <- 
  borrowers %>%
  filter(type.bor=="individual") %>%
  make_table(ptf, status.bor,
             "Table_Solvency_PF",
             gbv.original, gbv.residual, principal, n.loans)

tables$borrower_solvency_pg <- 
  borrowers %>%
  filter(type.bor=="corporate") %>%
  make_table(ptf, status.bor,
             "Table_Solvency_PG",
             gbv.original, gbv.residual, principal, n.loans)

# - area
tables$borrower_area <- 
  borrowers %>%
  make_table(ptf, area,
             "Table_PTF_Area",
             gbv.original, gbv.residual, principal, n.loans)

###### -- Guarantors                        --####
# guarantors yes/no  with gbv
temp.vars$loans.guarantors <- df0$loan %>% select(-type, -status) %>% 
  left_join(df0$guarantees, by=c("id.bor", "id.group"), relationship = "many-to-many") %>% 
  filter(status == "valid" | is.na(status)) %>%
  left_join(link0$guarantee.entity, by="id.guarantee",relationship = "many-to-many") %>% 
  left_join(df0$entity, by="id.entity") %>%
  filter(solvency.pf != "deceased" | solvency.pf != "insolvent" | status.pg != "canceled" | status.pg != "ceased") %>%
  select(id.loan, ptf, cluster.ptf, gbv.original, gbv.residual, principal, id.guarantee, type, amount.guarantee, status, origin.lien, rank.lien) %>% distinct() %>%
  group_by(id.loan) %>%
    summarise(
      across(c(ptf, cluster.ptf, gbv.original, gbv.residual, principal), first),
      n_guarantees = sum(!is.na(id.guarantee)),
      total_amount_guaranteed = sum(amount.guarantee),
      type = first(factor(type,levels=c(type= "lien", "surety", "confidi", "pledge", "other"))),
      status = first(factor(status,levels=c(type= "valid", "expired", "ineffective", "canceled"))),
      origin.lien = as.character(origin.lien),
      origin.lien = ifelse("judicial" %in% origin.lien & "voluntary" %in% origin.lien, NA, first(origin.lien)),
      rank.lien=min(rank.lien),
      .groups = "drop"
    ) %>% distinct() %>%
  mutate(range.gbv.residual = cut(gbv.residual, breaks = temp.vars$breaks, labels = temp.vars$labels, include.lowest = TRUE))
# temp.vars$loans.guarantors <- df0$loan %>% 
#   filter(!id.loan %in% temp.vars$loans.guarantors$id.loan) %>% 
#   #bind_rows(temp.vars$loans.guarantors) %>%
#   mutate(#flag_guarantor= ifelse(is.na(n_guarantees), "no", "yes"),
#          range.gbv.residual = cut(gbv.residual, breaks = temp.vars$breaks, labels = temp.vars$labels, include.lowest = TRUE))
#temp.vars$loans.guarantors %>% View

tables$guarantor.with.gbv <- 
  temp.vars$loans.guarantors %>%
    make_table(ptf, range.gbv.residual,
             "Table_Guarantor_GBV",
             gbv.original, gbv.residual, principal,n_guarantees, total_amount_guaranteed)


# - guarantors yes/no with type guarantors and solvency.guarantor
tables$guarantor.with.type <- 
  temp.vars$loans.guarantors %>%
  make_table(ptf, type,
             "Table_Guarantor_type",
             gbv.original, gbv.residual, principal, n_guarantees, total_amount_guaranteed)


# tables$guarantor.with.status <- 
#   temp.vars$loans.guarantors %>%
#   mutate(n.loans=1) %>%
#   make_table(flag_guarantor, status,
#              "Table_Guarantor_Status",
#              gbv.original, gbv.residual, principal, n.loans, n_guarantees)

tables$guarantor.with.lien <- 
  temp.vars$loans.guarantors %>%
  filter(type=="lien") %>%
  make_table(origin.lien, rank.lien,
             "Table_Guarantor_lien",
             gbv.original, gbv.residual, principal) %>% mutate(subcluster = as.character(subcluster))




#### -- Agreement summary pre-analysis      --####
source("tables/agreement_summary.R") #creation of temp.vars$agreement.summary with analysis detailed

#### -- Agreement summary tables creation   --####

#temp.vars$agreement.summary %>% View()


tables$agrement.status <- 
  temp.vars$agreement.summary %>%
  make_table(ptf, status,
             "Table_Agreement_Status",
             gbv.agreement, amount.agreement, paid)


tables$agrement.discount <- 
  temp.vars$agreement.summary %>%
  make_table(ptf, range.discount,
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


#### -- PPT summary pre-analysis            --####
source("tables/ppt_summary.R") #creation of temp.vars$ppt_summary with analysis detailed

#### -- PPT summary tables creation --####
tables$ppt.status <- 
  temp.vars$ppt.summary %>%
  make_table(ptf, status,
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

#Collections
#### -- Collections  ###########

temp.vars$collections <- df0$collection %>% group_by(id.bor, id.group, class, type) %>% 
  summarise(
    date.first.payment=min(date),
    date.last.payment= max(date),
    paid=sum(amount),
    .groups = "drop"
  ) %>% mutate(
    range.amount = cut(
      paid,
      breaks = c(0, 2500, 5000, 10000, 20000, Inf),  # Adjusted breaks
      labels = c("0-2.5k", "2.5k-5k", "5k-10k", "10k-20k", "20k+"),
      include.lowest = TRUE
    ))

temp.vars$collections.summary <- bind_rows(
  created.tables$agreement.summary %>% 
    select(id.bor, year=year.agreement,gbv.agreement, amount.expected=amount.agreement, paid, ptf) %>%
    mutate(class="extrajudicial", type="agreement"), 
  created.tables$ppt.summary %>% 
    select(id.bor, year=year.ppt, amount.expected=amount.ppt, paid, ptf) %>%
    mutate(class="judicial", type="pdr")
  )

temp.vars$collections.not.agr.pdr <- temp.vars$collections %>% 
  filter(!id.bor %in% temp.vars$collections.summary$id.bor) %>%
  left_join(df0$loan %>% select(id.bor, id.group, ptf), by=c("id.bor", "id.group")) %>% 
  mutate(year=as.numeric(format(date.first.payment, "%Y"))) %>%
  select(id.bor, class, type, paid, year) %>% 
  distinct()

created.tables$collections.summary <- bind_rows(temp.vars$collections.summary,temp.vars$collections.not.agr.pdr)


# find_duplicates(created.tables$collections.summary, id.bor) %>% group_by(id.bor) %>% filter("judicial" %in% class & "extrajudicial"%in% class) %>% View()
# there's 34 bors with judicial and extrajudicial info


tables$collections.class.type.ptf <- 
  created.tables$collections.summary %>%
  #mutate(class.type = paste(class, type, sep = "-")) %>%
  mutate(class.type = ifelse(!is.na(class), paste(class, type, sep = "-"), NA)) %>%
  make_table(ptf, class.type, 
             "Table_Collection_Class_Type_PTF", 
             gbv.agreement, amount.expected, paid)
tables$collections.class.type.ptf %>% View()

tables$collections.class.type <- 
  temp.vars$collections %>%
  make_table(class, type, 
             "Table_Collection_Class_Type", 
             paid)

tables$collections.amountRange <- 
  temp.vars$collections %>%
  make_table(class,range.amount,
                     "Table_Collection_amountRange",
                     paid)





#####
combined_tables <- bind_rows(tables) %>%
  select(title, cluster, subcluster, item, amount)

write.csv(combined_tables, "data.csv")


