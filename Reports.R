tables <- list()

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
tables$borrower_type <- created.tables$borrowers %>% 
  make_table_one_var(x= type.bor, title="Table_TypeBor")


#portfoglio, subportafoglio, n(borrowers) and gbvs
tables$totals_by_ptf <-  created.tables$borrowers %>%
  make_table(ptf, cluster.ptf, "Table_PTFs_GBV", n.loans, gbv.original, gbv.residual, principal)

#Originator and portfolio
tables$totals_by_originator <- created.tables$borrowers %>%
  make_table(super.originator,ptf, "Table_originator_ptf",n.loans, gbv.original, gbv.residual, principal)

#GBV ranges with n borrowers and sum gbv.residual
tables$borrower_gbv <-   created.tables$borrowers %>% 
  make_table_one_var(x= range.gbv.residual, title="Table_GBV_Ranges", gbv.residual, n.loans)

#Orchestra GBV ranges
tables$orchestra.gbv <-   created.tables$borrowers %>%  filter(ptf=="orchestra") %>%
  make_table(cluster.ptf, range.gbv.residual, title="Table_GBV_Ranges_Orchestra", gbv.residual, n.loans)

#Orchestra bad, utp
tables$orchestra.status <- df0$loan %>% filter(ptf=="orchestra") %>%
  make_table(cluster.ptf, status, title="Table_Status_Orchestra", gbv.residual)

#Orchestra bad, utp updated
tables$orchestra.status.updated <- df0$loan %>% filter(ptf=="orchestra") %>%
  make_table(cluster.ptf, status.updated, title="Table_Status_Updated_Orchestra", gbv.residual)
 
#Vienna solvency
tables$vienna.status.bor <- created.tables$borrowers %>% filter(ptf=="vienna") %>%
  make_table(cluster.ptf, status.bor, title="Table_Status_Vienna", gbv.residual, n.loans)

#Vienna GBV ranges
tables$vienna.gbv <- created.tables$borrowers %>% filter(ptf=="vienna") %>%
  make_table(cluster.ptf, range.gbv.residual, title="Table_GBV_Ranges_Vienna", gbv.residual, n.loans)

#Vienna GBV ranges
tables$vienna.gbv.small <- created.tables$borrowers %>% filter(ptf=="vienna") %>%
  mutate(
    small.range.gbv.residual = cut(
      gbv.residual,
      breaks = c(0, 2500, 5000, 10000, 20000, Inf),  # Adjusted breaks
      labels = c("0-2.5k", "2.5k-5k", "5k-10k", "10k-15k", "15k+"),
      include.lowest = TRUE
    )) %>%
  make_table(cluster.ptf, small.range.gbv.residual, title="Table_Small_GBV_Ranges_Vienna", gbv.residual, n.loans)


#Loans status
tables$loan_status_ptf <- df0$loan %>% 
    make_table(ptf, status, title="Table_ptf_status", gbv.original, gbv.residual, principal)

#Loans status updated
tables$loan_status_updated_ptf <-  df0$loan %>% 
  make_table(ptf, status.updated, title="Table_ptf_updated_status", gbv.original)

#Loans type
tables$loan_type_ptf <- df0$loan %>% 
  make_table(ptf, type, title="Table_ptf_type", gbv.original, gbv.residual, principal)

#Loans vintage
tables$loan_vintage_ptf <-  created.tables$borrowers %>%
  make_table(ptf, range.vintage, title="Table_ptf_vintage", n.loans, gbv.original, gbv.residual, principal)

#Type bor by portfolio
tables$loan_typebor_ptf <- created.tables$borrowers %>%
  make_table(ptf, type.bor, title="Table_ptf_bor", n.loans, gbv.original, gbv.residual, principal)

#corporate or individual with clusters and GBV sums
tables$borrower_type_withGBV <- created.tables$borrowers %>%
  make_table(type.bor, range.gbv.residual, "Table_TypeBor_GBV", n.loans, gbv.original, gbv.residual, principal)

#corporate or individual with clusters and GBV sums
tables$borrower_type_withGBV <- created.tables$borrowers %>%
  make_table(type.bor, range.gbv.residual, "Table_TypeBor_GBV", n.loans, gbv.original, gbv.residual, principal)


#In the table it's neccesary to filter by individual being I want to see the solvency(and it's for pf)
tables$borrower_solvency_pf <-  created.tables$borrowers %>% filter(type.bor=="individual") %>%
  make_table(ptf, status.bor, "Table_Solvency_PF", gbv.original, gbv.residual, principal, n.loans)

tables$borrower_solvency_pg <- created.tables$borrowers %>% filter(type.bor=="corporate") %>%
  make_table(ptf, status.bor, "Table_Solvency_PG", gbv.original, gbv.residual, principal, n.loans)

# - area
tables$borrower_area <- created.tables$borrowers %>%
  make_table(ptf, area, "Table_PTF_Area", gbv.original, gbv.residual, principal, n.loans)

###### -- Guarantees                        --####
source("tables/loans_guarantees.R")

tables$guarantor.with.gbv <- created.tables$loans.guarantees %>%
    make_table(ptf, range.gbv.residual, "Table_Guarantor_GBV", 
               gbv.original, gbv.residual, principal,n_guarantees, total_amount_guaranteed)


# - guarantors yes/no with type guarantors and solvency.guarantor
tables$guarantor.with.type <- created.tables$loans.guarantees %>%
  make_table(ptf, type, "Table_Guarantor_type", gbv.original, gbv.residual, principal, n_guarantees, total_amount_guaranteed)


# tables$guarantor.with.status <- 
#   created.tables$loans.guarantees %>%
#   mutate(n.loans=1) %>%
#   make_table(flag_guarantor, status,
#              "Table_Guarantor_Status",
#              gbv.original, gbv.residual, principal, n.loans, n_guarantees)

tables$guarantor.with.lien <- created.tables$loans.guarantees %>% filter(type=="lien") %>%
  make_table(origin.lien, rank.lien, "Table_Guarantor_lien",
             gbv.original, gbv.residual, principal) %>% mutate(subcluster = as.character(subcluster))


#### -- Agreement summary pre-analysis      --####
source("tables/agreement_summary.R") #creation of temp.vars$agreement.summary with analysis detailed

#### -- Agreement summary tables creation   --####

tables$agrement.status <- created.tables$agreement.summary %>%
  make_table(ptf, status, "Table_Agreement_Status", gbv.agreement, amount.agreement, paid)

tables$agrement.discount <- created.tables$agreement.summary %>%
  make_table(ptf, range.discount, "Table_Agreement_discount", gbv.agreement, amount.agreement, paid)

tables$agrement.status.discount <- created.tables$agreement.summary %>%
  make_table(range.discount, status, "Table_Agreement_status_discount", gbv.agreement, amount.agreement, paid)

tables$agrement.amountRange <- created.tables$agreement.summary %>%
  make_table_one_var(range.amount, "Table_Agreement_amountRange", gbv.agreement, amount.agreement, paid, n.instalment)

#They're all borrowers
#there's a many to many relationship but since I'm not gonna use it, idc
tables$paying.pdr.guarantor.borrower <- created.tables$agreement.summary %>%
  left_join(df0$counterparty %>% select(id.counterparty, role), by="id.counterparty") %>% 
  make_table(ptf, role, "Table_Role_PDR", amount.agreement, paid, residual)

#If it's dead that's probably why it stopped paying
#I only have 7 failed dead
tables$pdr.dead <- created.tables$agreement.summary %>%
  left_join(link0$counterparty.entity, by="id.counterparty") %>%
  left_join(df0$entity %>% select(id.entity, solvency.pf), by="id.entity") %>% 
  make_table(status, solvency.pf, "Table_PDR_Status_Solvency", amount.agreement, paid, residual)
tables$pdr.dead %>% View()


#### -- PPT summary pre-analysis            --####
source("tables/ppt_summary.R") #creation of temp.vars$ppt_summary with analysis detailed

#### -- PPT summary tables creation --####
tables$ppt.status <- created.tables$ppt.summary %>%
  make_table(cluster.ptf, status, "Table_PPT_Status", amount.ppt, paid, residual)

tables$ppt.range.amount <- created.tables$ppt.summary %>%
  make_table(cluster.ptf, range.amount.ppt, "Table_PPT_Range_Amount", amount.ppt, paid, residual)

tables$ppt.year <- created.tables$ppt.summary %>%
  make_table_one_var(year.ppt, "Table_PPT_Year", amount.ppt, paid, residual) %>% 
  mutate(cluster= as.character(cluster))

#They're all Vienna's borrowers
tables$paying.guarantor.borrower <- created.tables$ppt.summary %>%
  left_join(link0$counterparty.entity, "id.entity") %>%
  left_join(df0$counterparty %>% select(id.counterparty, role), by="id.counterparty") %>% 
  make_table_one_var(role, "Table_Role", amount.ppt, paid, residual)
  
#If it's insolvent that's probably why it stopped paying
#I only have 5 failed insolvent
tables$ppt.insolvent <- created.tables$ppt.summary %>%
  left_join(df0$entity %>% select(id.entity, solvency.pf), by="id.entity") %>%
  make_table(status, solvency.pf, "Table_PPT_Status_Solvency", amount.ppt, paid, residual)


#### -- Collections  ###########
source("tables/collections.summary.R")

tables$collections.class.type.ptf <- created.tables$collections.summary %>%
  mutate(class.type = ifelse(!is.na(class), paste(class, type, sep = "-"), NA)) %>%
  make_table(ptf, class.type, "Table_Collection_Class_Type_PTF", gbv.agreement, amount.expected, paid, gbv.original)

tables$collections.class.year.orchestra <- created.tables$collections.summary %>% filter(ptf=="orchestra") %>%
  make_table(year, class, "Table_Collection_Year_Class_Orchestra", gbv.agreement, amount.expected, paid, gbv.original) %>% 
  mutate(cluster=as.character(cluster))

tables$collections.class.year.candia <- created.tables$collections.summary %>% filter(ptf=="candia") %>%
  make_table(year, class, "Table_Collection_Year_Class_Candia", gbv.agreement, amount.expected, paid, gbv.original)%>% 
  mutate(cluster=as.character(cluster))

tables$collections.class.year.vienna <- created.tables$collections.summary %>%filter(ptf=="vienna") %>%
  make_table(year, class, "Table_Collection_Year_Class_Vienna", gbv.agreement, amount.expected, paid, gbv.original)%>% 
  mutate(cluster=as.character(cluster))

tables$collections.class.type <- temp.vars$collections %>%  
  make_table(class, type, "Table_Collection_Class_Type", paid)

tables$collections.amountRange <-  temp.vars$collections %>%
  make_table(class,range.amount, "Table_Collection_amountRange", paid)



#Solvency_pf and PDR/PPT status
tables$collections.solvency_status.vienna <- created.tables$collections.summary %>%filter(ptf=="vienna") %>% 
  left_join(created.tables$borrowers %>% select(id.bor, super.status.bor), by="id.bor") %>% 
  make_table(status, super.status.bor, "Table_Collection_Solvency_Status_Vienna", gbv.agreement, amount.expected, paid, gbv.original)

#Solvency_pf and discounts 
#Doing it with the super.status.bor there's really no difference for each status. Only when the discount is +50% it's for more insolvents than solvents
#Doing it for status.bor intead of super.status.bor doesn't really make much difference, so it's not worth it
tables$collections.solvency_discount.vienna <- created.tables$agreement.summary %>%filter(ptf=="vienna") %>% 
  left_join(created.tables$borrowers %>% select(id.bor, super.status.bor), by="id.bor") %>% 
  make_table(range.discount, super.status.bor, "Table_Collection_Solvency_Discount_Vienna", gbv.agreement, amount.agreement, paid, gbv.original)


tables$collections.solvency_detail.vienna <- created.tables$collections.summary %>%filter(ptf=="vienna") %>% 
  left_join(created.tables$borrowers %>% select(id.bor, super.status.bor, detail.status.bor), by="id.bor") %>% 
  make_table(super.status.bor, detail.status.bor, "Table_Collection_Solvency_Detail_Vienna", gbv.agreement, amount.expected, paid, gbv.original)

tables$collections.solvency_detail.orchestra <- created.tables$collections.summary %>%filter(ptf=="orchestra") %>% 
  left_join(created.tables$borrowers %>% select(id.bor, super.status.bor, detail.status.bor), by="id.bor") %>% 
  make_table(super.status.bor, detail.status.bor, "Table_Collection_Solvency_Detail_orchestra", gbv.agreement, amount.expected, paid, gbv.original)

tables$collections.solvency_detail.candia <- created.tables$collections.summary %>%filter(ptf=="candia") %>% 
  left_join(created.tables$borrowers %>% select(id.bor, super.status.bor, detail.status.bor), by="id.bor") %>% 
  make_table(super.status.bor, detail.status.bor, "Table_Collection_Solvency_Detail_candia", gbv.agreement, amount.expected, paid, gbv.original)



#####
temp.vars$collections.for.graph <- df0$collection %>% 
  left_join(created.tables$borrowers %>% select(id.bor, id.group, ptf), by=c("id.bor", "id.group")) %>%
  mutate(month_year = format(date, "%Y-%m")) %>% 
  group_by(month_year, class, ptf) %>% 
  summarise(total_amount = sum(amount), .groups="drop") 

ggplot(temp.vars$collections.for.graph %>%  filter(ptf=="vienna"), aes(x = as.Date(paste0(month_year, "-01")), y = total_amount, color=class)) +
  geom_line() +
  labs(x = "Date", y = "Amount", title = "Vienna's Collections by Class Over Time") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month") +
  scale_y_continuous(labels = label_number())+
  theme_minimal()

ggplot(temp.vars$collections.for.graph %>%  filter(ptf=="orchestra"), aes(x = as.Date(paste0(month_year, "-01")), y = total_amount, color=class)) +
  geom_line() +
  labs(x = "Date", y = "Amount", title = "Orchestra's Collections by Class Over Time") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month") +
  scale_y_continuous(labels = label_number())+
  theme_minimal()

ggplot(temp.vars$collections.for.graph %>%  filter(ptf=="candia"), aes(x = as.Date(paste0(month_year, "-01")), y = total_amount, color=class)) +
  geom_line() +
  labs(x = "Date", y = "Amount", title = "Candia's Collections by Class Over Time") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 month") +
  scale_y_continuous(labels = label_number())+
  theme_minimal()


#### -- Extra tables ####
created.tables$borrowers %>% filter(ptf=="candia", type.bor=="corporate") %>% 
  group_by(super.status.bor) %>% 
  summarise(n.borrowers = n_distinct(id.bor), sumGBV= sum(gbv.residual)) %>% View()

#####

combined_tables <- bind_rows(tables) %>% select(title, cluster, subcluster, item, amount)
write.csv(combined_tables, "data.csv")
