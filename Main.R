source("Library.R")
source("Functions.R")

temp.vars <- list()
date.cutoff <- as.Date("2023-11-15")
###-----------------------------------------------------------------------###
#-----          - Reading the files                                         -----         
###-----------------------------------------------------------------------###


df0 = list(
  loan = read_("Report", "Loans"),
  counterparty = read_("Report", "Counterparties"),
  entity = read_("Report", "Entities"),
  ppt.summary = read_("Report", "PPT.summary"),
  agreement.summary = read_("Report", "Agreement.summary"),
  collection = read_("Report", "Collections"),
  guarantees = read_("Report", "Guarantees")
)


link0 <- list(
  guarantee.entity = read_("Link", "Counterparties") %>% select(-flag.imputed),
  counterparty.entity = read_("Link", "Loans")
)


# Creation of the borrowers table   
source("tables/borrowers.R")


df0$loan <- df0$loan %>% 
  mutate(status.updated = case_when(
    status == "utp" & 
      (as.numeric(difftime(date.cutoff, date.status, units = "weeks")) >= 104) ~ "bad",  # 2 years = 104 weeks
    TRUE ~ as.character(status)
  ))


