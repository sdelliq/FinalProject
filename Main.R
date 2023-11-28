source("Library.R")
source("Functions.R")

temp.vars <- list()

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
source("borrowers.R")





