### Creation of the borrowers table        


# 400 borrowers exist in more than one borrower 
#check_multiple_associations(df0$loan, id.bor)%>% View


#Vars to group based on the previous check -> 1370536 is in two portfolios <- I'm keeping both cases
temp.vars$breaks <- c(0, 15000, 30000, 50000, 100000, 250000, Inf)
temp.vars$labels <- c("0-15k", "15-30k", "30-50k", "50-100k", "100-250k", "250k+")
created.tables$borrowers <- df0$loan %>% 
  arrange(desc(gbv.original)) %>% 
  group_by(id.bor) %>% 
  summarise(
    n.loans = n(),
    across(c(originator, id.group, ptf, cluster.ptf), first), # it takes the cluster.ptf with the highest gbv since I arranged by it
    across(c(gbv.original, gbv.residual, principal), ~sum(., na.rm = TRUE)),
    status.debt= first(status), #I put it here so I rename it
    date.status = min(date.status), # Since the status is the same for each borrower, I take the oldest date
    type.debt = first(factor(type,levels=c("mortgages (fondiario)","mortgages","personal loans","bank accounts","credit cards", "other"))),
    .groups = "drop"
  ) %>%
  
  left_join(df0$counterparty %>% 
              filter(role=="borrower") %>% 
              select(id.counterparty, id.bor, name), 
            by="id.bor") %>%
  
  left_join(link0$counterparty.entity, by="id.counterparty", relationship = "many-to-many") %>%
  
  left_join(df0$entity %>% 
              mutate(status.bor=coalesce(status.pg, solvency.pf)) %>% 
              select(id.entity, type.bor=type.subject, status.bor, province, region, area, solvency.pf), 
            by="id.entity") %>%
  select(-id.entity, -id.counterparty) %>% 
  distinct() %>%
  
  group_by(id.bor) %>%
  mutate(type.bor=case_when(
    "individual" %in% type.bor ~ "individual",
    TRUE ~ first(type.bor)  # Use the first value if conditions are not met
  )) %>%
  arrange(factor(status.bor, levels=c("deceased", "pensioner", "employee - permanent", "employee - n/a", "employee - temporary", "real estate", "self employed", "insolvent",
                                      "canceled","ceased","inactive","bankruptcy","liquidation", "insolvency", "other", "active"))) %>%
  mutate(
    status.bor= as.character(status.bor),
    status.bor = ifelse(type.bor=="individual" & is.na(solvency.pf), NA, status.bor)) %>% select(-solvency.pf) %>%
  slice(1) %>% 
  ungroup() %>%
  
  mutate(
    range.gbv.residual = cut(gbv.residual, breaks = temp.vars$breaks, labels = temp.vars$labels, include.lowest = TRUE),
    range.gbv.original = cut(gbv.original, breaks = temp.vars$breaks, labels = temp.vars$labels, include.lowest = TRUE),
    vintage = year(today()) - year(date.status),  # Calculate vintage
    range.vintage = cut(vintage, breaks = c(0, 1, 2, 3, 6, 11, 21, Inf),  
                        labels = c("0y", "1y", "2y", "3-5y", "6-10y", "11-20y", "20+"), 
                        include.lowest = TRUE),

    super.originator = case_when(
      str_detect(originator, "vela obg srl") ~ "vela obg srl",
      str_detect(originator, "bnl") ~ "bnl",
      str_detect(originator, "bnp paribas sa") ~ "bnp paribas sa",
      TRUE ~ as.character(originator)
    )
  )
