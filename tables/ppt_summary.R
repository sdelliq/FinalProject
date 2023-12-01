#I join the info from collections ppt and ppt.summary to see the differences
temp.vars$ppt.collection <- df0$collection %>% filter(type=="ppt") %>%
  left_join(df0$ppt.summary, by=c("id.bor", "id.group"), relationship = "many-to-many") %>% 
  group_by(id.bor) %>% 
  mutate(multiple.ppts= ifelse(n_distinct(id.ppt) > 1, TRUE, FALSE)) %>%  ungroup() %>%
  group_by(id.ppt, id.bor) %>%
  summarise(
    across(c(amount.ppt, year.ppt, status, residual, paid), first),
    paid.in.collections= ifelse(multiple.ppts==TRUE ,
                                sum(amount[date>date.agreement & date<=date.last.payment]), #Usually then there are two status, one failed or closedand I can take the date of the last.payment 
                                sum(amount)),
    date.last.paid=max(date.ppt),
    .groups = "drop"
  ) %>%
  distinct() %>%
  filter(paid!=paid.in.collections) 

#10 borrowers have a higher amount of paid in ppt.summary than in collections. 
# nrow(temp.vars$ppt.collection)

#There are 35k more in ppt.agreement than in collections
sum(temp.vars$ppt.collection$paid) - sum(temp.vars$ppt.collection$paid.in.collections)
#The geniuses added the amount of both classes, even when in ppt.summary we should only have the info of judicial ppt 
# I have to take the info from collections


temp.vars$ppt.summary <- df0$ppt.summary %>% left_join(temp.vars$ppt.collection %>% 
                                                         select(id.ppt, paid.in.collections, date.last.paid), 
                                                       by = join_by(id.ppt)) %>% 
  mutate(
    paid=ifelse(!is.na(paid.in.collections), paid.in.collections, paid),
    date.last.payment = ifelse(!is.na(date.last.paid), date.last.paid, date.last.payment),
    date.last.payment = as.Date(date.last.payment),
    residual =ifelse(residual==0,0,amount.ppt-paid),
    
    status.calculated = case_when( 
      (status=="closed" & residual ==0 & amount.ppt<paid) | paid == amount.ppt | residual==0 ~ 'closed',
      (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 > qt.months ~ 'failed',
      (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 <  qt.months ~ 'active',
      is.na(date.last.payment) ~ status,
      (paid != amount.ppt) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 > qt.months ~ 'failed',
      (paid != amount.ppt) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 < qt.months ~ 'active',
      TRUE ~ status
    )
  )
#temp.vars$ppt.summary %>% filter(status!=status.calculated) %>% View()
#19 different cases. I still have 28 closed, but I now have 68 active instead of 85 and 52 failed instead of 35. I take them, they make sense
temp.vars$ppt.summary <- temp.vars$ppt.summary %>% select(-status, status=status.calculated)
