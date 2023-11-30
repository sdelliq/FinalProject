#35 borrowers have a higher amount of paid in ppt.summary than in collections. 
#the sum of paid not considering them is 730k, considering them is 656k. Considering the amount PPT and amount residual it makes more sense to leave the Paid as it was, and not take the amount from collection
temp.vars$ppt.collection <- df0$collection %>% filter(type=="ppt") %>%
  left_join(df0$ppt.summary, by=c("id.bor", "id.group"), relationship = "many-to-many") %>% 
  filter(date>date.start & date.ppt<date) %>% #
  group_by(id.ppt, id.bor) %>%
  summarise(
    across(c(amount.ppt, year.ppt, status, residual, paid), first),
    paid.in.collections=ifelse(status=="failed", sum(amount[date.ppt<=date.last.payment]),sum(amount.ppt)),
    date.last.paid=max(date.ppt),
    .groups = "drop"
  ) %>%
  filter(paid!=paid.in.collections) 

#Since the difference here it's negative, I definitively need to take the data from collections, I'm missing money
#sum(temp.vars$ppt.collection$paid) - sum(temp.vars$ppt.collection$paid.in.collections)
#sum(df0$ppt.summary$paid)

temp.vars$ppt.summary <- df0$ppt.summary %>% left_join(temp.vars$ppt.collection %>% 
                                                         select(id.ppt, paid.in.collections, date.last.paid), 
                                                       by = join_by(id.ppt)) %>% 
  mutate(
    paid=ifelse(!is.na(paid.in.collections), paid.in.collections, paid),
    date.last.payment = ifelse(!is.na(date.last.paid), date.last.paid, date.last.payment)
  )
temp.vars$ppt.summary