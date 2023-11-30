temp.vars$agreement.collection <- df0$collection %>% filter(class=="extrajudicial" & type=="agreement") %>%
  left_join(df0$agreement.summary, by=c("id.bor", "id.group"), relationship = "many-to-many") %>%
  group_by(id.agreement, id.bor) %>%
  filter(date>date.agreement & date<date.end) %>%
  summarise(
    across(c(amount.agreement, status, residual, paid), first),
    paid.in.collections=ifelse(status=="failed", sum(amount[date<=date.last.payment]),sum(amount)),
    date.last.paid=max(date),
    .groups = "drop"
  ) %>%
  filter(paid!=paid.in.collections & id.bor!= "74691550") #this weirdo has two active agreements  