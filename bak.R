colSums(is.na(df0$loan))

check_multiple_associations(df0$loan, id.bor) %>% filter(cluster.ptf>1) %>% select(id.bor) %>% 
  left_join(df0$loan %>% select(id.bor, ptf, cluster.ptf), by="id.bor") %>% View()
#I wanted to see the loans (when the borrower only has one loan, so I'm sure of which loan we're talking about) but there are only 132, it isn't worth it
df0$collection %>%
  left_join(df0$loan, by = c("id.bor", "id.group")) %>%
  group_by(id.bor, id.group) %>%
  filter(n() == 1) %>%  # Retain rows where there's only one match
  ungroup() #%>% View()

#Previous code to only analyse collections
temp.vars$collections <- temp.vars$collections %>% group_by(id.bor, id.group) %>%
  summarise(
    date.first.payment=min(date.first.payment),
    date.last.payment= max(date.last.payment),
    total.amount=sum(total.amount),
    class =first(factor(class, levels = c("judicial", "extrajudicial"))),
    type= first(factor(type, levels = c("ppt", "agreement", "spontaneous"))),
    .groups = "drop"
  ) %>% mutate(
    range.amount = cut(
      total.amount,
      breaks = c(0, 2500, 5000, 10000, 20000, Inf),  # Adjusted breaks
      labels = c("0-2.5k", "2.5k-5k", "5k-10k", "10k-20k", "20k+"),
      include.lowest = TRUE
    )
  )
#temp.vars$collections %>% View()


#Previous code of agreement.collection
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

# temp.vars$agreement.proj <- df0$agreement.summary %>% 
#   group_by(id.bor, id.agreement) %>%
#   summarise(
#     across(c(id.counterparty,date.agreement,gbv.agreement, amount.agreement, date.start, date.end, n.instalment, length), first),
#     status = first(factor(status, c("closed", "active", "failed", "proposal"))),
#     paid = sum(paid),
#     residual=min(residual),
#     date.last.payment=max(date.last.payment),
#     .groups = "drop"
#   )
#   
# temp.vars$agreement.proj  %>% View()

#####
#see only the ones that finish with srl
#df0$loan %>% filter(str_match(ptf, "srl|s.r.l.")) %>% View() 

loans <- df0$loan
loans %>% select(ptf, cluster.ptf) %>% distinct() %>% View()
df0$loan %>% mutate(
  super.ftp = case_when(
    str_detect(ptf, "srl|s\\.r\\.l\\.") ~ "SRLs",
    str_detect(ptf, "nf_") ~ "NFs",
    str_detect(ptf, "bb") ~ "BBs",
    TRUE ~ as.character(ptf)
  ),
  super.cluster.ptf = case_when(
    str_detect(cluster.ptf, "small") ~ "small",
    str_detect(cluster.ptf, "master legal") ~ "master legal",
    str_detect(cluster.ptf, "light") ~ "light",
    TRUE ~ as.character(cluster.ptf)
  ),
) %>% group_by(super.ftp, super.cluster.ptf) %>% 
  summarise(sum.gbv=sum(gbv.residual), per_port=n()/nrow(df0$loan)) %>% 
  View()

#####
link0$counterparty.entity %>% nrow()
df0$counterparty %>% filter(!id.counterparty %in% link0$counterparty.entity$id.counterparty)
link0$counterparty.entity %>% View()
df0$counterparty %>% nrow()



####-- Understanding missing values in the link counterparties <- counterparties side (all bullshit) ---####
#Try to link more info - and understand why I have so many missing links
temp.vars$loan.bors <- df0$loan %>% distinct(id.bor) #3040
temp.vars$l.c.e.bors <- link0$counterparty.entity %>% left_join(df0$counterparty, by="id.counterparty") %>% 
  filter(role=="borrower") %>% select(id.bor) #1058 → I'm missing almost 2000 bors

df0$counterparty %>% filter(role=='borrower') %>% filter(id.bor %in% df0$loan$id.bor)  %>% summarise(n_distinct(id.bor)) #3040 
#I HAVE ALL THE BORS IN COUNTERPARTY, BUT NOT ALL COUNTERPARTIES IN THE LINK 

#I take the id.entity from guarantee when the role of the counterparty is guarantor 
temp.vars$new_guarantees_entities <- df0$counterparty %>% filter(role=="guarantor") %>% 
  left_join(df0$guarantees %>% select(id.bor, id.group, id.guarantee), by=c("id.bor", "id.group")) %>% 
  left_join(link0$guarantee.entity, by="id.guarantee") %>% select(-id.guarantee) %>% distinct() %>% 
  filter(!(is.na(id.entity)) & !(id.bor %in% temp.vars$l.c.e.bors)) %>% select (id.counterparty, id.entity)

link0$counterparty.entity <- bind_rows(link0$counterparty.entity, temp.vars$new_guarantees_entities) %>%  distinct()


#nrow(df0$counterparty) #4456
counterparties <- df0$counterparty %>% group_by(id.counterparty, id.bor) %>% n_distinct()
df0$counterparty <- df0$counterparty %>% group_by(id.bor, id.group, role, name, n.entities, flag.imputed) %>% 
  mutate(id= cur_group_id()) %>% ungroup() %>% left_join(link0$counterparty.entity, by="id.counterparty")

link0$counterparties.entity <- df0$counterparty %>% select(old.id.counterparty= id.counterparty, new.id.counterparty=id, id.entity)
#When the counterparty is actually the same, I give it the same id.entity as link
link0$counterparties.entity <- link0$counterparties.entity %>%
  group_by(new.id.counterparty) %>%
  fill(id.entity, .direction = "up") %>% fill(id.entity, .direction = "down") %>%
  ungroup()
#colSums(is.na(link0$counterparties.entity)) #I still have a ton of NAs 
df0$counterparty <- df0$counterparty %>% select(id.counterparty=id, id.bor, id.group, role, name, n.entities, flag.imputed) %>% distinct()  

#check_:
#find_duplicates(counterparties %>% filter(role=="borrower"), id.bor) %>% View()
# remain duplicates: 1221424, 1442444 because of different n.entities -> I take the highest number
# 1370536 remains duplicate since it has two different id.group -> I leave it like it is

#hard fix:
temp.vars$dup.bors <- c("1221424", "1442444")
temp.vars$duplicated_counterparties <- df0$counterparty %>% filter(id.bor %in% temp.vars$dup.bors) %>% group_by(id.bor) %>% mutate(n.entities = max(n.entities)) %>% slice(1)
df0$counterparty <- bind_rows(df0$counterparty %>% filter(!(id.bor %in% temp.vars$dup.bors)), temp.vars$duplicated_counterparties) 
#df0$counterparty %>% filter(role=="borrower") %>% nrow() #=3401 
#n_distinct(df0$loan$id.bor) #= 3400 (I have an extra one for the one with two portfolios)


