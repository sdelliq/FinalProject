read_ <- function(x, y) feather::read_feather(paste0("Data/", paste("Assignment", x, y, sep = "_") %>% paste0(".feather")))


find_duplicates <- function(df, column){
  return(df %>% filter(duplicated({{column}})|duplicated({{column}}, fromLast=T)))
}
#find_duplicates(infoprov.tocheck, cf.piva) %>% View()


find_duplicates_couple <- function(df, column1, column2) {
  return(df %>% 
           filter(duplicated(select(., {{ column1 }}, {{ column2 }})) | 
                    duplicated(select(., {{ column1 }}, {{ column2 }}), fromLast = TRUE))
  )
}

#All the columns that don't appear in this result, are ok to be summarised with first(column), since there's only one value
# I filter both by row and column (if the rows are very few, you can (not) categorize in Andrea's way: 'sti cazzi')
check_multiple_associations <- function(df, column)(
  df %>%
    group_by({{column}}) %>%
    summarise_all(n_distinct) %>%
    filter_all(any_vars(.>1)) %>%
    select({{column}}, where(~ is.numeric(.) && any(. > 1)))
) 


update_type.pg_column <- function(data) {
  result <- data %>%
    mutate(type.pg = case_when(
      str_detect(name, "societa' a responsabilita' limitata|srl\\b|s.r.l\\b|s.r.l.\\b|srls\\b")  ~ "srl",
      str_detect(name, "societa' per azioni|spa\\b|s.p.a\\b|s.p.a.\\b")  ~ "spa",
      str_detect(name, "d.i\\b|d.i.\\b")  ~ "di",
      str_detect(name, "ss\\b|s.s\\b|s.s.\\b|societa' semplice\\b")  ~ "ss",
      str_detect(name, "societa' in accomandita semplice|sas \\b|s.a.s\\b|s.a.s.\\b")  ~ "sas",
      str_detect(name, "societa' in nome collettivo|snc\\b|s.n.c\\b|s.n.c.\\b|sncs\\b")  ~ "snc",
      str_detect(name, "cooperativa|consortile|fidi|sc\\b |s.c\\b|s.c.\\b|scs\\b")  ~ "sc",
      str_detect(name, "ass.zione|associazione|non precisata") ~ "other",
      TRUE ~ type.pg
    ))
}

make_table <- function(df, x, y, title, ...) {
  df %>%
    mutate(n = 1L) %>%
    group_by(!!enquo(x), !!enquo(y)) %>%
    summarize_at(vars(n, !!!enquos(...)), sum, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_longer(c(n, !!!enquos(...)), names_to = "item", values_to = "amount") %>%
    rename(cluster := quo_name(ensym(x)),
           subcluster := quo_name(ensym(y))) %>%
    mutate(title = title, amount = as.numeric(amount)) %>%
    select(title, cluster, subcluster, item, amount)
}

make_table_one_var <- function(df, x, title, ...) {
  df %>%
    mutate(n = 1L) %>%
    group_by(!!enquo(x)) %>%
    summarize_at(vars(n, !!!enquos(...)), sum, na.rm = TRUE) %>%
    ungroup() %>%
    pivot_longer(c(n, !!!enquos(...)), names_to = "item", values_to = "amount") %>%
    rename(cluster := quo_name(ensym(x))) %>%
    mutate(title = title, amount = as.numeric(amount)) %>%
    select(title, cluster, item, amount)
}























################ -- Andrea's Functions --##########

###......... EXPORT: it prints the selected data and opens an excel file directly
export <- function(list.df, row.start = 2, col.start = 2) {
  
  #########......... 1) INTRO
  
  ###........ Wrap the DF into a list (if it is not already)
  if (!"list" %in% class(list.df)) {list.df <- list(list.df)}
  
  ###........ Define a command that allows you to open a computer file from R directly (but not IN R! Very cool function)
  open_command  <-  switch(Sys.info()[['sysname']],
                           Windows= 'open',
                           Linux  = 'xdg-open',
                           Darwin = 'open')
  
  ###........ Create a temporary file in the computer and an excel workbook in the R environment
  file.temp  <-  paste0(tempfile(), '.xlsx')
  wb  <-  createWorkbook()
  
  title.sheet = "data"
  
  
  #########......... 2) CREATE WORKBOOK
  
  ###......... Add worksheets
  addWorksheet(wb, title.sheet)
  showGridLines(wb, title.sheet, showGridLines = FALSE)
  
  ###......... Setting styles
  style.header  <-  createStyle(fgFill = "#366092", fontColour="white",halign = "LEFT", textDecoration = "bold", border = "bottom")
  style.title <- createStyle(border = "TopBottomLeftRight", borderColour = "black", borderStyle = "thick", textDecoration = "bold", halign = "center")
  
  style.table <- createStyle(halign = "left", border = "TopBottom", borderColour = "gray", borderStyle = "thin")
  # style.freq <- createStyle(numFmt = "#,##0;-#,##0;-", halign = "right")
  style.int <- createStyle(numFmt = "0;-0", halign = "right")
  style.perc <- createStyle(numFmt = "0.0%;-0.0%;-", halign = "right")
  style.num <- createStyle(numFmt = "#,##0.0;-#,##0.0;-", halign = "right")
  style.date <- createStyle(numFmt = "dd/mm/yyyy", halign = "left")
  style.char <- createStyle(halign = "left")
  
  
  #########......... 3) WRITE THE DATA (LOOP)
  for (i in 1:length(list.df)) {
    
    ###......... a) Select Data
    if(i > 1) {data.0 <- list.df[[i-1]]}  # previous table
    data.1 <- list.df[[i]]                # current table
    
    ###......... b) Declare Variable range for styles
    #. Frequencies
    # list.freq  <-  c("n","freq","count") %>% 
    #   paste0("^",.,"$") %>% paste(collapse = "|") %>%
    #   grep(colnames(data.1), value = T)
    
    #. Integers
    condition.int <- which(sapply(data.1, class)== "numeric")
    if (length(condition.int) > 0) {
      list.int  <-    data.1[condition.int] %>% 
        apply(2, FUN = function(x) x%%1 != 0) %>% 
        apply(2, FUN = function(x) sum(x, na.rm = T) == 0) %>%
        which() %>% names() #%>%
      # outersect(list.freq)
    } else {list.int <- character(0)}
    
    #. Percentages
    list.perc  <-  c("p","perc","weight","red","mod","surv","pdr","quota", "ratio") %>% 
      paste0("\\b",.,"\\b") %>% paste(collapse = "|") %>%
      grep(colnames(data.1), value = T)
    
    #. Numbers (not integers nor perc) and Dates
    # list.num <- which(sapply(data.1, class)== "numeric") %>% names() %>% outersect(c(list.freq, list.int, list.perc))
    list.num <- which(sapply(data.1, class)== "numeric") %>% names() %>% outersect(c(list.int, list.perc))
    list.date <- which(sapply(data.1, class)== "Date") %>% names()
    list.char <- c(which(sapply(data.1, class)== "character") %>% names(),
                   which(sapply(data.1, class)== "factor") %>% names())
    
    
    ###......... c) Cell coordinates
    title <- names(list.df)[i]
    if(i == 1) {row.table <- row.start + 2} else {row.table <- row.table + (nrow(data.0) + 4)}
    row.title <- row.table - 1
    
    colrange.table <- col.start:(col.start + ncol(data.1) - 1)
    
    # if (length(list.freq)>0) colrange.freq <- colrange.table[grep(list.freq %>% paste(collapse = "|"), colnames(data.1))]
    if (length(list.int)>0) colrange.int <- colrange.table[grep(list.int %>% paste(collapse = "|"), colnames(data.1))]
    if (length(list.perc)>0) colrange.perc <- colrange.table[grep(list.perc %>% paste(collapse = "|"), colnames(data.1))]
    if (length(list.num)>0) colrange.num <- colrange.table[grep(list.num %>% paste(collapse = "|"), colnames(data.1))]
    if (length(list.date)>0) colrange.date <- colrange.table[grep(list.date %>% paste(collapse = "|"), colnames(data.1))]
    if (length(list.char)>0) colrange.date <- colrange.table[grep(list.char %>% paste(collapse = "|"), colnames(data.1))]
    
    rowrange.table <- row.table:(row.table + nrow(data.1)) + 1
    
    ###......... e) Write and format Title
    mergeCells(wb, title.sheet, rows = row.title, cols = colrange.table)
    addStyle(wb, title.sheet, style.title, rows = row.title, cols = colrange.table)
    writeData(wb, title.sheet, title, startRow = row.title, startCol = colrange.table[1])
    
    ###......... f)  Write and format Table  
    writeData(wb, title.sheet, data.1, startCol = col.start, startRow = row.table, rowNames = FALSE, headerStyle = style.header)
    addStyle(wb, title.sheet, style.table, rows = rowrange.table, cols = colrange.table, gridExpand = T, stack = T)
    # if (length(list.freq)>0) addStyle(wb, title.sheet, style.freq, rows = rowrange.table, cols = colrange.freq, gridExpand = T, stack = T)
    if (length(list.int)>0) addStyle(wb, title.sheet, style.int, rows = rowrange.table, cols = colrange.int, gridExpand = T, stack = T)
    if (length(list.perc)>0) addStyle(wb, title.sheet, style.perc, rows = rowrange.table, cols = colrange.perc, gridExpand = T, stack = T)
    if (length(list.num)>0) addStyle(wb, title.sheet, style.num, rows = rowrange.table, cols = colrange.num, gridExpand = T, stack = T)
    if (length(list.date)>0) addStyle(wb, title.sheet, style.date, rows = rowrange.table, cols = colrange.date, gridExpand = T, stack = T)
    if (length(list.char)>0) addStyle(wb, title.sheet, style.char, rows = rowrange.table, cols = colrange.date, gridExpand = T, stack = T)
    
  }
  
  #########......... 4) COLWIDTH
  ###... Adjust ColWidth ("auto" won't work)
  ###... Take DF with max width and set length as the length of the row with most char for every col
  df.max <- list.df[[which(length(list.df) == max(length(list.df)))]] 
  colrange.width <- col.start:(col.start + ncol(df.max))
  width.vec <- apply(df.max, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
  width.vec.header <- nchar(colnames(df.max))  + 2
  setColWidths(wb, title.sheet, cols = colrange.width, widths = pmin(pmax(width.vec, width.vec.header), 30))
  
  #########......... 5) SAVE
  # setColWidths(wb, title.sheet, cols = colrange.table, widths = "auto")
  saveWorkbook(wb, file.temp, overwrite = TRUE)
  system(paste(open_command, file.temp))
  
}


###......... FREQ gives you a table with the frequencies of a certain variable
###......... and count the grouped sums
###......... format: will format the numbers and perc
###......... sort: a vector that sorts the table 
###......... NOTE: FIX FOR FACTORS! They must be converted to characters or will fail
freq  <-  function(df, group, var.1 = NULL, var.2 = NULL, format = T, dev = F, n.dec = 2, p.dec = 1, ...) {
  #########......... 1) Store Arguments
  group <- enexpr(group)
  var.1 <- enexpr(var.1)
  var.2 <- enexpr(var.2)
  
  #########......... 2) Generate Table
  table <- df %>% hyper.freq(!!group, !!var.1, !!var.2, format = F, dec = n.dec, ...) %>%
    mutate(p.freq = freq/freq[1]) %>%
    select(!!group, freq, p.freq, !!var.1, !!var.2)
  
  #########......... 3) Percentages
  if (!is.null(var.1) & !is.null(var.2)) {
    if (isTRUE(dev)) {
      table  <-  table %>% mutate(dev = !!var.2 - !!var.1, 
                                  p.dev = !!var.2/!!var.1 - 1)
    } else {
      table <- table %>% mutate(p.ratio = !!var.2/!!var.1)
    }
  } else if (!is.null(var.1) & is.null(var.2)) {
    table <- table %>% mutate(p.var = !!var.1/(!!var.1)[1])
  }
  
  ###... Remove 100% freq sums and round
  index.perc <- grep("^p\\.freq$|^p\\.var$", colnames(table))
  table[1, index.perc] <- NA
  table[, index.perc] <- round(table[, index.perc], p.dec + 2)
  
  #########......... 4) Format
  suppressWarnings({
    if (isTRUE(format)) {
      ###... Format frequencies and vars
      table  <-  table %>%
        mutate(freq = printf(freq, dec = 0)) %>%
        mutate_at(vars(matches("^dev$")), printf, dec = n.dec) %>%
        mutate_at(vars(starts_with("p.")), printf.perc, dec = (p.dec)) %>%
        mutate_at(vars(!!var.1, !!var.2), function(x) printf(x, dec = n.dec))
      
      ###... Replace NAS with blanks
      table  <-  table %>% replace(is.na(.) | . == "NA", "")
    }
  })  
  return(table)
}



###......... HYPER.FREQ: "freq" with unlimited columns
hyper.freq  <-  function(df, group, ..., type = "sum", all.levels = T , sort = NULL, desc = F, 
                         scale = "base", dec = 2, format = T) {
  
  #########......... 1) INTRO
  ###......... Quote variables
  group  <-  enquo(group)
  list.var <- enquos(...)
  
  ###......... Store levels if present
  if (is.factor(df[[quo_name(group)]])) {
    levels <- df[[quo_name(group)]] %>% levels()
  } else {
    levels <- df[[quo_name(group)]] %>% unique() %>% sort() %>% as.character()
  }
  
  
  ###......... Add counter to DF and make group var a character
  df <- df %>% mutate(freq = 1, !!group := as.character(!!group))
  
  ####......... Define function (sum, mean, median)
  fun.type  <-  function(var, FUN = sum) {
    expr_fun  <-  parse_expr(paste0(type,"(var)"))
    eval(expr_fun)
  }
  
  #########......... 2) Create freq table according to the number or arguments used
  list.select <- quos(!!group, matches("^freq_sum$|^sum$"), matches("_fun.type"), -matches("freq_fun.type"))
  
  table.body <- df %>% 
    group_by(!!group) %>% 
    summarize_at(vars(freq, !!!list.var), list(~sum(.), ~fun.type(.))) %>%
    mutate(!!group := replace_na(!!group,"NA")) %>%
    select(!!!list.select) %>%
    setNames(colnames(.) %>% gsub("^sum$","freq",.) %>% gsub("_sum|_fun.type","",.))
  
  table.sum <- df %>% 
    summarize_at(vars(freq, !!!list.var), list(~sum(.), ~fun.type(.))) %>%
    mutate(!!group := "TOTAL") %>%
    select(!!!list.select) %>%
    setNames(colnames(.) %>% gsub("^sum$","freq",.) %>% gsub("_sum|_fun.type","",.))
  
  table.final  <-  bind_rows(table.sum, table.body) 
  
  #########......... 3) Insert empty levels
  if (isTRUE(all.levels)) {
    ###......... a) Find missing arguments
    length.table  <-  ncol(table.final)
    row.temp  <-  outersect(table.final[[1]], levels) %>% intersect(levels) %>% as.character()
    
    ###......... b) Generate the row and bind it to the table
    df.temp  <-  matrix(0, nrow = length(row.temp), ncol = ncol(table.final), 
                        dimnames = list(NULL, colnames(table.final))) %>% data.frame()
    df.temp[[1]]  <-  row.temp
    table.final  <-  table.final %>% bind_rows(df.temp)
  }
  
  #########......... 4) Sort
  if (isTRUE(desc)) {
    if (is.null(sort)) {
      match  <-  match(table.final[[1]], c("TOTAL","NA", rev(levels)))
    } else if (length(sort) == 1) {
      table.final <- table.final %>% arrange(desc(!!sym(sort)))
      match  <-  match(table.final[[1]], c("TOTAL","NA"))
    } else if (length(sort) > 1) {
      match  <-  match(table.final[[1]], c("TOTAL","NA", rev(sort)))
    }  
  } else {
    if (is.null(sort)) {
      match  <-  match(table.final[[1]], c("TOTAL","NA", levels))
    } else if (length(sort) == 1) {
      table.final <- table.final %>% arrange(!!sym(sort))
      match  <-  match(table.final[[1]], c("TOTAL","NA"))
    } else if (length(sort) > 1) {
      match  <-  match(table.final[[1]], c("TOTAL","NA", sort))
    }  
  }
  
  table.final <- table.final[order(match, decreasing = F),]
  row.names(table.final)  <-  NULL 
  
  #########......... 5) Scale
  table.final  <-  table.final %>% mutate_at(vars(!!!list.var), printf, scale = scale, dec = dec, format = F)
  
  
  #########......... 6) Format
  suppressWarnings({
    if (isTRUE(format)) {
      ###... Format frequencies and vars
      table.final  <-  table.final %>%
        mutate(freq = printf(freq, dec = 0)) %>%
        mutate_at(vars(!!!list.var), function(x) printf(x, dec = dec))
      
      ###... Replace NAS with blanks
      table.final  <-  table.final %>% replace(is.na(.) | . == "NA", "")
    }
  })  
  
  #########......... 7) Return
  return(table.final %>% as.data.frame())
}


###......... PRINTF prints a number with various format and decimals
printf  <-  function(num, scale = "base", dec = 2, format = T) {
  
  ###... Round
  num.round <- case_when(scale == "k" ~ as.numeric(num)/10^3,
                         scale == "m" ~ as.numeric(num)/10^6,
                         scale == "b" ~ as.numeric(num)/10^9,
                         TRUE ~ as.numeric(num)) %>% round(dec)
  
  if (isTRUE(format)) {
    ###... Format
    num.format <- case_when(is.na(num.round) ~ NA_character_,
                            num.round == 0 ~ "-",
                            T ~ num.round %>% format(digits = pmax(dec, 1), nsmall = dec, big.mark = ",") %>% str_squish())
    return(num.format) 
  } else {
    return(num.round)
  }
}

printf.perc  <-  function(num, dec = 2) {
  num.round <- num * 100 %>% round(dec)
  num.format <- case_when(is.na(num.round) ~ NA_character_,
                          num.round == 0 ~ "-",
                          T ~ num.round %>% format(digits = dec, nsmall = dec, big.mark = ",") %>%
                            paste("%") %>% str_squish())
  
  ###... Return
  return(num.format)
}
###......... OUTERSECT: it's the opposite of intersect. Gives you the non-common elements of two vectors
###......... p.s. "outersect" is very etymologically incorrect, since the opposite of "inter" in latin is not "outer"
###......... of course, but a creative guy on StackOverflow came up with this name and well, it was effective
outersect  <-  function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}