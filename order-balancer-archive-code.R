####
# Generate true latin square
###

colors = unique(Lf$Color)
orders = unique(Lf$Condition)

color_combis = gtools::permutations(length(colors),length(colors),colors) %>% as.data.frame()
order_combis = gtools::permutations(length(orders),length(orders),orders) %>% as.data.frame()


#Ordered values
O = tibble(color = apply(color_combis, 1, function(row) paste(row,collapse=",")),
           order = apply(order_combis, 1, function(row) paste(row,collapse=",")),
)

final_orders = expand.grid(O$color, O$order)

final_orders = final_orders %>% rename(color_order = Var1, condition_order = Var2)

evaluate_imbalance <- function(odf, true_odf) {
  browser()
  imbalance = tibble(var1 = NA, var2 = NA)
  true_imbalance = tibble(var1 = NA, var2 = NA)
  imbalance_var1 = tibble(var = levels(true_odf$Var1), count = NA)
  imbalance_var2 = tibble(var = levels(true_odf$Var2), count = NA)
  odf %>% group_by(Var1) %>% summarise(count = n())
  imbalance$var1 = odf %>% group_by(Var1) %>% summarise(count = n())
  imbalance$var2 = odf %>% group_by(Var2) %>% summarise(count = n())
  true_imbalance$var1 = list(true_odf %>% group_by(Var1) %>% summarise(count = n()))
  true_imbalance$var2 = list(true_odf %>% group_by(Var2) %>% summarise(count = n()))
  
  
  return(list(imbalance,true_imbalance))
}

evaluate_imbalance(final_orders)



# shuffle final_orders
final_orders = rownames_to_column(final_orders)
final_orders_s = final_orders[sample(nrow(final_orders)), ]

# evaluate imbalance of the first 12
res = evaluate_imbalance(final_orders_s[1:12,], final_orders)

odf <- final_orders[sample(nrow(final_orders)),]

df <- df[sample(nrow(df)),]



####
# Library versus state
###

# Check State

Lf = Lf %>% group_by(Participant, Game) %>%
  summarise(
    color_order = paste(unique(Color), collapse=","),
    condition_order = paste(unique(Condition), collapse=",")
  ) %>% right_join(Lf)

Slf = Lf %>% group_by(Participant) %>% summarise(
  color_order = unique(color_order),
  condition_order = unique(condition_order)
)

Slf = final_orders %>% mutate(Participant = NA) %>% rbind(Slf)

# todo: how often is blue first, how often is red first etc.

Slf %>% group_by(color_order) %>% summarise(
  n()
)

Slf %>% group_by(condition_order) %>% summarise(
  n()
)

# Pick next from Library.


####
# Kiwi: Final Orders, with one preserved
###

Lf_k = Lf %>% filter(Game == "Kiwi")

colors = unique(Lf_k$Color)
orders = unique(Lf_k$Order)
conditions = unique(Lf_k$Condition)

color_combis = gtools::permutations(length(colors),length(colors),colors) %>% as.data.frame()
order_combis = gtools::permutations(length(orders),length(orders),orders) %>% as.data.frame()
condition_combis = gtools::permutations(length(conditions),length(conditions),conditions) %>% as.data.frame()

####
# PAM: Final Orders, with one preserved
###

Lf_p = Lf %>% filter(Game == "PAM")

colors = unique(Lf_p$Color)
orders = unique(Lf_p$Order)
conditions = unique(Lf_p$Condition)

order_combis = gtools::permutations(length(orders),length(orders),orders) %>% as.data.frame()
condition_combis = gtools::permutations(length(conditions),length(conditions),conditions) %>% as.data.frame()

order_combis = order_combis %>% filter(V1 == 1)  # only use order combis where 1 is first, so NO is always first.
condition_combis = condition_combis %>% filter(V1 == "NO")  # only use order combis where 1 is first, so NO is always first.


condition_combis = condition_combis %>%
  rowwise() %>%
  mutate(color_order = paste(across(everything()), collapse = ","),
         count = 1)

Lf %>% group_by(Game, condition_order, color_order,Participant) %>% 
  summarize(n()) %>% group_by(Game, condition_order) %>%
  summarize(n())

Lf %>% group_by(Game, color_order,Participant) %>% 
  summarize(n()) %>% group_by(Game, color_order) %>%
  summarize(n())

Lf = Lf %>% group_by(Participant, Game) %>%
  summarise(
    color_order = paste(unique(Color), collapse=","),
    condition_order = paste(unique(Condition), collapse=",")
  ) %>% right_join(Lf)

Lf %>% group_by(Game, Participant) %>%
  mutate(condition_order = paste(Condition,Order, sep="-")) %>%
  group_by(Game,Participant, condition_order) %>% 
  summarize(n()) %>% group_by(Game,condition_order) %>%
  summarize(n())

condition_occurence = Lf %>% group_by(Game, Participant) %>%
  mutate(condition_occurence = paste(Condition,Order, sep="-")) %>%
  group_by(Game,Participant, condition_occurence) %>% 
  summarize(n()) %>% group_by(Participant, Game,condition_occurence) %>%
  summarize(n = n())

color_occurence = Lf %>% group_by(Game, Participant) %>%
  mutate(color_occurence = paste(Color,Order, sep="-")) %>%
  group_by(Game,Participant, color_occurence) %>% 
  summarize(n()) %>% group_by(Participant, Game,color_occurence) %>%
  summarize(n = n())

condition_occurence %>% group_by(Game, condition_occurence) %>%
  summarize(n())

generate_order <- function(df) {
  df %>% arrange(order)
  
  # determine what stage we are at
  if (nrow(df) == 0) {
    return(df)
  }
  
  stage = df[1,]
  max_order = max(df$order)
  
  # determine how many are left to generate in this order
  more_df = df %>% filter(order == stage$order)
  rest_df = df %>% filter(order > stage$order)
  rest_df = df %>% filter(target != stage$target)
  
  new_order = stage$order
  
  for (i in stage$order+1:max_order) {
    
    rest_df = rest_df %>% filter(order > i)
    rest_df = rest_df %>% filter(target != stage$target)
    
  }
  
  
  if (nrow(more_df) > 1) {
    for (i in 1:length(more_df)) {
      all_df = rbind(all_df, generate_order(more_df[i,] %>% rbind(rest_df), order))
    }
    
    if (nrow(rest_df) == 0)
      generate_order(rest_df)
  }
  
  return(new_order)
}