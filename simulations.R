library(tidyverse)
library(actuar)
loop_probs <- seq(from = 0, to = 0.99, by = 0.01)
num_people <- 500
store_val <- 1000
store_normal_help <- data.frame(
  'probability' = c(numeric(length(loop_probs))),
  'sum_results' = c(numeric(length(loop_probs))),
  'min_results' = c(numeric(length(loop_probs))),
  'max_results' = c(numeric(length(loop_probs)))
)

count <- 0

# normal distribution

df <- data.frame(
  'performance' = c(rnorm(num_people*2, 7.5, 2))
)  %>% 
  dplyr::filter(performance >= 1.0, performance <= 15.0)

ids <- sample(df$performance, size = num_people, replace = F)
df <- df %>%
  filter(performance %in% ids) %>%
  mutate(
    'person' = c(1:length(performance))
  )


for(prob in loop_probs){
  count <- count + 1
  
  
  p_help <- prob
  
  total_helpers <- p_help*num_people
  
  sum_results_vec <- numeric(store_val)
  min_results_vec <- numeric(store_val)
  max_results_vec <- numeric(store_val)
  
  for(k in 1:store_val){
    
    helper_ids <- sample(df$person, size = total_helpers, replace = F)
    helpers <- df %>%
      filter(person %in% helper_ids)
    
    performers <- df %>%
      filter(! person %in% helper_ids)
    
    total_org <- df
    
    # for each person in the help group
    for(i in helpers$person){
      
        # select the helper of focus
        helper <- total_org %>%
        filter(person == i)
      
        # select a random receiver from the total organization
        receiver_id <- sample(total_org$person, size = 1)
        receiver <- total_org %>%
          filter(person == receiver_id)
      
        # is this person in the help group? 0 means no, 1 means yes
        receiver_in_help_group <- sum(helpers$person == receiver_id)
      
        # if yes, then nothing happens. if no, then continue
        if(receiver_in_help_group == 0){
        
          # the receiver gets plus the value of the helper
          new_performance <- receiver$performance + 1.5*helper$performance
        
          # store that new performance result in the contributor group
          performers[performers$person == receiver$person, 'performance'] <- new_performance
        
      }
    }
    sum_results_vec[k] <- sum(performers$performance)
    min_results_vec[k] <- min(performers$performance)
    max_results_vec[k] <- max(performers$performance)
    
    
  }

  
  sum_results <- mean(sum_results_vec)
  min_results <- mean(min_results_vec)
  max_results <- mean(max_results_vec)

  store_normal_help[[count, 'probability']] <- p_help
  store_normal_help[[count, 'sum_results']] <- sum_results
  store_normal_help[[count, 'min_results']] <- min_results
  store_normal_help[[count, 'max_results']] <- max_results


  
}














# pareto distribution
store_pareto_help <- data.frame(
  'probability' = c(numeric(length(loop_probs))),
  'sum_results' = c(numeric(length(loop_probs))),
  'min_results' = c(numeric(length(loop_probs))),
  'max_results' = c(numeric(length(loop_probs)))
)

count <- 0
set.seed(13)
d <- data.frame(
  'performance' = c(rpareto(num_people*5, 1, 1))
)  %>% 
  dplyr::filter(performance >= 1.0)


ids_pareto <- sample(d$performance, size = num_people, replace = F)
d <- d %>%
  filter(performance %in% ids_pareto) %>%
  mutate(
    'person' = c(1:length(performance))
  )


for(prob in loop_probs){
  count <- count + 1
  
  
  p_help <- prob
  
  total_helpers <- p_help*num_people
  
  sum_results_vec <- numeric(store_val)
  min_results_vec <- numeric(store_val)
  max_results_vec <- numeric(store_val)
  
  for(k in 1:store_val){
    
    helper_ids <- sample(d$person, size = total_helpers, replace = F)
    helpers <- d %>%
      filter(person %in% helper_ids)
    
    performers <- d %>%
      filter(! person %in% helper_ids)
    
    total_org <- d
    
    # for each person in the help group
    for(i in helpers$person){
      
      # select the helper of focus
      helper <- total_org %>%
        filter(person == i)
      
      # select a random receiver from the total organization
      receiver_id <- sample(total_org$person, size = 1)
      receiver <- total_org %>%
        filter(person == receiver_id)
      
      # is this person in the help group? 0 means no, 1 means yes
      receiver_in_help_group <- sum(helpers$person == receiver_id)
      
      # if yes, then nothing happens. if no, then continue
      if(receiver_in_help_group == 0){
        
        # the receiver gets plus the value of the helper
        new_performance <- receiver$performance + 1.5*helper$performance
        
        # store that new performance result in the contributor group
        performers[performers$person == receiver$person, 'performance'] <- new_performance
        
      }
    }
    # results
    sum_results_vec[k] <- sum(performers$performance)
    min_results_vec[k] <- min(performers$performance)
    max_results_vec[k] <- max(performers$performance)
    
    
  }
  
  
  sum_results <- mean(sum_results_vec)
  min_results <- mean(min_results_vec)
  max_results <- mean(max_results_vec)
  
  store_pareto_help[[count, 'probability']] <- p_help
  store_pareto_help[[count, 'sum_results']] <- sum_results
  store_pareto_help[[count, 'min_results']] <- min_results
  store_pareto_help[[count, 'max_results']] <- max_results
  
  
  
}





# Tidy



# label
store_normal_help$distribution <- c('Normal')
store_pareto_help$distribution <- c('Power')

# rescale to 0 1
store_normal_help <- store_normal_help %>%
  mutate(sum_scaled = scales::rescale(sum_results, to = c(0, 1))) %>%
  mutate(min_scaled = scales::rescale(min_results, to = c(0, 1))) %>%
  mutate(max_scaled = scales::rescale(max_results, to = c(0, 1)))

store_pareto_help <- store_pareto_help %>%
  mutate(sum_scaled = scales::rescale(sum_results, to = c(0, 1))) %>%
  mutate(min_scaled = scales::rescale(min_results, to = c(0, 1))) %>%
  mutate(max_scaled = scales::rescale(max_results, to = c(0, 1)))

# place into same data frame
df <- dplyr::bind_rows(store_normal_help, store_pareto_help)


                

# h line data frames
hline_sums <- data.frame(
  'distribution' = c('Normal', 'Power'),
  'baseline' = c(store_normal_help$sum_results[1], 
                 store_pareto_help$sum_results[1]),
  'baseline_scaled' = c(store_normal_help$sum_scaled[1],
                        store_pareto_help$sum_scaled[1])
)

hline_maxs <- data.frame(
  'distribution' = c('Normal', 'Power'),
  'baseline' = c(store_normal_help$max_results[1],
                 store_pareto_help$max_results[1]),
  'baseline_scaled' = c(store_normal_help$max_scaled[1],
                        store_pareto_help$max_scaled[1])
)

hline_mins <- data.frame(
  'distribution' = c('Normal', 'Power'),
  'baseline' = c(store_normal_help$min_results[1],
                 store_pareto_help$min_results[1]),
  'baseline_scaled' = c(store_normal_help$min_scaled[1],
                        store_pareto_help$min_scaled[1])
)

# pooled (sum)

ggplot(df, aes(x = probability, y = sum_scaled)) + 
  geom_point() + 
  facet_wrap(~distribution) + 
  geom_hline(data = hline_sums, aes(yintercept = baseline_scaled), linetype = 5) + 
  ylab('Organizational Performance (Sum)') + 
  xlab('Percent of Employees Helping') + 
  theme_classic() + 
  theme(axis.title=element_text(size= 9))

g1 <- ggplot(df, aes(x = probability, y = sum_scaled)) + 
  geom_point() + 
  facet_wrap(~distribution) + 
  geom_hline(data = hline_sums, aes(yintercept = baseline_scaled), linetype = 5) + 
  ylab('Organizational Performance (Sum)') + 
  xlab('Percent of Employees Helping') + 
  theme_classic() + 
  theme(axis.title=element_text(size= 9))


# max

ggplot(df, aes(x = probability, y = max_scaled)) + 
  geom_point() + 
  facet_wrap(~distribution) + 
  geom_hline(data = hline_maxs, aes(yintercept = baseline_scaled), linetype = 5) + 
  ylab('Organizational Performance (Max)') + 
  xlab('Percent of Employees Helping') + 
  theme_classic() + 
  theme(axis.title=element_text(size= 9))

g2 <- ggplot(df, aes(x = probability, y = max_scaled)) + 
  geom_point() + 
  facet_wrap(~distribution) + 
  geom_hline(data = hline_maxs, aes(yintercept = baseline_scaled), linetype = 5) + 
  ylab('Organizational Performance (Max)') + 
  xlab('Percent of Employees Helping') + 
  theme_classic() + 
  theme(axis.title=element_text(size= 9))


# min

ggplot(df, aes(x = probability, y = min_scaled)) + 
  geom_point() + 
  facet_wrap(~distribution) + 
  geom_hline(data = hline_mins, aes(yintercept = baseline_scaled), linetype = 5) + 
  ylab('Organizational Performance (Min)') + 
  xlab('Percent of Employees Helping') + 
  theme_classic() + 
  theme(axis.title=element_text(size= 9))

g3 <- ggplot(df, aes(x = probability, y = min_scaled)) + 
  geom_point() + 
  facet_wrap(~distribution) + 
  geom_hline(data = hline_mins, aes(yintercept = baseline_scaled), linetype = 5) + 
  ylab('Organizational Performance (Min)') + 
  xlab('Percent of Employees Helping') + 
  theme_classic() + 
  theme(axis.title=element_text(size= 9))



library(ggpubr)
ggarrange(g1, g2, g3,
          labels = c('A', 'B', 'C'),
          ncol = 1)

