##############################################################
# justifing use of ANOVA with current data.
# this code will load the summary data set of choice. It will
# then, over n permutations, sample the data under assumptions of
# no differences between groups. It will then calculate the F statistic 
# for each permutation. This allows identification of the 
# type 1 error rate under the null hypothesis.
#############################################################
rm(list=ls())
set.seed(42) # for reproducibility
# load packages and add some general settings
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(afex)

data_path = 'data-wrangled/' # for all data derivs
fig_path = 'figs/' # for figures
res_path = 'res/' # for inferential results
exp_strs <- c('lt','ts')
nreps <- 1000 # number of permutations to run
j_wdth <- 10 # this is for the task jumps fig
j_hgt <- j_wdth*(6/10)

#############################################################
# functions we will need for running the permutations
# 1. function to sample the data, and add it back to the 
# dataframe
sim_dat <- function(dat, exp_str, dv){
  
  sim_dat <- dat %>%
    filter(exp == exp_str) %>%
    mutate(!!dv := sample(.data[[dv]], size = n(), replace = FALSE)) 
  return(sim_dat)
}

# 2. function to calculate and return the F statistic for each permutation
run_F_stat <- function(dat, dv, data_transform = 'none', withinf, betweenf){
  # run the ANOVA and return the F statistic
  if (data_transform == 'none'){
    aov_res <- aov_ez(id = 'sub', 
                      dv = dv, 
                      data = dat, 
                      between = betweenf,
                      within = withinf)
  } else {
    aov_res <- aov_ez(id = 'sub', 
                      dv = dv, 
                      data = dat, 
                      between = betweenf,
                      within = withinf,
                      transformation = data_transform)
  }
  out <- tibble(F_stat = aov_res$anova_table$F,
                p_val = aov_res$anova_table$`Pr(>F)`,
                fx = rownames(aov_res$anova_table))
  return(out)
}

# 3. now put them together into one wrapper function
one_sim <- function(dat, exp_str, dv, data_transform = 'none', withinf='switch', betweenf='train_type'){
  sim_df <- sim_dat(dat, exp_str = exp_str, dv = dv)
  F_stats <- run_F_stat(sim_df, dv = dv, data_transform = data_transform, withinf = withinf, betweenf = betweenf)
  return(F_stats)
}

#############################################################
# read in the data
# first let's do the most negatively skewed disttribution, 
# which is accuracy from the learning transfer phase (-2.15)
lt_acc <- read.csv(paste(data_path, 'exp_lt_avg.csv', sep='')) %>% 
  filter(ses==3) %>%
  select(sub, transfer, full_transfer_first, train_type, accuracy) %>% 
  mutate(train_type=recode(train_type, `1` = 'stable', `2` = 'variable'),
         transfer=recode(transfer, `1` = 'identity', `2` = 'mixed'))
lt_acc$sub <- as.factor(lt_acc$sub)
lt_acc$train_type <- as.factor(lt_acc$train_type)
levels(lt_acc$train_type) <- c('stable','variable')
lt_acc$transfer <- as.factor(lt_acc$transfer)
lt_acc$exp <- 'lt'
sim_res <- do.call(rbind, 
                   replicate(nreps, one_sim(lt_acc, exp_str = 'lt', dv = 'accuracy', data_transform = 'sqrt', withinf='transfer', betweenf='train_type'), simplify=FALSE))

# save the output of the simulation results
write_csv(sim_res, paste0(data_path, 'type1sim_res_acc.csv'))

# report the % of p-values < 0.05 for each effect
type1 <- sim_res %>% group_by(fx) %>%
  summarise(type1_error = mean(p_val < 0.05)) %>%
  mutate(type1_error = round(type1_error, 2))

write_csv(type1, paste0(res_path, 'type1_error_acc.csv'))

# plot a histogram of p-values for each effect, with a vertical line on
# each plot where p=0.05 is
sim_res %>% ggplot(aes(x = p_val)) +
  geom_histogram(bins = 30, fill = '#d8b365', color = 'darkgrey') +
  geom_vline(xintercept = 0.05, color = 'black', lwd=1) +
  facet_wrap(~fx) +
  theme_bw() +
  labs(x = 'p-value', y = 'Count', title = 'Distribution of p-values under the null hypothesis') +
  theme(strip.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))
ggsave(paste0(fig_path, 'type1sim_hist_acc.pdf'), width = j_wdth, height = j_hgt, units = 'in', dpi = 300)

#############################################################################
# now we do the most +ve skewed distribution, which was general errors from the task switching phase
ts <- read.csv(paste(res_path, 'ts_test_dat.csv', sep=""))
ts$exp <- 'ts'
sim_res <- do.call(rbind, 
                   replicate(nreps, one_sim(ts, exp_str = 'ts', 
                                            dv = 'general_errors', 
                                            data_transform = 'none', 
                                            withinf='switch', 
                                            betweenf='train_type'), simplify=FALSE))

# save the output of the simulation results
write_csv(sim_res, paste0(data_path, 'type1sim_res_gen.csv'))

# report the % of p-values < 0.05 for each effect
type1 <- sim_res %>% group_by(fx) %>%
  summarise(type1_error = mean(p_val < 0.05)) %>%
  mutate(type1_error = round(type1_error, 2))

write_csv(type1, paste0(res_path, 'type1_error_gen.csv'))
#############################################################################


