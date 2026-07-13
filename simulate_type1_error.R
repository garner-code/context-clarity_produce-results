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
run_F_stat <- function(dat, dv, data_transform = 'none'){
  # run the ANOVA and return the F statistic
  if (data_transform == 'none'){
    aov_res <- aov_ez(id = 'sub', 
                      dv = dv, 
                      data = dat, 
                      between = 'train_type',
                      within = 'switch')
  } else {
    aov_res <- aov_ez(id = 'sub', 
                      dv = dv, 
                      data = dat, 
                      between = 'train_type',
                      within = 'switch',
                      transformation = data_transform)
  }
  out <- tibble(F_stat = aov_res$anova_table$F,
                p_val = aov_res$anova_table$`Pr(>F)`,
                fx = rownames(aov_res$anova_table))
  return(out)
}

# 3. now put them together into one wrapper function
one_sim <- function(dat, exp_str, dv, data_transform = 'none'){
  sim_df <- sim_dat(dat, exp_str = exp_str, dv = dv)
  F_stats <- run_F_stat(sim_df, dv = dv, data_transform = data_transform)
  return(F_stats)
}

#############################################################
# read in the data
dat <- read_csv(paste0(data_path, 'jumps.csv'))
sim_res <- do.call(rbind, 
                   replicate(nreps, one_sim(dat, exp_str = 'lt', dv = 'jumps', data_transform = 'none'), simplify=FALSE))

# save the output of the simulation results
write_csv(sim_res, paste0(data_path, 'type1sim_res.csv'))

# report the % of p-values < 0.05 for each effect
type1 <- sim_res %>% group_by(fx) %>%
            summarise(type1_error = mean(p_val < 0.05)) %>%
            mutate(type1_error = round(type1_error, 2))

write_csv(type1, paste0(res_path, 'type1_error.csv'))

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
ggsave(paste0(fig_path, 'type1sim_hist.pdf'), width = j_wdth, height = j_hgt, units = 'in', dpi = 300)

