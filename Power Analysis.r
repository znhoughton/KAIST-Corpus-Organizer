###version 1: #nsim = 1000, n = 150, b1 = -20, b2 = 0

library(tidyverse)
library(pwr)
library(brms)
library(lme4)
library(naniar)

options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1
surface_and_base_freqs = read_csv("Surface and Base Frequencies of Korean Passives v3.csv", locale = locale(encoding = "euc-kr"))
test = surface_and_base_freqs %>%
  group_by(Subnumber, `Base Frequency`, `Surface Frequency of the Exact Word`) %>%
  filter(Subnumber == 1)
test = test %>%
  mutate("Simulated RT" = rnorm(n(), mean = 600, sd = 150))
test = test %>%
  rename("simulatedRT" = "Simulated RT")
test = test %>%
  rename("BaseFreq" = "Base Frequency")
  
b0 = 800 #intercept taken from the above nc dataset
b1 = -20 #base coefficient, nc database estimate = -47.05 -- underestimated on purpose to be safe
b2 = 0 #surface coefficient
b3 = 0.00 #base:surface intercept


n = 100



d = tibble(
  subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
  mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
  mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
  mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
  mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
  mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
  mutate(b3j = 0) %>% # a specific value for each subject
  mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
  mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
  replace_with_na(replace = list(surface = -Inf)) %>%
  mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
  mutate(base = base - mean(base, na.rm = TRUE)) %>% 
  mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
  mutate(word = rep(test$`Specific Word`, times = n)) %>%
  #get rid of -Inf that result from trying to log(0) 
  mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
  
d = d[complete.cases(d), ]

sim_d = function(seed, n) {
  
  set.seed(seed)
  
  b0 = 800
  b1 = -20
  b2 = 0 
  b3 = 0
  
  d = tibble(
    subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
    mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
    mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
    mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
    mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
    mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
    mutate(b3j = 0) %>% # a specific value for each subject
    mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
    mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
    replace_with_na(replace = list(surface = -Inf)) %>%
    mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
    mutate(base = base - mean(base, na.rm = TRUE)) %>% 
    mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
    mutate(word = rep(test$`Specific Word`, times = n)) %>%
    #get rid of -Inf that result from trying to log(0) 
    mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
 
d = d[complete.cases(d), ]
  
update(fit,
         newdata = d, 
         seed = seed,
		 chains = 7, cores = 7, iter = 8000) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter")
}
 
fit = 
  brm(data = d, cores = 7, chains = 7, iter = 8000,
      rt ~ base*surface + (1+base*surface|subject) + (1|item),
      prior = c(set_prior("student_t(3, 795.3, 218.2)", class = "Intercept"),
                set_prior("student_t(3, 0, 218.2)", class = "b")),
      seed = 1)
	  
n_sim = 1000

s3 = 
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d, n = 100)) %>% 
  unnest(b1)
  
s3 %>% 
	filter(parameter == "base") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))
  
 s3 %>% 
	filter(parameter == "surface") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))

s_test = s3 %>%
	filter(parameter == "surface" | parameter == "base") %>%
	mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>%
	select(seed, parameter, check)

s_test2 = s_test %>% 
	pivot_wider(names_from = parameter, values_from = check)

s_test2 = s_test2 %>%
	group_by(seed) %>%
	mutate(base_but_not_surface = if_else(base == 1 & surface == 0, 1, 0)) %>%
	mutate(surface_but_not_base = if_else(base == 0 & surface == 1, 1, 0)) %>%
	mutate(surface_and_base = if_else(base == 1 & surface == 1, 1, 0)) %>%
	mutate(not_base_not_surface = if_else(base == 0 & surface == 0, 1, 0))

mean(s_test2$base_but_not_surface)
mean(s_test2$surface_but_not_base)
mean(s_test2$surface_and_base)
mean(s_test2$not_base_not_surface)

###version 2: #nsim = 1000, n = 150, b1 = -10, b2 = 0

library(tidyverse)
library(pwr)
library(brms)
library(lme4)
library(naniar)

options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1

surface_and_base_freqs = read_csv("Surface and Base Frequencies of Korean Passives v3.csv", locale = locale(encoding = "euc-kr"))

test = surface_and_base_freqs %>%
  group_by(Subnumber, `Base Frequency`, `Surface Frequency of the Exact Word`) %>%
  filter(Subnumber == 1)

test = test %>%
  mutate("Simulated RT" = rnorm(n(), mean = 600, sd = 150))

test = test %>%
  rename("simulatedRT" = "Simulated RT")

test = test %>%
  rename("BaseFreq" = "Base Frequency")
  
b0 = 800 #intercept taken from the above nc dataset
b1 = -10 #base coefficient, nc database estimate = -47.05 -- underestimated on purpose to be safe
b2 = 0 #surface coefficient
b3 = 0.00 #base:surface intercept


n = 100



d = tibble(
  subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
  mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
  mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
  mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
  mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
  mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
  mutate(b3j = 0) %>% # a specific value for each subject
  mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
  mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
  replace_with_na(replace = list(surface = -Inf)) %>%
  mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
  mutate(base = base - mean(base, na.rm = TRUE)) %>% 
  mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
  mutate(word = rep(test$`Specific Word`, times = n)) %>%
  #get rid of -Inf that result from trying to log(0) 
  mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni) 

d = d[complete.cases(d), ]

sim_d = function(seed, n) {
  
  set.seed(seed)
  
  b0 = 800
  b1 = -10
  b2 = 0 
  b3 = 0
  
  d = tibble(
    subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
    mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
    mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
    mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
    mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
    mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
    mutate(b3j = 0) %>% # a specific value for each subject
    mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
    mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
    replace_with_na(replace = list(surface = -Inf)) %>%
    mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
    mutate(base = base - mean(base, na.rm = TRUE)) %>% 
    mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
    mutate(word = rep(test$`Specific Word`, times = n)) %>%
    #get rid of -Inf that result from trying to log(0) 
    mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
	
d = d[complete.cases(d), ]
  
update(fit,
         newdata = d, 
         seed = seed,
		 chains = 7, cores = 7, iter = 8000) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter")
}
 
fit = 
  brm(data = d, cores = 7, chains = 7, iter = 8000,
      rt ~ base*surface + (1+base*surface|subject) + (1|item),
      prior = c(set_prior("student_t(3, 795.3, 218.2)", class = "Intercept"),
                set_prior("student_t(3, 0, 218.2)", class = "b")),
      seed = 1)
	  
n_sim = 1000

s3 = 
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d, n = 100)) %>% 
  unnest(b1)
  
s3 %>% 
	filter(parameter == "base") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))
  
 s3 %>% 
	filter(parameter == "surface") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))

s_test = s3 %>%
	filter(parameter == "surface" | parameter = "base") %>%
	mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>%
	select(seed, parameter, check)

s_test2 = s_test %>% 
	pivot_wider(names_from = parameter, values_from = check)

s_test2 = s_test2 %>%
	group_by(seed) %>%
	mutate(base_but_not_surface = if_else(base == 1 & surface == 0, 1, 0)) %>%
	mutate(surface_but_not_base = if_else(base == 0 & surface == 1, 1, 0)) %>%
	mutate(surface_and_base = if_else(base == 1 & surface == 1, 1, 0)) %>%
	mutate(not_base_not_surface = if_else(base == 0 & surface == 0, 1, 0))

###version 3: #nsim = 1000, n = 100, b1 = -30, b2 = 0

library(tidyverse)
library(pwr)
library(brms)
library(lme4)
library(naniar)

options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1

surface_and_base_freqs = read_csv("Surface and Base Frequencies of Korean Passives v3.csv", locale = locale(encoding = "euc-kr"))

test = surface_and_base_freqs %>%
  group_by(Subnumber, `Base Frequency`, `Surface Frequency of the Exact Word`) %>%
  filter(Subnumber == 1)

test = test %>%
  mutate("Simulated RT" = rnorm(n(), mean = 600, sd = 150))

test = test %>%
  rename("simulatedRT" = "Simulated RT")

test = test %>%
  rename("BaseFreq" = "Base Frequency")
  
b0 = 800 #intercept taken from the above nc dataset
b1 = -30 #base coefficient, nc database estimate = -47.05 -- underestimated on purpose to be safe
b2 = 0 #surface coefficient
b3 = 0.00 #base:surface intercept


n = 100



d = tibble(
  subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
  mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
  mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
  mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
  mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
  mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
  mutate(b3j = 0) %>% # a specific value for each subject
  mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
  mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
  replace_with_na(replace = list(surface = -Inf)) %>%
  mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
  mutate(base = base - mean(base, na.rm = TRUE)) %>% 
  mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
  mutate(word = rep(test$`Specific Word`, times = n)) %>%
  #get rid of -Inf that result from trying to log(0) 
  mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)

d = d[complete.cases(d), ]
 
sim_d = function(seed, n) {
  
  set.seed(seed)
  
  b0 = 800
  b1 = -30
  b2 = 0 
  b3 = 0
  
  d = tibble(
    subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
    mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
    mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
    mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
    mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
    mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
    mutate(b3j = 0) %>% # a specific value for each subject
    mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
    mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
    replace_with_na(replace = list(surface = -Inf)) %>%
    mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
    mutate(base = base - mean(base, na.rm = TRUE)) %>% 
    mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
    mutate(word = rep(test$`Specific Word`, times = n)) %>%
    #get rid of -Inf that result from trying to log(0) 
    mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
	
d = d[complete.cases(d), ]

  update(fit,
         newdata = d, 
         seed = seed,
		 chains = 7, cores = 7, iter = 8000) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter")
}

fit = 
  brm(data = d, cores = 7, chains = 7, iter = 8000,
      rt ~ base*surface + (1+base*surface|subject) + (1|item),
      prior = c(set_prior("student_t(3, 795.3, 218.2)", class = "Intercept"),
                set_prior("student_t(3, 0, 218.2)", class = "b")),
      seed = 1)
 
n_sim = 1000



s3 = 
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d, n = 100)) %>% 
  unnest(b1)
  
s3 %>% 
	filter(parameter == "base") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))
  
 s3 %>% 
	filter(parameter == "surface") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))

s_test = s3 %>%
	filter(parameter == "surface" | parameter = "base") %>%
	mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>%
	select(seed, parameter, check)

s_test2 = s_test %>% 
	pivot_wider(names_from = parameter, values_from = check)

s_test2 = s_test2 %>%
	group_by(seed) %>%
	mutate(base_but_not_surface = if_else(base == 1 & surface == 0, 1, 0)) %>%
	mutate(surface_but_not_base = if_else(base == 0 & surface == 1, 1, 0)) %>%
	mutate(surface_and_base = if_else(base == 1 & surface == 1, 1, 0)) %>%
	mutate(not_base_not_surface = if_else(base == 0 & surface == 0, 1, 0)) 
  summarise(power = mean(check))

###version 4: #nsim = 1000, n = 100, b1 = -10, b2 = -10

library(tidyverse)
library(pwr)
library(brms)
library(lme4)
library(naniar)

options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1

surface_and_base_freqs = read_csv("Surface and Base Frequencies of Korean Passives v3.csv", locale = locale(encoding = "euc-kr"))

test = surface_and_base_freqs %>%
  group_by(Subnumber, `Base Frequency`, `Surface Frequency of the Exact Word`) %>%
  filter(Subnumber == 1)

test = test %>%
  mutate("Simulated RT" = rnorm(n(), mean = 600, sd = 150))

test = test %>%
  rename("simulatedRT" = "Simulated RT")

test = test %>%
  rename("BaseFreq" = "Base Frequency")
  
b0 = 800 #intercept taken from the above nc dataset
b1 = -10 #base coefficient, nc database estimate = -47.05 -- underestimated on purpose to be safe
b2 = -10 #surface coefficient
b3 = 0.00 #base:surface intercept


n = 100



d = tibble(
  subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
  mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
  mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
  mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
  mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
  mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
  mutate(b3j = 0) %>% # a specific value for each subject
  mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
  mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
  replace_with_na(replace = list(surface = -Inf)) %>%
  mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
  mutate(base = base - mean(base, na.rm = TRUE)) %>% 
  mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
  mutate(word = rep(test$`Specific Word`, times = n)) %>%
  #get rid of -Inf that result from trying to log(0) 
  mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni) 
  
d = d[complete.cases(d), ]

sim_d = function(seed, n) {
  
  set.seed(seed)
  
  b0 = 800
  b1 = -10
  b2 = -10 
  b3 = 0
  
  d = tibble(
    subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
    mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
    mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
    mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
    mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
    mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
    mutate(b3j = 0) %>% # a specific value for each subject
    mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
    mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
    replace_with_na(replace = list(surface = -Inf)) %>%
    mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
    mutate(base = base - mean(base, na.rm = TRUE)) %>% 
    mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
    mutate(word = rep(test$`Specific Word`, times = n)) %>%
    #get rid of -Inf that result from trying to log(0) 
    mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
  
d = d[complete.cases(d), ]
  
  update(fit,
         newdata = d, 
         seed = seed,
		 chains = 7, cores = 7, iter = 8000) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter")
}
 
fit = 
  brm(data = d, cores = 7, chains = 7, iter = 8000,
      rt ~ base*surface + (1+base*surface|subject) + (1|item),
      prior = c(set_prior("student_t(3, 795.3, 218.2)", class = "Intercept"),
                set_prior("student_t(3, 0, 218.2)", class = "b")),
      seed = 1)
	  
n_sim = 1000

s3 = 
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d, n = 100)) %>% 
  unnest(b1)
  
s3 %>% 
	filter(parameter == "base") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))
  
 s3 %>% 
	filter(parameter == "surface") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))

s_test = s3 %>%
	filter(parameter == "surface" | parameter = "base") %>%
	mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>%
	select(seed, parameter, check)

s_test2 = s_test %>% 
	pivot_wider(names_from = parameter, values_from = check)

s_test2 = s_test2 %>%
	group_by(seed) %>%
	mutate(base_but_not_surface = if_else(base == 1 & surface == 0, 1, 0)) %>%
	mutate(surface_but_not_base = if_else(base == 0 & surface == 1, 1, 0)) %>%
	mutate(surface_and_base = if_else(base == 1 & surface == 1, 1, 0)) %>%
	mutate(not_base_not_surface = if_else(base == 0 & surface == 0, 1, 0))
	
	
###version 5: #nsim = 1000, n = 100, b1 = -10, b2 = -20 ###with 1000 simulations, surface comes out significant .708, base comes out significant only 0.168

library(tidyverse)
library(pwr)
library(brms)
library(lme4)
library(naniar)

options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1

surface_and_base_freqs = read_csv("Surface and Base Frequencies of Korean Passives v3.csv", locale = locale(encoding = "euc-kr"))

test = surface_and_base_freqs %>%
  group_by(Subnumber, `Base Frequency`, `Surface Frequency of the Exact Word`) %>%
  filter(Subnumber == 1)

test = test %>%
  mutate("Simulated RT" = rnorm(n(), mean = 600, sd = 150))

test = test %>%
  rename("simulatedRT" = "Simulated RT")

test = test %>%
  rename("BaseFreq" = "Base Frequency")
  
b0 = 800 #intercept taken from the above nc dataset
b1 = -10 #base coefficient, nc database estimate = -47.05 -- underestimated on purpose to be safe
b2 = -20 #surface coefficient
b3 = 0.00 #base:surface intercept


n = 100




d = tibble(
  subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
  mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
  mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
  mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
  mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
  mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
  mutate(b3j = 0) %>% # a specific value for each subject
  mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
  mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
  replace_with_na(replace = list(surface = -Inf)) %>%
  mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
  mutate(base = base - mean(base, na.rm = TRUE)) %>% 
  mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
  mutate(word = rep(test$`Specific Word`, times = n)) %>%
  #get rid of -Inf that result from trying to log(0) 
  mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
  
d = d[complete.cases(d), ]

sim_d = function(seed, n) {
  
  set.seed(seed)
  
  b0 = 800
  b1 = -10
  b2 = -20 
  b3 = 0
  
  d = tibble(
    subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
    mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
    mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
    mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
    mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
    mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
    mutate(b3j = 0) %>% # a specific value for each subject
    mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
    mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
    replace_with_na(replace = list(surface = -Inf)) %>%
    mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
    mutate(base = base - mean(base, na.rm = TRUE)) %>% 
    mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
    mutate(word = rep(test$`Specific Word`, times = n)) %>%
    #get rid of -Inf that result from trying to log(0) 
    mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
	
d = d[complete.cases(d), ]
  
update(fit,
         newdata = d, 
         seed = seed,
		 chains = 7, cores = 7, iter = 8000) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter")
}
 
fit = 
  brm(data = d, cores = 7, chains = 7, iter = 8000,
      rt ~ base*surface + (1+base*surface|subject) + (1|item),
      prior = c(set_prior("student_t(3, 795.3, 218.2)", class = "Intercept"),
                set_prior("student_t(3, 0, 218.2)", class = "b")),
      seed = 1)
	  
n_sim = 1000

s3 = 
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d, n = 100)) %>% 
  unnest(b1)
  
s3 %>% 
	filter(parameter == "base") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))
  
 s3 %>% 
	filter(parameter == "surface") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))

s_test = s3 %>%
	filter(parameter == "surface" | parameter = "base") %>%
	mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>%
	select(seed, parameter, check)

s_test2 = s_test %>% 
	pivot_wider(names_from = parameter, values_from = check)

s_test2 = s_test2 %>%
	group_by(seed) %>%
	mutate(base_but_not_surface = if_else(base == 1 & surface == 0, 1, 0)) %>%
	mutate(surface_but_not_base = if_else(base == 0 & surface == 1, 1, 0)) %>%
	mutate(surface_and_base = if_else(base == 1 & surface == 1, 1, 0)) %>%
	mutate(not_base_not_surface = if_else(base == 0 & surface == 0, 1, 0))

###version 6: #nsim = 1000, n = 100, b1 = -10, b2 = -30

library(tidyverse)
library(pwr)
library(brms)
library(lme4)
library(naniar)

options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1

surface_and_base_freqs = read_csv("Surface and Base Frequencies of Korean Passives v3.csv", locale = locale(encoding = "euc-kr"))

test = surface_and_base_freqs %>%
  group_by(Subnumber, `Base Frequency`, `Surface Frequency of the Exact Word`) %>%
  filter(Subnumber == 1)

test = test %>%
  mutate("Simulated RT" = rnorm(n(), mean = 600, sd = 150))

test = test %>%
  rename("simulatedRT" = "Simulated RT")

test = test %>%
  rename("BaseFreq" = "Base Frequency")
  
b0 = 800 #intercept taken from the above nc dataset
b1 = -10 #base coefficient, nc database estimate = -47.05 -- underestimated on purpose to be safe
b2 = -30 #surface coefficient
b3 = 0.00 #base:surface intercept


n = 100



d = tibble(
  subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
  mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
  mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
  mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
  mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
  mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
  mutate(b3j = 0) %>% # a specific value for each subject
  mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
  mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
  replace_with_na(replace = list(surface = -Inf)) %>%
  mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
  mutate(base = base - mean(base, na.rm = TRUE)) %>% 
  mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
  mutate(word = rep(test$`Specific Word`, times = n)) %>%
  #get rid of -Inf that result from trying to log(0) 
  mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
  
d = d[complete.cases(d), ]

sim_d = function(seed, n) {
  
  set.seed(seed)
  
  b0 = 800
  b1 = -10
  b2 = -30 
  b3 = 0
  
  d = tibble(
    subject = rep(1:n, each = length(test$Number))) %>% #create n subjects with 54 items each
    mutate(item = rep(1:length(test$Number),n)) %>% #create items, 54 items * n subjects
    mutate(b0j = rep(rnorm(n=n, mean = 0, sd = 207.49), each = length(test$Number))) %>% #a specific value for each subject, value taken from the group level effects of the nc model1
    mutate(b0k = rep(rnorm(n=length(test$Number), mean = 0, sd = 53.15), times = n)) %>% #a specific value for each item, value taken from the group level effects of the nc model1
    mutate(b1j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b1)), each = length(test$Number))) %>% #a specific value for each subject
    mutate(b2j = rep(rnorm(n=n, mean = 0, sd = abs(0.8 * b2)), each = length(test$Number))) %>% # a specific value for each subject
    mutate(b3j = 0) %>% # a specific value for each subject
    mutate(surface = rep(log(test$`Surface Frequency of the Exact Word`), times=n)) %>% #import surface frequencies
    mutate(base = rep(log(test$BaseFreq), times = n)) %>% #import base frequencies
    replace_with_na(replace = list(surface = -Inf)) %>%
    mutate(surface = surface - mean(surface, na.rm = TRUE)) %>%
    mutate(base = base - mean(base, na.rm = TRUE)) %>% 
    mutate(epsiloni = rep(rnorm(n=length(test$Number)*n, mean = 0, sd = 100))) %>%#noise
    mutate(word = rep(test$`Specific Word`, times = n)) %>%
    #get rid of -Inf that result from trying to log(0) 
    mutate(rt = (b0 + b0j + b0k) + (b1 + b1j) * base + (b2 + b2j) * surface + (b3 + b3j) * (base * surface) + epsiloni)
	
d = d[complete.cases(d), ]
  
update(fit,
         newdata = d, 
         seed = seed,
		 chains = 7, cores = 7, iter = 8000) %>% 
    fixef() %>% 
    data.frame() %>% 
    rownames_to_column("parameter")
}
 
fit = 
  brm(data = d, cores = 7, chains = 7, iter = 8000,
      rt ~ base*surface + (1+base*surface|subject) + (1|item),
      prior = c(set_prior("student_t(3, 795.3, 218.2)", class = "Intercept"),
                set_prior("student_t(3, 0, 218.2)", class = "b")),
      seed = 1)
	  
n_sim = 1000

s3 = 
  tibble(seed = 1:n_sim) %>% 
  mutate(b1 = map(seed, sim_d, n = 100)) %>% 
  unnest(b1)
  
s3 %>% 
	filter(parameter == "base") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))
  
 s3 %>% 
	filter(parameter == "surface") %>%
  mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>% 
  summarise(power = mean(check))

s_test = s3 %>%
	filter(parameter == "surface" | parameter = "base") %>%
	mutate(check = ifelse(Q97.5 < 0, 1, 0)) %>%
	select(seed, parameter, check)

s_test2 = s_test %>% 
	pivot_wider(names_from = parameter, values_from = check)

s_test2 = s_test2 %>%
	group_by(seed) %>%
	mutate(base_but_not_surface = if_else(base == 1 & surface == 0, 1, 0)) %>%
	mutate(surface_but_not_base = if_else(base == 0 & surface == 1, 1, 0)) %>%
	mutate(surface_and_base = if_else(base == 1 & surface == 1, 1, 0)) %>%
	mutate(not_base_not_surface = if_else(base == 0 & surface == 0, 1, 0))