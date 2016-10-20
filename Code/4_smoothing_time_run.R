
# FOR MALES IN ALABAMA - PROOF OF CONCEPT 

  # Preamble
setwd("~/Documents/BlackWhiteMortalityGap/Code")
source('4_smoothing_time_src.R') 
source('4_smoothing_time_fun.R') 
 
  # Data for all COD 

COD_list = c('Injuries', 'Cardiovascular', 'Cancers', 'Communicable', 
             'Non-communicable', 'All other causes')

ds_black = ds_white = ds_jags_bw = list()
N_COD = length(COD_list) 

for(i in 1:N_COD) { 
  current_cod = COD_list[i] 
  ds_black = subset_data(race='Black', sex='Male', cod=current_cod) 
  ds_white = subset_data(race='White', sex='Male', cod=current_cod) 
  ds_jags_bw[[i]] = combine_race_data(ds_black, ds_white) 
}

# Run model for each COD 
jags_bw = list() 

for(i in 1:N_COD) { 
  jags_bw[[i]] = bwmort_smooth_time(ds_jags_bw[[i]])  
}


# Extract and clean results so they can be used in CR LE functions 

results_cod_b = list() 
for(i in 1:N_COD) {
  smoothed_deaths_b = jagsresults(x=jags_bw[[i]], params=c('mu_b'))[, '50%']
  
  r = data.frame(year=ds_jags_bw[[i]]$year_b) 
  r$deaths_b = ds_jags_bw[[i]]$deaths_b
  r$smoothed_deaths_b = smoothed_deaths_b
  r$age_bins = ds_jags_bw[[i]]$age.bin_b
  r$pop = exp(ds_jags_bw[[i]]$lnpop_b)
  results_cod_b[[i]] = r[order(r$age_bins, r$year), ]
}

temp = smoothed_allcod_deaths_b = c()
nrows = length(results_cod_b[[1]]$smoothed_deaths_b)


  for(j in 1:nrows) { 
    for(i in 1:N_COD) { 
      temp[i] = results_cod_b[[i]]$smoothed_deaths_b[j]   
    }
    smoothed_allcod_deaths_b[j] = sum(temp) 
  }


ds_allcod_b = data.frame(year=results_cod_b[[1]]$year, 
                         age_bin=results_cod_b[[1]]$age_bins, 
                         pop = results_cod_b[[1]]$pop, 
                         smoothed_allcod_deaths_b) 

ds_allcod_b_1969 = ds_allcod_b[ds_allcod_b$year=='1', ]
ds_allcod_b_2013 = ds_allcod_b[ds_allcod_b$year=='45', ]

k = length(unique(ds_allcod_b_1969$age_bin))
ds_allcod_b_1969$num_ages = c(1, 4, rep(5, k-2))
ds_allcod_b_2013$num_ages = c(1, 4, rep(5, k-2))


lt1969 = life.table(data=ds_allcod_b_1969, num.ages.in.group='num_ages', 
                death.counts='smoothed_allcod_deaths_b', population.counts='pop')

lt2013 = life.table(data=ds_allcod_b_2013, num.ages.in.group='num_ages', 
                    death.counts='smoothed_allcod_deaths_b', population.counts='pop')



d = le_age_decomp(lt1969, name.lt1='y1969', lt2013, 'y2013', age.groups = 'age_bin') 













