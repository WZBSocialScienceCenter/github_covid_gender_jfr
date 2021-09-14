#
# Create weekly aggregated GitHub data by country and gender.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#

source('dataprep.R')

#### data loading / preparation ####

LAST_CONTRIBS_DATE = '2020-08-02'

# minimum number of contribs per user before and during pandemic respectively; set to NULL to disable filter
#MIN_N_CONTRIBS_BEFORE_DURING_COVID <- 10
MIN_N_CONTRIBS_BEFORE_DURING_COVID <- NULL
# minimum proportion of contributions made per user during pre-pandemic times; set to NULL to disable filter
#MIN_PROP_BUSICONTRIB <- 0.75
MIN_PROP_BUSICONTRIB <- NULL

userdata <- load_userdata(last_contribs_date = LAST_CONTRIBS_DATE, created_before = '2020-01-01')

print('data comes from collection dates:')
print(unique(userdata$collection_date) %>% sort())

if (!is.null(MIN_N_CONTRIBS_BEFORE_DURING_COVID)) {
  usercontribtimes <- load_prepared_contribtimes()    # daily_extreme_quantile = 0.99
  filtereduserids <- group_by(usercontribtimes, id, covid) %>%
    summarise(n_contribs = n()) %>%
    pivot_wider(names_prefix = 'covid', names_from = covid, values_from = n_contribs, values_fill = 0L) %>%
    filter(covid0 >= MIN_N_CONTRIBS_BEFORE_DURING_COVID, covid1 >= MIN_N_CONTRIBS_BEFORE_DURING_COVID) %>%
    pull(id)
  length(filtereduserids) / length(unique(usercontribtimes$id))
  userdata <- filter(userdata, id %in% filtereduserids)
  rm(usercontribtimes)
}

# table that translates ISO country codes to country names
countrycodes2names <- get_countrycodes2names(userdata)
write.csv(countrycodes2names, file.path('aggregdata', 'countrycodes2names.csv'), row.names = FALSE)

num_accounts_cntry_gender <- group_by(userdata, country_code, gender, .drop = FALSE) %>% count()
num_accounts_cntry_gender
write.csv(num_accounts_cntry_gender, file.path('aggregdata', 'num_accounts_cntry_gender.csv'), row.names = FALSE)

contribs_weekly <- load_contribs('weekly')
contribs_weekly <- prepare_contribs(userdata, contribs_weekly, latest_date = LAST_CONTRIBS_DATE)

result <- prepare_userdata(userdata, contribs_weekly, countrycodes2names)
userdata <- result[[1]]
contribs_weekly <- result[[2]]
rm(result)

result <- filter_countries(userdata, contribs_weekly, min_n_users = 20)
userdata <- result[[1]]
contribs_weekly <- result[[2]]
counts_countries <- result[[3]]
rm(result)


#### create weekly aggregated data by country and gender ####

contribs_weekly_gender <- aggregate_per_country_and_gender(userdata, contribs_weekly)
contribs_weekly_gender

write.csv(contribs_weekly_gender, file.path('aggregdata', 'contribs_weekly_gender.csv'), row.names = FALSE)
