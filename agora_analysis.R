library(tidyverse)
source("functions.R")
############################# loading data ###################################

df <- read.csv("data/data.csv", stringsAsFactors = F, na.strings = c("", "NA"))


############ if yes analysis #######################
if_yes_analysis <- df %>% 
  group_by(info_manteqa) %>% 
  summarise(
    minority_rep = if_yes(minority_rep), 
    idp_now = if_yes(idp_now),
    manteqa_leadership = if_yes(manteqa_leadership),
    com_group_ag = if_yes(com_group_ag),
    com_group_wash = if_yes(com_group_wash),
    com_group_edu = if_yes(com_group_edu),
    com_group_health = if_yes(com_group_health),
    com_group_finance = if_yes(com_group_finance),
    com_group_women = if_yes(com_group_women),
    com_group_youth = if_yes(com_group_youth),
    com_group_civ_soc = if_yes(com_group_civ_soc),
    former_sector = if_yes(former_sector),
    new_sector = if_yes(new_sector),
    job_needed = if_yes(job_needed),
    unemployed = if_yes(unemployed),
    financial_services = if_yes(financial_services),
    women_outside = if_yes(women_outside),
    women_barriers = if_yes(women_barriers),
    women_business = if_yes(women_business),
    women_challenge = if_yes(women_challenge),
    financial_services_equal = if_yes(financial_services_equal),
    work_migration = if_yes(work_migration),
    migrant_work = if_yes(migrant_work),
    migrant_jobs = if_yes(migrant_jobs),
    production_non_ag = if_yes(production_non_ag),
    export_non_ag = if_yes(export_non_ag),
    import_non_ag = if_yes(import_non_ag),
    trend_non_ag = if_yes(trend_non_ag),
    export_ag = if_yes(export_ag),
    import_ag = if_yes(import_ag),
    trend_ag = if_yes(trend_ag),
    production_livestock = if_yes(production_livestock),
    export_livestock = if_yes(export_livestock),
    import_livestock = if_yes(import_livestock),
    trend_livestock = if_yes(trend_livestock),
    markets_present = if_yes(markets_present),
    markets_other_manteqa = if_yes(markets_other_manteqa),
    market_access = if_yes(market_access),
    markets_marginal_access = if_yes(markets_marginal_access),
    trade_markets_in = if_yes(trade_markets_in),
    trade_markets_in_access = if_yes(trade_markets_in_access),
    voc_women = if_yes(voc_women),
    voc_men = if_yes(voc_men),
    public_transit = if_yes(public_transit),
    edu_access = if_yes(edu_access),
    health_access = if_yes(health_access),
    water_knowledge = if_yes(water_knowledge),
    water_repair = if_yes(water_repair),
    water_tools = if_yes(water_tools),
    water_staff = if_yes(water_staff),
    water_fee = if_yes(water_fee),
    attendance_boys = if_yes(attendance_boys),
    attendance_girls = if_yes(attendance_girls),
    edu_marginal_access = if_yes(edu_marginal_access),
    health_access_equal = if_yes(health_access_equal),
    # if no 
    enough_water = if_no(enough_water)
    )


############ if no analysis #######################

# if_no_analysis <- df %>% 
#   group_by(info_manteqa) %>% 
#   summarise(enough_water = if_no(enough_water)
#   )

##### indicators to be averaged ####################

agora_kii_averages<-df %>% 
  group_by(info_manteqa) %>% 
  summarize(school_fee_cost= mean(school_fee_cost),textbook_cost= mean(textbook_cost),uniforms_cost=mean(uniforms_cost),health_cost=mean(health_cost),transit_cost=mean(transit_cost),water_fee_amt = mean(water_fee_amt))



################ majority response analysis ##################

Majority_response_analysis <- df %>%
  group_by(info_manteqa) %>%
  summarise(hc_remain_perc = first(Mode_if_tie_both(hc_remain_perc)),
            nomads = first(Mode_if_tie_yes(nomads)),
            idp_perc = first(Mode_if_tie_both(idp_perc)),
            market_barrier = first(Mode_if_tie_both(market_barrier)),
            market_access_why = first(Mode_if_tie_both(market_access_why)),
            trade_markets_in_access_why = first(Mode_if_tie_both(trade_markets_in_access_why)),
            goods_access = first(Mode_if_tie_both(goods_access)),
            market_seasonality = first(Mode_if_tie_both(market_seasonality)),
            market_goods = first(Mode_if_tie_both(market_goods)),
            men_iliterate = first(Mode_if_tie_both(men_iliterate)),
            men_primary = first(Mode_if_tie_both(men_primary)),
            men_secondary = first(Mode_if_tie_both(men_secondary)),
            women_iliterate = first(Mode_if_tie_both(women_iliterate)),
            women_primary = first(Mode_if_tie_both(women_primary)),
            women_secondary = first(Mode_if_tie_both(women_secondary)),
            transit_sufficient = first(Mode_if_tie_both(transit_sufficient)),
            public_transit_challenge_type = first(Mode_if_tie_both(public_transit_challenge_type)),
            drinking_water = first(Mode_if_tie_both(drinking_water)),
            cleaning_water = first(Mode_if_tie_both(cleaning_water)),
            enough_water_why = first(Mode_if_tie_both(enough_water_why)),
            water_mgmt = first(Mode_if_tie_both(water_mgmt)),
            edu_access_why = first(Mode_if_tie_both(edu_access_why)),
            health_access_why = first(Mode_if_tie_both(health_access_why)),
            edu_teachers = first(Mode_if_tie_no(edu_teachers)),
            edu_books = first(Mode_if_tie_no(edu_books)),
            edu_training = first(Mode_if_tie_no(edu_training)),
            edu_supplies = first(Mode_if_tie_no(edu_supplies)),
            edu_sanitation = first(Mode_if_tie_no(edu_sanitation)),
            attendance_boys_perc = first(Mode_if_tie_both(attendance_boys_perc)),
            attendance_boys_why= first(Mode_if_tie_both(attendance_boys_why)),
            attendance_girls_perc = first(Mode_if_tie_both(attendance_girls_perc)),
            attendance_girls_why = first(Mode_if_tie_both(attendance_girls_why)),
            health_staff = first(Mode_if_tie_no(health_staff)),
            health_training = first(Mode_if_tie_no(health_training)),
            health_tools = first(Mode_if_tie_no(health_tools)),
            health_medication = first(Mode_if_tie_no(health_medication)),
            health_water = first(Mode_if_tie_no(health_water)),
            water_access_equal = first(Mode_if_tie_yes(water_access_equal))
            )



################# present analysis ############################3

Present_analysis <- df %>%
  group_by(info_manteqa) %>%
  summarise(refugee_return_now = un_strip(present(refugee_return_now)),
            manteqa_leadership_type = un_strip(present(manteqa_leadership_type)),
            minority_rep_type = un_strip(present(minority_rep_type)),
            com_group_ag_type = un_strip(present(com_group_ag_type)),
            com_group_wash_type = un_strip(present(com_group_wash_type)),
            com_group_edu_type = un_strip(present(com_group_edu_type)),
            com_group_health_type = un_strip(present(com_group_health_type)),
            com_group_finance_type = un_strip(present(com_group_finance_type)),
            com_group_civ_soc_type = un_strip(present(com_group_civ_soc_type)),
            com_group_civ_soc_sup = un_strip(present(com_group_civ_soc_sup)),
            info_language = un_strip(present(info_language)),
            econ_sector = un_strip(present(econ_sector)),
            former_sector_type = un_strip(present(former_sector_type)),
            new_sector_type = un_strip(present(new_sector_type)),
            job_needed_sector = un_strip(present(job_needed_sector)),
            unemployed_why = un_strip(present(unemployed_why)),
            financial_services_type = un_strip(present(financial_services_type)),
            women_barriers_type = un_strip(present(women_barriers_type)),
            women_business_sector = un_strip(present(women_business_sector)),
            women_challenge_type = un_strip(present(women_challenge_type)),
            financial_services_women = un_strip(present(financial_services_women)),
            work_migration_who = un_strip(present(work_migration_who)),
            production_non_ag_type = un_strip(present(production_non_ag_type)),
            export_non_ag_type = un_strip(present(export_non_ag_type)),
            import_non_ag_type = un_strip(present(import_non_ag_type)),
            trend_non_ag_type = un_strip(present(trend_non_ag_type)),
            production_ag = un_strip(present(production_ag)),
            production_ag_type = un_strip(present(production_ag_type)),
            export_ag_type = un_strip(present(export_ag_type)),
            import_ag_type = un_strip(present(import_ag_type)),
            trend_ag_type = un_strip(present(trend_ag_type)),
            production_animals = un_strip(present(production_animals)),
            production_livestock_type = un_strip(present(production_livestock_type)),
            export_livestock_type = un_strip(present(export_livestock_type)),
            import_livestock_type = un_strip(present(import_livestock_type)),
            trend_livestock_type = un_strip(present(trend_livestock_type)),
            markets_marginal_access_who = un_strip(present(markets_marginal_access_who)),
            goods_access_why = un_strip(present(goods_access_why)),
            good_types = un_strip(present(good_types)),
            voc_women_avail = un_strip(present(voc_women_avail)),
            voc_men_avail = un_strip(present(voc_men_avail)),
            voc_train_who = un_strip(present(voc_train_who)),
            voc_women_need = un_strip(present(voc_women_need)),
            voc_men_need = un_strip(present(voc_men_need)),
            edu_type = un_strip(present(edu_type)),
            edu_infrastructure = un_strip(present(edu_infrastructure)),
            health_services = un_strip(present(health_services)),
            edu_source = un_strip(present(edu_source)),
            water_access_marginal = un_strip(present(water_access_marginal)),
            edu_marginal_access_who = un_strip(present(edu_marginal_access_who)),
            health_access_marginal = un_strip(present(health_access_marginal))
            )


##### merge analysis dataframes #############


if_yes_and_majority <- full_join(if_yes_analysis, Majority_response_analysis, by = "info_manteqa")
if_yes_and_majority_present <- full_join(if_yes_and_majority, Present_analysis, by = "info_manteqa")
full_analysis <- full_join(if_yes_and_majority_present, agora_kii_averages, by = "info_manteqa")


############## Export analysis in CSV format ############
write_excel_csv(full_analysis, "AGORA_KII_DC12_Analysis.csv")



