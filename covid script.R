data_date = '2020-03-24'
cntry = "US"
dys = 30
sed = 23
split = 4


us <- raw %>% 
  split_to_country(cntry) %>% 
  get_dr_c()

og <- us
og <- og %>% 
  mutate(
    Country = cntry
  )

us <- us %>% 
  split_for_graph(split) %>% 
  prediction_model(og_d = og, days = dys, r_seed = sed, country = cntry)


pred_us <- us %>% 
  subset(Deaths_PD != 0) %>% 
  slice(-n()) %>% 
  arrange(Date) %>%
  slice(-1) %>% 
  bind_rows(og) %>% 
  arrange(Date) %>% 
  subset(select = c(Country, Date, Deaths, Deaths_PD, Deaths_RC, D_RC_C))


pred_us %>% 
  hchart(type = "line", hcaes(x=Date, y=Deaths), color = "red", name = "Total Deaths") %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_title(text = "Italys' projected Deaths",
           align = "left") %>% 
  hc_subtitle(text = "Using my prediction model",
              align = "left") %>% 
  hc_yAxis(labels = list(format = "{value}"), title = list(text = "Cumulative Number of Deaths")) %>% 
  hc_xAxis(title = list(text = "Date"),
           plotBands = list(
             list(
               label = list(text = "This is the Predicted Data"),
               color = "lightblue",
               from = datetime_to_timestamp(as.Date(data_date)),
               to = datetime_to_timestamp(as.Date('2023-01-01'))
             )))%>% 
  hc_legend(align = "left")


#TO LOOK AT RATES OF CHANGES
#og %>% 
  subset(Country == cntry) %>% 
  subset(!(is.na(Deaths_RC))) %>% 
  hchart( type = "scatter",
          hcaes(x = Deaths_PD, y = Deaths_RC),
          color = "red",
          regression = TRUE,
          borderColor = '#EBBA95',
          borderRadius = 10,
          borderWidth = 2) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_add_dependency("plugins/highcharts-regression.js") %>% 
  hc_title(text = "Rate of Change vs Actual Change",
           align = "left") %>% 
  hc_subtitle(text = c("For",cntry,"Deaths"),
              align = "left") %>% 
  hc_yAxis(labels = list(format = "{value}"),
           title = list(text = "Rate of change")) %>% 
  hc_xAxis(labels = list(format = "{value}"),
           title = list(text = "Cases per Day")) %>% 
  hc_legend(align = "left")
