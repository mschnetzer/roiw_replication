# LOAD PACKAGES ----

# Packages for data editing
librarian::shelf(tidyverse, fastDummies, DIGCLASS)
# Packages for data loading
librarian::shelf(eurostat, readxl, haven)
# Packages for calculation and output
librarian::shelf(lme4, ordinal, ggrepel, ggnewscale, texreg, htmlTable, kableExtra)

# LOAD DATA ----

## Load ESS data ----
# Please download ESS Round 9 (Ed. 3.1) for free at https://ess.sikt.no/en/datafile/b2b0bf39-176b-4eca-8d26-3c05ea83d2cb after registering. The file format is Stata *.dta!
ess <- read_dta("Data/ESS9e03_1.dta", encoding = "latin1")

# Convert haven_labelled into numeric
is.havenlab <- function(x) "haven_labelled" %in% class(x)
ess <- ess |> mutate(across(where(is.havenlab), as.numeric))

## Load Eurostat macro data ----
# ATTENTION: For convenience, we present the code originally used to retrieve Eurostat data but also provide the data as RData file in the replication folder!

# # Social expenditure
# socexp <- get_eurostat(id = "tps00098", 
#                        filters = list(geo = c(unique(ess$cntry), "UK"), 
#                                       time = 2013:2018), 
#                        time_format = "num") |> 
#   select(cntry = geo, year = time, socexp = values)
# # GDP per capita in PPS
# gdp <- get_eurostat(id = "tec00114", 
#                     filters = list(geo = c(unique(ess$cntry), "UK"), 
#                                    time = 2013:2018), 
#                     time_format = "num") |> 
#   select(cntry = geo, year = time, gdp = values)
# # People at risk of poverty or social exclusion
# pov <- get_eurostat(id = "ilc_peps01n", 
#                     filters = list(geo = c(unique(ess$cntry), "UK"), 
#                                    time = 2013:2018, 
#                                    unit = "PC", 
#                                    age = "TOTAL", 
#                                    sex = "T"), 
#                     time_format = "num") |> 
#   select(cntry = geo, year = time, poverty = values)
# # Gini coefficient of equivalised household income
# inc_gini <- get_eurostat(id = "ilc_di12", 
#                          filters = list(geo = c(unique(ess$cntry), "UK"), 
#                                         time = 2013:2018), 
#                          time_format = "num") |> 
#   select(cntry = geo, year = time, inc_gini.eu = values)
# 
# eurodat <- plyr::join_all(list(socexp, gdp, pov, inc_gini), 
#                           by = c("cntry", "year"), type = "left")
# 
# write_rds(eurodat, "Data/eurostat_3yr.RData")

# Rescale macro data for ordered logit estimation
eurodat <- readRDS("Data/eurostat_3yr.RData") |> 
  mutate(cntry = recode(cntry, "UK" = "GB")) |> 
  mutate(across(-c(cntry), ~as.numeric(scale(.)), .names = "{.col}_resc"), .by = year)

## Load WID data ----
# ATTENTION: the {wid} package for R is no longer available! For convenience, we present the code originally used to retrieve the data and provide the data as RData file in the replication folder!

# library(wid)
# wid.s <- download_wid(indicators = "shweal",
#                       areas = unique(ess$cntry),
#                       perc = c("p90p100", "p95p100","p99p100"),
#                       years = 2016:2018)
# wid.g <- download_wid(indicators = "ghweal",
#                       areas = unique(ess$cntry),
#                       years = 2016:2018)
# wid.r <- download_wid(indicators = "rhweal",
#                       areas = unique(ess$cntry),
#                       years = 2016:2018)
# wid.inc.s <- download_wid(indicators = "sdiinc",
#                           areas = unique(ess$cntry),
#                           perc = c("p90p100", "p95p100","p99p100"),
#                           years = 2016:2018)
# wid.inc.g <- download_wid(indicators = "gdiinc",
#                           areas = unique(ess$cntry),
#                           years = 2016:2018)
# 
# wid <- bind_rows(wid.s, wid.g, wid.r, wid.inc.s, wid.inc.g) |> 
#   mutate(cntry = country,
#          value = ifelse(variable == "rhweal992j", value, value * 100),
#          indicator = case_when(
#            percentile == "p90p100" & variable == "shweal992j" ~ "wlth_top10",
#            percentile == "p95p100" & variable == "shweal992j" ~ "wlth_top5",
#            percentile == "p99p100" & variable == "shweal992j" ~ "wlth_top1",
#            percentile == "p90p100" & variable == "sdiinc992j" ~ "inc_top10",
#            percentile == "p95p100" & variable == "sdiinc992j" ~ "inc_top5",
#            percentile == "p99p100" & variable == "sdiinc992j" ~ "inc_top1",
#            variable == "rhweal992j" ~ "wlth_1050",
#            variable == "ghweal992j" ~ "wlth_gini",
#            variable == "gdiinc992j" ~ "inc_gini")) |> 
#   pivot_wider(names_from = "indicator", id_cols = c(cntry, year)) |> 
#   arrange(desc(wlth_gini)) |>
#   mutate(rank = 1:n(), .by = year)
# 
# write_rds(wid, "wid.RData")

widdat <- readRDS("Data/wid.RData") |> 
  filter(year == 2018) |> 
  select(-year) |> 
  mutate(across(starts_with("wlth"), ~as.numeric(scale(.)), .names = "{.col}_resc"))

## Load HFCS data ----
# ATTENTION: The HFCS 2017 statistical tables can be downloaded directly from https://www.ecb.europa.eu/stats/ecb_surveys/hfcs/html/index.en.html For convenience, we provide the XLSX file in the replication folder.
hfcs <- read_xlsx("Data/HFCS - Statistical tables - wave 2017.xlsx", 
                  sheet = "J4 Net wealth inequality ind", skip = 2) |> 
  filter(`...2` %in% c("Gini coefficient", "Top 5% share", "Top 10% share")) |> 
  pivot_longer(cols = BE:FI, names_to = "cntry", values_to = "value") |> 
  select(cntry, indicator = `...2`, value) |> 
  mutate(value = as.numeric(value),
         indicator = case_when(str_detect(indicator, "5%") ~ "wlth_top5",
                               str_detect(indicator, "10%") ~ "wlth_top10",
                               str_detect(indicator, "Gini") ~ "wlth_gini")) |> 
  pivot_wider(names_from = indicator, values_from = value) |> 
  mutate(wlth_gini = wlth_gini*100) |> 
  mutate(across(-cntry, ~as.numeric(scale(.)), .names = "{.col}_resc"))


# CREATE NEW VARIABLES ----
ess <- ess |> 
  mutate(dpweight = pspwght*pweight,
         quintile = case_match(hinctnta,
                               1:2 ~ "Q1",
                               3:4 ~ "Q2",
                               5:6 ~ "Q3",
                               7:8 ~ "Q4", 
                               9:10 ~ "Q5", 
                               TRUE ~ NA_character_),
         n_quintile = as.numeric(str_extract(quintile, "[1-9]")),
         d_wltdffr = case_match(wltdffr, 1:4 ~ 1, 0:-4 ~ 0),
         female = case_match(gndr, 2 ~ 1, 1 ~ 0),
         academic = case_match(eisced, 6:7 ~1, c(0:5, 55) ~ 0),
         unemp12m = case_when(uemp12m == 1 ~ 1, uemp12m == 2 ~ 0, uemp3m == 2 ~ 0, TRUE ~ NA),
         d_blgetmg = case_match(blgetmg, 1 ~ 1, 2 ~ 0),
         d_lowearner = case_match(hinctnta, 1:3 ~ 1, 4:10 ~ 0),
         d_midearner = case_match(hinctnta, 4:7 ~ 1, c(1:3,8:10) ~ 0),
         d_highearner = case_match(hinctnta, 8:10 ~ 1, 1:7 ~ 0),
         d_diffinc = case_match(hincfel, 3:4 ~ 1, 1:2 ~ 0),
         d_incunfairlow = case_match(netifr, -1:-4 ~ 1 , 0:4 ~ 0),
         d_imprich = case_match(imprich, 1:2 ~ 1, 3:4 ~ 0),
         d_eqopp = case_match(ipeqopt, 1:3 ~ 1, 4:6 ~ 0),
         d_topinfr = case_match(topinfr, 1:4 ~ 1, 0:-4 ~ 0),
         d_polintr = case_match(polintr, 1:2 ~ 1, 3:4 ~ 0), 
         d_psppipla = case_match(psppipla, 4:5 ~ 1, 1:3 ~ 0), 
         d_trstprl = case_match(trstprl, 8:10 ~ 1, 0:7 ~ 0), 
         d_trstplt = case_match(trstplt, 8:10 ~ 1, 0:7 ~ 0), 
         d_stfdem = case_match(stfdem, 8:10 ~ 1, 0:7 ~ 0),
         d_vote = case_match(vote, 1 ~ 1, 2 ~ 0),
         r_sofrdst = car::recode(sofrdst, "1=5;2=4;4=2;5=1"),
         r_sofrwrk = car::recode(sofrwrk, "1=5;2=4;4=2;5=1"),
         r_sofrpr = car::recode(sofrpr, "1=5;2=4;4=2;5=1"),
         r_sofrprv = car::recode(sofrprv, "1=5;2=4;4=2;5=1"),
         d_sofrdst = case_match(sofrdst, 1:2 ~ 1, 3:5 ~ 0),
         d_sofrwrk = case_match(sofrwrk, 1:2 ~ 1, 3:5 ~ 0),
         d_sofrpr = case_match(sofrpr, 1:2 ~ 1, 3:5 ~ 0),
         d_sofrprv = case_match(sofrprv, 1:2 ~ 1, 3:5 ~ 0),
         selfemployed = case_when(emplrel == 2 ~ 1, emplrel %in% c(1,3) ~ 0, TRUE ~ NA),
         oesch = as.numeric(isco08_to_oesch(x = isco08, self_employed = selfemployed, 
                                            n_employees = emplno, n_classes = 5)),
         d_unskillworker = case_match(oesch, 5 ~ 1, 1:4 ~ 0, TRUE ~ NA),
         d_skillworker = case_match(oesch, 4 ~ 1, c(1:3,5) ~ 0, TRUE ~ NA),
         d_smallself = case_match(oesch, 3 ~ 1, c(1,2,4,5) ~ 0, TRUE ~ NA),
         d_lowservice = case_match(oesch, 2 ~ 1, c(1,3:5) ~ 0, TRUE ~ NA),
         d_highservice = case_match(oesch, 1 ~ 1, 2:5 ~ 0, TRUE ~ NA),
         d_workingclass = case_match(oesch, 4:5 ~ 1, 1:3 ~ 0, TRUE ~ NA),
         d_lowermiddle = case_match(oesch, 2:3 ~ 1, c(1, 4:5) ~ 0, TRUE ~ NA),
         d_uppermiddle = case_match(oesch, 1 ~ 1, 2:5 ~ 0, TRUE ~ NA))


# Country table with welfare regimes
cntry <- tibble(cntry = unique(ess$cntry), regime = NA) |> 
  mutate(regime = case_match(cntry,
                             c("AT","BE","DE","FR","NL") ~ "Conservative",
                             c("FI","NO","SE","DK","IS") ~ "Social democratic",
                             c("BG","CZ","EE","HR","HU","LT","LV","ME","PL","RS","SI","SK") ~ "Post-socialist",
                             c("IE","GB","CH") ~ "Liberal",
                               c("CY","ES","IT","PT") ~ "Family-oriented"))


# FIGURES ----

## Figure 1: Compare Gini for Wealth and Income ----
ginidat <- left_join(eurodat |> filter(year == 2018) |> 
                       select(cntry, Income = inc_gini.eu), 
                     widdat |> select(cntry, Wealth = wlth_gini)) |> 
  arrange(desc(Wealth)) |> 
  mutate(rank = 1:n()) |> 
  pivot_longer(cols = -c(cntry,rank), names_to = "type", values_to = "values")

ginidat |> ggplot(aes(x = reorder(cntry, rank, na.rm=T))) +
  geom_segment(aes(y = 0, yend = values, xend = cntry), linewidth = 0.1, 
               color="grey70") +
  geom_point(aes(y = values, shape = type, color = type), size = 2) +
  geom_hline(aes(yintercept = mean(values[type == "Income"])),
             linewidth = 0.2, linetype = "longdash", color = "navyblue") +
  geom_hline(aes(yintercept = mean(values[type == "Wealth"], na.rm=T)), 
             linewidth = 0.2, linetype = "longdash", color = "firebrick") +
  annotate("text", x=28, y = 77, label = "Average", size = 3.3, 
           family = "Roboto Condensed") +
  scale_color_manual(values = c("navyblue", "firebrick"), 
                     labels = c("Income" = "Disposable income", 
                                "Wealth" = "Net wealth"), 
                     guide = guide_legend(reverse = T)) +
  scale_shape_manual(values = c(19,17), labels = c("Income" = "Disposable income", 
                                                   "Wealth" = "Net wealth"),
                     guide = guide_legend(reverse = T, override.aes = list(size=3))) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  labs(x = NULL, y = "Gini index") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave("Figures/compare_gini.png", width = 8, height = 4, dpi = 320, bg = "white")
ggsave("Figures/compare_gini.pdf", width = 8, height = 4, bg = "white", device=cairo_pdf)


## Figure 2: Scatter plot b/w Top 5% and Unfair perception ----
esswlt <- ess |> 
  select(cntry, dweight, pweight, d_wltdffr) |> 
  filter(!is.na(d_wltdffr)) |> 
  summarise(share = sum(d_wltdffr*dweight)/sum(dweight)*100, 
            .by = cntry)

scatdat <- left_join(esswlt, widdat |> select(cntry, wlth_top5)) |>  
  left_join(cntry |> select(cntry, regime))

scatdat  |>  
  ggplot(aes(x = wlth_top5, y = share)) +
  geom_vline(xintercept = mean(scatdat$wlth_top5), 
             linetype = "longdash", linewidth = 0.3) +
  geom_hline(yintercept = mean(scatdat$share), linetype = "longdash", linewidth = 0.3) +
  geom_smooth(method='lm', formula=y~x, fill="gray90", color="black", 
              linewidth=0.5, level=0.9) +
  geom_point(color = "black", size = 1) +
  ggpubr::stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, label.x = 52.5, 
                   label.y = 25, family = "Roboto Condensed", hjust = 0.5) +
  geom_label_repel(aes(label = cntry, fill = regime), size=3, label.padding = 0.15, 
                   box.padding = 0.12, force_pull = 0.05, 
                   color = ifelse(scatdat$regime %in% c("Conservative","Family-oriented","Liberal","Post-socialist"), "white","black"),
                   min.segment.length = 1, seed = 1234, family = "Roboto Condensed") +
  scale_y_continuous(labels = scales::percent_format(scale=1, accuracy=1)) +
  scale_x_continuous(labels = scales::percent_format(scale=1, accuracy=1),
                     expand = c(0.02, 0.05)) +
  scale_fill_manual(name = "Welfare regime:",
                    values = futurevisions::futurevisions("cancri"),
                    guide = guide_legend(override.aes = aes(color = "transparent", label = "aa"),
                                         title.position = "left")) +
  labs(x="Net wealth share of the top 5%", 
       y="Perception of unfairly large wealth differences") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(legend.position = "top",
        legend.title = element_text(size = 11, margin = margin(r = 3)),
        legend.text = element_text(size = 11, margin = margin(r = 4)),
        legend.key.spacing = unit(0, "pt"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25),
        axis.title = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))
ggsave("Figures/scatterplot_wr.png", width = 8, height = 4.5, dpi = 320, bg = "white")
ggsave("Figures/scatterplot_wr.pdf", width = 8, height = 4.5, bg = "white", device = cairo_pdf)


## Figure 3: Compare unfair perception income vs wealth ----
esswltinc <- ess |> 
  select(cntry, dweight, pweight, d_wltdffr, d_topinfr) |> 
  pivot_longer(cols = c(d_wltdffr, d_topinfr), names_to = "var", values_to = "value") |> 
  filter(!is.na(value)) |> 
  summarise(share = sum(value*dweight)/sum(dweight), 
            .by = c(cntry, var)) |> 
  left_join(cntry |> select(cntry, regime))

orderbywealth <- esswltinc |> filter(var == "d_wltdffr") |> 
  arrange(-share) |> pull(cntry)

plotdat <- esswltinc |> mutate(cntry = factor(cntry, levels = orderbywealth),
                               share = share*100)

# Correlation between wealth and income inequality perception
plotdat |> pivot_wider(names_from = "var", values_from = "share") |> 
  select(starts_with("d_")) |> cor()

plotdat |> 
  ggplot(aes(x = cntry, y = share, group = var)) +
  geom_linerange(data = plotdat |> group_by(cntry) |> slice_max(share, n = 1), 
                 aes(ymin = 0, ymax = share), linewidth = 5.3, 
                 alpha = .3, color = "gray80") +
  geom_point(aes(color = var), size = 5, shape = 15) + 
  geom_text(aes(label = round(share, 0)), color = "white", size = 3.3, hjust = .5,
            vjust = .5, family = "Roboto Condensed") +
  scale_color_manual(name = NULL,
                     values = c("gray20", "gray60"), 
                     labels = c("Income of top 10%", "Wealth differences"),
                     guide = guide_legend(reverse = T)) +
  new_scale_color() +
  geom_point(aes(y=-2.3, color = regime), size=5, shape = 15,  
             data = plotdat |> slice_head(n=1, by=cntry)) +
  geom_text(aes(label = cntry, y = -2.3), size = 3.3, color = "white", 
            family = "Roboto Condensed", vjust = .5, hjust = .5,
            data = plotdat |> slice_head(n=1, by=cntry)) +
  scale_color_manual(name = "Welfare regime:",
                    values = futurevisions::futurevisions("cancri"),
                    guide = guide_legend(title.position = "left", position = "bottom",
                                         keywidth = 0.7, keyheight = 0.7,
                                         )) +
  scale_y_continuous(labels = scales::percent_format(scale=1)) + 
  labs(x = NULL, y = "Share of response: unfairly high") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.25),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.85),
        legend.title = element_text(size = 11, margin = margin(r = 3)),
        legend.text = element_text(size = 11),
        legend.key.spacing = unit(0, "pt"),
        legend.key.spacing.x = unit(0.5, "lines"))
ggsave("Figures/incvswealth.png", width = 8, height = 4.3, dpi = 320, bg = "white")
ggsave("Figures/incvswealth.pdf", width = 8, height = 4.3, bg = "white", device = cairo_pdf)


# DESCRIPTIVES ----

## Select variables ----
vars <- tribble(~variable, ~name, ~source,
                "dpweight", "Weights", "ESS",
                "cntry", "Country", "ESS",
                "wltdffr", "Perception of wealth differences", "ESS",
                "fairwlth", "Perception of wealth differences", "ESS",
                "wlth_top1", "Top 1% wealth share", "WID", 
                "wlth_top5", "Top 5% wealth share", "WID",
                "wlth_top5_resc", "Top 5% wealth share", "WID",
                "wlth_top10", "Top 10% wealth share", "WID",
                "wlth_top10_resc", "Top 10% wealth share", "WID",
                "wlth_gini", "Wealth Gini", "WID",
                "wlth_gini_resc", "Wealth Gini", "WID",
                "gdp", "GDP per capita (PPP)", "Eurostat",
                "gdp_resc", "GDP per capita (PPP)", "Eurostat",
                "socexp", "Social expenditure", "Eurostat",
                "socexp_resc", "Social expenditure", "Eurostat",
                "unemp", "Unemployment rate", "Eurostat",
                "unemp_resc", "Unemployment rate", "Eurostat",
                "poverty", "Poverty rate", "Eurostat",
                "poverty_resc", "Poverty rate", "Eurostat",
                "inc_gini.eu", "Income Gini coefficient", "Eurostat",
                "inc_gini.eu_resc", "Income Gini coefficient", "Eurostat",
                "inc_8020", "S80/20 Ratio of income", "Eurostat",
                "inc_b40", "Income share of bottom 40%", "Eurostat",
                "female", "Gender (female)", "ESS",
                "agea", "Age", "ESS",
                "d_blgetmg", "Ethnic minority", "ESS",
                "d_lowearner", "Income: Bottom 3 deciles", "ESS",
                "d_midearner", "Income: Middle 4 deciles", "ESS",
                "d_highearner", "Income: Top 3 deciles", "ESS",
                "n_quintile", "Income quintile", "ESS",
                "d_workingclass", "Social class: working class", "ESS",
                "d_lowermiddle", "Social class: lower middle", "ESS",
                "d_uppermiddle", "Social class: upper middle", "ESS",
                "unemp12m", "Experience with long-term unempl.", "ESS",
                "r_sofrdst", "Justice principle: Equality", "ESS",
                "r_sofrwrk", "Justice principle: Equity", "ESS",
                "r_sofrpr", "Justice principle: Need", "ESS",
                "r_sofrprv", "Justice principle: Entitlement", "ESS",
                "d_topinfr", "Top income perceived unfair", "ESS",
                "d_trstprl", "Trust in parliament", "ESS",
                "d_stfdem", "Satisfied with democracy", "ESS",
                "(Intercept)", "Constant", NA) |> 
  mutate(variable = fct_inorder(variable))

## Prepare regression data set ----
mldat <- ess |>
  select(any_of(vars$variable)) |> 
  left_join(eurodat |> filter(year == 2018)) |> 
  left_join(widdat) |> 
  left_join(cntry) |> 
  fastDummies::dummy_cols(select_columns = "cntry", remove_most_frequent_dummy = T) |>
  mutate(fairwlth = wltdffr,
         fairwlthfac = factor(ifelse(wltdffr > 0, 1, 0)),
         fairwlthord = factor(fairwlth, ordered = T, levels = -4:4)) |> 
  drop_na()


## Descriptive statistics ----
desc.micro <- mldat |> 
  select(cntry, wltdffr, female, agea, d_midearner, d_highearner, d_lowermiddle, d_uppermiddle, unemp12m, r_sofrdst, r_sofrwrk, r_sofrpr, r_sofrprv, d_topinfr, d_trstprl, d_stfdem) |> 
  pivot_longer(cols = -cntry, names_to = "variable", values_to = "values") |> 
  summarise(across(values, list(mean = ~mean(.x, na.rm = T), sd = ~sd(.x, na.rm = T),
                                min = ~min(.x), max = ~max(.x))), .by = variable) 

desc.macro <- mldat |> 
  select(cntry, wlth_top5, wlth_top10, wlth_gini, gdp, socexp, poverty, inc_gini.eu) |>
  slice(1, .by = cntry) |> 
  pivot_longer(cols = -cntry, names_to = "variable", values_to = "values") |> 
  summarise(across(values, list(mean = ~mean(.x, na.rm = T), sd = ~sd(.x, na.rm = T),
                                min = ~min(.x), max = ~max(.x))), .by = variable)

bind_rows(desc.macro, desc.micro) |> 
  mutate(across(where(is.numeric), ~round(.x, 1)),
         variable = fct_relevel(fct_inorder(variable), "wltdffr", after = 0)) |> 
  arrange(variable) |> 
  left_join(vars) |> 
  select(name, source, starts_with("values")) |> 
  addHtmlTableStyle(align = "llrrrr") |> 
  htmlTable(header = c("", "Source", "Mean", "S.D.", "Min.", "Max."),
            rnames = F,
            rgroup = c("Dependent variable", "Macro variables", "Micro variables"), 
            n.rgroup = c(1, 7), 
            ctable = T,
            caption = "Descriptive statistics of the variables of interest") |> 
  save_kable("Tables/descriptive.html")


# REGRESSIONS ----

## Ordered logistic mixed-effects model ----
olfit0 <- ordinal::clmm(fairwlthord ~ 1 + (1 | cntry), 
                        data = mldat, weights = dpweight)
olfit1 <- ordinal::clmm(fairwlthord ~ wlth_top5_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + (1 | cntry), 
                        data = mldat, weights = dpweight)
olfit2 <- ordinal::clmm(fairwlthord ~ wlth_top5_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + (1 | cntry), 
                        data = mldat, weights = dpweight)
olfit3 <- ordinal::clmm(fairwlthord ~ wlth_top5_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), 
                        data = mldat, weights = dpweight)
olfit4 <- ordinal::clmm(fairwlthord ~ wlth_top10_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), 
                        data = mldat, weights = dpweight)
olfit5 <- ordinal::clmm(fairwlthord ~ wlth_gini_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), 
                        data = mldat, weights = dpweight)

htmlreg(list(olfit1,olfit2,olfit3,olfit4,olfit5), file = "Tables/mlmolreg2.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        custom.gof.names = c("Log Likelihood","AIC","BIC","Num. groups: cntry",
                             "Var: cntry (Intercept)"),
        include.nobs = F, # because nobs in clmm are sum of weights
        custom.gof.rows = list("Num. obs" = rep("31435", 5)), # manual nobs
        reorder.gof = c(3,4,2,1,5,6),
        groups = list("Macro variables" = 1:7, "Micro variables" = 8:21), 
        caption = "Mixed-effects ordered logistic Regression", 
        custom.model.names = paste0("(",1:5,")"))

# Intraclass correlation coefficient (ICC)
# latent approach for ordered logit: var_b/(var_b + pi^2/3)
# https://lnalborczyk.github.io/blog/2017-10-10-icc/
var_b <- as.numeric(VarCorr(olfit0)) # between-group variation
ICC <- var_b / (var_b + pi^2/3)


## Robustness: Linear mixed effects model ----
fit0 <- lmer(fairwlth ~ 1 + (1 | cntry), data = mldat, weights = dpweight, REML = F)
fit1 <- lmer(fairwlth ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + (1 | cntry), data = mldat, weights = dpweight, REML = F)
fit2 <- lmer(fairwlth ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + (1 | cntry), data = mldat, weights = dpweight, REML = F)
fit3 <- lmer(fairwlth ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = mldat, weights = dpweight, REML = F)
fit4 <- lmer(fairwlth ~ wlth_top10 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = mldat, weights = dpweight, REML = F)
fit5 <- lmer(fairwlth ~ wlth_gini + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = mldat, weights = dpweight, REML = F)


htmlreg(list(fit1,fit2,fit3,fit4,fit5), file = "Tables/mlmreg.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        groups = list("Macro variables" = 1:7, "Micro variables" = 8:21), 
        caption = "Multilevel Model Regression", 
        custom.model.names = paste0("(",1:5,")"))


## Robustness: Binomial logistic mixed-effects model ----
## See also http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/
lfit0 <- glmer(fairwlthfac ~ 1 + (1 | cntry), data = mldat, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
lfit1 <- glmer(fairwlthfac ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + (1 | cntry), data = mldat, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
lfit2 <- glmer(fairwlthfac ~ wlth_top5_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + (1 | cntry), data = mldat, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
lfit3 <- glmer(fairwlthfac ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = mldat, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
lfit4 <- glmer(fairwlthfac ~ wlth_top10 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = mldat, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
lfit5 <- glmer(fairwlthfac ~ wlth_gini + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = mldat, family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))


htmlreg(list(lfit1,lfit2,lfit3,lfit4,lfit5), file = "Tables/mlmblreg.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        groups = list("Macro variables" = 1:7, "Micro variables" = 8:21), 
        caption = "Mixed-effects binomial logistic Regression", 
        custom.model.names = paste0("(",1:5,")"))


# Combined table of OLS and binomial logit 
htmlreg(list(fit3,fit4,fit5,lfit3,lfit4,lfit5), file = "Tables/robcheck_model.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        groups = list("Macro variables" = 1:7, "Micro variables" = 8:21), 
        caption = "Linear mixed-effects and binomial mixed-effects model", 
        custom.header = list("OLS" = 1:3, "Binomial logit" = 4:6),
        custom.model.names = paste0("(",1:6,")"))


## Robustness with HFCS data ----
robhfcs <- mldat |>
  select(-starts_with("wlth_")) |> 
  left_join(hfcs) |> 
  left_join(cntry) |> 
  drop_na()

hfcs0 <- ordinal::clmm(fairwlthord ~ 1 + (1 | cntry), data = robhfcs, weights = dpweight)
hfcs1 <- ordinal::clmm(fairwlthord ~ wlth_top5_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + (1 | cntry), data = robhfcs, weights = dpweight)
hfcs2 <- ordinal::clmm(fairwlthord ~ wlth_top5_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + (1 | cntry), data = robhfcs, weights = dpweight)
hfcs3 <- ordinal::clmm(fairwlthord ~ wlth_top5_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = robhfcs, weights = dpweight)
hfcs4 <- ordinal::clmm(fairwlthord ~ wlth_top10_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = robhfcs, weights = dpweight)
hfcs5 <- ordinal::clmm(fairwlthord ~ wlth_gini_resc + inc_gini.eu_resc + gdp_resc + socexp_resc + poverty_resc + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = robhfcs, weights = dpweight)

htmlreg(list(hfcs1, hfcs2, hfcs3, hfcs4, hfcs5), file = "Tables/robcheck_hfcs.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        groups = list("Macro variables" = 1:7, "Micro variables" = 8:21), 
        caption = "Mixed-effects ordered logit with HFCS data", 
        custom.model.names = paste0("(",1:5,")"))


## Robustness with trend in macro vars ----
chgdat <- eurodat |> 
  filter(year > 2015) |> 
  reframe(across(c(socexp, gdp, poverty, inc_gini.eu), 
                 ~mean((./lag(.)-1)*100, na.rm = T)),
            .by = c(cntry))

chginq <- readRDS("Data/wid.RData") |> 
  reframe(across(starts_with("wlth"), 
                 ~mean((./lag(.)-1)*100, na.rm = T)),
          .by = c(cntry))

robchg <- mldat |>
  select(-starts_with("wlth_"), -c(socexp, gdp, poverty, inc_gini.eu)) |> 
  left_join(chgdat) |> 
  left_join(chginq) |> 
  left_join(cntry) |> 
  drop_na()

chg0 <- ordinal::clmm(fairwlthord ~ 1 + (1 | cntry), data = robchg, weights = dpweight, REML = F)
chg1 <- ordinal::clmm(fairwlthord ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + (1 | cntry), data = robchg, weights = dpweight)
chg2 <- ordinal::clmm(fairwlthord ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + (1 | cntry), data = robchg, weights = dpweight)
chg3 <- ordinal::clmm(fairwlthord ~ wlth_top5 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = robchg, weights = dpweight)
chg4 <- ordinal::clmm(fairwlthord ~ wlth_top10 + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = robchg, weights = dpweight)
chg5 <- ordinal::clmm(fairwlthord ~ wlth_gini + inc_gini.eu + gdp + socexp + poverty + female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + (1 | cntry), data = robchg, weights = dpweight)

htmlreg(list(chg1, chg2, chg3, chg4, chg5), file = "Tables/robcheck_trend.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        groups = list("Macro variables" = 1:7, "Micro variables" = 8:21), 
        caption = "Mixed-effects ordered logit with macro trend", 
        custom.model.names = paste0("(",1:5,")"))


## Aggregate table of robustness checks: HFCS and Trend
htmlreg(list(hfcs3, hfcs4, hfcs5, chg3, chg4, chg5), 
        file = "Tables/robcheck_summary.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        groups = list("Macro variables" = 1:7, "Micro variables" = 8:21), 
        caption = "Robustness check with HFCS and macro trend", 
        custom.header = list("HFCS data" = 1:3, "Macro trend" = 4:6),
        custom.model.names = paste0("(",1:6,")"))

## Fixed effects regression ----
ctyfe <- paste(names(mldat |> select(starts_with("cntry_"))), collapse = "+")

## OLS
olsfe <- lm(formula(paste0("fairwlth ~ female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + ", ctyfe)), 
            data = mldat, weights = dpweight)

## Binomial logit
blfe <- glm(formula(paste0("fairwlthfac ~ female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + ", ctyfe)), 
            data = mldat, weights = dpweight, family = "binomial")

## Ordered logit
olfe <- ordinal::clm(formula(paste0("fairwlthord ~ female + agea + d_midearner + d_highearner + d_lowermiddle + d_uppermiddle + unemp12m + d_topinfr + r_sofrdst + r_sofrwrk + r_sofrpr + r_sofrprv + d_trstprl + d_stfdem + ", ctyfe)), 
                     data = mldat, weights = dpweight)


htmlreg(list(olfe, olsfe, blfe), file = "Tables/fereg.html", 
        custom.coef.map = split(vars$name, vars$variable),
        digits = 3, stars = c(0.001, 0.01, 0.05),
        caption = "Fixed Effects Regression", 
        custom.gof.rows = list("Country FE" = rep("Yes",3)), 
        custom.model.names = c("Ordered Logit", "OLS", "Binomial Logit"))
