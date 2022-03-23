#######################################
# Źródła danych:
# 1. ECB SDW;
#   - CBD2 (stany kredytów, aktywa) 
#   - MIR (nowe kredyty, oprocentowanie)
# 2. Eurostat (PKB)

# Ad. 1. Mapowanie poszczególnych pozycji CBD2 na FINREP (można zobaczyć z którego formularza są dane):
# https://www.ecb.europa.eu/stats/supervisory_prudential_statistics/consolidated_banking_data/shared/data/CBD2_catalogue_20210218.zip

#######################################

packages = c("tidyverse", "ecb", "eurostat", "RcppRoll", "readxl", "imputeTS", "writexl", 
             "patchwork", "ggrepel", "gghighlight", "gridExtra")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# tu co� nowego
stworz_date <- function(x) {
  
  okres=case_when(substr(x,6,7)=="Q1" ~ lubridate::make_date(as.numeric(substr(x,1,4)),3,31),
                  substr(x,6,7)=="Q2" ~ lubridate::make_date(as.numeric(substr(x,1,4)),6,30),
                  substr(x,6,7)=="Q3" ~ lubridate::make_date(as.numeric(substr(x,1,4)),9,30),
                  substr(x,6,7)=="Q4" ~ lubridate::make_date(as.numeric(substr(x,1,4)),12,31))
  
  return(okres)
  
}

stworz_grupe_krajow <- function(x) {
  
  grupa = case_when(x %in% c("AT","BE","CY","EE","FI","FR","GR","ES","IE","LT",
                               "LU","LV","MT","NL","DE","PT","SK","SI","IT") ~ "EA",
                    !x %in% c("AT","BE","CY","EE","FI","FR","GR","ES","IE","LT",
                                "LU","LV","MT","NL","DE","PT","SK","SI","IT", "PL") ~ "non-EA",
                    TRUE ~ "PL")
  return(grupa)
  
}

eu <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GR", "ES", "IE", "LT", 
        "LU", "LV", "MT", "NL", "DE", "PL", "PT", "RO", "SK", "SI", "SE", "HU", "IT")

ea <- c("AT", "BE", "CY", "EE", "FI", "FR", "GR", "ES", "IE", "LT", "LU", "LV", "MT", "NL", 
        "DE", "PT", "SK", "SI", "IT")

##### DANE ŹRÓDŁOWE - uwaga, dane EBA z dysku P, trzeba ściągnąć ze strony nowy plik przy aktualizacji

# PORTFELE - KREDYTY MIESZKANIOWE (NA CEL)
portfel_rre_eur <- ecb::get_data("CBD2.Q..W0.67.S1M._Z.A.F.A1135._X.ALL.CA._Z.LE._T.EUR") %>% 
  select(c("ref_area", "obstime", "obsvalue")) %>% 
  rename(geo = "ref_area",
         time = "obstime",
         rre_loans = "obsvalue") %>% 
  filter(geo %in% eu)

# PORTFELE - KREDYTY DLA GD ZABEZPIECZONE NA NIERUCHOMOŚCI
portfel_zabezp_rre_eur <- ecb::get_data("CBD2.Q..W0.67.S1M._Z.A.F.A1131._X.ALL.CA._Z.LE._T.EUR") %>% 
  select(c("ref_area", "obstime", "obsvalue")) %>% 
  rename(geo = "ref_area",
         time = "obstime",
         mort_loans = "obsvalue") %>% 
  filter(geo %in% eu)


# AKTYWA
aktywa_eur <- ecb::get_data("CBD2.Q..W0.67._Z._Z.A.A.A0000._X.ALL.CA._Z.LE._T.EUR")%>% 
  select(c("ref_area", "obstime", "obsvalue")) %>% 
  rename(geo = "ref_area",
         time = "obstime",
         assets = "obsvalue") %>% 
  filter(geo %in% eu)

# PKB I RACHUNKI NARODOWE
pkb <- eurostat::get_eurostat(id="namq_10_gdp", time_format = "num")

pkb_eur <- pkb%>% 
  filter(unit == "CP_MEUR" & na_item == "B1GQ" & s_adj == "NSA") %>% 
  select(-c("na_item", "unit", "s_adj")) %>% 
  arrange(geo, time) %>% 
  group_by(geo) %>% 
  mutate(gdp_eur_ann = RcppRoll::roll_sum(values, 4, align = "right", fill = NA),
         y = floor(time),
         q = case_when(time - floor(time) == 0.25 ~ "-Q2",
                       time - floor(time) == 0.50 ~ "-Q3",
                       time - floor(time) == 0.75 ~ "-Q4",
                       time - floor(time) == 0.00 ~ "-Q1"),
         data = paste(y, q, sep = "")) %>% 
  select(-c("y", "q", "time")) %>% 
  rename(time = data,
         gdp_eur = values) %>% 
  filter(geo %in% eu)

# PKB W WALUCIE KRAJOWEJ
pkb_lc <- pkb %>% 
  filter(unit == "CP_MNAC" & na_item == "B1GQ" & s_adj == "NSA") %>% 
  select(-c("na_item", "unit", "s_adj")) %>% 
  arrange(geo, time) %>% 
  group_by(geo) %>% 
  mutate(gdp_lc_ann = RcppRoll::roll_sum(values, 4, align = "right", fill = NA),
         y = floor(time),
         q = case_when(time - floor(time) == 0.25 ~ "-Q2",
                       time - floor(time) == 0.50 ~ "-Q3",
                       time - floor(time) == 0.75 ~ "-Q4",
                       time - floor(time) == 0.00 ~ "-Q1"),
         data = paste(y, q, sep="")) %>% 
  select(-c("y", "q", "time")) %>% 
  rename(time = data,
         gdp_lc = values) %>% 
  filter(geo %in% eu)

# pkb_lc <- eurostat::get_eurostat(id="naidq_10_gdp", time_format = "num")%>% 
#   filter(unit == "CP_MNAC" & na_item == "B1GQ" & s_adj == "NSA") %>% 
#   select(-c("na_item", "unit", "s_adj")) %>% 
#   arrange(geo, time) %>% 
#   group_by(geo) %>% 
#   mutate(gdp_lc_ann = RcppRoll::roll_sum(values, 4, align = "right", fill = NA),
#          y = floor(time),
#          q = case_when(time - floor(time) == 0.25 ~ "-Q2",
#                        time - floor(time) == 0.50 ~ "-Q3",
#                        time - floor(time) == 0.75 ~ "-Q4",
#                        time - floor(time) == 0.00 ~ "-Q1"),
#          data = paste(y, q, sep="")) %>% 
#   select(-c("y", "q", "time")) %>% 
#   rename(time = data,
#          gdp_lc = values)

# PŁACE (wages and salaries)
wages_lc <- pkb %>% 
  filter(unit == "CP_MNAC" & na_item == "D11" & s_adj == "NSA") %>% 
  select(-c("na_item", "unit", "s_adj")) %>% 
  arrange(geo, time) %>% 
  mutate(y = floor(time),
         q = case_when(time - floor(time) == 0.25 ~ "-Q2",
                       time - floor(time) == 0.50 ~ "-Q3",
                       time - floor(time) == 0.75 ~ "-Q4",
                       time - floor(time) == 0.00 ~ "-Q1"),
         data = paste(y, q, sep="")) %>% 
  select(-c("y", "q", "time")) %>% 
  rename(time = data, 
         wages = values) %>% 
  filter(geo %in% eu)

# NOWE I RENEGOCJOWANE KREDYTY W WALUCIE KRAJOWEJ
flow_lc <- ecb::get_data("MIR.M..B.A2C..B.A.2250..N") %>% 
  select(c("ref_area", "maturity_not_irate", starts_with("obs"))) %>% 
  pivot_wider(id_cols = c("ref_area", "obstime"), 
              names_from = "maturity_not_irate",
              values_from = "obsvalue")%>% 
  arrange(obstime) %>% 
  select(-c("K")) %>% 
  mutate(across(c("F", "I","O", "P"), ~replace_na(.x, 0)),
         suma = rowSums(across(c("F", "I","O", "P"))),
         flow_lc = ifelse(is.na(A), suma, A)) %>% 
  select(c("ref_area", "obstime", "flow_lc")) %>% 
  mutate(q=case_when(as.numeric(substr(obstime,6,7))<=3 ~ "-Q1",
                     as.numeric(substr(obstime,6,7))>3 & as.numeric(substr(obstime,6,7))<=6 ~ "-Q2",
                     as.numeric(substr(obstime,6,7))>6 & as.numeric(substr(obstime,6,7))<=9 ~ "-Q3",
                     as.numeric(substr(obstime,6,7))>9 & as.numeric(substr(obstime,6,7))<=12 ~ "-Q4"),
         y=substr(obstime, 1, 4),
         time = paste(y, q, sep="")) %>% 
  select(-c("y", "q", "obstime")) %>% 
  rename(geo = ref_area) %>% 
  group_by(geo, time) %>% 
  summarise(flow_lc = sum(flow_lc)) %>% 
  filter(geo %in% eu)

# NOWE KREDYTY - UWAGA: SĄ DUŻE BRAKI DANYCH, TRZEBA IMPUTOWAĆ
  # 1. tworzę kombinację wszystkich krajów UE (bez PL) i wszystkich dat (expand_grid)
  # 2. Dołączam oryginalne dane -> dla niektórych okresów pojawiają się NA
  # 3. Dołączam dane PL z naszej strony (bo u nas więcej danych niż w ECB SDW)
  # 4. Imputuję brakujące wartości

  # - UE
ecb_flow_eu_newonly <- ecb::get_data("MIR.M..B.A2C..B.A.2250..P") %>%
  select(c("ref_area", starts_with("obs")))%>%
  mutate(okres = lubridate::ceiling_date(lubridate::make_date(as.numeric(substr(obstime,1,4)),
                                      as.numeric(substr(obstime,6,7)),
                                      20), unit = "months")-1) %>%
  rename(geo = ref_area) %>% 
  filter(!is.na(obsvalue),
         geo %in% eu,
         !geo == "PL") %>% 
  select(-c("obstime"))

  # tworzy sekwencję dat miesięcznych od najstarszej do najnowszej (funkcja seq)
okres <- lubridate::ceiling_date(seq(as.Date(min(ecb_flow_eu_newonly$okres)), 
                                    as.Date(max(ecb_flow_eu_newonly$okres)), by = "months"))-1
geo <- unique(ecb_flow_eu_newonly$geo)

flow_eu_newonly <- expand_grid(okres, geo) %>% 
  mutate(okres = lubridate::as_date(if_else(lubridate::day(okres) < 20, okres -1, okres))) %>% 
  arrange(geo, okres)%>% 
  left_join(ecb_flow_eu_newonly, by = c("geo", "okres")) %>% 
  arrange(geo, okres)

  # - PL
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.nbp.pl/statystyka/pieniezna_i_bankowa/dwn/stopy_proc_pl_srdW.xlsx"
download.file(dataURL, destfile=temp, mode='wb')

zb <- readxl::read_excel(temp, sheet ="4 OPN2PLN", range = cell_rows(c(10, 14))) %>%
  filter(nr==2)
kol<-tail(as.vector(colnames(zb)),-5)

flow_pl_newonly<-gather(zb, kol,
                key = "okres", value = "obsvalue")

flow_pl_newonly$okres<-lubridate::ceiling_date(as.Date(as.numeric(flow_pl_newonly$okres), origin="1899-12-30"), 
                                       unit = "months") -1
flow_pl_newonly <-  mutate(flow_pl_newonly, geo = "PL") %>% 
  select(c("geo", "okres", "obsvalue"))

flow_lc_newonly <- bind_rows(flow_eu_newonly, flow_pl_newonly) %>% 
  group_by(geo) %>% 
  mutate(obsvalue_imp = imputeTS::na_interpolation(obsvalue),
         time = paste(lubridate::year(okres),
                      "-Q",
                      lubridate::quarter(okres, type = "quarter"),
                      sep = "")) %>% 
  group_by(time, geo) %>% 
  summarise(pure_flow_lc_imp = sum(obsvalue_imp))

rm(ecb_flow_eu_newonly, flow_eu_newonly, flow_pl_newonly)


# OPROCENTOWANIE KREDYTÓW - NOWE I RENEGOCJOWANE
ir <- ecb::get_data("MIR.M..B.A2C..R.A.2250..N") %>%
  select(c("ref_area", "maturity_not_irate", starts_with("obs"))) %>%
  filter(as.numeric(substr(obstime,1,4))>2017 & maturity_not_irate %in% c("A", "F", "I", "O", "P"))%>%
  pivot_wider(id_cols = c("ref_area", "obstime"),
              names_from = "maturity_not_irate",
              values_from = "obsvalue") %>%
  arrange(ref_area, obstime)

wagi <- ecb::get_data("MIR.M..B.A2C..B.A.2250..N") %>%
  select(c("ref_area", "maturity_not_irate", starts_with("obs"))) %>%
  pivot_wider(id_cols = c("ref_area", "obstime"),
              names_from = "maturity_not_irate",
              values_from = "obsvalue")%>%
  arrange(obstime) %>%
  select(-c("K")) %>%
  mutate(across(c("F", "I","O", "P"), ~replace_na(.x, 0)),
         suma_FIOP = rowSums(across(c("F", "I","O", "P"))),
         razem = ifelse(is.na(A), suma_FIOP, A)) %>%
  mutate(waga_F = F/razem,
         waga_I = I/razem,
         waga_O = O/razem,
         waga_P = P/razem) %>%
  select(c("ref_area", "obstime", starts_with("waga_")))

stopy_UE <- ir %>%
  left_join(wagi, by = c("ref_area", "obstime")) %>%
  mutate(wazona_srednia = F*waga_F + I*waga_I + O*waga_O + P*waga_P,
         flaga_obliczone =  ifelse(is.na(A), 1, 0),
         oprocentowanie = ifelse(is.na(A), wazona_srednia, A)) %>%
  select(c("ref_area", "obstime", "oprocentowanie", "waga_F", "flaga_obliczone")) %>%
  rename(udz_stopy_do_1Y = waga_F,
         geo = ref_area)  %>% 
  filter(geo %in% eu) %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

# OPROCENTOWANIE KREDYTÓW - TYLKO NOWE

stopy_UE_newonly <- ecb::get_data("MIR.M..B.A2C..R.A.2250..P") %>% 
  select(c("ref_area", starts_with("obs"))) %>% 
  filter(!is.na(obsvalue)) %>% 
  rename(geo = ref_area,
         oprocentowanie = obsvalue) %>%
  filter(geo %in% eu)  %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

# DANE EBA (UWAGA - DANE Z DYSKU P, PRZY AKTUALIZAJI TRZEBA ŚCIĄGNĄĆ NOWY PLIK ZE STRONY EBA)
eba_names <- readxl::read_excel("P:/WPM/Real Estate/International/Dane EBA/EBA Interactive Dashboard - Q3 2021 - Protected.xlsm",
                                sheet = "Mapping", 
                                skip = 1,
                                col_names = TRUE)%>% 
  rename(indic = Label) %>% 
  select(-c("Sheet"))

eba <- readxl::read_excel("P:/WPM/Real Estate/International/Dane EBA/EBA Interactive Dashboard - Q3 2021 - Protected.xlsm",
                          sheet = "Data Annex")%>% 
  rename(geo = NSA,
         indic = lbl) %>% 
  select(c("geo", "indic", starts_with("20"))) %>% 
  left_join(eba_names, by = c("indic")) %>% 
  mutate(across(starts_with("20"), as.character))%>% 
  pivot_longer(cols = starts_with("20"), names_to = "okres", values_to = "value") %>% 
  mutate(value = as.numeric(value),
         q=case_when(as.numeric(substr(okres,5,6))<=3 ~ "-Q1",
                     as.numeric(substr(okres,5,6))>3 & as.numeric(substr(okres,5,6))<=6 ~ "-Q2",
                     as.numeric(substr(okres,5,6))>6 & as.numeric(substr(okres,5,6))<=9 ~ "-Q3",
                     as.numeric(substr(okres,5,6))>9 & as.numeric(substr(okres,5,6))<=12 ~ "-Q4"),
         time = paste(substr(okres,1,4), q)) %>% 
  select(-c("q", "okres")) %>% 
  filter(geo %in% eu)

rm(ir, wagi, eba_names)


##### WSKAZNIKI

wsk_kred_PKB <- portfel_rre_eur %>% 
  left_join(pkb_eur, by = c("geo", "time")) %>% 
  mutate(kredyty_PKB = rre_loans/(gdp_eur_ann*1e3) * 100) %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

wsk_kred_aktywa <- portfel_rre_eur %>% 
  left_join(aktywa_eur, by = c("geo", "time")) %>% 
  mutate(kredyty_aktywa = rre_loans/assets * 100) %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

wsk_flow_pkb_z_renegoc <- flow_lc %>% 
  filter(flow_lc > 0) %>% 
  left_join(pkb_lc, by = c("geo", "time")) %>% 
  mutate(flow_pkb = flow_lc/gdp_lc_ann * 100) %>% 
  filter(flow_pkb > 0 & geo !="U2") %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

wsk_flow_pkb_pure <- flow_lc_newonly %>% 
  filter(pure_flow_lc_imp > 0) %>% 
  left_join(pkb_lc, by = c("geo", "time")) %>% 
  mutate(flow_pkb = pure_flow_lc_imp/gdp_lc_ann * 100) %>% 
  filter(flow_pkb > 0 & as.numeric(substr(time, 1, 4) > 2017) & geo !="U2") %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

wsk_flow_wages <-  flow_lc_newonly %>% 
  filter(pure_flow_lc_imp > 0) %>% 
  left_join(wages_lc, by = c("geo", "time")) %>% 
  mutate(flow_wages = pure_flow_lc_imp/wages * 100) %>% 
  filter(flow_wages > 0 & as.numeric(substr(time, 1, 4) > 2017) & geo !="U2")%>% 
  mutate(grupa = stworz_grupe_krajow(geo))

wsk_npl <- eba %>% 
  filter(indic == "T22_3" & as.numeric(substr(time, 1, 4) > 2018))%>%
  mutate(npl = value * 100) %>% 
  select(c("geo", "time", "npl")) %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

wsk_renegoc <- flow_lc_newonly %>% 
  left_join(flow_lc, by = c("geo", "time")) %>% 
  mutate(udz_reneg = (flow_lc-pure_flow_lc_imp)/flow_lc*100) %>% 
  arrange(geo, time) %>% 
  filter(udz_reneg>0) %>% 
  mutate(grupa = stworz_grupe_krajow(geo))

# do_sprawdzenia_wsk_zabezp_celowe <- inner_join(portfel_rre_eur, portfel_zabezp_rre_eur, by = c("geo", "time"))%>% 
  # mutate(zabezp_celowe = mort_loans/rre_loans*100) %>% 
  # mutate(grupa = stworz_grupe_krajow(geo))
# DO SPRAWDZENIA - PORÓWNAĆ Z FINREP DLA PL

# test <- wsk_zabezp_celowe %>% filter(geo=="PL")


##### WYKRESY

destination <-  "P:/WPM/Real Estate/International/rre_intl_plots.pdf"
pdf(file = destination)

# Flow/PKB (z renegocjowanymi)

ggplot(filter(wsk_flow_pkb_z_renegoc, time == max(wsk_flow_wages$time)), 
       aes(x = reorder(geo, -flow_pkb), 
           y = flow_pkb,
           fill = grupa)
       ) +
  geom_bar(stat = "identity") + 
  scale_fill_manual("grupa", values=c("PL"="#e50040", "EA"="#2f578c", "non-EA"="#6f6f6f"))+
  scale_y_continuous(expand = expansion(mult = c(0, NA)))+
  labs(fill="grupa")+
  # theme(legend.position = "none" )+
  xlab("Kraj") + 
  ylab("Nowe i renegocjowane kredyty do PKB (%)")+
  labs(caption = "Nowe i renegocjowane")+
  theme(plot.caption = element_text(size = 6))+
  ggtitle(unique(filter(wsk_flow_pkb_z_renegoc, time == max(wsk_flow_wages$time))$time))



flow_do_wstegi <- wsk_flow_pkb_z_renegoc %>% 
  group_by(time) %>% 
  summarise(flow_min = min(flow_pkb), flow_max = max(flow_pkb)) %>% 
  left_join(filter(wsk_flow_pkb_z_renegoc, geo == "PL"), by = c("time")) %>% 
  rename(PL = flow_pkb) %>% 
  filter(as.numeric(substr(time, 1, 4)) > 2010) %>% 
  select(c("time", starts_with("flow_m"), "PL")) %>% 
  mutate(okres = stworz_date(time))


ggplot(flow_do_wstegi, aes(okres)) +
  geom_ribbon(aes(ymin = flow_min, ymax = flow_max), fill = "#c8c8c8") +
  geom_line(aes(y = PL), colour = "#2f578c")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Nowe i renegocjowane kredyty do PKB (%)")

# Flow/PKB (tylko nowe)

ggplot(filter(wsk_flow_pkb_pure, time == max(wsk_flow_wages$time)), 
       aes(x = reorder(geo, -flow_pkb), 
           y = flow_pkb, 
           fill = grupa)
       ) +
  # scale_fill_manual(name = "geo", values=c("#e50040","#2f578c"))+
  geom_bar(stat = "identity") + 
  scale_fill_manual("grupa", values=c("PL"="#e50040", "EA"="#2f578c", "non-EA"="#ffcc00"))+
  scale_y_continuous(expand = expansion(mult = c(0, NA)))+
  labs(fill="grupa")+
  xlab("Kraj") + 
  ylab("Nowe kredyty do PKB (%)")+
  labs(caption = "Nowe kredyty")+
  theme(plot.caption = element_text(size = 6))+
  ggtitle(unique(filter(wsk_flow_pkb_pure, time == max(wsk_flow_wages$time))$time))



flow_do_wstegi_pure <- wsk_flow_pkb_pure %>% 
  group_by(time) %>% 
  summarise(flow_min = min(flow_pkb), flow_max = max(flow_pkb)) %>% 
  left_join(filter(wsk_flow_pkb_pure, geo == "PL"), by = c("time")) %>% 
  rename(PL = flow_pkb) %>% 
  filter(as.numeric(substr(time, 1, 4)) > 2010) %>% 
  select(c("time", starts_with("flow_m"), "PL")) %>% 
  mutate(okres = stworz_date(time))


ggplot(flow_do_wstegi_pure, aes(okres)) +
  geom_ribbon(aes(ymin = flow_min, ymax = flow_max, fill = "UE")) +
  geom_line(aes(y = PL, colour = "PL"))+
  # theme_minimal()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Nowe kredyty do PKB (%)\n")+
  scale_x_date(expand = expansion(mult = c(0, NA)))+
  scale_colour_manual("",values="#e50040")+
  scale_fill_manual("",values="#c8c8c8")


# w2 / w1

# Flow do płac (nowe)
ggplot(filter(wsk_flow_wages, time == max(wsk_flow_wages$time)), 
       aes(x = reorder(geo, -flow_wages), 
           y = flow_wages, 
           fill = grupa)
) +
  # scale_fill_manual(name = "geo", values=c("#e50040","#2f578c"))+
  geom_bar(stat = "identity") + 
  scale_fill_manual("grupa", values=c("PL"="#e50040", "EA"="#2f578c", "non-EA"="#ffcc00"))+
  scale_y_continuous(expand = expansion(mult = c(0, NA)))+
  labs(fill="grupa")+
  xlab("Kraj") + 
  ylab("Nowe kredyty do płac (%)")+
  labs(caption = "Nowe kredyty")+
  theme(plot.caption = element_text(size = 6))+
  ggtitle(unique(filter(wsk_flow_wages, time == max(wsk_flow_wages$time))$time))

flow_do_wstegi_wages <- wsk_flow_wages %>% 
  group_by(time) %>% 
  summarise(flow_min = min(flow_wages), flow_max = max(flow_wages)) %>% 
  left_join(filter(wsk_flow_wages, geo == "PL"), by = c("time")) %>% 
  rename(PL = flow_wages) %>% 
  filter(as.numeric(substr(time, 1, 4)) > 2010) %>% 
  select(c("time", starts_with("flow_m"), "PL")) %>% 
  mutate(okres = stworz_date(time))


ggplot(flow_do_wstegi_wages, aes(okres)) +
  geom_ribbon(aes(ymin = flow_min, ymax = flow_max, fill = "UE")) +
  geom_line(aes(y = PL, colour = "PL"))+
  # theme_minimal()+
  # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylab("Nowe kredyty do płac (%)\n")+
  scale_x_date(expand = expansion(mult = c(0, NA)))+
  scale_colour_manual("",values="#2f578c")+
  scale_fill_manual("",values="#c8c8c8")

# oprocentowanie
ggplot(filter(stopy_UE_newonly, obstime == max(stopy_UE_newonly$obstime) & !geo == "U2"), 
       aes(x = reorder(geo, -oprocentowanie), 
           y = oprocentowanie, 
           fill = grupa)
       ) +
  scale_fill_manual("grupa", values=c("PL"="#e50040", "EA"="#2f578c", "non-EA"="#ffcc00"))+
  scale_y_continuous(expand = expansion(mult = c(0, NA)))+
  geom_bar(stat = "identity") + 
  # theme(legend.position = "none" )+
  xlab("Kraj") + 
  ylab("Oprocentowanie nowych kredytów (%)")+
  ggtitle(max(stopy_UE_newonly$obstime))

ggplot(filter(stopy_UE, obstime == max(stopy_UE$obstime) & !geo == "U2"), 
       aes(x=udz_stopy_do_1Y, y=oprocentowanie)) + 
  geom_point()+
  ggtitle( max(stopy_UE$obstime))

# npl vs stopy

npl_stopy <- wsk_npl %>% 
  group_by(geo) %>% 
  summarise(max_npl = max(npl)) %>% 
  left_join(filter(stopy_UE_newonly, obstime == max(stopy_UE_newonly$obstime) & grupa == "EA"), by = c("geo")) %>% 
  filter(!is.na(oprocentowanie) & max_npl < 20)

ggplot(npl_stopy, aes(x=max_npl, y=oprocentowanie)) + 
  geom_point(color="#2f578c",
             fill="#2f578c",
             shape=19) +
  labs(caption = "Kraje EA")+
  xlab ("Maksymalny poziom NPL w latach 2019-2021 (%)")+
  # ylab("Oprocentowanie we wrześniu 2021 r.(%)")+
  geom_smooth(method='lm', formula= y~x, se = FALSE, colour = "#2f578c")

# Poziom npl

ggplot(filter(wsk_npl, time == max(wsk_npl$time)), 
       aes(x = reorder(geo, -npl), 
           y = npl, 
           fill = ifelse(geo == "PL", "Highlited","Normal")
           )
       ) +
  scale_fill_manual(name = "geo", values=c("#e50040","#2f578c"))+
  # scale_y_continuous(expand = expansion(mult = c(0, NA)))+
  geom_bar(stat = "identity") + 
  theme(legend.position = "none" )+
  xlab("Kraj") + 
  ylab(paste("Poziom NPL", max(wsk_npl$time), "(%)"))

# scatter portfele

scatter_portfele <- filter(wsk_kred_aktywa[,c(1,2,5)], time == max(wsk_kred_aktywa$time)) %>%
  left_join(wsk_kred_PKB[,c(1,2,6)], by = c("time","geo")) %>% 
  filter(!is.na(kredyty_PKB) & !is.na(kredyty_aktywa)) %>% 
  mutate(grupa=case_when(geo=="PL"~ 1, TRUE ~ 2))



ggplot(scatter_portfele, aes(x=kredyty_aktywa, y=kredyty_PKB, color = as.factor(grupa))) + 
  geom_point(shape = 18,
             size = 5) +
  scale_color_manual(values = c("#e50040", "#2f578c")) +
  theme(legend.position = "none") +
  geom_text_repel(aes(label = geo)) + 
  xlab ("Kredyty do aktywów (%)") +
  ylab("Kredyty do PKB (%)") +
  ggtitle(unique(filter(scatter_portfele, time == max(scatter_portfele$time))$time))

# Relacja zabezpieczonych do celowych
# ggplot(filter(wsk_zabezp_celowe, time == max(wsk_zabezp_celowe$time) & !geo == "U2"), 
#        aes(x = reorder(geo, -zabezp_celowe), 
#            y = zabezp_celowe, 
#            fill = grupa)
#       ) +
#   scale_fill_manual("grupa", values=c("PL"="#e50040", "EA"="#2f578c", "non-EA"="#ffcc00"))+
#   scale_y_continuous(expand = expansion(mult = c(0, NA)))+
#   geom_bar(stat = "identity") + 
#   # theme(legend.position = "none" )+
#   xlab("Kraj") + 
#   ylab("Relacja kredytów zabezpieczonych do celowych (%)") +
#   ggtitle(unique(filter(wsk_zabezp_celowe, time == max(wsk_zabezp_celowe$time))$time))

# Udział kredytów renegocjowanych
ggplot(filter(wsk_renegoc, time == max(wsk_renegoc$time) & !geo == "U2"), 
       aes(x = reorder(geo, -udz_reneg), 
           y = udz_reneg, 
           fill = grupa)
      ) +
  scale_fill_manual("grupa", values=c("PL"="#e50040", "EA"="#2f578c", "non-EA"="#ffcc00"))+
  scale_y_continuous(expand = expansion(mult = c(0, NA)))+
  geom_bar(stat = "identity") + 
  # theme(legend.position = "none" )+
  xlab("Kraj") + 
  ylab("Udział renegocjowanych (%)")+
  ggtitle(max(wsk_renegoc$time))

# grid.arrange(ls(pattern = "^w\\d"), ncol = 2)
# grid.arrange(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, ncol = 2)


dev.off() 


# Udział kredytów na zmienną stopę (raport EMF)
# source("P:/WPM/Real Estate/International/Dane EMF/read_EMF_report.R")