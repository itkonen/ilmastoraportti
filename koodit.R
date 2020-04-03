## @knitr suomikhk01

pxweb_query_list <- 
    list("Kasvihuonekaasu"=c("00"),
         "Päästöluokka"=c("1","2","3","4","5"),
         "Tiedot"=c("emission"),         "Vuosi"=c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
d <- 
    pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/ymp/khki/statfin_khki_pxt_111k.px",
              query = pxweb_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
    as_tibble() %>%
    rename(values = `Päästö, tuhatta tonnia CO2-ekv.`) %>%
    mutate(Vuosi = as.integer(as.character(Vuosi)),
           Päästöluokka = str_sub(Päästöluokka, 3)) %>%
    mutate(Päästöluokka_f = factor(Päästöluokka, unique(Päästöluokka)),
           Päästöluokka = str_wrap(Päästöluokka_f, width = 30))

ds <-
    d %>%
    group_by(Vuosi) %>%
    summarise(Nettopäästöt = sum(values))

ggplot() +
    geom_col(aes(x = Vuosi,
                 y = values/1000,
                 fill = Päästöluokka),
             data = d) +
    geom_line(aes(x = Vuosi,
                  y = Nettopäästöt/1000,
                  color = "Nettopäästöt"),
              data = ds) +
    geom_hline(yintercept = 0, size = 0.3) +
    scale_color_manual(name = "", values = "black") +
    scale_fill_brewer(palette="Set2") +
    scale_y_continuous(breaks = seq(-25, 100, 25)) +
    scale_x_continuous(breaks = seq(1990, 2020, 5),
                       expand = expansion(mult = c(0,0), add = c(0,1))) +
    theme_bw() +
    labs(title = "Kasvihuonekaasupäästöt ja nielut Suomessa",
         subtitle = "Miljoonaa tonnia CO2-ekv.",
         fill = "Päästöluokka",
         y = "", x = "", caption = "Lähde: Tilastokeskus.")

## @knitr suomikhk02

pxweb_query_list <- 
    list("Kasvihuonekaasu"=c("00"),         "Päästöluokka"=c("1A1","1A2","1A3","1A4a","1A4b","1A4c","1A5","1B","2A","2B","2C","2D","2F","2G","2H","3A","3B","3D","3F","3G","3H","5A","5B","5D"),
         "Tiedot"=c("emission"),
         "Vuosi"=c("2018"))
d <- 
    pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/ymp/khki/statfin_khki_pxt_111k.px",
              query = pxweb_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
    as_tibble() %>%
    rename(values = `Päästö, tuhatta tonnia CO2-ekv.`) %>% 
    mutate(Vuosi = as.integer(as.character(Vuosi)),
           Päästöluokka = str_trim(Päästöluokka),
           Koodi = str_extract(Päästöluokka, "^[^\\s]*"),
           Pääkoodi = str_sub(Koodi, 1, 1),
           Päästöluokka = str_extract(Päästöluokka, "(?<=\\s).*"))

comma <- function(x) str_trim(format(round(x,1), nsmall = 1, decimal.mark = ",", scientific = FALSE))

pääluokat <-
    tribble(
        ~Pääkoodi, ~Pääluokka,
        "1", "Energiasektori",
        "2", "Teollisuusprosessit ja tuotteiden käyttö",
        "3", "Maatalous",
        "4", "Maankäyttö, maankäytön muutokset ja metsätalous (LULUCF)",
        "5", "Jätteiden käsittely")
 
d %>%
    full_join(pääluokat, by = "Pääkoodi") %>%
    filter(Pääkoodi != Koodi) %>% 
    mutate(osuus = comma(100*values/sum(values)),
           lab = str_c(Päästöluokka, "\n", comma(values/1000), "\u00A0Mt\n", osuus, "\u00A0%")) %>%
    ggplot(aes(area = values,
               fill = Pääluokka,
               subgroup = Pääluokka,
               label = lab)) +
    guides(fill = "none") +
    geom_treemap(colour = "white", size = 1) +
    geom_treemap_subgroup_border(colour = "black", size = 1) +
    geom_treemap_text(reflow = T) +
    scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#E78AC3", "#A6D854")) +
    labs(title = "Suomen kasvihuonekaasupäästöt vuonna 2018",
         subtitle = expression(Miljoonaa~tonnia~CO[2]-ekv.),
         caption = "Lähde: Tilastokeskus.")


## @knitr edgar

if (!file.exists("edgar50.xls")) {
  ##  download.file("http://edgar.jrc.ec.europa.eu/booklet2018/EDGARv5.0_FT2017_fossil_CO2_booklet2018.xls", "edgar50.xls")
  download.file("https://edgar.jrc.ec.europa.eu/booklet2019/EDGARv5.0_FT2018_fossil_CO2_GHG_booklet2019.xls", "edgar50.xls")
}

## Yhdistä tiedot välilehdiltä (huom. GDP ja väliluku saatavilla vasta 1990 alkaen)
edgar50 <-
    readxl::read_excel("edgar50.xls", sheet = 2) %>%
    drop_na(country_name) %>%
    gather("year", "CO2", -country_name) %>%
    left_join(
        readxl::read_excel("edgar50.xls", sheet = 5) %>%
        gather("year", "CO2 per GDP", -country_name), 
        by = c("country_name", "year")) %>%
    left_join(
        readxl::read_excel("edgar50.xls", sheet = 4) %>%
        gather("year", "CO2 per capita", -country_name),
        by = c("country_name", "year")) 


maanimien_alaraja <- 100
muut_maat <- c("Finland", "Sweden", "Norway", "Denmark", "Iceland")
d <-
    edgar50 %>%
    mutate(capita = CO2/`CO2 per capita`,
           Elintaso = CO2 / `CO2 per GDP` / capita,
           Tuloluokka = cut(Elintaso,
                            c(0, 5, 10, 20, Inf),
                            c("Alle 5",
                              "5–10",
                              "10–20",
                              "Yli 20"))) %>%
    filter(year == 2018,
           !(country_name %in% c("GLOBAL TOTAL", "EU28"))) %>%
    arrange(desc(`CO2 per capita`)) %>%
    mutate(country_name = fct_inorder(country_name),
           country_name = fct_recode(country_name,
                                     Israel = "Israel and Palestine, State of",
                                     Italy = "Italy, San Marino and the Holy See",
                                     France = "France and Monaco",
                                     Spain = "Spain and Andorra",
                                     Switzerland = "Switzerland and Liechtenstein"),
           maat = countrycode::
               countrycode(as.character(country_name),
                           'country.name', 'cldr.short.fi', nomatch = "Käännösvirhe"),
           maat = fct_recode(maat,
                             `Kongon dem. tasav.` = "Kongon demokraattinen tasavalta"),
           lab = if_else(CO2 > maanimien_alaraja  | country_name %in% muut_maat | capita > 50,
                               as.character(maat), ""),
           cum = cumsum(capita),
           xmin = lag(cum, default = 0),
           x = (cum+xmin)/2) %>%
    drop_na()

ggplot(d, aes(xmin = xmin, xmax = cum,
              ymin = 0, ymax = `CO2 per capita`,
              x = x, y = 0, #group = year,
              fill = Tuloluokka,
              label = lab)) +
    theme_bw() +
    geom_rect(show.legend = TRUE, color = rgb(0,0,0,0.0), size = 0.0) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim=c(-8, 21)) +
    scale_x_continuous(breaks = seq(0, 7000, 1000), expand = c(0,0)) +
    scale_fill_brewer(palette = "Spectral") +
    ggrepel::geom_text_repel(size = 3.3,
                             force = 0.1,
                             vjust = 1,
                             nudge_y = -2,
                             angle = 90,
                             direction = "x",
                             box.padding = 0.1,
                             xlim = c(0,8000),
                             segment.alpha = 0.3) +
    theme(legend.position = c(.9, .75)) +
    labs(title = "Hiilidioksidipäästöt fossiilisista polttoaineista henkeä kohti ja kumulatiivinen väkiluku maittain",
         subtitle = "Tonnia henkeä kohti, vuonna 2018",
         fill = "BKT henkeä kohti\nTuhatta dollaria vuodessa",
         y = "",
         x = "Kumulatiivinen väkiluku, miljoonaa henkeä",
         caption = paste("Kuviossa on nimetty Pohjoismaat sekä maat, jotka tuottavat yli", maanimien_alaraja, "Gt päästöjä tai joiden väkiluku on yli 50 miljoonaa henkeä.\nLähde: EDGAR v5.0."))

## @knitr subsidies

if (!file.exists("subsidies.xlsx")) {
  ## download.file("https://www.iea.org/media/publications/weo/Subsidies%202015-2017.xlsx", "subsidies.xlsx")
  download.file("https://iea.blob.core.windows.net/assets/6c446601-d4a6-40b1-b6dc-801210da62c5/IEA-Subsidies-2010-18.xlsx", "subsidies.xlsx")
}

d <-
  readxl::read_excel("subsidies.xlsx", sheet = 1, skip = 10) %>%
  gather(key = "Year", value = "value", -(1:2)) %>%
  mutate(Country = countrycode::countrycode(as.character(Country),
                                            'country.name',
                                            'cldr.short.fi',
                                            nomatch = "Käännösvirhe"),
         Product = fct_recode(Product,
                              Öljy = "Oil", Sähkö = "Electricity", Kaasu = "Gas", Kivihiili = "Coal"))

top10 <-
    d %>%
    filter(Product == "Total",
             Year == max(Year)) %>%
    top_n(10, value) %>%
    arrange(value) %>%
    pull(Country)


d %>%
    filter(Country %in% top10,
           Product != "Total",
           Year == max(Year),
           value != 0) %>%
    mutate(Country = fct_relevel(Country, top10)) %>%
    ggplot(aes(x = Country, y = value/1000, fill = Product)) +
    geom_col() +
    coord_flip() +
    scale_fill_brewer(palette="Set2") +
    theme_bw() +
    labs(y = "Miljardia dollaria vuodessa", fill = "Tuen kohde", x = "",
         title = paste("Tuet fossiilisille polttoaineille maittain vuonna", max(d$Year)),
         subtitle = "10 eniten tukia myöntävää maata",
         caption = "Lähde: OECD/IEA.")



