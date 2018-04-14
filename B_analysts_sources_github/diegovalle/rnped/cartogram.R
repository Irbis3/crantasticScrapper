
states_c <- rnpe_c %>%
  filter(date >= as.Date("2014-01-12") & date <= as.Date("2014-12-31")) %>%
  group_by(fuerocomun_desapentidad) %>%
  summarise(count_c = n()) %>%
  rename(ent = fuerocomun_desapentidad)


states_f <- rnpe_f %>%
  filter(date >= as.Date("2014-01-01") & date <= as.Date("2014-12-31")) %>%
  group_by(fuerofederal_ultimaentidad) %>%
  summarise(count_f= n()) %>%
  rename(ent = fuerofederal_ultimaentidad)

states <- full_join(states_c, states_f)
states[is.na(states)] <- 0
states$count <- states$count_c + states$count_f

last_date_states <- structure(list(state = c("AGUASCALIENTES", "BAJA CALIFORNIA", 
                                             "BAJA CALIFORNIA SUR", "CAMPECHE", "CHIAPAS", "CHIHUAHUA", "COAHUILA", 
                                             "COLIMA", "DISTRITO FEDERAL", "DURANGO", "GUANAJUATO", "GUERRERO", 
                                             "HIDALGO", "JALISCO", "MEXICO", "MICHOACAN", "MORELOS", "NAYARIT", 
                                             "NUEVO LEON", "OAXACA", "PUEBLA", "QUERETARO", "QUINTANA ROO", 
                                             "SAN LUIS POTOSI", "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS", 
                                             "TLAXCALA", "VERACRUZ", "YUCATAN", "ZACATECAS"), population = c(1281831L, 
                                                                                                             3467081L, 756298L, 903297L, 5230729L, 3697867L, 2948985L, 719297L, 
                                                                                                             8861308L, 1758752L, 5801584L, 3560996L, 2866507L, 7900181L, 16786568L, 
                                                                                                             4585616L, 1912698L, 1216265L, 5061762L, 4003599L, 6173057L, 1994460L, 
                                                                                                             1559842L, 2745055L, 2975944L, 2919369L, 2375748L, 3529818L, 1272415L, 
                                                                                                             8026516L, 2109679L, 1571820L)), .Names = c("state", "population"
                                                                                                             ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                                                                                                                                        -32L))

states <- merge(states, last_date_states, by.x = "ent", by.y = "state", all.y = TRUE)
states$rate <- states$count / states$population * 10^5
states[is.na(states)] <- 0

statebins_continuous(states, "ent", "rate",
                     brewer_pal="YlOrRd",
                     legend_title= "Annual Disappearance Rate",
                     plot_title = "2014 DISAPPEARANCE RATE\n(FEDERAL AND LOCAL)",
                     title_position = "top") +
  infographic_theme2() +
  theme_bare()
ggsave("graphs/cartogram.png", width = 4.98, height = 6, dpi = 100)
# write.csv(df, "rnpe.csv", row.names=FALSE)
