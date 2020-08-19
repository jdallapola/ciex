# Lista de pessoas com ações desenvolvidas em Cuba

# Critérios: Não ser "Unidentified by CIEX", ter ano de ação conhecido, estar entre 1964 e 1979, 

countries_df = read.csv("https://raw.githubusercontent.com/jdallapola/ciex/master/scripts/GeoMap%20Timeline/export_data_geomap_timeline.csv", encoding = "UTF-8")
nrow(countries_df)

b4_Amnesty = filter(countries_df, date <= "1979-08-15" & date  >= "1964-03-31")
a_Amnesty = filter(countries_df, date  >= "1979-01-01")

#82% das ações identificadas ocorreram entre o Golpe Militar e a aprovação da Lei da Anistia
nrow(b4_Amnesty)/nrow(countries_df)
nrow(a_Amnesty)/nrow(countries_df)    

cuba = filter(b4_Amnesty, iso3 == "CUB")

cuba_name_list = count(cuba$stndr_name)

#run network_ties.R script before to create MIL_ties df
MIL_names = data.frame("stndr_name" = unique(MIL_ties$stndr_name))
check = purrr::map_df(MIL_names, ~ .x %in% cuba_name_list$x)
MIL_cub = mutate(MIL_names, "check" = check$stndr_name)%>%
  filter(check == TRUE)

write.csv(cuba_name_list, "./cuba_name_list.csv")
write.csv(MIL_cub, "./MIL_cub.csv")


test = filter(main_df, stndr_name %in% sel_names)%>%
              select(stndr_name, cntry_action, date_action, ties, ties_ids, year,birth, n_doc)
test
count(test$stndr_name)



sel_names = c("Aderval Alves Coqueiro",
              "Aluizio Palhano",
              "Antônio Benetazzo",
              "Ailton Adalberto Mortari",
              "Boanerges de Souza Massa",
              "Carlos Eduardo Pires Fleury",
              "Edson Neves Quaresma",
              "Flávio Carvalho Molina",
              "Francisco José de Oliveira",
              "Frederico Eduardo Mayr",
              "Gastone Lucia de Carvalho Beltrão",
              "João Carlos Cavalcanti Reis",
              "João Leonardo da Silva Rocha",
              "Joaquim Câmara Ferreira",
              "Luiz Almeida Araújo",
              "Luiz Inácio Maranhão Filho",
              "Luiz José da Cunha",
              "Marcio Beck Machado",
              "Marco Antonio da Silva Lima",
              "Maria Augusta Thomaz",
              "Onofre Pinto",
              "Jeová Assis Gomes",
              "Marco Antonio Braz de Carvalho",
              "Izis Dias de Oliveira",
              "Rui Carlos Vieira Berbert",
              "Soledad Barret Viedma",
              "Virgílio Gomes da Silva",
              "Luiz Eurico Tejera Lisboa")


