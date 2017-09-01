# Question 1.
q1pseudo.name = "q1pseudo"
q1colNames = colNames = c("Alt.1a", "Alt.1b", "Alt.1c", "Alt.1d", "Alt.1e", q1pseudo.name)
q1aNames = c("q1a1", "q1a2", "q1a3", "q1a4", "q1a5")
q1question.name = "q1a"
q1criterion.name = "wc1.car"

# Question 2.
q2pseudo.name = "q2pseudo"
q2colNames = colNames = c("Alt.2a", "Alt.2b", "Alt.2c", "Alt.2d", "Alt.2e", q2pseudo.name)
q2aNames = c("q2a1", "q2a2", "q2a3", "q2a4", "q2a5")
q2question.name = "q2a"
q2criterion.name = "wc2.car"

# Question 3.
q3pseudo.name = "q3pseudo"
q3colNames = colNames = c("Alt.3a", "Alt.3b", "Alt.3c", "Alt.3d", "Alt.3e", q3pseudo.name)
q3aNames = c("q3a1", "q3a2", "q3a3", "q3a4", "q3a5")
q3question.name = "q3a"
q3criterion.name = "wc3.car"

# Question 4.
q4pseudo.name = "q4pseudo"
q4colNames = colNames = c("Alt.4a", "Alt.4b", "Alt.4c", "Alt.4d", "Alt.4e", q4pseudo.name)
q4aNames = c("q4a1", "q4a2", "q4a3", "q4a4", "q4a5")
q4question.name = "q4a"
q4criterion.name = "wc4.car"

# Question 5.
q5pseudo.name = "q5pseudo"
q5colNames = colNames = c("Alt.5a", "Alt.5b", "Alt.5c", "Alt.5d", "Alt.5e", q5pseudo.name)
q5aNames = c("q5a1", "q5a2", "q5a3", "q5a4", "q5a5")
q5question.name = "q5a"
q5criterion.name = "wc5.car"

# Question 6.
q6pseudo.name = "q6pseudo"
q6colNames = colNames = c("Alt.6a", "Alt.6b", "Alt.6c", "Alt.6d", "Alt.6e", q6pseudo.name)
q6aNames = c("q6a1", "q6a2", "q6a3", "q6a4", "q6a5")
q6question.name = "q6a"
q6criterion.name = "wc6.car"

# Question 7.
q7pseudo.name = "q7pseudo"
q7colNames = colNames = c("Alt.7a", "Alt.7b", "Alt.7c", "Alt.7d", "Alt.7e", q7pseudo.name)
q7aNames = c("q7a1", "q7a2", "q7a3", "q7a4", "q7a5")
q7question.name = "q7a"
q7criterion.name = "wc7.car"

# Question 8.
q8pseudo.name = "q8pseudo"
q8colNames = colNames = c("Alt.8a", "Alt.8b", "Alt.8c", "Alt.8d", "Alt.8e", q8pseudo.name)
q8aNames = c("q8a1", "q8a2", "q8a3", "q8a4", "q8a5")
q8question.name = "q8a"
q8criterion.name = "wc8.car"

q9pseudo.name = "q9pseudo"
q9colNames = colNames = c("Alt.9a", "Alt.9b", "Alt.9c", "Alt.9d", "Alt.9e", q9pseudo.name)
q9aNames = c("q9a1", "q9a2", "q9a3", "q9a4", "q9a5")
q9question.name = "q9a"
q9criterion.name = "wc9.car"

# Question 10.
q0pseudo.name = "q0pseudo"
q0colNames = colNames = c("Alt.10a", "Alt.10b", "Alt.10c", "Alt.10d", "Alt.10e", q0pseudo.name)
q0aNames = c("q0a1", "q0a2", "q0a3", "q0a4", "q0a5")
q0question.name = "q0a"
q0criterion.name = "wc10.car"

# Filter (samma för båda grupperna)
konFilter <- c("Vilket.kön.har.du.=='Man'",
               "Vilket.kön.har.du.=='Kvinna'",
               "Vilket.kön.har.du.=='Annat/inget kön'",
               "Vilket.kön.har.du.=='Vill ej uppge'")

aldFilter <- c("Hur.gammal.är.du.=='15-24'",
               "Hur.gammal.är.du.=='25-34'",
               "Hur.gammal.är.du.=='35-44'",
               "Hur.gammal.är.du.=='45-54'",
               "Hur.gammal.är.du.=='55-64'",
               "Hur.gammal.är.du.=='65+'")

bosFilter <- c("Hur.länge.har.du.bott.i.Upplands.Väsby.=='0-4 år'",
               "Hur.länge.har.du.bott.i.Upplands.Väsby.=='5-9 år'",
               "Hur.länge.har.du.bott.i.Upplands.Väsby.=='10 år eller mer'")

sysFilter <- c("Vilken.är.din.huvudsakliga.sysselsättning.=='Arbetssökande eller i arbetsmarknadspolitisk åtgärd'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Egen företagare'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Förvärvsarbetande (anställd)'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Har sjuk- eller aktivitetsersättning'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Långtidssjukskriven (mer än 3 månader)'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Pensionär'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Student'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Tjänstledig eller föräldraledig'",
               "Vilken.är.din.huvudsakliga.sysselsättning.=='Övrigt'")

utbFilter <- c("Vilken.är.din.högsta.utbildningsnivå.=='Annan eftergymnasial utbildning'",
               "Vilken.är.din.högsta.utbildningsnivå.=='Forskarutbildning'",
               "Vilken.är.din.högsta.utbildningsnivå.=='Grundskola eller motsvarande obligatorisk skola'",
               "Vilken.är.din.högsta.utbildningsnivå.=='Gymnasium, folkhögskola eller motsvarande'",
               "Vilken.är.din.högsta.utbildningsnivå.=='Högskola/universitet'",
               "Vilken.är.din.högsta.utbildningsnivå.=='Inte gått ut grundskola eller motsvarande obligatorisk skola'")

borFilter <- c("Var.bor.du.=='Antuna/Älvsunda'",
               "Var.bor.du.=='Brunnby-Vik'",
               "Var.bor.du.=='Carlslund/Brunnby Park'",
               "Var.bor.du.=='Dragonvägen'",
               "Var.bor.du.=='Eds Glesbygd'",
               "Var.bor.du.=='Ekebo'",
               "Var.bor.du.=='Ekeby/Sköldnora'",
               "Var.bor.du.=='Folkparksområdet'",
               "Var.bor.du.=='Fresta glesbygd'",
               "Var.bor.du.=='Frestaby'",
               "Var.bor.du.=='Glädjen/Skälbys arbetsområde'",
               "Var.bor.du.=='Grimstaby'",
               "Var.bor.du.=='Hasselbacken'",
               "Var.bor.du.=='Hasselgatan'",
               "Var.bor.du.=='Holmen'",
               "Var.bor.du.=='Infra City'",
               "Var.bor.du.=='Johannesdal'",
               "Var.bor.du.=='Kavallerigatan/Vilundaparken'",
               "Var.bor.du.=='Länk-/Klock-/Kedje-/Bygelvägen'",
               "Var.bor.du.=='Lindhemsvägen'",
               "Var.bor.du.=='Messingen/Optimus'",
               "Var.bor.du.=='Njursta'",
               "Var.bor.du.=='Norra Bollstanäs'",
               "Var.bor.du.=='Norra Nordanvägen'",
               "Var.bor.du.=='Norra Vik'",
               "Var.bor.du.=='Odenslunda'",
               "Var.bor.du.=='Prästgårdsmarken'",
               "Var.bor.du.=='Runby'",
               "Var.bor.du.=='Runby Backar/Lövsta'",
               "Var.bor.du.=='Sanda Ängar'",
               "Var.bor.du.=='Sigma/Apoteksskogen'",
               "Var.bor.du.=='Sjukyrkoberget'",
               "Var.bor.du.=='Skälby'",
               "Var.bor.du.=='Smedby 2'",
               "Var.bor.du.=='Smedby arbetsområde'",
               "Var.bor.du.=='Södra Bollstanäs'",
               "Var.bor.du.=='Södra Prästgårdsmarken'",
               "Var.bor.du.=='Stallgatan'",
               "Var.bor.du.=='Stora Wäsby gård'",
               "Var.bor.du.=='Väsby villastad/Tegelbruket'",
               "Var.bor.du.=='Vatthagen'",
               "Var.bor.du.=='Vilunda/Korpkulla'")

temLbls <- c("1. Parker & grönområden",
             "2. Diversity in housing supply",
             "3. Levandegöra gemensamma platser",
             "4. Kommunikationer",
             "5. Kultur & fritid",
             "6. Utbildning",
             "7. Omsorg",
             "8. Skolan",
             "9. Trygghet",
             "10. Hållbar utveckling")

frgLbls <- list(c("1a. Bevara existerande större grönområden",
                  "1b. Anlägga parker i existerande stadsdelar",
                  "1c. Bygga bostäder nära grönområden",
                  "1d. Rusta upp befintliga parker",
                  "1e. Skapa bättre tillgänglighet till större grönområden"), 
                c("2a. Offer more housing types",
                  "2b. Offer more apartment sizes",
                  "2c. Offer small-scale land ownership",
                  "2d. Preserve the conceptual foundations of the buildings from the 1970s",
                  "2e. Offer more housing near the water"),
                c("3a. Blanda trafikslagen", 
                  "3b. Förlägga parkering längs med gator",
                  "3c. Vända entréer mot gator",
                  "3d. Förlägga publika lokaler i transparenta bottenvåningar",
                  "3e. Trygga parkeringslösningar under bostäder"),
                c("4a. Koppla ihop nya gator med befintliga för att stärka kopplingen till intilliggande stadsdelar och minska barriärerna som de stora vägarna utgör.", 
                  "4b. Förbättra kommunikationerna nattetid mellan olika delar av kommunen",
                  "4c. Förbättra kommunikationerna till och från Uppsala",
                  "4d. Förbättra de nord-sydliga och öst-västliga stråken via ett finmaskigare och väl integrerat stadsnät",
                  "4e. Förbättra kommunikationerna till och från Stockholms innerstad"),
                c("5a. Utöka utbudet av kultur- sport- och fritidsaktiviteter", 
                  "5b. Skapa bättre möjligheter för festivaler och konserter",
                  "5c. Skapa fler förutsättningar för utomhussporter",
                  "5d. Skapa marknadsplatser utomhus",
                  "5e. Erbjuda kommunala bidrag för kultur- och fritidsprojekt"),
                c("6a. Rusta upp äldre skolor", 
                  "6b. Bygg nya skolor",
                  "6c. Förbättra skolgårdarnas fysiska miljöer",
                  "6d. Höj kvaliteten i grundskolan",
                  "6e. Höj kvaliteten på gymnasieutbildningarna"),
                c("7a. Fler kultur- och fritidsaktiviteter för äldre", 
                  "7b. Fler kultur- och fritidsaktiviteter för barn och ungdomar",
                  "7c. Förbättra äldreomsorgen i kommunen",
                  "7d. Fler ungdomsgårdar och fältassistenter",
                  "7e. Minska barngrupperna i förskolan"),
                c("8a. Mindre barngrupper i förskolan", 
                  "8b. Höj kvaliteten i undervisningen",
                  "8c. Mer kompetensutveckling för skolor och lärare",
                  "8d. Mer modern informationsteknologi (IT) i undervisningen",
                  "8e. Involvera vårdnadshavare mer i skolan"),
                c("9a. Öka tryggheten kring stationsområdet", 
                  "9b. Fler poliser i centrala Väsby",
                  "9c. Förbättra belysningen i centrala Väsby",
                  "9d. Begränsa öppettider för alkoholutskänkning i centrala Väsby",
                  "9e. Förläng öppettider för affärsverksamhet i centrala Väsby"),
                c("10a. Minska förbrukningen av energi", 
                  "10b. Minska transporter och buller",
                  "10c. Öka klimatanpassning och kretsloppstänkande",
                  "10d. Prioritera miljövänliga transportsätt (gång, cykel, kollektivtrafik)",
                  "10e. Minska miljögifter och farliga kemikalier i naturen")
)