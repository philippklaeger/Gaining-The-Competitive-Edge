###########################################################
#                 D O W N L O A D                         #
###########################################################
  


# 1. Set Working Directory / Workspace



###########################################################


# 2. Loading Packages
library(rtweet)
library(dplyr)
library(ggpubr)
library(httr)
library(jsonlite)
library(readr)
library(rstatix)
library(stringr)

  
###########################################################


# 3. Loading Seeds
seeds <- read.csv(file = './data/seeds.csv', sep = ";")
seeds <- as.data.frame(seeds)
candidates <- read.csv(file = './data/candidates.csv', sep = ",")
candidates <- as.data.frame(candidates)


###########################################################
  

# 4. Data I - Political Marketing
## Official List of Candidates:
## https://www.bpb.de/nachschlagen/zahlen-und-fakten/bundestagswahlen/339917/2021-kandidierende

CDU_raw <- get_timelines(c("@cducsubt", "@CDU", "@csu_bt", "@ArminLaschet", "@Markus_Soeder", 
                              "@peteraltmaier", "@jensspahn","@_FriedrichMerz", "@PaulZiemiak", "@n_roettgen", "@rbrinkhaus",
                              "@DaniLudwigMdB", "@hahnflo", "@JoWadephul", "@DrAndreasNick", "@PSchnieder", "@HeikeBrehmerMdB",
                              "@c_bernstiel", "@StephPilsinger", "@StefingerMdB", "@felixschreiner", "@MichaelKuffer",
                              "@DerLenzMdB", "@RonjaKemmer", "@Peter_Beyer", "@cdu_schweiger", "@KartesMdB", "@christophploss",
                              "@VriesChristoph", "@alexander_throm", "@AntjeTillmann", "@kdgroehler", "@ChialoJoe", "@RuedigerKruse",
                              "@BjoernSimon", "@ninawarken", "@groehe", "@TKuban96", "@thorsten_frei", "@MdbWendt", "@katrin_staffler",
                              "@MBiadaczMdB", "@GruebelMdb", "@Peter_Stein_CDU", "@MarcHenrichmann", "@TinoSorge", "@mvabercron",
                              "@ManderlaGisela", "@MaikBeermann", "@SylviaPantel", "@Thomas_Bareiss", "@franksteffel",
                              "@PSchnieder", "@MarcusWeinberg", "@thomasgebhart", "@PatrickSensburg", "@SteffenBilger", "@JoSteiniger",
                              "@Erwin_Rueddel", "@TSchipanski", "@MechthildHeil", "@MaikBeermann", "@YvonneMagwas", "@MdbWendt",
                              "@RuedigerKruse", "@mueller_sepp", "@gero_storjohann", "@matthiaszimmer", "@TErndl", "@fritzfelgentreu",
                              "@Kai_Whittaker", "@MarcusWeinberg", "@KarstenMoering", "@HHirte", "@anjaweisgerber", "@VolkerUllrich",
                              "@smuellermdb", "@KLeikert", "@SteinekeCDU", "@JanaSchimke", "@Florian_Ossner", "@smuellermdb",
                              "@JM_Luczak", "@andreasscheuer", "@DoroBaer", "@MGrosseBroemer", "@juergenhardt", "@NadineSchoen",
                              "@ChristianHirte", "@wanderwitz", "@RKiesewetter", "@HBraun", "@JuliaKloeckner", "@akk"), 
                            n = 1000, 
                            type = "recent",
                            include_rts = TRUE,
                            retryonratelimit = TRUE,
                            lang = "de")


Gruene_raw <- get_timelines(c("@Die_Gruenen", "@GrueneBundestag", "@ABaerbock",
                                 "@GoeringEckardt", "@RenateKuenast", "@JTrittin", "@Oliver_Krischer", "@ebner_sha",
                                 "@MdB_Stroebele", "@KonstantinNotz", "@BriHasselmann", "@cem_oezdemir", "@Ricarda_Lang",
                                 "@KathaSchulze", "@MargareteBause", "@KaiGehring", "@svenlehmann", "@lisapaus", "@K_SA",
                                 "@KirstenKappert", "@OWvonHoltz", "@DanyWagner_DA", "@UweKekeritz", "@SteffiLemke",
                                 "@Ingrid_Nestle", "@julia_verlinden", "@OWvonHoltz", "@KottingUhl", "@MatthiasGastel",
                                 "@StefanGelbhaar", "@BeateWaRo", "@ManuelaRottman", "@katdro", "@svenlehmann", "@tobiaslindner",
                                 "@GruenClaudia", "@KatjaKeul", "@MarkusTressel", "@ChrisKuehn_mdb", "@MatthiasGastel",
                                 "@Schmidt_MdB", "@badulrichmartha", "@sven_kindler", "@bhoffmann_mdb", "@margit_stumpp",
                                 "@annachristmann", "@HajdukBundestag", "@Erhard_Grundl", "@ManuelaRottman", "@monikalazar",
                                 "@ChrisKuehn_mdb", "@MariaKlSchmeink", "@ulle_schauws", "@W_SK", "@fbrantner", "@nouripour",
                                 "@markuskurthmdb", "@IreneMihalic", "@UweKekeritz", "@ManuelSarrazin", "@filizgreen",
                                 "@DJanecek", "@TabeaRoessner", "@ekindeligoez", "@GrueneBeate", "@markuskurthmdb",
                                 "@agnieszka_mdb", "@ulle_schauws", "@max_lucks"), 
                               n = 1000, 
                               type = "recent",
                               include_rts = TRUE,
                               retryonratelimit = TRUE,
                               lang = "de")


SPD_raw <- get_timelines(c("@spdde", "@spdbt", "@OlafScholz",
                              "@HeikoMaas", "@hubertus_heil", "@larsklingbeil", "@KuehniKev", "@EskenSaskia",
                              "@NowaboFM", "@Ralf_Stegner", "@Karl_Lauterbach", "@Timon_Gremmels", "@SiemtjeMdB", "@AnneBressem",
                              "@HellmichMdB", "@stadler_svenja", "@Schwarz_MdB", "@UliFreese", "@DennisRohde", "@ThomasHitschler",
                              "@BrunnerGanzOhr", "@GabyKatzmarek", "@Andreas_Rimkus", "@A_Gloeckner", "@Lothar_Binding", "@matthiasbartke",
                              "@IsabelMackensen", "@KatjaMast", "@Kaiser_SPD", "@KorkmazGT", "@FalkoMohrs", "@baldy_daniel",
                              "@nadjasthamer", "@Krawallstein", "@Lina_Seitzl", "@jessi_rosenthal", "@AnniKlose", "@El_KaWeh_",
                              "@josephineortleb", "@FrankeEdgar", "@dieschmidt", "@jungeinberlin", "@DennisRohde", "@Lothar_Binding",
                              "@CPetryMdB", "@SteffenSonja", "@fritzfelgentreu", "@mischrodi", "@UliGroetsch", "@larscastellucci",
                              "@RitaHaglKehl", "@KlausMindrup", "@michael_thews", "@dieschmidt", "@MuellerChemnitz", "@SteffenSonja",
                              "@NielsAnnen", "@stonie_kiel", "@josephineortleb", "@MarjaVoellers", "@LeniBreymaier", "@UlliNissen",
                              "@MetinHakverdi", "@NinaScheer_SPD", "@matthiasbartke", "@SoenkeRix", "@HildeMattheis",
                              "@baerbelbas", "@HildeMattheis", "@FrankSchwabe", "@BaerbelKofler", "@GaHeinrich", "@danielakolbe",
                              "@CanselK", "@NinaScheer_SPD", "@MuellerChemnitz", "@zierke", "@s_schwartze", "@Achim_P",
                              "@soerenbartol", "@MechthildRawert", "@rischwasu", "@FlorianPost", "@sebast_hartmann", "@oezdemir_spd",
                              "@DirkWieseSPD", "@UteVogt", "@NilsSchmid"), 
                            n = 1000, 
                            type = "recent",
                            include_rts = TRUE,
                            retryonratelimit = TRUE,
                            lang = "de")


FDP_raw <- get_timelines(c("@fdp", "@FDP_Fraktion", "@c_lindner",
                              "@ria_schroeder", "@MAStrackZi", "@KatjaSuding", "@DFoest", "@reuther_bernd",
                              "@_MartinHagen", "@DanielaKluckert", "@BraFDP", "@ManuelHoeferlin", "@MarcusFaber",
                              "@f_schaeffler", "@MdBKlein", "@alexmuellerfdp", "@cad59", "@franksitta",
                              "@theliberalfrank", "@Otto_Fricke", "@DjirSarai", "@florian_toncar", "@christianduerr",
                              "@reinholdmdb", "@Meyer_FDP", "@EUTheurer", "@c_jung77", "@JBrandenburgFDP", "@torstenherbst",
                              "@nicole_ae_bauer", "@bstrasser", "@OlliLuksic", "@johannesvogel", "@Lambsdorff", "@nicolabeerfdp",
                              "@starkwatzinger", "@sandra_weeser", "@johannesvogel", "@koehler_fdp", "@GydeJ", "@krusehamburg",
                              "@LindaTeuteberg", "@MarcoBuschmann", "@noreenthiel", "@KonstantinKuhle", "@HartmutEbbing",
                              "@TillMansmann", "@PeterHeidtFDP", "@RenataAlt_MdB", "@_MartinNeumann", "@michael_g_link",
                              "@StephanThomae", "@theliberalfrank", "@BrittaDassler", "@carina_konrad", "@MTodtenhausen",
                              "@MdBKlein", "@HoubenReinhard", "@aggelidis_fdp", "@DFoest", "@KatrinHelling", "@reuther_bernd",
                              "@nicole_ae_bauer", "@hacker_fdp", "@fdp_hessel", "@carina_konrad", "@HoffmannForest", "@Matthiasnoelke",
                              "@jensbeeck", "@HerbrandMarkus", "@PascalKober", "@HoubenReinhard", "@ullaihnen", "@mseesternpauly",
                              "@muellerboehm", "@Meyer_FDP", "@k_willkomm", "@DanielaKluckert"), 
                            n = 1000, 
                            type = "recent",
                            include_rts = TRUE,
                            retryonratelimit = TRUE,
                            lang = "de")


AfD_raw <- get_timelines(c("@AfD", "@AfDimBundestag", "@Alice_Weidel", "@Tino_Chrupalla",
                              "@Beatrix_vStorch", "@Frank_Pasemann", "@M_Reichardt_AfD", "@Renner_AfD", "@MdB_Lucassen",
                              "@Th_Seitz_AfD", "@JuergenBraunAfD", "@Leif_Erik_Holm", "@PeterFelser", "@Thomas_Ehrhorn",
                              "@Paul_Podolay", "@DrHollnagel", "@ulschzi", "@Marcus_Buehl", "@WolfgangWiehle", "@Th_Seitz_AfD",
                              "@EnricoKomning", "@Witt_Uwe", "@andreasbleckmdb", "@DrFriesenMdB", "@Gerold_Otten",
                              "@M_HarderKuehnel", "@CorinnaMiazga", "@PetrBystronAfD", "@Nicole_Hoechst", "@h_weyel",
                              "@EnricoKomning", "@DirkSpaniel", "@SteffenKotre", "@MarcBernhardAfD", "@Martin_Hess_AfD",
                              "@Marcus_Buehl", "@AfDProtschka", "@Buettner_MdB", "@ChrWirthMdB", "@Witt_Uwe", "@Pohl_MdB",
                              "@Rene_Springer", "@Gerold_Otten", "@Marcus_Buehl", "@R_Hartwig_AfD", "@Schneider_AfD",
                              "@ElsnervonGronow", "@Jochen_Haug", "@mrosek1958", "@NKleinwaechter", "@KayGottschalk1",
                              "@h_weyel", "@DrFriesenMdB", "@M_HarderKuehnel", "@GottfriedCurio", "@MalteKaufmann",
                              "@StBrandner", "@JoanaCotar"), 
                            n = 1000, 
                            type = "recent",
                            include_rts = TRUE,
                            retryonratelimit = TRUE,
                            lang = "de")


Linke_raw <- get_timelines(c("@dieLinke", "@Linksfraktion", "@Janine_Wissler", "@DietmarBartsch",
                                "@katjakipping", "@b_riexinger", "@GregorGysi", "@CarenLay", "@MdB_Freihold", "@michel_brandt_",
                                "@HESommer", "@AkbulutGokay", "@MdB_Schreiber", "@MartinaRenner", "@FrStraetmanns",
                                "@pascalmeiser", "@Norbert_MdB", "@UllaJelpke", "@SBarrientosK", "@victorperli",
                                "@NicoleGohlke", "@berlinliebich", "@jankortemdb", "@Amira_M_Ali", "@HeikeHaensel",
                                "@LoetzschMdB", "@ch_buchholz", "@SevimDagdelen", "@AkbulutGokay", "@HESommer", "@AndrejHunko",
                                "@AlexanderSNeu", "@Diether_Dehm", "@NiemaMovassat", "@MartinaRenner", "@LoetzschMdB",
                                "@jankortemdb", "@PetraPauMaHe", "@voglerk", "@DorisAchelwilm", "@victorperli",
                                "@ZaklinNastic", "@tpflueger", "@UllaJelpke", "@ZdebelHubertus", "@joerg_cezanne",
                                "@katrin_staffler", "@FrStraetmanns", "@SylviaGabelmann", "@kerwolter", "@MartinNeise"), 
                              n = 1000, 
                              type = "recent",
                              include_rts = TRUE,
                              retryonratelimit = TRUE,
                              lang = "de")


###########################################################


# 5. Data II - User-Generated Discussions
## First Iteration
Discussions_1_raw <- search_tweets2(q = seeds$Hashtags, 
                         n = 1000, 
                         type = "recent",
                         include_rts = TRUE,
                         retryonratelimit = TRUE,
                         lang = "de")

## Second & Third Iteration
Discussions_4_raw <- search_tweets2(c("bundestagswahl", "bundestagswahl2021", "bundestagswahl21", "btw2021",
                            "kanzlerkandidat", "kanzlerkandidatin", "bundesregierung", "bundestagswahlkampf", "bundestag",
                            "christdemokratischeunion", "cdu", "csu", "cducsu", "bundescdu", "christdemokraten",
                            "gruene", "grüne", "diegruenen", "diegrünen", "buendnis90", "bündnis90",
                            "buendnis90diegruenen", "sozialdemokratischepartei", "spd", "sozialdemokraten",
                            "bundesspd", "freiedemokratischepartei", "fdp", "dieliberalen", "bundesfdp",
                            "alternativefuerdeutschland", "alternativefürdeutschland", "afd", "bundesafd",
                            "dielinke", "dielinken", "linkspartei", "bundeslinke",
                            "laschet", "arminlaschet", "baerbock", "annalenabaerbock", "habeck", "roberhabeck",
                            "scholz", "olafscholz", "lindner", "christianlindner", "weidel", "aliceweidel",
                            "chrupalla", "tinochrupalla", "wissler", "janinewissler", "bartsch", "dietmarbartsch",
                            "triell", "laschetverhindern", "laschetdarfnichtkanzlerwerden", "laschetlacht",
                            "laschetluegt", "niemehrcdu", "baerplag", "grueneverhindern", "baerbockplag",
                            "g20", "freegeorgthiel", "freeassange", "kinderdurchseuchung", "praesenzpflichtaussetzen",
                            "luftfiltersofort", "afdp", "nawalny", "noafd", "fckafd", "afdrausausdenparlamenten",
                            "fcknzs", "meuthen", "lanz", "maischberger", "linkebpt", "sed", "sozialklimagerecht",
                            "merkel", "sozialegerechtigkeitwaehlen", "zweifuerdeutschland", "sommerinterview",
                            "laschetdarfnichtkanzlerwerden", "esken", "gruenermist", "deutschland", "wiegehtsdeutschland",
                            "machtdaslandgerecht", "triell", "laschet", "scholz", "cdurausausderregierung",
                            "linksrutsch", "unionabwählen", "blockiaa", "klimaschutz", "noafd",
                            "allesistdrin", "bereitweilihresseid", "iaa", "klimakrise", "afghanistan",
                            "groko", "scholzpacktdasan", "sauerland", "sozialepolitikfürdich",
                            "olafscholz", "maaßen", "maassen", "stegnertourbundestag21", "steineke",
                            "fürsieda", "wirecard", "einevonhier", "brandenburgleben", "wk61", "wegenmorgen",
                            "cumex", "vielzutun", "vierkampf", "eu", "niegabesmehrzutun", "baerbock", "freiheit",
                            "digitalisierung", "deutschlandabernormal", "berlin", "wirholendasdirektmandat",
                            "brandner", "wk194", "altenburgerland", "gera", "altenburg", "thüringen",
                            "greiz", "annewill", "warburg", "ltlsa", "sachsenanhalt", "mietendeckel",
                            "hlt", "wahlarena", "klartext", "klargehtdas", "teamspdrlp", "dreyer", "worms",
                            "p7btw21show", "gemeinsamvoran", "csupt21", "guterplan", "cdigitallyunited",
                            "zukunftsteam", "gillamoos", "mietenwahl", "schlagabtausch", "r2g", "hbbue",
                            "sachsen", "kubicki", "spitzenrunde", "afdwaehlen", "afdwirkt", "afdwählen", "corona",
                            "ichwaehlelinks", "unionabwählen", "schlussrunde", "vierkampf", "mindestlohn",
                            "keinweiterso", "zukunfterkämpfen"), 
                          n = 1000, 
                          type = "recent",
                          include_rts = TRUE,
                          retryonratelimit = TRUE,
                          lang = "de")


###########################################################


# 6. Exporting Data

write_as_csv(AfD_raw, './data/AfD_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(CDU_raw, './data/CDU_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(FDP_raw, './data/FDP_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(Gruene_raw, './data/Gruene_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(Linke_raw, './data/Linke_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(SPD_raw, './data/SPD_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

write_as_csv(Discussions_1_raw, './data/Discussions_1_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(Discussions_2_raw, './data/Discussions_2_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(Discussions_3_raw, './data/Discussions_3_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
write_as_csv(Discussions_4_raw, './data/Discussions_4_raw.csv', prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
