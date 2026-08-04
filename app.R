library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(httr)
library(jsonlite)
library(xml2)
library(DT)
library(dplyr)
library(plotly)
library(shinycssloaders)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) &&
                             nchar(as.character(a[1])) > 0) a else b

INST_LABELS <- c(
  "University of California, San Francisco" = "UCSF",
  "University of California San Francisco"  = "UCSF",
  "UCSF"                                    = "UCSF",
  "Icahn School of Medicine at Mount Sinai" = "Mt. Sinai",
  "Colorado State University"               = "CSU",
  "Albert Einstein College of Medicine"     = "Einstein",
  "Boston University School of Medicine"    = "BU Med",
  "Harvard Medical School"                  = "Harvard",
  "Brown University"                        = "Brown",
  "Origami Therapeutics"                    = "Origami",
  "Mayo Clinic"                             = "Mayo",
  "University of California Santa Barbara"  = "UCSB",
  "UC Santa Barbara"                        = "UCSB",
  "UCLA"                                    = "UCLA",
  "University of California, Los Angeles"   = "UCLA",
  "Neural Stem Cell Institute"              = "NSCI",
  "Washington University in St. Louis"      = "WashU",
  "Washington University in St Louis"       = "WashU",
  "University of Cambridge"                 = "Cambridge",
  "University of Cambridge Drug Discovery"  = "Cambridge",
  "Mayo Clinic, Jacksonville"               = "Mayo",
  "Mayo Clinic Florida"                     = "Mayo FL",
  "Johns Hopkins University"                = "JHU",
  "Baylor College of Medicine"              = "BCM",
  "UC San Diego"                            = "UCSD",
  "University of Pennsylvania"              = "Penn",
  "University of Oxford"                    = "Oxford",
  "Northwestern University"                 = "Northwestern",
  "Weill Cornell Medical College"           = "Weill Cornell",
  "Massachusetts Institute of Technology"   = "MIT",
  "The Scripps Research Institute"          = "Scripps",
  "University of Pittsburgh"                = "Pitt",
  "University of Massachusetts"             = "UMass",
  "University of Southern California"       = "USC",
  "University College London"               = "UCL",
  "University of Edinburgh"                 = "Edinburgh",
  "University of Toronto"                   = "Toronto",
  "Washington University School of Medicine"= "WashU Med",
  "Washington University"                   = "WashU",
  "Boston Children's Hospital/Harvard Medical School" = "BCH/Harvard",
  "ADVantage Neuroscience Consulting"       = "ADVantage",
  "Oxiant Discovery"                        = "Oxiant",
  "University of Wisconsin Madison"         = "UW-Madison",
  "University of Wisconsin"                 = "UW-Madison"
)

SCIENTISTS <- list(
  list(name="Acosta-Uribe J",    affil="University of California Santa Barbara",
       aliases=c("Acosta-Uribe J", "Acosta Uribe J")),
  list(name="Boeve BF",           affil="Mayo Clinic",
       aliases=c("Boeve B", "Boeve BF")),
  list(name="Boeynaems S",       affil="Baylor College of Medicine",
       aliases=c("Boeynaems S", "Boeynaems SB")),
  list(name="Bowles KR",         affil="University of Edinburgh",
       aliases=c("Bowles K", "Bowles KR")),
  list(name="Boxer AL",          affil="University of California, San Francisco",
       aliases=c("Boxer A", "Boxer AL")),
  list(name="Butler DC",         affil="Neural Stem Cell Institute",
       aliases=c("Butler D", "Butler DC")),
  list(name="Clarke JH",         affil="University of Cambridge",
       aliases=c("Clarke J", "Clarke JH")),
  list(name="Clelland CD",        affil="University of California San Francisco",
       aliases=c("Clelland CD", "Clelland C")),
  list(name="Crary JF",          affil="Icahn School of Medicine at Mount Sinai",
       aliases=c("Crary J", "Crary JF")),
  list(name="Cuervo AM",         affil="Albert Einstein College of Medicine",
       aliases=c("Cuervo AM", "Cuervo A")),
  list(name="Diamond MI",        affil="UT Southwestern Medical Center",
       aliases=c("Diamond M", "Diamond MI")),
  list(name="Dickson DW",        affil="Mayo Clinic",
       aliases=c("Dickson D", "Dickson DW")),
  list(name="Disney MD",         affil="The Scripps Research Institute",
       aliases=c("Disney M", "Disney MD")),
  list(name="Duff KE",           affil="University College London",
       aliases=c("Duff K", "Duff KE")),
  list(name="Elahi FM",          affil="Icahn School of Medicine at Mount Sinai",
       aliases=c("Elahi F", "Elahi FM")),
  list(name="Emborg ME",         affil="University of Wisconsin Madison",
       aliases=c("Emborg M", "Emborg ME")),
  list(name="Farrell K",         affil="Icahn School of Medicine at Mount Sinai",
       aliases=c("Farrell K", "Farrell KK")),
  list(name="Feldman HH",        affil="UC San Diego",
       aliases=c("Feldman H", "Feldman HH")),
  list(name="Frost B",           affil="Brown University"),
  list(name="Gan L",             affil="Weill Cornell Medical College"),
  list(name="Geschwind DH",      affil="UCLA",
       aliases=c("Geschwind D", "Geschwind DH")),
  list(name="Gestwicki JE",      affil="University of California San Francisco",
       aliases=c("Gestwicki J", "Gestwicki JE")),
  list(name="Goate AM",          affil="Icahn School of Medicine at Mount Sinai",
       aliases=c("Goate A", "Goate AM")),
  list(name="Grinberg LT",       affil="Mayo Clinic",
       aliases=c("Grinberg L", "Grinberg LT")),
  list(name="Haggarty SJ",       affil="Harvard Medical School",
       aliases=c("Haggarty S", "Haggarty SJ")),
  list(name="Han S",             affil="Northwestern University"),
  list(name="Hoffman BJ",         affil="Origami Therapeutics"),
  list(name="Holtzman DM",       affil="Washington University in St. Louis",
       aliases=c("Holtzman D", "Holtzman DM")),
  list(name="Hyman BT",          affil="Harvard Medical School",
       aliases=c("Hyman B", "Hyman BT")),
  list(name="Ichida JK",         affil="University of Southern California",
       aliases=c("Ichida J", "Ichida JK")),
  list(name="Kampmann M",        affil="University of California San Francisco",
       aliases=c("Kampmann M", "Kampmann ME")),
  list(name="Kao A",             affil="University of California, San Francisco"),
  list(name="Karch CM",          affil="Washington University in St Louis",
       aliases=c("Karch C", "Karch CM")),
  list(name="Kelly JW",          affil="The Scripps Research Institute",
       aliases=c("Kelly J", "Kelly JW")),
  list(name="Kosik KS",          affil="UC Santa Barbara",
       aliases=c("Kosik K", "Kosik KS")),
  list(name="Kovacs GG",         affil="University of Toronto",
       aliases=c("Kovacs G", "Kovacs GG")),
  list(name="Krichevsky AM",     affil="Harvard Medical School",
       aliases=c("Krichevsky A", "Krichevsky AM")),
  list(name="Lasagna-Reeves CA", affil="Baylor College of Medicine",
       aliases=c("Lasagna-Reeves C", "Lasagna-Reeves CA", "Lasagna Reeves CA")),
  list(name="Lee SE",            affil="UCSF",
       aliases=c("Lee S", "Lee SE")),
  list(name="May PC",             affil="ADVantage Neuroscience Consulting",
       aliases=c("May PC")),
  list(name="McKee A",           affil="Boston University School of Medicine",
       aliases=c("McKee A", "McKee AC")),
  list(name="Mead E",            affil="University of Oxford",
       aliases=c("Mead E", "Mead EM")),
  list(name="Miller BL",         affil="University of California, San Francisco",
       aliases=c("Miller B", "Miller BL")),
  list(name="Miller TM",         affil="Washington University",
       aliases=c("Miller T", "Miller TM")),
  list(name="Morimoto RI",       affil="Northwestern University",
       aliases=c("Morimoto R", "Morimoto RI")),
  list(name="Murphy EJ",         affil="University of Oxford",
       aliases=c("Murphy E", "Murphy EJ")),
  list(name="Murray ME",         affil="Mayo Clinic Florida",
       aliases=c("Murray M", "Murray ME")),
  list(name="Nestler EJ",        affil="Icahn School of Medicine at Mount Sinai",
       aliases=c("Nestler E", "Nestler EJ")),
  list(name="Neylan TC",         affil="UCSF",
       aliases=c("Neylan T", "Neylan TC")),
  list(name="Onyike CU",         affil="Johns Hopkins University",
       aliases=c("Onyike C", "Onyike CU")),
  list(name="Orr ME",            affil="Washington University School of Medicine",
       aliases=c("Orr M", "Orr ME")),
  list(name="Petersson EJ",      affil="University of Pennsylvania",
       aliases=c("Petersson EJ", "Petersson E")),
  list(name="Possin KL",         affil="University of California, San Francisco",
       aliases=c("Possin K", "Possin KL")),
  list(name="Rabinovici G",      affil="University of California, San Francisco",
       aliases=c("Rabinovici G", "Rabinovici GD")),
  list(name="Rauch JN",          affil="University of Massachusetts",
       aliases=c("Rauch J", "Rauch JN")),
  list(name="Rexach JE",         affil="UCLA",
       aliases=c("Rexach J", "Rexach JE")),
  list(name="Rubinsztein DC",    affil="University of Cambridge",
       aliases=c("Rubinsztein D", "Rubinsztein DC")),
  list(name="Seeley WW",         affil="UCSF",
       aliases=c("Seeley W", "Seeley WW")),
  list(name="Shoichet BK",       affil="University of California, San Francisco",
       aliases=c("Shoichet B", "Shoichet BK")),
  list(name="Skidmore J",        affil="University of Cambridge"),
  list(name="Southworth DR",     affil="University of California San Francisco",
       aliases=c("Southworth D", "Southworth DR")),
  list(name="Spillantini MG",    affil="University of Cambridge",
       aliases=c("Spillantini M", "Spillantini MG")),
  list(name="Steen JA",          affil="Boston Children's Hospital/Harvard Medical School",
       aliases=c("Steen J", "Steen JA")),
  list(name="Stehouwer JS",      affil="University of Pittsburgh",
       aliases=c("Stehouwer J", "Stehouwer JS")),
  list(name="Svensson S",        affil="Oxiant Discovery",
       aliases=c("Svensson S", "Svensson SE")),
  list(name="Temple S",          affil="Neural Stem Cell Institute"),
  list(name="Tsai LH",           affil="Massachusetts Institute of Technology",
       aliases=c("Tsai LH", "Tsai L")),
  list(name="VandeVrede L",      affil="University of California, San Francisco",
       aliases=c("VandeVrede L", "Vandevrede L")),
  list(name="Vasdev N",          affil="University of Toronto"),
  list(name="Walsh C",           affil="University of California San Francisco",
       aliases=c("Walsh C", "Walsh CM")),
  list(name="Woerman AL",        affil="Colorado State University",
       aliases=c("Woerman A", "Woerman AL")),
  list(name="Yokoyama JS",       affil="University of California, San Francisco",
       aliases=c("Yokoyama J", "Yokoyama JS")),
  list(name="Zheng H",           affil="Baylor College of Medicine")
)
SCIENTIST_NAMES <- sapply(SCIENTISTS, `[[`, "name")

SCIENTIST_INST <- setNames(
  sapply(SCIENTISTS, function(s) INST_LABELS[s$affil] %||% s$affil),
  SCIENTIST_NAMES
)

SCIENTIST_PICKER_LABELS <- SCIENTIST_NAMES
names(SCIENTIST_PICKER_LABELS) <- SCIENTIST_NAMES

DATA_TYPE_COLORS <- c(
  "Whole Genome Sequencing" = "#e05fa0",
  "Exome Sequencing"        = "#3dbfbf",
  "RNA-seq"                 = "#f07c3a",
  "Imaging"                 = "#4a7fc1",
  "Proteomics"              = "#5b9e6b",
  "Computational code"      = "#9b6bbf",
  "Phenotypic data"         = "#e8694a",
  "Other"                   = "#7a8999"
)
ALL_DATA_TYPES        <- names(DATA_TYPE_COLORS)
ALL_DATA_TYPES_FILTER <- setdiff(ALL_DATA_TYPES, "Other")

MODEL_COLORS <- c(
  "Human"       = "#4a7fc1",
  "Mouse"       = "#e8694a",
  "In-vitro"    = "#5b9e6b",
  "In-silico"   = "#9b6bbf",
  "Multi-model" = "#e8a84a",
  "Other"       = "#7a8999"
)
ALL_MODELS        <- names(MODEL_COLORS)
ALL_MODELS_FILTER <- setdiff(ALL_MODELS, "Other")

ALL_FOCUS_TAGS <- c("Tau", "TDP-43", "\u03b1-Synuclein", "Amyloid-\u03b2",
                    "Neuroinflammation", "Protein Aggregation",
                    "Autophagy", "Immune response", "Genomics/Sequencing", 
                    "Biomarkers", "Other")

DATA_SHARING_LEVELS <- c("Public Repository", "Controlled Access", "Upon Request", "Not disclosed")

DATA_SHARING_FILTER_LEVELS <- c("Public Repository", "Controlled Access", "Mixed",
                                "Upon Request", "Not disclosed")

DATA_SHARING_COLORS <- c(
  "Public Repository" = "#2e8060",
  "Controlled Access" = "#c88030",
  "Mixed"             = "#7060a8",
  "Upon Request"      = "#3a6a8a",
  "Not disclosed"     = "#555555"
)

DATA_SHARING_ICONS <- c(
  "Public Repository" = "\U0001F7E2",
  "Controlled Access" = "\U0001F7E1",
  "Mixed"             = "\U0001F7E3",
  "Upon Request"      = "\U0001F535",
  "Not disclosed"     = "\u26AA"
)

NEURO_QUERY <- paste0(
  "(neurodegeneration OR tauopathy OR tau OR Alzheimer OR dementia OR ",
  "\"frontotemporal dementia\" OR \"Parkinson disease\" OR \"TDP-43\" OR ",
  "amyloid OR synuclein OR neurofibrillary OR \"MAPT\" OR \"APOE\" OR ",
  "\"neurodegeneration\" OR \"brain atrophy\" OR prion OR \"Lewy body\" OR ",
  "\"ALS\" OR \"amyotrophic lateral sclerosis\" OR \"Huntington\")"
)

AFFIL_SHORT <- list(
  "University of California, San Francisco" = paste0(
    "(\"University of California San Francisco\" OR ",
    "\"University of California, San Francisco\" OR ",
    "UCSF OR \"Gladstone Institute\" OR \"Memory and Aging Center\" OR ",
    "\"Weill Institute for Neurosciences\" OR \"UCSF Memory\" OR ",
    "\"San Francisco VA\" OR \"Sandler Neurosciences Center\")"),
  "University of California San Francisco" = paste0(
    "(\"University of California San Francisco\" OR ",
    "\"University of California, San Francisco\" OR ",
    "UCSF OR \"Gladstone Institute\" OR \"Memory and Aging Center\" OR ",
    "\"Weill Institute for Neurosciences\" OR \"UCSF Memory\" OR ",
    "\"San Francisco VA\" OR \"Sandler Neurosciences Center\")"),
  "UCSF" = paste0(
    "(\"University of California San Francisco\" OR ",
    "\"University of California, San Francisco\" OR ",
    "UCSF OR \"Gladstone Institute\" OR \"Memory and Aging Center\" OR ",
    "\"Weill Institute for Neurosciences\" OR \"UCSF Memory\" OR ",
    "\"San Francisco VA\" OR \"Sandler Neurosciences Center\")"),
  "University of California Santa Barbara" = paste0(
    "(\"University of California Santa Barbara\" OR ",
    "\"University of California, Santa Barbara\" OR \"UC Santa Barbara\" OR UCSB)"),
  "UC Santa Barbara" = paste0(
    "(\"University of California Santa Barbara\" OR ",
    "\"University of California, Santa Barbara\" OR \"UC Santa Barbara\" OR UCSB)"),
  "University of California, Los Angeles" = paste0(
    "(UCLA OR \"University of California Los Angeles\" OR ",
    "\"University of California, Los Angeles\" OR \"David Geffen School of Medicine\")"),
  "UCLA" = paste0(
    "(UCLA OR \"University of California Los Angeles\" OR ",
    "\"University of California, Los Angeles\" OR \"David Geffen School of Medicine\")"),
  "UC San Diego" = paste0(
    "(\"UC San Diego\" OR \"University of California San Diego\" OR ",
    "\"University of California, San Diego\" OR UCSD)"),
  "Washington University in St. Louis" = paste0(
    "(\"Washington University\" OR \"Washington University in St. Louis\" OR ",
    "\"Washington University in St Louis\" OR \"Washington University School of Medicine\")"),
  "Washington University in St Louis" = paste0(
    "(\"Washington University\" OR \"Washington University in St. Louis\" OR ",
    "\"Washington University in St Louis\" OR \"Washington University School of Medicine\")"),
  "Washington University School of Medicine" = paste0(
    "(\"Washington University\" OR \"Washington University in St. Louis\" OR ",
    "\"Washington University in St Louis\" OR \"Washington University School of Medicine\")"),
  "Washington University" = paste0(
    "(\"Washington University\" OR \"Washington University in St. Louis\" OR ",
    "\"Washington University in St Louis\" OR \"Washington University School of Medicine\")"),
  "Icahn School of Medicine at Mount Sinai" = paste0(
    "(\"Mount Sinai\" OR \"Icahn School of Medicine\" OR \"Friedman Brain Institute\")"),
  "Harvard Medical School" = paste0(
    "(\"Harvard Medical\" OR \"Harvard University\" OR ",
    "\"Massachusetts General Hospital\" OR \"Brigham and Women\")"),
  "Boston Children's Hospital/Harvard Medical School" = paste0(
    "(\"Harvard Medical\" OR \"Boston Children\" OR \"Harvard University\")"),
  "Massachusetts Institute of Technology" = "(MIT OR \"Massachusetts Institute of Technology\" OR \"Picower Institute\")",
  "University of Cambridge" = paste0(
    "(\"University of Cambridge\" OR \"Cambridge University\" OR ",
    "\"MRC Laboratory of Molecular Biology\" OR \"Wellcome-MRC Cambridge Stem Cell Institute\")"),
  "University of Cambridge Drug Discovery" = "(\"University of Cambridge\" OR \"Cambridge University\")",
  "Mayo Clinic"             = "(\"Mayo Clinic\" OR \"Mayo Foundation\")",
  "Mayo Clinic, Jacksonville" = "(\"Mayo Clinic\" OR \"Mayo Foundation\")",
  "Mayo Clinic Florida"     = "(\"Mayo Clinic\" OR \"Mayo Foundation\" OR \"Mayo Clinic Florida\")",
  "Baylor College of Medicine" = "(\"Baylor College of Medicine\" OR \"Baylor College\")",
  "Northwestern University" = "(Northwestern OR \"Northwestern University\" OR \"Feinberg School of Medicine\")",
  "Brown University"        = "(\"Brown University\" OR \"Brown Univ\")",
  "University of Edinburgh" = "(\"University of Edinburgh\" OR Edinburgh OR \"UK Dementia Research Institute\")",
  "University College London" = "(\"University College London\" OR UCL OR \"UK Dementia Research Institute\")",
  "Albert Einstein College of Medicine" = paste0(
    "(\"Albert Einstein College\" OR \"Einstein College of Medicine\" OR \"Montefiore Medical\")"),
  "UT Southwestern Medical Center" = "(\"UT Southwestern\" OR \"University of Texas Southwestern\")",
  "The Scripps Research Institute" = paste0(
    "(\"Scripps Research\" OR \"The Scripps Research Institute\" OR \"Scripps Research Institute\")"),
  "Weill Cornell Medical College" = "(\"Weill Cornell\" OR \"Cornell University\" OR \"Weill Medical College\")",
  "University of Wisconsin Madison" = paste0(
    "(\"University of Wisconsin\" OR \"University of Wisconsin-Madison\" OR ",
    "\"University of Wisconsin, Madison\" OR \"Wisconsin National Primate\")"),
  "University of Wisconsin" = paste0(
    "(\"University of Wisconsin\" OR \"University of Wisconsin-Madison\" OR ",
    "\"University of Wisconsin, Madison\")"),
  "University of Toronto"   = "(\"University of Toronto\" OR Toronto OR \"Krembil Brain Institute\" OR \"CAMH\")",
  "University of Oxford"    = "(Oxford OR \"University of Oxford\" OR \"Radcliffe Department of Medicine\")",
  "University of Southern California" = paste0(
    "(\"University of Southern California\" OR USC OR \"Keck School of Medicine\" OR \"USC Keck\")"),
  "University of Pennsylvania" = "(\"University of Pennsylvania\" OR \"Penn Medicine\" OR \"Perelman School of Medicine\")",
  "University of Massachusetts" = "(\"University of Massachusetts\" OR UMass OR \"UMass Medical\")",
  "Neural Stem Cell Institute" = "\"Neural Stem Cell\"",
  "Colorado State University" = "(\"Colorado State University\" OR \"Colorado State\")",
  "University of Pittsburgh" = "(\"University of Pittsburgh\" OR Pittsburgh OR \"UPMC\")",
  "Boston University School of Medicine" = paste0(
    "(\"Boston University\" OR \"BU School of Medicine\" OR \"Boston University Alzheimer\")"),
  "Johns Hopkins University" = "(\"Johns Hopkins\" OR \"Johns Hopkins University\" OR \"Johns Hopkins Medicine\")",
  "Oxiant Discovery"         = "Oxiant",
  "Origami Therapeutics"     = "Origami",
  "ADVantage Neuroscience Consulting" = "ADVantage"
)

do_esearch <- function(query, max_results, max_retries = 3, api_key = "") {
  api_param <- if (nchar(api_key) > 0) paste0("&api_key=", api_key) else ""
  delay <- if (nchar(api_key) > 0) 0.15 else 0.4
  url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
    "?db=pubmed&term=", utils::URLencode(query, reserved = TRUE),
    "&retmax=", max_results, "&retmode=json", api_param
  )
  for (attempt in 1:max_retries) {
    Sys.sleep(delay)
    tryCatch({
      resp <- GET(url, timeout(30))
      if (status_code(resp) == 200) {
        txt <- content(resp, "text", encoding = "UTF-8")
        if (!grepl("rate limit", txt, ignore.case = TRUE)) {
          dat <- fromJSON(txt)
          return(dat$esearchresult$idlist %||% character(0))
        }
      }
    }, error = function(e) NULL)
    Sys.sleep(1)
  }
  return(character(0))
}

fetch_pmids <- function(scientist_name, affiliation, max_results = 50, api_key = "", aliases = NULL) {
  all_ids <- character(0)
  nm_base       <- c(aliases, scientist_name, gsub("-", " ", scientist_name))
  name_variants <- unique(nm_base)
  name_clause <- paste(sprintf('"%s"[Author]', name_variants), collapse = " OR ")
  if (length(name_variants) > 1) name_clause <- paste0("(", name_clause, ")")
  affil_kw     <- AFFIL_SHORT[[affiliation]] %||% paste0('"', affiliation, '"')
  affil_clause <- paste0(affil_kw, "[Affiliation]")
  q1 <- paste0(name_clause, " AND ", affil_clause, " AND 2020:2026[PDAT]")
  s1 <- do_esearch(q1, max_results, api_key = api_key)
  all_ids <- union(all_ids, s1)
  q2 <- paste0(name_clause, " AND ", affil_clause, " AND ", NEURO_QUERY, " AND 2020:2026[PDAT]")
  all_ids <- union(all_ids, do_esearch(q2, max_results, api_key = api_key))
  if (length(s1) < 15) {
    q3 <- paste0(name_clause, " AND ", NEURO_QUERY, " AND 2020:2026[PDAT]")
    all_ids <- union(all_ids, do_esearch(q3, max_results, api_key = api_key))
  }
  unique(all_ids)[seq_len(min(length(all_ids), max_results))]
}

fetch_details <- function(pmids, max_retries = 3, api_key = "") {
  if (length(pmids) == 0) return(NULL)
  ids_str   <- paste(pmids, collapse = ",")
  api_param <- if (nchar(api_key) > 0) paste0("&api_key=", api_key) else ""
  delay     <- if (nchar(api_key) > 0) 0.15 else 0.4
  sum_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=",
                    ids_str, "&retmode=json", api_param)
  abs_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=",
                    ids_str, "&rettype=abstract&retmode=xml", api_param)
  for (attempt in 1:max_retries) {
    tryCatch({
      Sys.sleep(delay)
      sum_resp <- GET(sum_url, timeout(30))
      Sys.sleep(delay)
      abs_resp <- GET(abs_url, timeout(30))
      if (status_code(sum_resp) == 200 && status_code(abs_resp) == 200) {
        sum_txt <- content(sum_resp, "text", encoding = "UTF-8")
        if (!grepl("rate limit", sum_txt, ignore.case = TRUE)) {
          sum_data <- fromJSON(sum_txt)
          uids     <- sum_data$result$uids
          if (!is.null(uids) && length(uids) > 0) {
            abs_xml   <- read_xml(content(abs_resp, "text", encoding = "UTF-8"))
            abs_nodes <- xml_find_all(abs_xml, "//PubmedArticle")
            
            abstracts  <- list()
            da_pubmed  <- list()
            databanks  <- list()
            
            for (n in abs_nodes) {
              pmid <- xml_text(xml_find_first(n, ".//PMID"))
              
              ab_nodes <- xml_find_all(n, ".//AbstractText")
              ab_parts <- c()
              da_parts <- c()
              for (ab in ab_nodes) {
                lbl  <- tolower(xml_attr(ab, "Label") %||% "")
                txt  <- xml_text(ab)
                if (grepl("data avail|data sharing|data access|code avail|availability of data|
                           data and code|data statement|availability statement", lbl)) {
                  da_parts <- c(da_parts, txt)
                } else {
                  ab_parts <- c(ab_parts, txt)
                }
              }
              abstracts[[pmid]] <- paste(ab_parts, collapse = " ")
              if (length(da_parts) > 0) da_pubmed[[pmid]] <- paste(da_parts, collapse = " ")
              
              db_nodes <- xml_find_all(n, ".//DataBank")
              if (length(db_nodes) > 0) {
                db_entries <- sapply(db_nodes, function(db) {
                  db_name <- xml_text(xml_find_first(db, ".//DataBankName"))
                  acc_ids  <- xml_text(xml_find_all(db, ".//AccessionNumber"))
                  if (length(acc_ids) > 0)
                    paste0(db_name, ":", paste(acc_ids, collapse = ","))
                  else db_name
                })
                databanks[[pmid]] <- paste(db_entries, collapse = "; ")
              }
            }
            
            rows <- lapply(uids, function(uid) {
              item <- sum_data$result[[uid]]
              if (is.null(item)) return(NULL)
              authors <- tryCatch({
                if (!is.null(item$authors) && is.data.frame(item$authors) && nrow(item$authors) > 0)
                  paste(item$authors$name, collapse = ", ") else ""
              }, error = function(e) "")
              doi_val <- tryCatch({
                aids <- item$articleids
                if (is.data.frame(aids) && "idtype" %in% names(aids)) {
                  idx <- which(tolower(aids$idtype) == "doi")
                  if (length(idx) > 0) aids$value[idx[1]] else ""
                } else ""
              }, error = function(e) "")
              data.frame(
                pmid          = uid,
                title         = item$title    %||% "",
                authors       = authors,
                journal       = item$fulljournalname %||% item$source %||% "",
                year          = gsub(".*?(\\d{4}).*", "\\1", item$pubdate %||% ""),
                doi           = doi_val %||% "",
                abstract      = abstracts[[uid]] %||% "",
                da_raw        = da_pubmed[[uid]] %||% "",
                da_databanks  = databanks[[uid]] %||% "",
                stringsAsFactors = FALSE
              )
            })
            return(do.call(rbind, Filter(Negate(is.null), rows)))
          }
        }
      }
    }, error = function(e) { message("fetch_details retry ", attempt, " failed: ", e$message) })
    Sys.sleep(1)
  }
  return(NULL)
}

pmid_to_pmcid_single <- function(pmid, api_key = "") {
  api_param <- if (nchar(api_key) > 0) paste0("&api_key=", api_key) else ""
  Sys.sleep(if (nchar(api_key) > 0) 0.12 else 0.4)
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi",
                "?dbfrom=pubmed&db=pmc&id=", pmid,
                "&retmode=json", api_param)
  tryCatch({
    resp <- GET(url, timeout(15))
    if (status_code(resp) != 200) return("")
    dat   <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    lsets <- dat$linksets
    if (is.null(lsets) || length(lsets) == 0) return("")
    for (lsdb in lsets[[1]]$linksetdbs %||% list()) {
      if (identical(lsdb$linkname, "pubmed_pmc") && length(lsdb$links) > 0)
        return(as.character(lsdb$links[[1]]))
    }
    return("")
  }, error = function(e) "")
}

fetch_da_for_pmid <- function(pmid, api_key = "", doi = "") {
  pmcid   <- pmid_to_pmcid_single(pmid, api_key)
  da_text <- if (nchar(pmcid) > 0) fetch_pmc_da(pmcid, api_key) else ""
  
  if (nchar(da_text) == 0 && nchar(doi) > 0)
    da_text <- fetch_doi_da(doi)
  
  list(pmid = pmid, da_text = da_text)
}

fetch_doi_da <- function(doi) {
  if (nchar(trimws(doi)) == 0) return("")
  url <- paste0("https://doi.org/", trimws(doi))
  tryCatch({
    Sys.sleep(0.5)
    resp <- GET(url, timeout(20),
                add_headers(
                  "User-Agent" = "Mozilla/5.0 (compatible; NeuroPublicationDashboard/1.0)",
                  "Accept"     = "text/html,application/xhtml+xml"
                ))
    if (status_code(resp) != 200) return("")
    html <- content(resp, "text", encoding = "UTF-8")
    
    da_pat <- paste0("data.avail|data.shar|data.access|code.avail|",
                     "availability.of.data|data.and.code|data.statement|",
                     "availability.statement|data.deposition|accession.number")
    
    da_text <- tryCatch({
      doc   <- read_html(html)
      nodes <- xml_find_all(doc, ".//*[self::h2 or self::h3 or self::h4 or self::h5]")
      found <- ""
      for (nd in nodes) {
        if (grepl(da_pat, tolower(xml_text(nd)))) {
          parent   <- xml_parent(nd)
          siblings <- xml_children(parent)
          nd_pos   <- which(sapply(siblings, identical, nd))
          paras    <- c()
          for (s in seq(nd_pos + 1, min(nd_pos + 6, length(siblings)))) {
            tag <- xml_name(siblings[[s]])
            if (tag %in% c("p", "div", "ul", "ol", "section")) {
              paras <- c(paras, xml_text(siblings[[s]]))
            } else if (tag %in% c("h2","h3","h4","h5")) {
              break 
            }
          }
          if (length(paras) > 0) { found <- paste(paras, collapse = " "); break }
        }
      }
      if (nchar(found) == 0) {
        attr_nodes <- xml_find_all(doc, paste0(
          ".//*[contains(translate(@class,'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
          "'abcdefghijklmnopqrstuvwxyz'),'data-avail') or ",
          "contains(translate(@id,'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
          "'abcdefghijklmnopqrstuvwxyz'),'data-avail')]"))
        if (length(attr_nodes) > 0) found <- xml_text(attr_nodes[[1]])
      }
      found
    }, error = function(e) "")
    
    if (nchar(da_text) == 0) {
      m <- regexpr(
        paste0("(?i)(?:<h[2-5][^>]*>\\s*(?:data|code)\\s*(?:availability|sharing|",
               "access|statement)[^<]*</h[2-5]>)(.*?)(?=<h[2-5])"),
        html, perl = TRUE)
      if (m > 0) {
        raw_chunk <- regmatches(html, m)
        da_text <- trimws(gsub("<[^>]+>", " ", raw_chunk))
        da_text <- trimws(gsub("\\s{2,}", " ", da_text))
        da_text <- substr(da_text, 1, 2000)
      }
    }
    
    trimws(da_text)
  }, error = function(e) "")
}

fetch_pmc_da_batch <- function(pmids, api_key = "", workers = 5, dois = NULL) {
  if (length(pmids) == 0) return(list())
  lapply(seq_along(pmids), function(i) {
    doi <- if (!is.null(dois) && length(dois) >= i) dois[i] else ""
    fetch_da_for_pmid(pmids[i], api_key, doi = doi)
  })
}

pmids_to_pmcids <- function(pmids, api_key = "") {
  if (length(pmids) == 0) return(setNames(character(0), character(0)))
  result <- setNames(character(length(pmids)), pmids)
  for (pmid in pmids) result[pmid] <- pmid_to_pmcid_single(pmid, api_key)
  result
}

fetch_pmc_da <- function(pmcid, api_key = "") {
  if (is.na(pmcid) || nchar(pmcid) == 0) return("")
  api_param <- if (nchar(api_key) > 0) paste0("&api_key=", api_key) else ""
  Sys.sleep(if (nchar(api_key) > 0) 0.12 else 0.4)
  url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
                "?db=pmc&id=", pmcid, "&rettype=xml&retmode=xml", api_param)
  tryCatch({
    resp <- GET(url, timeout(25))
    if (status_code(resp) != 200) return("")
    xml  <- read_xml(content(resp, "text", encoding = "UTF-8"))
    
    da_pattern <- paste0("data.avail|data.shar|data.access|code.avail|",
                         "availability.of.data|data.and.code|data.statement|",
                         "availability.statement|data.deposition|",
                         "supplementary.data|accession.number")
    
    sec_nodes <- xml_find_all(xml, ".//sec")
    for (sec in sec_nodes) {
      title_node <- xml_find_first(sec, "title")
      if (!inherits(title_node, "xml_missing")) {
        lbl <- tolower(xml_text(title_node))
        if (grepl(da_pattern, lbl)) {
          txt <- paste(xml_text(xml_find_all(sec, ".//p")), collapse = " ")
          if (nchar(trimws(txt)) > 10) return(trimws(txt))
          txt <- trimws(xml_text(sec))
          if (nchar(txt) > 10) return(txt)
        }
      }
    }
    
    for (sec in sec_nodes) {
      sec_type <- tolower(xml_attr(sec, "sec-type") %||% "")
      if (grepl(da_pattern, sec_type)) {
        txt <- paste(xml_text(xml_find_all(sec, ".//p")), collapse = " ")
        if (nchar(trimws(txt)) > 10) return(trimws(txt))
      }
    }
    
    for (nn in xml_find_all(xml, ".//notes")) {
      lbl <- tolower(xml_attr(nn, "notes-type") %||% "")
      if (grepl(da_pattern, lbl)) {
        txt <- trimws(xml_text(nn))
        if (nchar(txt) > 10) return(txt)
      }
    }
    
    for (cm in xml_find_all(xml, ".//custom-meta")) {
      name_node <- xml_find_first(cm, "meta-name")
      if (!inherits(name_node, "xml_missing")) {
        if (grepl(da_pattern, tolower(xml_text(name_node)))) {
          val_node <- xml_find_first(cm, "meta-value")
          if (!inherits(val_node, "xml_missing")) {
            txt <- trimws(xml_text(val_node))
            if (nchar(txt) > 10) return(txt)
          }
        }
      }
    }
    
    repo_pattern <- paste0("dbgap|geo accession|zenodo|dryad|figshare|github|",
                           "sra accession|egas|synapse|niagads|amp-pd|",
                           "available upon request|upon reasonable request|",
                           "data are available|data is available|",
                           "deposited at|deposited in|accession number|",
                           "data cannot be shared|data not available")
    for (p in xml_find_all(xml, ".//p")) {
      txt <- trimws(xml_text(p))
      if (nchar(txt) > 20 && nchar(txt) < 1500 &&
          grepl(repo_pattern, tolower(txt))) {
        return(txt)
      }
    }
    
    return("")
  }, error = function(e) "")
}

classify_keywords <- function(title, abstract) {
  text <- tolower(paste(title, abstract))
  
  is_human    <- grepl("patient|patients|clinical|cohort|human|humans|postmortem|post-mortem|autopsy|\\bcsf\\b|plasma|serum|biomarker|\\bmri\\b|\\bpet\\b|\\bmmse\\b|individual|individuals|participant|participants|subject|subjects|population|populations|ancestry|demographic|men|women|healthy control|cases|volunteers|pedigree|family|families|males|females|genome-wide association|\\bgwas\\b", text)
  is_mouse    <- grepl("\\bmouse\\b|\\bmice\\b|murine|\\bapp/ps1\\b|\\b5xfad\\b|\\btg2576\\b|tau p301s|rodent|rodents|transgenic|knockout|wild-type|wildtype|\\bwt\\b|c57bl/6|c57bl|tg4510|ps19|p301l|3xtg", text)
  is_invitro  <- grepl("cell culture|\\bipsc\\b|\\bipscs\\b|organoid|organoids|\\bin vitro\\b|hek293|primary culture|cell line|cell lines|purified protein|fibroblast|fibroblasts|sh-sy5y|hela|n2a|in-vitro|assay|in vitro|stem cell", text)
  is_insilico <- grepl("in silico|in-silico|simulation|computational model|machine learning|bioinformatics|pipeline|molecular dynamics|\\bmd simulation\\b|docking|virtual screening|mathematical model|deep learning|neural network|artificial intelligence|\\bai\\b", text)
  
  is_wgs      <- grepl("whole.genome sequenc|\\bwgs\\b|whole genome seq|genome-wide sequenc|genomic sequenc", text)
  is_exome    <- grepl("exome sequenc|\\bwes\\b|whole.exome|exome capture|exome-wide|targeted exome", text)
  is_rnaseq   <- grepl(paste0("rna.seq|rnaseq|scrna-seq|scrnaseq|single.cell rna|single.nucleus rna|",
                              "snrna-seq|10x genomics|dropseq|drop-seq|smart-seq|spatial transcriptom|",
                              "bulk rna|transcriptom|transcriptomic|gene expression omnibus|\\bgeo\\b.*rna|",
                              "differential.expression|deseq|edger|rna sequenc|microarray|gene expression profil"), text)
  is_imaging  <- grepl(paste0("\\bmri\\b|\\bpet\\b|neuroimaging|\\bfmri\\b|diffusion tensor|\\bdti\\b|",
                              "brain imaging|structural mri|functional mri|positron emission|",
                              "computed tomography|\\bct scan\\b|confocal|fluorescence microscop|",
                              "electron microscop|\\btem\\b|\\bsem\\b|immunofluoresc|immunohistochem|",
                              "\\beeg\\b|electroencephalograph|\\bmeg\\b|magnetoencephalograph|\\bfnirs\\b|",
                              "two-photon|multiphoton|cryo-em|cryo-electron|radiotracer|autoradiograph"), text)
  is_proteom  <- grepl(paste0("proteom|mass spectrometry|\\bms/ms\\b|\\blc-ms\\b|protein quantif|",
                              "protein expression|western blot|elisa|immunoassay|multiplex|",
                              "\\bsomascan\\b|\\bolink\\b|phosphoproteom|interactom|\\btmt\\b|tandem mass tag|",
                              "\\bitraq\\b|\\bsilac\\b|immunoprecipitation|\\bco-ip\\b|protein array"), text)
  is_comp     <- grepl(paste0("bioinformatics|machine learning|deep learning|neural network|",
                              "\\bgwas\\b|algorithm|computational|network analysis|simulation|",
                              "natural language processing|\\bnlp\\b|large language model|",
                              "\\bllm\\b|artificial intelligence|\\bai\\b.*model|",
                              "software|pipeline|workflow|tool development|molecular dynamics|",
                              "\\bmd simulation\\b|molecular docking|in silico|cheminformatics|systems biology"), text)
  is_phenotyp <- grepl(paste0("patient|patients|clinical trial|cohort|biomarker|cerebrospinal fluid|",
                              "\\bcsf\\b|plasma|serum|longitudinal study|postmortem|post-mortem|",
                              "autopsy|neuropatholog|\\bmouse\\b|\\bmice\\b|\\brat\\b|\\brats\\b|",
                              "transgenic|knockout|drosophila|c\\. elegans|zebrafish|",
                              "non-human primate|\\bnhp\\b|animal model|\\brodent\\b|in vivo|",
                              "cell culture|\\bipsc\\b|induced pluripotent|\\bneuron\\b|hek293|",
                              "primary culture|organoid|brain organoid|\\bin vitro\\b|biochemical|",
                              "recombinant|purified protein|\\bcrispr\\b|transfection|",
                              "cognitive|behavior|behaviour|symptom|diagnosis|clinical|",
                              "\\bmmse\\b|mini-mental state|\\bmoca\\b|neuropsychological|cross-sectional|case-control"), text)
  
  data_type <- if      (is_rnaseq)   "RNA-seq"
  else if (is_wgs)      "Whole Genome Sequencing"
  else if (is_exome)    "Exome Sequencing"
  else if (is_proteom)  "Proteomics"
  else if (is_imaging)  "Imaging"
  else if (is_comp)     "Computational code"
  else if (is_phenotyp) "Phenotypic data"
  else                  "Other"
  
  detected_models <- c(
    if (is_mouse) "Mouse",
    if (is_invitro) "In-vitro",
    if (is_insilico) "In-silico",
    if (is_human) "Human"
  )
  if (length(detected_models) == 0) detected_models <- "Other"
  
  model_type <- if (length(detected_models) > 1) "Multi-model" else detected_models[1]
  model_details_str <- if (model_type == "Multi-model") paste(detected_models, collapse = ", ") else ""
  
  focus_tags <- c(
    if (grepl("\\btau\\b|tauopathy|neurofibrillary|phospho.tau|p-tau",                   text)) "Tau",
    if (grepl("tdp-43|tardbp|tdp43",                                                     text)) "TDP-43",
    if (grepl("alpha.synuclein|\\bsynuclein\\b|lewy body|\\bsnca\\b",                     text)) "\u03b1-Synuclein",
    if (grepl("\\bamyloid\\b|\\babeta\\b|a-beta|\\bapp\\b|presenilin|\\bplaque\\b",        text)) "Amyloid-\u03b2",
    if (grepl("neuroinflamm|\\bmicroglia\\b|\\bastrocyte\\b|cytokine|trem2",              text)) "Neuroinflammation",
    if (grepl("aggregat|\\bfibril\\b|oligomer|misfolded|phase.separ|condensate",         text)) "Protein Aggregation",
    if (grepl("\\bautophagy\\b|lysosom|ubiquitin|proteasome|mitophagy",                  text)) "Autophagy",
    if (grepl("immune|immunity|t cell|b cell|macrophage|lymphocyte|neuroimmun",          text)) "Immune response",
    if (grepl("whole.genome|exome|scrna|transcriptom|single.cell|spatial|sequenc",       text)) "Genomics/Sequencing",
    if (grepl("biomarker|\\bcsf\\b|plasma marker|serum marker|diagnostic marker",        text)) "Biomarkers"
  )
  if (length(focus_tags) == 0) focus_tags <- "Other"
  
  list(
    dataType      = data_type,
    focus         = paste(focus_tags, collapse = ", "),
    model         = model_type,
    model_details = model_details_str,
    reasoning     = "Keyword-matched (Claude not yet run)"
  )
}

classify_da_keywords <- function(da_text, databanks) {
  da_text   <- trimws(da_text   %||% "")
  databanks <- trimws(databanks %||% "")
  
  mk <- function(cat, repo="", acc="",
                 cat_data=NULL, cat_code="",
                 repo_data="", acc_data="", repo_code="", acc_code="") {
    list(da_category      = cat,
         da_repository    = repo,
         da_accession     = acc,
         da_category_data = if (is.null(cat_data)) cat else cat_data,
         da_category_code = cat_code,
         da_repo_data     = repo_data,
         da_acc_data      = acc_data,
         da_repo_code     = repo_code,
         da_acc_code      = acc_code)
  }
  
  if (nchar(databanks) > 0) {
    db_lower <- tolower(databanks)
    if (grepl("dbgap|egas|synapse|amp.pd|niagads|terra|controlled|restricted", db_lower))
      return(mk("Controlled Access", repo=databanks, acc=databanks))
    else
      return(mk("Public Repository", repo=databanks, acc=databanks))
  }
  
  if (nchar(da_text) == 0) return(mk("Not disclosed"))
  
  txt <- tolower(da_text)
  
  code_pat <- paste0("\\bgithub\\.com\\b|\\bzenodo\\b|\\bosf\\b|",
                     "open.science.framework|\\bfigshare\\b|",
                     "code.(?:is|are|was|has.been).(?:publicly.)?(?:available|deposited|shared)|",
                     "analysis.code|source.code|scripts?.(?:are|is).available|",
                     "code.available|software.available")
  has_code  <- grepl(code_pat, txt, perl=TRUE)
  code_repo <- if (has_code) extract_repo_name_code(da_text) else ""
  code_acc  <- if (has_code) extract_accession_code(da_text) else ""
  
  ctrl_pat <- paste0("\\bdbgap\\b|\\begas\\b|\\bsynapse\\b|\\bamp.pd\\b|\\bniagads\\b|",
                     "\\bterra\\b|controlled.access|restricted.access|",
                     "approved.researchers|data.access.committee|upon.approval|",
                     "available.to.qualified|approved.investigators|",
                     "phs\\d+|accession.phs|single.sign.on|gp2\\.org|",
                     "access.application")
  
  pub_pat  <- paste0("\\bgeo\\b|gene.expression.omnibus|\\bsra\\b|sequence.read.archive|",
                     "\\bdryad\\b|\\beuropean.nucleotide\\b|\\bena\\b|",
                     "\\barray.express\\b|\\bpdb\\b|protein.data.bank|",
                     "\\bukbiobank\\b|uk.biobank|\\bnda\\b|nimh.data.archive|",
                     "\\bcil\\b|cell.image.library|",
                     "gse\\d+|srp\\d+|prjna\\d+|",
                     "publicly.(?:available|deposited)|",
                     "openly.available|",
                     "deposited.(?:at|in)|",
                     "available.(?:at|from).(?:https?|the|our)|",
                     "available.at.https|",
                     "can.be.accessed.at|available.online|",
                     "free(?:ly)?.(?:available|accessible)|",
                     "open.access(?:ible)?")
  
  has_email  <- grepl("[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}",
                      da_text, perl=TRUE)
  is_request <- grepl(paste0("available.upon.request|upon.reasonable.request|",
                             "request.to.the.corresponding|available.from.the.author|",
                             "available.on.request|reasonable.request|",
                             "contact.the.corresponding|request.from.the.lead.contact|",
                             "upon.request.from|submit.requests?|",
                             "please.contact|please.submit"), txt)
  is_ctrl    <- grepl(ctrl_pat, txt)
  is_pub     <- grepl(pub_pat,  txt)
  is_notshared <- grepl(paste0("not.available|not.shared|data.cannot|privacy.concern|",
                               "no.data|restrictions.apply|proprietary|confidential|",
                               "cannot.be.shared|not.publicly.available"), txt)
  
  data_repo <- extract_repo_name_data(da_text)
  data_acc  <- extract_accession_data(da_text)
  contact   <- if (has_email && is_request) extract_contact(da_text) else ""
  
  has_ctrl_repo <- grepl(paste0("\\bdbgap\\b|\\begas\\b|\\bsynapse\\b|\\bamp.pd\\b|",
                                "\\bniagads\\b|\\bterra\\b|phs\\d+|accession.phs|",
                                "single.sign.on|gp2\\.org"), txt)
  
  cat_data <- if      (has_email && is_request && !has_ctrl_repo) "Upon Request"
  else if (is_ctrl)                                             "Controlled Access"
  else if (is_pub && !is_notshared)                                   "Public Repository"
  else if (is_pub && is_request)                                      "Upon Request"
  else if (is_request)                                                "Upon Request"
  else                                                                "Not disclosed"
  
  cat_code_val <- if (has_code) "Public Repository" else ""
  
  combined <- if (nchar(cat_code_val) > 0 &&
                  cat_data %in% c("Controlled Access","Upon Request","Not disclosed")) {
    "Mixed"
  } else {
    cat_data
  }
  
  mk(combined,
     repo      = paste(c(data_repo, code_repo)[nchar(c(data_repo,code_repo))>0], collapse="; "),
     acc       = paste(c(if(cat_data=="Upon Request") contact else data_acc,
                         code_acc)[nchar(c(if(cat_data=="Upon Request") contact else data_acc,
                                           code_acc))>0], collapse="; "),
     cat_data  = cat_data,
     cat_code  = cat_code_val,
     repo_data = data_repo,
     acc_data  = if (cat_data=="Upon Request") contact else data_acc,
     repo_code = code_repo,
     acc_code  = code_acc)
}

extract_repo_name <- function(txt) {
  repos <- c("GEO","SRA","Zenodo","Dryad","GitHub","figshare","ArrayExpress",
             "ENA","PDB","OSF","UKBB","NDA","CIL","dbGaP","Synapse","NIAGADS",
             "EGAS","AMP-PD","Terra","NIMH","GP2")
  found <- repos[sapply(repos, function(r) grepl(r, txt, ignore.case=TRUE))]
  if (length(found)>0) paste(found, collapse=", ") else ""
}

extract_repo_name_code <- function(txt) {
  repos <- c("GitHub","Zenodo","OSF","figshare","GitLab","Bitbucket")
  found <- repos[sapply(repos, function(r) grepl(r, txt, ignore.case=TRUE))]
  if (length(found)>0) paste(found, collapse=", ") else ""
}

extract_repo_name_data <- function(txt) {
  repos <- list(
    list(name="GEO",               pat="\\bGEO\\b"),
    list(name="SRA",               pat="\\bSRA\\b"),
    list(name="Dryad",             pat="\\bDryad\\b"),
    list(name="ArrayExpress",      pat="ArrayExpress"),
    list(name="ENA",               pat="\\bENA\\b"),
    list(name="PDB",               pat="\\bPDB\\b"),
    list(name="UKBB",              pat="\\bUKBB\\b|UK Biobank"),
    list(name="NDA",               pat="\\bNDA\\b"),
    list(name="CIL",               pat="\\bCIL\\b|[Cc]ell [Ii]mage [Ll]ibrary"),
    list(name="dbGaP",             pat="\\bdbGaP\\b"),
    list(name="Synapse",           pat="\\bSynapse\\b"),
    list(name="NIAGADS",           pat="\\bNIAGADS\\b"),
    list(name="EGAS",              pat="\\bEGAS\\b"),
    list(name="AMP-PD",            pat="\\bAMP.PD\\b"),
    list(name="Terra",             pat="\\bTerra\\b"),
    list(name="NIMH",              pat="\\bNIMH\\b"),
    list(name="GP2",               pat="\\bGP2\\b")
  )
  found <- sapply(repos, function(r) if (grepl(r$pat, txt, perl=TRUE)) r$name else NA)
  found <- found[!is.na(found)]
  if (length(found)>0) paste(found, collapse=", ") else ""
}

extract_accession <- function(txt) {
  patterns <- c(
    "GSE\\d+", "GSM\\d+", "SRP\\d+", "SRR\\d+", "PRJNA\\d+",
    "EGAS\\d{11}",
    "phs\\d+\\.v\\d+\\.p\\d+|phs\\d+",
    "syn\\d+",
    "SCV\\d+",
    "10\\.\\d{4,9}/zenodo\\.\\d+",
    "doi:\\s*10\\.\\d{4,9}/[^\\s)\\].,;]+",
    "https?://github\\.com/[\\w\\-\\.]+/[\\w\\-\\.]+",
    "github\\.com/[\\w\\-\\.]+/[\\w\\-\\.]+"
  )
  hits <- c()
  for (p in patterns) {
    m <- regmatches(txt, gregexpr(p, txt, perl=TRUE))[[1]]
    m <- sub("[)\\].,;\\s]+$", "", m)
    m <- m[nchar(m) > 0]
    if (length(m) > 0) hits <- c(hits, m)
  }
  hits <- unique(hits)
  bare <- grep("^github\\.com", hits, value=TRUE)
  full <- grep("^https?://github\\.com", hits, value=TRUE)
  if (length(full) > 0 && length(bare) > 0) hits <- hits[!hits %in% bare]
  bare_doi <- grep("^10\\.", hits, value=TRUE)
  pref_doi <- grep("^doi:", hits, value=TRUE)
  if (length(pref_doi) > 0) {
    pref_stripped <- sub("^doi:\\s*", "", pref_doi)
    hits <- hits[!(hits %in% bare_doi & hits %in% pref_stripped | hits %in% bare_doi & bare_doi %in% pref_stripped)]
  }
  if (length(hits) > 0) paste(hits, collapse="; ") else ""
}

extract_accession_code <- function(txt) {
  patterns <- c(
    "https?://github\\.com/[\\w\\-\\.]+/[\\w\\-\\.]+",
    "github\\.com/[\\w\\-\\.]+/[\\w\\-\\.]+",
    "10\\.\\d{4,9}/zenodo\\.\\d+",
    "doi:\\s*10\\.\\d{4,9}/zenodo\\.[^\\s)\\].,;]+"
  )
  hits <- c()
  for (p in patterns) {
    m <- regmatches(txt, gregexpr(p, txt, perl=TRUE))[[1]]
    m <- sub("[)\\].,;\\s]+$", "", m)
    m <- m[nchar(m) > 0]
    if (length(m) > 0) hits <- c(hits, m)
  }
  hits <- unique(hits)
  bare <- grep("^github\\.com", hits, value=TRUE)
  full <- grep("^https?://github\\.com", hits, value=TRUE)
  if (length(full) > 0 && length(bare) > 0) hits <- hits[!hits %in% bare]
  if (length(hits) > 0) paste(hits, collapse="; ") else ""
}

extract_accession_data <- function(txt) {
  patterns <- c(
    "GSE\\d+", "GSM\\d+", "SRP\\d+", "SRR\\d+", "PRJNA\\d+",
    "EGAS\\d{11}",
    "phs\\d+\\.v\\d+\\.p\\d+|phs\\d+",
    "syn\\d+",
    "SCV\\d+",
    "doi:\\s*10\\.(?!5281/zenodo)\\d{4,9}/[^\\s)\\].,;]+",
    "10\\.(?!5281/zenodo)\\d{4,9}/[^\\s)\\].,;]+"
  )
  hits <- c()
  for (p in patterns) {
    m <- regmatches(txt, gregexpr(p, txt, perl=TRUE))[[1]]
    m <- sub("[)\\].,;\\s]+$", "", m)
    m <- m[nchar(m) > 0]
    if (length(m) > 0) hits <- c(hits, m)
  }
  hits      <- unique(hits)
  bare_doi  <- grep("^10\\.", hits, value=TRUE)
  pref_doi  <- sub("^doi:\\s*", "", grep("^doi:", hits, value=TRUE))
  if (length(pref_doi) > 0)
    hits <- hits[!(hits %in% bare_doi & bare_doi %in% pref_doi)]
  if (length(hits) > 0) paste(hits, collapse="; ") else ""
}

extract_contact <- function(txt) {
  if (nchar(trimws(txt)) == 0) return("")
  parts <- c()
  
  email_match <- regmatches(txt, gregexpr(
    "[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}", txt, perl = TRUE))[[1]]
  if (length(email_match) > 0)
    parts <- c(parts, email_match)
  
  name_match <- regmatches(txt, regexpr(
    paste0("(?:submit requests? (?:for[^.]+)?to|",
           "requests? to|contact|lead contact|corresponding author)[:\\s]+",
           "([A-Z][a-zA-Z.]+(?:\\s[A-Z][a-zA-Z.]+){0,3})"),
    txt, perl = TRUE, ignore.case = TRUE))
  if (length(name_match) > 0 && nchar(name_match) > 0) {
    name_clean <- trimws(sub(
      "(?:submit requests? (?:for[^.]+)?to|requests? to|contact|lead contact|corresponding author)[:\\s]+",
      "", name_match, ignore.case = TRUE, perl = TRUE))
    if (nchar(name_clean) > 3 && !name_clean %in% parts)
      parts <- c(name_clean, parts)
  }
  
  if (length(parts) > 0) paste(parts, collapse = " — ") else ""
}

claude_classify_batch <- function(papers_df, api_key, classify_da = FALSE) {
  build_prompt <- function(df) {
    entries <- mapply(function(pmid, title, abstract, da_raw, da_databanks) {
      da_section <- ""
      if (nchar(trimws(da_raw)) > 0)
        da_section <- paste0("\nData Availability Statement: ", substr(trimws(da_raw), 1, 600))
      else if (nchar(trimws(da_databanks)) > 0)
        da_section <- paste0("\nDatabanks deposited: ", trimws(da_databanks))
      sprintf("[PMID:%s]\nTitle: %s\nAbstract: %s%s",
              pmid, title, substr(abstract, 1, 400), da_section)
    }, df$pmid, df$title, df$abstract,
    df$da_raw %||% "", df$da_databanks %||% "", SIMPLIFY = FALSE)
    
    da_instructions <- if (classify_da) paste0(
      '\n\nAlso classify data sharing. Papers often share DATA and CODE separately.\n',
      '"da_category": overall — one of ["Public Repository","Controlled Access","Mixed","Upon Request","Not disclosed"]\n',
      '  Use "Mixed" when data is controlled-access BUT code is publicly available (e.g. GP2/dbGaP + GitHub/Zenodo)\n',
      '"da_category_data": category for the underlying DATA only (same options, no "Mixed")\n',
      '"da_category_code": category for CODE/scripts only — "Public Repository" if code on GitHub/Zenodo/OSF, else ""\n',
      '"da_repository": all repository names found (data + code), comma-separated\n',
      '"da_repo_data": data repository name(s) only (GEO, SRA, dbGaP, Synapse, etc.)\n',
      '"da_acc_data": data accession ID / phs number / DOI, or contact email if Upon Request\n',
      '"da_repo_code": code repository name(s) only (GitHub, Zenodo, OSF, figshare, etc.)\n',
      '"da_acc_code": code repository URL, DOI, or accession\n',
      '"da_accession": combined accession info (data + code)\n',
      'KEY RULES:\n',
      '  - Human genomic/cohort data is almost always Controlled Access (dbGaP/EGAS/Synapse)\n',
      '  - Extract email from parentheses: "Y.T.Q. (yquiroz@mgh.harvard.edu)" → da_acc_data: "Y.T.Q. — yquiroz@mgh.harvard.edu"\n',
      '  - If no DA statement exists at all: da_category = "Not disclosed", all other da_ fields = ""\n'
    ) else ""
    
    paste0(
      'You are a neuroscience research classifier. Classify each paper below.\n\n',
      'Output ONLY a valid JSON array -- no markdown fences, no preamble.\n',
      'Each element must have: "pmid", "data_type", "focus", "model", "model_details", "reasoning"',
      if (classify_da) paste0(
        ', "da_category", "da_category_data", "da_category_code",',
        ' "da_repository", "da_repo_data", "da_acc_data", "da_repo_code", "da_acc_code", "da_accession"'
      ) else "",
      '.\n',
      '"model": one of ["Human", "Mouse", "In-vitro", "In-silico", "Multi-model", "Other"]\n',
      '"model_details": if Multi-model, a comma-separated list of the models (e.g., "In-vitro, Mouse"). Otherwise, leave blank.\n',
      da_instructions,
      '\nPapers:\n\n', paste(entries, collapse = "\n\n")
    )
  }
  
  n        <- nrow(papers_df)
  results <- lapply(seq_len(n), function(i) {
    base <- list(
      pmid          = papers_df$pmid[i],
      data_type     = papers_df$data_type[i] %||% "Other",
      focus         = papers_df$focus[i]     %||% "Other",
      model         = papers_df$model[i]     %||% "Other",
      model_details = papers_df$model_details[i] %||% "",
      reasoning     = "Claude unavailable -- keyword fallback"
    )
    if (classify_da) {
      kw <- classify_da_keywords(papers_df$da_raw[i]       %||% "",
                                 papers_df$da_databanks[i] %||% "")
      base$da_category      <- kw$da_category
      base$da_repository    <- kw$da_repository
      base$da_accession     <- kw$da_accession
      base$da_category_data <- kw$da_category_data
      base$da_category_code <- kw$da_category_code
      base$da_repo_data     <- kw$da_repo_data
      base$da_acc_data      <- kw$da_acc_data
      base$da_repo_code     <- kw$da_repo_code
      base$da_acc_code      <- kw$da_acc_code
    }
    base
  })
  
  tryCatch({
    body <- list(
      model      = "claude-haiku-4-5-20251001",
      max_tokens = 4000,
      messages   = list(list(role = "user", content = build_prompt(papers_df)))
    )
    resp <- POST(
      "https://api.anthropic.com/v1/messages",
      add_headers("x-api-key" = api_key, "anthropic-version" = "2023-06-01",
                  "content-type" = "application/json"),
      body = toJSON(body, auto_unbox = TRUE), timeout = 60
    )
    sc <- status_code(resp)
    if (sc == 200) {
      raw      <- fromJSON(content(resp, "text", encoding = "UTF-8"))
      raw_text <- raw$content[[1]]$text
      clean    <- trimws(gsub("```json|```", "", raw_text))
      parsed   <- tryCatch(fromJSON(clean, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed) && is.list(parsed)) {
        for (item in parsed) {
          idx <- which(papers_df$pmid == as.character(item$pmid))
          if (length(idx) == 1) {
            results[[idx]]$data_type     <- item$data_type %||% "Other"
            results[[idx]]$focus         <- item$focus     %||% "Other"
            results[[idx]]$model         <- item$model     %||% "Other"
            results[[idx]]$model_details <- item$model_details %||% ""
            results[[idx]]$reasoning     <- item$reasoning %||% ""
            if (classify_da) {
              cat_c  <- item$da_category   %||% "Not disclosed"
              repo_c <- item$da_repository %||% ""
              acc_c  <- item$da_accession  %||% ""
              raw_i  <- papers_df$da_raw[idx]      %||% ""
              db_i   <- papers_df$da_databanks[idx] %||% ""
              kw     <- classify_da_keywords(raw_i, db_i)
              results[[idx]]$da_category      <- cat_c
              results[[idx]]$da_repository    <- repo_c
              results[[idx]]$da_accession     <- acc_c
              results[[idx]]$da_category_data <- item$da_category_data %||% kw$da_category_data
              results[[idx]]$da_category_code <- item$da_category_code %||% kw$da_category_code
              results[[idx]]$da_repo_data     <- item$da_repo_data     %||% kw$da_repo_data
              results[[idx]]$da_acc_data      <- item$da_acc_data      %||% kw$da_acc_data
              results[[idx]]$da_repo_code     <- item$da_repo_code     %||% kw$da_repo_code
              results[[idx]]$da_acc_code      <- item$da_acc_code      %||% kw$da_acc_code
            }
          }
        }
      } else {
        showNotification("Claude returned unparseable JSON.", type = "warning")
      }
    } else {
      err_body <- content(resp, "text", encoding = "UTF-8")
      err_msg  <- tryCatch(fromJSON(err_body)$error$message, error = function(e) err_body)
      showNotification(paste0("Claude API error (", sc, "): ", substr(err_msg, 1, 120)),
                       type = "error", duration = 15)
    }
  }, error = function(e) {
    showNotification(paste0("Claude connection error: ", e$message), type = "error", duration = 15)
  })
  
  do.call(rbind, lapply(results, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
}


APP_CSS <- "
body, .wrapper { background-color: #242424 !important; }
.content-wrapper, .main-footer {
  background-color: #242424 !important;
  font-family: 'Helvetica Neue', Arial, sans-serif;
  color: #f0f0f0 !important;
}
.main-sidebar, .left-side { background-color: #1a1a1a !important; }
.sidebar-menu > li > a { color: #999999 !important; }
.sidebar-menu > li.active > a,
.sidebar-menu > li > a:hover { color: #f0f0f0 !important; background-color: #2e2e2e !important; }
.sidebar-menu > li.active > a { border-left: 3px solid #e8a84a !important; }
.main-header .navbar, .main-header .logo {
  background-color: #1a1a1a !important; border-bottom: 2px solid #e8a84a !important; }
.main-header .navbar .nav > li > a { color: #999999 !important; }
.main-header .logo { color: #f0f0f0 !important; font-weight: 700; letter-spacing: 0.3px; font-size: 15px; }
.box {
  background: #2e2e2e !important; border: 1px solid #3a3a3a !important;
  border-top: 2px solid #e8a84a !important; border-radius: 6px !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.5) !important; color: #f0f0f0 !important; }
.box-header { border-bottom: 1px solid #3a3a3a !important; color: #f0f0f0 !important; background: transparent !important; }
.box-header .box-title { color: #f0f0f0 !important; font-weight: 600; font-size: 14px; }

/* Data type / Model badges */
.bdt{border-radius:4px;padding:2px 8px;font-size:11px;font-weight:600;color:white;white-space:nowrap;display:inline-block;}
.bdt-Phenotypicdata{background:#e8694a;}
.bdt-WholeGenomeSequencing{background:#e05fa0;}
.bdt-ExomeSequencing{background:#3dbfbf;}
.bdt-RNAseq{background:#f07c3a;}
.bdt-Imaging{background:#4a7fc1;}
.bdt-Proteomics{background:#5b9e6b;}
.bdt-Computationalcode{background:#9b6bbf;}
.bdt-Human{background:#4a7fc1;}
.bdt-Mouse{background:#e8694a;}
.bdt-Invitro{background:#5b9e6b;}
.bdt-Insilico{background:#9b6bbf;}
.bdt-Multimodel{background:#e8a84a;}
.bdt-Other{background:#7a8999;}

/* Data sharing badges */
.dsh{border-radius:12px;padding:2px 9px;font-size:11px;font-weight:600;color:white;white-space:nowrap;display:inline-block;}
.dsh-PublicRepository{background:#2e8060;}
.dsh-ControlledAccess{background:#c88030;}
.dsh-Mixed{background:#7060a8;}
.dsh-UponRequest{background:#3a6a8a;}
.dsh-Notdisclosed{background:#555555;}

/* Institution badge */
.inst-badge{display:inline-block;background:#1e1e1e;border:1px solid #e8a84a;
  color:#e8a84a;border-radius:4px;padding:1px 6px;font-size:10px;font-weight:600;
  font-family:monospace;margin-left:5px;vertical-align:middle;letter-spacing:0.3px;}

/* Table */
table.dataTable { background-color: #2e2e2e !important; color: #f0f0f0 !important; }
table.dataTable thead th {
  background-color: #242424 !important; color: #e8a84a !important;
  border-bottom: 1px solid #3a3a3a !important; font-size: 12px;
  text-transform: uppercase; letter-spacing: 0.5px; }
table.dataTable tbody tr { background-color: #2e2e2e !important; color: #f0f0f0 !important; }
table.dataTable tbody tr:nth-child(even) { background-color: #282828 !important; }
table.dataTable tbody tr:not(.child) { cursor: pointer; }
table.dataTable tbody tr:hover td { background-color: #383838 !important; }
table.dataTable tbody td { border-top: 1px solid #363636 !important; color: #f0f0f0 !important; }
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_paginate { color: #999999 !important; }
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select {
  background-color: #242424 !important; border: 1px solid #4a4a4a !important;
  color: #f0f0f0 !important; border-radius: 4px; }
.dataTables_wrapper .dataTables_paginate .paginate_button {
  color: #999999 !important; background: #242424 !important;
  border: 1px solid #4a4a4a !important; border-radius: 4px; }
.dataTables_wrapper .dataTables_paginate .paginate_button.current,
.dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: #e8a84a !important; color: #1a1a1a !important; border-color: #e8a84a !important; }

/* Detail panel */
.claude-badge{display:inline-block;background:#2a2a2a;border:1px solid #e8a84a;color:#e8a84a;
  border-radius:12px;padding:2px 10px;font-size:11px;font-weight:600;margin-left:6px;}
.detail-panel{background:#242424;border:1px solid #3a3a3a;border-left:3px solid #e8a84a;
  border-radius:8px;padding:16px;margin:4px 0;}
.detail-panel h4{color:#f0f0f0;margin-bottom:10px;font-size:16px;}
.detail-panel p{color:#aaaaaa;margin-bottom:6px;font-size:13px;}
.detail-panel .abstract-text{color:#dddddd;font-size:12px;line-height:1.7;}
.reasoning-box{background:#1e1e1e;border-left:3px solid #5a9a7a;padding:8px 12px;
  border-radius:0 6px 6px 0;margin-top:8px;font-size:12px;color:#aaaaaa;font-style:italic;}
.da-box{background:#1e2a1e;border:1px solid #3a5a3a;border-left:3px solid #5abf80;
  border-radius:0 6px 6px 0;padding:10px 14px;margin-top:10px;font-size:12px;color:#c0e0c0;}
.da-box.controlled{background:#2a2010;border-color:#5a4a10;border-left-color:#c88030;color:#e0d090;}
.da-box.request{background:#10202a;border-color:#1a4a6a;border-left-color:#4a9abf;color:#90c0e0;}
.da-box.notdisclosed{background:#1e1e1e;border-color:#3a3a3a;border-left-color:#666666;color:#888888;}
.da-box + .da-box{margin-top:4px;}
.da-stmt-preview{font-size:11px;opacity:0.75;margin-top:6px;font-style:italic;line-height:1.6;}
.da-stmt-full{font-size:11px;opacity:0.75;margin-top:6px;font-style:italic;line-height:1.6;display:none;}
.da-more-btn{background:none;border:none;color:inherit;opacity:0.6;font-size:10px;cursor:pointer;
  padding:2px 0;text-decoration:underline;font-style:normal;margin-top:2px;display:block;}
.da-more-btn:hover{opacity:1;}

/* Misc */
.filter-badge{display:inline-block;background:#242424;border:1px solid #e8a84a;color:#e8a84a;
  border-radius:20px;padding:3px 12px;font-size:11px;font-family:monospace;
  text-align:center;width:100%;margin-top:6px;}
.api-key-box{background:#242424;border:1px solid #3a3a3a;border-radius:8px;padding:14px;}
.api-status-ok{color:#5abf80;font-size:13px;font-weight:600;}
.api-status-warn{color:#e8a84a;font-size:13px;}
.api-status-err{color:#e05c5c;font-size:13px;}
.classify-progress{background:#242424;border:1px solid #3a3a3a;border-radius:8px;
  padding:10px 14px;margin-top:8px;font-size:12px;color:#aaaaaa;}
.overview-pubs-count{display:inline-block;background:#1e1e1e;border:1px solid #e8a84a;
  color:#e8a84a;border-radius:12px;padding:1px 10px;font-size:12px;
  font-family:monospace;margin-left:8px;vertical-align:middle;}
.form-control {background-color: #1e1e1e !important; border: 1px solid #4a4a4a !important; color: #f0f0f0 !important;}
.selectize-input {background-color: #1e1e1e !important; border: 1px solid #4a4a4a !important; color: #f0f0f0 !important;}
.selectize-input .item {background: #f0f0f0 !important; color: #1a1a1a !important; border: 1px solid #cccccc !important; border-radius: 3px !important;}
.bootstrap-select .dropdown-toggle, .bootstrap-select button {background-color: #1e1e1e !important; border: 1px solid #4a4a4a !important; color: #f0f0f0 !important;}
.bootstrap-select .dropdown-menu {background-color: #2e2e2e !important; border: 1px solid #4a4a4a !important;}
.bootstrap-select .dropdown-menu li a, .bootstrap-select .dropdown-menu li a span.text {color: #f0f0f0 !important; background-color: transparent !important;}
.bootstrap-select .dropdown-menu li.selected a, .bootstrap-select .dropdown-menu li a:hover {background-color: #3a3a3a !important; color: #f0f0f0 !important;}
.bootstrap-select .filter-option-inner-inner { color: #f0f0f0 !important; }
.bootstrap-select .bs-searchbox input {background-color: #242424 !important; border: 1px solid #555555 !important; color: #f0f0f0 !important;}
.selectize-dropdown {background-color: #2e2e2e !important; border: 1px solid #4a4a4a !important;}
.selectize-dropdown .option { color: #f0f0f0 !important; }
.selectize-dropdown .option:hover, .selectize-dropdown .option.active {background-color: #3a3a3a !important; color: #f0f0f0 !important;}
.checkbox label, label { color: #aaaaaa !important; }
a.action-button, a[id^='sel_'] { color: #e8a84a !important; }
.btn-primary { background-color: #e8a84a !important; border-color: #d09030 !important; color: #1a1a1a !important; font-weight: 700; }
.btn-primary:hover { background-color: #f0b858 !important; }
.btn-warning { background-color: #3a6a8a !important; border-color: #4a7a9a !important; color: #ffffff !important; font-weight: 600; }
.btn-warning:hover { background-color: #4a7a9a !important; }
.btn-default { background-color: #2e2e2e !important; border-color: #4a4a4a !important; color: #f0f0f0 !important; }
.btn-default:hover { background-color: #3a3a3a !important; }
.btn-success { background-color: #2e8060 !important; border-color: #3a9870 !important; color: #ffffff !important; font-weight: 600; }
.btn-success:hover { background-color: #3a9870 !important; }
.irs--shiny .irs-bar { background: #e8a84a !important; border-top-color: #e8a84a !important; border-bottom-color: #e8a84a !important; }
.irs--shiny .irs-handle { background: #e8a84a !important; border-color: #d09030 !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background: #c88030 !important; }
.irs--shiny .irs-line { background: #3a3a3a !important; }
.irs-min, .irs-max { color: #999999 !important; background: #1e1e1e !important; }
.progress { background-color: #1e1e1e !important; }
.progress-bar { background-color: #e8a84a !important; }
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: #1a1a1a; }
::-webkit-scrollbar-thumb { background: #4a4a4a; border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: #e8a84a; }
a { color: #e8a84a !important; } a:hover { color: #f8c870 !important; }
hr { border-color: #3a3a3a !important; }
.material-switch > label::before { background: #3a3a3a !important; }
.material-switch > input:checked + label::before { background: #c88030 !important; }
.material-switch > input:checked + label::after { background: #e8a84a !important; }
"


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = HTML("Publication Dashboard"),
    tags$li(class = "dropdown", style = "padding:8px 16px 0 0;",
            uiOutput("header_claude_status"))
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Overview",      tabName = "dash"),
      menuItem("Settings",      tabName = "sets")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML(APP_CSS))),
    tabItems(
      
      tabItem(tabName = "dash",
              fluidRow(
                box(width = 12, status = "primary",
                    title = "Search Publications from PubMed",
                    div(style = "background:#2a2418;border:1px solid #5a4010;border-left:3px solid #e8a84a;border-radius:6px;padding:12px 16px;margin-bottom:14px;",
                        tags$p(style = "margin:0 0 6px;font-size:13px;color:#e8a84a;font-weight:600;",
                               icon("circle-info"), "  How to use"),
                        tags$ul(style = "margin:0;padding-left:18px;color:#aaaaaa;font-size:12.5px;line-height:1.9;",
                                tags$li("Select one or more scientists from the dropdown, then click ",
                                        tags$strong("Search"), " \u2014 or click ",
                                        tags$strong("Search All"), " to fetch all 73 researchers."),
                                tags$li("This performs a ", tags$strong("live search on PubMed"),
                                        " using the E-utilities API. For PMC full-text data availability",
                                        " extraction, fetching may take longer per paper."),
                                tags$li("Use the filters below to narrow results. Click any table row to expand details.")
                        )
                    ),
                    fluidRow(
                      column(5,
                             pickerInput("sel_scientists", "Select scientists",
                                         choices  = SCIENTIST_NAMES, multiple = TRUE,
                                         options  = list(`live-search` = TRUE, `actions-box` = TRUE,
                                                         `selected-text-format` = "count > 3",
                                                         `count-selected-text`  = "{0} scientists"))),
                      column(2, br(), actionButton("btn_fetch_sel", "Search",     class = "btn-primary btn-block")),
                      column(2, br(), actionButton("btn_fetch_all", "Search All", class = "btn-warning btn-block")),
                      column(3, br(), uiOutput("fetch_status_ui"))
                    ),
                    uiOutput("progress_ui"),
                    uiOutput("claude_progress_ui"),
                    hr(style = "border-color:#e2e8f0;margin:14px 0 10px;"),
                    fluidRow(
                      column(2,
                             tags$label("Year Range", style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             sliderInput("f_year", label = NULL, min = 2020, max = 2026, value = c(2020, 2026), step = 1, sep = "", ticks = FALSE),
                             uiOutput("year_debug")),
                      column(2,
                             tags$label("Data Type", style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;", checkboxGroupInput("f_datatype", label = NULL, choices = ALL_DATA_TYPES_FILTER, selected = ALL_DATA_TYPES_FILTER, inline = FALSE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;", actionLink("sel_all_types", "All", style = "font-size:11px;"), span("|", style = "color:#cbd5e1;font-size:11px;"), actionLink("sel_none_types", "None", style = "font-size:11px;"))),
                      column(2,
                             tags$label("Study Focus", style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;", checkboxGroupInput("f_focus", label = NULL, choices = ALL_FOCUS_TAGS, selected = ALL_FOCUS_TAGS, inline = FALSE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;", actionLink("sel_all_focus", "All", style = "font-size:11px;"), span("|", style = "color:#cbd5e1;font-size:11px;"), actionLink("sel_none_focus", "None", style = "font-size:11px;"))),
                      column(2,
                             tags$label("Model", style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;", checkboxGroupInput("f_model", label = NULL, choices = ALL_MODELS_FILTER, selected = ALL_MODELS_FILTER, inline = FALSE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;", actionLink("sel_all_models", "All", style = "font-size:11px;"), span("|", style = "color:#cbd5e1;font-size:11px;"), actionLink("sel_none_models", "None", style = "font-size:11px;"))),
                      column(2,
                             tags$label("Data Sharing", style = "color:#475569;font-size:11px;text-transform:uppercase;letter-spacing:.8px;"),
                             div(style = "margin-top:4px;", checkboxGroupInput("f_sharing", label = NULL, choices = DATA_SHARING_FILTER_LEVELS, selected = DATA_SHARING_FILTER_LEVELS, inline = FALSE)),
                             div(style = "display:flex;gap:6px;margin-top:2px;", actionLink("sel_all_sharing", "All", style = "font-size:11px;"), span("|", style = "color:#cbd5e1;font-size:11px;"), actionLink("sel_none_sharing", "None", style = "font-size:11px;"))),
                      column(2, br(), actionButton("btn_reset_filters", "Reset Filters", class = "btn-default btn-block btn-sm", icon = icon("undo")), uiOutput("filter_count_badge"))
                    )
                )
              ),
              fluidRow(
                box(width = 12, status = "primary",
                    title = uiOutput("overview_pubs_title"),
                    collapsible = TRUE, collapsed = FALSE,
                    withSpinner(DTOutput("overview_pub_table"), color = "#3b63c8"))
              ),
              fluidRow(
                box(width = 3, status = "primary", title = "Data Type",
                    withSpinner(plotlyOutput("plot_datatype", height = "320px"), color = "#3b63c8")),
                box(width = 3, status = "primary", title = "Study Focus",
                    withSpinner(plotlyOutput("plot_focus", height = "320px"), color = "#3b63c8")),
                box(width = 3, status = "primary", title = "Model",
                    withSpinner(plotlyOutput("plot_model", height = "320px"), color = "#3b63c8")),
                box(width = 3, status = "primary", title = "Data Sharing",
                    withSpinner(plotlyOutput("plot_sharing", height = "320px"), color = "#3b63c8"))
              )
      ),
      
      
      tabItem(tabName = "sets",
              fluidRow(
                box(width = 5, status = "primary", title = "Fetch Settings",
                    sliderInput("max_per_sci", "Max publications per scientist",
                                min = 5, max = 100, value = 50, step = 5),
                    br(),
                    materialSwitch("fetch_pmc_da",
                                   "Fetch PMC full-text for data availability (slower, more accurate)",
                                   value = TRUE, status = "primary"),
                    sliderInput("pmc_workers", "PMC papers per scientist (max DA lookups)",
                                min = 1, max = 8, value = 5, step = 1),
                    p("Limits how many PMC full-text lookups are done per scientist.",
                      style = "color:#64748b;font-size:12px;margin-top:-8px;"),
                    p("When enabled, papers with no data availability in PubMed XML will be looked up",
                      " in PubMed Central. Only works for open-access articles.",
                      style = "color:#64748b;font-size:12px;margin-top:4px;"),
                    br(),
                    
                    div(class = "api-key-box", style = "margin-bottom:14px;",
                        tags$label("Claude API Key", 
                                   style = "color:#475569;font-size:13px;margin-bottom:6px;display:block;"),
                        passwordInput("api_key_input", label = NULL, 
                                      placeholder = "Paste your sk-ant-... key here"),
                        actionButton("btn_save_key", "Save Claude Key", 
                                     class = "btn-primary btn-sm", icon = icon("key")),
                        uiOutput("api_key_feedback"),
                        
                        div(style = "color:#888888;font-size:11.5px;margin-top:12px;line-height:1.5;border-top:1px solid #3a3a3a;padding-top:8px;",
                            strong("What it does: ", style="color:#aaaaaa;"), 
                            "Powers the AI to read abstract contexts.", br(),
                            strong("How to get a key:", style="color:#aaaaaa;"),
                            tags$ol(style = "padding-left:16px; margin-top:4px; margin-bottom:0;",
                                    tags$li(tags$a(href="https://console.anthropic.com/", target="_blank", "Log in to the Anthropic Console", icon("external-link-alt", style="font-size:10px;"))),
                                    
                                    tags$li("Go to Settings > API Keys, click 'Create Key', and paste it above.")
                            )
                        )
                    ),
                    
                    div(class = "api-key-box", style = "margin-bottom:14px;",
                        tags$label("NCBI API Key",
                                   style = "color:#475569;font-size:13px;margin-bottom:6px;display:block;"),
                        passwordInput("ncbi_api_key_input", label = NULL,
                                      placeholder = "Paste NCBI key for faster fetching..."),
                        actionButton("btn_save_ncbi_key", "Save NCBI Key",
                                     class = "btn-primary btn-sm", icon = icon("key")),
                        uiOutput("ncbi_key_feedback"),
                        
                        div(style = "color:#888888;font-size:11.5px;margin-top:12px;line-height:1.5;border-top:1px solid #3a3a3a;padding-top:8px;",
                            strong("What it does: ", style="color:#aaaaaa;"), 
                            "Fetches publications from PubMed. A key verifies your identity and boosts your fetch speed limit from 3 to 10 requests per second (over 3x faster).", br(),
                            strong("How to get a key:", style="color:#aaaaaa;"),
                            tags$ol(style = "padding-left:16px; margin-top:4px; margin-bottom:0;",
                                    tags$li(tags$a(href="https://www.ncbi.nlm.nih.gov/", target="_blank", "Log in to NCBI", icon("external-link-alt", style="font-size:10px;")), " using a 3rd-party account (Google, university, etc.)."),
                                    tags$li("Click your username (top right) to go to Account Settings."),
                                    tags$li("Scroll to 'API Key Management', click 'Create an API Key', and paste it above.")
                            )
                        )
                    ),
                    
                    div(style = "background:#1e2a1e;border:1px solid #2e4a2e;border-radius:8px;padding:12px;",
                        p(icon("check-circle"), " PubMed E-utilities: Adaptive speed limits enabled.",
                          style = "color:#5abf80;font-size:13px;margin:0 0 4px;font-weight:600;"),
                        p("Claude Haiku is used for classification.",
                          style = "color:#888888;font-size:12px;margin:0;"))
                ),
                box(width = 4, status = "primary", title = "Export",
                    downloadButton("dl_csv",     "Download CSV (filtered)", class = "btn-primary btn-block"),
                    br(), br(),
                    downloadButton("dl_csv_all", "Download CSV (all)",      class = "btn-default btn-block"),
                    br(),
                    p("Fields: pmid, title, authors, journal, year, scientist, institution, data_type, focus, reasoning, da_category, da_category_data, da_category_code, da_repo_data, da_acc_data, da_repo_code, da_acc_code, da_raw.",
                      style = "color:#475569;font-size:12px;margin-top:8px;")),
                box(width = 3, status = "primary", title = "Data Sharing Categories",
                    div(style = "font-size:12px;color:#aaaaaa;line-height:2;",
                        div(style = "margin-bottom:10px;",
                            span(style = "display:inline-block;background:#2e8060;color:#fff;border-radius:10px;padding:1px 8px;font-size:11px;font-weight:600;margin-right:6px;",
                                 "\U0001F7E2 Public Repository"),
                            br(),
                            span(style = "color:#888;font-size:11px;",
                                 "GEO, SRA, Zenodo, Dryad, GitHub, figshare, ArrayExpress, ENA, OSF, CIL \u2014 with accession ID or link")
                        ),
                        div(style = "margin-bottom:10px;",
                            span(style = "display:inline-block;background:#c88030;color:#fff;border-radius:10px;padding:1px 8px;font-size:11px;font-weight:600;margin-right:6px;",
                                 "\U0001F7E1 Controlled Access"),
                            br(),
                            span(style = "color:#888;font-size:11px;",
                                 "dbGaP, AMP-PD, Synapse, NIAGADS, EGAS, Terra \u2014 requires application or data use agreement, with ID/link")
                        ),
                        div(style = "margin-bottom:10px;",
                            span(style = "display:inline-block;background:#7060a8;color:#fff;border-radius:10px;padding:1px 8px;font-size:11px;font-weight:600;margin-right:6px;",
                                 "\U0001F7E3 Mixed"),
                            br(),
                            span(style = "color:#888;font-size:11px;",
                                 "Controlled-access data + publicly available code \u2014 e.g. GP2/dbGaP data with GitHub or Zenodo code")
                        ),
                        div(style = "margin-bottom:10px;",
                            span(style = "display:inline-block;background:#3a6a8a;color:#fff;border-radius:10px;padding:1px 8px;font-size:11px;font-weight:600;margin-right:6px;",
                                 "\U0001F535 Upon Request"),
                            br(),
                            span(style = "color:#888;font-size:11px;",
                                 "Available from corresponding author on request \u2014 contact email extracted where present")
                        ),
                        div(style = "margin-bottom:10px;",
                            span(style = "display:inline-block;background:#555555;color:#fff;border-radius:10px;padding:1px 8px;font-size:11px;font-weight:600;margin-right:6px;",
                                 "\u26AA Not disclosed"),
                            br(),
                            span(style = "color:#888;font-size:11px;",
                                 "Data are explicitly not shared, or no availability statement was found.")
                        )
                    )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    publications       = data.frame(),
    fetching           = FALSE,
    fetch_msg          = "",
    fetch_prog         = 0,
    classifying        = FALSE,
    classify_msg       = "",
    classify_prog      = 0,
    api_key            = "",
    api_key_valid      = FALSE,
    api_key_ncbi       = "",
    api_key_ncbi_valid = FALSE
  )
  
  filtered_pubs <- reactive({
    df <- rv$publications
    if (nrow(df) == 0) return(df)
    if (!is.null(input$f_year) && length(input$f_year) == 2) {
      yr <- suppressWarnings(as.integer(trimws(as.character(df$year))))
      df <- df[!is.na(yr) & yr >= input$f_year[1] & yr <= input$f_year[2], ]
    }
    if (!is.null(input$f_focus) && length(input$f_focus) < length(ALL_FOCUS_TAGS)) {
      pat <- paste(gsub("([.()+*])", "\\\\\\1", input$f_focus), collapse = "|")
      df  <- df[grepl(pat, df$focus, ignore.case = TRUE), ]
    }
    if (!is.null(input$f_datatype) && length(input$f_datatype) < length(ALL_DATA_TYPES_FILTER)) {
      known_match <- df$data_type %in% input$f_datatype
      unknown_row <- df$data_type == "Other"
      df <- df[known_match | unknown_row, ]
    }
    if (!is.null(input$f_model) && length(input$f_model) < length(ALL_MODELS_FILTER)) {
      known_match <- df$model %in% input$f_model
      unknown_row <- df$model == "Other"
      df <- df[known_match | unknown_row, ]
    }
    if (!is.null(input$f_sharing) && length(input$f_sharing) < length(DATA_SHARING_FILTER_LEVELS)) {
      if ("da_category" %in% names(df))
        df <- df[df$da_category %in% input$f_sharing, ]
    }
    type_order <- c(ALL_DATA_TYPES_FILTER, "Other")
    df$data_type <- factor(df$data_type, levels = type_order)
    df <- df[order(df$data_type), ]
    df$data_type <- as.character(df$data_type)
    df
  })
  
  observeEvent(input$btn_save_key, {
    key <- trimws(input$api_key_input)
    if (nchar(key) > 20 && startsWith(key, "sk-ant")) {
      rv$api_key <- key; rv$api_key_valid <- TRUE
      showNotification("Claude API key saved.", type = "message")
    } else { rv$api_key_valid <- FALSE; showNotification("Key format looks wrong.", type = "error") }
  })
  observeEvent(input$btn_save_ncbi_key, {
    key <- trimws(input$ncbi_api_key_input)
    if (nchar(key) > 10) {
      rv$api_key_ncbi <- key; rv$api_key_ncbi_valid <- TRUE
      showNotification("NCBI API key saved! Fetching speed increased.", type = "message")
    } else {
      rv$api_key_ncbi <- ""; rv$api_key_ncbi_valid <- FALSE
      showNotification("NCBI key cleared.", type = "warning")
    }
  })
  
  output$api_key_feedback <- renderUI({
    if (rv$api_key_valid) div(class = "api-status-ok", icon("check-circle"), " Key active")
    else if (nchar(rv$api_key) == 0) div(style = "color:#64748b;font-size:12px;")
    else div(class = "api-status-err", icon("times-circle"), " Key format error")
  })
  output$ncbi_key_feedback <- renderUI({
    if (rv$api_key_ncbi_valid)
      div(class = "api-status-ok", style = "margin-top:8px;", icon("check-circle"), " NCBI Key Active (Fast Mode)")
    else
      div(style = "color:#64748b;font-size:12px;margin-top:8px;", "No key \u2014 Standard Speed Limit")
  })
  output$header_claude_status <- renderUI({
    if (rv$classifying) span(class = "claude-badge", icon("spinner", class = "fa-spin"), " Classifying...")
    else if (rv$api_key_valid) span(class = "claude-badge", icon("robot"), " Claude active")
    else span(style = "color:#64748b;font-size:11px;padding-top:14px;display:block;", "\u2328 keyword mode")
  })
  output$ai_api_key_status <- renderUI({
    if (rv$api_key_valid) div(class = "api-status-ok", icon("check-circle"), " API key active")
    else div(class = "api-status-warn", icon("exclamation-triangle"), " No API key \u2014 enter key to enable Claude")
  })
  
  do_fetch <- function(scientists) {
    rv$fetching     <- TRUE
    rv$publications <- data.frame()
    total      <- length(scientists)
    all_df     <- list()
    seen_pmids <- character(0)
    use_pmc    <- isTRUE(isolate(input$fetch_pmc_da))
    
    for (i in seq_along(scientists)) {
      sci           <- scientists[[i]]
      rv$fetch_msg  <- sprintf("Fetching: %s  (%d / %d)", sci$name, i, total)
      rv$fetch_prog <- round((i - 1) / total * 100)
      updateProgressBar(session, "fetch_progress", value = rv$fetch_prog)
      
      pmids     <- fetch_pmids(sci$name, sci$affil,
                               max_results = isolate(input$max_per_sci),
                               api_key     = rv$api_key_ncbi,
                               aliases     = sci$aliases)
      new_pmids  <- setdiff(pmids, seen_pmids)
      seen_pmids <- c(seen_pmids, new_pmids)
      
      if (length(new_pmids) > 0) {
        details <- fetch_details(new_pmids, api_key = rv$api_key_ncbi)
        if (!is.null(details) && nrow(details) > 0) {
          details$scientist   <- sci$name
          details$institution <- INST_LABELS[sci$affil] %||% sci$affil
          
          cls               <- mapply(classify_keywords, details$title, details$abstract, SIMPLIFY = FALSE)
          details$data_type     <- sapply(cls, `[[`, "dataType")
          details$focus         <- sapply(cls, `[[`, "focus")
          details$model         <- sapply(cls, `[[`, "model")
          details$model_details <- sapply(cls, `[[`, "model_details")
          details$reasoning     <- sapply(cls, `[[`, "reasoning")
          
          da_kw <- mapply(classify_da_keywords,
                          details$da_raw %||% "",
                          details$da_databanks %||% "",
                          SIMPLIFY = FALSE)
          details$da_category      <- sapply(da_kw, `[[`, "da_category")
          details$da_repository    <- sapply(da_kw, `[[`, "da_repository")
          details$da_accession     <- sapply(da_kw, `[[`, "da_accession")
          details$da_category_data <- sapply(da_kw, `[[`, "da_category_data")
          details$da_category_code <- sapply(da_kw, `[[`, "da_category_code")
          details$da_repo_data     <- sapply(da_kw, `[[`, "da_repo_data")
          details$da_acc_data      <- sapply(da_kw, `[[`, "da_acc_data")
          details$da_repo_code     <- sapply(da_kw, `[[`, "da_repo_code")
          details$da_acc_code      <- sapply(da_kw, `[[`, "da_acc_code")
          
          if (use_pmc) {
            not_found_idx <- which(details$da_category == "Not disclosed")
            if (length(not_found_idx) > 0) {
              nf_pmids <- details$pmid[not_found_idx]
              nf_dois  <- if ("doi" %in% names(details)) details$doi[not_found_idx] else rep("", length(not_found_idx))
              rv$fetch_msg <- sprintf(
                "PMC + DOI data availability: %s — fetching %d papers...",
                sci$name, length(nf_pmids))
              pmc_results <- fetch_pmc_da_batch(nf_pmids, api_key = rv$api_key_ncbi,
                                                workers = isolate(input$pmc_workers) %||% 5,
                                                dois = nf_dois)
              for (res in pmc_results) {
                if (!is.list(res) || is.null(res$pmid)) next
                j <- which(details$pmid == res$pmid)
                if (length(j) == 1 && nchar(res$da_text) > 0) {
                  details$da_raw[j] <- res$da_text
                  kw <- classify_da_keywords(res$da_text, details$da_databanks[j] %||% "")
                  details$da_category[j]      <- kw$da_category
                  details$da_repository[j]    <- kw$da_repository
                  details$da_accession[j]     <- kw$da_accession
                  details$da_category_data[j] <- kw$da_category_data
                  details$da_category_code[j] <- kw$da_category_code
                  details$da_repo_data[j]     <- kw$da_repo_data
                  details$da_acc_data[j]      <- kw$da_acc_data
                  details$da_repo_code[j]     <- kw$da_repo_code
                  details$da_acc_code[j]      <- kw$da_acc_code
                }
              }
            }
          }
          
          all_df[[length(all_df) + 1]] <- details
          rv$publications <- do.call(rbind, all_df)
        }
      }
      Sys.sleep(0.05)
    }
    
    rv$fetch_msg  <- ""
    rv$fetch_prog <- 100
    rv$fetching   <- FALSE
    if (rv$api_key_valid && isTRUE(isolate(input$claude_auto)))
      shinyjs::delay(600, do_claude_classify())
  }
  
  observeEvent(input$btn_fetch_sel, {
    req(!rv$fetching)
    scientists <- if (length(input$sel_scientists) > 0)
      Filter(function(s) s$name %in% input$sel_scientists, SCIENTISTS)
    else SCIENTISTS
    do_fetch(scientists)
  })
  observeEvent(input$btn_fetch_all, { req(!rv$fetching); do_fetch(SCIENTISTS) })
  
  do_claude_classify <- function() {
    req(rv$api_key_valid)
    df <- rv$publications
    req(nrow(df) > 0)
    if (rv$classifying) return()
    classify_da      <- isTRUE(isolate(input$claude_da))
    batch_size       <- isolate(input$claude_batch_size) %||% 8
    n                <- nrow(df)
    batches          <- split(seq_len(n), ceiling(seq_len(n) / batch_size))
    total_b          <- length(batches)
    rv$classifying   <- TRUE
    rv$classify_prog <- 0
    
    for (bi in seq_along(batches)) {
      idx   <- batches[[bi]]
      batch <- df[idx, , drop = FALSE]
      rv$classify_msg  <- sprintf("Claude classifying batch %d / %d  (%d papers)...", bi, total_b, nrow(batch))
      rv$classify_prog <- round((bi - 1) / total_b * 100)
      updateProgressBar(session, "classify_progress", value = rv$classify_prog)
      
      result <- claude_classify_batch(batch, rv$api_key, classify_da = classify_da)
      
      if (!is.null(result) && nrow(result) > 0) {
        for (j in seq_len(nrow(result))) {
          row_idx <- which(rv$publications$pmid == result$pmid[j])
          if (length(row_idx) == 1) {
            rv$publications$data_type[row_idx]     <- result$data_type[j]
            rv$publications$focus[row_idx]         <- result$focus[j]
            rv$publications$model[row_idx]         <- result$model[j]
            rv$publications$model_details[row_idx] <- result$model_details[j]
            rv$publications$reasoning[row_idx]     <- result$reasoning[j]
            if (classify_da && "da_category" %in% names(result)) {
              rv$publications$da_category[row_idx]      <- result$da_category[j]
              rv$publications$da_repository[row_idx]    <- result$da_repository[j]
              rv$publications$da_accession[row_idx]     <- result$da_accession[j]
              if ("da_category_data" %in% names(result))
                rv$publications$da_category_data[row_idx] <- result$da_category_data[j]
              if ("da_category_code" %in% names(result))
                rv$publications$da_category_code[row_idx] <- result$da_category_code[j]
              if ("da_repo_data" %in% names(result))
                rv$publications$da_repo_data[row_idx]     <- result$da_repo_data[j]
              if ("da_acc_data"  %in% names(result))
                rv$publications$da_acc_data[row_idx]      <- result$da_acc_data[j]
              if ("da_repo_code" %in% names(result))
                rv$publications$da_repo_code[row_idx]     <- result$da_repo_code[j]
              if ("da_acc_code"  %in% names(result))
                rv$publications$da_acc_code[row_idx]      <- result$da_acc_code[j]
            }
          }
        }
      }
      Sys.sleep(0.1)
    }
    
    rv$classify_prog <- 100; rv$classify_msg <- ""; rv$classifying <- FALSE
    showNotification(sprintf("Claude classified %d publications.", n), type = "message", duration = 5)
  }
  observeEvent(input$btn_classify_now, { do_claude_classify() })
  
  output$fetch_status_ui <- renderUI({
    if (rv$fetching)
      div(style = "color:#2563eb;font-size:13px;", icon("spinner", class = "fa-spin"), " ", rv$fetch_msg)
    else if (nrow(rv$publications) > 0)
      div(style = "color:#16a34a;font-size:13px;", icon("check-circle"),
          sprintf(" %d publications loaded", nrow(rv$publications)))
  })
  output$progress_ui <- renderUI({
    if (!rv$fetching) return(NULL)
    div(style = "margin-top:10px;",
        progressBar("fetch_progress", value = rv$fetch_prog, display_pct = TRUE, striped = TRUE, status = "info"))
  })
  output$claude_progress_ui <- renderUI({
    if (!rv$classifying) return(NULL)
    div(class = "classify-progress",
        icon("robot"), " ", rv$classify_msg, br(),
        progressBar("classify_progress", value = rv$classify_prog, display_pct = TRUE,
                    striped = TRUE, status = "primary"))
  })
  output$ai_classify_controls <- renderUI({
    if (!rv$api_key_valid) return(p("Enter your API key to enable Claude.", style = "color:#64748b;font-size:13px;"))
    n <- nrow(rv$publications)
    if (n == 0) return(p("Fetch publications first.", style = "color:#64748b;font-size:13px;"))
    tagList(
      p(sprintf("%d publications loaded.", n), style = "color:#475569;font-size:13px;"),
      actionButton("btn_classify_now", "Classify / Re-classify All with Claude",
                   class = "btn-success", icon = icon("robot"))
    )
  })
  
  observeEvent(input$sel_all_types,    { updateCheckboxGroupInput(session, "f_datatype", selected = ALL_DATA_TYPES_FILTER) })
  observeEvent(input$sel_none_types,   { updateCheckboxGroupInput(session, "f_datatype", selected = character(0)) })
  observeEvent(input$sel_all_focus,    { updateCheckboxGroupInput(session, "f_focus",    selected = ALL_FOCUS_TAGS) })
  observeEvent(input$sel_none_focus,   { updateCheckboxGroupInput(session, "f_focus",    selected = character(0)) })
  observeEvent(input$sel_all_models,   { updateCheckboxGroupInput(session, "f_model",    selected = ALL_MODELS_FILTER) })
  observeEvent(input$sel_none_models,  { updateCheckboxGroupInput(session, "f_model",    selected = character(0)) })
  observeEvent(input$sel_all_sharing,  { updateCheckboxGroupInput(session, "f_sharing",  selected = DATA_SHARING_FILTER_LEVELS) })
  observeEvent(input$sel_none_sharing, { updateCheckboxGroupInput(session, "f_sharing",  selected = character(0)) })
  observeEvent(input$btn_reset_filters, {
    updateSliderInput(session,        "f_year",     value = c(2020, 2026))
    updateCheckboxGroupInput(session, "f_datatype", selected = ALL_DATA_TYPES_FILTER)
    updateCheckboxGroupInput(session, "f_focus",    selected = ALL_FOCUS_TAGS)
    updateCheckboxGroupInput(session, "f_model",    selected = ALL_MODELS_FILTER)
    updateCheckboxGroupInput(session, "f_sharing",  selected = DATA_SHARING_FILTER_LEVELS)
  })
  
  output$year_debug <- renderUI({
    df <- rv$publications
    if (nrow(df) == 0) return(NULL)
    yrs <- sort(unique(trimws(as.character(df$year))), decreasing = TRUE)
    div(style = "font-size:10px;color:#475569;margin-top:3px;", paste("In data:", paste(yrs, collapse = ", ")))
  })
  output$filter_count_badge <- renderUI({
    n_f <- nrow(filtered_pubs()); n_t <- nrow(rv$publications)
    if (n_t == 0) return(NULL)
    lbl <- if (n_f == n_t) sprintf("All %d pubs", n_t) else sprintf("%d / %d", n_f, n_t)
    div(class = "filter-badge", lbl)
  })
  
  output$plot_datatype <- renderPlotly({
    df <- filtered_pubs()
    if (nrow(df) == 0) return(plotly_empty())
    counts <- as.data.frame(table(data_type = df$data_type), stringsAsFactors = FALSE)
    type_order <- c(ALL_DATA_TYPES_FILTER[ALL_DATA_TYPES_FILTER %in% counts$data_type],
                    if ("Other" %in% counts$data_type) "Other")
    counts$data_type <- factor(counts$data_type, levels = rev(type_order))
    counts <- counts[order(counts$data_type), ]
    colors <- DATA_TYPE_COLORS[as.character(counts$data_type)]
    plot_ly(counts, x = ~Freq, y = ~data_type, type = "bar", orientation = "h",
            marker = list(color = colors),
            hovertemplate = "<b>%{y}</b><br>%{x} publications<extra></extra>") %>%
      layout(xaxis = list(title = "Publications", zeroline = FALSE, gridcolor = "#3a3a3a",
                          tickfont = list(color = "#aaaaaa"), titlefont = list(color = "#aaaaaa")),
             yaxis = list(title = "", tickfont = list(size = 12, color = "#f0f0f0")),
             margin = list(l = 10, r = 20, t = 10, b = 40),
             plot_bgcolor = "#2e2e2e", paper_bgcolor = "#2e2e2e",
             font = list(family = "Helvetica Neue, Arial, sans-serif", color = "#aaaaaa")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_focus <- renderPlotly({
    df <- filtered_pubs()
    if (nrow(df) == 0) return(plotly_empty())
    tags_vec  <- trimws(unlist(strsplit(df$focus, ",\\s*")))
    counts    <- sort(table(tags_vec), decreasing = FALSE)
    counts_df <- data.frame(focus = names(counts), n = as.integer(counts), stringsAsFactors = FALSE)
    focus_colors <- c("Tau"="#e8a84a", "TDP-43"="#e07060", "\u03b1-Synuclein"="#5abf80",
                      "Amyloid-\u03b2"="#c05050", "Neuroinflammation"="#9070c8",
                      "Protein Aggregation"="#d08040", "Autophagy"="#4a98c8",
                      "Immune response"="#d06090", "Genomics/Sequencing"="#5aaa90",
                      "Biomarkers"="#5090c0", "Other"="#777777")
    bar_colors <- focus_colors[counts_df$focus]
    bar_colors[is.na(bar_colors)] <- "#777777"
    plot_ly(counts_df, x = ~n, y = ~focus, type = "bar", orientation = "h",
            marker = list(color = bar_colors),
            hovertemplate = "<b>%{y}</b><br>%{x} publications<extra></extra>") %>%
      layout(xaxis = list(title = "Publications", zeroline = FALSE, gridcolor = "#3a3a3a",
                          tickfont = list(color = "#aaaaaa"), titlefont = list(color = "#aaaaaa")),
             yaxis = list(title = "", tickfont = list(size = 12, color = "#f0f0f0")),
             margin = list(l = 10, r = 20, t = 10, b = 40),
             plot_bgcolor = "#2e2e2e", paper_bgcolor = "#2e2e2e",
             font = list(family = "Helvetica Neue, Arial, sans-serif", color = "#aaaaaa")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_model <- renderPlotly({
    df <- filtered_pubs()
    if (nrow(df) == 0 || !"model" %in% names(df)) return(plotly_empty())
    counts <- as.data.frame(table(model = df$model), stringsAsFactors = FALSE)
    model_order <- c(ALL_MODELS_FILTER[ALL_MODELS_FILTER %in% counts$model], 
                     if ("Other" %in% counts$model) "Other")
    counts$model <- factor(counts$model, levels = rev(model_order))
    counts <- counts[order(counts$model), ]
    colors <- MODEL_COLORS[as.character(counts$model)]
    plot_ly(counts, x = ~Freq, y = ~model, type = "bar", orientation = "h",
            marker = list(color = colors),
            hovertemplate = "<b>%{y}</b><br>%{x} publications<extra></extra>") %>%
      layout(xaxis = list(title = "Publications", zeroline = FALSE, gridcolor = "#3a3a3a",
                          tickfont = list(color = "#aaaaaa"), titlefont = list(color = "#aaaaaa")),
             yaxis = list(title = "", tickfont = list(size = 12, color = "#f0f0f0")),
             margin = list(l = 10, r = 20, t = 10, b = 40),
             plot_bgcolor = "#2e2e2e", paper_bgcolor = "#2e2e2e",
             font = list(family = "Helvetica Neue, Arial, sans-serif", color = "#aaaaaa")) %>%
      config(displayModeBar = FALSE)
  })
  
  output$plot_sharing <- renderPlotly({
    df <- filtered_pubs()
    if (nrow(df) == 0 || !"da_category" %in% names(df)) return(plotly_empty())
    counts <- as.data.frame(table(da_category = df$da_category), stringsAsFactors = FALSE)
    share_order <- rev(DATA_SHARING_FILTER_LEVELS[DATA_SHARING_FILTER_LEVELS %in% counts$da_category])
    counts$da_category <- factor(counts$da_category, levels = share_order)
    counts <- counts[order(counts$da_category), ]
    colors <- DATA_SHARING_COLORS[as.character(counts$da_category)]
    plot_ly(counts, x = ~Freq, y = ~da_category, type = "bar", orientation = "h",
            marker = list(color = colors),
            hovertemplate = "<b>%{y}</b><br>%{x} publications<extra></extra>") %>%
      layout(xaxis = list(title = "Publications", zeroline = FALSE, gridcolor = "#3a3a3a",
                          tickfont = list(color = "#aaaaaa"), titlefont = list(color = "#aaaaaa")),
             yaxis = list(title = "", tickfont = list(size = 12, color = "#f0f0f0")),
             margin = list(l = 10, r = 20, t = 10, b = 40),
             plot_bgcolor = "#2e2e2e", paper_bgcolor = "#2e2e2e",
             font = list(family = "Helvetica Neue, Arial, sans-serif", color = "#aaaaaa")) %>%
      config(displayModeBar = FALSE)
  })
  
  make_single_badge <- function(cat) {
    cl  <- gsub(" ", "", cat)
    ico <- DATA_SHARING_ICONS[cat] %||% "\u26AA"
    sprintf('<span class="dsh dsh-%s">%s %s</span>', cl, ico, cat)
  }
  
  da_badge_html <- function(pub_row) {
    cat      <- pub_row$da_category      %||% "Not disclosed"
    cat_data <- pub_row$da_category_data %||% ""
    cat_code <- pub_row$da_category_code %||% ""
    if (cat == "Mixed" && nchar(cat_code) > 0 && nchar(cat_data) > 0) {
      paste0(make_single_badge(cat_data), "&nbsp;",
             sprintf('<span class="dsh dsh-PublicRepository" style="font-size:10px;padding:1px 6px;">\U0001F7E2 Code</span>'))
    } else {
      make_single_badge(cat)
    }
  }
  
  make_acc_html <- function(category, accession) {
    if (is.null(accession) || nchar(accession %||% "") == 0) return("")
    if (category == "Upon Request") {
      tokens    <- trimws(strsplit(accession, " \u2014 |;")[[1]])
      formatted <- Filter(nchar, sapply(tokens, function(tok) {
        tok <- trimws(tok)
        if (grepl("@", tok))
          sprintf('<a href="mailto:%s" style="color:inherit;">%s</a>', tok, tok)
        else tok
      }))
      paste0("<br><strong>Contact:</strong> ", paste(formatted, collapse = " &nbsp;|&nbsp; "))
    } else {
      tokens <- trimws(strsplit(accession, ";\\s*")[[1]])
      formatted <- sapply(tokens, function(tok) {
        tok <- trimws(tok)
        tok <- sub("[)\\].,;]+$", "", tok)
        tok <- trimws(tok)
        if (nchar(tok) == 0) return(NA_character_)
        
        if (grepl("^https?://", tok)) {
          sprintf('<a href="%s" target="_blank" style="color:inherit;">%s</a>', tok, tok)
        } else if (grepl("^github\\.com/", tok)) {
          sprintf('<a href="https://%s" target="_blank" style="color:inherit;">%s</a>', tok, tok)
        } else if (grepl("^SCV\\d+", tok, ignore.case = TRUE)) {
          sprintf('<a href="https://www.ncbi.nlm.nih.gov/clinvar/?term=%s" target="_blank" style="color:inherit;">%s</a>',
                  tok, tok)
        } else if (grepl("^GSE\\d+|^GSM\\d+", tok)) {
          sprintf('<a href="https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=%s" target="_blank" style="color:inherit;">%s</a>',
                  tok, tok)
        } else if (grepl("^SRP\\d+|^SRR\\d+|^PRJNA\\d+", tok)) {
          sprintf('<a href="https://www.ncbi.nlm.nih.gov/sra/?term=%s" target="_blank" style="color:inherit;">%s</a>',
                  tok, tok)
        } else if (grepl("^phs\\d+", tok, ignore.case = TRUE)) {
          sprintf('<a href="https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=%s" target="_blank" style="color:inherit;">%s</a>',
                  sub("\\..*", "", tok), tok)
        } else if (grepl("^doi:|^10\\.", tok)) {
          doi_clean <- sub("^doi:\\s*", "", tok)
          doi_clean <- sub("[)\\].,;]+$", "", doi_clean)
          sprintf('<a href="https://doi.org/%s" target="_blank" style="color:inherit;">%s</a>',
                  doi_clean, tok)
        } else {
          tok
        }
      })
      formatted <- formatted[!is.na(formatted)]
      paste0("<br><strong>Accession / Link:</strong> ",
             paste(formatted, collapse = " &nbsp;&bull;&nbsp; "))
    }
  }
  
  make_da_row <- function(category, repo, accession, label = NULL) {
    if (nchar(category %||% "") == 0 || category == "Not disclosed") return("")
    box_class <- switch(category,
                        "Public Repository" = "da-box",
                        "Controlled Access" = "da-box controlled",
                        "Upon Request"      = "da-box request",
                        "da-box notdisclosed")
    ico      <- DATA_SHARING_ICONS[category] %||% "\u26AA"
    lbl      <- if (!is.null(label)) paste0(" (", label, ")") else ""
    repo_h   <- if (nchar(repo %||% "") > 0) paste0("<br><strong>Repository:</strong> ", repo) else ""
    acc_h    <- make_acc_html(category, accession)
    sprintf('<div class="%s"><strong>%s %s%s</strong>%s%s</div>',
            box_class, ico, category, lbl, repo_h, acc_h)
  }
  
  da_box_html <- function(pub_row, raw_text) {
    cat      <- pub_row$da_category      %||% "Not disclosed"
    cat_data <- pub_row$da_category_data %||% cat
    cat_code <- pub_row$da_category_code %||% ""
    
    stmt <- ""
    if (!is.null(raw_text) && nchar(raw_text %||% "") > 10) {
      uid      <- gsub("[^a-zA-Z0-9]", "", pub_row$pmid %||% as.character(runif(1)))
      preview  <- substr(raw_text, 1, 200)
      needs_more <- nchar(raw_text) > 200
      if (needs_more) {
        stmt <- paste0(
          '<div class="da-stmt-preview" id="da-prev-', uid, '">',
          htmltools::htmlEscape(preview), '\u2026',
          '<button class="da-more-btn" onclick="',
          'document.getElementById(\'da-prev-', uid, '\').style.display=\'none\';',
          'document.getElementById(\'da-full-', uid, '\').style.display=\'block\';',
          'return false;">Show full statement \u25bc</button>',
          '</div>',
          '<div class="da-stmt-full" id="da-full-', uid, '">',
          htmltools::htmlEscape(raw_text),
          '<button class="da-more-btn" onclick="',
          'document.getElementById(\'da-full-', uid, '\').style.display=\'none\';',
          'document.getElementById(\'da-prev-', uid, '\').style.display=\'block\';',
          'return false;">Show less \u25b2</button>',
          '</div>'
        )
      } else {
        stmt <- paste0(
          '<div class="da-stmt-preview">',
          htmltools::htmlEscape(raw_text),
          '</div>'
        )
      }
    }
    
    if (cat == "Mixed" && nchar(cat_code) > 0) {
      data_row <- make_da_row(cat_data,
                              pub_row$da_repo_data %||% "",
                              pub_row$da_acc_data  %||% "",
                              label = "Data")
      code_row <- make_da_row(cat_code,
                              pub_row$da_repo_code %||% "",
                              pub_row$da_acc_code  %||% "",
                              label = "Code")
      paste0(data_row, code_row, stmt)
    } else {
      paste0(make_da_row(cat,
                         pub_row$da_repository %||% "",
                         pub_row$da_accession  %||% ""),
             stmt)
    }
  }
  
  render_detail <- function(pub) {
    if (is.null(pub)) return("")
    col        <- DATA_TYPE_COLORS[pub$data_type] %||% "#7a8999"
    inst_label <- if (!is.null(pub$institution) && nchar(pub$institution %||% "") > 0)
      pub$institution else ""
    
    html_content <- div(class = "detail-panel",
                        h4(pub$title),
                        p(strong("Authors: "), pub$authors),
                        p(strong("Journal: "), pub$journal, "  \u00b7  ", strong("Year: "), pub$year),
                        p(strong("Scientist: "), pub$scientist,
                          if (nchar(inst_label) > 0) span(class = "inst-badge", inst_label)),
                        p(strong("Data Type: "),
                          span(pub$data_type, style = paste0("color:", col, ";font-weight:600;"))),
                        p(strong("Model: "), 
                          if (pub$model == "Multi-model" && !is.null(pub$model_details) && nchar(pub$model_details) > 0) 
                            paste0("Multi-model (", pub$model_details, ")") 
                          else pub$model),
                        p(strong("Focus: "), pub$focus),
                        if (!is.null(pub$reasoning) && nchar(pub$reasoning %||% "") > 5 &&
                            !startsWith(pub$reasoning %||% "", "Keyword"))
                          div(class = "reasoning-box", icon("robot"), " Claude: ", pub$reasoning),
                        HTML(da_box_html(pub, pub$da_raw %||% "")),
                        if (!is.na(pub$abstract) && nchar(pub$abstract) > 5)
                          tagList(strong("Abstract:", style = "color:#475569;"),
                                  p(class = "abstract-text", pub$abstract)),
                        div(style = "margin-top:10px;display:flex;gap:14px;flex-wrap:wrap;",
                            a(href = paste0("https://pubmed.ncbi.nlm.nih.gov/", pub$pmid),
                              target = "_blank", style = "color:#3b63c8;font-size:13px;",
                              paste0("PubMed (PMID: ", pub$pmid, ") \u2197")),
                            if (!is.null(pub$doi) && nchar(pub$doi %||% "") > 4)
                              a(href = paste0("https://doi.org/", pub$doi),
                                target = "_blank", style = "color:#3b63c8;font-size:13px;",
                                paste0("Full article (DOI: ", pub$doi, ") \u2197"))
                        )
    )
    as.character(html_content)
  }
  
  build_display_df <- function(df) {
    bdt <- function(dt) {
      cl <- gsub("[/ -]", "", dt)
      sprintf('<span class="bdt bdt-%s">%s</span>', cl, dt)
    }
    
    bmd <- function(md, details) {
      cl <- gsub("[- ]", "", md)
      display_text <- if (md == "Multi-model" && !is.null(details) && nchar(details) > 0 && details != "Multi-model") {
        paste0("Multi-model (", details, ")")
      } else {
        md
      }
      sprintf('<span class="bdt bdt-%s">%s</span>', cl, display_text)
    }
    
    sci_html <- mapply(function(nm, inst) {
      if (!is.null(inst) && nchar(inst %||% "") > 0)
        sprintf('%s <span class="inst-badge">%s</span>', nm, inst)
      else nm
    }, df$scientist, df$institution %||% "", SIMPLIFY = TRUE)
    
    da_col <- if ("da_category" %in% names(df))
      sapply(seq_len(nrow(df)), function(i) da_badge_html(df[i, ]))
    else rep('<span class="dsh dsh-Notdisclosed">\u26AA Not disclosed</span>', nrow(df))
    
    details_html <- sapply(seq_len(nrow(df)), function(i) render_detail(df[i, ]))
    
    data.frame(
      Title         = paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/', df$pmid,
                             '" target="_blank">',
                             ifelse(nchar(df$title) > 78,
                                    paste0(substr(df$title, 1, 78), "\u2026"), df$title),
                             '</a>'),
      `Data Type`   = sapply(df$data_type, bdt),
      Focus         = df$focus,
      Model         = mapply(bmd, df$model %||% "Other", df$model_details %||% "", SIMPLIFY = TRUE),
      `Data Sharing`= da_col,
      Year          = df$year,
      Scientist     = sci_html,
      Journal       = ifelse(nchar(df$journal) > 34,
                             paste0(substr(df$journal, 1, 34), "\u2026"), df$journal),
      `_details`    = details_html,
      check.names   = FALSE, stringsAsFactors = FALSE
    )
  }
  
  output$overview_pubs_title <- renderUI({
    n_f <- nrow(filtered_pubs()); n_t <- nrow(rv$publications)
    if (n_t == 0) return(span("Filtered Publications", style = "color:#1e293b;"))
    lbl <- if (n_f == n_t) sprintf("%d", n_t) else sprintf("%d / %d", n_f, n_t)
    tagList(span("Filtered Publications", style = "color:#1e293b;"),
            span(lbl, class = "overview-pubs-count"))
  })
  
  output$overview_pub_table <- renderDT({
    df <- filtered_pubs()
    if (nrow(df) == 0) {
      msg <- if (nrow(rv$publications) == 0)
        "No publications fetched yet \u2014 use the buttons above to fetch from PubMed."
      else "No publications match the current filters."
      return(datatable(data.frame(Note = msg), rownames = FALSE,
                       options = list(dom = "t", paging = FALSE)))
    }
    display_df <- build_display_df(df)
    datatable(display_df, escape = FALSE, selection = "none", rownames = FALSE,
              options = list(
                pageLength = 12, scrollX = TRUE, dom = "lfrtip",
                columnDefs = list(
                  list(visible = FALSE, targets = 8),
                  list(width = "30%", targets = 0),
                  list(width = "12%", targets = 4)
                )
              ),
              callback = JS("
        table.on('click', 'tbody td', function(e) {
          if (e.target.tagName === 'A') return;
          var tr = $(this).closest('tr');
          if (tr.hasClass('child') || tr.closest('.child').length) return;
          var row = table.row(tr);
          if (row.child.isShown()) {
            row.child.hide();
          } else {
            row.child(row.data()[8]).show();
          }
        });
      "))
  })
  
  output$ai_log_section <- renderUI({
    if (!rv$api_key_valid) return(NULL)
    tagList(h4("Classification Log", style = "color:#475569;font-size:14px;"),
            withSpinner(DTOutput("ai_log_table"), color = "#5a4fcf"))
  })
  
  output$ai_log_table <- renderDT({
    df <- rv$publications
    if (nrow(df) == 0)
      return(datatable(data.frame(Note = "No data \u2014 fetch publications first."), rownames = FALSE))
    g <- function(col, def="") if (col %in% names(df)) df[[col]] else rep(def, nrow(df))
    display <- data.frame(
      PMID           = df$pmid,
      Title          = ifelse(nchar(df$title) > 55, paste0(substr(df$title, 1, 55), "\u2026"), df$title),
      `Data Type`    = df$data_type,
      Focus          = df$focus,
      Model          = ifelse(df$model == "Multi-model" & nchar(df$model_details %||% "") > 0, 
                              paste0("Multi-model (", df$model_details, ")"), 
                              df$model),
      `Sharing`      = g("da_category"),
      `Data`         = g("da_category_data"),
      `Code`         = g("da_category_code"),
      `Repo (data)`  = g("da_repo_data"),
      `ID / Contact` = g("da_acc_data"),
      `Repo (code)`  = g("da_repo_code"),
      `Code ID`      = g("da_acc_code"),
      Reasoning      = { r <- df$reasoning %||% ""; ifelse(nchar(r) > 70, paste0(substr(r, 1, 70), "\u2026"), r) },
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(display, rownames = FALSE, selection = "none",
              options = list(pageLength = 20, scrollX = TRUE, dom = "lfrtip",
                             columnDefs = list(list(width = "18%", targets = 1),
                                               list(width = "15%", targets = 12))))
  })
  
  mk_csv <- function(df) {
    cols <- intersect(c("pmid","doi","title","authors","journal","year","scientist",
                        "institution","data_type","focus","model","model_details","reasoning",
                        "da_category","da_category_data","da_category_code",
                        "da_repository","da_accession",
                        "da_repo_data","da_acc_data","da_repo_code","da_acc_code",
                        "da_raw"),
                      names(df))
    df[, cols, drop = FALSE]
  }
  output$dl_csv <- downloadHandler(
    filename = function() paste0("neuropubs_filtered_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(mk_csv(filtered_pubs()), f, row.names = FALSE))
  output$dl_csv_all <- downloadHandler(
    filename = function() paste0("neuropubs_all_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(mk_csv(rv$publications), f, row.names = FALSE))
}

shinyApp(ui, server)
