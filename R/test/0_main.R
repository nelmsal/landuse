STATE_STR = 'NV'
STATE_FIPS = '32'

# PATHS

ESI_DIR <- "\\\\econha01/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling"
STATE_DIR <- paste0(ESI_DIR, "/", STATE_STR)

SENSING_DIR <- paste0(STATE_DIR, "/data/sensing")



zoning_csv <- "~/Dropbox/ESI-Wharton Freddie Project/NJ Dataset/NJ.maxden.bg_0128.csv"

cbg_income_csv <- "~/Desktop/R/git/philamonitor/data/census/data/cbg_b19.csv"

cbg_household_csv = "~/Desktop/R/git/philamonitor/data/census/data/cbg_b25.csv"

cbg_pop_csv = "~/Desktop/R/git/philamonitor/data/census/data/cbg_b02.csv"

area_character_csv = "python/area_characteristics_nj.csv"

# created in EXTRACT
remote_sensing_csv =
    glue::glue("{SENSING_DIR}/remote_sensing_{STATE_STR}.csv")

comp_tif = glue("comp_{STATE_FIPS}.tif")

cities_csv = "data/cities/uscities.csv"



# 1. HELP functions

# 2. MERGE

# 3. EXTRACT/REMOTE SENSING

# 4. PAD

# 5. LEFT

# 6. LAG

