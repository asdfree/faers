# side effect guestbook
# violet you're turning violet
# vi'lent dose response
library(httr)

tf <- tempfile()

this_url <- "https://fis.fda.gov/content/Exports/faers_ascii_2023q1.zip"

GET( this_url , write_disk( tf ) , progress() )

unzipped_files <- unzip( tf , exdir = tempdir() )
read_faers <-
	function( this_fn ){
		read.table( this_fn , sep = "$" , header = TRUE , comment.char = "" , quote = "" )
	}
# one record per report
faers_demo_df <- read_faers( grep( 'DEMO23Q1\\.txt$' , unzipped_files , value = TRUE ) )

# one or more record per report
faers_drug_df <- read_faers( grep( 'DRUG23Q1\\.txt$' , unzipped_files , value = TRUE ) )

# zero or more records per report
faers_outcome_df <- read_faers( grep( 'OUTC23Q1\\.txt$' , unzipped_files , value = TRUE ) )
# limit the outcome file to deaths
faers_deaths_df <- subset( faers_outcome_df , outc_cod == 'DE' )

# merge demographics with each reported death
faers_df <-	merge( faers_demo_df , faers_deaths_df )

# confirm that the analysis file matches the number of death outcomes
stopifnot( nrow( faers_deaths_df ) == nrow( faers_df ) )

# confirm zero reports include multiple deaths from the same reported adverse event
stopifnot( nrow( faers_df ) == length( unique( faers_df[ , 'primaryid' ] ) ) )
# faers_fn <- file.path( path.expand( "~" ) , "FAERS" , "this_file.rds" )
# saveRDS( faers_df , file = faers_fn , compress = FALSE )
# faers_df <- readRDS( faers_fn )
faers_df <- 
	transform( 
		faers_df , 
		
		physician_reported = as.numeric( occp_cod == "MD" ) ,
		
		reporter_country_categories = 
			ifelse( reporter_country == 'US' , 'USA' ,
			ifelse( reporter_country == 'COUNTRY NOT SPECIFIED' , 'missing' ,
			ifelse( reporter_country == 'JP' , 'Japan' ,
			ifelse( reporter_country == 'UK' , 'UK' ,
			ifelse( reporter_country == 'CA' , 'Canada' ,
			ifelse( reporter_country == 'FR' , 'France' ,
				'Other' ) ) ) ) ) ) ,
		
		init_fda_year = as.numeric( substr( init_fda_dt , 1 , 4 ) )
		
	)
	
nrow( faers_df )

table( faers_df[ , "reporter_country_categories" ] , useNA = "always" )
mean( faers_df[ , "init_fda_year" ] , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "reporter_country_categories" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( faers_df[ , "sex" ] ) )

prop.table(
	table( faers_df[ , c( "sex" , "reporter_country_categories" ) ] ) ,
	margin = 2
)
sum( faers_df[ , "init_fda_year" ] , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "reporter_country_categories" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( faers_df[ , "init_fda_year" ] , 0.5 , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "reporter_country_categories" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_faers_df <- subset( faers_df , age_grp == "E" )
mean( sub_faers_df[ , "init_fda_year" ] , na.rm = TRUE )
var( faers_df[ , "init_fda_year" ] , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "reporter_country_categories" ] ,
	var ,
	na.rm = TRUE 
)
t.test( init_fda_year ~ physician_reported , faers_df )
this_table <- table( faers_df[ , c( "physician_reported" , "sex" ) ] )

chisq.test( this_table )
glm_result <- 
	glm( 
		init_fda_year ~ physician_reported + sex , 
		data = faers_df
	)

summary( glm_result )
stopifnot( nrow( faers_df ) == 37704 )
library(dplyr)
faers_tbl <- as_tibble( faers_df )
faers_tbl %>%
	summarize( mean = mean( init_fda_year , na.rm = TRUE ) )

faers_tbl %>%
	group_by( reporter_country_categories ) %>%
	summarize( mean = mean( init_fda_year , na.rm = TRUE ) )
library(data.table)
faers_dt <- data.table( faers_df )
faers_dt[ , mean( init_fda_year , na.rm = TRUE ) ]

faers_dt[ , mean( init_fda_year , na.rm = TRUE ) , by = reporter_country_categories ]
library(duckdb)
con <- dbConnect( duckdb::duckdb() , dbdir = 'my-db.duckdb' )
dbWriteTable( con , 'faers' , faers_df )
dbGetQuery( con , 'SELECT AVG( init_fda_year ) FROM faers' )

dbGetQuery(
	con ,
	'SELECT
		reporter_country_categories ,
		AVG( init_fda_year )
	FROM
		faers
	GROUP BY
		reporter_country_categories'
)
