if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
faers_cat <- get_catalog( "faers" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( faers_cat ) ) / ceiling( nrow( faers_cat ) / 7 ) )
faers_cat <- faers_cat[ record_categories == this_sample_break , ]
lodown( "faers" , faers_cat )
if( any( faers_cat$year == 2016 & faers_cat$quarter == 4 ) ){
faers_drug_df <- 
	readRDS( file.path( getwd() , "2016 q4/drug16q4.rds" ) )

faers_outcome_df <- 
	readRDS( file.path( getwd() , "2016 q4/outc16q4.rds" ) )

faers_demo_df <- 
	readRDS( file.path( getwd() , "2016 q4/demo16q4.rds" ) )

faers_df <- merge( faers_drug_df , faers_outcome_df )

faers_df <- merge( faers_df , faers_demo_df , all.x = TRUE )

faers_df <- 
	transform( 
		faers_df , 
		
		physician_reported = as.numeric( occp_cod == "MD" ) ,
		
		init_fda_year = as.numeric( substr( init_fda_dt , 1 , 4 ) )
		
	)
	
nrow( faers_df )

table( faers_df[ , "outc_code" ] , useNA = "always" )
mean( faers_df[ , "init_fda_year" ] , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "outc_code" ] ,
	mean ,
	na.rm = TRUE 
)
prop.table( table( faers_df[ , "sex" ] ) )

prop.table(
	table( faers_df[ , c( "sex" , "outc_code" ) ] ) ,
	margin = 2
)
sum( faers_df[ , "init_fda_year" ] , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "outc_code" ] ,
	sum ,
	na.rm = TRUE 
)
quantile( faers_df[ , "init_fda_year" ] , 0.5 , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "outc_code" ] ,
	quantile ,
	0.5 ,
	na.rm = TRUE 
)
sub_faers_df <- subset( faers_df , age_grp == "E" )
mean( sub_faers_df[ , "init_fda_year" ] , na.rm = TRUE )
var( faers_df[ , "init_fda_year" ] , na.rm = TRUE )

tapply(
	faers_df[ , "init_fda_year" ] ,
	faers_df[ , "outc_code" ] ,
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
library(dplyr)
faers_tbl <- tbl_df( faers_df )
faers_tbl %>%
	summarize( mean = mean( init_fda_year , na.rm = TRUE ) )

faers_tbl %>%
	group_by( outc_code ) %>%
	summarize( mean = mean( init_fda_year , na.rm = TRUE ) )
}
