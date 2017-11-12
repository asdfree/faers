if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

faers_cat <-
	get_catalog( "faers" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( faers_cat ) ) / ceiling( nrow( faers_cat ) / 5 ) )

faers_cat <- unique( rbind( faers_cat[ record_categories == this_sample_break , ] , faers_cat[ faers_cat$year == 2016 , ] ) )

lodown( "faers" , faers_cat )
