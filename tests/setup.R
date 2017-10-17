if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

faers_cat <-
	get_catalog( "faers" ,
		output_dir = file.path( getwd() ) )

# sample 40% of the records
which_records <- sample( seq( nrow( faers_cat ) ) , round( nrow( faers_cat ) * 0.40 ) )

# always sample year == 2016
faers_cat <- unique( rbind( faers_cat[ which_records , ] , subset( faers_cat , year == 2016 ) ) )

lodown( "faers" , faers_cat )
