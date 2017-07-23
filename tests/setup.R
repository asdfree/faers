if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

faers_cat <-
	get_catalog( "faers" ,
		output_dir = file.path( getwd() ) )

# sample 75% of the records
which_records <- sample( seq( nrow( faers_cat ) ) , round( nrow( faers_cat ) * 0.75 ) )

# always sample year == 2016
faers_cat <- unique( faers_cat[ which_records , ] , subset( faers_cat , year == 2016 ) )

lodown( "faers" , faers_cat )
