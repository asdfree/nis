# 
# 
# 
library(Hmisc)

tf <- tempfile()

dat_url <- "https://ftp.cdc.gov/pub/Vaccines_NIS/NISPUF21.DAT"

download.file( dat_url , tf , mode = 'wb' )

r_input_url <- "https://ftp.cdc.gov/pub/Vaccines_NIS/NISPUF21.R"

r_input_lines <- readLines( r_input_url )

# do not let the script do the save()
r_input_lines <- gsub( "^save\(" , "# save(" , r_input_lines )

# redirect flat file to tf
r_input_lines <- gsub( '\"path\-to\-file\/(.*)\.DAT\"' , "tf" , r_input_lines )

eval( parse( text = r_input_lines ) )

nis_df <- NISPUF21

names( nis_df ) <- tolower( names( nis_df ) )

nis_df[ , 'one' ] <- 1
# nis_fn <- file.path( path.expand( "~" ) , "NIS" , "this_file.rds" )
# saveRDS( nis_df , file = nis_fn , compress = FALSE )
# nis_df <- readRDS( nis_fn )
options( survey.lonely.psu = "adjust" )

library(survey)

nis_design <- 
	svydesign(
		id = ~ seqnumhh , 
		strata = ~ stratum , 
		weights = ~ provwt_c , 
		data = subset( nis_df , provwt_c > 0 ) 
	)
nis_design <- 
	
	update( 
		
		nis_design , 
		
		dtap_3p =

			as.numeric(

				( p_numdah >= 3 ) |
				( p_numdhi >= 3 ) |
				( p_numdih >= 3 ) |
				( p_numdta >= 3 ) |
				( p_numdtp >= 3 )

			) ,
		
		dtap_4p =

			as.numeric(

				( p_numdah >= 4 ) |
				( p_numdhi >= 4 ) |
				( p_numdih >= 4 ) |
				( p_numdta >= 4 ) |
				( p_numdtp >= 4 )

			)
			
	)
sum( weights( nis_design , "sampling" ) != 0 )

svyby( ~ one , ~ state , nis_design , unwtd.count )
svytotal( ~ one , nis_design )

svyby( ~ one , ~ state , nis_design , svytotal )
svymean( ~ p_nuhepx , nis_design , na.rm = TRUE )

svyby( ~ p_nuhepx , ~ state , nis_design , svymean , na.rm = TRUE )
svymean( ~ sex , nis_design , na.rm = TRUE )

svyby( ~ sex , ~ state , nis_design , svymean , na.rm = TRUE )
svytotal( ~ p_nuhepx , nis_design , na.rm = TRUE )

svyby( ~ p_nuhepx , ~ state , nis_design , svytotal , na.rm = TRUE )
svytotal( ~ sex , nis_design , na.rm = TRUE )

svyby( ~ sex , ~ state , nis_design , svytotal , na.rm = TRUE )
svyquantile( ~ p_nuhepx , nis_design , 0.5 , na.rm = TRUE )

svyby( 
	~ p_nuhepx , 
	~ state , 
	nis_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ childnm , 
	denominator = ~ bf_endr06 , 
	nis_design ,
	na.rm = TRUE
)
sub_nis_design <- subset( nis_design , p_utdpol == 1 )
svymean( ~ p_nuhepx , sub_nis_design , na.rm = TRUE )
this_result <- svymean( ~ p_nuhepx , nis_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ p_nuhepx , 
		~ state , 
		nis_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nis_design )
svyvar( ~ p_nuhepx , nis_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ p_nuhepx , nis_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ p_nuhepx , nis_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ dtap_3p , nis_design ,
	method = "likelihood" )
svyttest( p_nuhepx ~ dtap_3p , nis_design )
svychisq( 
	~ dtap_3p + sex , 
	nis_design 
)
glm_result <- 
	svyglm( 
		p_nuhepx ~ dtap_3p + sex , 
		nis_design 
	)

summary( glm_result )
library(srvyr)
nis_srvyr_design <- as_survey( nis_design )
nis_srvyr_design %>%
	summarize( mean = survey_mean( p_nuhepx , na.rm = TRUE ) )

nis_srvyr_design %>%
	group_by( state ) %>%
	summarize( mean = survey_mean( p_nuhepx , na.rm = TRUE ) )

results <-
	svyby( 
		~ p_utd431h314_rout_s , 
		~ raceethk , 
		nis_design , 
		svymean
	)

stopifnot( round( results[ "HISPANIC" , "p_utd431h314_rout_sUTD" , ] , 3 ) == .711 )
stopifnot( round( results[ "NON-HISPANIC WHITE ONLY" , "p_utd431h314_rout_sUTD" , ] , 3 ) == .742 )
stopifnot( round( results[ "NON-HISPANIC BLACK ONLY" , "p_utd431h314_rout_sUTD" , ] , 3 ) == .647 )
stopifnot( round( results[ "HISPANIC" , "se.p_utd431h314_rout_sUTD" , ] , 3 ) == .015 )
stopifnot( round( results[ "NON-HISPANIC WHITE ONLY" , "se.p_utd431h314_rout_sUTD" , ] , 3 ) == .009 )
stopifnot( round( results[ "NON-HISPANIC BLACK ONLY" , "se.p_utd431h314_rout_sUTD" , ] , 3 ) == .022 )