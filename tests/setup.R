if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
nis_cat <- get_catalog( "nis" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( nis_cat ) ) / ceiling( nrow( nis_cat ) / 2 ) )
nis_cat <- nis_cat[ record_categories == this_sample_break , ]
nis_cat <- lodown( "nis" , nis_cat )
if( any( nis_cat$year == 2015 & nis_cat$directory == 'main' ) ){











options( survey.lonely.psu = "adjust" )

library(survey)

nis_df <- readRDS( file.path( getwd() , "2015 main.rds" ) )

nis_design <- 
	svydesign(
		id = ~ seqnumhh , 
		strata = ~ stratum , 
		weights = ~ provwt_d , 
		data = subset( nis_df , provwt_d > 0 ) 
	) 
nis_design <- 
	
	update( 
		
		nis_design , 
		
		state_name =
		
			factor(
			
				state ,
				
				levels = 
					c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 
					21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
					37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 
					55, 56, 66, 72, 78) ,
					
				labels = 
					c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
					"COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", 
					"FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA",
					"IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
					"MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", 
					"MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE",
					"NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", 
					"NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA",
					"RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE",
					"TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON",
					"WEST VIRGINIA", "WISCONSIN", "WYOMING", "GUAM", "PUERTO RICO",
					"U.S. VIRGIN ISLANDS")
					
			) ,
			
		sex = 
			factor( 
				ifelse( sex %in% 1:2 , sex , NA ) , 
				labels = c( "male" , "female" )
			) ,
			
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

svyby( ~ one , ~ state_name , nis_design , unwtd.count )
svytotal( ~ one , nis_design )

svyby( ~ one , ~ state_name , nis_design , svytotal )
svymean( ~ p_nuhepx , nis_design , na.rm = TRUE )

svyby( ~ p_nuhepx , ~ state_name , nis_design , svymean , na.rm = TRUE )
svymean( ~ sex , nis_design , na.rm = TRUE )

svyby( ~ sex , ~ state_name , nis_design , svymean , na.rm = TRUE )
svytotal( ~ p_nuhepx , nis_design , na.rm = TRUE )

svyby( ~ p_nuhepx , ~ state_name , nis_design , svytotal , na.rm = TRUE )
svytotal( ~ sex , nis_design , na.rm = TRUE )

svyby( ~ sex , ~ state_name , nis_design , svytotal , na.rm = TRUE )
svyquantile( ~ p_nuhepx , nis_design , 0.5 , na.rm = TRUE )

svyby( 
	~ p_nuhepx , 
	~ state_name , 
	nis_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
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
		~ state_name , 
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
	group_by( state_name ) %>%
	summarize( mean = survey_mean( p_nuhepx , na.rm = TRUE ) )

}
