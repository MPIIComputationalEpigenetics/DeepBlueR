setwd("build/")

#build up-to-date version of R API
api_update <- system("python deepblue_r.py")
other_update = system('../R/helpingFunctions.R')

#check if the API was updated
if(api_update == 0 | other_update == 0)
{
    setwd("../")
    library(devtools)

    #update the documentation and check the package
    check <- devtools::check()

    #if there are no errors install package


    if(check) {
    	devtools::install()
    }
    else {
    	print(check)
    }
}