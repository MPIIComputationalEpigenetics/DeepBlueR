setwd("build/")

#build up-to-date version of R API
api_update <- system("python deepblue_r.py")

if(!require(devtools)) stop("Package devtools needs to be installed.")

#check if the API was updated
if(api_update == 0)
{
    setwd("../")

    #update the documentation and check the package
    check <- devtools::check()

    #if there are no errors install package
    if(!is.list(check)) {
        devtools::install(build_vignettes = TRUE)
    } else {
    	print(check)
    }
}