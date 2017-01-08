.onAttach <- function(libname, pkgname){
    packageStartupMessage("Welcome to the DeepBlueR package")

    if(RCurl::url.exists("http://deepblue.mpi-inf.mpg.de")){
        packageStartupMessage("DeepBlue is online")
    }else{
        warning("DeepBlue could not be reached. Check network connectivity.")
    }
}
