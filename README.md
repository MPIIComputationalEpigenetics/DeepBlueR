# DeepBlue Epigenomic Data Server - R
*April 6, 2016*

##  Installation and set up
`DeepBlue-R` can be directly installed from [github](https://github.com/MPIIComputationalEpigenetics//DeepBlue-R)
```{r eval=FALSE}
library(devtools)
install_github("MPIIComputationalEpigenetics/DeepBlue-R")
```

## Commands
A list of all commands is available at your [API Page](http://deepblue.mpi-inf.mpg.de/api.php)

You can have more information about them in R typing:
```{r eval=FALSE}
?deepblue.<COMMAND_NAME>
```

## Use cases
You can list ours use cases converted to R with the command:
```{r eval=FALSE}
demo(package = "DeepBlue")
```
and executing them with:
```{r eval=FALSE}
demo("use_case1", package = "DeepBlue")
```

## Examples
We are still converting ours examples to R. For while you can access the [examples in Python](deepblue.mpi-inf.mpg.de/api.php)


---
authors: *Felipe Albrecht* <<felipe.albrecht@mpi-inf.mpg.de>>
         *Markus List* <<markus.list@mpi-inf.mpg.de>>
         *Nadia Ashraf* <<nadia.ashraf98@gmail.com>>
