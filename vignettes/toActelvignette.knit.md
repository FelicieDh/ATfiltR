---
title: "toActel()"
author: "ATfiltR Team"
date: "2022-09-20"
output:
  prettydoc::html_pretty:
    theme: hpstr
    highlight: github
vignette: >
  %\VignetteIndexEntry{6. toActel()}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

`toActel()` allows you to prepare your data for further processing in [actel](https://github.com/hugomflavio/actel). You will need to have done at least some data processing with `ATfiltR`: [**compiled your data**](CompileDatavignette.html) and [**attributed deployments and animals**](wWindowvignette.html) to it. The rest is optional


## toActel()

The function `toActel()` will use all the info you provided with using `ATfiltR` to format your files for basic use in `actel`.



<br />
<br />  

### General arguments  


```r
toActel(detection.folder="Detections", data.folder="Data",
                  target.folder="Actel")
```

**detection.folder**  
<hr />

You should have an R project, and within the project directory you should have a folder that contains your ATfiltR files. Indicate the name of the folder in quotation marks. By default we indicated "Detections". 

`ATfiltR` will identify the files in the `detection.folder` that are eligible for formatting in `actel` and you'll be able to pick which file you want to process.

**data.folder**  
<hr />

You should have an R project, and within the project directory you should have a folder that contains all of your other (non-telemetry) data files. Indicate the name of the folder in quotation marks. By default we indicated "Data". *This can be the same as detection.folder*


**target.folder**  
<hr />
In what folder would you like to host your files formatted for `actel`? *This can be the same as detection.folder and/or data.folder*

