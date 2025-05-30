Functions for converting coordinates are in the folder called R.
Use the `*_to_*()` functions to convert single values between coordinate systems. Use the 
`coordswitch()` function to generate conversions for a data frame and append the converted 
values as new columns.

Each function is documented using roxygen2 comments within the function script. If you would 
like to update documentation, make changes there. The processed help files are stored in the 
man folder, which is automatically generated with `devtools::document()`, along with the 
NAMESPACE file.

The DESCRIPTION file needs to be updated, following specific guidelines (see 
https://methodsblog.com/2015/11/30/building-your-first-r-package/ as an example).

The original function is stored in the archived folder. To test out the functions, use the 
script in the testing folder.
