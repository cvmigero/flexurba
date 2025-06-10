Dear Benjamin, CRAN Maintainers,

Thanks for your review of our submission.  

> Please also provide authors and (year) for the third reference in your DESCRIPTION file.

Done.

> \dontrun{} should only be used if the example really cannot be executed (e.g. because of missing additional software, missing API keys, ...) by the user.
> That's why wrapping examples in \dontrun{} adds the comment ("\# Not run:") as a warning for the user.
> Does not seem necessary.
> Please replace \dontrun with \donttest.
> Please unwrap the examples if they are executable in \< 5 sec, or replace dontrun{} with \donttest{}.
> -\> download_GHSLdata.Rd For more details: <https://contributor.r-project.org/cran-cookbook/general_issues.html#structuring-of-examples>

Done. We changed the examples and replaced the tag with \donttest.

> Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()).
> This is not allowed by CRAN policies.
> Please omit any default path in writing functions.
> In your examples/vignettes/tests you can write to tempdir().
> For more details: \<<https://contributor.r-project.org/cran-cookbook/code_issues.html#writing-files-and-directories-to-the-home-filespace>

Fixed. The `filenames` arguments in the functions `download_GHSLdata` and `crop_GHSLdata` are now required. Additionally, both functions require the user to specify an `output_directory`, so I believe we are not writing by default here. All code chunks in the vignette `flexurba.Rmd` are also set to `eval = FALSE`, ensuring no files are written to the user's file system. If I have  overlooked any other code that may cause the specific issue, please let me know and I will happy to fix it.
