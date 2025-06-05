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

Fixed. The argument 'filenames', 'output_filenames' and 'global_filenames' in the functions download_GHSLdata and crop_GHSLdata are now required. In addition, these functions require the argument 'output_directory' which has to be specified by the user.
