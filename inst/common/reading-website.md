We will explain this a bit during the course, but read this to start
learning how the website is structured and how to read certain things.
Specifically, there are a few "syntax" type formatting of the text in
this website to be aware of:

-   Folder names always end with a `/`, for example `data/` means the
    data folder.
-   R variables are always shown as is. For instance, for the code
    `x <- 10`, `x` is a variable because it was assigned with 10.
-   Functions always end with `()`, for instance `mean()` or
    `read_csv()`.
-   Sometimes functions have their package name appended with `::` to
    indicate to run the code from the specific package, since we likely
    haven't loaded the package with `library()`. For instance, to
    install packages from GitHub using the `{pak}` package we use
    `pak::pkg_install("user/packagename")`. You'll learn about this more
    later.
-   Reading tasks always start with a statement "Reading task" and are
    enclosed in a "callout block". Read within this block. We will
    usually go over the section again to reinforce the concepts and
    address any questions.
