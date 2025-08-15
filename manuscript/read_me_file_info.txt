https://r-pkgs.org/other-markdown.html#sec-readme

    Why should I use it?
    How do I use it?
    How do I get it?

Here’s a good template for README:

    A paragraph that describes the high-level purpose of the package.

    An example that shows how to use the package to solve a simple problem.

    Installation instructions, giving code that can be copied and pasted into R.

    An overview that describes the main components of the package. For more complex packages, this will point to vignettes for more details. This is also a good place to describe how your package fits into the ecosystem of its target domain.

You’ll need to remember to re-render README.Rmd periodically and, most especially, before release. The best function to use for this is devtools::build_readme(), because it is guaranteed to render README.Rmd against the current source code of your package.



 18.2 NEWS

The README is aimed at new users, whereas the NEWS file is aimed at existing users: it should list all the changes in each release that a user might notice or want to learn more about. As with README, it’s a well-established convention for open source software to have a NEWS file, which is also sometimes called a changelog.








