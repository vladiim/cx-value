# Description

***

# Dependencies

* [R](http://www.r-project.org/)

* [Pandoc](http://johnmacfarlane.net/pandoc/)

* Latex

> - [MacTex](http://www.tug.org/mactex/) for Mac

> - [LaTeX](http://www.latex-project.org/) for Windows

* Salesforce login, password and server set up as environment variables.

***

# Structure

* Data getters and stored data is in `./data/**`

* Graphing functions are in `./graphs/**`

* Report templates are in `./reports/templates`

* The reporting functions are in `./reports/run.R`.

***

# Running reports

The reports are written in a mix of [Markdown](http://commonmark.org/) and [R](http://www.r-project.org/) in the `./report/template/**` directory.

To run a report:

* Start R within the top-level directory `R`

* Load the initialiser `source('init.R')`

* Run one of the reports within `./report/run.R` e.g. `runReport.test()`

* The output of the report can be found in `./report/output/**`.

_Note_ in order to keep the repository size down, the output of the reports (inc images) has been ignored from Git.

***

# Writing reports

***