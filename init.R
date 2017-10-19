# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("shiny", "shinydashboard", "htmlwidgets","DT","xtable","markdown", "corrplot", 
                "rdrop2", "Cairo", "shinyBS", "rmarkdown", "rsconnect", "RMySQL")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  rsconnect::setAccountInfo(name='kmiyachi', token='48BC42E8AFEB0EFBEC9CE89C43D7A6B8', secret='UYtDWsjJzV+d+JraHQVFfhAQZYzsoGEeDkvtvrvp')
}

invisible(sapply(my_packages, install_if_missing))


