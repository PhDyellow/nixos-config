{config, pkgs, ...}:
{
  home.file.r-config = {
    target = ".Rprofile";
    text = ''
                    ##Always install from csiro cran mirror when calling install.packages()
                    local({r <- getOption("repos")
                    r["CRAN"] <- "https://cran.csiro.au/"
                    options(repos=r)})

                    ##Dont ask to save workspace...actually, really I should just alias R to "R --no-save"
                    #q <- function (save="no", ...) {
                    #        quit(save=save, ...)}

                    #Stop Rstudio from checking for updates. Leave that to the nix shell
                    RSTUDIO_DISABLE_CHECK_FOR_UPDATES=1

                    ##Get tab completion on library names
                    utils::rc.settings(ipck=TRUE)

                  '';
  };

}
