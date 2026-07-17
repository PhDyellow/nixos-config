{
  enable = true;
  ## doesn't build, fix should be easy but
  ## time is more valuable than avoiding an unused GUI
  # package = (pkgs.recoll.override {withGui = false;});
  settings = {
    nocjk = true;
    loglevel = 5;
    underscorasletter = true;
    cachedir = "/para/resources/recoll___cache/";
    aspellLanguage = "en";
    topdirs = [
      "/para/archives"
      "/para/projects"
      "/para/resources"
      "/para/areas"
    ];
    "/para/resources" = {
      skippedNames = [ "recoll___cache" ];
    };
  };
}
