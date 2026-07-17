{config, pkgs, lib, ...}:
{
  programs.texlive = {
    enable = true;
    # The contents of extraPackages is
    # passed to ~texlive.combine~ directly
    extraPackages = tpkgs: {
      inherit (tpkgs)
        scheme-basic
        scheme-full
        biber
        collection-bibtexextra
        collection-mathscience
        collection-latexrecommended
        collection-latexextra
        collection-pictures
        collection-plaingeneric
        collection-fontsrecommended
        collection-xetex
        collection-luatex
        dvisvgm
        dvipng
        pgf;
    };
  };
}
