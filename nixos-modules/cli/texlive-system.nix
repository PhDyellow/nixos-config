{config, pkgs, ...}:
let
  # moderncv is already in TeXLive
  moderncv = pkgs.stdenvNoCC.mkDerivation {
    pname = "latex-moderncv";
    version = "2.0.0";

    outputs = [  "tex" "out"  ];

    passthru.tlDeps = with pkgs.texlive; [
      latex
      luatex

      microtype
      fancyhdr
      etoolbox
      geometry
      url
    ];

    src = inputs.moderncv;

    installPhase = ''
                runHook preInstall

                path="$tex/tex/latex/moderncv/"
                mkdir -p "$path"
                cp $src/*.{cls,sty} "$path"

                # $out must be defined, but does not
                # need to be used.
                # By putting `out` second in `outputs`,
                # `out` will not be the default
                mkdir -p $out

                runHook postInstall
                '';
  };
  awesomecv = pkgs.stdenvNoCC.mkDerivation {
    pname = "latex-awesome-cv";
    version = "1.6.1";

    phases = [ "installPhase" ];

    outputs = [  "tex" "out"  ];

    passthru.tlDeps = with pkgs.texlive; [
      latex
      luatex
      roboto
      fontawesome5
      xstring
      xifthen
      xcolor
      enumitem
      ragged2e
      geometry
      fancyhdr
      etoolbox
      setspace
      unicode-math
      sourcesanspro
      tcolorbox
      parskip
      hyperref
      bookmark
    ];

    src = inputs.awesomecv;

    installPhase = ''
                runHook preInstall

                path="$tex/tex/latex/awesome-cv/"
                mkdir -p "$path"
                cp "$src/awesome-cv.cls" "$path"

                # $out must be defined, but does not
                # need to be used.
                # By putting `out` second in `outputs`,
                # `out` will not be the default
                mkdir -p $out

                runHook postInstall
                '';
  };
  altacv = pkgs.stdenvNoCC.mkDerivation {
    pname = "latex-altacv";
    version = "1.1.3";

    outputs = [  "tex" "out"  ];

    passthru.tlDeps = with pkgs.texlive; [
      latex
      luatex
      academicons
      fontawesome5
      xcolor
      tcolorbox
      enumitem
      etoolbox
      dashrule
      multirow
      changepage
    ];

    src = inputs.altacv;

    installPhase = ''
                runHook preInstall

                path="$tex/tex/latex/AltaCV/"
                mkdir -p "$path"
                cp "$src/altacv.cls" "$path"

                # $out must be defined, but does not
                # need to be used.
                # By putting `out` second in `outputs`,
                # `out` will not be the default
                mkdir -p $out

                runHook postInstall
                '';
  };

  texlive-extended = pkgs.texliveFull.withPackages (ps:  [
    altacv
    awesomecv
  ]);
in {
  environment.systemPackages = with pkgs; [
    texlive-extended
  ];
}
