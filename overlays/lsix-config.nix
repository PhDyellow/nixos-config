final: prev:
{
  lsix = prev.lsix.overrideAttrs (oldAttrs: {
    postInstall = ''
            substituteInPlace $out/bin/lsix \
              --replace tilesize=120 tilesize=390 \
              --replace \#fontfamily=Dejavu-Sans fontfamily=Dejavu-Sans \
              --replace fontsize=\$\(\(tilewidth/10\)\) fontsize=20
          '';
  });
}
