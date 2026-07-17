{config, pkgs, ...}:
{
  environment = {
    sessionVariables = {
      ENCHANT_CONFIG_DIR="/home/phil/.config/enchant";
    };
    systemPackages = with pkgs; [
      (aspellWithDicts (dicts: with dicts; [ en en-computers en-science]))
      hunspellDicts.en-au-large

      enchant

      (nuspell.withDicts (dicts: with dicts; [
        en-au-large
      ]))
    ];

  };
}
