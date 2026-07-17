{config, pkgs, ...}:
{
  systemd.timers."bib_reorganise" = {
    description = "Move new bib entries to main collection to improve caching";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "2h";
      OnUnitActiveSec = "2h";
      Unit = "bib-reorganise";
    };
  };
  systemd.services."bib_reorganise" = {

    # set this service as a oneshot job
    serviceConfig = {
      Type = "oneshot";
    };

    # have the job run this shell script
    script = with pkgs; ''
                #!/bin/bash

                # large, changes less frequently
                BIG=/para/areas/bibliography___CITE/readings.bib
                BIG2=/para/areas/bibliography___CITE/readings2.org
                # small, changes more frequently
                SMALL=/para/areas/bibliography___CITE/new_refs.bib
                SMALL2=/para/areas/bibliography___CITE/new_refs2.org

                # size of small before script actually does anything
                MAXSIZE=5000

                # get file size
                FILESIZE=$(stat -c%s "$SMALL")
                FILESIZE2=$(stat -c%s "$SMALL2")

                if ((FILESIZE > MAXSIZE)); then
                    # when $SMALL exceeds $MAXSIZE, move its content to $BIG
                    cat "$SMALL" >> "$BIG"
                    echo "" > "$SMALL"
                fi

                if ((FILESIZE2 > MAXSIZE)); then
                    # when $SMALL2 exceeds $MAXSIZE, move its content to $BIG2
                    cat "$SMALL2" >> "$BIG2"
                    echo "" > "$SMALL2"
                fi
              '';
  };
}
