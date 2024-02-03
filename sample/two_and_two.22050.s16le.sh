#!/usr/bin/env sh

set -e

# close standard input to ignore it
exec 0>/dev/null

[ "$SAMEDEC_EVENT" = "Severe Thunderstorm Warning" ]
[ "$SAMEDEC_ORIGINATOR" = "Weather Service" ]
[ "$SAMEDEC_SIGNIFICANCE" = "W" ]

lifetime=$(( SAMEDEC_PURGETIME - SAMEDEC_ISSUETIME))
[ "$lifetime" -eq $(( 1*60*60 + 30*60 )) ]

echo "+OK"
