#!/usr/bin/env sh

set -e

# close standard input to ignore it
exec 0>/dev/null

[ "$SAMEDEC_EVENT" = "National Periodic Test" ]
[ "$SAMEDEC_ORG" = "PEP" ]
[ "$SAMEDEC_SIGNIFICANCE" = "T" ]
[ "$SAMEDEC_SIG_NUM" -eq 0 ]
[ "$SAMEDEC_LOCATIONS" = "000000" ]
[ "$SAMEDEC_IS_NATIONAL" = "Y" ]

lifetime=$(( SAMEDEC_PURGETIME - SAMEDEC_ISSUETIME))
[ "$lifetime" -eq $(( 30*60 )) ]

echo "+OK"
