set -e

# close standard input to ignore it
exec 0>/dev/null

[[ $SAMEDEC_EVENT = "National Periodic Test" ]]
[[ $SAMEDEC_ORG = "PEP" ]]
[[ $SAMEDEC_SIGNIFICANCE = "T" ]]
[[ $SAMEDEC_LOCATIONS = "000000" ]]

lifetime=$(( $SAMEDEC_PURGETIME - $SAMEDEC_ISSUETIME))
[[ $lifetime -eq $(( 30*60 )) ]]

echo "+OK"
