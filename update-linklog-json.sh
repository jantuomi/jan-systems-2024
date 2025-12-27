#!/bin/sh

curl -v -H Accept:'application/json' -H "Authorization: Bearer $LNHT_TOKEN" https://api.ln.ht/v1/posts/recent -o linklog.json
