#!/bin/bash

source "load_configs.sh"
echo "Read solr install dir to be ${solr[host]} ${solr[instance]} ${solr[install]}"
${solr[install]}/bin/solr stop -all
