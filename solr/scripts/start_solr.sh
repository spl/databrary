#!/bin/bash

cd `pwd`/`git rev-parse --show-cdup`
source "load_configs.sh"
echo "Read solr install dir to be ${solr[host]} ${solr[instance]} ${solr[install]}"
${solr[install]}/bin/solr start
