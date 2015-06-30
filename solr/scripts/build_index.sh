#!/bin/bash
echo "Loading config"
indexer_dir="`pwd`/`git rev-parse --show-cdup`/solr/indexer"
source "`pwd`/`git rev-parse --show-cdup`/load_configs.sh"

#
PGHOST=${db[host]}
if [ -z "$PGHOST" ]; then
   PGHOST="localhost"
fi
PGPORT=${db[port]}
PGDB=${db[db]}
PGUSER=${db[user]}
PGPW=${db[pass]}

SOLRHOST=${solr[host]}
SOLRPORT=${solr[port]}

cd $indexer_dir

# Run the actual indexer
echo "Compiling and running indexer with host:$PGHOST:$PGPORT using $PGUSER@$PGDB"
echo "Solr parametrs are set to: $SOLRHOST:$SOLRPORT"
mvn compile && mvn scala:run -DaddArgs="$PGHOST|$PGPORT|$PGDB|$PGUSER|$PGPW"

# Load the new index into solr by dumping and replacing
# Delete
echo "Replacing Solr DB"
curl http://$SOLRHOST:$SOLRPORT/solr/Databrary/update --data '<delete><query>*:*</query></delete>' -H 'Content-type:text/xml; charset=utf-8'

# Commit delete
curl http://$SOLRHOST:$SOLRPORT/solr/Databrary/update --data '<commit/>' -H 'Content-type:text/xml; charset=utf-8'
# Load new
curl http://$SOLRHOST:$SOLRPORT/solr/Databrary/update/json?commit=true --data-binary @databrary.json -H 'Content-type:application/json'
