#!/bin/bash

##################
### Config
##################
solr_version=5.1.0
solr_archive_url="https://archive.apache.org/dist/lucene/solr/$solr_version/solr-$solr_version.zip"
##################

curr_dir=`pwd`
top_dir=$curr_dir/`git rev-parse --show-cdup`
cd $top_dir
source "load_configs.sh"

echo "Downloading (if needed) and installing Solr-$solr_version ..."
echo "Installing to $top_dir/${solr[install]}"
echo "Solr core will be located in $top_dir/${solr[instance]}"
echo $curr_dir, $top_dir, $top_dir/${solr[install]} $top_dir/${solr[instance]}

echo "Stopping any running solr instances..."
bash $top_dir/solr/scripts/stop_solr.sh

core_loc=$top_dir/${solr[install]}/server/solr/databrary-core

# Download solr if install directory does not exist
if [ ! -d "$top_dir/${solr[install]}" ]; then
   prev_dir=`echo $top_dir/${solr[install]} | sed 's,/*[^/]\+/*$,,'`
   mkdir -p $prev_dir
   cd /tmp
   if [ ! -e solr-$solr_version.zip ]; then
      wget $solr_archive_url
   fi
   unzip solr-$solr_version.zip
   mv solr-$solr_version $top_dir/${solr[install]}

fi

# If we are installing the core to a directory other than in the Databrary
# directory, then copy everything there and set that up as the instance
if [ ! -e $top_dir/${solr[instance]} ]; then
   mkdir -p $top_dir/${solr[instance]}
   cp -rf $top_dir/solr/solrCore/* $top_dir/${solr[instance]}
fi

# Symlink our core directory so that it is auto-detected
if [ -e $core_loc ]; then
   rm $core_loc
fi
ln -s $top_dir/${solr[instance]} $core_loc

# Start solr
cd $curr_dir

echo "Starting solr"
bash $top_dir/solr/scripts/start_solr.sh
