#!/bin/bash

# Simple scripts that create db and grant proper permissions
# First param is db
# Second param is user name to grant access

# Check if a parameter is passed
if [ $# -eq 0 ]; then
    echo "No parameter provided. Provide db name and user"
    exit 1
fi
if [ $# -eq 1 ]; then
    echo "Only one parameter provided. Missing user name"
    exit 1
fi

DBNAME="$1"
DBUSER="$2"

#TODO: first check if user doesn't exist otherwise error is thrown
#psql -c "CREATE USER $DBUSER;"

psql -c "CREATE DATABASE \"$DBNAME\";"

psql -c "ALTER DATABASE \"$DBNAME\" OWNER TO $DBUSER;"

psql -c "GRANT ALL PRIVILEGES ON DATABASE \"$DBNAME\" TO $DBUSER;"
