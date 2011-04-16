#!/bin/bash

export FW_CONFIG_FILE=$(pwd)/ts.cfg

erl -pa ebin -sname ts -s appmon -s ts -mnesia dir '"/tmp/ts_db"'
