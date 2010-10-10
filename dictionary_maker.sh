#!/bin/bash
#
# Copyright 2009 Marc Worrell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
 
##
## usage dictionary_maker.sh {debug|start|stop|restart}
##
## Use "debug" to start an interactive shell (highly recommended when installing the db on the first run).
## Use "start" in production. This boots zotonic in an erlang vm with a heart beat process.
##

# Change this to your base directory
#BASE=/home/dictionary_maker
BASE=.

# Change this to the complete path to dictionary_maker.sh (this script)
DM_SH=$BASE/dictionary_maker.sh

# Change this to the directory where you have unpacked dictionary_maker
# IMPORTANT: this directory must be called dictionary_maker or dictionary_maker-x.y where x.y is the version number.
#DM=$BASE/dictionary_maker
DM=.

# Change this to point to the erlang vm
ERL="/usr/local/bin/erl"

# The include path for the erlang vm, add when needed for your application.
PA="$DM/ebin $DM/deps/*/ebin"

# The name of the Erlang node, this must be unique on your host.
SNAME=dm001

# Set the hostname to the fully qualified domain name of your host, or leave it as localhost.
# HOSTNAME=`hostname`
# HOSTNAME=your.domain.com
HOSTNAME=localhost

# The command used to restart dictionary_maker when crashed, only used after a "dictionary_maker.sh start"
export HEART_COMMAND="$DM_SH start"

## The port and IP address dictionary_maker will bind to (defaults to all ip addresses and port 8000)
export DM_IP=any
export DM_PORT=8000

# The filename where dictionary_maker writes its unix process Id to, for monitoring applications.
export DM_PIDFILE=$BASE/dictionary_maker.pid

pushd $DM >/dev/null


function start() {
    echo "Starting dictionary_maker $SNAME"
    $ERL -pa $PA -name $SNAME@$HOSTNAME -boot start_sasl -heart -detached -s dictionary_maker
}

function stop() {
    echo "Stopping dictionary_maker $SNAME"
    $ERL -noshell -pa $PA -sname ${SNAME}_stop -s dictionary_maker stop $SNAME@$HOSTNAME
}

case $1 in

  start)
    start
    ;;
 
  debug)
    $ERL +P 10000000 +K true -pa $PA -name $SNAME@$HOSTNAME -boot start_sasl -s dictionary_maker
    ;;
 
  stop)
    stop
    ;;

  shell)
    $ERL -sname dictionary_maker_shell -remsh $SNAME@$HOSTNAME
    ;;

  restart)
    echo "Restarting dictionary_maker"
    stop
    start
    ;;

  *)
    echo "Usage: $0 {debug|start|stop|restart}"
    exit 1
esac

popd > /dev/null
 
exit 0