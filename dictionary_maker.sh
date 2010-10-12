#!/bin/bash
 
##
## usage dictionary_maker {debug|shell|status|start|stop|restart}
##
## Use "debug" to start an interactive shell (highly recommended when installing the db on the first run).
## Use "start" in production. This boots dictionary_maker in an erlang vm with a heart beat process.
##

# Change this to your base directory
BASE=/home/dictionary_maker/dictionary_maker

# Change this to the complete path to dictionary_maker (this script)
DM_SH=/etc/rc.d/dictionary_maker

# Change this to the directory where you have unpacked dictionary_maker
# IMPORTANT: this directory must be called dictionary_maker or dictionary_maker-x.y where x.y is the version number.
#DM=$BASE/dictionary_maker
DM=BASE

# Change this to point to the erlang vm
ERL="/usr/bin/erl"

# The include path for the erlang vm, add when needed for your application.
PA="$DM/ebin $DM/deps/*/ebin"

# The name of the Erlang node, this must be unique on your host.
SNAME=dm001

# Set the hostname to the fully qualified domain name of your host, or leave it as localhost.
# HOSTNAME=`hostname`
# HOSTNAME=your.domain.com
HOSTNAME=localhost

EXEC_USER=dictionary_maker

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
    su -c "$ERL -pa $PA -name $SNAME@$HOSTNAME -boot start_sasl -heart -detached -s dictionary_maker" $EXEC_USER
}

function stop() {
    echo "Stopping dictionary_maker $SNAME"
    su -c "$ERL -noshell -pa $PA -sname ${SNAME}_stop -s dictionary_maker stop $SNAME@$HOSTNAME" $EXEC_USER
}

function status() {
    if [ -f $DM_PIDFILE ]
    then
        if ps ax|grep `cat $DM_PIDFILE` > /dev/null 2>&1
        then
            echo "Dictionary Maker is running"
        else
            echo "Dictionary Maker is not running"
            echo "Erasing stale PID file $DM_PIDFILE"
            rm $DM_PIDFILE
            exit 1
        fi
    else
        echo "Dictionary Maker is not running"
        exit 1
    fi
}

case $1 in

  start)
    start
    ;;
 
  debug)
    su -c "$ERL +P 10000000 +K true -pa $PA -name $SNAME@$HOSTNAME -boot start_sasl -s dictionary_maker" $EXEC_USER
    ;;
 
  stop)
    stop
    ;;
  
  status)
    status
    ;;

  shell)
    su -c "$ERL -sname dictionary_maker_shell -remsh $SNAME@$HOSTNAME" $EXEC_USER
    ;;

  restart)
    echo "Restarting dictionary_maker"
    stop
    start
    ;;

  *)
    echo "Usage: $0 {debug|shell|status|start|stop|restart}"
    exit 1
esac

popd > /dev/null
 
exit 0