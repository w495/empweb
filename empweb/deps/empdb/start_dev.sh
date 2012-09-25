#!/bin/sh

    PATH=$PATH:/usr/local/lib/
    export PATH

    IP=`ifconfig  | grep 'inet addr:'| grep -v '127.0.0.1' | \
        cut -d: -f2 | awk '{print $1}'`

    NODENAME="empire_db@$IP"
    CONFIG=empire_db
    COOKIE=QKDFVRMXUFKAFWQFMAJA

    ERL_ARGS="+K true +A 128 +P 1000000"

    ERL_MAX_ETS_TABLES=140000
    export ERL_MAX_ETS_TABLES

    OTHERBINPATH="./deps/*/ebin"
    LOCALLBINPATH=`find ./ebin -type d -printf '%h/%f '`
    BINPATH=${OTHERBINPATH}" "${LOCALLBINPATH}
    SESSIONDBPATH='"./priv/session-db/"'

    echo "**************************************"
    echo "NODENAME --> "            $NODENAME
    echo "CONFIG --> "              $CONFIG
    echo "ERL_ARGS --> "            $ERL_ARGS
    echo "ERL_MAX_ETS_TABLES --> "  $ERL_MAX_ETS_TABLES
    echo "OTHERBINPATH --> "        $OTHERBINPATH
    echo "LOCALLBINPATH --> "       $LOCALLBINPATH
    echo "BINPATH --> "             $BINPATH
    echo "SESSIONDBPATH --> "       $SESSIONDBPATH
    echo "**************************************"

    rm -rf $SESSIONDBPATH

    #         
    erl \
        -pz $BINPATH \
        -boot start_sasl \
        -config ${CONFIG} \
        -name ${NODENAME} \
        -setcookie ${COOKIE} \
        -s sys_reloader \
        -s empire_db \
        -mnesia dir $SESSIONDBPATH \
        ${ERL_ARGS} \
    "$@"
