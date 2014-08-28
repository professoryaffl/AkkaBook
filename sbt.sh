#!/bin/bash
 
WORKSPACE=~
CP_SEP=":"
if [ "$(uname)" == "MINGW32_NT-6.1" ]; then
    CP_SEP=";"
    WORKSPACE=$LOCALAPPDATA
else
if [ "$(uname)" == "CYGWIN_NT-6.1-WOW64" ]; then
  CP_SEP=";"
  WORKSPACE=$LOCALAPPDATA
fi
fi
 
JAVA_OPTS="-Dsbt.global.base=$WORKSPACE/.sbt -Dsbt.boot.directory=$WORKSPACE/.sbt/boot -Dsbt.ivy.home=$WORKSPACE/.ivy2 -Dhttp.proxyHost=surf-proxy.intranet.db.com -Dhttp.proxyPort=8080 -Dhttp.nonProxyHosts=*.db.com -Xmx1024M -XX:MaxPermSize=512m -XX:ReservedCodeCacheSize=128m -cp sbt-launch.jar${CP_SEP}jansi-1.11.jar"
 
exec java $JAVA_OPTS xsbt.boot.Boot "$@"
