#!/bin/sh
# Shell script to perform an extensive test of the partial evaluator

CURRYBIN="`pwd`/../../bin"

VERBOSE=no
if [ "$1" = "-v" ] ; then
  VERBOSE=yes
fi

# use the right Curry system for the tests:
PATH=$CURRYBIN:$PATH
export PATH

# clean up before
cd test && $CURRYBIN/cleancurry -r
cd ..

# execute all tests:
# set appropriate timeout:
TIMEOUT=5
if [ -x "$CURRYBIN/pakcs" ] ; then
  TIMEOUT=30
fi

TESTDRIVERARGS="-v -t $TIMEOUT -Snatural -Anone -Awfo -Awqo --assert --eval"

LOGFILE=xxx$$

if [ $VERBOSE = yes ] ; then
  $CURRYBIN/runcurry TestDriver.curry $TESTDRIVERARGS
  if [ $? -gt 0 ] ; then
    exit 1
  fi
else
  $CURRYBIN/runcurry TestDriver.curry $TESTDRIVERARGS > $LOGFILE 2>&1
  if [ $? -gt 0 ] ; then
    echo "ERROR in peval:"
    cat $LOGFILE
    exit 1
  fi
fi

################ end of tests ####################
# Clean:
/bin/rm -f $LOGFILE
