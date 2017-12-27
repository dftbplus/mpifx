#!/bin/bash
srcdir=$(dirname $1)
fyppdir=$srcdir/../external/fypp
$fyppdir/fypp -I$(dirname $1) $1
