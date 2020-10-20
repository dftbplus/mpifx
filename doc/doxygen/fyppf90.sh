#!/bin/bash
srcdir=$(dirname $1)
fypp -I$(dirname $1) $1
