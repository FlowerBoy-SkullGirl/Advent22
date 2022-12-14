#!/bin/bash

filen=`echo $1 | sed 's/Template/puzzle1/g'`
cp $1 ./$2/$filen
