#!/bin/bash
cat $2 | while read ser
do
   echo $ser;
   ./logcutter $1 $ser > Serializers/$ser.txt
done

