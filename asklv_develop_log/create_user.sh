#!/bin/bash

# 创建light001到light500用户
for i in {001..500}
do
    # echo "light$i"
    ./ejabberdctl register light$i localhost light$i
done