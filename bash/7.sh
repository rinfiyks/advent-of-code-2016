#!/bin/bash

cat 7.txt | sed 's/\(.\)\1\{3,\}/\1 \1/g' | grep -Ev '\[[a-z ]*([a-z])([a-z])\2\1[a-z ]*\]' | grep -Ec '([a-z])([a-z])\2\1'

cat 7.txt | sed 's/\(.\)\1\{2,\}/\1 \1/g' | grep -Ec '(^|\])[a-z ]*(.)(.)\2.*\[[a-z ]*\3\2\3|\[[a-z ]*(.)(.)\4.*\][a-z ]*\5\4\5'
