#!/bin/bash

for f in Progs-APS0/*
do
	var=$(./APS0/a.out < $f | swipl -s APS0/typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$f \n$var\n\n"
	fi
done
