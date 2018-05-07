#!/bin/bash

for f in Progs-APS0/*
do
	var=$(./APS0/a.out < $f | swipl -s APS0/semanticInterpreter.pl -g main_stdin 2>&1)
	if [[ $var != *"42"* ]]; then
		printf "$f \n$var\n\n"
	fi
done
