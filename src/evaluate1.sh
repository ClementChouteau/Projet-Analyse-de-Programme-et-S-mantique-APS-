#!/bin/bash

for f in Progs-APS0/*
do
	var=$(./APS1/a.out < $f | swipl -s APS1/semanticInterpreter.pl -g main_stdin 2>&1)
	if [[ $var != *"42"* ]]; then
		printf "$f \n$var\n\n"
	fi
done

for f in Progs-APS1/*
do
	var=$(./APS1/a.out < $f | swipl -s APS1/semanticInterpreter.pl -g main_stdin 2>&1)
	if [[ $var != *"42"* ]]; then
		printf "$f \n$var\n\n"
	fi
done
