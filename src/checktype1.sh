#!/bin/bash

for f in Progs-APS0/*
do
	var=$(./APS1/a.out < $f | swipl -s APS1/typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$f \n$var\n\n"
	fi
done

for f in Progs-APS1/*
do
	var=$(./APS1/a.out < $f | swipl -s APS1/typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$f \n$var\n\n"
	fi
done
