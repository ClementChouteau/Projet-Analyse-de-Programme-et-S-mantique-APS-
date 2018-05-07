#!/bin/bash

for f in Progs-APS0/*
do
	var=$(./APS3/a.out < $f | swipl -s APS3/typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$f \n$var\n\n"
	fi
done

for f in Progs-APS1/*
do
	var=$(./APS3/a.out < $f | swipl -s APS3/typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$f \n$var\n\n"
	fi
done

for f in Progs-APS2/*
do
	var=$(./APS3/a.out < $f | swipl -s APS3/typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$f \n$var\n\n"
	fi
done

for f in Progs-APS3/*
do
	var=$(./APS3/a.out < $f | swipl -s APS3/typeChecker.pl -g main_stdin 2>&1)
	if [[ $var != *"void"* ]]; then
		printf "$f \n$var\n\n"
	fi
done
