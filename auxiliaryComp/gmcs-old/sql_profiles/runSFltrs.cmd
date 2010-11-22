#!/bin/sh

Executable = /usr/bin/python
Universe = vanilla
getenv = true
input = 
output = runSFltrs.out
error = runSFltrs.warning
Log = runSFltrs.log
arguments = "run_specific_filters.py ospID username password"
transfer_executable = false
Queue
