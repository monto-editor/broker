#!/bin/sh
stack exec broker -- \
  --debug \
  --source 'tcp://*:5000' \
  --sink 'tcp://*:5001' \
  --registration 'tcp://*:5002' \
  --servicesFrom "Port 5010" \
  --servicesTo "Port 5035"
