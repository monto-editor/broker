#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

$DIR/dist/build/broker/broker \
	--debug \
	--source 'tcp://*:5000' \
	--sink 'tcp://*:5001' \
	--registration 'tcp://*:5004' \
	--discovery 'tcp://*:5005' \
	--config 'tcp://*:5007' \
	--servicesFrom "Port 5010" \
	--servicesTo "Port 5025" \
        --topic "[Source,Product,Language,ServiceID]"
