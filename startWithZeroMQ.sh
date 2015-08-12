./dist/build/broker/broker \
	--debug \
	--source 'tcp://*:5000' \
	--sink 'tcp://*:5001' \
	--registration 'tcp://*:5009'