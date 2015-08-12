./dist/build/broker/broker \
	--debug \
	--websocket \
	--source '5000' \
	--sink '5001' \
	--registration 'tcp://*:5009'