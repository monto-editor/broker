trap "kill -- -$$" 2
$JAVA_HOME/bin/java -jar websockets/dist/broker-websockets.jar &
./dist/build/broker/broker \
	--debug \
	--source 'tcp://*:5000' \
	--sink 'tcp://*:5001' \
	--registration 'tcp://*:5004' \
	--servicesFrom 5010 \
	--servicesTo 5025
