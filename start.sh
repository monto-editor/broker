trap "kill -- -$$" 2
$JAVA_HOME/bin/java -jar websockets/dist/broker-websockets.jar \
	-c tcp://*:5000 \
	-k tcp://*:5001 \
	-i tcp://*:5005 \
	-o tcp://*:5007 &
./dist/build/broker/broker \
	--debug \
	--source 'tcp://*:5000' \
	--sink 'tcp://*:5001' \
	--registration 'tcp://*:5004' \
	--discovery 'tcp://*:5005' \
	--config 'tcp://*:5007' \
	--servicesFrom "Port 5010" \
	--servicesTo "Port 5025"
