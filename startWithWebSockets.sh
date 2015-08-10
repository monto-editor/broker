./dist/build/broker/broker \
	--debug \
	--websocket \
	--source '5000' \
	--sink '5001' \
	--registration 'tcp://*:5009' \
	--servers '[(tokens/java,[Source],"tcp://*:5010",""),(ast/java,[Source],"tcp://*:5011",""),(outline/java,[Source,ast/json],"tcp://*:5012",""),(completions/java,[Source,ast/json],"tcp://*:5013",""),(errors/java,[Source],"tcp://*:5014","")]'
