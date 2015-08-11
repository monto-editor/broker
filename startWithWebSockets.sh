./dist/build/broker/broker \
	--debug \
	--websocket \
	--source '5000' \
	--sink '5001' \
	--registration 'tcp://*:5009' \
	--servers '[(tokens/json,[Source],"tcp://*:5010","")]'

	#,(ast/json,[Source],"tcp://*:5011"),(outline/json,[Source,ast/json],"tcp://*:5012"),(completions/json,[Source,ast/json],"tcp://*:5013"),(errors/json,[Source],"tcp://*:5014")]'
