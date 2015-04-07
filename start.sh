./dist/build/broker/broker --debug \
	--source 'tcp://*:5000' \
	--sink 'tcp://*:5001' \
	--servers '[(tokens/json,[Source],"tcp://*:5010"),(ast/json,[Source],"tcp://*:5011"),(outline/json,[Source,ast/json],"tcp://*:5012"),(completions/json,[Source,ast/json],"tcp://*:5013")]'
