{
	App = {
		name = "defaultapname"
		
		default-deployment = {
			ns = name
		}
		
		deployments = {
		}
		
		ns = deployments.foo.ns
	}
	
	app = App:{
		name = "my app name"
		derive-marker-for-debugging = true
		default-deployment = {}
		deployments = {
			foo = default-deployment:{ name = "foo" }
		}
	}
}.app.ns
