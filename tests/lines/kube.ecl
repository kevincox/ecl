{
	App = {
		name = "default app name"
		
		default-deployment = {
			ns = name
		}
		
		deployments = {
		}
		
		ns = deployments.foo.ns
	}
	
	MyApp = App:{
		name = "my app name"
		derive-marker-for-debugging = true
		default-deployment = {}
		deployments = {
			foo = default-deployment:{ name = "foo" }
		}
	}
}.MyApp.ns
