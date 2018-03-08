{
	App = {
		name = "DefaultApp Name"

		default-deployment = {}:{
			namespace = name
		}

		deployment = {}

		api = default-deployment:deployment
	}

	MyApp = App:{
		name = "MyApp Name"
		deployment = { name = "My Deployment" }
	}
}.MyApp.api
